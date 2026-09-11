#lang racket/base

;; q/scripts/run-tests/flake-forensics.rkt — Suite-interference flake forensic capture
;;
;; v1.00.29 W3 (campaign v1.00.29): post-failure forensic bundles for
;; suite-context-only flaky failures. When a test fails in a full-suite or
;; grouped worker while passing standalone, `capture-flake-forensics!` records
;; one complete environment snapshot as JSON under
;; artifacts/proof-graph/v1.00.29-w3/bundles/<incident-id>.json so the failure
;; can later be reduced and replayed (see flake-reduce.rkt).
;;
;; Capture contract:
;;   - post-failure only: a green run never produces a bundle (#f is returned,
;;     nothing is written);
;;   - every mandatory field (flake-forensics-mandatory-fields) is ALWAYS
;;     present in the bundle — a probe that cannot answer records the string
;;     "unknown" instead of omitting the key;
;;   - never throws: every probe is individually guarded
;;     (with-handlers -> "unknown"), so a partial environment degrades the
;;     bundle instead of aborting the capture;
;;   - CWD-independent: the repo root is resolved from the module source
;;     location (with current-load-relative-directory / run-file /
;;     current-directory fallbacks), never from the caller's CWD alone;
;;   - deterministic given identical inputs: every environment probe is an
;;     injectable parameter (current-forensics-git-runner,
;;     current-forensics-env, current-forensics-now, ...), so tests can pin
;;     the world and assert structure instead of time values.
;;
;; incident-id = first 16 hex chars of the SHA-256 over the canonical
;; (sorted-key) JSON of the bundle without the id itself.
;;
;; STABILITY: internal (test runner infrastructure)

(require racket/date
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         (only-in "sha256.rkt" sha256-hex))

(provide capture-flake-forensics!
         flake-forensics-schema
         flake-forensics-mandatory-fields
         bundles-dir
         bundle-path-for
         resolve-repo-root
         canonical-bundle-string
         incident-id
         jsexpr->json-string
         write-json-bundle!
         guarded
         as-string
         env-lookup
         seconds->iso8601
         sorted-paths-digest
         parse-worktree-porcelain
         worktree-identities
         compiled-dir-identities
         probe-filesystem-residue
         git-probe
         current-forensics-env
         current-forensics-git-runner
         current-forensics-now
         current-forensics-proc-reader
         current-forensics-fd-reader
         forensics-repo-root)

;; ============================================================
;; Schema + mandatory fields
;; ============================================================

(define flake-forensics-schema "q.flake-forensics/1")

;; Every key in this list MUST be present in every bundle. Probes record the
;; string "unknown" when they cannot answer; they never drop the key.
(define flake-forensics-mandatory-fields
  '(schema incident-id
           source-note
           root-cause-class
           git
           workflow
           failing-test-file
           behavior-ids
           predecessor-sequence
           shard
           worker
           scheduler-mode
           seed
           selected-manifest-digest
           compiled-state
           prepared-env
           environment-policy-digest
           temp-roots
           filesystem-residue
           child-process-tree
           surviving-pids
           open-handles
           worktrees
           captured-at
           test-durations
           original-stdout
           original-stderr
           original-result-artifact
           rerun-ancestry))

;; ============================================================
;; Small guarded helpers — probes degrade to "unknown", never throw
;; ============================================================

;; Run a probe thunk; map every failure and every #f answer to "unknown".
;; List-valued probes legitimately return '() (an empty recorded list).
(define (guarded thunk)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define v (thunk))
    (if (not v) "unknown" v)))

;; Coerce path/symbol/string/number evidence into a JSON-safe string.
(define (as-string v)
  (cond
    [(string? v) v]
    [(path? v) (path->string v)]
    [(symbol? v) (symbol->string v)]
    [(number? v) (format "~a" v)]
    [else "unknown"]))

(define (env-lookup name)
  (guarded (lambda () ((current-forensics-env) name))))

(define (pad2 n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))

;; UTC RFC 3339 timestamp from Unix seconds.
(define (seconds->iso8601 s)
  (define d (seconds->date s #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

;; Canonical digest over the sorted, de-duplicated list of path strings
;; (same canonicalization as inventory.rkt's selected-paths-digest).
(define (sorted-paths-digest paths)
  (define canonical (string-join (remove-duplicates (sort paths string<?)) "\n"))
  (sha256-hex (string->bytes/utf-8 canonical)))

;; ============================================================
;; Repo-root resolution — CWD-independent
;; ============================================================

(define (repo-root-candidate? p)
  (and p (directory-exists? p) (file-exists? (build-path p "scripts" "run-tests.rkt"))))

;; Walk from `start` upward looking for the repo root (dir with
;; scripts/run-tests.rkt); #f when the filesystem root is reached.
(define (find-repo-root-from start)
  (let loop ([d (and start (simplify-path start))])
    (cond
      [(not d) #f]
      [(repo-root-candidate? d) (path->directory-path d)]
      [else
       (define up (simplify-path (build-path d 'up)))
       (and (not (equal? (path->string up) (path->string d))) (loop up))])))

(define (module-source-dir)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (let-values ([(base _name _dir?) (split-path (resolved-module-path-name
                                                  (variable-reference->resolved-module-path
                                                   (#%variable-reference))))])
      base)))

;; Resolution chain: test override, module source dir, load-relative dir,
;; run-file dir, CWD. First candidate that looks like the repo root wins.
(define (resolve-repo-root)
  (or (find-repo-root-from (forensics-repo-root))
      (find-repo-root-from (module-source-dir))
      (find-repo-root-from (current-load-relative-directory))
      (find-repo-root-from (let ([rf (find-system-path 'run-file)]) (and (path? rf) (path-only rf))))
      (find-repo-root-from (current-directory))))

;; ============================================================
;; Tiny JSON writer (dependency-free, canonical: sorted object keys)
;; ============================================================

(define (json-escape-string s out)
  (display "\"" out)
  (for ([c (in-string s)])
    (cond
      [(char=? c #\") (display "\\\"" out)]
      [(char=? c #\\) (display "\\\\" out)]
      [(char=? c #\newline) (display "\\n" out)]
      [(char=? c #\return) (display "\\r" out)]
      [(char=? c #\tab) (display "\\t" out)]
      [(char<? c #\space)
       (define h (format "~x" (char->integer c)))
       (define padded (string-append (make-string (- 4 (string-length h)) #\0) h))
       (display (string-append "\\u" padded) out)]
      [else (write-char c out)]))
  (display "\"" out))

(define (json-number-string v)
  (if (rational? v)
      (if (exact? v)
          (number->string v)
          (format "~a" v))
      "unknown"))

(define (write-json* v out)
  (cond
    [(string? v) (json-escape-string v out)]
    [(boolean? v) (display (if v "true" "false") out)]
    [(number? v) (display (json-number-string v) out)]
    [(null? v) (display "null" out)]
    [(symbol? v) (json-escape-string (symbol->string v) out)]
    [(path? v) (json-escape-string (path->string v) out)]
    [(pair? v)
     (display "[" out)
     (for ([x (in-list v)]
           [i (in-naturals)])
       (when (> i 0)
         (display "," out))
       (write-json* x out))
     (display "]" out)]
    [(hash? v)
     (define keys
       (sort (hash-map v (lambda (k _v) k))
             (lambda (a b) (string<? (format "~a" a) (format "~a" b)))))
     (display "{" out)
     (for ([k (in-list keys)]
           [i (in-naturals)])
       (when (> i 0)
         (display "," out))
       (json-escape-string (format "~a" k) out)
       (display ":" out)
       (write-json* (hash-ref v k) out))
     (display "}" out)]
    [else (json-escape-string (format "~a" v) out)]))

;; Serialize a jsexpr-shaped value (hashes with symbol/string keys, lists,
;; strings, numbers, booleans) into a canonical JSON string.
(define (jsexpr->json-string v)
  (define out (open-output-string))
  (write-json* v out)
  (get-output-string out))

;; ============================================================
;; Probes
;; ============================================================

;; Run a git command in the repo root; #f on any failure.
(define (git-probe args)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define root (resolve-repo-root))
    (define git (find-executable-path "git"))
    (and root
         git
         (let-values ([(sp out in err) (parameterize ([current-directory root])
                                         (apply subprocess #f #f #f git args))])
           (close-output-port in)
           (define out-s (string-trim (port->string out)))
           (close-input-port out)
           (close-input-port err)
           (subprocess-wait sp)
           (and (zero? (subprocess-status sp)) (not (string=? out-s "")) out-s)))))

;; Parse `git worktree list --porcelain` output into a list of
;; (hash 'path _ 'head _ 'branch _) records — pure, for tests.
(define (parse-worktree-porcelain text)
  (for/list ([block (in-list (string-split text "\n\n"))]
             #:when (non-empty-string? (string-trim block)))
    (define fields
      (for/hash ([line (in-list (string-split (string-trim block) "\n"))])
        (define kv (string-split line " " #:trim? #f))
        (values (string->symbol (car kv))
                (if (null? (cdr kv))
                    #t
                    (string-trim (string-join (cdr kv) " "))))))
    (hasheq 'path
            (or (hash-ref fields 'worktree #f) "unknown")
            'head
            (or (hash-ref fields 'HEAD #f) "unknown")
            'branch
            (or (hash-ref fields 'branch #f) "unknown"))))

(define (worktree-identities)
  (guarded (lambda ()
             (define text ((current-forensics-git-runner) (list "worktree" "list" "--porcelain")))
             (and (string? text) (parse-worktree-porcelain text)))))

;; Bounded recursive walk collecting directories named `compiled`.
(define (find-compiled-dirs root max-depth)
  (define (walk dir depth acc)
    (cond
      [(> depth max-depth) acc]
      [(>= (length acc) 16) acc]
      [else
       (for/fold ([acc acc]) ([e (in-list (sort (directory-list dir) path<?))])
         (define p (build-path dir e))
         (cond
           [(and (directory-exists? p) (string=? (path->string e) "compiled")) (append acc (list p))]
           [(directory-exists? p) (walk p (add1 depth) acc)]
           [else acc]))]))
  (walk root 0 '()))

;; Bounded mtime scan: newest .zo under `dir` vs newest .rkt under its parent
;; scope. A source newer than the bytecode => "stale".
;; Bounded recursive walk collecting files whose name ends in `ext`.
(define (bounded-files-with-ext root ext max-depth max-count)
  (define (walk dir depth acc)
    (cond
      [(> depth max-depth) acc]
      [(>= (length acc) max-count) acc]
      [else
       (for/fold ([acc acc]) ([e (in-list (sort (directory-list dir) path<?))])
         (define p (build-path dir e))
         (cond
           [(and (file-exists? p) (string-suffix? (path->string e) ext)) (append acc (list p))]
           [(directory-exists? p) (walk p (add1 depth) acc)]
           [else acc]))]))
  (walk root 0 '()))

(define (newest-mtime files)
  (for/fold ([m #f]) ([f (in-list files)])
    (define t (file-or-directory-modify-seconds f))
    (if (or (not m) (> t m)) t m)))

(define (compiled-dir-state compiled-dir)
  (guarded (lambda ()
             (define scope (simplify-path (build-path compiled-dir 'up)))
             (define newest-zo (newest-mtime (bounded-files-with-ext compiled-dir ".zo" 1 256)))
             (define newest-src (newest-mtime (bounded-files-with-ext scope ".rkt" 3 256)))
             (cond
               [(and newest-zo newest-src (> newest-src newest-zo)) "stale"]
               [(and newest-zo newest-src) "fresh"]
               [else "unknown"]))))

;; Compiled-dir identity: sha256 of the sorted repo-relative compiled-dir
;; path list + a fresh/stale verdict from the mtime comparison.
(define (compiled-dir-identities repo-root)
  (guarded (lambda ()
             (if repo-root
                 (let ()
                   (define dirs (find-compiled-dirs repo-root 4))
                   (define rels
                     (for/list ([d (in-list dirs)])
                       (path->string (find-relative-path repo-root d))))
                   (define state
                     (cond
                       [(null? dirs) "unknown"]
                       [(member "stale" (map compiled-dir-state dirs)) "stale"]
                       [else "fresh"]))
                   (hasheq 'paths-digest (sorted-paths-digest rels) 'state state))
                 "unknown"))))

;; Bounded depth-1 residue scan: leftover .tmp/.log/.pid files and Windows
;; `nul` artifacts in the repo root. '() is a valid recorded list.
(define (probe-filesystem-residue repo-root)
  (guarded (lambda ()
             (if repo-root
                 (for/list ([e (in-list (sort (directory-list repo-root) path<?))]
                            #:when (residue-entry? (build-path repo-root e)))
                   (path->string e))
                 '()))))

(define (residue-entry? p)
  (and (file-exists? p)
       (let ([name (string-downcase (path->string (file-name-from-path p)))])
         (or (string=? name "nul")
             (for/or ([ext (in-list '(".tmp" ".log" ".pid"))])
               (string-suffix? name ext))))))

;; /proc scan: descendant processes of this process (depth-bounded BFS).
(define (read-proc-entries)
  (for/list ([d (in-list (directory-list "/proc"))]
             #:when (let ([s (path->string d)]) (regexp-match? #rx"^[0-9]+$" s)))
    (define pid (string->number (path->string d)))
    (define cmd
      (with-handlers ([exn:fail? (lambda (_) "")])
        (define raw (file->bytes (build-path "/proc" d "cmdline")))
        ;; cmdline is NUL-separated; render NULs as spaces.
        (string-trim (list->string (for/list ([b (in-bytes raw)])
                                     (if (zero? b)
                                         #\space
                                         (integer->char b)))))))
    (define ppid
      (with-handlers ([exn:fail? (lambda (_) 0)])
        (stat-ppid (file->string (build-path "/proc" d "stat")))))
    (hasheq 'pid pid 'ppid ppid 'cmd cmd)))

;; /proc/<pid>/stat layout: "pid (comm) state ppid ..."; comm may contain
;; spaces and parentheses, so parse ppid after the LAST close parenthesis.
(define (stat-ppid stat)
  (let loop ([i (- (string-length stat) 1)])
    (cond
      [(< i 0) 0]
      [(char=? (string-ref stat i) #\))
       (define rest (string-split (string-trim (substring stat (+ i 1))) " "))
       (if (>= (length rest) 2)
           (or (string->number (cadr rest)) 0)
           0)]
      [else (loop (- i 1))])))

(define (descendants-of pid entries depth)
  (if (<= depth 0)
      '()
      (for/list ([e (in-list entries)]
                 #:when (= (hash-ref e 'ppid) pid))
        (hasheq 'pid
                (hash-ref e 'pid)
                'ppid
                (hash-ref e 'ppid)
                'cmd
                (hash-ref e 'cmd)
                'children
                (descendants-of (hash-ref e 'pid) entries (- depth 1))))))

;; This process's pid via /proc (no getpid in racket/base); 0 when
;; unavailable, which simply yields an empty tree downstream.
(define (self-pid)
  (with-handlers ([exn:fail? (lambda (_) 0)])
    (define stat (file->string "/proc/self/stat"))
    (or (string->number (car (string-split stat " "))) 0)))

(define (default-proc-tree)
  (guarded (lambda ()
             (define entries (read-proc-entries))
             (define children (descendants-of (self-pid) entries 3))
             (and (pair? children) children))))

(define (default-open-fds)
  (guarded (lambda ()
             (define fds (sort (map path->string (directory-list "/proc/self/fd")) string<?))
             (and (pair? fds) fds))))

;; ============================================================
;; Injected-world parameters (defined after the probes they default to;
;; tests pin these, the defaults probe for real)
;; ============================================================

;; (-> string? (or/c string? #f)) — environment variable lookup.
(define current-forensics-env (make-parameter getenv))

;; (-> (listof string?) (or/c string? #f)) — git command runner; the default
;; shells out to `git` inside the resolved repo root.
(define current-forensics-git-runner (make-parameter git-probe))

;; (-> exact-positive-integer?) — Unix seconds clock for `captured-at`.
(define current-forensics-now (make-parameter current-seconds))

;; (-> any/c) — /proc-based child process tree reader (list of hashes with
;; pid/ppid/cmd, or #f when unavailable). Tests inject fakes.
(define current-forensics-proc-reader (make-parameter (lambda () (default-proc-tree))))

;; (-> any/c) — open file-descriptor reader (list of strings, or #f).
(define current-forensics-fd-reader (make-parameter (lambda () (default-open-fds))))

;; Optional repo-root override (tests point this at a fixture tree).
(define forensics-repo-root (make-parameter #f))

;; ============================================================
;; Bundle identity + emission
;; ============================================================

(define (bundles-dir repo-root)
  (build-path repo-root "artifacts" "proof-graph" "v1.00.29-w3" "bundles"))

(define (bundle-path-for repo-root incident)
  (build-path (bundles-dir repo-root) (format "~a.json" incident)))

;; Canonical form: the bundle's sorted-key JSON without the id itself and
;; without the write-artifact pointer ('bundle-path is a side effect of the
;; capture, not part of the incident identity).
(define (canonical-bundle-string bundle)
  (jsexpr->json-string (hash-remove (hash-remove bundle 'incident-id) 'bundle-path)))

;; incident-id = first 16 hex of sha256 over the canonical bundle string.
(define (incident-id bundle)
  (substring (sha256-hex (string->bytes/utf-8 (canonical-bundle-string bundle))) 0 16))

;; Write a bundle as canonical JSON bytes (UTF-8, trailing newline).
(define (write-json-bundle! path bundle)
  (define dir (path-only path))
  (when dir
    (make-directory* dir))
  (call-with-output-file
   path
   (lambda (out)
     (write-bytes (string->bytes/utf-8 (string-append (jsexpr->json-string bundle) "\n")) out))
   #:exists 'replace))

;; ============================================================
;; Main entry
;; ============================================================

;; #:failing? is the post-failure gate: #f => no capture, returns #f, writes
;; nothing. Every probe degrades to "unknown"; the function never throws.
;; Returns the recorded bundle hash (including 'incident-id and
;; 'bundle-path) or #f on a green run.
(define (capture-flake-forensics! #:failing? failing?
                                  #:failing-test-file [failing-test-file "unknown"]
                                  #:behavior-ids [behavior-ids '()]
                                  #:predecessor-sequence [predecessor-sequence '()]
                                  #:shard [shard "unknown"]
                                  #:worker [worker "unknown"]
                                  #:scheduler-mode [scheduler-mode "unknown"]
                                  #:seed [seed "unknown"]
                                  #:selected-manifest-digest [selected-manifest-digest "unknown"]
                                  #:environment-keys [environment-keys #f]
                                  #:test-durations [test-durations "unknown"]
                                  #:original-stdout [original-stdout "unknown"]
                                  #:original-stderr [original-stderr "unknown"]
                                  #:original-result-artifact [original-result-artifact "unknown"]
                                  #:rerun-ancestry [rerun-ancestry '()]
                                  #:root-cause-class [root-cause-class "unknown"]
                                  #:source-note [source-note "v1.00.29 W3 post-failure capture"]
                                  #:filesystem-residue [filesystem-residue 'AUTO]
                                  #:repo-root [repo-root 'AUTO]
                                  #:write? [write? #t])
  (if (not failing?)
      #f
      (guarded-capture
       (lambda ()
         (define root
           (if (eq? repo-root 'AUTO)
               (resolve-repo-root)
               repo-root))
         (define residue
           (if (eq? filesystem-residue 'AUTO)
               (probe-filesystem-residue root)
               filesystem-residue))
         (define git-commit
           (guarded (lambda () ((current-forensics-git-runner) (list "rev-parse" "HEAD")))))
         (define git-tree
           (guarded (lambda () ((current-forensics-git-runner) (list "rev-parse" "HEAD^{tree}")))))
         (define bundle
           (hasheq 'schema
                   flake-forensics-schema
                   'source-note
                   source-note
                   'root-cause-class
                   root-cause-class
                   'git
                   (hasheq 'commit git-commit 'tree git-tree)
                   'workflow
                   (hasheq 'workflow-id
                           (env-lookup "GSD_WORKFLOW_ID")
                           'run-id
                           (env-lookup "GSD_RUN_ID")
                           'job-id
                           (env-lookup "GSD_JOB_ID"))
                   'failing-test-file
                   (as-string failing-test-file)
                   'behavior-ids
                   behavior-ids
                   'predecessor-sequence
                   predecessor-sequence
                   'shard
                   (as-string shard)
                   'worker
                   (as-string worker)
                   'scheduler-mode
                   (as-string scheduler-mode)
                   'seed
                   (as-string seed)
                   'selected-manifest-digest
                   (as-string selected-manifest-digest)
                   'compiled-state
                   (compiled-dir-identities root)
                   'prepared-env
                   (env-lookup "GSD_PREPARED_ENV")
                   'environment-policy-digest
                   (if (and (pair? environment-keys) (andmap string? environment-keys))
                       (sorted-paths-digest environment-keys)
                       "unknown")
                   'temp-roots
                   (hasheq 'tmpdir (env-lookup "TMPDIR") 'temp (env-lookup "TEMP"))
                   'filesystem-residue
                   residue
                   'child-process-tree
                   (guarded (lambda () ((current-forensics-proc-reader))))
                   'surviving-pids
                   (guarded (lambda ()
                              (define tree ((current-forensics-proc-reader)))
                              (if (and (list? tree) (pair? tree))
                                  (for/list ([e (in-list tree)]
                                             #:when (hash? e))
                                    (hash-ref e 'pid "unknown"))
                                  "unknown")))
                   'open-handles
                   (guarded (lambda () ((current-forensics-fd-reader))))
                   'worktrees
                   (worktree-identities)
                   'captured-at
                   (seconds->iso8601 ((current-forensics-now)))
                   'test-durations
                   test-durations
                   'original-stdout
                   original-stdout
                   'original-stderr
                   original-stderr
                   'original-result-artifact
                   original-result-artifact
                   'rerun-ancestry
                   rerun-ancestry))
         (define id (incident-id bundle))
         (define full (hash-set bundle 'incident-id id))
         (define path
           (if root
               (bundle-path-for root id)
               "unknown"))
         (define write-result
           (cond
             [(not write?) "skipped (dry-run)"]
             [(eq? path "unknown") "skipped (repo root unresolved)"]
             [else
              (guarded (lambda ()
                         (write-json-bundle! path full)
                         (as-string path)))]))
         (hash-set full 'bundle-path write-result)))))

;; Whole-capture guard: even an unexpected internal error must not propagate;
;; it degrades to a minimal recorded bundle that still carries the schema and
;; every mandatory key (all "unknown").
(define (guarded-capture build!)
  (with-handlers ([exn:fail? (lambda (_e)
                               (hash-set (minimal-bundle
                                          (lambda () (seconds->iso8601 ((current-forensics-now)))))
                                         'incident-id
                                         "unknown"))])
    (build!)))

(define (minimal-bundle now-thunk)
  (hasheq 'schema
          flake-forensics-schema
          'incident-id
          "unknown"
          'source-note
          "capture degraded: probe failure"
          'root-cause-class
          "unknown"
          'git
          (hasheq 'commit "unknown" 'tree "unknown")
          'workflow
          (hasheq 'workflow-id "unknown" 'run-id "unknown" 'job-id "unknown")
          'failing-test-file
          "unknown"
          'behavior-ids
          "unknown"
          'predecessor-sequence
          "unknown"
          'shard
          "unknown"
          'worker
          "unknown"
          'scheduler-mode
          "unknown"
          'seed
          "unknown"
          'selected-manifest-digest
          "unknown"
          'compiled-state
          "unknown"
          'prepared-env
          "unknown"
          'environment-policy-digest
          "unknown"
          'temp-roots
          (hasheq 'tmpdir "unknown" 'temp "unknown")
          'filesystem-residue
          "unknown"
          'child-process-tree
          "unknown"
          'surviving-pids
          "unknown"
          'open-handles
          "unknown"
          'worktrees
          "unknown"
          'captured-at
          (guarded now-thunk)
          'test-durations
          "unknown"
          'original-stdout
          "unknown"
          'original-stderr
          "unknown"
          'original-result-artifact
          "unknown"
          'rerun-ancestry
          "unknown"))
