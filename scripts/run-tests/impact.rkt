#lang racket/base

;; impact.rkt — change-impact test selection and deterministic prioritization.
;;
;; W4 (conservative change-impact selection) + W6 (deterministic prioritization)
;; of the TDD/test-suite improvement plan (.planning/TDD-TEST-STRATEGY-PLAN.md).
;;
;; DESIGN SPIKE DECISION (W4 action 3): the dependency graph is built with a
;; SYNTAX-BASED require extractor (read + s-expression walk), NOT
;; compiler-derived dependency info. Reasons:
;;   - compiler info requires loading/compiling every module (slow; contract
;;     side effects; varies with local bytecode cache state);
;;   - the syntax extractor is deterministic and cheap;
;;   - it is CONSERVATIVE: a false-positive edge (e.g. a quoted (require ...)
;;     datum) can only ADD dependents → a larger selection, never a smaller
;;     one — the safe direction for a fail-open selector. A missing edge would
;;     under-select, which is exactly what this design refuses to risk.
;;
;; Invariants (plan constraints):
;;   - fail open: any uncertainty (dynamic require, macros, generated code,
;;     config/package/runner changes, graph parse failure) escalates to a
;;     declared broad suite — never a silent narrowing;
;;   - empty selection is an error when any non-test source changed;
;;   - selection and ordering are SEPARATE: --prioritize impact reorders the
;;     already-selected set only (checked by tests/test-run-tests-impact.rkt);
;;   - every selected test and every escalation carries machine-readable
;;     reasons; ties break stably by repository path.

(require racket/contract
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/set
         racket/string
         racket/system
         json)

;; git adapter
(provide git-changed-files
         ;; changed-file categorization
         categorize-changed-file
         ;; coverage manifest
         load-coverage-manifest
         generate-covers-manifest-entries
         write-covers-manifest!
         covers-of-file
         ;; dependency graph
         extract-requires
         build-dependency-graph
         graph-parse-failures
         ;; selection core
         compute-impact-selection
         selection-selected
         selection-escalations
         selection-escalated?
         selection-doc-only?
         selection-fallback-suites
         selection->jsexpr
         ;; runner-level orchestration
         run-impact-selection!
         print-impact-explain
         impact-suites-for-changed-files
         ;; prioritization (W6)
         load-failure-history
         make-prioritize-ctx
         prioritize-partition
         partition-entries->jsexpr
         render-order-json
         prioritize-tiers
         embed-impact-in-results!
         ;; helper
         path-sort)

;; ============================================================
;; Root resolution
;; ============================================================

;; Repo root for runner-level orchestration (three levels up from this file:
;; scripts/run-tests/impact.rkt → repo root).
(define repo-root (simplify-path (build-path (syntax-source #'here) 'up 'up 'up)))

(define (root-relative root p)
  (path->string (find-relative-path (simple-form-path root) (simple-form-path p))))

(define (path-sort files)
  (sort (remove-duplicates files) string<?))

;; ============================================================
;; Git adapter (subprocess only, no network)
;; ============================================================

;; git-changed-files : path-string? string? string?
;;   -> (values (listof string?) boolean?)
;; Returns (values files ok?). ok? = #f on ANY git failure (missing ref,
;; not a repository, git absent) — callers escalate.
(define (git-changed-files root base head)
  (with-handlers ([exn:fail? (lambda (_) (values '() #f))])
    (define out (open-output-string))
    (parameterize ([current-directory root])
      (define ok?
        (parameterize ([current-output-port out]
                       [current-error-port (open-output-string)])
          (system* (find-executable-path "git") "diff" "--name-only" (format "~a...~a" base head))))
      (unless ok?
        (raise 'git-failed))
      (values (filter non-empty-string? (string-split (get-output-string out) "\n")) #t))))

;; ============================================================
;; Changed-file categorization
;; ============================================================

;; Deterministic bucket for a changed (repo-relative) path.
(define (categorize-changed-file rel)
  (cond
    [(string-prefix? rel "tests/")
     (cond
       [(or (string-contains? rel "/fixtures/")
            (string-contains? rel "/golden/")
            (string-contains? rel "/testdata/"))
        'fixture]
       [(equal? (path->string (file-name-from-path rel)) "info.rkt") 'config]
       [(or (string-suffix? rel ".rkt") (string-suffix? rel ".ss") (string-suffix? rel ".rktl"))
        'test]
       [else 'other])]
    [(or (string-prefix? rel "scripts/")
         (string-prefix? rel ".github/actions/")
         (string-suffix? rel "Makefile")
         (string-suffix? rel ".mk"))
     'runner-helper]
    [(or (string-prefix? rel ".github/workflows/")
         (equal? (path->string (file-name-from-path rel)) "info.rkt")
         (string-suffix? rel ".json")
         (string-prefix? rel "config/"))
     'config]
    [(string-contains? rel "/generated/") 'generated]
    [(or (string-suffix? rel ".rkt") (string-suffix? rel ".ss") (string-suffix? rel ".rktl"))
     'production]
    [else 'doc]))

;; ============================================================
;; Coverage manifest (@covers, W4 action 2)
;; ============================================================

;; load-coverage-manifest : path-string?
;;   -> (values (hash/c string? (listof string?)) (hash/c string? string?) symbol?)
;; covers: test file → covered modules; sources: test file → provenance
;; ("metadata" | "manual-review"); status: 'loaded | 'missing | 'corrupt.
;; Missing/corrupt manifests degrade to empty — unmapped sources then
;; escalate (fail open), never silently under-select.
(define (load-coverage-manifest root)
  (define path (build-path root "tests" ".coverage-manifest.json"))
  (define (empty)
    (values (hash) (hash) 'missing))
  (with-handlers ([exn:fail? (lambda (_) (values (hash) (hash) 'corrupt))])
    (cond
      [(not (file-exists? path)) (empty)]
      [else
       (define j (with-input-from-file path read-json))
       (define entries (hash-ref j 'entries #f))
       (unless (list? entries)
         (raise 'bad-manifest))
       (define covers (hash))
       (define sources (hash))
       (for ([e (in-list entries)]
             #:when (and (hash? e) (string? (hash-ref e 'test #f))))
         (set! covers (hash-set covers (hash-ref e 'test) (filter string? (hash-ref e 'covers '()))))
         (set! sources
               (hash-set sources (hash-ref e 'test) (format "~a" (hash-ref e 'source "metadata")))))
       (values covers sources 'loaded)])))

;; generate-covers-manifest-entries : path-string? -> (listof hash?)
;; Deterministic manifest body: one entry per test file carrying @covers
;; metadata, merged with manual-review entries from any pre-existing
;; manifest (manual entries survive regeneration; a reviewer's mapping wins
;; until the tag is added and re-reviewed).
(define (generate-covers-manifest-entries root)
  (define existing-path (build-path root "tests" ".coverage-manifest.json"))
  (define manual
    (with-handlers ([exn:fail? (lambda (_) (hash))])
      (cond
        [(not (file-exists? existing-path)) (hash)]
        [else
         (define j (with-input-from-file existing-path read-json))
         (for/hash ([e (in-list (hash-ref j 'entries '()))]
                    #:when (and (hash? e) (equal? (hash-ref e 'source #f) "manual-review")))
           (values (hash-ref e 'test) e))])))
  (define test-dir (build-path root "tests"))
  (define from-metadata
    (for/list ([f (in-directory test-dir)]
               #:when (and (file-exists? f)
                           (string-suffix? (path->string f) ".rkt")
                           (not (string-contains? (path->string f) "/compiled/"))
                           (pair? (covers-of-file root (root-relative root f)))))
      (define rel (root-relative root f))
      (hasheq 'test rel 'covers (covers-of-file root rel) 'source "metadata")))
  (define metadata-tests (map (lambda (h) (hash-ref h 'test)) from-metadata))
  (define manual-entries
    (for/list ([(t e) (in-hash manual)]
               #:unless (member t metadata-tests))
      e))
  (sort (append from-metadata manual-entries) string<? #:key (lambda (h) (hash-ref h 'test))))

;; covers-of-file : path-string? string? -> (listof string?)
;; Reads @covers tokens from a test file's 50-line header (same header
;; window as classify-metadata.rkt). Root-parameterized so fixture trees in
;; tests work; the runner path uses the live repo root.
(define (covers-of-file root rel)
  (define p (build-path root rel))
  (define covers '())
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (call-with-input-file
     p
     (lambda (port)
       (for ([_ (in-range 50)]
             #:break (eof-object? (peek-byte port)))
         (define line (read-line port))
         (when (string? line)
           (define m (regexp-match #rx";+[ \t]*@covers[ \t]+(.*)$" line))
           (when m
             (set! covers
                   (append covers
                           (filter non-empty-string?
                                   (map string-trim (string-split (cadr m) #rx"[ \t,]+"))))))))))
    covers))

;; write-covers-manifest! : path-string? [string?] -> hash?
;; Regenerates tests/.coverage-manifest.json from @covers metadata (via
;; generate-covers-manifest-entries, which preserves manual-review entries).
;; STRICT (W4 action 1): every @covers token must resolve to an existing
;; file relative to root — unknown paths raise, so the checked-in manifest
;; stays trustworthy. Returns the written jsexpr (deterministic body).
(define (write-covers-manifest! root [runner-version "unknown"])
  (define entries (generate-covers-manifest-entries root))
  (for ([e (in-list entries)])
    (for ([c (in-list (hash-ref e 'covers '()))])
      (unless (file-exists? (build-path root c))
        (raise-user-error 'generate-covers-manifest
                          "~a: @covers target does not exist: ~a"
                          (hash-ref e 'test)
                          c))))
  (define payload
    (hasheq 'schema_version
            1
            'generated_by
            "racket scripts/run-tests.rkt --generate-covers-manifest"
            'runner_version
            runner-version
            'entries
            entries))
  (define path (build-path root "tests" ".coverage-manifest.json"))
  (call-with-output-file path
                         #:exists 'truncate/replace
                         (lambda (out)
                           (write-json payload out)
                           (newline out)))
  payload)

;; ============================================================
;; Syntax-based dependency extractor (header decision record)
;; ============================================================

;; extract-requires : path-string? path-string? -> (cons (listof string?) symbol?)
;; Returns repo-root-relative targets of every require spec it can see plus
;; 'ok | 'parse-error. Only file-path specs resolve (strings, (file "…"),
;; sub-forms of only-in/except-in/prefix-in/rename-in/submod). Collection
;; requires cannot name repo modules under this repo's relative-require
;; convention and are ignored.
(define (extract-requires root source)
  (with-handlers ([exn:fail? (lambda (_) (cons '() 'parse-error))])
    ;; Plain `read` rejects `#lang` lines inside a module context, and
    ;; enabling read-accept-lang would load language reader plugins
    ;; (nondeterministic, code-executing). Deterministic alternative:
    ;; strip leading #lang / #reader / shebang LINES, then read datums.
    (define text (file->string source))
    (define body
      (let loop ([t text])
        (define m
          (regexp-match-positions #rx"^[ \t]*(?:#lang[^\n]*\n|#reader[^\n]*\n|#![^\n]*)\n?" t))
        (if m
            (loop (substring t (cdar m)))
            t)))
    (define forms
      (let ([port (open-input-string body)])
        (let loop ([out '()])
          (define d (read port))
          (if (eof-object? d)
              (reverse out)
              (loop (cons d out))))))
    (unless (list? forms)
      (raise-user-error 'extract-requires "not a module body"))
    (define specs '())
    (let walk ([d forms]
               [depth 0])
      (when (> depth 400)
        (raise-user-error 'extract-requires "form nesting too deep"))
      (cond
        ;; Only proper `(require spec ...)` forms contribute specs; dotted
        ;; `(require . x)` junk and quoted data are ignored.
        [(and (pair? d) (list? (cdr d)) (eq? (car d) 'require)) (set! specs (append specs (cdr d)))]
        [(pair? d)
         (cond
           ;; Proper lists iterate FLAT: list length must not consume the
           ;; nesting budget (a `(provide id ...)` with >400 identifiers
           ;; caused spurious parse-error → fail-open escalation on every
           ;; diff; fixed by iterating elements at constant depth).
           [(list? d)
            (for ([x (in-list d)])
              (walk x (add1 depth)))]
           ;; Dotted pairs still descend on both halves (e.g. alists like
           ;; (code . "message") that in-list would reject).
           [else
            (walk (car d) (add1 depth))
            (walk (cdr d) (add1 depth))])]
        [(vector? d)
         (for ([x (in-vector d)])
           (walk x (add1 depth)))]
        [else (void)]))
    (define targets
      (remove-duplicates (filter values
                                 (for/list ([spec (in-list specs)])
                                   (require-spec->path root source spec)))))
    (cons (path-sort targets) 'ok)))

;; require-spec->path : root src spec -> (or/c string? #f)
(define (require-spec->path root source spec)
  (define (resolve s)
    (and
     (string? s)
     (with-handlers ([exn:fail? (lambda (_) #f)])
       (define candidate
         (if (absolute-path? s)
             s
             (simplify-path (build-path (path-only source) s))))
       (and (file-exists? candidate)
            (member (path-get-extension candidate) '(#".rkt" #".ss" #".rktl"))
            (let ([rel (root-relative root candidate)])
              (and (not (string-prefix? rel "..")) (not (string-contains? rel "/compiled/")) rel))))))
  (let loop ([spec spec])
    (match spec
      [(? string?) (resolve spec)]
      [`(file ,s) (and (string? s) (resolve s))]
      [`(submod ,rest ...)
       (for/or ([x (in-list rest)])
         (loop x))]
      [(cons (or 'only-in 'except-in 'prefix-in 'rename-in) rest)
       (for/or ([x (in-list rest)])
         (loop x))]
      [`(lib ,_) #f]
      [`(planet ,_) #f]
      [(? symbol?) #f]
      [`(quote ,_) #f]
      [_ #f])))

;; extraction-result->status : (cons (listof string?) symbol?) -> symbol?
;; extract-requires returns (cons targets status); the status lives in the
;; cdr of that PAIR. Accessing it via (cdr (file . result)) was the W4 bug
;; that silently dropped every parse failure (fail-closed under-selection).
(define (extraction-result->status result)
  (cdr result))

;; build-dependency-graph : path-string? (listof string?) (listof string?)
;;   -> (values hash? hash? hash?)
;; Forward graph file → direct requires (restricted to the file set) and
;; reverse graph file → direct dependents; adjacency lists are path-sorted.
;; Third value: hash file → 'parse-error for unparseable modules.
(define (build-dependency-graph root files)
  (define parsed
    (for/list ([f (in-list files)])
      (cons f (extract-requires root (build-path root f)))))
  (define file-set (list->set files))
  (define parse-failures
    (for/hash ([pr (in-list parsed)]
               #:when (eq? (extraction-result->status (cdr pr)) 'parse-error))
      (values (car pr) 'parse-error)))
  (define graph
    (for/hash ([pr (in-list parsed)])
      (values (car pr)
              (if (eq? (extraction-result->status (cdr pr)) 'parse-error)
                  '()
                  (filter (lambda (t) (set-member? file-set t)) (cadr pr))))))
  (define reverse
    (for*/fold ([acc (for/hash ([f (in-list files)])
                       (values f '()))])
               ([(f targets) (in-hash graph)]
                [t (in-list targets)])
      (hash-update acc t (lambda (deps) (sort (remove-duplicates (cons f deps)) string<?)))))
  (values graph reverse parse-failures))

;; graph-parse-failures : hash? -> (listof string?)
(define (graph-parse-failures failures)
  (path-sort (hash-keys failures)))

;; ============================================================
;; Selection core (pure, root-parameterized — fixture-testable)
;; ============================================================

;; Content-scan triggers (fail open). Scanned at HEAD (the working tree IS
;; head in CI and in the developer's pre-push state).
(define (source-escalation-scan root rel)
  (with-handlers ([exn:fail? (lambda (_) '(missing-file . "changed file absent at HEAD (deleted?)"))])
    (define text (file->string (build-path root rel)))
    (cond
      [(regexp-match? #rx"#lang[ \t]+reader" text) '(generated-code . "#lang reader plugin")]
      [(regexp-match? #rx"[[(]dynamic-require" text)
       '(dynamic-require . "uses dynamic-require (graph edge unknowable)")]
      [(regexp-match? #rx"define-syntax" text) '(macro-change . "defines macros (define-syntax)")]
      [else #f])))

;; compute-impact-selection : root changed covers sources test-universe -> hash?
;; covers/sources come from load-coverage-manifest; test-universe is the set
;; (as list) of runnable test files. Selection record keys:
;;   selected    — (listof hash) {file reason-code changed-file mapping-source
;;                  dependency-path?} sorted by file, one entry per test file
;;   escalations — (listof hash) {code changed-file detail fallback-suites}
;;   escalated?  — boolean (any escalation present)
;;   fallback-suites — deduped sorted suite name strings
;;   doc-only?   — no code-bearing change at all
;;   changed     — categorized change report
;;   manifest-status — 'loaded | 'missing | 'corrupt (for reporting)
(define (compute-impact-selection root
                                  changed
                                  covers
                                  sources
                                  test-universe
                                  #:manifest-status [manifest-status 'loaded])
  (define universe (list->set test-universe))
  (define module->tests
    (for*/fold ([acc (hash)])
               ([(t targets) (in-hash covers)]
                #:when (set-member? universe t)
                [m (in-list targets)])
      (hash-update acc m (lambda (ts) (sort (remove-duplicates (cons t ts)) string<?)) '())))
  (define fallback-suites
    (map symbol->string
         (sort (remove-duplicates (cons 'fast (impact-suites-for-changed-files changed))) symbol<?)))
  (define escalations '())
  (define (escalate! code changed-file detail)
    (set!
     escalations
     (cons
      (hasheq 'code code 'changed-file changed-file 'detail detail 'fallback-suites fallback-suites)
      escalations)))
  (define changed-sorted (path-sort changed))
  (define categorized
    (for/list ([f (in-list changed-sorted)])
      (cons f (categorize-changed-file f))))
  ;; Runner/helper, config, fixture, generated changes escalate outright.
  (for ([pair (in-list categorized)])
    (define f (car pair))
    (case (cdr pair)
      [(runner-helper) (escalate! 'runner-helper-change f "runner/helper scripts affect every test")]
      [(config) (escalate! 'config-change f "configuration/workflow/package change")]
      [(fixture) (escalate! 'fixture-change f "fixture data change; consumer set is not mapped")]
      [(generated) (escalate! 'generated-code f "generated code change")]
      [(production)
       (define scan (source-escalation-scan root f))
       (when scan
         (escalate! (car scan) f (cdr scan)))]
      [else (void)]))
  (define production-changed
    (for/list ([pair (in-list categorized)]
               #:when (eq? (cdr pair) 'production))
      (car pair)))
  (define test-changed
    (for/list ([pair (in-list categorized)]
               #:when (eq? (cdr pair) 'test))
      (car pair)))
  ;; Dependency graph over the production tree (deterministic).
  (define production-files
    (path-sort (for/list ([f (in-directory root)]
                          #:when
                          (and (file-exists? f)
                               (let ([rel (root-relative root f)])
                                 (and (member (path-get-extension f) '(#".rkt" #".ss" #".rktl"))
                                      (not (string-prefix? rel "tests/"))
                                      (not (string-prefix? rel "scripts/"))
                                      (not (string-prefix? rel ".github/"))
                                      (not (string-prefix? rel "docs/"))
                                      (not (string-contains? rel "/compiled/"))))))
                 (root-relative root f))))
  (define-values (graph rgraph parse-failures) (build-dependency-graph root production-files))
  ;; Fail open when an unparseable module sits INSIDE the change's
  ;; dependency cone: the changed module itself, or any module it
  ;; transitively requires (the graph edge set is then unknown, so no
  ;; selection computed from it can be trusted). An unparseable module
  ;; elsewhere in the tree — e.g. a downstream dependent we cannot walk —
  ;; cannot change which tests this change's own requires reach (direct
  ;; covers still apply), so it must not contaminate unrelated
  ;; selections. The fixture tree intentionally contains an unparseable
  ;; broken/broken.rkt in EVERY case to pin this boundary.
  (define (requires-closure-of m)
    (define seen (mutable-set))
    (let loop ([frontier (hash-ref graph m '())])
      (for ([d (in-list frontier)]
            #:unless (set-member? seen d))
        (set-add! seen d)
        (loop (hash-ref graph d '()))))
    (set->list seen))
  (define parse-failed-changed
    (list->set (for/list ([m (in-list production-changed)]
                          #:when (hash-ref parse-failures m #f))
                 m)))
  (define parse-failed-in-cone
    (for*/list ([m (in-list production-changed)]
                [d (in-list (requires-closure-of m))]
                #:when (hash-ref parse-failures d #f))
      d))
  (for ([f (in-list (append (set->list parse-failed-changed)
                            (remove-duplicates parse-failed-in-cone)))])
    (escalate! 'graph-parse-failure
               f
               (if (set-member? parse-failed-changed f)
                   "changed module cannot be parsed; dependency graph incomplete"
                   "a module the changed code requires cannot be parsed; graph incomplete")))
  ;; Transitive dependents (deterministic BFS, lexicographic frontier).
  ;; paths: dependent → full chain m → … → dependent (all hops; the reason
  ;; strings surface the whole chain). Frontier order is sorted at every
  ;; level, so a dependent reachable through several parents gets the
  ;; lexicographically-first parent chain — stable across runs.
  (define (dependents-of m)
    (let loop ([frontier (list m)]
               [paths (hash m (list m))])
      (define next
        (sort (remove-duplicates (for*/list ([f (in-list frontier)]
                                             [d (in-list (hash-ref rgraph f '()))]
                                             #:unless (hash-ref paths d #f))
                                   d))
              string<?))
      (if (null? next)
          paths
          (loop next
                (for/fold ([acc paths]) ([d (in-list next)])
                  (define parent
                    (for/or ([f (in-list frontier)]
                             #:when (member f (hash-ref graph d '())))
                      f))
                  (hash-set acc d (append (hash-ref acc parent) (list d))))))))
  ;; Selection bookkeeping: one entry per file, best (lowest-rank) reason wins.
  (define selected '())
  (define (reason-rank code)
    (case code
      [(changed-test-file) 0]
      [(direct-cover) 1]
      [(transitive-dependent) 2]
      [else 3]))
  (define (select-entry file reason-code changed-file mapping-source [dependency-path #f])
    (hasheq 'file
            file
            'reason-code
            reason-code
            'changed-file
            changed-file
            'mapping-source
            mapping-source
            'dependency-path
            dependency-path))
  (define (add-selected! entry)
    (define file (hash-ref entry 'file))
    (define existing (findf (lambda (e) (equal? (hash-ref e 'file) file)) selected))
    (cond
      [(not existing) (set! selected (cons entry selected))]
      [(< (reason-rank (hash-ref entry 'reason-code)) (reason-rank (hash-ref existing 'reason-code)))
       (set! selected
             (cons entry (filter (lambda (e) (not (equal? (hash-ref e 'file) file))) selected)))]
      [else (void)]))
  ;; Changed test files select themselves (the L0 loop is the degenerate L1).
  (for ([f (in-list test-changed)]
        #:when (set-member? universe f))
    (add-selected! (select-entry f 'changed-test-file f "self")))
  ;; Production modules: direct covers, then transitive dependents' covers.
  (for ([m (in-list production-changed)])
    (define direct (hash-ref module->tests m '()))
    (for ([t (in-list direct)])
      (add-selected! (select-entry t 'direct-cover m (hash-ref sources t "@covers manifest"))))
    (define dep-paths (dependents-of m))
    (for ([(d path) (in-hash dep-paths)]
          #:unless (equal? d m))
      (for ([t (in-list (hash-ref module->tests d '()))])
        (add-selected! (select-entry t
                                     'transitive-dependent
                                     m
                                     (hash-ref sources t "@covers manifest")
                                     (string-join path " → ")))))
    (define any-covered-dependent?
      (for/or ([(d _) (in-hash dep-paths)])
        (pair? (hash-ref module->tests d '()))))
    (when (and (null? direct) (not any-covered-dependent?))
      (escalate! 'unmapped-source
                 m
                 "no @covers mapping reaches this changed module (direct or transitive)")))
  (define doc-only?
    (and (pair? categorized)
         (andmap (lambda (cat) (and (memq cat '(doc other)) #t)) (map cdr categorized))))
  (hasheq 'selected
          (sort selected string<? #:key (lambda (e) (hash-ref e 'file)))
          'escalations
          (sort escalations
                string<?
                #:key (lambda (e)
                        (format "~a ~a" (hash-ref e 'code) (or (hash-ref e 'changed-file) ""))))
          'escalated?
          (pair? escalations)
          'fallback-suites
          fallback-suites
          'doc-only?
          doc-only?
          'manifest-status
          manifest-status
          'changed
          (for/list ([pair (in-list categorized)])
            (hasheq 'file (car pair) 'category (cdr pair)))))

;; selection-selected : hash? -> (listof string?)
(define (selection-selected sel)
  (map (lambda (e) (hash-ref e 'file)) (hash-ref sel 'selected)))
(define (selection-escalations sel)
  (hash-ref sel 'escalations))
(define (selection-escalated? sel)
  (hash-ref sel 'escalated? #f))
(define (selection-doc-only? sel)
  (hash-ref sel 'doc-only? #f))
(define (selection-fallback-suites sel)
  (hash-ref sel 'fallback-suites))

;; JSON boundary: internal selection/escalation/category values are symbols
;; (used for case/memq reasoning); jsexpr VALUES must be strings — convert
;; here, never upstream (write-json raises on symbols).
(define (esc->jsexpr e)
  (hasheq 'code
          (symbol->string (hash-ref e 'code))
          'changed-file
          (hash-ref e 'changed-file)
          'detail
          (hash-ref e 'detail)
          'fallback-suites
          (hash-ref e 'fallback-suites)))
(define (sel->jsexpr e)
  (hasheq 'file
          (hash-ref e 'file)
          'reason-code
          (symbol->string (hash-ref e 'reason-code))
          'changed-file
          (hash-ref e 'changed-file)
          'mapping-source
          (hash-ref e 'mapping-source)
          'dependency-path
          (hash-ref e 'dependency-path)))

;; selection->jsexpr : hash? ... -> jsexpr. CI contract: objects carrying a
;; "reason-code" + "file" key are exactly the selected set; escalation /
;; fallback keys anywhere in the tree are counted by the shadow-job summary.
(define (selection->jsexpr sel #:base [base #f] #:head [head #f] #:universe-size [n #f])
  (hasheq 'mode
          "impact"
          'base
          (or base "")
          'head
          (or head "")
          'selected_count
          (length (hash-ref sel 'selected))
          'selected
          (map sel->jsexpr (hash-ref sel 'selected))
          'escalations
          (map esc->jsexpr (hash-ref sel 'escalations))
          'escalated_broad
          (hash-ref sel 'escalated? #f)
          'fallback_suites
          (hash-ref sel 'fallback-suites)
          'doc_only
          (hash-ref sel 'doc-only? #f)
          'manifest_status
          (symbol->string (hash-ref sel 'manifest-status 'loaded))
          'changed
          (for/list ([pair (in-list (hash-ref sel 'changed))])
            (hasheq 'file (hash-ref pair 'file) 'category (symbol->string (hash-ref pair 'category))))
          'universe_size
          (or n 0)))

;; ============================================================
;; Fallback suite computation (declared broad suite)
;; ============================================================

;; Area suites affected by the changed files, from the same directory
;; vocabulary the classifiers use.
(define (impact-suites-for-changed-files changed)
  (remove-duplicates
   (for/list ([f (in-list changed)])
     (cond
       [(or (string-prefix? f "runtime/") (string-contains? f "/runtime/")) 'runtime]
       [(or (string-prefix? f "tui/") (string-contains? f "/tui/")) 'tui]
       [(or (string-prefix? f "extensions/") (string-contains? f "/extensions/")) 'extensions]
       [(or (string-prefix? f "security/") (string-contains? f "/security/")) 'security]
       [(string-contains? f "/workflows/") 'workflows]
       [(string-prefix? f "arch/") 'arch]
       [(string-prefix? f "tests/") 'fast]
       [else 'fast]))))

;; ============================================================
;; Failure history (W6 action 2) — retained CI JSON artifacts only
;; ============================================================

;; load-failure-history : (or/c path-string? #f) integer? real?
;;   -> (values hash? symbol?)
;; Accepts a retained per-file CI JSON artifact (W0 schema) or a directory
;; of them; from a directory the N newest by mtime are read. Failing and
;; TIMEOUT files accumulate a decaying weight (decay^recency-rank) so a
;; stale failure cannot permanently dominate ordering. Missing or corrupt
;; input yields a NEUTRAL (empty) history plus a status symbol — never an
;; error, never a guess.
(define (load-failure-history path [recency-limit 5] [decay 1/2])
  (cond
    [(not path) (values (hash) 'disabled)]
    [else
     (define files
       (cond
         [(directory-exists? path)
          (map path->string
               (sort (filter (lambda (p)
                               (and (file-exists? p) (member (path-get-extension p) '(#".json"))))
                             (directory-list path #:build? #t))
                     >
                     #:key file-or-directory-modify-seconds))]
         [(file-exists? path) (list (path->string (simple-form-path path)))]
         [else '()]))
     (cond
       [(null? files) (values (hash) 'missing)]
       [else
        (define corrupt? (box #f))
        (define weights
          (for/fold ([acc (hash)])
                    ([p (in-list files)]
                     [k (in-naturals)]
                     #:break (or (unbox corrupt?) (>= k recency-limit)))
            (define w (expt decay k))
            (define parsed
              (with-handlers ([exn:fail? (lambda (_)
                                           (set-box! corrupt? #t)
                                           #f)])
                (call-with-input-file p read-json)))
            (cond
              [(not (hash? parsed))
               (begin
                 (set-box! corrupt? #t)
                 acc)]
              [else
               (define file-entries (hash-ref parsed 'files #f))
               (cond
                 [(not (list? file-entries))
                  (begin
                    (set-box! corrupt? #t)
                    acc)]
                 [else
                  (for/fold ([acc acc])
                            ([fe (in-list file-entries)]
                             #:when (and (hash? fe) (string? (hash-ref fe 'path #f))))
                    (define cat (hash-ref fe 'category #f))
                    (if (and cat (member (string-downcase (format "~a" cat)) '("fail" "timeout")))
                        (hash-update acc (hash-ref fe 'path) (lambda (x) (+ x w)) 0)
                        acc))])])))
        (values (if (unbox corrupt?)
                    (hash)
                    weights)
                (if (unbox corrupt?) 'corrupt 'loaded))])]))

;; ============================================================
;; Deterministic prioritization (W6 action 1)
;; ============================================================

;; Tier vocabulary (lower rank = run earlier). Ordering is defined ONLY over
;; the selected set — prioritize-partition must never add or drop a file.
(define prioritize-tiers
  #hasheq((explicit . 0)
          (direct . 1)
          (transitive . 2)
          (boundary . 3)
          (recent-failure . 4)
          (remaining . 5)))

;; make-prioritize-ctx : (listof string?) (listof hash?) hash? hash? -> hash?
;;   explicit-files      — files named on the command line (L0 current-test loop)
;;   selection-entries   — the selection record's selected list
;;   history-weights     — {file → weight}
;;   boundary-by-file    — {file → @boundary metadata value}
(define (make-prioritize-ctx explicit-files selection-entries history-weights boundary-by-file)
  (hasheq 'explicit
          (list->set explicit-files)
          'entries
          (for/hash ([e (in-list selection-entries)])
            (values (hash-ref e 'file) e))
          'history
          history-weights
          'boundaries
          boundary-by-file))

;; prioritize-partition : (listof string?) hash?
;;   -> (values (listof string?) (listof hash?))
;; Returns the SAME set reordered plus per-file entries {file tier tier-rank
;; priority-reason selection-reason-code weight} in emitted order. Total
;; deterministic order: (tier-rank, weight-desc within tier 4, path).
(define (prioritize-partition files ctx)
  (define ranked
    (for/list ([f (in-list files)])
      (cons f (tier-of f ctx))))
  (define ordered
    (sort ranked
          (lambda (a b)
            (define ra (hash-ref (cdr a) 'tier-rank))
            (define rb (hash-ref (cdr b) 'tier-rank))
            (define wa (hash-ref (cdr a) 'weight 0))
            (define wb (hash-ref (cdr b) 'weight 0))
            (cond
              [(< ra rb) #t]
              [(> ra rb) #f]
              [(= ra 4)
               (cond
                 [(> wa wb) #t]
                 [(< wa wb) #f]
                 [else (string<? (car a) (car b))])]
              [else (string<? (car a) (car b))]))))
  (values (map car ordered)
          (for/list ([pair (in-list ordered)])
            (define e (cdr pair))
            (hasheq 'file
                    (car pair)
                    'tier
                    (symbol->string (hash-ref e 'tier))
                    'tier-rank
                    (hash-ref e 'tier-rank)
                    'priority-reason
                    (hash-ref e 'priority-reason)
                    'selection-reason-code
                    (let ([c (hash-ref e 'selection-reason #f)]) (and c (symbol->string c)))
                    'weight
                    (let ([w (hash-ref e 'weight #f)]) (and w (exact->inexact w)))))))

;; tier-of : string? hash? -> hash? (private)
;; Cond order mirrors tier ranks: explicit → direct → transitive → boundary
;; → recent-failure → remaining.
(define (tier-of f ctx)
  (define explicit? (set-member? (hash-ref ctx 'explicit) f))
  (define sel (hash-ref (hash-ref ctx 'entries) f #f))
  (define sel-code (and sel (hash-ref sel 'reason-code #f)))
  (define weight (hash-ref (hash-ref ctx 'history) f #f))
  (define boundary (hash-ref (hash-ref ctx 'boundaries) f #f))
  (define boundary-contract? (and sel (member boundary '("integration" "e2e" "contract"))))
  (define (mk tier reason)
    (hasheq 'tier
            tier
            'tier-rank
            (hash-ref prioritize-tiers tier)
            'priority-reason
            reason
            'selection-reason
            sel-code
            'weight
            weight))
  (cond
    [explicit? (mk 'explicit "explicitly named current-test file")]
    [(eq? sel-code 'direct-cover)
     (mk 'direct (format "direct @covers of changed module ~a" (hash-ref sel 'changed-file)))]
    [(eq? sel-code 'changed-test-file) (mk 'direct "changed test file")]
    [(eq? sel-code 'transitive-dependent)
     (mk 'transitive
         (format "transitive dependent~a"
                 (if (and sel (hash-ref sel 'dependency-path #f))
                     (format " via ~a" (hash-ref sel 'dependency-path))
                     "")))]
    [boundary-contract?
     (mk 'boundary (format "changed-boundary contract test (@boundary ~a)" boundary))]
    [weight (mk 'recent-failure (format "recent failure (weight ~a)" (format-weight weight)))]
    [else (mk 'remaining "selected; no higher-priority signal")]))

(define (format-weight w)
  (~a (exact->inexact w)))

;; partition-entries->jsexpr : (listof hash?) (listof hash?) hash? -> jsexpr
;; Order payload for JSON evidence. Deterministic: every list is explicitly
;; sorted / emitted in run order; entries appear with the serial partition
;; first, then the parallel partition (matching execution).
(define (partition-entries->jsexpr serial-entries parallel-entries history-info)
  (hasheq 'prioritized
          #t
          'policy
          "impact-tiers/1"
          'history
          (if (symbol? history-info)
              (symbol->string history-info)
              history-info)
          'serial
          serial-entries
          'parallel
          parallel-entries))

;; render-order-json : (listof string?) (listof hash?) -> string?
;; Byte-deterministic serialization of an emitted order (W6 verify: two
;; runs with identical inputs must render byte-identical strings). Key
;; order is fixed by construction (jsexpr hasheq written with write-json);
;; no timestamps, no absolute paths, no unordered iteration.
(define (render-order-json ordered-files entries)
  (define payload (hasheq 'policy "impact-tiers/1" 'order ordered-files 'entries entries))
  (with-output-to-string (lambda ()
                           (write-json payload)
                           (newline))))

;; embed-impact-in-results! : path-string? hash? (or/c hash? #f) (listof hash?) -> void?
;; Post-run JSON evidence step (W5 shadow / W6): augments the runner's
;; results JSON with `selection`, `prioritization`, and `changed_files` keys
;; so the shadow CI job can extract the full reasoned record from a single
;; artifact. The results file stays the source of truth for outcomes — this
;; only ADDS provenance. Silent no-op when the results file is absent or
;; unreadable (evidence embedding must never turn a run into a crash).
;; changed entries may be path strings (run-impact-selection! return
;; value) or hashes with a 'file key (older callers); both embed as
;; plain strings so the CI contract (changed_files: [string]) holds.
(define (changed->path c)
  (if (string? c)
      c
      (hash-ref c 'file)))

(define (embed-impact-in-results! results-path selection prioritize-payload changed)
  ;; Evidence embedding MUST NOT be a silent green (W5 contract): a
  ;; failure here leaves impact-results.json without its selection
  ;; section, which the CI shadow guard then fails as an unexplained
  ;; empty selection. Surface the reason on stderr instead of (void).
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (eprintf
                      ";; embed-impact-in-results!: FAILED, results JSON left unaugmented: ~a~n"
                      (exn-message e)))])
    (when (file-exists? results-path)
      (define j (call-with-input-file results-path read-json))
      (when (hash? j)
        (define augmented
          (hash-set* j
                     'selection
                     (selection->jsexpr selection)
                     'prioritization
                     (or prioritize-payload (hasheq 'prioritized #f))
                     'changed_files
                     (map changed->path changed)))
        (call-with-output-file results-path
                               #:exists 'truncate/replace
                               (lambda (out)
                                 (write-json augmented out)
                                 (newline out)))))))

;; ============================================================
;; Runner-level orchestration
;; ============================================================

;; run-impact-selection! : string? string? ... -> (values (listof string?) hash? (listof string?))
;; Returns the files to run, the selection record, and the changed-file list
;; for reporting. Escalated selections run the declared broad fallback.
;; Doc-only changes return an empty file list (explicit no-op, never a
;; silent pass — the caller prints and exits 0 with JSON evidence).
;; collect : symbol? -> (listof string?) (suite → files, e.g. collect-test-files)
(define (run-impact-selection! base head #:root [root repo-root] #:collect [collect (lambda (s) '())])
  (define-values (changed git-ok?) (git-changed-files root base head))
  (define universe (collect 'all))
  (cond
    [(not git-ok?)
     (define sel
       (hasheq 'selected
               '()
               'escalations
               (list (hasheq 'code
                             'git-failure
                             'changed-file
                             #f
                             'detail
                             "git diff failed (missing ref / not a repo)"
                             'fallback-suites
                             '("fast")))
               'escalated?
               #t
               'fallback-suites
               '("fast")
               'doc-only?
               #f
               'manifest-status
               'git
               'changed
               '()))
     (values (collect 'fast) sel '())]
    [else
     (define-values (covers sources manifest-status) (load-coverage-manifest root))
     (define sel
       (compute-impact-selection root
                                 changed
                                 covers
                                 sources
                                 universe
                                 #:manifest-status manifest-status))
     (cond
       [(hash-ref sel 'doc-only?) (values '() sel changed)]
       [(hash-ref sel 'escalated?)
        (define fallback-files
          (path-sort (append* (for/list ([s (in-list (hash-ref sel 'fallback-suites))])
                                (collect (string->symbol s))))))
        (values fallback-files sel changed)]
       ;; Empty-selection-is-an-error (plan invariant) is enforced by the
       ;; runner: it exits 3 with the selection JSON rather than running
       ;; zero tests. Here we just return the (possibly empty) selection.
       [else (values (selection-selected sel) sel changed)])]))

;; print-impact-explain : hash? ... -> void (human-readable --explain view)
;; changed file → category → selected test (reason, mapping, dependency
;; path) → escalation + fallback reasons.
(define (print-impact-explain sel #:base [base #f] #:head [head #f])
  (printf ";; ── impact selection~a~a ──~n"
          (if base
              (format " base=~a" base)
              "")
          (if head
              (format " head=~a" head)
              ""))
  (printf ";; changed files:~n")
  (for ([c (in-list (hash-ref sel 'changed '()))])
    (printf ";;   ~a  [~a]~n" (hash-ref c 'file) (hash-ref c 'category)))
  (cond
    [(hash-ref sel 'doc-only?)
     (printf ";; doc-only change: no code-bearing files → zero-source-change no-op~n")]
    [else
     (printf ";; selected tests (~a):~n" (length (hash-ref sel 'selected)))
     (for ([e (in-list (hash-ref sel 'selected))])
       (printf ";;   ~a~n;;     reason: ~a  changed: ~a  source: ~a~a~n"
               (hash-ref e 'file)
               (hash-ref e 'reason-code)
               (hash-ref e 'changed-file)
               (hash-ref e 'mapping-source)
               (if (hash-ref e 'dependency-path #f)
                   (format "  via: ~a" (hash-ref e 'dependency-path))
                   "")))
     (when (pair? (hash-ref sel 'escalations))
       (printf ";; escalations (fail-open):~n")
       (for ([e (in-list (hash-ref sel 'escalations))])
         (printf ";;   ~a  ~a  ~a  → fallback: ~a~n"
                 (hash-ref e 'code)
                 (or (hash-ref e 'changed-file) "-")
                 (hash-ref e 'detail)
                 (string-join (hash-ref e 'fallback-suites "fast") " "))))]))
