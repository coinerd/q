#lang racket/base
(require json
         racket/file
         racket/path
         racket/list
         racket/string
         racket/system
         racket/port
         racket/runtime-path
         "evidence-repair-format.rkt"
         "evidence-runner-coverage.rkt"
         "review-proof.rkt"
         "verification-job.rkt"
         "composition-root.rkt"
         "../../util/json/checksum.rkt")
(provide prepare-baseline-probe!
         run-baseline-probe!
         validate-baseline-probe!
         export-baseline-probe!
         validate-exported-baseline-probe!
         recover-evidence-registration-authorization
         authorize-evidence-registration
         consume-evidence-registration-authorization!)
(define-runtime-path runner "../../scripts/gsd-evidence-probe-runner.rkt")
(define (need x message)
  (unless x
    (error 'evidence-probe message)))
(define (read-structured-output raw)
  (define in (open-input-string raw))
  (define value (read-json in))
  (need (and (hash? value) (regexp-match? #px"^[ \t\r\n]*$" (port->string in)))
        "ambiguous adapter output")
  value)
(define (git root . args)
  (define out (open-output-string))
  (define ok
    (parameterize ([current-directory root]
                   [current-output-port out])
      (apply system* (find-executable-path "git") args)))
  (need ok (format "git failed: ~a" args))
  (string-trim (get-output-string out)))
(define (id! id)
  (need (and (string? id) (regexp-match? #px"^[A-Za-z0-9][A-Za-z0-9_-]{0,79}$" id))
        "unsafe operation id"))
(define (safe root relative)
  (need (and (string? relative)
             (regexp-match? #px"^tests/[A-Za-z0-9_./-]+[.]rkt$" relative)
             (not (member ".." (string-split relative "/"))))
        "unsupported test path")
  (define p (build-path root relative))
  (for/fold ([p root]) ([part (in-list (string-split relative "/"))])
    (define next (build-path p part))
    (need (not (link-exists? next)) "linked test path")
    next)
  p)
(define (no-links! p)
  (define absolute (simplify-path (path->complete-path p) #f))
  (for/fold ([prefix #f]) ([part (in-list (explode-path absolute))])
    (define next
      (if prefix
          (build-path prefix part)
          part))
    (need (not (link-exists? next)) "linked managed resource")
    next)
  p)
(define (store c)
  (no-links! (build-path (current-owned-proof-root) (hash-ref c 'plan-id) "probes")))
(define (work-root c id)
  ;; Generation-scoped like the ledger: each receipt generation's bounded
  ;; repair owns separate worktrees, so a later generation's "repair-1"
  ;; never collides with a prior generation's retained artifacts.
  (define generation (hash-ref c 'receipt-generation 0))
  (build-path (simplify-path (build-path (current-owned-proof-root) 'up))
              "evidence-repairs"
              (hash-ref c 'plan-id)
              (if (zero? generation)
                  (format "W~a" (hash-ref c 'wave))
                  (format "W~a.g~a" (hash-ref c 'wave) generation))
              "worktrees"
              id))
(define (read-json-file p)
  (call-with-input-file p read-json))
(define (retain root value)
  (make-directory* root)
  (define digest (canonical-evidence-digest value))
  (define p (build-path root (string-append digest ".json")))
  (if (file-exists? p)
      (need (equal? value (read-json-file p)) "retention collision")
      (call-with-output-file p
                             (lambda (o) (display (canonical-evidence-json value) o))
                             #:exists 'error))
  (file-or-directory-permissions p #o444)
  (hasheq 'sha256 digest 'path (path->string p)))
(define (reload ref)
  (no-links! (hash-ref ref 'path))
  (define v (read-json-file (hash-ref ref 'path)))
  (need (equal? (canonical-evidence-digest v) (hash-ref ref 'sha256)) "retained digest mismatch")
  v)
(define (clean root)
  (need (string=? "" (git root "status" "--porcelain" "--untracked-files=all")) "checkout dirty"))
(define (manifest root entries)
  (for/list ([e entries])
    (define p (safe root (hash-ref e 'path)))
    (need (equal? (sha256-file p) (hash-ref e 'sha256)) "test digest mismatch")
    e))
(define (check-spec base s)
  (need (valid-probe-spec? s) "invalid probe spec")
  (define c (hash-ref s 'context))
  (need (valid-evidence-context? c) "invalid context")
  (need (equal? (path->string (simplify-path (path->complete-path base))) (hash-ref c 'repo))
        "repository mismatch")
  (clean base)
  (need (equal? (git base "rev-parse" "HEAD") (hash-ref c 'head)) "candidate HEAD changed")
  (need (equal? (git base "rev-parse" "HEAD^{tree}") (hash-ref c 'tree)) "candidate tree changed")
  (need (equal? (hash-ref s 'suite-export) "evidence-suite") "unsupported suite export")
  (need (equal? (hash-ref s 'runtime)
                (hasheq 'racket-version (version) 'platform (symbol->string (system-type 'os))))
        "runtime mismatch")
  (need (and (exact-positive-integer? (hash-ref s 'timeout-sec)) (<= (hash-ref s 'timeout-sec) 600))
        "invalid timeout")
  (define files (append (hash-ref s 'test-files) (hash-ref s 'harness-files)))
  (define paths (map (lambda (e) (hash-ref e 'path)) files))
  (need (= (length paths) (length (remove-duplicates paths))) "duplicate overlay paths")
  (need (equal? (hash-ref s 'test-paths)
                (map (lambda (e) (hash-ref e 'path)) (hash-ref s 'test-files)))
        "test selection mismatch")
  (need (= (length (hash-ref s 'test-paths)) 1) "first slice requires one test module")
  (need (regression-command-covered? base
                                     (hash-ref c 'head)
                                     (hash-ref s 'focused-normalized-command)
                                     (car (hash-ref s 'test-paths)))
        "unapproved frozen regression coverage")
  (manifest base files)
  (git base "merge-base" "--is-ancestor" (hash-ref s 'baseline-head) (hash-ref c 'head))
  (need (equal? (git base "rev-parse" (string-append (hash-ref s 'baseline-head) "^{tree}"))
                (hash-ref s 'baseline-tree))
        "baseline tree mismatch")
  (need (not (equal? (hash-ref s 'baseline-tree) (hash-ref c 'tree))) "baseline identical")
  files)
(define (prepare-baseline-probe! base s id)
  (id! id)
  (define files (check-spec base s))
  (define c (hash-ref s 'context))
  (define root (no-links! (work-root c id)))
  (need (not (directory-exists? root)) "operation already prepared")
  (make-directory* root)
  (define b (build-path root "baseline"))
  (define a (build-path root "candidate"))
  (git base "worktree" "add" "--detach" (path->string b) (hash-ref s 'baseline-head))
  (git base "worktree" "add" "--detach" (path->string a) (hash-ref c 'head))
  (for ([e files])
    (define p (hash-ref e 'path))
    (define dst (safe b p))
    (make-directory* (path-only dst))
    (copy-file (safe a p) dst #t))
  (apply git b "add" "--" (map (lambda (e) (hash-ref e 'path)) files))
  (git b
       "-c"
       "user.name=Evidence Probe"
       "-c"
       "user.email=probe@invalid"
       "commit"
       "--allow-empty"
       "-m"
       "Exact candidate test overlay")
  (hasheq 'racket-sha256
          (sha256-file (find-executable-path "racket"))
          'kind
          "prepared-probe/v1"
          'operation-id
          id
          'spec
          s
          'baseline
          (path->string b)
          'candidate
          (path->string a)
          'overlay-head
          (git b "rev-parse" "HEAD")
          'runner-sha256
          (sha256-file runner)))
(define (job-facts job)
  (hasheq 'id
          (symbol->string (verification-job-id job))
          'identity
          (verification-job-identity-key job)
          'pid
          (verification-job-pid job)
          'start-ticks
          (verification-job-proc-start-ticks job)
          'start-ms
          (inexact->exact (floor (verification-job-start-ms job)))
          'end-ms
          (inexact->exact (floor (verification-job-end-ms job)))
          'state
          (symbol->string (verification-job-state job))
          'exit
          (verification-job-exit-code job)
          'reaped
          (verification-job-reaped? job)
          'command-sha256
          (verification-job-command-digest job)))
(define publication-guards (make-hash))
(define (run-baseline-probe! base p id #:guard guard)
  (if (not (and (procedure? guard) (guard)))
      (hasheq 'status "blocked" 'code "stale-guard")
      (with-handlers
          ([exn:fail?
            (lambda (e)
              (hasheq 'status "blocked" 'code "probe-refused" 'diagnostic (exn-message e)))])
        (id! id)
        (need (equal? id (hash-ref p 'operation-id)) "operation mismatch")
        (define s (hash-ref p 'spec))
        (define c (hash-ref s 'context))
        (check-spec base s)
        (need (equal? (sha256-file (find-executable-path "racket")) (hash-ref p 'racket-sha256))
              "runtime executable changed")
        (need (equal? (sha256-file runner) (hash-ref p 'runner-sha256)) "runner changed")
        (define results
          (for/list ([role '(baseline candidate)])
            (need (guard) "stale guard before launch")
            (define cwd (hash-ref p role))
            (clean cwd)
            (define before (git cwd "rev-parse" "HEAD"))
            (define module (path->string (safe cwd (car (hash-ref s 'test-paths)))))
            (define timeout (* 1000.0 (hash-ref s 'timeout-sec)))
            (define identity
              (verification-identity (hash-ref c 'repo)
                                     (format "W~a" (hash-ref c 'wave))
                                     (format "probe-~a-~a" id role)
                                     cwd
                                     (hash-ref p 'runner-sha256)))
            (define reg (current-gsd-verification-registry))
            (define started
              (parameterize ([current-directory cwd])
                (verification-start! reg
                                     identity
                                     (path->string (find-executable-path "racket"))
                                     (list (path->string runner) module)
                                     #:timeout-ms timeout)))
            (need (start-result-started? started) "not a fresh job")
            (define job (verification-wait reg (start-result-job-id started) timeout))
            (need (guard) "stale guard after wait")
            (need (and (eq? (verification-job-state job) 'completed)
                       (equal? (verification-job-exit-code job) 0)
                       (verification-job-reaped? job)
                       (verification-job-proc-start-ticks job))
                  "nonterminal or failed adapter job")
            (clean cwd)
            (need (equal? before (git cwd "rev-parse" "HEAD")) "checkout changed during job")
            (manifest cwd (append (hash-ref s 'test-files) (hash-ref s 'harness-files)))
            (define raw (file->string (verification-job-log-path job)))
            (define structured (read-structured-output raw))
            (hasheq 'role
                    (symbol->string role)
                    'before
                    before
                    'after
                    before
                    'job
                    (job-facts job)
                    'raw-log
                    (retain (store c) (hasheq 'bytes raw))
                    'structured
                    (retain (store c) structured))))
        (need (guard) "stale guard before retention")
        (define m
          (hasheq 'kind
                  "baseline-probe/v1"
                  'context
                  c
                  'operation-id
                  id
                  'spec
                  (retain (store c) s)
                  'overlay-head
                  (hash-ref p 'overlay-head)
                  'execution-paths
                  (hasheq 'racket
                          (path->string (find-executable-path "racket"))
                          'runner
                          (path->string runner)
                          'work-root
                          (path->string (work-root c id)))
                  'racket-sha256
                  (hash-ref p 'racket-sha256)
                  'runner-sha256
                  (hash-ref p 'runner-sha256)
                  'relation
                  "baseline-with-test-overlay-v1"
                  'chronology
                  "posthoc-reproduction"
                  'results
                  results))
        (define ref (retain (store c) m))
        (validate-baseline-probe! base ref c)
        (need (guard) "stale publication guard")
        (hash-set! publication-guards (hash-ref ref 'sha256) guard)
        ref)))
(define (validate-baseline-probe! root ref c)
  (validate-core root ref c #f))
(define (validate-core root ref c bundle)
  (define (resolve r)
    (if bundle
        (begin
          (need (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref r 'sha256)) "unsafe digest")
          (build-path bundle (string-append (hash-ref r 'sha256) ".json")))
        (hash-ref r 'path)))
  (define (reload r)
    (define p (no-links! (resolve r)))
    (define bytes (file->bytes p))
    (define v (read-json-file p))
    (need (equal? bytes (string->bytes/utf-8 (canonical-evidence-json v)))
          "noncanonical retained bytes")
    (need (equal? (canonical-evidence-digest v) (hash-ref r 'sha256)) "retained digest mismatch")
    v)
  (need (valid-evidence-context? c) "invalid validation context")
  (define (owned-ref! r)
    (unless bundle
      (need (equal? (simplify-path (path->complete-path (hash-ref r 'path)))
                    (simplify-path (path->complete-path
                                    (build-path (store c)
                                                (string-append (hash-ref r 'sha256) ".json")))))
            "reference outside managed retention")
      (need (not (link-exists? (hash-ref r 'path))) "linked retention")))
  (owned-ref! ref)
  (define m (reload ref))
  (owned-ref! (hash-ref m 'spec))
  (need (equal? c (hash-ref m 'context)) "context mismatch")
  (id! (hash-ref m 'operation-id))
  (need (equal? (hash-ref m 'relation) "baseline-with-test-overlay-v1") "unsupported relation")
  (define s (reload (hash-ref m 'spec)))
  (need (valid-probe-spec? s) "invalid retained spec")
  (need (equal? c (hash-ref s 'context)) "spec context mismatch")
  (need (regression-command-covered? root
                                     (hash-ref c 'head)
                                     (hash-ref s 'focused-normalized-command)
                                     (car (hash-ref s 'test-paths)))
        "unapproved frozen regression coverage")
  (unless bundle
    (need (equal? (sha256-file (find-executable-path "racket")) (hash-ref m 'racket-sha256))
          "runtime executable digest mismatch"))
  (need (equal? (sha256-file runner) (hash-ref m 'runner-sha256)) "adapter digest mismatch")
  (when bundle
    (need (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref m 'racket-sha256))
          "invalid recorded runtime hash")
    (need (equal? (sha256-file
                   (no-links! (build-path bundle (string-append (hash-ref m 'runner-sha256) ".rkt"))))
                  (hash-ref m 'runner-sha256))
          "exported runner mismatch"))
  (need (if bundle
            (and (equal? (hash-ref (hash-ref s 'runtime) 'racket-version) (version))
                 (member (hash-ref (hash-ref s 'runtime) 'platform) '("unix" "windows" "macosx")))
            (equal? (hash-ref s 'runtime)
                    (hasheq 'racket-version (version) 'platform (symbol->string (system-type 'os)))))
        "runtime mismatch")
  (define base (hash-ref s 'baseline-head))
  (define head (hash-ref c 'head))
  (define overlay (hash-ref m 'overlay-head))
  (git root "merge-base" "--is-ancestor" base head)
  (need (equal? (git root "rev-parse" (string-append base "^{tree}")) (hash-ref s 'baseline-tree))
        "baseline tree mismatch")
  (need (equal? (git root "rev-parse" (string-append head "^{tree}")) (hash-ref c 'tree))
        "candidate tree mismatch")
  (need (equal? (git root "rev-parse" (string-append overlay "^")) base) "overlay parent mismatch")
  (define files (append (hash-ref s 'test-files) (hash-ref s 'harness-files)))
  (define allowed (map (lambda (e) (hash-ref e 'path)) files))
  (need (pair? (filter (lambda (p) (not (member p allowed)))
                       (string-split (git root "diff" "--name-only" base head) "\n")))
        "no implementation difference")
  (define changed (string-split (git root "diff" "--name-only" base overlay) "\n"))
  (need (andmap (lambda (p) (member p allowed)) changed) "implementation overlay forbidden")
  (for ([e files])
    (define path (hash-ref e 'path))
    (safe root path)
    (for ([commit (list head overlay)])
      (need (regexp-match? #px"^100644 blob " (git root "ls-tree" commit "--" path))
            "test must be regular nonexecutable blob")
      (define blob (git root "rev-parse" (string-append commit ":" path)))
      (need (equal? blob (git root "rev-parse" (string-append head ":" path)))
            "overlay bytes mismatch")
      ;; Hash bytes, not trimmed git stdout.
      (define out (open-output-bytes))
      (need (parameterize ([current-directory root]
                           [current-output-port out])
              (system* (find-executable-path "git") "cat-file" "blob" blob))
            "missing test blob")
      (define tmp (make-temporary-file "probe-blob-~a"))
      (dynamic-wind void
                    (lambda ()
                      (call-with-output-file tmp
                                             (lambda (o) (write-bytes (get-output-bytes out) o))
                                             #:exists 'truncate)
                      (need (equal? (sha256-file tmp) (hash-ref e 'sha256))
                            "test blob digest mismatch"))
                    (lambda () (delete-file tmp)))))
  (define target (hash-ref s 'target))
  (define case-identity #f)
  (need (= (length (hash-ref m 'results)) 2) "missing results")
  (for ([r (hash-ref m 'results)]
        [role '("baseline" "candidate")])
    (need (equal? role (hash-ref r 'role)) "role mismatch")
    (define expected (if (equal? role "baseline") overlay head))
    (need (and (equal? expected (hash-ref r 'before)) (equal? expected (hash-ref r 'after)))
          "snapshot mismatch")
    (define j (hash-ref r 'job))
    (define paths (hash-ref m 'execution-paths #f))
    (when bundle
      (need paths "capture lacks portable execution paths"))
    (define cwd
      (path->string (build-path (if bundle
                                    (hash-ref paths 'work-root)
                                    (work-root c (hash-ref m 'operation-id)))
                                role)))
    (define expected-command
      (sha256-string (string-join (list (if bundle
                                            (hash-ref paths 'racket)
                                            (path->string (find-executable-path "racket")))
                                        (if bundle
                                            (hash-ref paths 'runner)
                                            (path->string runner))
                                        (path->string (build-path cwd
                                                                  (car (hash-ref s 'test-paths)))))
                                  "\u0000")))
    (need (equal? expected-command (hash-ref j 'command-sha256)) "job command digest mismatch")
    (need (equal? (verification-identity-key
                   (verification-identity (hash-ref c 'repo)
                                          (format "W~a" (hash-ref c 'wave))
                                          (format "probe-~a-~a" (hash-ref m 'operation-id) role)
                                          cwd
                                          (hash-ref m 'runner-sha256)))
                  (hash-ref j 'identity))
          "job identity mismatch")
    (owned-ref! (hash-ref r 'raw-log))
    (owned-ref! (hash-ref r 'structured))
    (need (and (equal? (hash-ref j 'state) "completed")
               (= (hash-ref j 'exit) 0)
               (hash-ref j 'reaped)
               (exact-positive-integer? (hash-ref j 'pid))
               (hash-ref j 'start-ticks)
               (<= (hash-ref j 'start-ms) (hash-ref j 'end-ms)))
          "invalid retained job facts")
    (define raw (reload (hash-ref r 'raw-log)))
    (define result (reload (hash-ref r 'structured)))
    (need (equal? (read-structured-output (hash-ref raw 'bytes)) result) "raw structured mismatch")
    (need (and (hash-ref result 'compile-ok)
               (hash-ref result 'load-ok)
               (hash-ref result 'suite-completed)
               (= (hash-ref result 'errors) 0))
          "compile/load/setup failure")
    (define cases (hash-ref result 'cases))
    (need (= (hash-ref result 'successes)
             (count (lambda (x) (equal? (hash-ref x 'status) "success")) cases))
          "success count mismatch")
    (need (= (hash-ref result 'failures)
             (count (lambda (x) (equal? (hash-ref x 'status) "failure")) cases))
          "failure aggregate mismatch")
    (define targets
      (filter (lambda (x)
                (and (equal? (hash-ref x 'test-case-name) (hash-ref target 'test-case-name))
                     (equal? (hash-ref x 'suite-path) (hash-ref target 'suite-path))))
              cases))
    (need (= (length targets) 1) "target missing or ambiguous")
    (define t (car targets))
    (need (equal? (hash-ref target 'test-path) (car (hash-ref s 'test-paths)))
          "target module mismatch")
    (need (= (hash-ref t 'check-count) 1) "target must execute exactly one check")
    (define ident (hash-ref t 'checks))
    (when (equal? role "baseline")
      (need (equal? (hash-ref t 'failure-check) (car ident)) "RED exception identity mismatch"))
    (if case-identity
        (need (equal? ident case-identity) "candidate check identity mismatch")
        (set! case-identity ident))
    (need (equal? (hash-ref (car ident) 'name) (hash-ref target 'check-name)) "check type mismatch")
    (need (equal? (hash-ref (car ident) 'location) (hash-ref target 'check-location))
          "check location mismatch")
    (for ([x cases])
      (need (equal? (hash-ref x 'status)
                    (if (and (eq? x t) (equal? role "baseline")) "failure" "success"))
            "unrelated failure or missing RED"))
    (need (= (hash-ref result 'failures) (if (equal? role "baseline") 1 0)) "failure count mismatch"))
  m)
;; Publish atomically and reuse only an independently revalidated bundle.
;; Prepare/finalize and response-loss recovery may request this exact export.
(define (export-baseline-probe! repo ref c destination)
  (validate-baseline-probe! repo ref c)
  (define target (path->complete-path destination))
  (define (existing)
    (validate-exported-baseline-probe! repo target ref c)
    (read-json-file (build-path target "bundle.json")))
  (cond
    [(directory-exists? target) (existing)]
    [else
     (define parent (path-only target))
     (make-directory* parent)
     (define pending (make-temporary-file "probe-export-pending-~a" 'directory parent))
     (delete-directory pending)
     (dynamic-wind void
                   (lambda ()
                     (export-baseline-probe/raw! repo ref c pending)
                     (validate-exported-baseline-probe! repo pending ref c)
                     (with-handlers ([exn:fail:filesystem? (lambda (e)
                                                             (unless (directory-exists? target)
                                                               (raise e)))])
                       (rename-file-or-directory pending target))
                     (existing))
                   (lambda ()
                     (when (directory-exists? pending)
                       (delete-directory/files pending))))]))
;; Export uses an isolated object database: delivery refs are never modified.
(define (export-baseline-probe/raw! repo ref c destination)
  (define m (validate-baseline-probe! repo ref c))
  (need (hash-has-key? m 'execution-paths) "capture lacks portable execution paths")
  (need (not (directory-exists? destination)) "export destination exists")
  (make-directory* destination)
  (define refs
    (append (list ref (hash-ref m 'spec))
            (append-map (lambda (r) (list (hash-ref r 'raw-log) (hash-ref r 'structured)))
                        (hash-ref m 'results))))
  (for ([r refs])
    (copy-file (hash-ref r 'path)
               (build-path destination (string-append (hash-ref r 'sha256) ".json"))))
  (copy-file runner (build-path destination (string-append (hash-ref m 'runner-sha256) ".rkt")))
  (define tmp (make-temporary-file "probe-export-~a" 'directory))
  (define bundle (path->complete-path (build-path destination "objects.bundle")))
  (dynamic-wind
   void
   (lambda ()
     (git tmp "clone" "--bare" "--shared" (path->string (path->complete-path repo)) "objects.git")
     (define db (build-path tmp "objects.git"))
     (git db "update-ref" "refs/heads/probe-overlay" (hash-ref m 'overlay-head))
     (git db "update-ref" "refs/heads/probe-candidate" (hash-ref c 'head))
     (git db
          "bundle"
          "create"
          (path->string bundle)
          "refs/heads/probe-overlay"
          "refs/heads/probe-candidate"))
   (lambda () (delete-directory/files tmp)))
  (define metadata
    (hasheq 'kind "baseline-probe-export/v1" 'probe ref 'git-bundle-sha256 (sha256-file bundle)))
  (call-with-output-file (build-path destination "bundle.json")
                         (lambda (out) (display (canonical-evidence-json metadata) out)))
  metadata)
(define (validate-exported-baseline-probe! repo destination ref c)
  (no-links! destination)
  (define metadata (read-json-file (no-links! (build-path destination "bundle.json"))))
  (need (equal? (hash-ref metadata 'kind) "baseline-probe-export/v1") "unsupported export")
  (need (equal? ref (hash-ref metadata 'probe)) "export reference mismatch")
  ;; The supplied repository is the candidate trust anchor, not the exported refs.
  (need (equal? (git repo "rev-parse" (string-append (hash-ref c 'head) "^{tree}"))
                (hash-ref c 'tree))
        "trusted candidate mismatch")
  (define bundle (no-links! (path->complete-path (build-path destination "objects.bundle"))))
  (need (equal? (sha256-file bundle) (hash-ref metadata 'git-bundle-sha256)) "bundle digest mismatch")
  (define tmp (make-temporary-file "probe-audit-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (git tmp "init" "--bare")
                  (git tmp "bundle" "verify" (path->string bundle))
                  (git tmp "fetch" (path->string bundle) "refs/heads/*:refs/heads/*")
                  (validate-core tmp ref c destination))
                (lambda () (delete-directory/files tmp))))
(struct authority (base context ref operation guard) #:authentic)
(define (recover-evidence-registration-authorization base c ref id #:guard guard)
  (need (and (procedure? guard) (guard)) "stale recovery guard")
  (define m (validate-baseline-probe! base ref c))
  (need (equal? id (hash-ref m 'operation-id)) "operation mismatch")
  (clean base)
  (need (equal? (git base "rev-parse" "HEAD") (hash-ref c 'head)) "current HEAD guard refused")
  (need (guard) "stale recovery guard")
  (define token (authority base c ref id guard))
  (hash-set! issued token #t)
  token)
(define issued (make-weak-hasheq))
(define (authorize-evidence-registration base c ref id)
  (id! id)
  (define m (validate-baseline-probe! base ref c))
  (need (equal? id (hash-ref m 'operation-id)) "operation mismatch")
  (clean base)
  (need (equal? (git base "rev-parse" "HEAD") (hash-ref c 'head)) "current HEAD guard refused")
  (define guard (hash-ref publication-guards (hash-ref ref 'sha256) #f))
  (need (and guard (guard)) "no current owned publication guard")
  (define token (authority base c ref id guard))
  (hash-set! issued token #t)
  token)
(define (consume-evidence-registration-authorization! token base c ref id)
  (need (and (authority? token)
             (hash-ref issued token #f)
             (equal? (authority-base token) base)
             (equal? (authority-context token) c)
             (equal? (authority-ref token) ref)
             (equal? (authority-operation token) id))
        "invalid capability")
  (need ((authority-guard token)) "stale registration guard")
  (validate-baseline-probe! base ref c)
  (clean base)
  (need (equal? (git base "rev-parse" "HEAD") (hash-ref c 'head)) "stale capability")
  (hash-remove! issued token)
  #t)
