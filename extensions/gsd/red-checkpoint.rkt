#lang racket/base
;; Preimplementation test-checkpoint data, deliberately NOT a GREEN receipt.
;; Structural validity is not owned assertion evidence or dispatch permission.
;; The checkpoint owner authenticates a test-only commit and both structured
;; assertion failure and declared owned RED. No passing receipt is constructed.
(require json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "review-proof.rkt"
         "delivery-receipt.rkt"
         (only-in "delivery-verifier.rkt"
                  current-gsd-delivery-branch-context
                  branch-delivery-context-ref)
         "composition-root.rkt"
         "verification-job.rkt"
         "evidence-repair-format.rkt"
         "evidence-runner-coverage.rkt"
         (rename-in "author-scope.rkt" [frozen-author-files frozen-files])
         "../../util/json/checksum.rkt"
         "../../scripts/gsd-evidence-probe-runner.rkt")
(provide valid-red-checkpoint?
         capture-red-checkpoint!
         validate-red-checkpoint!
         begin-red-test-author!
         begin-red-implementation!
         verify-red-checkpoint-green!
         frozen-author-files)

(define-runtime-path adapter "../../scripts/gsd-evidence-probe-runner.rkt")
(define (need value message)
  (unless value
    (error 'red-checkpoint "~a" message))
  value)
(define (git repo . args)
  (define out (open-output-string))
  (need (parameterize ([current-directory repo]
                       [current-output-port out]
                       [current-error-port (open-output-nowhere)])
          (apply system* (find-executable-path "git") args))
        (format "Git refused ~s" args))
  (string-trim (get-output-string out)))
(define (clean repo)
  (need (equal? (git repo "status" "--porcelain" "--untracked-files=all") "")
        "dirty execution checkout")
  (need (committed-delivery-snapshot repo) "missing committed source"))
(define frozen-author-files frozen-files)
(define (begin-red-test-author! base plan wave repo attempt fence #:guard guard)
  (locked base
          plan
          wave
          attempt
          fence
          (lambda (root)
            (need (guard) "stale guard before test-author reservation")
            (define prior (load-state root))
            (cond
              [prior (validate-at root base plan wave repo attempt fence guard)]
              [else
               (define source (clean repo))
               (define frozen (frozen-verify-binding base plan wave))
               (define files (frozen-files base plan wave))
               (need (ormap (lambda (path) (string-prefix? path "tests/")) files)
                     "frozen FILES cannot authorize test authorship")
               (need (guard) "stale guard before test-author intent")
               (define intent
                 (hasheq 'status
                         "test-author-started"
                         'baseline-source
                         source
                         'phase-frozen
                         frozen
                         'scope-files
                         files))
               (save root intent)
               intent]))))
(define (regular-test repo head path)
  (need (and (regexp-match? #px"^tests/[A-Za-z0-9_/-]+[.]rkt$" path)
             (regexp-match? #px"^100644 blob [0-9a-f]{40}\t" (git repo "ls-tree" head "--" path)))
        "test is not a regular committed Racket file")
  (for ([n (in-range 1 (add1 (length (string-split path "/"))))])
    (need (not (link-exists? (apply build-path repo (take (string-split path "/") n))))
          "linked test path"))
  (hasheq 'path path 'sha256 (sha256-file (build-path repo path))))
(define (changed repo from to)
  (string-split (git repo "diff" "--name-only" "--no-renames" from to "--") "\n"))
(define (check-history repo from to allowed)
  (git repo "merge-base" "--is-ancestor" from to)
  (for ([commit (in-list (string-split (git repo "rev-list" (string-append from ".." to)) "\n"))])
    (for ([path
           (in-list
            (string-split
             (git repo "diff-tree" "-m" "--no-commit-id" "--name-only" "--no-renames" "-r" commit)
             "\n"))])
      (need (member path allowed) (format "phase changed forbidden path ~s" path)))))
(define (location-key base plan wave attempt fence)
  (need (and (hex? plan 64)
             (exact-nonnegative-integer? wave)
             (nonblank? attempt)
             (exact-nonnegative-integer? fence))
        "invalid attempt identity")
  (define root
    (build-path (simplify-path (build-path (current-owned-proof-root) 'up))
                "red-checkpoints"
                plan
                (format "W~a" wave)
                (sha256-string
                 (format "~a:~a:~a" (path->string (path->complete-path base)) attempt fence))))
  root)
(define (locked base plan wave attempt fence thunk)
  (define root (location-key base plan wave attempt fence))
  (for ([path (list root (build-path root "checkpoint.json") (build-path root "checkpoint.lock"))])
    (need (not (link-exists? path)) "linked checkpoint resource"))
  (make-directory* root)
  (call-with-file-lock/timeout #f
                               'exclusive
                               (lambda () (thunk root))
                               (lambda () (error 'red-checkpoint "checkpoint busy"))
                               #:lock-file (build-path root "checkpoint.lock")))
(define (save root value)
  (call-with-atomic-output-file
   (build-path root "checkpoint.json")
   (lambda (out tmp) (write-json (hash-set value 'sha256 (canonical-evidence-digest value)) out))))
(define (load-state root)
  (define path (build-path root "checkpoint.json"))
  (and (file-exists? path)
       (let ([value (call-with-input-file path read-json)])
         (need (and (hash? value)
                    (equal? (hash-ref value 'sha256 #f) (canonical-evidence-digest value)))
               "checkpoint integrity mismatch")
         value)))
(define (structured raw)
  (define in (open-input-string raw))
  (define result (read-json in))
  (need (and (hash? result)
             (regexp-match? #px"^[ \t\r\n]*$" (port->string in))
             (equal? (hash-ref result 'kind #f) "structured-probe-result/v1")
             (eq? (hash-ref result 'compile-ok #f) #t)
             (eq? (hash-ref result 'load-ok #f) #t)
             (eq? (hash-ref result 'suite-completed #f) #t)
             (equal? (hash-ref result 'errors #f) 0)
             (exact-positive-integer? (hash-ref result 'failures #f)))
        "not a completed behavioral failure")
  (need (for/or ([case (in-list (hash-ref result 'cases '()))])
          (and (equal? (hash-ref case 'status #f) "failure")
               (member (hash-ref case 'failure-check #f) (hash-ref case 'checks '()))))
        "failure has no literal assertion identity")
  result)
(define (validate-at root base plan wave repo attempt fence guard)
  (need (guard) "stale live guard")
  (define state (load-state root))
  (need (and state (equal? (hash-ref state 'status #f) "red-ready"))
        "no completed RED checkpoint; do not relaunch")
  (define checkpoint (hash-ref state 'checkpoint))
  (need (valid-red-checkpoint? checkpoint) "invalid checkpoint")
  (define source (clean repo))
  (define frozen (frozen-verify-binding base plan wave))
  (define files (frozen-files base plan wave))
  (need (and (equal? (hash-ref checkpoint 'plan-id) plan)
             (equal? (hash-ref checkpoint 'wave) wave)
             (equal? (hash-ref checkpoint 'attempt-id) attempt)
             (equal? (hash-ref checkpoint 'attempt-fence) fence)
             (equal? (hash-ref checkpoint 'repo)
                     (path->string (simplify-path (path->complete-path repo))))
             (equal? (hash-ref checkpoint 'branch) (hash-ref source 'branch))
             (equal? (hash-ref checkpoint 'origin) (hash-ref source 'origin))
             (equal? (hash-ref checkpoint 'snapshot-sha256) (hash-ref frozen 'snapshot-sha256))
             (equal? (hash-ref checkpoint 'frozen-files) files)
             (member (hash-ref checkpoint 'selected-verify)
                     (owned-chain-stages (hash-ref frozen 'normalized-command)))
             (equal? (hash-ref state 'adapter-sha256) (sha256-file adapter))
             (equal? (hash-ref state 'racket-sha256) (sha256-file (find-executable-path "racket"))))
        "checkpoint authority drift")
  (define tests (hash-ref checkpoint 'test-files))
  (for ([entry (in-list tests)])
    (need (equal? entry (regular-test repo (hash-ref source 'head) (hash-ref entry 'path)))
          "tests changed after RED"))
  (check-history repo
                 (hash-ref checkpoint 'checkpoint-head)
                 (hash-ref source 'head)
                 (filter (lambda (path) (not (string-prefix? path "tests/"))) files))
  (define raw (build-path root "adapter.log"))
  (need (and (file-exists? raw)
             (not (link-exists? raw))
             (equal? (sha256-file raw) (hash-ref state 'raw-sha256)))
        "structured RED raw bytes changed")
  (structured (file->string raw))
  (define ref (hash-ref state 'red))
  (define binding (hash-ref ref 'binding))
  (for ([key '(plan-id wave attempt-id attempt-fence snapshot-sha256 branch origin)])
    (need (equal? (hash-ref binding key #f) (hash-ref checkpoint key))
          "RED binding differs from checkpoint"))
  (need (equal? (hash-ref binding 'repo) (hash-ref source 'repo))
        "RED repository binding differs from checkpoint")
  (need (and (equal? (hash-ref binding 'head #f) (hash-ref checkpoint 'checkpoint-head))
             (equal? (hash-ref binding 'tree #f) (hash-ref checkpoint 'checkpoint-tree)))
        "RED source binding differs from checkpoint")
  (define red
    (load-owned-red-proof (build-path (current-owned-proof-root) plan)
                          ref
                          (hash-ref ref 'binding)
                          #:command (hash-ref state 'command)
                          #:focused-normalized (hash-ref checkpoint 'selected-verify)))
  (need (equal? (simplify-path (path->directory-path (hash-ref red 'execution-cwd)))
                (simplify-path (path->directory-path (hash-ref checkpoint 'repo))))
        "RED execution checkout differs from checkpoint")
  (need (guard) "stale guard after validation")
  state)
(define (validate-red-checkpoint! base plan wave repo attempt fence #:guard guard)
  (locked base
          plan
          wave
          attempt
          fence
          (lambda (root) (validate-at root base plan wave repo attempt fence guard))))
;; RED permits one implementation dispatch, not an unbounded model retry.
;; Persist before launch; missing/lost response is an honest stop on replay.
(define (begin-red-implementation! base plan wave repo attempt fence #:guard guard)
  (locked base
          plan
          wave
          attempt
          fence
          (lambda (root)
            (define state (validate-at root base plan wave repo attempt fence guard))
            (need (not (hash-has-key? state 'implementation-launch))
                  "implementation already launched; ambiguous dispatch is not repeated")
            (define source (clean repo))
            (need (guard) "stale guard before implementation intent")
            (define next
              (hash-set state 'implementation-launch (hasheq 'status "started" 'source source)))
            (save root next)
            next)))

(define (capture-red-checkpoint! base plan wave repo baseline attempt fence #:guard guard)
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (hasheq 'status "blocked" 'code "red-checkpoint-refused" 'diagnostic (exn-message e)))])
    (locked base
            plan
            wave
            attempt
            fence
            (lambda (root)
              (define prior (load-state root))
              (define author-start?
                (and prior (equal? (hash-ref prior 'status #f) "test-author-started")))
              (cond
                [(and prior (not author-start?))
                 (define result (validate-at root base plan wave repo attempt fence guard))
                 (need (equal? baseline (hash-ref (hash-ref result 'checkpoint) 'baseline-head))
                       "baseline changed")
                 result]
                [else
                 (need (guard) "stale guard before checkpoint")
                 (define source (clean repo))
                 (define head (hash-ref source 'head))
                 (need (and (hex? baseline 40) (not (equal? baseline head))) "no test-author commit")
                 (define frozen (frozen-verify-binding base plan wave))
                 (define files (frozen-files base plan wave))
                 (when author-start?
                   (define before (hash-ref prior 'baseline-source))
                   (need (and (equal? baseline (hash-ref before 'head))
                              (equal? frozen (hash-ref prior 'phase-frozen))
                              (equal? files (hash-ref prior 'scope-files))
                              (for/and ([key '(repo branch origin)])
                                (equal? (hash-ref before key) (hash-ref source key))))
                         "test-author reservation authority changed"))
                 (define tests (filter (lambda (path) (string-prefix? path "tests/")) files))
                 (need (pair? tests) "frozen FILES has no tests")
                 (check-history repo baseline head tests)
                 (need (pair? (changed repo baseline head)) "test-author made no progress")
                 (define entries (map (lambda (path) (regular-test repo head path)) tests))
                 (define normalized (hash-ref frozen 'normalized-command))
                 (define selected
                   (for*/first ([stage (in-list (or (owned-chain-stages normalized) '()))]
                                [path (in-list tests)]
                                #:when (and (owned-red-focused-command? stage)
                                            (regression-command-covered? repo head stage path)))
                     (cons stage path)))
                 (need selected "frozen Verify does not cover an eligible test")
                 (define module (build-path repo (cdr selected)))
                 (validate-regression-module! module #:bindings? #f)
                 (define checkpoint
                   (hasheq 'schema
                           "red-checkpoint/v1"
                           'plan-id
                           plan
                           'wave
                           wave
                           'attempt-id
                           attempt
                           'attempt-fence
                           fence
                           'snapshot-sha256
                           (hash-ref frozen 'snapshot-sha256)
                           'repo
                           (path->string (simplify-path (path->complete-path repo)))
                           'branch
                           (hash-ref source 'branch)
                           'origin
                           (hash-ref source 'origin)
                           'baseline-head
                           baseline
                           'baseline-tree
                           (git repo "rev-parse" (string-append baseline "^{tree}"))
                           'checkpoint-head
                           head
                           'checkpoint-tree
                           (hash-ref source 'tree)
                           'frozen-files
                           files
                           'selected-verify
                           (car selected)
                           'test-files
                           entries))
                 (need (valid-red-checkpoint? checkpoint) "invalid captured checkpoint")
                 ;; Durable launch intent is written BEFORE any child starts. An
                 ;; interrupted intent pauses; restart never spends a second launch.
                 (save root (hasheq 'status "intent" 'checkpoint checkpoint))
                 (need (guard) "stale guard before structured RED")
                 (define registry (current-gsd-verification-registry))
                 (define identity
                   (verification-identity (path->string (path->complete-path base))
                                          (format "W~a" wave)
                                          (format "red-checkpoint-~a-~a" attempt fence)
                                          (path->string (path->complete-path repo))
                                          (sha256-file adapter)))
                 (define started
                   (parameterize ([current-directory repo])
                     (verification-start! registry
                                          identity
                                          (path->string (find-executable-path "racket"))
                                          (list (path->string adapter)
                                                (path->string (path->complete-path module)))
                                          #:timeout-ms 60000.0)))
                 (need (start-result-started? started) "structured job is not fresh")
                 (define job (verification-wait registry (start-result-job-id started) 60000.0))
                 (need (and (eq? (verification-job-state job) 'completed)
                            (equal? (verification-job-exit-code job) 0)
                            (verification-job-reaped? job)
                            (verification-job-proc-start-ticks job))
                       "structured RED job incomplete")
                 (need (and (guard) (equal? source (clean repo)))
                       "source or live authority changed during RED")
                 (define raw (file->string (verification-job-log-path job)))
                 (structured raw)
                 (call-with-atomic-output-file (build-path root "adapter.log")
                                               (lambda (out tmp) (display raw out)))
                 (define-values (red-job ref)
                   (run-owned-red-verification base
                                               plan
                                               wave
                                               attempt
                                               fence
                                               repo
                                               (car selected)
                                               #:guard guard
                                               #:origin "preimplementation"
                                               #:timeout-sec 600))
                 (need ref "declared focused Verify did not reproduce behavioral RED")
                 (need (guard) "stale guard before RED registration")
                 (define proof
                   (load-owned-red-proof
                    (build-path (current-owned-proof-root) plan)
                    ref
                    (hash-ref ref 'binding)
                    #:command (expand-frozen-command
                               (car selected)
                               (path->string (simplify-path (path->complete-path base)))
                               (path->string (simplify-path (path->complete-path repo)))
                               (and (current-gsd-delivery-branch-context)
                                    (branch-delivery-context-ref (current-gsd-delivery-branch-context)
                                                                 'worktree-path)
                                    #t)
                               #:jobs #f)
                    #:focused-normalized (car selected)))
                 (define result
                   (hasheq 'status
                           "red-ready"
                           'checkpoint
                           checkpoint
                           'red
                           ref
                           'command
                           (hash-ref proof 'command)
                           'adapter-sha256
                           (sha256-file adapter)
                           'racket-sha256
                           (sha256-file (find-executable-path "racket"))
                           'raw-sha256
                           (sha256-file (build-path root "adapter.log"))
                           'structured-job
                           (hasheq 'id
                                   (symbol->string (verification-job-id job))
                                   'pid
                                   (verification-job-pid job)
                                   'proc-start-ticks
                                   (verification-job-proc-start-ticks job)
                                   'state
                                   "completed"
                                   'exit-code
                                   0
                                   'reaped
                                   #t)))
                 (save root result)
                 (validate-at root base plan wave repo attempt fence guard)])))))

;; This is candidate assertion evidence, never a full declared Verify receipt.
(define (passing raw)
  (define in (open-input-string raw))
  (define value (read-json in))
  (need (and (hash? value)
             (regexp-match? #px"^[ \t\r\n]*$" (port->string in))
             (equal? (hash-ref value 'kind #f) "structured-probe-result/v1")
             (for/and ([key '(compile-ok load-ok suite-completed)])
               (eq? (hash-ref value key #f) #t))
             (equal? (hash-ref value 'errors #f) 0)
             (equal? (hash-ref value 'failures #f) 0)
             (exact-positive-integer? (hash-ref value 'successes #f)))
        "not a completed candidate success")
  (define cases (hash-ref value 'cases '()))
  (need (and (list? cases)
             (= (length cases) (hash-ref value 'successes))
             (for/and ([case (in-list cases)])
               (and (hash? case)
                    (equal? (hash-ref case 'status #f) "success")
                    (let ([checks (hash-ref case 'checks #f)])
                      (and (list? checks)
                           (pair? checks)
                           (for/and ([check (in-list checks)])
                             (and (hash? check)
                                  (member (hash-ref check 'name #f)
                                          '("check-equal?" "check-eq?"
                                                           "check-eqv?"
                                                           "check-true"
                                                           "check-false"
                                                           "check-not-false"
                                                           "check-="))
                                  (list? (hash-ref check 'location #f)))))))))
        "candidate cases lack actual literal success")
  value)
(define (verify-red-checkpoint-green! base plan wave repo attempt fence #:guard guard)
  (locked base
          plan
          wave
          attempt
          fence
          (lambda (root)
            (define state (validate-at root base plan wave repo attempt fence guard))
            (define source (clean repo))
            (define context
              (hasheq 'checkpoint
                      (hash-ref state 'checkpoint)
                      'source
                      source
                      'adapter-sha256
                      (sha256-file adapter)
                      'racket-sha256
                      (sha256-file (find-executable-path "racket"))))
            (define digest (canonical-evidence-digest context))
            (define raw-path (build-path root "candidate.log"))
            (define prior (hash-ref state 'green #f))
            (cond
              [prior
               (need (and (equal? (hash-ref prior 'status #f) "captured")
                          (equal? (hash-ref prior 'context #f) context)
                          (equal? (hash-ref prior 'context-sha256 #f) digest)
                          (file-exists? raw-path)
                          (not (link-exists? raw-path))
                          (equal? (sha256-file raw-path) (hash-ref prior 'raw-sha256 #f)))
                     "ambiguous or different candidate intent; do not relaunch")
               (passing (file->string raw-path))
               (need (and (guard) (equal? source (clean repo))) "stale candidate reuse")
               state]
              [else
               (define checkpoint (hash-ref state 'checkpoint))
               (define selected (hash-ref checkpoint 'selected-verify))
               (define test
                 (for/first ([entry (in-list (hash-ref checkpoint 'test-files))]
                             #:when (regression-command-covered? repo
                                                                 (hash-ref source 'head)
                                                                 selected
                                                                 (hash-ref entry 'path)))
                   (hash-ref entry 'path)))
               (need test "selected stage covers no eligible test")
               (define intent (hasheq 'status "intent" 'context context 'context-sha256 digest))
               (save root (hash-set state 'green intent))
               (need (guard) "stale guard before candidate launch")
               (define registry (current-gsd-verification-registry))
               (define started
                 (parameterize ([current-directory repo])
                   (verification-start!
                    registry
                    (verification-identity (path->string (path->complete-path base))
                                           (format "W~a" wave)
                                           (format "candidate-~a-~a-~a" attempt fence digest)
                                           (path->string (path->complete-path repo))
                                           (sha256-file adapter))
                    (path->string (find-executable-path "racket"))
                    (list (path->string adapter)
                          (path->string (path->complete-path (build-path repo test))))
                    #:timeout-ms 60000.0)))
               (need (start-result-started? started) "candidate job not fresh")
               (define job (verification-wait registry (start-result-job-id started) 60000.0))
               (need (and (eq? (verification-job-state job) 'completed)
                          (equal? (verification-job-exit-code job) 0)
                          (verification-job-reaped? job)
                          (exact-positive-integer? (verification-job-pid job))
                          (exact-positive-integer? (verification-job-proc-start-ticks job)))
                     "candidate job not completed and reaped")
               (need (and (guard)
                          (equal? source (clean repo))
                          (equal? (hash-ref context 'adapter-sha256) (sha256-file adapter))
                          (equal? (hash-ref context 'racket-sha256)
                                  (sha256-file (find-executable-path "racket"))))
                     "candidate authority changed during execution")
               (define raw (file->string (verification-job-log-path job)))
               (need (not (link-exists? raw-path)) "linked candidate raw")
               (call-with-atomic-output-file raw-path (lambda (out tmp) (display raw out)))
               (passing raw)
               (define result
                 (hash-set state
                           'green
                           (hash-set* intent
                                      'status
                                      "captured"
                                      'raw-sha256
                                      (sha256-file raw-path)
                                      'job
                                      (hasheq 'id
                                              (symbol->string (verification-job-id job))
                                              'pid
                                              (verification-job-pid job)
                                              'proc-start-ticks
                                              (verification-job-proc-start-ticks job)
                                              'command-sha256
                                              (verification-job-command-digest job)
                                              'start-ms
                                              (inexact->exact (floor (verification-job-start-ms job)))
                                              'end-ms
                                              (inexact->exact (floor (verification-job-end-ms job)))
                                              'state
                                              "completed"
                                              'reaped
                                              #t
                                              'exit-code
                                              0))))
               (validate-at root base plan wave repo attempt fence guard)
               (save root result)
               (load-state root)]))))

(define fields
  '(schema plan-id
           wave
           attempt-id
           attempt-fence
           snapshot-sha256
           repo
           branch
           origin
           baseline-head
           baseline-tree
           checkpoint-head
           checkpoint-tree
           frozen-files
           selected-verify
           test-files))
(define (hex? value size)
  (and (string? value) (= (string-length value) size) (regexp-match? #px"^[0-9a-f]+$" value)))
(define (nonblank? value)
  (and (string? value) (positive? (string-length (string-trim value)))))
(define (exact-fields? value expected)
  (and (hash? value)
       (= (hash-count value) (length expected))
       (for/and ([key (in-list expected)])
         (hash-has-key? value key))))

(define (valid-red-checkpoint? value)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (exact-fields? value fields)
         (equal? (hash-ref value 'schema) "red-checkpoint/v1")
         (for/and ([key '(wave attempt-fence)])
           (exact-nonnegative-integer? (hash-ref value key)))
         (for/and ([key '(plan-id snapshot-sha256)])
           (hex? (hash-ref value key) 64))
         (for/and ([key '(baseline-head baseline-tree checkpoint-head checkpoint-tree)])
           (hex? (hash-ref value key) 40))
         (not (equal? (hash-ref value 'baseline-head) (hash-ref value 'checkpoint-head)))
         (not (equal? (hash-ref value 'baseline-tree) (hash-ref value 'checkpoint-tree)))
         (for/and ([key '(attempt-id repo branch origin selected-verify)])
           (nonblank? (hash-ref value key)))
         (literal-author-scope? (hash-ref value 'frozen-files))
         (let ([tests (hash-ref value 'test-files)])
           (and (list? tests)
                (pair? tests)
                (for/and ([entry (in-list tests)])
                  (and (exact-fields? entry '(path sha256))
                       (hex? (hash-ref entry 'sha256) 64)
                       (literal-author-scope? (list (hash-ref entry 'path)))
                       ;; Test-only is a necessary syntactic boundary, not an
                       ;; assertion that a test file cannot change behavior.
                       (string-prefix? (hash-ref entry 'path) "tests/")
                       (member (hash-ref entry 'path) (hash-ref value 'frozen-files))
                       #t))
                (= (length tests)
                   (length (remove-duplicates (map (lambda (entry) (hash-ref entry 'path))
                                                   tests)))))))))
