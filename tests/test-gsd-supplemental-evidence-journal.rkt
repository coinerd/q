#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; Storage/CAS tests, not live proof or controller acceptance. All approvals pass
;; through a simulated trusted frontend driving the REAL broker and authority.
(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/effective-verify-binding.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         (only-in "../extensions/gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide journal-suite
         (struct-out fixture)
         with-fixture
         approve!
         revoke-approval!
         task
         base-binding
         advance-to!
         step!
         facts-for
         operation-path
         worker
         start-lock-holder
         finish-worker)

(define (hex n ch)
  (string->immutable-string (make-string n ch)))
(define context
  (hasheq 'plan-id
          (hex 64 #\a)
          'wave
          1
          'attempt-id
          "fixture-old-attempt"
          'attempt-fence
          3
          'receipt-generation
          2
          'receipt-sha256
          (hex 64 #\b)
          'evidence-revision
          0
          'repo
          (string-append "/fixture" "/never-executed")
          'branch
          "fixture"
          'origin
          "https://example.invalid/fixture"
          'head
          (hex 40 #\c)
          'tree
          (hex 40 #\d)
          'snapshot-sha256
          (hex 64 #\e)
          'green-proof-sha256
          (hex 64 #\f)))
(define base-binding
  (hasheq 'context context 'coordinator-fence 7 'old-author-marker-sha256 (hex 64 #\1)))
(define command
  (string-join
   '("raco fmt -i runtime/local.rkt" "raco make runtime/local.rkt"
                                     "racket scripts/run-tests.rkt tests/test-existing.rkt"
                                     "racket scripts/run-tests.rkt --suite fast"
                                     "racket scripts/lint-all.rkt")
   " && "))
(define test-path "tests/test-local-witness.rkt")
(define task
  (seal-supplemental-record (hasheq 'kind
                                    "supplemental-evidence-task/v1"
                                    'context
                                    context
                                    'base-command
                                    command
                                    'effective-command
                                    (make-effective-verify-command command test-path)
                                    'test-path
                                    test-path
                                    'harness-path
                                    "tests/helpers/local-witness.rkt"
                                    'owner-paths
                                    '("README.md")
                                    'baseline-head
                                    (hex 40 #\1)
                                    'baseline-tree
                                    (hex 40 #\2)
                                    'control-plane-head
                                    (hex 40 #\3)
                                    'bounds
                                    (hasheq 'model-dispatches
                                            1
                                            'verify-timeout-sec
                                            14400
                                            'probe-timeout-sec
                                            600
                                            'deadline-sec
                                            3600))))
(struct fixture (root workspace grant base) #:transparent)
(define (interactive-grant ref)
  (define digest (hash-ref ref 'sha256))
  (define id
    (register-approval-request-for-channel!
     (current-approval-channel)
     digest
     (hasheq 'proposal ref 'record (load-supplemental-record ref))))
  (unless id
    (error 'fixture "real broker rejected request"))
  ;; This is the simulated trusted frontend's explicit decision, not a serialized
  ;; approved flag supplied to the authority or journal.
  (unless (approval-decide! id digest #t)
    (error 'fixture "broker rejected decision"))
  (define-values (outcome grant) (approval-await-grant id digest 1000))
  (unless (eq? outcome 'approved)
    (error 'fixture "broker did not produce a grant"))
  grant)
(define (approve! ref)
  (approve-supplemental-proposal! ref (interactive-grant ref)))
(define (revoke-approval! approval)
  (define ref (propose-retain! "revocation-proposal" (hasheq 'reason "fixture") (list approval)))
  (revoke! ref (interactive-grant ref)))
(define (with-fixture proc
                      #:payload [payload (hasheq 'task task 'expires-at (+ (current-seconds) 1800))])
  (when (current-approval-channel)
    (error 'fixture "refusing to replace an active frontend"))
  (define workspace (make-temporary-file "supplemental-journal-test-~a" 'directory))
  (define root (build-path workspace "private"))
  (dynamic-wind (lambda ()
                  (make-directory root #o700)
                  (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
                (lambda ()
                  (parameterize ([current-supplemental-authority-root root])
                    (define grant (approve! (propose-retain! "task-proposal" payload)))
                    (proc (fixture root workspace grant base-binding))))
                (lambda ()
                  (clear-approval-channel!)
                  (delete-directory/files workspace))))
(define (operation-path f [suffix ".json"])
  (build-path (fixture-root f)
              "supplemental-operations"
              (string-append (hash-ref (fixture-grant f) 'sha256) suffix)))
(define (step! f state [facts #f])
  (define old (load-supplemental-operation (fixture-grant f)))
  (transition-supplemental-operation! (fixture-grant f)
                                      (hash-ref old 'revision)
                                      (hash-ref old 'state)
                                      state
                                      (or facts (facts-for f state old))))
(define (facts-for f state old)
  (define facts (hash-ref old 'facts))
  (case (if (symbol? state)
            state
            (string->symbol state))
    [(reserved)
     (hasheq 'allocation
             (hasheq 'attempt-id "fixture-new-attempt" 'attempt-fence 4 'coordinator-fence 8))]
    [(launched) (hasheq 'dispatch-id "fixture-only-dispatch")]
    [(candidate-sealed)
     (hasheq 'candidate-ref
             (propose-retain! "candidate-proposal"
                              (hasheq 'task (hash-ref old 'proposal-ref))
                              (list (hash-ref old 'proposal-ref))))]
    [(candidate-approved) (hasheq 'candidate-approval-ref (approve! (hash-ref facts 'candidate-ref)))]
    [(verifying) (hasheq 'verify-job (hasheq 'id "fixture-verify-job" 'owner "fixture-controller"))]
    [(green-captured)
     (hasheq 'green-proof-ref (hasheq 'kind "fixture-proof-not-live" 'sha256 (hex 64 #\4)))]
    [(probing) (hasheq 'probe-job (hasheq 'id "fixture-probe-job" 'owner "fixture-controller"))]
    [(probe-captured)
     (hasheq 'probe-proof-ref (hasheq 'kind "fixture-probe-not-live" 'sha256 (hex 64 #\5)))]
    [(supersession-approved)
     (define ref
       (propose-retain! "supersession-proposal"
                        (hasheq 'fixture "no publication")
                        (list (hash-ref old 'proposal-ref))))
     (hasheq 'supersession-proposal-ref ref 'supersession-approval-ref (approve! ref))]
    [(published) (hasheq 'publication (hasheq 'fixture-head (hex 40 #\6)))]
    [(receipt-certified)
     (hasheq 'receipt (hasheq 'fixture-generation 3 'fixture-receipt (hex 64 #\7)))]
    [(registered) (hasheq 'registration (hasheq 'fixture-operation "registration-1" 'revision 1))]
    [(completed) (hasheq 'readiness "fixture-only-not-semantic-validation")]
    [else (hasheq)]))
(define sequence
  '(reserved launched
             candidate-sealed
             candidate-approved
             verifying
             green-captured
             probing
             probe-captured
             supersession-approved
             published
             receipt-certified
             registered
             completed))
(define (advance-to! f state)
  (start-supplemental-operation! (fixture-grant f) (fixture-base f))
  (for ([next (in-list sequence)]
        #:break (equal? (hash-ref (load-supplemental-operation (fixture-grant f)) 'state)
                        (if (symbol? state)
                            (symbol->string state)
                            state)))
    (step! f next))
  (load-supplemental-operation (fixture-grant f)))

;; Fresh processes use the real private records, not injected authority modules.
(define-runtime-path self "test-gsd-supplemental-evidence-journal.rkt")
(define (worker f mode . args)
  (define-values (process out in err)
    (apply subprocess
           #f
           #f
           #f
           (find-executable-path "racket")
           self
           mode
           (path->string (fixture-root f))
           (hash-ref (fixture-grant f) 'sha256)
           args))
  (list process out in err))
(define (finish-worker w)
  (close-output-port (list-ref w 2))
  (define output (port->string (list-ref w 1)))
  (define errors (port->string (list-ref w 3)))
  (subprocess-wait (car w))
  (close-input-port (list-ref w 1))
  (close-input-port (list-ref w 3))
  (values (subprocess-status (car w)) output errors))
(define (start-lock-holder f)
  (define w (worker f "--hold-lock"))
  (unless (equal? (read-line (cadr w)) "locked")
    (error 'fixture "worker failed to acquire OS lock"))
  w)

(define journal-suite
  (test-suite "Supplemental private journal: real approvals, one-shot CAS, not proof validation"
    (test-case "initialization pins immutable base, grant, proposal, bounds and expiry"
      (with-fixture
       (lambda (f)
         (define r (start-supplemental-operation! (fixture-grant f) (fixture-base f)))
         (check-true (immutable? r))
         (check-true (immutable? (hash-ref r 'base-binding)))
         (check-equal? (hash-ref r 'state) "proposal-approved")
         (check-equal? (hash-ref r 'revision) 0)
         (check-false (hash-ref r 'launch-consumed))
         (check-true (<= (hash-ref r 'deadline-at) (hash-ref r 'expires-at)))
         (check-equal? (bitwise-and #o7777 (file-or-directory-permissions (operation-path f) 'bits))
                       #o600))))
    (test-case "base context must be exactly the operator-approved task context"
      (with-fixture
       (lambda (f)
         (for ([base
                (list (hasheq 'campaign "caller-selected")
                      (hash-set (fixture-base f) 'context (hash-set context 'attempt-fence 999)))])
           (check-exn #rx"approved task context"
                      (lambda () (start-supplemental-operation! (fixture-grant f) base))))
         (check-false (file-exists? (operation-path f))))))
    (test-case "same grant init is idempotent after launch, never an allowance reset"
      (with-fixture (lambda (f)
                      (define launched (advance-to! f 'launched))
                      (define bytes (file->bytes (operation-path f)))
                      (check-equal? (start-supplemental-operation! (fixture-grant f) (fixture-base f))
                                    launched)
                      (check-equal? bytes (file->bytes (operation-path f)))
                      (check-exn #rx"different base"
                                 (lambda ()
                                   (start-supplemental-operation!
                                    (fixture-grant f)
                                    (hash-set (fixture-base f) 'new-task "renamed")))))))
    (test-case "only exact revision and state advance; malformed revisions refuse"
      (with-fixture (lambda (f)
                      (advance-to! f 'reserved)
                      (for ([revision '(0 -1 1.0 #f)]
                            [state '(reserved reserved reserved proposal-approved)])
                        (check-exn exn:fail?
                                   (lambda ()
                                     (transition-supplemental-operation! (fixture-grant f)
                                                                         revision
                                                                         state
                                                                         'launched
                                                                         (hasheq 'dispatch-id
                                                                                 "wrong")))))
                      (check-equal? (hash-ref (load-supplemental-operation (fixture-grant f)) 'state)
                                    "reserved"))))
    (test-case "finite lifecycle refuses skipping, unknown states, redispatch and reallocation"
      (with-fixture
       (lambda (f)
         (advance-to! f 'launched)
         (for ([to '(reserved launched verifying new-task permanent-approved)])
           (check-exn exn:fail? (lambda () (step! f to (hasheq)))))
         ;; Typed pending states are installed only by reconciliation.
         (for ([to '(recover-pending-verified
                     recover-pending-published
                     recover-pending-receipt-certified
                     recover-pending-registration)])
           (check-exn #rx"pending recovery"
                      (lambda () (step! f to (hasheq)))))
         (check-equal? (hash-ref (load-supplemental-operation (fixture-grant f)) 'revision) 2))))
    (test-case "one exact finite lifecycle stores descriptors but asserts no live proof semantics"
      (with-fixture (lambda (f)
                      (define complete (advance-to! f 'completed))
                      (check-equal? (hash-ref complete 'revision) (length sequence))
                      (check-true (hash-ref complete 'launch-consumed))
                      (check-equal? (hash-ref (hash-ref complete 'facts) 'readiness)
                                    "fixture-only-not-semantic-validation")
                      (check-exn #rx"terminal" (lambda () (step! f 'launched (hasheq)))))))
    (test-case "facts are append-only including allocation, old proof and nested bindings"
      (with-fixture
       (lambda (f)
         (advance-to! f 'green-captured)
         (define original (load-supplemental-operation (fixture-grant f)))
         (for ([key '(allocation green-proof-ref context)])
           (check-exn #rx"cannot change"
                      (lambda () (step! f 'probing (hasheq key "replacement" 'probe-job "fixture")))))
         (check-equal? original (load-supplemental-operation (fixture-grant f))))))
    (test-case "mutable hashes and executable/nonfinite caller data never enter storage"
      (with-fixture (lambda (f)
                      (check-exn exn:fail?
                                 (lambda ()
                                   (start-supplemental-operation! (fixture-grant f)
                                                                  (hash-copy (fixture-base f)))))
                      (advance-to! f 'reserved)
                      (for ([facts (list (make-hasheq)
                                         (hasheq 'dispatch-id +inf.0)
                                         (hasheq 'dispatch-id (lambda () #t))
                                         (hasheq 'dispatch-id (make-hasheq)))])
                        (check-exn exn:fail? (lambda () (step! f 'launched facts)))))))
    (test-case "caller strings are copied; mutation cannot rewrite bound facts"
      (with-fixture (lambda (f)
                      (advance-to! f 'reserved)
                      (define id (string-copy "dispatch-one"))
                      (define r (step! f 'launched (hasheq 'dispatch-id id)))
                      (string-set! id 0 #\X)
                      (check-equal? (hash-ref (hash-ref r 'facts) 'dispatch-id) "dispatch-one")
                      (check-true (immutable? (hash-ref (hash-ref r 'facts) 'dispatch-id))))))
    (test-case "proposal refs, fake grants, caller states and booleans cannot initialize"
      (with-fixture
       (lambda (f)
         (define proposal (hash-ref (load-supplemental-record (fixture-grant f)) 'proposal))
         (for ([fake (list #t
                           (hasheq 'approved #t)
                           proposal
                           (hasheq 'kind "task-approval" 'sha256 (hex 64 #\0))
                           (hasheq 'state "proposal-approved" 'revision 0))])
           (check-exn exn:fail? (lambda () (start-supplemental-operation! fake (fixture-base f)))))
         (check-false (directory-exists? (build-path (fixture-root f) "supplemental-operations"))))))
    (test-case "revocation is checked on every transition and idempotent initialization"
      (with-fixture
       (lambda (f)
         (advance-to! f 'launched)
         (define bytes (file->bytes (operation-path f)))
         (revoke-approval! (fixture-grant f))
         (check-exn #rx"authenticated" (lambda () (step! f 'candidate-sealed)))
         (check-exn #rx"authenticated"
                    (lambda () (start-supplemental-operation! (fixture-grant f) (fixture-base f))))
         (check-equal? bytes (file->bytes (operation-path f))))))
    (test-case "linked candidate approval must be private, exact, and remain unrevoked"
      (with-fixture (lambda (f)
                      (advance-to! f 'candidate-sealed)
                      (check-exn #rx"linked private approval"
                                 (lambda ()
                                   (step! f
                                          'candidate-approved
                                          (hasheq 'candidate-approval-ref (fixture-grant f)))))
                      (define r (step! f 'candidate-approved))
                      (revoke-approval! (hash-ref (hash-ref r 'facts) 'candidate-approval-ref))
                      (check-exn #rx"linked private approval" (lambda () (step! f 'verifying))))))
    (test-case "blocked/revoked are terminal, including after replay initialization"
      (for ([stop '(blocked revoked)])
        (with-fixture
         (lambda (f)
           (advance-to! f 'reserved)
           (define stopped (step! f stop (hasheq 'reason "fixture failure")))
           (check-true (hash-ref stopped 'launch-consumed))
           (check-equal? stopped (start-supplemental-operation! (fixture-grant f) (fixture-base f)))
           (check-exn #rx"terminal" (lambda () (step! f 'launched (hasheq 'dispatch-id "retry"))))))))
    (test-case "task shape, finite bounds and absolute operator expiry are mandatory"
      (for ([payload (list task
                           (hasheq 'task task 'expires-at 0)
                           (hasheq 'task task 'expires-at (- (current-seconds) 1))
                           (hasheq 'task task 'expires-at (+ (current-seconds) (* 10 86400)))
                           (hasheq 'task task 'expires-at "forever")
                           (hasheq 'task task 'expires-at (+ (current-seconds) 60) 'approved #t)
                           (hasheq 'task
                                   (seal-supplemental-record
                                    (hash-set task
                                              'bounds
                                              (hash-set (hash-ref task 'bounds) 'model-dispatches 2)))
                                   'expires-at
                                   (+ (current-seconds) 60)))])
        (with-fixture
         (lambda (f)
           (check-exn #rx"bounded task"
                      (lambda () (start-supplemental-operation! (fixture-grant f) (fixture-base f)))))
         #:payload payload)))
    (test-case "operator expiry blocks transition and prevents grant initialization from extending time"
      (define expiry (+ (current-seconds) 2))
      (with-fixture
       (lambda (f)
         (advance-to! f 'reserved)
         (define bytes (file->bytes (operation-path f)))
         (sleep (max 0 (+ 0.05 (- expiry (/ (current-inexact-milliseconds) 1000)))))
         (check-exn #rx"unexpired operator expiry"
                    (lambda ()
                      (transition-supplemental-operation! (fixture-grant f)
                                                          1
                                                          'reserved
                                                          'launched
                                                          (hasheq 'dispatch-id "expired"))))
         (check-exn #rx"unexpired operator expiry"
                    (lambda () (start-supplemental-operation! (fixture-grant f) (fixture-base f))))
         (check-equal? bytes (file->bytes (operation-path f))))
       #:payload (hasheq 'task task 'expires-at expiry)))
    (test-case "project-supplied root fields are inert facts, never a storage location"
      (with-fixture (lambda (f)
                      (define project (build-path (fixture-workspace f) "project"))
                      (make-directory project)
                      (define base (hash-set (fixture-base f) 'journal-root (path->string project)))
                      (parameterize ([current-directory project])
                        (start-supplemental-operation! (fixture-grant f) base))
                      (check-equal? (directory-list project) '())
                      (check-true (file-exists? (operation-path f))))))
    (test-case "author/model phase cannot consume operation even with a genuine grant"
      (with-fixture (lambda (f)
                      (advance-to! f 'reserved)
                      (parameterize ([current-gsd-author-phase (hasheq 'phase 'test-author)])
                        (check-exn #rx"author phase"
                                   (lambda () (step! f 'launched (hasheq 'dispatch-id "denied")))))
                      (check-equal? (hash-ref (load-supplemental-operation (fixture-grant f)) 'state)
                                    "reserved"))))
    (test-case "no root is inferred from project context or working directory"
      (with-fixture (lambda (f)
                      (for ([root (list #f "relative" (fixture-workspace f))])
                        (parameterize ([current-supplemental-authority-root root])
                          (check-exn exn:fail?
                                     (lambda ()
                                       (start-supplemental-operation! (fixture-grant f)
                                                                      (fixture-base f)))))))))
    (test-case "tampered, public-mode and symlink snapshots refuse without replacement"
      (for ([kind '(tamper mode symlink)])
        (with-fixture (lambda (f)
                        (advance-to! f 'reserved)
                        (define path (operation-path f))
                        (case kind
                          [(tamper)
                           (call-with-output-file path
                                                  (lambda (o) (display "{\"state\":\"launched\"}" o))
                                                  #:exists 'truncate)]
                          [(mode) (file-or-directory-permissions path #o644)]
                          [(symlink)
                           (define moved (build-path (fixture-workspace f) "old"))
                           (rename-file-or-directory path moved)
                           (make-file-or-directory-link moved path)])
                        (check-exn exn:fail?
                                   (lambda () (load-supplemental-operation (fixture-grant f))))))))
    (test-case "symlinked operation directories and unsafe lock files refuse before writing"
      (for ([kind '(directory-symlink lock-symlink lock-mode)])
        (with-fixture
         (lambda (f)
           (advance-to! f 'reserved)
           (case kind
             [(directory-symlink)
              (define dir (build-path (fixture-root f) "supplemental-operations"))
              (define moved (build-path (fixture-workspace f) "moved-operations"))
              (rename-file-or-directory dir moved)
              (make-file-or-directory-link moved dir)]
             [(lock-symlink)
              (define lock (operation-path f ".lock"))
              (define moved (build-path (fixture-workspace f) "moved-lock"))
              (rename-file-or-directory lock moved)
              (make-file-or-directory-link moved lock)]
             [(lock-mode) (file-or-directory-permissions (operation-path f ".lock") #o644)])
           (check-exn exn:fail? (lambda () (step! f 'launched (hasheq 'dispatch-id "denied"))))))))
    (test-case "hardlinked and nonregular snapshots fail closed before reading"
      (for ([kind '(hardlink fifo)])
        (with-fixture (lambda (f)
                        (advance-to! f 'reserved)
                        (define path (operation-path f))
                        (case kind
                          [(hardlink)
                           (check-true (system* (find-executable-path "ln")
                                                path
                                                (build-path (fixture-workspace f) "hardlink")))]
                          [(fifo)
                           (delete-file path)
                           (check-true (system* (find-executable-path "mkfifo") path))
                           (file-or-directory-permissions path #o600)])
                        (check-exn exn:fail?
                                   (lambda () (load-supplemental-operation (fixture-grant f))))))))
    (test-case "concurrent threads get exactly one reservation and one dispatch CAS"
      (with-fixture
       (lambda (f)
         (start-supplemental-operation! (fixture-grant f) (fixture-base f))
         (define results (make-channel))
         (define threads
           (for/list ([i (in-range 8)])
             (thread (lambda ()
                       (with-handlers ([exn:fail? (lambda (_) (channel-put results 'lost))])
                         (transition-supplemental-operation! (fixture-grant f)
                                                             0
                                                             'proposal-approved
                                                             'reserved
                                                             (hasheq 'allocation (hasheq 'id i)))
                         (transition-supplemental-operation! (fixture-grant f)
                                                             1
                                                             'reserved
                                                             'launched
                                                             (hasheq 'dispatch-id (number->string i)))
                         (channel-put results 'dispatch))))))
         (define outcomes
           (for/list ([i (in-range 8)])
             (sync/timeout 10 results)))
         (for-each thread-wait threads)
         (check-equal? (count (lambda (v) (eq? v 'dispatch)) outcomes) 1)
         (check-equal? (hash-ref (load-supplemental-operation (fixture-grant f)) 'state)
                       "launched"))))
    (test-case "same-proposal re-approval is one admissible operation, never a second allowance"
      (with-fixture
       (lambda (f)
         ;; The fixture already approved the task proposal once; a second
         ;; genuine owner decision must return the EXISTING approval identity.
         (define proposal (hash-ref (load-supplemental-record (fixture-grant f)) 'proposal))
         (define grant2 (approve! proposal))
         (check-equal? (hash-ref grant2 'sha256) (hash-ref (fixture-grant f) 'sha256))
         (start-supplemental-operation! (fixture-grant f) (fixture-base f))
         ;; Both identities denote the SAME keyed operation: concurrent CAS
         ;; admits exactly one reservation and one dispatch.
         (define results (make-channel))
         (define threads
           (for/list ([g (in-list (list (fixture-grant f) grant2))]
                      [i (in-naturals)])
             (thread
              (lambda ()
                (with-handlers ([exn:fail? (lambda (_) (channel-put results 'lost))])
                  (transition-supplemental-operation! g 0 'proposal-approved 'reserved
                                                      (hasheq 'allocation (hasheq 'id i)))
                  (transition-supplemental-operation! g 1 'reserved 'launched
                                                      (hasheq 'dispatch-id (number->string i)))
                  (channel-put results 'dispatch))))))
         (define outcomes (for/list ([i (in-range 2)]) (sync/timeout 10 results)))
         (for-each thread-wait threads)
         (check-equal? (count (lambda (v) (eq? v 'dispatch)) outcomes) 1)
         (check-equal? (hash-ref (load-supplemental-operation (fixture-grant f)) 'state) "launched")
         (check-equal?
          (count (lambda (n) (regexp-match? #px"\\.json$" n))
                 (map path->string
                      (directory-list (build-path (fixture-root f) "supplemental-operations"))))
          1))))
    (test-case "independent OS processes cannot both reserve the same grant"
      (with-fixture
       (lambda (f)
         (start-supplemental-operation! (fixture-grant f) (fixture-base f))
         (define a (worker f "--cas" "first"))
         (define b (worker f "--cas" "second"))
         (define-values (ac ao ae) (finish-worker a))
         (define-values (bc bo be) (finish-worker b))
         (check-equal? (sort (list ac bc) <) '(0 3) (string-append ao ae bo be))
         (check-equal? (hash-ref (load-supplemental-operation (fixture-grant f)) 'revision) 1))))))

(module+ main
  (define args (current-command-line-arguments))
  (when (>= (vector-length args) 3)
    (define mode (vector-ref args 0))
    (define root (string->path (vector-ref args 1)))
    (define grant
      (hasheq 'kind "task-approval" 'sha256 (string->immutable-string (vector-ref args 2))))
    (parameterize ([current-supplemental-authority-root root])
      (cond
        [(equal? mode "--hold-lock")
         (define lock
           (build-path root
                       "supplemental-operations"
                       (string-append (hash-ref grant 'sha256) ".lock")))
         (call-with-file-lock/timeout #f
                                      'exclusive
                                      (lambda ()
                                        (displayln "locked")
                                        (flush-output)
                                        (read-byte))
                                      (lambda () (exit 4))
                                      #:lock-file lock)]
        [(equal? mode "--cas")
         (with-handlers ([exn:fail? (lambda (e)
                                      (displayln (exn-message e))
                                      (exit 3))])
           (transition-supplemental-operation! grant
                                               0
                                               'proposal-approved
                                               'reserved
                                               (hasheq 'allocation (hasheq 'id (vector-ref args 3))))
           (exit 0))]
        [(equal? mode "--crash-launch")
         (parameterize ([current-supplemental-journal-crash-cut (lambda (cut)
                                                                  (when (eq? cut 'intent-written)
                                                                    (exit 55)))])
           (transition-supplemental-operation! grant
                                               1
                                               'reserved
                                               'launched
                                               (hasheq 'dispatch-id "process-crash")))]))))
(module+ test
  (exit (run-tests journal-suite)))
