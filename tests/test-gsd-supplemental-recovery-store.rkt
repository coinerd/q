#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; Protected storage tests use the existing genuine interactive broker fixture.
(require rackunit
         racket/file
         racket/port
         racket/runtime-path
         racket/system
         (only-in "test-gsd-supplemental-evidence-journal.rkt"
                  with-fixture
                  fixture-grant
                  fixture-root
                  fixture-workspace
                  task
                  base-binding
                  approve!
                  revoke-approval!
                  operation-path
                  advance-to!
                  step!)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/evidence-repair-format.rkt")
(define allocation (hasheq 'attempt-id "attempt-3" 'attempt-fence 6 'coordinator-fence 6))
(define (launch! grant)
  (start-supplemental-operation! grant base-binding)
  (transition-supplemental-operation! grant
                                      0
                                      "proposal-approved"
                                      "reserved"
                                      (hasheq 'allocation allocation))
  (transition-supplemental-operation! grant
                                      1
                                      "reserved"
                                      "launched"
                                      (hasheq 'dispatch-id "provider-failed")))
(define (block! grant)
  (transition-supplemental-operation! grant
                                      2
                                      "launched"
                                      "blocked"
                                      (hasheq 'block-reason "quota exhausted")))
(define (pointer proposal approval)
  (hasheq 'schema "supplemental-pointer/v1" 'task-proposal proposal 'task-approval approval))
(define (put path value)
  (display-to-file (canonical-evidence-json value) path #:exists 'truncate)
  (file-or-directory-permissions path #o600))
(define-runtime-path self "test-gsd-supplemental-recovery-store.rkt")
;; Each contender authenticates the real private records in a separate OS process.
(define (claim-worker root path old target proposal approval)
  (define-values (process out in err)
    (subprocess #f #f #f (find-executable-path "racket") self "--claim" (path->string root)))
  (write (list (path->string path) old target proposal approval) in)
  (newline in)
  (flush-output in)
  (unless (equal? (read-line out) "ready")
    (error 'fixture "claim worker did not become ready"))
  (list process out in err))
(define (finish-claim-worker w)
  (define output (port->string (cadr w)))
  (define errors (port->string (list-ref w 3)))
  (subprocess-wait (car w))
  (close-input-port (cadr w))
  (close-input-port (list-ref w 3))
  (values (subprocess-status (car w)) output errors))
(module+ main
  (define args (current-command-line-arguments))
  (when (and (= (vector-length args) 2) (equal? (vector-ref args 0) "--claim"))
    (parameterize ([current-supplemental-authority-root (string->path (vector-ref args 1))])
      (define (freeze v)
        (cond
          [(string? v) (string->immutable-string v)]
          [(hash? v)
           (for/hasheq ([(k value) (in-hash v)])
             (values k (freeze value)))]
          [(list? v) (map freeze v)]
          [else v]))
      (define request (freeze (read)))
      (displayln "ready")
      (flush-output)
      (read-line)
      (read-line)
      (with-handlers ([exn:fail? (lambda (e)
                                   (displayln (exn-message e))
                                   (exit 3))])
        (apply install-supplemental-recovery-claim! request)
        (exit 0)))))
(module+ test
  (test-case "different approved children compete under the real cross-process authority lock"
    (with-fixture
     (lambda (f)
       (define grant (fixture-grant f))
       (launch! grant)
       (block! grant)
       (define-values (parent-ref parent terminal) (inspect-supplemental-recovery-parent grant))
       (define parent-bytes (file->bytes (operation-path f)))
       (define old (pointer parent-ref grant))
       (define path (build-path (fixture-workspace f) "pointer.json"))
       (put path old)
       (define child
         (seal-supplemental-record (hash-set* parent
                                              'kind
                                              "supplemental-evidence-task/v2"
                                              'recovery
                                              (hasheq 'parent-proposal
                                                      parent-ref
                                                      'parent-approval
                                                      grant
                                                      'terminal-sha256
                                                      (hash-ref terminal 'sha256)
                                                      'pointer-sha256
                                                      (supplemental-record-digest old)
                                                      'allocation
                                                      allocation))))
       (define proposals
         (for/list ([offset '(1700 1750)])
           (propose-retain! "task-proposal"
                            (hasheq 'task child 'expires-at (+ (current-seconds) offset)))))
       (define approvals (map approve! proposals))
       (define targets (map pointer proposals approvals))
       (define workers
         (for/list ([proposal proposals]
                    [approval approvals]
                    [target targets])
           (claim-worker (fixture-root f) path old target proposal approval)))
       ;; Barrier: both independent processes are alive before either attempts the CAS.
       (for ([w workers])
         (displayln "go" (list-ref w 2))
         (close-output-port (list-ref w 2)))
       (define statuses
         (for/list ([w workers])
           (define-values (status output errors) (finish-claim-worker w))
           (unless (member status '(0 3))
             (eprintf "claim worker: ~a~a" output errors))
           (check-equal? errors "")
           status))
       (check-equal? (sort statuses <) '(0 3))
       (for ([proposal proposals]
             [approval approvals]
             [target targets]
             [status statuses])
         (if (zero? status)
             (begin
               (check-equal? (install-supplemental-recovery-claim! path old target proposal approval)
                             target)
               (check-false (load-supplemental-operation approval)))
             (check-exn exn:fail?
                        (lambda ()
                          (install-supplemental-recovery-claim! path old target proposal approval)))))
       (check-equal? (file->bytes (operation-path f)) parent-bytes))))
  (test-case "every non-blocked lifecycle state is ineligible and inspected without mutation"
    (for ([state '(proposal-approved reserved
                                     launched
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
                                     completed)])
      (with-fixture (lambda (f)
                      (advance-to! f state)
                      (define bytes (file->bytes (operation-path f)))
                      (check-exn exn:fail?
                                 (lambda () (inspect-supplemental-recovery-parent (fixture-grant f))))
                      (check-equal? (file->bytes (operation-path f)) bytes)))))
  (test-case "blocked without a consumed original dispatch and revoked terminal are ineligible"
    (for ([from '(proposal-approved reserved launched)]
          [to '(blocked blocked revoked)])
      (with-fixture (lambda (f)
                      (advance-to! f from)
                      (step! f to (hasheq 'block-reason "not eligible"))
                      (define bytes (file->bytes (operation-path f)))
                      (check-exn exn:fail?
                                 (lambda () (inspect-supplemental-recovery-parent (fixture-grant f))))
                      (check-equal? (file->bytes (operation-path f)) bytes)))))
  (test-case "historical terminal inspection never reconciles pending WAL or changes raw parent"
    (with-fixture
     (lambda (f)
       (define grant (fixture-grant f))
       (launch! grant)
       (define bytes (file->bytes (operation-path f)))
       (parameterize ([current-supplemental-journal-crash-cut (lambda (cut)
                                                                (when (eq? cut 'intent-written)
                                                                  (error 'cut "pending")))])
         (check-exn exn:fail? (lambda () (block! grant))))
       (define intent (file->bytes (operation-path f ".intent.json")))
       (check-exn exn:fail? (lambda () (inspect-supplemental-recovery-parent grant)))
       (check-equal? (file->bytes (operation-path f)) bytes)
       (check-equal? (file->bytes (operation-path f ".intent.json")) intent))))
  (test-case "v2 lineage is strict; claim and active pointer are required on every journal access"
    (with-fixture
     (lambda (f)
       (define grant (fixture-grant f))
       (launch! grant)
       (block! grant)
       (define-values (parent-ref parent terminal) (inspect-supplemental-recovery-parent grant))
       (define bytes (file->bytes (operation-path f)))
       (define old (pointer parent-ref grant))
       (define path (build-path (fixture-workspace f) "pointer.json"))
       (put path old)
       (define lineage
         (hasheq 'parent-proposal
                 parent-ref
                 'parent-approval
                 grant
                 'terminal-sha256
                 (hash-ref terminal 'sha256)
                 'pointer-sha256
                 (supplemental-record-digest old)
                 'allocation
                 allocation))
       (define child
         (seal-supplemental-record
          (hash-set* parent 'kind "supplemental-evidence-task/v2" 'recovery lineage)))
       (check-true (valid-supplemental-task? child))
       (for ([bad (list (hash-remove lineage 'allocation)
                        (hash-set lineage 'allocation (hash-set allocation 'attempt-fence #f))
                        (hash-set lineage 'extra "not allowed")
                        (hash-set lineage 'parent-approval parent-ref))])
         (check-false (valid-supplemental-task?
                       (seal-supplemental-record (hash-set child 'recovery bad)))))
       (define proposal
         (propose-retain! "task-proposal"
                          (hasheq 'task child 'expires-at (+ (current-seconds) 1800))))
       (define approval (approve! proposal))
       (define target (pointer proposal approval))
       (put path target)
       (check-exn exn:fail? (lambda () (load-supplemental-operation approval)))
       (put path old)
       (check-equal? (install-supplemental-recovery-claim! path old target proposal approval) target)
       (check-false (load-supplemental-operation approval))
       (start-supplemental-operation! approval base-binding)
       (put path old)
       (check-exn exn:fail? (lambda () (load-supplemental-operation approval)))
       (check-exn exn:fail?
                  (lambda ()
                    (transition-supplemental-operation! approval
                                                        0
                                                        "proposal-approved"
                                                        "reserved"
                                                        (hasheq 'allocation allocation))))
       (put path target)
       (check-equal? (hash-ref (load-supplemental-operation approval) 'state) "proposal-approved")
       (check-exn exn:fail? (lambda () (inspect-supplemental-recovery-parent approval)))
       (revoke-approval! approval)
       (check-exn exn:fail? (lambda () (load-supplemental-operation approval)))
       ;; Permanent claim cannot be reassigned even after child revocation.
       (define other
         (propose-retain! "task-proposal"
                          (hasheq 'task child 'expires-at (+ (current-seconds) 1700))))
       (define other-approval (approve! other))
       (check-exn exn:fail?
                  (lambda ()
                    (install-supplemental-recovery-claim! path
                                                          old
                                                          (pointer other other-approval)
                                                          other
                                                          other-approval)))
       (check-equal? (file->bytes (operation-path f)) bytes)
       (revoke-approval! grant)
       (check-exn exn:fail? (lambda () (inspect-supplemental-recovery-parent grant)))
       (check-equal? (file->bytes (operation-path f)) bytes))))
  (test-case "historical expiry exception is narrow and never grants parent execution"
    (with-fixture (lambda (f)
                    (define grant (fixture-grant f))
                    (launch! grant)
                    (block! grant)
                    (define bytes (file->bytes (operation-path f)))
                    (sleep 4)
                    (check-exn exn:fail? (lambda () (load-supplemental-operation grant)))
                    (define-values (_proposal _task terminal)
                      (inspect-supplemental-recovery-parent grant))
                    (check-equal? (hash-ref terminal 'state) "blocked")
                    (check-equal? (file->bytes (operation-path f)) bytes)
                    (revoke-approval! grant)
                    (check-exn exn:fail? (lambda () (inspect-supplemental-recovery-parent grant))))
                  #:payload (hasheq 'task task 'expires-at (+ (current-seconds) 3)))))
