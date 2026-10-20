#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; Real private files, real broker approvals, injected process/storage cuts. No
;; live campaign dispatch, verification, publication or delivery is performed.
(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/port
         racket/system
         "test-gsd-supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt")
(provide crash-suite)

;; Historical author/proof bytes are deliberately outside the new private ledger.
;; They include the original completed/spent marker and its old successor ref.
(define (with-preserved-fixture proc)
  (with-fixture
   (lambda (f)
     (define project (build-path (fixture-workspace f) "project-fixture"))
     (make-directory project)
     (define marker (build-path project "original-author.json"))
     (define green (build-path project "old-green.json"))
     (define receipt (build-path project "old-receipt.json"))
     (define originals
       (list (cons marker
                   #"{\"status\":\"completed\",\"spent\":true,\"successor\":\"old-successor\"}")
             (cons green #"{\"genuine-historical-fixture\":\"GREEN-only-for-C0\"}")
             (cons receipt #"{\"generation\":2,\"receipt\":\"historical-fixture\"}")))
     (for ([item (in-list originals)])
       (call-with-output-file (car item) (lambda (out) (write-bytes (cdr item) out))))
     (define bound
       (struct-copy fixture
                    f
                    [base
                     (hash-set* (fixture-base f)
                                'old-author-marker-path
                                (path->string marker)
                                'old-green-proof-path
                                (path->string green)
                                'old-receipt-path
                                (path->string receipt))]))
     (dynamic-wind
      void
      (lambda () (proc bound))
      (lambda ()
        (for ([item (in-list originals)])
          (check-equal? (file->bytes (car item)) (cdr item) "historical bytes must not change")))))))
(define (fault cut)
  (lambda (at)
    (when (eq? at cut)
      (error 'injected-crash "~a" cut))))
(define (assert-no-relaunch f expected)
  (check-equal? (start-supplemental-operation! (fixture-grant f) (fixture-base f)) expected)
  (check-exn exn:fail?
             (lambda ()
               (transition-supplemental-operation! (fixture-grant f)
                                                   (hash-ref expected 'revision)
                                                   (hash-ref expected 'state)
                                                   'launched
                                                   (hasheq 'dispatch-id "forbidden-relaunch")))))
(define (check-interrupted-transition from to cut)
  (with-preserved-fixture
   (lambda (f)
     (define old (advance-to! f from))
     (define additions (facts-for f to old))
     (parameterize ([current-supplemental-journal-crash-cut (fault cut)])
       (check-exn #rx"injected-crash" (lambda () (step! f to additions))))
     (check-true (file-exists? (operation-path f ".intent.json")))
     (define blocked (load-supplemental-operation (fixture-grant f)))
     (check-equal? (hash-ref blocked 'state) "blocked")
     (check-false (recovery-pending-state-name (hash-ref blocked 'state)))
     (check-equal? (hash-ref blocked 'revision) (+ 2 (hash-ref old 'revision)))
     (check-true (hash-ref blocked 'launch-consumed))
     (check-equal? (hash-ref (hash-ref blocked 'facts) 'block-reason) "interrupted-transition")
     (for ([(key value) (in-hash (hash-ref old 'facts))])
       (check-equal? (hash-ref (hash-ref blocked 'facts) key) value "old capture must survive"))
     (for ([(key value) (in-hash additions)])
       (check-equal? (hash-ref (hash-ref blocked 'facts) key) value "new bound facts must survive"))
     (check-false (file-exists? (operation-path f ".intent.json")))
     (check-equal? (load-supplemental-operation (fixture-grant f)) blocked)
     (assert-no-relaunch f blocked))))
;; LATE cut: the interrupted intent holds fully bound captured facts and a
;; spent launch, so recovery yields a typed pending state and the explicit
;; authenticated same-operation reconciliation completes it exactly once
;; without any new launch, allocation, dispatch or reconstructed descriptor.
(define (check-interrupted-reconcile from to cut [probe 'completed])
  (with-preserved-fixture
   (lambda (f)
     (define old (advance-to! f from))
     (define additions (facts-for f to old))
     (parameterize ([current-supplemental-journal-crash-cut (fault cut)])
       (check-exn #rx"injected-crash" (lambda () (step! f to additions))))
     (check-true (file-exists? (operation-path f ".intent.json")))
     (define target-name (if (symbol? to) (symbol->string to) to))
     (define pending (load-supplemental-operation (fixture-grant f)))
     (define pending-view? (equal? (hash-ref pending 'state) (recovery-pending-state-name to)))
     (if pending-view?
         (begin
           (check-equal? (hash-ref pending 'revision) (hash-ref old 'revision))
           (check-true (hash-ref pending 'launch-consumed))
           (check-equal? (hash-ref (hash-ref pending 'facts) 'pending-recovery) target-name)
           (check-true (hash-has-key? (hash-ref pending 'facts) 'interrupted-intent))
           (for ([(key value) (in-hash (hash-ref old 'facts))])
             (check-equal? (hash-ref (hash-ref pending 'facts) key) value "old capture must survive"))
           ;; While pending, no normal transition is possible at all.
           (check-exn #rx"pending recovery must reconcile"
                      (lambda () (step! f probe))))
         (begin
           ;; A snapshot-written cut already installed the exact intended
           ;; record; the leftover intent still fences normal transitions.
           (check-equal? (hash-ref pending 'state) target-name)
           (check-equal? (hash-ref pending 'revision) (add1 (hash-ref old 'revision)))
           (check-true (hash-ref pending 'launch-consumed))
           (for ([(key value) (in-hash additions)])
             (check-equal? (hash-ref (hash-ref pending 'facts) key) value))
           (check-exn #rx"unrecovered write-ahead intent"
                      (lambda () (step! f probe)))))
     ;; Wrong CAS expectations reconcile nothing.
     (check-exn #rx"stale operation revision"
                (lambda ()
                  (reconcile-supplemental-operation!
                   (fixture-grant f)
                   (add1 (hash-ref pending 'revision))
                   (hash-ref pending 'state))))
     ;; Exact authenticated same-operation continuation.
     (define done
       (reconcile-supplemental-operation!
        (fixture-grant f)
        (hash-ref pending 'revision)
        (hash-ref pending 'state)))
     (check-equal? (hash-ref done 'state) target-name)
     (check-equal? (hash-ref done 'revision) (add1 (hash-ref old 'revision)))
     (for ([(key value) (in-hash additions)])
       (check-equal? (hash-ref (hash-ref done 'facts) key) value "same-operation facts are exact"))
     (check-false (file-exists? (operation-path f ".intent.json")))
     (check-equal? (load-supplemental-operation (fixture-grant f)) done)
     ;; Exactly once: nothing is left to reconcile, and no launch follows.
     (check-exn #rx"no pending recovery"
                (lambda ()
                  (reconcile-supplemental-operation!
                   (fixture-grant f)
                   (hash-ref pending 'revision)
                   (hash-ref pending 'state))))
     (assert-no-relaunch f done))))
(define (check-interrupted-init cut)
  (with-preserved-fixture
   (lambda (f)
     (parameterize ([current-supplemental-journal-crash-cut (fault cut)])
       (check-exn #rx"injected-crash"
                  (lambda () (start-supplemental-operation! (fixture-grant f) (fixture-base f)))))
     (define blocked (start-supplemental-operation! (fixture-grant f) (fixture-base f)))
     (check-equal? (hash-ref blocked 'state) "blocked")
     (check-equal? (hash-ref blocked 'revision) 1)
     (check-true (hash-ref blocked 'launch-consumed))
     (assert-no-relaunch f blocked))))

(define crash-suite
  (test-suite "Supplemental write-ahead crash cuts never reset old or new allowance"
    (test-case "initial intent without snapshot is a permanent typed block"
      (check-interrupted-init 'intent-written))
    (test-case "initial snapshot with pending intent cannot become a fresh task"
      (check-interrupted-init 'snapshot-written))
    (test-case "ambiguous reservation consumes permission; cannot allocate twice"
      (check-interrupted-transition 'proposal-approved 'reserved 'intent-written))
    (test-case "launch intent is durable before any model dispatch"
      (check-interrupted-transition 'reserved 'launched 'intent-written))
    (test-case "launch snapshot with pending intent cannot authorize redispatch"
      (check-interrupted-transition 'reserved 'launched 'snapshot-written))
    (test-case "candidate sealing crash preserves dispatch/allocation identity"
      (check-interrupted-transition 'launched 'candidate-sealed 'intent-written))
    (test-case "verify admission crash records job descriptor but never calls it proof"
      (check-interrupted-transition 'candidate-approved 'verifying 'snapshot-written))
    (test-case "GREEN capture intent preserves the original captured descriptor"
      (check-interrupted-reconcile 'verifying 'green-captured 'intent-written 'probing))
    (test-case "probe admission crash never creates a second model or verification job"
      (check-interrupted-transition 'green-captured 'probing 'intent-written))
    (test-case "probe capture snapshot keeps GREEN and probe descriptors unchanged"
      (check-interrupted-transition 'probing 'probe-captured 'snapshot-written))
    (test-case "supersession intent retains exact private approval linkage"
      (check-interrupted-transition 'probe-captured 'supersession-approved 'intent-written))
    (test-case "publication cut recovery re-installs the exact publication, never republishes"
      (check-interrupted-reconcile 'supersession-approved 'published 'snapshot-written 'receipt-certified))
    (test-case "receipt certification interruption continues with the exact receipt"
      (check-interrupted-reconcile 'published 'receipt-certified 'intent-written 'registered))
    (test-case "registration interruption continues exactly once without any new launch"
      (check-interrupted-reconcile 'receipt-certified 'registered 'snapshot-written 'completed))
    (test-case "completion interruption preserves every retained proof/receipt/registration"
      (check-interrupted-transition 'registered 'completed 'intent-written))
    (test-case "after intent cleanup an exception leaves a committed spent launch, not retry permission"
      (with-preserved-fixture
       (lambda (f)
         (advance-to! f 'reserved)
         (parameterize ([current-supplemental-journal-crash-cut (fault 'intent-cleared)])
           (check-exn #rx"injected-crash" (lambda () (step! f 'launched))))
         (define launched (load-supplemental-operation (fixture-grant f)))
         (check-equal? (hash-ref launched 'state) "launched")
         (check-equal? (hash-ref launched 'revision) 2)
         (check-true (hash-ref launched 'launch-consumed))
         (check-false (file-exists? (operation-path f ".intent.json")))
         (assert-no-relaunch f launched))))
    (test-case "actual process exit after fsynced launch intent recovers blocked without dispatch"
      (with-preserved-fixture (lambda (f)
                                (advance-to! f 'reserved)
                                (define w (worker f "--crash-launch"))
                                (define-values (code output errors) (finish-worker w))
                                (check-equal? code 55 (string-append output errors))
                                (define blocked (load-supplemental-operation (fixture-grant f)))
                                (check-equal? (hash-ref blocked 'state) "blocked")
                                (check-equal? (hash-ref (hash-ref blocked 'facts) 'dispatch-id)
                                              "process-crash")
                                (assert-no-relaunch f blocked))))
    (test-case "live independent OS lock excludes CAS; process death releases kernel lock"
      (with-preserved-fixture
       (lambda (f)
         (advance-to! f 'proposal-approved)
         (define holder (start-lock-holder f))
         (dynamic-wind void
                       (lambda ()
                         (define contender (worker f "--cas" "contender"))
                         (define-values (code output errors) (finish-worker contender))
                         (check-equal? code 3 (string-append output errors))
                         (check-regexp-match #rx"lock busy" output))
                       (lambda ()
                         (subprocess-kill (car holder) #t)
                         (define-values (_code _output _errors) (finish-worker holder))
                         (void)))
         ;; The persistent .lock filename is not itself a stale lock.
         (check-true (file-exists? (operation-path f ".lock")))
         (check-equal? (hash-ref (step! f 'reserved) 'state) "reserved"))))
    (test-case "a second crash during recovery preserves the same blocked revision and facts"
      (for ([cut '(recovery-snapshot-written recovery-intent-cleared)])
        (with-preserved-fixture
         (lambda (f)
           (advance-to! f 'green-captured)
           (parameterize ([current-supplemental-journal-crash-cut (fault 'intent-written)])
             (check-exn #rx"injected-crash" (lambda () (step! f 'probing))))
           (parameterize ([current-supplemental-journal-crash-cut (fault cut)])
             (check-exn #rx"injected-crash"
                        (lambda () (load-supplemental-operation (fixture-grant f)))))
           (define blocked-bytes (file->bytes (operation-path f)))
           (define blocked (load-supplemental-operation (fixture-grant f)))
           (check-equal? (hash-ref blocked 'state) "blocked")
           (check-equal? blocked-bytes (file->bytes (operation-path f)))
           (assert-no-relaunch f blocked)))))
    (test-case "malformed pending intent never replaces the last intact snapshot"
      (with-preserved-fixture
       (lambda (f)
         (advance-to! f 'launched)
         (define bytes (file->bytes (operation-path f)))
         (call-with-output-file (operation-path f ".intent.json")
                                (lambda (out)
                                  (display "{\"next\":{\"state\":\"proposal-approved\"}}" out))
                                #:permissions #o600)
         (check-exn exn:fail? (lambda () (load-supplemental-operation (fixture-grant f))))
         (check-equal? bytes (file->bytes (operation-path f))))))
    (test-case "reconcile crash after the snapshot write resumes the same operation exactly once"
      (with-preserved-fixture
       (lambda (f)
         (advance-to! f 'receipt-certified)
         (parameterize ([current-supplemental-journal-crash-cut (fault 'intent-written)])
           (check-exn #rx"injected-crash" (lambda () (step! f 'registered))))
         (define pending (load-supplemental-operation (fixture-grant f)))
         (check-equal? (hash-ref pending 'state) (recovery-pending-state-name 'registered))
         ;; A crash after the reconcile snapshot write but before the intent
         ;; cleanup installs the state yet leaves the exact-once fence up.
         (parameterize ([current-supplemental-journal-crash-cut (fault 'reconcile-snapshot-written)])
           (check-exn #rx"injected-crash"
                      (lambda ()
                        (reconcile-supplemental-operation!
                         (fixture-grant f)
                         (hash-ref pending 'revision)
                         (hash-ref pending 'state)))))
         (define installed (load-supplemental-operation (fixture-grant f)))
         (check-equal? (hash-ref installed 'state) "registered")
         (check-true (file-exists? (operation-path f ".intent.json")))
         ;; The leftover intent still fences every normal transition.
         (check-exn #rx"unrecovered write-ahead intent" (lambda () (step! f 'completed)))
         ;; Resuming with the SAME pre-crash pending expectations completes
         ;; the identical operation exactly once.
         (define done
           (reconcile-supplemental-operation!
            (fixture-grant f)
            (hash-ref pending 'revision)
            (hash-ref pending 'state)))
         (check-equal? (hash-ref done 'state) "registered")
         (check-equal? (hash-ref done 'revision) (hash-ref installed 'revision))
         (check-false (file-exists? (operation-path f ".intent.json")))
         (check-exn #rx"no pending recovery"
                    (lambda ()
                      (reconcile-supplemental-operation!
                       (fixture-grant f)
                       (hash-ref pending 'revision)
                       (hash-ref pending 'state))))
         (assert-no-relaunch f done))))
    (test-case "revocation cannot interleave with an in-flight journal admission"
      (with-preserved-fixture
       (lambda (f)
         (advance-to! f 'reserved)
         (define at-cut (make-channel))
         (define go (make-semaphore))
         (define outcome (make-channel))
         (define worker
           (thread
            (lambda ()
              (parameterize ([current-supplemental-journal-crash-cut
                              (lambda (at)
                                (when (eq? at 'intent-written)
                                  (channel-put at-cut #t)
                                  (semaphore-wait go)))])
                (channel-put
                 outcome
                 (with-handlers ([exn:fail? (lambda (e) (cons 'error e))])
                   (step! f 'launched)))))))
         (check-true (sync/timeout 10 at-cut))
         ;; The admission critical section excludes revocation: while the
         ;; journal write is paused inside its lock, revocation must wait.
         (define done (make-channel))
         (thread
          (lambda ()
            (with-handlers ([exn:fail? (lambda (e) (channel-put done e))])
              (revoke-approval! (fixture-grant f))
              (channel-put done 'revoked))))
         (check-false (sync/timeout 0.4 done)
                      "revocation must wait for the shared admission/revocation lock")
         (semaphore-post go)
         (sync/timeout 10 worker)
         (define launched (sync/timeout 10 outcome))
         ;; The transition won the serialization: it committed launched while
         ;; the approval was still valid, and revocation completed only after.
         (check-equal? (hash-ref launched 'state) "launched")
         (check-true (hash-ref launched 'launch-consumed))
         (check-equal? (sync/timeout 10 done) 'revoked)
         ;; After revocation nothing can read, transition or launch again.
         (check-exn exn:fail? (lambda () (load-supplemental-operation (fixture-grant f))))
         (check-exn #rx"authenticated"
                    (lambda ()
                      (transition-supplemental-operation!
                       (fixture-grant f)
                       (hash-ref launched 'revision)
                       'launched
                       'candidate-sealed
                       (hasheq 'candidate-ref "never")))))))
    (test-case "revocation before recovery leaves intent and historical captures untouched"
      (with-preserved-fixture
       (lambda (f)
         (advance-to! f 'verifying)
         (parameterize ([current-supplemental-journal-crash-cut (fault 'intent-written)])
           (check-exn #rx"injected-crash" (lambda () (step! f 'green-captured))))
         (define old (file->bytes (operation-path f)))
         (define pending (file->bytes (operation-path f ".intent.json")))
         (revoke-approval! (fixture-grant f))
         (check-exn exn:fail? (lambda () (load-supplemental-operation (fixture-grant f))))
         (check-equal? old (file->bytes (operation-path f)))
         (check-equal? pending (file->bytes (operation-path f ".intent.json"))))))))
(module+ test
  (exit (run-tests crash-suite)))
