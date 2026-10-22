#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 14400
;; Real private broker, production fresh-worker factory, isolated proof ladder.
(require json
         racket/runtime-path
         (only-in "../extensions/gsd/review-proof.rkt" load-receipt-owned-proof)
         (only-in "../extensions/gsd/evidence-probe.rkt" validate-baseline-probe!)
         rackunit
         rackunit/text-ui
         "helpers/supplemental-disposition-fixture.rkt"
         "helpers/supplemental-go-fixture.rkt"
         "helpers/supplemental-worker-fixture.rkt"
         "helpers/honest-delivery-fixture.rkt"
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/command-handlers.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-publication.rkt"
         "../extensions/gsd/evidence-repair-ledger.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt")
(define-runtime-path controller "../extensions/gsd/supplemental-evidence-controller.rkt")
(define (api name)
  (dynamic-require controller name))
(define (files-under root)
  (for/hash ([p (find-files file-exists? root)])
    (values p (file->bytes p))))
(define (preserved! files)
  (for ([(p bytes) (in-hash files)])
    (check-equal? (file->bytes p) bytes (path->string p))))
(define (go fx factory)
  (define base (supplemental-fixture-root fx))
  (define plan (supplemental-fixture-plan fx))
  (define parsed
    (gsd-plan (list (gsd-wave 1
                              "Supplemental"
                              'pending
                              "STALE IMPLEMENTATION GOAL"
                              '("q/runtime/local.rkt")
                              '()
                              (supplemental-fixture-c0 fx)
                              '()))
              #f
              '()
              '()))
  (execute-campaign-request!
   (make-campaign-request base
                          (load-campaign-record base plan)
                          (lambda (wave) (build-single-wave-prompt base parsed wave))
                          (lambda _ (error 'disposition-campaign "ordinary verifier forbidden"))
                          #:timeout-sec 7200
                          #:delivery-reader (test-delivered-reader)
                          #:delivery-coordinator
                          (lambda _ (error 'disposition-campaign "ordinary coordinator forbidden")))
   (lambda (prompt) ((factory) prompt))))
(define (wave rec)
  (findf (lambda (w) (= (campaign-wave-index w) 1)) (campaign-record-waves rec)))
(define (decide-only kind decisions)
  (lambda (event payload)
    (when (equal? event "supplemental-evidence.approval-requested")
      (define requested (hash-ref payload 'proposal-kind))
      (set-box! decisions (append (unbox decisions) (list requested)))
      (unless (approval-decide! (hash-ref payload 'request-id)
                                (hash-ref payload 'commitment-digest)
                                (equal? kind requested))
        (error 'disposition-campaign "genuine broker decision failed")))))

(define (exercise-spent-disposition)
  (call-with-disposition-fixture
   (lambda (df)
     (define fx (disposition-fixture-fx df))
     (define base (supplemental-fixture-root fx))
     (define plan (supplemental-fixture-plan fx))
     (define parent (disposition-fixture-parent-pointer df))
     (define before (files-under (supplemental-fixture-private fx)))
     (define receipt (file->bytes (delivery-journal-path base plan 1)))
     (define-values (ref _task)
       ((api 'propose-supplemental-disposition!) base
                                                 plan
                                                 1
                                                 (hash-ref (hash-ref parent 'task-proposal) 'sha256)
                                                 (+ (current-seconds) 14400)))
     (define approval (fixture-approve! ref))
     ((api 'install-supplemental-disposition!) base plan 1 ref approval)
     (define dispatches 0)
     ;; Intentionally no edits: a genuinely consumed author invocation, not
     ;; a fabricated journal transition or a positive author/proof stub.
     (define (factory)
       (lambda (_prompt)
         (set! dispatches (add1 dispatches))
         'ok))
     (void (go fx factory))
     (define terminal (load-supplemental-operation approval))
     (check-equal? (hash-ref terminal 'state) "blocked")
     (check-equal? (hash-ref terminal 'revision) 3)
     (check-true (hash-ref terminal 'launch-consumed))
     (check-equal? dispatches 1)
     (define rec (load-campaign-record base plan))
     (define count (campaign-wave-attempt-count (wave rec)))
     (define usage (campaign-usage-summary rec))
     (void (go fx factory))
     (check-equal? dispatches 1)
     (check-equal? (load-supplemental-operation approval) terminal)
     (check-equal? (campaign-wave-attempt-count (wave (load-campaign-record base plan))) count)
     (check-equal? (campaign-usage-summary (load-campaign-record base plan)) usage)
     (for ([proposer (list propose-supplemental-recovery! (api 'propose-supplemental-disposition!))])
       (check-exn exn:fail?
                  (lambda ()
                    (proposer base plan 1 (hash-ref ref 'sha256) (+ (current-seconds) 14400)))))
     (check-equal? (file->bytes (delivery-journal-path base plan 1)) receipt)
     (preserved! before))))

(define (exercise-disposition [cut-state #f] #:second? [second? #f])
  (call-with-disposition-fixture
   (lambda (df)
     (define fx (disposition-fixture-fx df))
     (define base (supplemental-fixture-root fx))
     (define repo (supplemental-fixture-repo fx))
     (define plan (supplemental-fixture-plan fx))
     (define private (supplemental-fixture-private fx))
     (define parent (disposition-fixture-parent-pointer df))
     (define frozen (files-under (build-path base ".planning" "campaigns" plan "plan-snapshot")))
     (define private-before (files-under private))
     (define journal-path (delivery-journal-path base plan 1))
     (define journal-before (file->bytes journal-path))
     (define receipt-before (load-delivery-journal base plan 1))
     (define source-before (file->bytes (build-path repo "runtime/local.rkt")))
     (define rec-before (load-campaign-record base plan))
     (define usage-before (campaign-usage-summary rec-before))
     (define pointer-path (supplemental-pointer-path base plan 1))
     (define pointer-before (file->bytes pointer-path))
     (define failed (hash-ref (disposition-fixture-amendment df) 'allocation))
     (define-values (ref task)
       ((api 'propose-supplemental-disposition!) base
                                                 plan
                                                 1
                                                 (hash-ref (hash-ref parent 'task-proposal) 'sha256)
                                                 (+ (current-seconds) 14400)))
     (check-equal? (hash-ref task 'kind) "supplemental-evidence-task/v3")
     (define parent-task
       (hash-ref (hash-ref (load-supplemental-record (hash-ref parent 'task-proposal)) 'payload)
                 'task))
     (for ([(key value) (in-hash parent-task)]
           #:unless (member key '(kind sha256 control-plane-head disposition)))
       (check-equal? (hash-ref task key) value))
     (check-equal? (file->bytes pointer-path) pointer-before)
     (preserved! private-before)
     (check-equal? (campaign-usage-summary (load-campaign-record base plan)) usage-before)
     (check-equal? (campaign-wave-current-attempt (wave (load-campaign-record base plan)))
                   (campaign-wave-current-attempt (wave rec-before)))
     (define-values (factory observation)
       (make-scripted-worker-factory (build-path base "worker-sessions")
                                     repo
                                     (supplemental-fixture-test-path fx)
                                     (supplemental-fixture-harness-path fx)
                                     #:project-dir (build-path base "q")))
     ;; Retaining a T grants nothing; the spent v2 pointer cannot launch.
     (void (go fx factory))
     (check-equal? (unbox (worker-observation-sessions observation)) '())
     (check-equal? (file->bytes pointer-path) pointer-before)
     (check-exn
      exn:fail?
      (lambda ()
        ((api 'install-supplemental-disposition!) base plan 1 ref (hash-ref parent 'task-approval))))
     (check-equal? (file->bytes pointer-path) pointer-before)
     (check-equal? (length (find-files (lambda (p)
                                         (regexp-match? #rx"supplemental-disposition-.*[.]json$" p))
                                       private))
                   (if second? 1 0))
     (define approval (fixture-approve! ref))
     ((api 'install-supplemental-disposition!) base plan 1 ref approval)
     (define installed-private (files-under private))
     (check-equal? (unbox (worker-observation-sessions observation)) '())
     (check-false (load-supplemental-operation approval))
     (check-equal? (campaign-wave-current-attempt (wave (load-campaign-record base plan)))
                   (campaign-wave-current-attempt (wave rec-before)))
     (define home (build-path base "worker-home"))
     (make-directory home)
     (define env (environment-variables-copy (current-environment-variables)))
     (environment-variables-set! env #"HOME" (path->bytes home))
     (environment-variables-set! env #"PLTUSERHOME" (path->bytes home))
     (parameterize ([current-environment-variables env]
                    [current-gsd-freshness-check
                     (lambda (_) (campaign-freshness "fixture" "fixture" #f #f #f))])
       (void (go fx factory)))
     (check-equal? (length (unbox (worker-observation-sessions observation))) 1)
     (check-equal? (file->string (build-path repo (supplemental-fixture-test-path fx)))
                   (string-replace witness-source "new regression" "provider authored regression"))
     (check-equal? (file->string (build-path repo (supplemental-fixture-harness-path fx)))
                   (string-replace helper-source "(= answer 42)" "(equal? answer 42)"))
     (define op (load-supplemental-operation approval))
     (check-equal? (hash-ref op 'state) "candidate-sealed")
     (define allocation (hash-ref (hash-ref op 'facts) 'allocation))
     (check-equal? (campaign-wave-attempt-count (wave (load-campaign-record base plan)))
                   (add1 (campaign-wave-attempt-count (wave rec-before))))
     (check-not-equal? (hash-ref allocation 'attempt-id) (hash-ref failed 'attempt-id))
     (for ([key '(attempt-fence coordinator-fence)])
       (check-true (> (hash-ref allocation key) (hash-ref failed key))))
     ;; This admission is now on the NEW allocation, not immutable failed fences.
     (define decisions (box '()))
     (define captures 0)
     (define probe-launches '())
     ;; Observe real owned subprocess launches at the security guard
     ;; boundary; the parent guard still runs, and no proof/result is
     ;; replaced. Each R1 has one baseline and one candidate job.
     (define launch-observer
       (make-security-guard (current-security-guard)
                            (lambda (who path modes)
                              (when (and (eq? who 'subprocess)
                                         path
                                         ;; Git fact helpers also isolate processes
                                         ;; with setsid. Count only the actual owned
                                         ;; verification delegate, not those helpers.
                                         (for/or ([frame (in-list (continuation-mark-set->context
                                                                   (current-continuation-marks)))])
                                           (define location (cdr frame))
                                           (and (srcloc? location)
                                                (path? (srcloc-source location))
                                                (equal? (file-name-from-path (srcloc-source location))
                                                        (string->path "verification-job.rkt"))))
                                         (memq 'execute modes)
                                         (equal? (path->string (file-name-from-path path)) "setsid"))
                                (if (regexp-match? #rx"evidence-repairs"
                                                   (path->string (current-directory)))
                                    (set! probe-launches (cons (current-directory) probe-launches))
                                    (set! captures (add1 captures)))))
                            (lambda _ (void))))
     (define snapshot-path
       (build-path private
                   "supplemental-operations"
                   (string-append (hash-ref approval 'sha256) ".json")))
     (define interrupted? #f)
     ;; intent-written is before snapshot replacement. Read the WAL
     ;; directly: this hook holds the journal owner lock. Escape models
     ;; process loss, bypassing the controller's exception/block handler.
     (define (interrupt-at target thunk)
       (let/ec lost
         (parameterize ([current-supplemental-journal-crash-cut
                         (lambda (at)
                           (when (and (eq? at 'intent-written)
                                      (equal?
                                       (hash-ref (hash-ref (call-with-input-file
                                                            (path-replace-extension snapshot-path
                                                                                    #".intent.json")
                                                            read-json)
                                                           'next)
                                                 'state)
                                       target))
                             (set! interrupted? #t)
                             (lost 'interrupted)))])
           (call-with-values thunk list))))
     (define (advance-k)
       (parameterize ([current-security-guard launch-observer])
         (advance-supplemental-evidence! base
                                         plan
                                         1
                                         #:publisher (decide-only "candidate-proposal" decisions))))
     (when (equal? cut-state "green-captured")
       (check-equal? (interrupt-at cut-state advance-k) 'interrupted)
       (check-true interrupted?)
       (check-equal? (hash-ref (load-supplemental-operation approval) 'state)
                     "recover-pending-verified")
       (check-equal? captures 1)
       (check-equal? (file->bytes journal-path) journal-before))
     (define-values (k-stop k-detail) (advance-k))
     (check-equal? k-stop 'paused k-detail)
     (check-equal? (hash-ref (load-supplemental-operation approval) 'state) "probe-captured")
     (check-equal? (unbox decisions) '("candidate-proposal" "supersession-proposal"))
     ;; K did not authorize publication; S is a later separate broker decision.
     (check-equal? (file->bytes journal-path) journal-before)
     (define captured-facts (hash-ref (load-supplemental-operation approval) 'facts))
     (define requests-before (length (unbox (worker-observation-requests observation))))
     (check-true (positive? requests-before))
     (define (advance-s)
       (parameterize ([current-security-guard launch-observer]
                      [current-gsd-approved-head-publisher
                       (lambda (_base _plan _wave source _attempt _fence _coordinator _evidence)
                         (hasheq 'status
                                 "published"
                                 'branch
                                 (hash-ref source 'branch)
                                 'head
                                 (hash-ref source 'head)))])
         (advance-supplemental-evidence! base
                                         plan
                                         1
                                         #:publisher (decide-only "supersession-proposal"
                                                                  decisions))))
     (define late-journal #f)
     (when (equal? cut-state "receipt-certified")
       (check-equal? (interrupt-at cut-state advance-s) 'interrupted)
       (check-true interrupted?)
       (check-equal? (hash-ref (load-supplemental-operation approval) 'state)
                     "recover-pending-receipt-certified")
       ;; Public certification is durable, but promotion has not run at all.
       (set! late-journal (file->bytes journal-path))
       (define certified (load-delivery-journal base plan 1))
       (check-not-equal? late-journal journal-before)
       (check-not-equal? (hash-ref certified 'receipt) (hash-ref receipt-before 'receipt))
       (check-true (valid-delivery-receipt? (hash-ref certified 'receipt)))
       (define predecessor (load-receipt-owned-proof (hash-ref receipt-before 'receipt) base plan 1))
       (check-equal? (hash-ref predecessor 'execution-cwd) (hash-ref (hash-ref task 'context) 'repo))
       (check-true (directory-exists? (hash-ref predecessor 'execution-cwd)))
       (define generation (length (hash-ref certified 'receipt-history)))
       (define repair (load-evidence-repair base plan 1 generation))
       (check-equal? (length (if repair
                                 (hash-ref repair 'registrations)
                                 '()))
                     0)
       ;; Passive predecessor authentication must not relax live v2 admission.
       (check-exn exn:fail?
                  (lambda () (load-supplemental-operation (hash-ref parent 'task-approval))))
       (check-equal? captures 1)
       (check-equal? (length probe-launches) 2)
       (check-equal? (length (unbox (worker-observation-sessions observation))) 1)
       (check-equal? (length (unbox (worker-observation-requests observation))) requests-before)
       (check-equal? (file->bytes journal-path) late-journal)
       (check-not-equal? generation (hash-ref (hash-ref task 'context) 'receipt-generation))
       (check-not-equal? (supplemental-record-digest (hash-ref certified 'receipt))
                         (hash-ref (hash-ref task 'context) 'receipt-sha256))
       ;; Context discovery is intentionally pure: a pending WAL must be
       ;; reconciled by the owner, not silently by this read-only query.
       (check-exn #rx"entry cannot reconcile pending intent"
                  (lambda () (current-evidence-context base plan 1))))
     (when (equal? cut-state "registered")
       (check-equal? (interrupt-at cut-state advance-s) 'interrupted)
       (check-true interrupted?)
       (check-equal? (hash-ref (load-supplemental-operation approval) 'state)
                     "recover-pending-registration")
       ;; This is an actual post-receipt, post-registrar cut, NOT a
       ;; disposition claim/pointer installation cut.
       (set! late-journal (file->bytes journal-path))
       (define certified (load-delivery-journal base plan 1))
       (check-not-equal? (hash-ref certified 'receipt) (hash-ref receipt-before 'receipt))
       (check-equal?
        (length (hash-ref
                 (load-evidence-repair base plan 1 (length (hash-ref certified 'receipt-history)))
                 'registrations))
        1))
     (define-values (done detail) (advance-s))
     (check-equal? done 'completed detail)
     (when late-journal
       (check-equal? (file->bytes journal-path) late-journal)
       (check-not-equal? (current-evidence-context base plan 1) (hash-ref task 'context)))
     (check-equal? captures 1 "cached GREEN must not rerun C1")
     (check-equal? (length probe-launches) 2 "one real baseline/candidate R1 pair")
     (check-equal? (length (remove-duplicates probe-launches)) 2)
     (check-equal? (length (unbox (worker-observation-requests observation))) requests-before)
     (for ([(key value) (in-hash captured-facts)])
       (check-equal? (hash-ref (hash-ref (load-supplemental-operation approval) 'facts) key) value))
     (check-equal? (hash-ref (load-supplemental-operation approval) 'state) "completed")
     (check-equal? (unbox decisions)
                   '("candidate-proposal" "supersession-proposal" "supersession-proposal"))
     (define readiness (evidence-readiness base plan 1))
     (check-equal? (hash-ref readiness 'status) "ready")
     (check-equal? (hash-ref readiness 'code) "registered-probe-ready")
     (define usage-after (campaign-usage-summary (load-campaign-record base plan)))
     (for ([accessor (list usage-summary-attempts-with-usage
                           usage-summary-missing-attempts
                           usage-summary-input-tokens
                           usage-summary-output-tokens
                           usage-summary-total-tokens
                           usage-summary-cost-usd)])
       (define before-value (accessor usage-before))
       (define after-value (accessor usage-after))
       (when (number? before-value)
         (check-true (and (number? after-value) (>= after-value before-value))
                     "spent accounting must not be reset")))
     (define journal-after (load-delivery-journal base plan 1))
     (check-equal? (hash-ref journal-after 'evidence-author-repair)
                   (hash-ref receipt-before 'evidence-author-repair))
     (define generation (length (hash-ref journal-after 'receipt-history)))
     (define registrations (hash-ref (load-evidence-repair base plan 1 generation) 'registrations))
     (check-equal? (length registrations) 1)
     (define registration (car registrations))
     (validate-baseline-probe! (hash-ref (hash-ref registration 'context) 'repo)
                               (hash-ref registration 'probe)
                               (hash-ref registration 'context))
     ;; Authenticate the receipt-owned GREEN, including the physical
     ;; predecessor execution port used by the late recovery path.
     (define owned (load-receipt-owned-proof (hash-ref journal-after 'receipt) base plan 1))
     (check-equal? (hash-ref owned 'command) (supplemental-fixture-c1 fx))
     (check-equal? (hash-ref owned 'argv) (list "/bin/sh" "-c" (supplemental-fixture-c1 fx)))
     (check-equal? generation (add1 (length (hash-ref receipt-before 'receipt-history))))
     (check-equal? (hash-ref journal-after 'receipt-history)
                   (append (hash-ref receipt-before 'receipt-history)
                           (list (hash-ref receipt-before 'receipt))))
     (define promotion (hash-ref (load-supplemental-promotion-intent base plan 1) 'promotion))
     (check-equal? (hash-ref promotion 'task-proposal) ref)
     (check-equal? (hash-ref promotion 'task-approval) approval)
     (check-equal? (hash-ref promotion 'generation) generation)
     (check-equal? (hash-ref promotion 'red-proof-ref) (hash-ref registration 'probe))
     ;; The public journal JSON reader returns mutable strings/hashes;
     ;; private authority references require immutable transport values.
     (define (audit-ref key)
       (define raw (hash-ref promotion key))
       (hasheq 'kind
               (string->immutable-string (hash-ref raw 'kind))
               'sha256
               (string->immutable-string (hash-ref raw 'sha256))))
     (for ([prefix '(candidate supersession)])
       (define proposal (audit-ref (string->symbol (format "~a-proposal" prefix))))
       (define approved (audit-ref (string->symbol (format "~a-approval" prefix))))
       (check-not-false (validate-supplemental-approval approved proposal)))
     (check-equal? (hash-ref owned 'execution-cwd) (hash-ref (hash-ref registration 'context) 'repo))
     (check-equal? (hash-ref owned 'execution-cwd)
                   (hash-ref (load-receipt-owned-proof (hash-ref receipt-before 'receipt) base plan 1)
                             'execution-cwd)
                   "G1 and replay reuse the authenticated predecessor port")
     (define stable-journal (file->bytes journal-path))
     (define-values (again again-detail) (advance-s))
     (check-equal? again 'completed again-detail)
     (check-equal? (file->bytes journal-path) stable-journal)
     (check-equal? (hash-ref (load-evidence-repair base plan 1 generation) 'registrations)
                   registrations)
     (check-equal? captures 1)
     (check-equal? (length probe-launches) 2 "promotion replay cannot launch a probe")
     (check-equal? (length (unbox (worker-observation-requests observation))) requests-before)
     (preserved! frozen)
     (preserved! private-before)
     (check-equal? (file->bytes (build-path repo "runtime/local.rkt")) source-before)
     (define-values (_parent-ref _parent-task parent-terminal)
       (inspect-supplemental-disposition-parent (hash-ref parent 'task-approval)))
     (check-equal? parent-terminal (disposition-fixture-parent-operation df))
     (check-equal? (load-supplemental-operation (hash-ref (disposition-fixture-root-pointer df)
                                                          'task-approval))
                   (disposition-fixture-root-operation df))
     (void (go fx factory))
     (check-equal? (length (unbox (worker-observation-sessions observation))) 1)
     (preserved! installed-private)
     (check-equal? (campaign-usage-summary (load-campaign-record base plan)) usage-after)
     (check-exn exn:fail?
                (lambda ()
                  ((api 'propose-supplemental-disposition!) base
                                                            plan
                                                            1
                                                            (hash-ref ref 'sha256)
                                                            (+ (current-seconds) 14400)))))
   #:second? second?))
(define evidence-suite
  (test-suite "one further disposition through the actual campaign runner"
    (test-case "failed v3 is permanently spent; no fourth generation or redispatch"
      (exercise-spent-disposition))
    (test-case "inert T, one monotonic real worker, separate K/S, C1/G1/R1/P completion"
      (exercise-disposition))
    (test-case "v3 captured verification WAL replays without a second C1 or author"
      (exercise-disposition "green-captured"))
    (test-case "v3 post-receipt registration WAL replays exactly one registration"
      (exercise-disposition "registered"))
    (test-case "v3 certified receipt before promotion resumes from zero registrations to one"
      (exercise-disposition "receipt-certified"))
    (test-case "amendment-2: spent v3 ancestry, real worker and complete K/S proof ladder; no successor"
      (exercise-disposition #f #:second? #t))))
(module+ main
  ;; The explicit slow command exercises the new route once, not five historical
  ;; amendment-1 campaigns as well. raco test retains the full regression suite.
  (exit (run-tests (test-suite "amendment-2 production campaign"
                     (test-case "real amendment-2 ladder"
                       (exercise-disposition #f #:second? #t))))))
(provide evidence-suite)
(module+ test
  (define transcript (open-output-string))
  (define result
    (parameterize ([current-output-port transcript]
                   [current-error-port transcript])
      (run-tests evidence-suite)))
  (define text (get-output-string transcript))
  (display text)
  (check-equal? result 0))
;; Explicit historical baseline reproduction: behavioral CLI refusal, not a missing API.
(module+ baseline
  (define baseline-cli
    (build-path "/tmp/bug88-disposition-baseline" "scripts" "gsd-evidence-replan.rkt"))
  (unless (file-exists? baseline-cli)
    (error 'baseline
           "requires isolated approved-baseline checkout at /tmp/bug88-disposition-baseline"))
  (define output (open-output-string))
  (define code
    (parameterize ([current-output-port output]
                   [current-error-port output])
      (system*/exit-code (find-executable-path "racket")
                         (path->string baseline-cli)
                         "--verb"
                         "propose-disposition")))
  (define text (get-output-string output))
  (fprintf (current-output-port)
           "Behavioral baseline: --verb propose-disposition\nexit: ~a\n~a"
           code
           text)
  (check-equal? code 2)
  (check-regexp-match
   #rx"--verb must be propose[|]propose-recovery[|]authorize[|]inspect[|]status[|]revoke"
   text))
