#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 1200
;; Owner runs this long gate; child gate compiles only. No live records.
(require rackunit
         racket/file
         (only-in "../runtime/settings.rkt" load-project-settings)
         "helpers/supplemental-go-fixture.rkt"
         "helpers/honest-delivery-fixture.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/owned-lane.rkt"
         "../extensions/gsd/wave-runner-port.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd-planning/execution-policy.rkt")
(define (put-pointer path pointer)
  (display-to-file (canonical-evidence-json pointer) path #:exists 'truncate)
  (file-or-directory-permissions path #o600))
(module+ test
  (test-case "genuine W1 recovery child: permanent claim, crash replay, one new dispatch, no fallback"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define repo (supplemental-fixture-repo fx))
       (define plan (supplemental-fixture-plan fx))
       (fixture-finalize-predecessor! fx)
       (define parent-pointer (load-supplemental-pointer root plan 1))
       (define parent-tref (hash-ref parent-pointer 'task-proposal))
       (define parent-aref (hash-ref parent-pointer 'task-approval))
       (define path (supplemental-pointer-path root plan 1))
       (define private (supplemental-fixture-private fx))
       (define op-path
         (build-path private
                     "supplemental-operations"
                     (string-append (hash-ref parent-aref 'sha256) ".json")))
       (define calls 0)
       (define fallback 0)
       (define author #f)
       (define (runner _)
         (set! calls (add1 calls))
         (check-equal? (hash-ref (current-gsd-author-phase) 'phase) 'test-author)
         (when author
           (fixture-author-commit! repo))
         'ok)
       (define (go)
         (run-campaign! root
                        (load-campaign-record root plan)
                        #:isolate? #f
                        #:timeout-sec 30
                        #:delivery-reader (test-delivered-reader)
                        #:delivery-coordinator (lambda _
                                                 (set! fallback (add1 fallback))
                                                 (error 'recovery "ordinary fallback forbidden"))
                        #:runner runner
                        #:verifier (lambda _ (error 'recovery "ordinary verifier forbidden"))))
       ;; A genuine consumed provider/no-edit failure, not a hand-imported op.
       (void (go))
       (check-equal? calls 1)
       (define parent-op (load-supplemental-operation parent-aref))
       (check-equal? (hash-ref parent-op 'state) "blocked")
       (check-equal? (hash-ref parent-op 'revision) 3)
       (define parent-bytes (file->bytes op-path))
       (define receipt-bytes (file->bytes (delivery-journal-path root plan 1)))
       (define historical
         (for/list ([ref (list parent-tref parent-aref)])
           (file->bytes (build-path private (string-append (hash-ref ref 'sha256) ".json")))))
       ;; A real project provider/model selection change outside the source repo
       ;; is loaded, but cannot grant a second invocation on the spent parent.
       (make-directory* (build-path root ".q"))
       (define config-path (build-path root ".q" "config.json"))
       (display-to-file "{\"default-provider\":\"kimi-coding\",\"default-model\":\"kimi-for-coding\"}"
                        config-path
                        #:exists 'truncate)
       (check-equal? (hash-ref (load-project-settings root) 'default-provider) "kimi-coding")
       (check-equal? (hash-ref (load-project-settings root) 'default-model) "kimi-for-coding")
       (void (go))
       (check-equal? calls 1 "provider config change/reentry cannot refresh a spent task")
       (check-equal? fallback 0)
       (check-equal? (file->bytes op-path) parent-bytes)
       (define expiry (+ (current-seconds) 3600))
       (define (propose [sha (hash-ref parent-tref 'sha256)])
         (propose-supplemental-recovery! root plan 1 sha expiry))
       (check-exn exn:fail? (lambda () (propose (make-string 64 #\a))))
       ;; Source drift, cancellation and budget pause are explicit refusals.
       (display-to-file "drift" (build-path repo "drift"))
       (check-exn exn:fail? propose)
       (delete-file (build-path repo "drift"))
       (define rec (load-campaign-record root plan))
       (set-campaign-cancellation! rec (make-campaign-cancellation "cancelled" (current-seconds)))
       (persist-campaign! root rec)
       (check-exn exn:fail? propose)
       (set-campaign-cancellation! rec #f)
       (set-campaign-record-budget-pause!
        rec
        (campaign-budget-pause 'max-cost 1 2 "budget" (current-seconds)))
       (persist-campaign! root rec)
       (check-exn exn:fail? propose)
       (set-campaign-record-budget-pause! rec #f)
       (persist-campaign! root rec)
       (define w1 (cadr (campaign-record-waves rec)))
       (define saved-status (campaign-wave-status w1))
       (for ([status '(pending awaiting-delivery verifying done)])
         (set-campaign-wave-status! w1 status)
         (persist-campaign! root rec)
         (check-exn exn:fail? propose))
       (set-campaign-wave-status! w1 saved-status)
       (define fence (campaign-fence-token rec))
       (set-campaign-fence-token! rec (add1 fence))
       (persist-campaign! root rec)
       (check-exn exn:fail? propose)
       (set-campaign-fence-token! rec fence)
       (persist-campaign! root rec)
       (define campaign-path (build-path root ".planning" "campaigns" (string-append plan ".rktd")))
       (define campaign-bytes (file->bytes campaign-path))
       (define-values (child task) (propose))
       (check-equal? (hash-ref task 'kind) "supplemental-evidence-task/v2")
       (check-not-equal?
        (hash-ref task 'sha256)
        (hash-ref (hash-ref (hash-ref (load-supplemental-record parent-tref) 'payload) 'task)
                  'sha256))
       (define (install ref approval)
         (install-supplemental-recovery! root plan 1 ref approval))
       (check-exn exn:fail? (lambda () (install child #t)))
       (check-exn exn:fail? (lambda () (fixture-interactive-grant child #f)))
       (check-exn exn:fail? (lambda () (install child parent-aref)))
       (define approval (fixture-approve! child))
       ;; Revalidate AFTER approval, not merely at inert proposal creation.
       (define (refuse-install!)
         (check-exn exn:fail? (lambda () (install child approval)))
         (check-equal? (load-supplemental-pointer root plan 1) parent-pointer)
         (check-equal? (file->bytes op-path) parent-bytes)
         (check-equal? (file->bytes (delivery-journal-path root plan 1)) receipt-bytes))
       (define saved-attempt (campaign-wave-current-attempt w1))
       (for ([stale (list (campaign-attempt "foreign-attempt"
                                            (campaign-attempt-fence-token saved-attempt)
                                            0)
                          (campaign-attempt (campaign-attempt-id saved-attempt)
                                            (add1 (campaign-attempt-fence-token saved-attempt))
                                            0))])
         (set-campaign-wave-current-attempt! w1 stale)
         (persist-campaign! root rec)
         (refuse-install!))
       (set-campaign-wave-current-attempt! w1 saved-attempt)
       (set-campaign-cancellation! rec
                                   (make-campaign-cancellation "cancelled after approval"
                                                               (current-seconds)))
       (persist-campaign! root rec)
       (refuse-install!)
       (set-campaign-cancellation! rec #f)
       (set-campaign-record-budget-pause!
        rec
        (campaign-budget-pause 'max-cost 1 2 "budget" (current-seconds)))
       (persist-campaign! root rec)
       (refuse-install!)
       (set-campaign-record-budget-pause! rec #f)
       (persist-campaign! root rec)
       (display-to-file "post-approval drift" (build-path repo "drift"))
       (refuse-install!)
       (delete-file (build-path repo "drift"))
       ;; A changed receipt/context is not rescued by an otherwise valid child grant.
       (define journal (load-delivery-journal root plan 1))
       (save!
        root
        plan
        1
        (hash-set journal 'receipt (hash-set (hash-ref journal 'receipt) 'evidence "stale context")))
       (check-exn exn:fail? (lambda () (install child approval)))
       (check-equal? (load-supplemental-pointer root plan 1) parent-pointer)
       (display-to-file receipt-bytes (delivery-journal-path root plan 1) #:exists 'truncate)
       (check-equal? (file->bytes op-path) parent-bytes)
       (define target
         (hasheq 'schema "supplemental-pointer/v1" 'task-proposal child 'task-approval approval))
       ;; Even genuinely approved, an unclaimed child + hand-edited pointer is inert.
       (put-pointer path target)
       (check-exn exn:fail?
                  (lambda () (start-supplemental-operation! approval (hash-ref task 'context))))
       (check-false (supplemental-author-pending? root plan 1))
       (put-pointer path parent-pointer)
       ;; Scope/context corruption requires new real approval and still cannot install.
       (for ([bad
              (list
               (hash-set task 'owner-paths '())
               (hash-set task 'context (hash-set (hash-ref task 'context) 'branch "foreign"))
               (hash-set task
                         'recovery
                         (hash-set (hash-ref task 'recovery) 'terminal-sha256 (make-string 64 #\a)))
               (hash-set task
                         'recovery
                         (hash-set (hash-ref task 'recovery) 'pointer-sha256 (make-string 64 #\b))))])
         (define bad-ref
           (propose-retain! "task-proposal"
                            (hasheq 'task (seal-supplemental-record bad) 'expires-at expiry)))
         (define bad-approval (fixture-approve! bad-ref))
         (check-exn exn:fail? (lambda () (install bad-ref bad-approval))))
       (define competitor
         (propose-retain! "task-proposal" (hasheq 'task task 'expires-at (add1 expiry))))
       (define competitor-approval (fixture-approve! competitor))
       ;; Cut 1: permanent claim durable, old pointer still current.
       (parameterize ([current-supplemental-journal-crash-cut (lambda (cut)
                                                                (when (eq? cut
                                                                           'recovery-claim-written)
                                                                  (error 'cut "claim")))])
         (check-exn exn:fail? (lambda () (install child approval))))
       (check-equal? (load-supplemental-pointer root plan 1) parent-pointer)
       (check-exn exn:fail? (lambda () (install competitor competitor-approval)))
       (check-exn exn:fail? (lambda () (load-supplemental-operation approval)))
       ;; A foreign pointer cannot be silently repaired by migration retry.
       (put-pointer path (hash-set parent-pointer 'task-proposal competitor))
       (check-exn exn:fail? (lambda () (install child approval)))
       (put-pointer path parent-pointer)
       ;; Cut 2: pointer already new; exact approval replay succeeds, no child init.
       (parameterize ([current-supplemental-journal-crash-cut (lambda (cut)
                                                                (when (eq? cut
                                                                           'recovery-pointer-written)
                                                                  (error 'cut "pointer")))])
         (check-exn exn:fail? (lambda () (install child approval))))
       (check-equal? (install child approval) target)
       (check-false (load-supplemental-operation approval))
       (check-exn exn:fail?
                  (lambda () (write-supplemental-pointer! root plan 1 parent-tref parent-aref)))
       (check-exn exn:fail?
                  (lambda ()
                    (propose-supplemental-recovery! root plan 1 (hash-ref child 'sha256) expiry)))
       (check-equal?
        (file->bytes campaign-path)
        campaign-bytes
        "proposal/migration never reset attempt counters, fences or cumulative accounting")
       (set! author #t)
       (void (go))
       (check-equal? calls 2 "one parent launch plus exactly one new child test-author launch")
       (check-equal? fallback 0)
       (check-equal? (hash-ref (load-supplemental-operation approval) 'state) "candidate-sealed")
       (void (go))
       (check-equal? calls 2 "second /go never redispatches")
       (define live (load-campaign-record root plan))
       (define attempt (campaign-wave-current-attempt (cadr (campaign-record-waves live))))
       (define port
         (make-owned-runner-port root
                                 plan
                                 1
                                 (campaign-attempt-id attempt)
                                 (campaign-attempt-fence-token attempt)
                                 (lambda () (load-campaign-record root plan))
                                 (make-wave-runner-port (lambda (_)
                                                          (set! fallback (add1 fallback))
                                                          (wave-execution-outcome 'done
                                                                                  "forbidden")))))
       (check-equal? (wave-execution-outcome-kind ((gsd-wave-runner-port-run port) 1)) 'failed)
       (check-equal? fallback 0 "internal retry cannot enter ordinary author")
       (check-equal? (file->bytes op-path) parent-bytes)
       (check-equal? (file->bytes (delivery-journal-path root plan 1)) receipt-bytes)
       (for ([ref (list parent-tref parent-aref)]
             [bytes historical])
         (check-equal? (file->bytes (build-path private
                                                (string-append (hash-ref ref 'sha256) ".json")))
                       bytes))
       ;; Parent revocation remains effective for every subsequent child admission.
       (define rev
         (propose-retain! "revocation-proposal" (hasheq 'reason "stop recovery") (list parent-aref)))
       (revoke! rev (fixture-interactive-grant rev))
       (check-exn exn:fail? (lambda () (load-supplemental-operation approval)))
       (check-false (supplemental-author-pending? root plan 1))
       (check-exn exn:fail? (lambda () (install child approval)))
       (void (go))
       (check-equal? calls 2 "revoked lineage cannot dispatch a child")
       (check-equal? fallback 0 "revocation never enables ordinary fallback")
       (check-equal? (file->bytes (delivery-journal-path root plan 1)) receipt-bytes)
       (for ([ref (list parent-tref parent-aref)]
             [bytes historical])
         (check-equal? (file->bytes (build-path private
                                                (string-append (hash-ref ref 'sha256) ".json")))
                       bytes))
       (check-equal? (file->bytes op-path) parent-bytes))
     #:completed-author? #t
     #:physical-worktree? #t)))
