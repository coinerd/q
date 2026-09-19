#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-coordinator.rkt
;; B2a: deterministic journal-driven delivery stage machine. ONE controller
;; effect per invocation (injectable seam; the default production seam shells
;; scripts/gsd-delivery.py). Every effect re-runs the receipt blocker against
;; durable state, checks cancellation/fence/takeover, records separate
;; delivery usage, and advances the journal one stage on success. Outcomes:
;;   'ok       — exactly one effect ran and the journal advanced one stage
;;   'delivered— terminal: journal was already at the delivered stage
;;   'awaiting-review / 'retryable / 'blocked — typed stop, journal untouched
;; Model or journal completion alone never sets delivered — only controller
;; evidence does. The outer run-campaign! loop re-enters after each step.
(require rackunit
         racket/file
         racket/format
         racket/string
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/delivery-journal.rkt"
                  delivery-stages
                  load-delivery-journal
                  record-delivery-receipt!
                  update-delivery-journal!)
         "../extensions/gsd/delivery-coordinator.rkt")

(define (receipt-head)
  (hasheq 'repo
          "git@github.com:example/q.git"
          'branch
          "campaign/test"
          'origin
          "https://github.com/example/q.git"
          'evidence
          "docs/reports/gsd-wave-evidence/a-w0.rktd"
          'head
          (make-string 40 #\a)
          'tree
          (make-string 40 #\b)
          'attempt-id
          "attempt-1"
          'attempt-fence
          7
          'verified-at
          1789500000))

(define (call-with-campaign count proc)
  (define dir (make-temporary-file "delivery-coordinator-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning/waves"))
     (call-with-output-file
      (build-path dir ".planning/PLAN.md")
      (lambda (out)
        (display "# Plan: Delivery coordinator test\n\n## Waves\n\n" out)
        (for ([i (in-range count)])
          (fprintf out "- [Inbox] W~a: Test → waves/W~a-test.md\n" i i)
          (display-to-file "# Test\n\nGoal: test\n\n## Verify\n\nraco test .\n"
                           (build-path dir ".planning/waves" (format "W~a-test.md" i))))))
     (proc dir (migrate-campaign! dir)))
   (lambda () (delete-directory/files dir))))

(define (done-record dir)
  (define rec (migrate-campaign! dir))
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
  (set-campaign-wave-current-attempt! (car (campaign-record-waves rec))
                                      (campaign-attempt "attempt-1" 7 0))
  (set-campaign-fence-token! rec 7)
  (set-campaign-wave-delivery-branch! (car (campaign-record-waves rec)) "campaign/test")
  (set-campaign-wave-delivery-head-sha! (car (campaign-record-waves rec)) (make-string 40 #\a))
  (persist-campaign! dir rec)
  (record-delivery-receipt! dir (campaign-plan-id rec) 0 (receipt-head))
  rec)

(define (stage-count dir plan)
  (define journal (load-delivery-journal dir plan 0))
  (and journal (hash-ref journal 'stage #f)))

(module+ test
  (test-case "one eligible controller effect per invocation; journal advances one stage at a time"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define calls '())
       (define (controller b p w target)
         (set! calls (cons target calls))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (for ([expected (in-list '("implementation-review" "implementation-pr"
                                                          "implementation-ci"
                                                          "implementation-merged"))])
         (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
         (check-eq? (delivery-outcome-kind outcome) 'ok)
         (check-equal? (stage-count dir plan) expected))
       (check-equal?
        (reverse calls)
        '("implementation-review" "implementation-pr" "implementation-ci" "implementation-merged")))))
  (test-case "terminal delivered stage is reported and never re-executed"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define ready (done-record dir))
                          (define plan (campaign-plan-id ready))
                          (update-delivery-journal! dir plan 0 (hasheq 'stage "delivered"))
                          (define calls 0)
                          (define (controller b p w target)
                            (set! calls (add1 calls))
                            (delivery-effect-result 'ok (hasheq 'stage target)))
                          (define outcome
                            (run-delivery-coordinator! dir plan 0 #:controller controller))
                          (check-eq? (delivery-outcome-kind outcome) 'delivered)
                          (check-equal? calls 0))))
  (test-case "awaiting-review stops immediately and never advances the journal"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define (controller b p w target)
         (delivery-effect-result 'awaiting-review (hasheq 'stage target 'reason "no reviewer")))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'awaiting-review)
       (check-equal? (delivery-outcome-message outcome) "no reviewer")
       (check-equal? (stage-count dir plan) "context-ready"))))
  (test-case "retryable controller failure is a typed stop, not fabricated success"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define (controller b p w target)
         (delivery-effect-result 'retryable (hasheq 'stage target 'reason "CI pending")))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'retryable)
       (check-equal? (delivery-outcome-message outcome) "CI pending")
       (check-equal? (stage-count dir plan) "context-ready"))))
  (test-case "blocker failure refuses any controller call"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define start-rec rec) ; fresh campaign: no DONE wave, no receipt authority yet
       (persist-campaign! dir start-rec)
       (record-delivery-receipt! dir (campaign-plan-id start-rec) 0 (receipt-head))
       (define count 0)
       (define (controller b p w target)
         (set! count (add1 count))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (define outcome
         (run-delivery-coordinator! dir (campaign-plan-id start-rec) 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'blocked)
       (check-equal? count 0))))
  (test-case "takeover during the effect rejects the stale continuation"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define ready (done-record dir))
                          (define plan (campaign-plan-id ready))
                          (define (controller b p w target)
                            (define live (load-campaign-record dir plan))
                            (set-campaign-fence-token! live 8) ; another coordinator took over
                            (persist-campaign! dir live)
                            (delivery-effect-result 'ok (hasheq 'stage target)))
                          (define outcome
                            (run-delivery-coordinator! dir plan 0 #:controller controller))
                          (check-eq? (delivery-outcome-kind outcome) 'blocked)
                          (check-equal? (stage-count dir plan) "context-ready"))))
  (test-case "usage is recorded separately without touching implementation usage"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define (controller b p w target)
         (delivery-effect-result 'ok (hasheq 'stage target 'model-calls 2 'tokens 500 'cost 0.01)))
       (void (run-delivery-coordinator! dir plan 0 #:controller controller))
       (define journal (load-delivery-journal dir plan 0))
       (check-equal? (hash-ref journal 'model-calls #f) 2)
       (check-equal? (hash-ref journal 'tokens #f) 500)
       (check-equal? (hash-ref journal 'cost #f) 0.01)
       (define durable (load-campaign-record dir plan))
       (check-false (for/or ([w (in-list (campaign-record-waves durable))])
                      (define tokens (usage-summary-total-tokens (wave-usage-summary w)))
                      (and tokens (positive? tokens)))))))
  (test-case "default controller interpret maps controller verdicts to typed outcomes"
    (check-equal?
     (delivery-effect-result-kind (default-delivery-controller-interpret "implementation-merged"
                                                                         (hasheq 'status "merged")))
     'ok)
    (check-equal? (delivery-effect-result-kind (default-delivery-controller-interpret
                                                "implementation-merged"
                                                (hasheq 'status "already-merged")))
                  'ok)
    ;; already-merged preserves the payload so the binding dispatch stage
    ;; actions can route the idempotent resume identity verbatim.
    (check-equal?
     (hash-ref (delivery-effect-result-data (default-delivery-controller-interpret
                                             "implementation-merged"
                                             (hasheq 'status "already-merged" 'pr 7 'head "h")))
               'pr
               #f)
     7)
    (check-equal?
     (delivery-effect-result-kind
      (default-delivery-controller-interpret
       "implementation-merged"
       (hasheq 'status "awaiting-review" 'reason "no genuine independent human approval")))
     'awaiting-review)
    (check-equal? (delivery-effect-result-kind
                   (default-delivery-controller-interpret "sync" (hasheq 'status "synchronized")))
                  'ok)
    (check-equal? (delivery-effect-result-kind (default-delivery-controller-interpret
                                                "sync"
                                                (hasheq 'status "pending" 'reason "detached HEAD")))
                  'blocked)
    ;; W2 PR creation is deterministic and idempotent at the durable branch identity.
    (check-equal?
     (delivery-effect-result-kind (default-delivery-controller-interpret
                                   "implementation-pr"
                                   (hasheq 'status "opened" 'pr 42 'branch "campaign/test")))
     'ok)
    (check-equal?
     (delivery-effect-result-kind (default-delivery-controller-interpret
                                   "implementation-pr"
                                   (hasheq 'status "exists" 'pr 42 'branch "campaign/test")))
     'ok)
    ;; W2 CI success advances; unresolved CI remains a typed stop, never a fabricated green.
    (check-equal?
     (delivery-effect-result-kind (default-delivery-controller-interpret
                                   "implementation-ci"
                                   (hasheq 'status "green" 'pr 42 'branch "campaign/test")))
     'ok)
    (check-equal? (delivery-effect-result-kind
                   (default-delivery-controller-interpret
                    "implementation-ci"
                    (hasheq 'status "delivery-pending" 'reason "required check test (0) is pending")))
                  'blocked))
  (test-case "default controller refuses stages without a controller action (typed stop, never fabricated)"
    (define dir (make-temporary-file "coordinator-noop-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (define r (default-delivery-controller dir (make-string 64 #\a) 0 "future-stage"))
                    (check-eq? (delivery-effect-result-kind r) 'blocked)
                    (check-true (string-contains? (hash-ref (delivery-effect-result-data r) 'reason)
                                                  "not implemented")))
                  (lambda () (delete-directory/files dir))))
  (test-case "PR resolution statuses map to typed outcomes (arg contract: resolve feeds merge)"
    ;; The merge-ready controller derives the PR identity from the
    ;; authenticated resolve, never from the wave record (no PR number is
    ;; stored there). resolved -> ok with the PR; none/ambiguous -> typed
    ;; stop with an actionable reason so no merge is attempted without --pr.
    (check-equal?
     (delivery-effect-result-kind (default-delivery-controller-interpret
                                   "implementation-merged"
                                   (hasheq 'status "resolved" 'pr 7 'branch "campaign/test")))
     'ok)
    (check-equal? (hash-ref (delivery-effect-result-data
                             (default-delivery-controller-interpret
                              "implementation-merged"
                              (hasheq 'status "resolved" 'pr 7 'branch "campaign/test")))
                            'pr
                            #f)
                  7)
    (define none
      (default-delivery-controller-interpret "implementation-merged"
                                             (hasheq 'status "none" 'branch "campaign/test")))
    (check-eq? (delivery-effect-result-kind none) 'blocked)
    (check-true (string-contains? (hash-ref (delivery-effect-result-data none) 'reason)
                                  "no open pull request"))
    (check-true (string-contains? (hash-ref (delivery-effect-result-data none) 'reason)
                                  "implementation-merged"))
    (define unexpected
      (default-delivery-controller-interpret "implementation-merged"
                                             (hasheq 'status "delivery-pending" 'reason "api down")))
    (check-eq? (delivery-effect-result-kind unexpected) 'blocked)
    (check-true (string-contains? (hash-ref (delivery-effect-result-data unexpected) 'reason)
                                  "api down")))
  (test-case "reviewed controller verdict maps to ok (implementation-review)"
    (define reviewed
      (default-delivery-controller-interpret
       "implementation-review"
       (hasheq 'status "reviewed" 'head (make-string 40 #\a) 'reviewed-sha (make-string 40 #\a))))
    (check-eq? (delivery-effect-result-kind reviewed) 'ok)
    (check-equal? (hash-ref (delivery-effect-result-data reviewed) 'reviewed-sha #f)
                  (make-string 40 #\a))
    (check-eq? (delivery-effect-result-kind
                (default-delivery-controller-interpret
                 "implementation-review"
                 (hasheq 'status "awaiting-review" 'reason "review artifact absent")))
               'awaiting-review))
  (test-case "typed-stop reason preservation parses the delivery-pending stdout payload"
    (define stop
      (parse-delivery-stop "{\"status\":\"delivery-pending\",\"reason\":\"CI pending: test (0)\"}"
                           "implementation-ci"))
    (check-eq? (delivery-effect-result-kind stop) 'blocked)
    (check-true (string-contains? (hash-ref (delivery-effect-result-data stop) 'reason) "CI pending"))
    (check-false (parse-delivery-stop "not json at all" "implementation-review"))
    (check-false (parse-delivery-stop "{\"status\":\"reviewed\"}" "implementation-review"))
    (check-false (parse-delivery-stop "" "implementation-review")))
  (test-case "implementation-review routes to the review action (never the not-implemented stop)"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define ready (done-record dir))
                          (define plan (campaign-plan-id ready))
                          (define r (default-delivery-controller dir plan 0 "implementation-review"))
                          (check-eq? (delivery-effect-result-kind r) 'blocked)
                          (define reason (hash-ref (delivery-effect-result-data r) 'reason))
                          (check-false (string-contains? reason "not implemented"))
                          ;; the stub campaign repo has no origin: the python failure
                          ;; reason must survive the exit-2 boundary (previously
                          ;; discarded with the empty stderr)
                          (check-true (string-contains? reason "delivery command failed")))))
  (test-case "W2 PR and CI stages route to their controller actions"
    (call-with-campaign 1
                        (lambda (dir rec)
                          (define ready (done-record dir))
                          (define plan (campaign-plan-id ready))
                          (for ([stage (in-list '("implementation-pr" "implementation-ci"
                                                                      "implementation-merged"))])
                            (define r (default-delivery-controller dir plan 0 stage))
                            (check-eq? (delivery-effect-result-kind r) 'blocked)
                            (define reason (hash-ref (delivery-effect-result-data r) 'reason))
                            (check-false (string-contains? reason "not implemented")
                                         (format "~a must dispatch to its Python action" stage)))))))
(test-case "binding stages route to explicit Python actions"
  (call-with-campaign 1
                      (lambda (dir rec)
                        (define ready (done-record dir))
                        (define plan (campaign-plan-id ready))
                        (for ([stage (in-list '("binding-prepared" "binding-review"
                                                                   "binding-pr"
                                                                   "binding-ci"
                                                                   "binding-merged"
                                                                   "governance"))])
                          (define r (default-delivery-controller dir plan 0 stage))
                          (define reason (hash-ref (delivery-effect-result-data r) 'reason))
                          (check-false (string-contains? reason "not implemented")
                                       (format "~a must dispatch to its Python action" stage))))))
(test-case "binding CI/merge dispatch acts only on typed STRING statuses"
  ;; Regression (independent review F1): statuses arrive as JSON strings via
  ;; string->jsexpr. Comparing them against a quoted SYMBOL list never
  ;; matched, so binding-ci/binding-merged silently fell through to the
  ;; resolve passthrough and the journal advanced without the CI/merge
  ;; action ever running — fabricated progress.
  (define head (make-string 40 #\a))
  (define plan (make-string 64 #\b))
  (define (resolved-identity status)
    (delivery-effect-result 'ok (hasheq 'status status 'pr 77 'head head)))
  ;; resolved + valid identity -> the stage action runs with the exact
  ;; resolved PR identity.
  (let ([calls '()])
    (define (fake-run action . args)
      (set! calls (cons (cons action args) calls))
      (if (equal? action "binding-resolve-pr")
          (resolved-identity "resolved")
          (delivery-effect-result 'ok (hasheq 'status "green"))))
    (define r (binding-dispatch-stage-action fake-run "binding-ci" plan 3 "binding-ci"))
    (check-eq? (delivery-effect-result-kind r) 'ok)
    (check-equal? (hash-ref (delivery-effect-result-data r) 'status #f) "green")
    (check-equal? (length calls) 2)
    ;; R1 regression: the resolve invocation carries ONLY the per-action
    ;; flag. The run seam appends --repo/--plan/--wave itself; an unknown
    ;; --plan-id flag turned every dispatch into an argparse usage error.
    (check-equal? (cdr (car (reverse calls))) (list "--expected-branch" "binding/bbbbbbbbbbbb-w3"))
    (define action-call (car calls))
    (check-equal? (car action-call) "binding-ci")
    (define args (cdr action-call))
    (check-equal? (list-ref args 0) "--pr")
    (check-equal? (list-ref args 1) "77")
    (check-equal? (list-ref args 2) "--expected-branch")
    (check-equal? (list-ref args 3) "binding/bbbbbbbbbbbb-w3")
    (check-equal? (list-ref args 4) "--expected-head")
    (check-equal? (list-ref args 5) head))
  ;; binding-merge dispatch carries the binding evidence path.
  (let ([calls '()])
    (define (fake-run action . args)
      (set! calls (cons (cons action args) calls))
      (if (equal? action "binding-resolve-pr")
          (resolved-identity "resolved")
          (delivery-effect-result 'ok (hasheq 'status "merged"))))
    (define r (binding-dispatch-stage-action fake-run "binding-merged" plan 3 "binding-merge"))
    (check-eq? (delivery-effect-result-kind r) 'ok)
    (check-equal? (hash-ref (delivery-effect-result-data r) 'status #f) "merged")
    (check-equal? (length calls) 2)
    (define args (cdr (car calls)))
    (check-equal? (list-ref args 0) "--pr")
    (check-equal? (list-ref args 1) "77")
    (check-equal? (list-ref args 2) "--expected-head")
    (check-equal? (list-ref args 4) "--expected-branch")
    (check-equal? (list-ref args 6) "--evidence")
    (check-equal? (list-ref args 7) (format "docs/reports/gsd-wave-evidence/~a-w3.rktd" plan)))
  ;; already-merged -> idempotent passthrough of the resolve result; the
  ;; protected merge action must NOT run again. The resolve result must be
  ;; returned verbatim (identity, not a copy).
  (let ([calls '()])
    (define resolve-result (resolved-identity "already-merged"))
    (define (fake-run action . args)
      (set! calls (cons (cons action args) calls))
      resolve-result)
    (define r (binding-dispatch-stage-action fake-run "binding-merged" plan 3 "binding-merge"))
    (check-eq? r resolve-result)
    (check-equal? (hash-ref (delivery-effect-result-data r) 'status #f) "already-merged")
    (check-equal? (length calls) 1))
  ;; resolved status WITHOUT a usable identity fails closed as blocked —
  ;; never a silent passthrough.
  (let ([calls '()])
    (define (fake-run action . args)
      (set! calls (cons (cons action args) calls))
      (delivery-effect-result 'ok (hasheq 'status "resolved" 'pr "not-a-number" 'head head)))
    (define r (binding-dispatch-stage-action fake-run "binding-ci" plan 3 "binding-ci"))
    (check-eq? (delivery-effect-result-kind r) 'blocked)
    (check-true (string-contains? (hash-ref (delivery-effect-result-data r) 'reason)
                                  "no usable binding PR identity"))
    (check-equal? (length calls) 1))
  ;; A symbol status (seam violation shape) must NOT dispatch the action;
  ;; the downstream interpret seam blocks it as an unexpected status.
  (let ([calls '()])
    (define (fake-run action . args)
      (set! calls (cons (cons action args) calls))
      (delivery-effect-result 'ok (hasheq 'status 'resolved 'pr 77 'head head)))
    (define r (binding-dispatch-stage-action fake-run "binding-ci" plan 3 "binding-ci"))
    (check-eq? (delivery-effect-result-kind r) 'ok)
    (check-equal? (length calls) 1)
    (check-eq? (delivery-effect-result-kind
                (default-delivery-controller-interpret "binding-ci" (delivery-effect-result-data r)))
               'blocked))
  ;; none -> typed passthrough for the interpret seam to block.
  (let ([calls '()])
    (define (fake-run action . args)
      (set! calls (cons (cons action args) calls))
      (delivery-effect-result 'ok (hasheq 'status "none" 'branch "binding/bbbbbbbbbbbb-w3")))
    (define r (binding-dispatch-stage-action fake-run "binding-ci" plan 3 "binding-ci"))
    (check-equal? (hash-ref (delivery-effect-result-data r) 'status #f) "none")
    (check-equal? (length calls) 1)))
