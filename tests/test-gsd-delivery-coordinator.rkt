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
         json
         racket/format
         racket/runtime-path
         racket/string
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/plan-snapshot.rkt" make-plan-snapshot!)
         (only-in "../extensions/gsd/delivery-journal.rkt"
                  delivery-stages
                  load-delivery-journal
                  record-delivery-receipt!
                  reconcile-repair-tail-receipt!
                  update-delivery-journal!)
         "../extensions/gsd/delivery-coordinator.rkt"
         (only-in (file "../util/version.rkt") q-version))
;; Red-first (repo convention): the v1.00.33 W0 generation-aware binding
;; helpers are required dynamically until the coordinator module provides
;; them.
(define-runtime-path coordinator-module "../extensions/gsd/delivery-coordinator.rkt")
(define-runtime-path handoff-module "../extensions/gsd/delivery-handoff.rkt")
(define binding-generation (dynamic-require coordinator-module 'binding-generation))
(define default-binding-branch (dynamic-require coordinator-module 'default-binding-branch))
(define default-binding-staging-path
  (dynamic-require coordinator-module 'default-binding-staging-path))
(define default-active-delivery-evidence-path
  (dynamic-require coordinator-module 'default-active-delivery-evidence-path))

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

(define (call-with-campaign count proc #:title [title "# Plan: Delivery coordinator test"])
  (define dir (make-temporary-file "delivery-coordinator-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning/waves"))
     (call-with-output-file
      (build-path dir ".planning/PLAN.md")
      (lambda (out)
        (display (string-append title "\n\n## Waves\n\n") out)
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

(define-runtime-path journal-module "../extensions/gsd/delivery-journal.rkt")
(define record-remote-pending! (dynamic-require journal-module 'record-remote-pending!))
(define clear-remote-pending! (dynamic-require journal-module 'clear-remote-pending!))

(module+ test
  (test-case "W3 preflight runs before the first ladder action and blocks it when refused"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       (define calls '())
       (define (controller b p w target)
         (set! calls (cons target calls))
         (if (equal? target "delivery-preflight")
             (delivery-effect-result
              'blocked
              (hasheq
               'stage
               target
               'reason
               (format
                "branch-not-published: branch campaign/test head ~a is not published on origin; push the verified head first"
                (make-string 40 #\a))))
             (delivery-effect-result 'ok (hasheq 'stage target))))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'blocked)
       (check-true (string-contains? (delivery-outcome-message outcome) "branch-not-published"))
       (check-true (string-contains? (delivery-outcome-message outcome) "campaign/test"))
       (check-true (string-contains? (delivery-outcome-message outcome) (make-string 40 #\a)))
       (check-true (string-contains? (delivery-outcome-message outcome) "push the verified head"))
       (check-equal? (reverse calls) '("delivery-preflight"))
       ;; The journal never advanced: the ladder entry stays sealed.
       (check-equal? (stage-count dir plan) "context-ready"))))
  ;; Register F7 (this campaign's W5): the artifact provenance gate runs before the
  ;; F5 preflight. A current-wave artifact directory without a SHA256SUMS
  ;; binding is provenance drift; delivery is blocked at context-ready.
  ;; BUG-0009: the fixture's version-tagged branch/artifact names derive from
  ;; q-version, because the production gate keys "current wave" off the
  ;; canonical version.
  (test-case "F7 provenance gate blocks delivery on drifting artifacts"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       ;; The durable receipt branch carries the version tag the gate uses
       ;; to identify the current wave directory.
       (define journal-path (build-path dir ".planning" "campaigns" plan "coordinator-w0.json"))
       (define journal-datum (call-with-input-file journal-path read-json))
       (define receipt (hash-ref journal-datum 'receipt))
       (define patched-receipt (hash-set receipt 'branch (format "campaign/v~a-w0" q-version)))
       (call-with-output-file journal-path
                              (lambda (out)
                                (write-json (hash-set journal-datum 'receipt patched-receipt) out))
                              #:exists 'truncate)
       ;; Keep the campaign record consistent with the patched receipt branch
       ;; (the durable blocker refuses provenance mismatches first).
       (define rec* (load-campaign-record dir plan))
       (set-campaign-wave-delivery-branch! (for/first ([w (in-list (campaign-record-waves rec*))])
                                             w)
                                           (format "campaign/v~a-w0" q-version))
       (persist-campaign! dir rec*)
       ;; Drift: declared artifact files with no SHA256SUMS binding.
       (define adir (build-path dir "q" "artifacts" "probe" (format "v~a-w0" q-version)))
       (make-directory* adir)
       (display-to-file "{\n \"recorded-head\": \"0000000000000000000000000000000000000000\"\n}\n"
                        (build-path adir "matrix.json"))
       (define calls '())
       (define (controller b p w target)
         (set! calls (cons target calls))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'blocked)
       (check-true (string-contains? (delivery-outcome-message outcome) "artifact-provenance")
                   "the typed provenance refusal surfaces")
       (check-true (string-contains? (delivery-outcome-message outcome) "binds no SHA256SUMS")
                   "the drift reason is preserved")
       (check-equal? calls '() "no ladder action ran past the provenance gate")
       (check-equal? (stage-count dir plan) "context-ready"))))

  ;; R11 (this campaign's W5): the frozen manifest title carries the campaign version,
  ;; so the provenance gate stays strict even when the delivery branch follows
  ;; the executor's campaign/<hash8>/w<N> shape and therefore has no version
  ;; tag. Before this, such a branch silently degraded the gate to historical
  ;; (advisory) mode and current-wave drift passed unchecked.
  (test-case "R11: provenance gate is strict without a version-tagged branch"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       ;; Drift: a declared current-wave artifact directory with no SHA256SUMS.
       (define adir (build-path dir "q" "artifacts" "probe" (format "v~a-w0" q-version)))
       (make-directory* adir)
       (display-to-file "{\n \"recorded-head\": \"0000000000000000000000000000000000000000\"\n}\n"
                        (build-path adir "matrix.json"))
       (define calls '())
       (define (controller b p w target)
         (set! calls (cons target calls))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'blocked)
       (check-true (string-contains? (delivery-outcome-message outcome) "artifact-provenance")
                   "the provenance gate refused")
       (check-true (string-contains? (delivery-outcome-message outcome) "binds no SHA256SUMS")
                   "current-wave drift is enforced without a version-tagged branch")
       (check-equal? calls '() "no ladder action ran past the provenance gate"))
     #:title (format "# Plan: v~a Delivery coordinator test" q-version)))

  (test-case "W3 branch-not-published durable gate names branch and head with the remedy"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (done-record dir))
       (define plan (campaign-plan-id ready))
       ;; Replace the verified receipt with a remote-pending marker: the
       ;; Verify receipt was withheld because the branch was never pushed.
       (delete-file (build-path dir ".planning" "campaigns" plan "coordinator-w0.json"))
       (record-remote-pending! dir
                               plan
                               0
                               "campaign/test"
                               (make-string 40 #\a)
                               "branch not published on origin; receipt not verified")
       (define calls '())
       (define (controller b p w target)
         (set! calls (cons target calls))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind outcome) 'blocked)
       (check-true (string-contains? (delivery-outcome-message outcome) "branch-not-published"))
       (check-true (string-contains? (delivery-outcome-message outcome) "campaign/test"))
       (check-true (string-contains? (delivery-outcome-message outcome) (make-string 40 #\a)))
       (check-true (string-contains? (delivery-outcome-message outcome) "push the verified head"))
       (check-equal? calls '() "no controller effect ran before the typed refusal")
       ;; Publishing resolves the marker: the ladder may proceed.
       (clear-remote-pending! dir plan 0)
       (record-delivery-receipt! dir plan 0 (receipt-head))
       (define resumed (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind resumed) 'ok)
       (check-equal? (reverse calls) '("delivery-preflight" "implementation-review"))))))

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
       (check-equal? (reverse calls)
                     '("delivery-preflight" "implementation-review"
                                            "implementation-pr"
                                            "implementation-ci"
                                            "implementation-merged")))))
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
  (test-case "full ladder walks through sync to the delivered stage"
    ;; Regression for the resume gap observed live (2026-10-05): a campaign
    ;; whose delivery is already merged and published resumes from
    ;; context-ready and must be able to reach the terminal delivered stage.
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
                                                          "implementation-merged"
                                                          "binding-prepared"
                                                          "binding-review"
                                                          "binding-pr"
                                                          "binding-ci"
                                                          "binding-merged"
                                                          "governance"
                                                          "sync"
                                                          "delivered"))])
         (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
         (check-eq? (delivery-outcome-kind outcome) 'ok expected)
         (check-equal? (stage-count dir plan) expected))
       ;; The next invocation short-circuits at the terminal stage.
       (define after (run-delivery-coordinator! dir plan 0 #:controller controller))
       (check-eq? (delivery-outcome-kind after) 'delivered)
       (check-equal? (stage-count dir plan) "delivered"))))

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
    ;; W3 register F5: the preflight ready verdict is an ok — a published
    ;; head proceeds through the handoff seam.
    (check-equal?
     (delivery-effect-result-kind (default-delivery-controller-interpret "delivery-preflight"
                                                                         (hasheq 'status "ready")))
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
  ;; v1.00.33 W0: dispatch is generation-aware and requires the campaign
  ;; base-dir explicitly (never guesses the binding branch). These cases use
  ;; an empty root (no journal -> generation 0 -> unsuffixed names).
  (define no-journal-root (make-temporary-file "binding-dispatch-~a" 'directory))
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
    (define r
      (binding-dispatch-stage-action fake-run
                                     "binding-ci"
                                     plan
                                     3
                                     "binding-ci"
                                     #:base-dir no-journal-root))
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
    (define r
      (binding-dispatch-stage-action fake-run
                                     "binding-merged"
                                     plan
                                     3
                                     "binding-merge"
                                     #:base-dir no-journal-root))
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
    (define r
      (binding-dispatch-stage-action fake-run
                                     "binding-merged"
                                     plan
                                     3
                                     "binding-merge"
                                     #:base-dir no-journal-root))
    (check-eq? r resolve-result)
    (check-equal? (hash-ref (delivery-effect-result-data r) 'status #f) "already-merged")
    (check-equal? (length calls) 1))
  ;; resolved status WITHOUT a usable identity fails closed as blocked —
  ;; never a silent passthrough.
  (let ([calls '()])
    (define (fake-run action . args)
      (set! calls (cons (cons action args) calls))
      (delivery-effect-result 'ok (hasheq 'status "resolved" 'pr "not-a-number" 'head head)))
    (define r
      (binding-dispatch-stage-action fake-run
                                     "binding-ci"
                                     plan
                                     3
                                     "binding-ci"
                                     #:base-dir no-journal-root))
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
    (define r
      (binding-dispatch-stage-action fake-run
                                     "binding-ci"
                                     plan
                                     3
                                     "binding-ci"
                                     #:base-dir no-journal-root))
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
    (define r
      (binding-dispatch-stage-action fake-run
                                     "binding-ci"
                                     plan
                                     3
                                     "binding-ci"
                                     #:base-dir no-journal-root))
    (check-equal? (hash-ref (delivery-effect-result-data r) 'status #f) "none")
    (check-equal? (length calls) 1)))

(test-case "binding republication is generation-aware after a repair tail"
  ;; v1.00.33 W0 (audit §21.4): a reconciled journal (one prior receipt)
  ;; MUST direct binding dispatch at the -r1 branch, and the staging path
  ;; must mirror it — governance admits exactly one merged PR per binding
  ;; branch, so reusing the generation-0 name would resolve the EXHAUSTED
  ;; branch and report already-merged (fabricated progress).
  (define plan (make-string 64 #\b))
  (define root (make-temporary-file "binding-gen-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define (receipt head)
       (hasheq 'repo
               "/repo"
               'branch
               "campaign/w3"
               'head
               head
               'tree
               (make-string 40 #\c)
               'origin
               "https://github.com/example/q.git"
               'verified-at
               1
               'evidence
               "verify"
               'attempt-id
               "attempt-1"
               'attempt-fence
               2))
     (record-delivery-receipt! root plan 3 (receipt (make-string 40 #\a)))
     (define gen0 (binding-generation root plan 3))
     (check-equal? gen0 0)
     (reconcile-repair-tail-receipt! root
                                     plan
                                     3
                                     (receipt (make-string 40 #\d))
                                     #:expected-attempt-id "attempt-1"
                                     #:expected-fence 2
                                     #:head-ancestor? (lambda (_old _new) #t))
     (check-equal? (binding-generation root plan 3) 1)
     (check-equal? (default-binding-branch plan 3 #:base-dir root) "binding/bbbbbbbbbbbb-w3-r1")
     (check-equal? (default-binding-staging-path root plan 3)
                   (path->string (build-path root ".planning" "campaigns" plan "binding-w3-r1")))
     (define w (make-campaign-wave 3 "repair" 'awaiting-delivery 5 #f))
     (set-campaign-wave-delivery-branch! w "campaign/w3")
     (set-campaign-wave-delivery-head-sha! w (make-string 40 #\d))
     (define handoff
       ((dynamic-require handoff-module 'persist-delivery-handoff!) root plan w "repair pending"))
     (check-equal? (hash-ref (call-with-input-file handoff read) 'binding)
                   (format "docs/reports/gsd-wave-evidence/~a-w3-r1.rktd" plan))
     ;; dispatch with the reconciled journal resolves the -r1 branch
     (let ([calls '()])
       (define (fake-run action . args)
         (set! calls (cons (cons action args) calls))
         (delivery-effect-result 'ok (hasheq 'status "none")))
       (binding-dispatch-stage-action fake-run "binding-ci" plan 3 "binding-ci" #:base-dir root)
       (check-equal? (cdr (car (reverse calls)))
                     (list "--expected-branch" "binding/bbbbbbbbbbbb-w3-r1")))
     (let ([calls '()])
       (define (fake-run action . args)
         (set! calls (cons (cons action args) calls))
         (delivery-effect-result 'ok (hasheq 'status "resolved" 'pr 77 'head (make-string 40 #\e))))
       (binding-dispatch-stage-action fake-run
                                      "binding-merged"
                                      plan
                                      3
                                      "binding-merge"
                                      #:base-dir root)
       (check-not-false (member (format "docs/reports/gsd-wave-evidence/~a-w3-r1.rktd" plan)
                                (cdr (car calls))))))
   (lambda () (delete-directory/files root #:must-exist? #f))))

(test-case "active implementation source path is derived from frozen declarations and generation"
  (define plan (make-string 64 #\c))
  (define root (make-temporary-file "active-source-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory* (build-path root ".planning" "waves"))
                  (define plan-text "# Plan\n\n- [Inbox] W3: Repair → waves/W3-repair.md\n")
                  (define wave-text
                    (string-append "# W3: Repair\n\n## Files\n\n"
                                   "- File: `docs/reports/gsd-wave-evidence/v9.9.9-w3.rktd`\n"
                                   "- File: `docs/reports/gsd-wave-reviews/v9.9.9-w3.rktd`\n"
                                   "- File: `docs/reports/gsd-wave-validation/v9.9.9-w3.rktd`\n"))
                  (display-to-file plan-text (build-path root ".planning" "PLAN.md"))
                  (display-to-file wave-text (build-path root ".planning" "waves" "W3-repair.md"))
                  (make-plan-snapshot! root plan plan-text #:plan-id plan)
                  (define (receipt head)
                    (hasheq 'repo
                            "/repo"
                            'branch
                            "campaign/w3"
                            'head
                            head
                            'tree
                            (make-string 40 #\c)
                            'origin
                            "https://github.com/example/q.git"
                            'verified-at
                            1
                            'evidence
                            "verify"
                            'attempt-id
                            "attempt-1"
                            'attempt-fence
                            2))
                  (record-delivery-receipt! root plan 3 (receipt (make-string 40 #\a)))
                  (reconcile-repair-tail-receipt! root
                                                  plan
                                                  3
                                                  (receipt (make-string 40 #\d))
                                                  #:expected-attempt-id "attempt-1"
                                                  #:expected-fence 2
                                                  #:head-ancestor? (lambda (_old _new) #t))
                  (check-equal? (default-active-delivery-evidence-path root plan 3)
                                "docs/reports/gsd-wave-evidence/v9.9.9-w3-r1.rktd"))
                (lambda () (delete-directory/files root #:must-exist? #f))))

(module+ test
  ;; REVIEW-2 item 4 (BUG-0079): publication-only replay is ONE explicit
  ;; initial run-delivery-coordinator! transition — never inside the pure
  ;; durable-blocker re-evaluated before/after every ladder effect, and never
  ;; after the journal reached the delivered stage. A failed replay surfaces
  ;; as the branch-not-published blocker instead of being swallowed into
  ;; ladder admission.
  (define-runtime-path publication-module "../extensions/gsd/delivery-publication.rkt")
  (define publication-publisher
    (dynamic-require publication-module 'current-gsd-approved-head-publisher))
  (define publication-publish! (dynamic-require publication-module 'publish-approved-head!))
  (define load-remote-pending* (dynamic-require journal-module 'load-remote-pending))

  ;; Approved-but-unpublished setup: a done wave bound to the approved
  ;; identity, a durable confirmed publication intent, and NO journal
  ;; receipt — the crash window between a successful publication readback
  ;; and receipt certification.
  (define (approved-unpublished-campaign! dir)
    (define rec (migrate-campaign! dir))
    (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
    (set-campaign-wave-current-attempt! (car (campaign-record-waves rec))
                                        (campaign-attempt "attempt-1" 7 0))
    (set-campaign-fence-token! rec 7)
    (set-campaign-wave-delivery-branch! (car (campaign-record-waves rec)) "campaign/test")
    (set-campaign-wave-delivery-head-sha! (car (campaign-record-waves rec)) (make-string 40 #\a))
    (persist-campaign! dir rec)
    rec)

  (test-case "publication-only replay is one initial transition, never re-run by post-effect checks"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (approved-unpublished-campaign! dir))
       (define plan (campaign-plan-id ready))
       (define publisher-calls 0)
       (define (controller b p w target)
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (parameterize ([publication-publisher
                       (lambda (_base _plan _wave snapshot _aid _af _fence _evidence)
                         (set! publisher-calls (add1 publisher-calls))
                         (hasheq 'status
                                 (if (= publisher-calls 1) "published" "already-published")
                                 'branch
                                 (hash-ref snapshot 'branch)
                                 'head
                                 (hash-ref snapshot 'head)))])
         (publication-publish! dir
                               plan
                               0
                               (receipt-head)
                               "attempt-1"
                               7
                               7
                               "approved evidence"
                               (lambda () (load-campaign-record dir plan)))
         (check-equal? publisher-calls 1)
         (check-false (load-delivery-journal dir plan 0))
         (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
         (check-eq? (delivery-outcome-kind outcome) 'ok)
         (check-equal? publisher-calls
                       2
                       "replay runs exactly once as the initial transition, never again post-effect")
         (define journal (load-delivery-journal dir plan 0))
         (check-equal? (hash-ref (hash-ref journal 'receipt) 'head) (make-string 40 #\a))
         (check-false (load-remote-pending* dir plan 0))))))

  (test-case "delivered journal short-circuits before any publication replay"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (approved-unpublished-campaign! dir))
       (define plan (campaign-plan-id ready))
       (record-delivery-receipt! dir plan 0 (receipt-head))
       (update-delivery-journal! dir plan 0 (hasheq 'stage "delivered"))
       (define publisher-calls 0)
       (define (controller b p w target)
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (parameterize ([publication-publisher
                       (lambda (_base _plan _wave snapshot _aid _af _fence _evidence)
                         (set! publisher-calls (add1 publisher-calls))
                         (hasheq 'status
                                 "published"
                                 'branch
                                 (hash-ref snapshot 'branch)
                                 'head
                                 (hash-ref snapshot 'head)))])
         (publication-publish! dir
                               plan
                               0
                               (receipt-head)
                               "attempt-1"
                               7
                               7
                               "approved evidence"
                               (lambda () (load-campaign-record dir plan)))
         (check-equal? publisher-calls 1)
         (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
         (check-eq? (delivery-outcome-kind outcome) 'delivered)
         (check-equal? publisher-calls 1 "no publication replay after the delivered stage")))))

  (test-case "failed publication replay surfaces as the branch-not-published blocker"
    (call-with-campaign
     1
     (lambda (dir rec)
       (define ready (approved-unpublished-campaign! dir))
       (define plan (campaign-plan-id ready))
       (define controller-calls 0)
       (define (controller b p w target)
         (set! controller-calls (add1 controller-calls))
         (delivery-effect-result 'ok (hasheq 'stage target)))
       (parameterize ([publication-publisher (lambda args
                                               (hasheq 'status "blocked" 'reason "remote diverged"))])
         ;; The intent is durable but the readback now refuses: replay fails.
         (with-handlers ([exn:fail? void])
           (publication-publish! dir
                                 plan
                                 0
                                 (receipt-head)
                                 "attempt-1"
                                 7
                                 7
                                 "approved evidence"
                                 (lambda () (load-campaign-record dir plan))))
         (check-true
          (file-exists?
           (build-path dir ".planning" "campaigns" plan "coordinator-w0.publication.json")))
         (define outcome (run-delivery-coordinator! dir plan 0 #:controller controller))
         (check-eq? (delivery-outcome-kind outcome) 'blocked)
         (check-true (string-contains? (delivery-outcome-message outcome) "branch-not-published"))
         (check-true (string-contains? (delivery-outcome-message outcome) "remote diverged"))
         (check-equal? controller-calls 0 "a failed replay never admits the ladder")
         (check-false (load-delivery-journal dir plan 0)))))))

(module+ test
  ;; v1.00.33 W1 (BUG: scoped artifact checkout root). The F7 artifact
  ;; provenance gate must inspect the checkout BOUND TO THE DURABLE RECEIPT
  ;; — a clean worktree on the receipt branch at the exact verified head —
  ;; never the idle base checkout (which under worktree isolation sits on
  ;; main and either misses the genuine wave artifacts or could carry
  ;; untracked fake ones). Selection is fail-closed: wrong branch, stale
  ;; head, dirty/untracked worktree, or a missing receipt checkout all
  ;; refuse; only a non-git base (synthetic fixture) keeps the legacy
  ;; unscoped root.
  (require racket/system
           racket/port
           (only-in (file "../scripts/run-tests/sha256.rkt") sha256 bytes->hex-string))

  (define (receipt-for branch head)
    (hash-set (hash-set (receipt-head) 'branch branch) 'head head))

  ;; ---- Real-git behavior fixtures (isolated receipt worktree vs idle base)
  (define GIT-PATH (find-executable-path "git"))
  (define (git-check! dir . args)
    (define ok
      (apply system*
             GIT-PATH
             "-C"
             (path->string dir)
             "-c"
             "user.name=GSD Test"
             "-c"
             "user.email=gsd-test@example.invalid"
             args))
    (check-true ok (format "git ~a" (string-join args " "))))
  (define (git-value dir . args)
    (define out (open-output-string))
    (define ok
      (parameterize ([current-output-port out])
        (apply system* GIT-PATH "-C" (path->string dir) args)))
    (check-true ok (format "git ~a" (string-join args " ")))
    (string-trim (get-output-string out)))

  (define provenance-branch "campaign/v9.9.9-w0")
  (define (write-valid-artifact! repo base-sha)
    (define adir (build-path repo "artifacts" "probe" "v9.9.9-w0"))
    (make-directory* adir)
    (define matrix (format "{\n \"recorded-head\": \"~a\"\n}\n" base-sha))
    (display-to-file matrix (build-path adir "matrix.json"))
    (define digest
      (bytes->hex-string (sha256 (call-with-input-file (build-path adir "matrix.json") port->bytes))))
    (display-to-file (format "~a  artifacts/probe/v9.9.9-w0/matrix.json\n" digest)
                     (build-path adir "SHA256SUMS")))

  (define (setup-provenance-campaign! proj branch head)
    (make-directory* (build-path proj ".planning/waves"))
    (call-with-output-file
     (build-path proj ".planning/PLAN.md")
     (lambda (out)
       (display "# Plan: provenance root test\n\n## Waves\n\n- [Inbox] W0: Test → waves/W0-test.md\n"
                out)))
    (display-to-file "# Test\n\nGoal: test\n\n## Verify\n\nraco test .\n"
                     (build-path proj ".planning/waves" "W0-test.md"))
    (define rec (migrate-campaign! proj))
    (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
    (set-campaign-wave-current-attempt! (car (campaign-record-waves rec))
                                        (campaign-attempt "attempt-1" 7 0))
    (set-campaign-fence-token! rec 7)
    (set-campaign-wave-delivery-branch! (car (campaign-record-waves rec)) branch)
    (set-campaign-wave-delivery-head-sha! (car (campaign-record-waves rec)) head)
    (persist-campaign! proj rec)
    (record-delivery-receipt! proj (campaign-plan-id rec) 0 (receipt-for branch head))
    rec)

  ;; artifact-location: 'branch (genuine committed wave artifacts on the
  ;; receipt branch) or 'main (artifact bytes committed only on idle main —
  ;; the wrong checkout).
  (define (build-provenance-sandbox artifact-location)
    (define tmp (make-temporary-file "provenance-root-~a" 'directory))
    (define proj (build-path tmp "proj"))
    (define repo (build-path proj "q"))
    (make-directory* repo)
    (git-check! repo "init" "-b" "main")
    (display-to-file "base\n" (build-path repo "README.md"))
    (git-check! repo "add" "-A")
    (git-check! repo "commit" "-m" "base")
    (define base-sha (git-value repo "rev-parse" "HEAD"))
    (git-check! repo "branch" provenance-branch)
    (when (eq? artifact-location 'main)
      (write-valid-artifact! repo base-sha)
      (git-check! repo "add" "-A")
      (git-check! repo "commit" "-m" "main artifacts"))
    (when (eq? artifact-location 'branch)
      (git-check! repo "checkout" provenance-branch)
      (write-valid-artifact! repo base-sha)
      (git-check! repo "add" "-A")
      (git-check! repo "commit" "-m" "wave artifacts")
      (git-check! repo "checkout" "main"))
    (define branch-tip (git-value repo "rev-parse" provenance-branch))
    (define wt (build-path tmp "wt-receipt"))
    (git-check! repo "worktree" "add" (path->string wt) provenance-branch)
    (define rec (setup-provenance-campaign! proj provenance-branch branch-tip))
    (values tmp proj wt rec))

  (when GIT-PATH
    (test-case "provenance gate inspects the isolated receipt worktree, not the idle base checkout"
      ;; RED: the base checkout (idle main) lacks the wave artifact directory;
      ;; the genuine committed artifacts live in the receipt worktree. The
      ;; gate must select the receipt worktree and PASS; before the fix it
      ;; linted the idle base and blocked with a missing current-wave
      ;; directory.
      (define-values (tmp proj wt rec) (build-provenance-sandbox 'branch))
      (dynamic-wind void
                    (lambda ()
                      (define plan (campaign-plan-id rec))
                      (define calls '())
                      (define (controller b p w target)
                        (set! calls (cons target calls))
                        (delivery-effect-result 'ok (hasheq 'stage target)))
                      (define outcome (run-delivery-coordinator! proj plan 0 #:controller controller))
                      (check-eq? (delivery-outcome-kind outcome)
                                 'ok
                                 (format "gate must clear on the receipt worktree, got: ~a"
                                         (delivery-outcome-message outcome)))
                      (check-equal? (stage-count proj plan) "implementation-review")
                      (check-equal? (reverse calls) '("delivery-preflight" "implementation-review")))
                    (lambda () (delete-directory/files tmp #:must-exist? #f))))
    (test-case "provenance gate rejects a dirty receipt worktree (no uncommitted byte inspection)"
      (define-values (tmp proj wt rec) (build-provenance-sandbox 'branch))
      (dynamic-wind void
                    (lambda ()
                      ;; Planted uncommitted byte: the worktree is no longer a faithful
                      ;; receipt-head checkout, so the gate must refuse to inspect it.
                      (display-to-file "planted\n" (build-path wt "scratch.txt"))
                      (define plan (campaign-plan-id rec))
                      (define calls '())
                      (define (controller b p w target)
                        (set! calls (cons target calls))
                        (delivery-effect-result 'ok (hasheq 'stage target)))
                      (define outcome (run-delivery-coordinator! proj plan 0 #:controller controller))
                      (check-eq? (delivery-outcome-kind outcome) 'blocked)
                      (check-true (string-contains? (delivery-outcome-message outcome)
                                                    "no clean receipt-bound checkout"))
                      (check-equal? calls '() "no ladder action ran past the refused gate")
                      (check-equal? (stage-count proj plan) "context-ready"))
                    (lambda () (delete-directory/files tmp #:must-exist? #f))))
    (test-case "provenance gate never accepts artifact bytes from the wrong checkout"
      ;; Artifact bytes committed only on idle main (never on the receipt
      ;; branch). Before the fix the gate linted the idle base and ACCEPTED
      ;; them; fail-closed selection must inspect the receipt worktree and
      ;; block on the missing current-wave directory instead.
      (define-values (tmp proj wt rec) (build-provenance-sandbox 'main))
      (dynamic-wind void
                    (lambda ()
                      (define plan (campaign-plan-id rec))
                      (define (controller b p w target)
                        (delivery-effect-result 'ok (hasheq 'stage target)))
                      (define outcome (run-delivery-coordinator! proj plan 0 #:controller controller))
                      (check-eq? (delivery-outcome-kind outcome) 'blocked)
                      (check-true (string-contains? (delivery-outcome-message outcome)
                                                    "artifact-provenance"))
                      (check-true (string-contains? (delivery-outcome-message outcome)
                                                    "no artifact version directory"))
                      (check-equal? (stage-count proj plan) "context-ready"))
                    (lambda () (delete-directory/files tmp #:must-exist? #f))))))

(module+ test
  ;; v1.00.33 W1: unit matrix for the fail-closed checkout-root selection,
  ;; driven through the production git seam ((repo-dir args) -> (values
  ;; exit-code stdout-string)) with scripted worktree-list/status replies.
  ;; Red-first (repo convention): required dynamically until the coordinator
  ;; module provides the selector.
  (define artifact-root-selector
    (dynamic-require coordinator-module 'default-artifact-provenance-root))

  (define receipt-branch-unit "campaign/test")
  (define receipt-head-unit (make-string 40 #\a))
  (define main-head-unit (make-string 40 #\f))
  ;; Fixture-derived checkout path-strings (no hardcoded absolute paths):
  ;; the scripted worktree listings and status keys are plain strings, so
  ;; the unit seam only needs paths derived from a temp fixture root.
  (define fixture-root (make-temporary-file "provenance-unit-~a" 'directory))
  (define q-dir (path->string (build-path fixture-root "proj" "q")))
  (define wt-receipt-dir (path->string (build-path fixture-root "wt-receipt")))
  (define wt-stale-dir (path->string (build-path fixture-root "wt-stale")))
  (define wt-detached-dir (path->string (build-path fixture-root "wt-detached")))
  (define (porcelain-text entries)
    (string-join (for/list ([e (in-list entries)])
                   (string-append "worktree "
                                  (list-ref e 0)
                                  "\nHEAD "
                                  (list-ref e 1)
                                  "\n"
                                  (if (list-ref e 2)
                                      (string-append "branch refs/heads/" (list-ref e 2) "\n")
                                      "detached\n")))
                 "\n"))
  (define (fake-git worktree-listing statuses)
    (lambda (repo-dir args)
      (cond
        [(equal? args (list "worktree" "list" "--porcelain")) (values 0 worktree-listing)]
        [(equal? args (list "status" "--porcelain"))
         (values 0
                 (hash-ref statuses
                           (if (path? repo-dir)
                               (path->string repo-dir)
                               repo-dir)
                           ""))]
        [else (values 1 "")])))

  (dynamic-wind
   void
   (lambda ()
     (test-case "provenance checkout root: clean exact receipt worktree is selected over the idle base"
       (call-with-campaign
        1
        (lambda (dir rec)
          (define ready (done-record dir))
          (define plan (campaign-plan-id ready))
          (define listing
            (porcelain-text (list (list q-dir main-head-unit "main")
                                  (list wt-receipt-dir receipt-head-unit receipt-branch-unit))))
          (define selected
            (artifact-root-selector dir plan 0 #:run-git (fake-git listing (hash wt-receipt-dir ""))))
          (check-equal? (and selected (path->string selected)) wt-receipt-dir))))
     (test-case "provenance checkout root: nonisolated lawful base checkout still qualifies (legacy preserved)"
       (call-with-campaign
        1
        (lambda (dir rec)
          (define ready (done-record dir))
          (define plan (campaign-plan-id ready))
          ;; No linked worktrees: the base checkout itself is receipt-bound and clean.
          (define listing (porcelain-text (list (list q-dir receipt-head-unit receipt-branch-unit))))
          (define selected
            (artifact-root-selector dir plan 0 #:run-git (fake-git listing (hash q-dir ""))))
          (check-equal? (and selected (path->string selected)) q-dir))))
     (test-case "provenance checkout root: dirty or untracked receipt worktree is rejected (fail closed)"
       (call-with-campaign
        1
        (lambda (dir rec)
          (define ready (done-record dir))
          (define plan (campaign-plan-id ready))
          (define listing
            (porcelain-text (list (list q-dir main-head-unit "main")
                                  (list wt-receipt-dir receipt-head-unit receipt-branch-unit))))
          (check-false (artifact-root-selector
                        dir
                        plan
                        0
                        #:run-git
                        (fake-git listing (hash wt-receipt-dir " M artifacts/probe/m.json\n")))
                       "tracked modification rejects the worktree")
          (check-false (artifact-root-selector
                        dir
                        plan
                        0
                        #:run-git
                        (fake-git listing (hash wt-receipt-dir "?? artifacts/probe/fake.json\n")))
                       "untracked (planted) artifact rejects the worktree"))))
     (test-case "provenance checkout root: stale head, wrong branch, and detached worktrees never qualify"
       (call-with-campaign
        1
        (lambda (dir rec)
          (define ready (done-record dir))
          (define plan (campaign-plan-id ready))
          ;; Receipt branch checked out at a stale head.
          (define stale
            (porcelain-text (list (list q-dir main-head-unit "main")
                                  (list wt-stale-dir (make-string 40 #\b) receipt-branch-unit))))
          (check-false (artifact-root-selector dir plan 0 #:run-git (fake-git stale (hash))))
          ;; Only a wrong branch exists (the idle base): must NOT be accepted
          ;; even if it happens to hold artifact bytes.
          (define wrong-branch (porcelain-text (list (list q-dir main-head-unit "main"))))
          (check-false (artifact-root-selector dir plan 0 #:run-git (fake-git wrong-branch (hash))))
          ;; A detached worktree at the exact head still lacks the receipt branch.
          (define detached
            (porcelain-text (list (list q-dir main-head-unit "main")
                                  (list wt-detached-dir receipt-head-unit #f))))
          (check-false (artifact-root-selector dir plan 0 #:run-git (fake-git detached (hash)))))))
     (test-case "provenance checkout root: non-git bases keep the legacy unscoped root"
       (call-with-campaign 1
                           (lambda (dir rec)
                             (define ready (done-record dir))
                             (define plan (campaign-plan-id ready))
                             (define no-git (lambda (_dir _args) (values 128 "")))
                             ;; No .git marker in base-dir: legacy child q/ root.
                             (check-equal? (artifact-root-selector dir plan 0 #:run-git no-git)
                                           (build-path dir "q"))
                             ;; A Git marker means enumeration failure must never fall back.
                             (make-directory (build-path dir "q"))
                             (display-to-file "gitdir: missing" (build-path dir "q" ".git"))
                             (check-false (artifact-root-selector dir plan 0 #:run-git no-git))
                             (delete-file (build-path dir "q" ".git"))
                             (make-directory (build-path dir ".git"))
                             (check-false (artifact-root-selector dir plan 0 #:run-git no-git))))))
   (lambda () (delete-directory/files fixture-root #:must-exist? #f))))
