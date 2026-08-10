#lang racket/base

;; @speed fast
;; @suite workflows

;; BOUNDARY: integration

;; tests/helpers/gsd-golden-trace.rkt — Deterministic semantic GSD workflow traces
;;
;; v0.99.89 W0 "Golden Workflow Traces": the refactoring oracle for W1–W4.
;;
;; Captures a normalized, fully deterministic semantic trace of a GSD
;; workflow scenario: commands, FSM transitions, campaign record, PLAN/STATE/
;; wave projections, completion outbox, campaign result, and event order.
;; Nondeterministic fields (timestamps, correlation IDs, session IDs, random
;; temp names) are excluded by construction — the trace contains only durable
;; semantics. This is what makes the trace a "golden" oracle: the same
;; scenario always produces the same trace, so W1–W4 may prove behavioral
;; equivalence by comparing traces.
;;
;; The trace shape is stable and reused by W1 (pure transition kernel),
;; W2 (plan/state projection kernel), W3 (command parsing) and W4 (facade
;; thinning) to verify behavior preservation.

(require racket/file
         racket/path
         racket/string
         racket/match
         racket/format
         racket/port
         (only-in "../../extensions/gsd/campaign-state.rkt"
                  migrate-campaign!
                  persist-campaign!
                  load-campaign-record
                  canonical-wave-status
                  campaign-plan-id
                  campaign-fence-token
                  campaign-record-provenance
                  campaign-record-cancellation
                  campaign-cancellation-reason
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-attempt-count
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  set-campaign-wave-status!)
         (only-in "../../extensions/gsd/go-orchestrator.rkt"
                  make-campaign-request
                  execute-campaign-request!
                  campaign-result-status
                  campaign-result-completed-waves)
         (only-in "../../extensions/gsd/wave-completion.rkt" make-event-id load-outbox)
         (only-in "../../extensions/gsd/wave-docs.rkt"
                  parse-plan-index
                  read-wave-doc
                  plan-overall-status
                  wave-index-entry-idx
                  wave-index-entry-slug
                  wave-index-entry-status)
         (only-in "../../extensions/gsd/state-machine.rkt"
                  gsm-ctx-current
                  gsm-ctx-history
                  gsm-ctx-transition-to!)
         (only-in "../../extensions/gsd/session-state.rkt" make-gsd-context gsd-ctx-set-event-bus!)
         (only-in "../../util/loop-result.rkt" make-loop-result))

;; ============================================================
;; Trace record
;; ============================================================

;; Golden trace of one scenario. All fields are deterministic.
;; fsm: chronological list of (from to) FSM transitions.
;; record: normalized campaign record (see campaign->semantic).
;; projections: hash of plan-index / wave-docs / state-table / plan-overall.
;; outbox: list of completion event ids.
;; result: (hash 'status ... 'completed ...) of the campaign result.
;; events: ordered list of (event-name from to)-style entries.
(struct golden-trace (scenario commands fsm final-mode record projections outbox result events)
  #:transparent)

;; ============================================================
;; Deterministic fixture project
;; ============================================================

;; (index title slug) triples. Titles/slugs are fixed so manifest hashes,
;; plan ids, wave doc paths and content hashes are deterministic.
(define golden-wave-specs '((0 "Trace Wave Alpha" "alpha") (1 "Trace Wave Beta" "beta")))

(define (plan-index-line spec)
  (match spec
    [(list idx title slug) (format "- [Inbox] W~a: ~a → waves/W~a-~a.md" idx title idx slug)]))

;; Write a complete deterministic .planning/ fixture (PLAN.md, STATE.md,
;; per-wave docs) into an existing directory.
(define (seed-golden-project! dir [specs golden-wave-specs])
  (define planning (build-path dir ".planning"))
  (make-directory* (build-path planning "waves"))
  (call-with-output-file (build-path planning "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Golden Trace Campaign\n\n## Waves\n\n" out)
                           (for ([spec specs])
                             (displayln (plan-index-line spec) out)))
                         #:exists 'truncate)
  (call-with-output-file
   (build-path planning "STATE.md")
   (lambda (out)
     (display "# State: Golden Trace Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n" out)
     (for ([spec specs])
       (match spec
         [(list idx title _) (fprintf out "| W~a | ~a | PENDING |\n" idx title)])))
   #:exists 'truncate)
  (call-with-output-file
   (build-path planning "VALIDATION.md")
   (lambda (out)
     (display "# Validation: Golden Trace Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n" out)
     (for ([spec specs])
       (match spec
         [(list idx title _) (fprintf out "| W~a | ~a | PENDING |\n" idx title)])))
   #:exists 'truncate)
  (for ([spec specs])
    (match spec
      [(list idx _ slug)
       (call-with-output-file
        (build-path planning "waves" (format "W~a-~a.md" idx slug))
        (lambda (out)
          (fprintf out
                   "# Wave ~a\nStatus: Inbox\n\nDeterministic golden fixture content for wave ~a.\n"
                   idx
                   idx))
        #:exists 'truncate)]))
  dir)

;; Create a fresh temporary golden project.
(define (make-golden-project #:wave-specs [specs golden-wave-specs])
  (seed-golden-project! (make-temporary-file "gsd-golden-~a" 'directory) specs))

;; Rewrite the fixture with new wave specs (used by the replan scenario).
(define (rewrite-golden-plan! dir specs)
  (seed-golden-project! dir specs)
  dir)

(define (cleanup-golden-project! dir)
  (delete-directory/files dir #:must-exist? #f))

;; ============================================================
;; Semantic normalization
;; ============================================================

;; Campaign record -> deterministic semantic projection.
;; Timestamps (created-at/updated-at/attempt started-at), correlation ids and
;; random tokens never enter the trace.
(define (campaign->semantic rec)
  (hash 'plan-id
        (campaign-plan-id rec)
        'fence
        (campaign-fence-token rec)
        'provenance
        (campaign-record-provenance rec)
        'cancellation
        (and (campaign-record-cancellation rec)
             (hash 'reason (campaign-cancellation-reason (campaign-record-cancellation rec))))
        'waves
        (for/list ([w (campaign-record-waves rec)])
          (list (campaign-wave-index w)
                (campaign-wave-status w)
                (campaign-wave-attempt-count w)
                (and (campaign-wave-current-attempt w)
                     (list (campaign-attempt-id (campaign-wave-current-attempt w))
                           (campaign-attempt-fence-token (campaign-wave-current-attempt w))))))))

;; The durable record on disk (the oracle reads disk truth, not caller state).
(define (durable-record dir rec)
  (or (load-campaign-record dir (campaign-plan-id rec)) rec))

;; ============================================================
;; Projection capture
;; ============================================================

(define state-row-rx #rx"^\\| *W([0-9]+) *\\| *([^|]+) *\\| *([^|]+) *\\|")

(define (parse-state-rows path)
  (if (file-exists? path)
      (for/list ([line (string-split (call-with-input-file path port->string) "\n")]
                 #:when (regexp-match state-row-rx line))
        (let ([m (regexp-match state-row-rx line)])
          (list (string->number (cadr m)) (canonical-wave-status (string-trim (list-ref m 3))))))
      '()))

;; PLAN.md index statuses, wave doc statuses, STATE/VALIDATION rows, overall.
(define (capture-projections dir)
  (define plan-path (build-path dir ".planning" "PLAN.md"))
  (define entries
    (if (file-exists? plan-path)
        (parse-plan-index (call-with-input-file plan-path port->string))
        '()))
  (hash 'plan-index
        (for/list ([e entries])
          (list (wave-index-entry-idx e) (canonical-wave-status (wave-index-entry-status e))))
        'wave-docs
        (for/list ([e entries])
          (list (wave-index-entry-idx e)
                (let ([doc (read-wave-doc dir (wave-index-entry-idx e) (wave-index-entry-slug e))])
                  (if doc
                      (canonical-wave-status (hash-ref doc 'status))
                      'missing))))
        'state-table
        (parse-state-rows (build-path dir ".planning" "STATE.md"))
        'validation-table
        (parse-state-rows (build-path dir ".planning" "VALIDATION.md"))
        'plan-overall
        (plan-overall-status dir)))

;; ============================================================
;; Event collection
;; ============================================================

;; Extract a deterministic payload from a wrapped GSD event.
(define (event-payload name wrapped)
  (cond
    [(memq name '(gsd.transition.succeeded gsd.transition.attempted))
     (define d (and (hash? wrapped) (hash-ref wrapped 'data #f)))
     (list (and d (hash-ref d 'from #f)) (and d (hash-ref d 'to #f)))]
    [else #f]))

;; Collector wired into a session context's event bus.
(define (make-trace-collector)
  (define events (box '()))
  (values (lambda (name wrapped)
            (set-box! events (append (unbox events) (list (cons name (event-payload name wrapped))))))
          (lambda () (unbox events))))

;; ============================================================
;; FSM helper
;; ============================================================

;; Chronological (from to) transition list from the context history.
(define (chronological-fsm ctx)
  (map (lambda (h) (list (car h) (cadr h))) (reverse (gsm-ctx-history ctx))))

;; ============================================================
;; Campaign execution driver (production path)
;; ============================================================

;; outcome symbols: ok | cancelled | error
(define (outcome->loop-result outcome)
  (case outcome
    [(ok) (make-loop-result '() 'completed (hash))]
    [(cancelled) (make-loop-result '() 'cancelled (hash))]
    [(error) (make-loop-result '() 'error (hash))]
    [else (error 'outcome->loop-result "unknown outcome: ~s" outcome)]))

;; Run the production /go campaign machinery (request -> execute-campaign-request!)
;; with per-wave deterministic outcomes. Drives the FSM exactly like the real
;; /go handler callbacks: transition to 'executing before the run and to
;; 'verifying before completion. Returns the final campaign-result.
(define (run-golden-request! dir rec ctx #:outcomes outcomes #:approve? [approve? #t])
  (define request
    (make-campaign-request dir
                           rec
                           (lambda (wave-idx)
                             (gsm-ctx-transition-to! ctx 'executing)
                             (format "prompt-W~a" wave-idx))
                           (lambda (wave-idx)
                             (gsm-ctx-transition-to! ctx 'verifying)
                             approve?)))
  (define outcome-box (box 0))
  (execute-campaign-request! request
                             (lambda (prompt)
                               (define n (unbox outcome-box))
                               (set-box! outcome-box (add1 n))
                               (outcome->loop-result (list-ref outcomes n)))))

;; ============================================================
;; Trace capture
;; ============================================================

;; Wrap a scenario: create an isolated GSD context with an event collector,
;; run the thunk (which receives dir + ctx and returns (values rec result)),
;; then capture the full normalized trace.
(define (with-golden-trace scenario commands thunk)
  (define dir (make-golden-project))
  (dynamic-wind
   void
   (lambda ()
     (define ctx (make-gsd-context))
     (define-values (collect publish) (make-trace-collector))
     (gsd-ctx-set-event-bus! ctx collect)
     (define-values (rec result) (thunk dir ctx))
     (capture-golden-trace dir ctx (durable-record dir rec) scenario commands result (publish)))
   (lambda () (cleanup-golden-project! dir))))

;; campaign-result or a plain semantic result hash (for scenarios that end
;; without a campaign run).
(define (normalize-result result)
  (if (hash? result)
      result
      (hash 'status
            (campaign-result-status result)
            'completed
            (campaign-result-completed-waves result))))

;; Assemble the golden trace datum from a finished scenario.
(define (capture-golden-trace dir ctx rec scenario commands result events)
  (golden-trace scenario
                commands
                (chronological-fsm ctx)
                (gsm-ctx-current ctx)
                (campaign->semantic rec)
                (capture-projections dir)
                (load-outbox dir (campaign-plan-id rec))
                (normalize-result result)
                events))

;; ============================================================
;; Scenario drivers
;; ============================================================

;; plan-creation: durable output of a planning turn -> seeded campaign.
(define (scenario-plan-creation dir ctx)
  (values (migrate-campaign! dir) (hash 'status 'seeded 'completed '())))

;; go-success: every wave completes, verifier approves.
(define (scenario-go-success dir ctx)
  (define rec (migrate-campaign! dir))
  (values rec (run-golden-request! dir rec ctx #:outcomes '(ok ok))))

;; go-verifier-reject: runner succeeds but the verifier rejects -> FAILED,
;; DONE is never committed (verifier-first).
(define (scenario-go-verifier-reject dir ctx)
  (define rec (migrate-campaign! dir))
  (values rec (run-golden-request! dir rec ctx #:outcomes '(ok ok) #:approve? #f)))

;; go-failure: runner error on the first wave -> FAILED, no advancement.
(define (scenario-go-failure dir ctx)
  (define rec (migrate-campaign! dir))
  (values rec (run-golden-request! dir rec ctx #:outcomes '(error ok))))

;; go-interruption: runner cancellation -> INTERRUPTED, no advancement.
(define (scenario-go-interruption dir ctx)
  (define rec (migrate-campaign! dir))
  (values rec (run-golden-request! dir rec ctx #:outcomes '(cancelled ok))))

;; retry-interrupted: first run interrupted, second run completes the wave.
;; The second run must be a fresh process: reload the durable record.
(define (scenario-retry-interrupted dir ctx)
  (define rec (migrate-campaign! dir))
  (run-golden-request! dir rec ctx #:outcomes '(cancelled ok))
  (define plan-id (campaign-plan-id rec))
  (define rec2 (load-campaign-record dir plan-id))
  (values rec2 (run-golden-request! dir rec2 ctx #:outcomes '(ok ok))))

;; campaign-resume: first run completes W0 and fails W1; a fresh process
;; reloads the durable record and resumes, completing W1.
(define (scenario-campaign-resume dir ctx)
  (define rec (migrate-campaign! dir))
  (define first (run-golden-request! dir rec ctx #:outcomes '(ok error)))
  (define plan-id (campaign-plan-id rec))
  (define rec2 (load-campaign-record dir plan-id))
  (define second (run-golden-request! dir rec2 ctx #:outcomes '(ok)))
  (values rec2 second))

;; replan: after a completed campaign, the plan is rewritten and migration
;; seeds a fresh campaign identity (new plan-id), leaving the old record file.
(define (scenario-replan dir ctx)
  (define rec (migrate-campaign! dir))
  (run-golden-request! dir rec ctx #:outcomes '(ok ok))
  (define old-id (campaign-plan-id rec))
  (rewrite-golden-plan! dir '((0 "Trace Wave Gamma" "gamma")))
  (define new-rec (migrate-campaign! dir))
  (values new-rec (hash 'status 'replanned 'completed '() 'old-plan-id old-id)))

;; milestone-close: all waves done -> campaign-complete with both waves.
(define (scenario-milestone-close dir ctx)
  (define rec (migrate-campaign! dir))
  (values rec (run-golden-request! dir rec ctx #:outcomes '(ok ok))))

;; crash-between-commit-and-projection: commit W0 (DONE + outbox) and fail W1
;; via the production path, then simulate the crash by restoring the
;; projections to their pre-completion state (the process died between the
;; durable commit and the mark-wave-status!/update-state-table! calls). The
;; durable record stays committed; PLAN/STATE/wave projections are stale.
(define (scenario-crash-between-commit-and-projection dir ctx)
  (define rec (migrate-campaign! dir))
  (define plan-id (campaign-plan-id rec))
  ;; Production commit: W0 done (verifier-first), W1 failed -> campaign stops.
  (run-golden-request! dir rec ctx #:outcomes '(ok error))
  ;; CRASH: no projection update follows the commit. Restore the deterministic
  ;; fixture projections to their pre-completion state.
  (seed-golden-project! dir)
  (define rec2 (load-campaign-record dir plan-id))
  (values rec2 (hash 'status 'crash-injected 'completed '(0))))

;; ============================================================
;; Provide
;; ============================================================

(provide golden-trace
         golden-trace-scenario
         golden-trace-commands
         golden-trace-fsm
         golden-trace-final-mode
         golden-trace-record
         golden-trace-projections
         golden-trace-outbox
         golden-trace-result
         golden-trace-events
         golden-wave-specs
         make-golden-project
         seed-golden-project!
         rewrite-golden-plan!
         cleanup-golden-project!
         campaign->semantic
         durable-record
         capture-projections
         chronological-fsm
         run-golden-request!
         with-golden-trace
         capture-golden-trace
         scenario-plan-creation
         scenario-go-success
         scenario-go-verifier-reject
         scenario-go-failure
         scenario-go-interruption
         scenario-retry-interrupted
         scenario-campaign-resume
         scenario-replan
         scenario-milestone-close
         scenario-crash-between-commit-and-projection)
