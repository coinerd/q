#lang racket/base

;; tests/ux-frontend-event-order-test.rkt -- Cross-frontend end-to-end event-order tests
;;
;; W6 (v1.00.02): drive an event-order matrix through BOTH production frontend
;; event subscribers -- the TUI registry reducers (q/tui/state-events/, wired
;; through `register-event-reducer!` + `apply-event-to-state`) and the GUI
;; `make-gui-event-subscriber` writing into a boxed gui-state -- and require
;; that both frontends end every ordering with:
;;   * the SAME full artifact body for each artifact,
;;   * the SAME stable artifact id,
;;   * the SAME artifact lifecycle state.
;; Companion to tests/ux-gui-tui-parity-test.rkt (which pins producer-event
;; semantics).  This suite extends the parity contract to the production
;; @speed fast  ;; @suite default
;; @boundary unit
;; frontend subscriptions and to event ORDER independence.  Only artifacts are
;; asserted; no private streaming buffers are inspected.
;;
;; Matrix: (a) normal order; (b) model.stream.completed AFTER
;; assistant.message.completed; (c) thinking deltas interleaved after content
;; deltas; (d) duplicate completion events; (e) thinking-only turn;
;; (f) cancellation mid-thinking; (g) runtime error mid-thinking.

(require rackunit
         "../tui/state.rkt"
         "../gui/gui-types.rkt"
         "../gui/state-sync.rkt"
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/conversation-reducer.rkt"
         "../util/event/event.rkt")

;; --------------------------------------------------------------------------
;; Event construction
;; --------------------------------------------------------------------------

(define SESSION "sess-e2e")
(define TURN "turn-e2e")

(define time-counter (box 1000))

(define (make-event* type payload)
  (define now (unbox time-counter))
  (set-box! time-counter (+ (unbox time-counter) 1))
  (make-event type now SESSION TURN payload))

;; event matrices -----------------------------------------------------------

(define (normal-order-events)
  (list (make-event* "turn.started" (hasheq 'scope "prompt"))
        (make-event* "model.stream.thinking" (hasheq 'delta "Plan: "))
        (make-event* "model.stream.thinking" (hasheq 'delta "step one."))
        (make-event* "model.stream.delta" (hasheq 'delta "Hello "))
        (make-event* "model.stream.delta" (hasheq 'delta "world."))
        (make-event* "model.stream.completed" (hasheq))
        (make-event* "assistant.message.completed" (hasheq 'content "Hello world."))
        (make-event* "turn.completed" (hasheq 'scope "prompt" 'reason "completed"))))

(define (reversed-terminal-events)
  ;; (b) model.stream.completed arrives BEFORE assistant.message.completed is
  ;; NOT the only reversed order worth pinning: pin BOTH terminal orders by
  ;; delivering assistant.message.completed first, then the stream marker.
  (list (make-event* "turn.started" (hasheq 'scope "prompt"))
        (make-event* "model.stream.thinking" (hasheq 'delta "Reason first. "))
        (make-event* "model.stream.delta" (hasheq 'delta "Answer second."))
        (make-event* "assistant.message.completed" (hasheq 'content "Answer second."))
        (make-event* "model.stream.completed" (hasheq))
        (make-event* "turn.completed" (hasheq 'scope "prompt" 'reason "completed"))))

(define (interleaved-thinking-events)
  ;; (c) thinking deltas arrive interleaved AFTER content deltas.
  (list (make-event* "turn.started" (hasheq 'scope "prompt"))
        (make-event* "model.stream.delta" (hasheq 'delta "one "))
        (make-event* "model.stream.thinking" (hasheq 'delta "t1 "))
        (make-event* "model.stream.delta" (hasheq 'delta "two "))
        (make-event* "model.stream.thinking" (hasheq 'delta "t2 "))
        (make-event* "model.stream.delta" (hasheq 'delta "three"))
        (make-event* "model.stream.thinking" (hasheq 'delta " t3"))
        (make-event* "model.stream.completed" (hasheq))
        (make-event* "assistant.message.completed" (hasheq 'content "one two three"))
        (make-event* "turn.completed" (hasheq 'scope "prompt" 'reason "completed"))))

(define (duplicate-completion-events)
  (list (make-event* "turn.started" (hasheq 'scope "prompt"))
        (make-event* "model.stream.thinking" (hasheq 'delta "Reasoning body."))
        (make-event* "model.stream.delta" (hasheq 'delta "answer"))
        (make-event* "model.stream.completed" (hasheq))
        (make-event* "model.stream.completed" (hasheq))
        (make-event* "model.stream.completed" (hasheq))
        (make-event* "assistant.message.completed" (hasheq 'content "answer"))
        (make-event* "assistant.message.completed" (hasheq 'content "answer"))
        (make-event* "turn.completed" (hasheq 'scope "prompt" 'reason "completed"))))

(define (thinking-only-events)
  ;; (e) model emits reasoning and no visible content.
  (list (make-event* "turn.started" (hasheq 'scope "prompt"))
        (make-event* "model.stream.thinking" (hasheq 'delta "Only thinking. "))
        (make-event* "model.stream.thinking" (hasheq 'delta "No answer."))
        (make-event* "model.stream.completed" (hasheq))
        (make-event* "assistant.message.completed" (hasheq 'content ""))
        (make-event* "turn.completed" (hasheq 'scope "prompt" 'reason "completed"))))

(define (cancelled-mid-thinking-events)
  ;; (f) cancellation while reasoning is still streaming.
  (list (make-event* "turn.started" (hasheq 'scope "prompt"))
        (make-event* "model.stream.thinking" (hasheq 'delta "Partial reasoning before cancel."))
        (make-event* "turn.cancelled" (hasheq))))

(define (error-mid-thinking-events)
  ;; (g) provider error while reasoning is still streaming.
  (list (make-event* "turn.started" (hasheq 'scope "prompt"))
        (make-event* "model.stream.thinking" (hasheq 'delta "Partial reasoning before error."))
        (make-event* "runtime.error" (hasheq 'error "provider exploded mid-thinking"))))

;; --------------------------------------------------------------------------
;; Production adapters
;; --------------------------------------------------------------------------

;; TUI: the real registry reducers via the production apply-event-to-state.
(define (run-tui events)
  (for/fold ([st (initial-ui-state)]) ([evt (in-list events)])
    (apply-event-to-state st evt)))

;; GUI: the production subscriber writing into a boxed gui-state.
(define (run-gui events)
  (define box* (box (make-gui-state)))
  (define on-event (make-gui-event-subscriber box*))
  (for ([evt (in-list events)])
    (on-event evt))
  (unbox box*))

;; --------------------------------------------------------------------------
;; Artifact views (parity is asserted on artifacts, never on private buffers)
;; --------------------------------------------------------------------------

;; reducer-artifact-view :: (Option artifact) -> list or #f
;; Canonical comparable view: kind/session/turn/id/body/lifecycle.
(define (artifact-view artifact)
  (and artifact
       (list (conversation-artifact-kind artifact)
             (conversation-artifact-session-id artifact)
             (conversation-artifact-turn-id artifact)
             (conversation-artifact-id artifact)
             (conversation-artifact-body artifact)
             (conversation-artifact-lifecycle artifact))))

(define (tui-thinking st)
  (artifact-view (reducer-thinking-artifact (ui-state-conversation-reducer st) SESSION TURN)))
(define (tui-assistant st)
  (artifact-view (reducer-assistant-artifact (ui-state-conversation-reducer st) SESSION TURN)))
(define (gui-thinking st)
  (artifact-view (reducer-thinking-artifact (gui-state-conversation-reducer st) SESSION TURN)))
(define (gui-assistant st)
  (artifact-view (reducer-assistant-artifact (gui-state-conversation-reducer st) SESSION TURN)))

;; Stable ids: both frontends must agree on the canonical artifact ids, and
;; they must equal the ids derived from the identity triple alone.
(define EXPECTED-THINKING-ID (make-artifact-id SESSION TURN 'thinking))
(define EXPECTED-ASSISTANT-ID (make-artifact-id SESSION TURN 'assistant))

;; Projected artifacts: what each frontend actually emitted into its render
;; surface (TUI transcript entries / GUI messages), identified by artifact meta.
(define (tui-projected-artifacts st)
  (for/list ([entry (in-list (ui-state-transcript st))]
             #:when (hash-ref (transcript-entry-meta entry) 'artifact #f))
    (artifact-view (hash-ref (transcript-entry-meta entry) 'artifact))))

;; GUI messages are stored oldest-first; TUI transcript entries are stored
;; newest-first.  Reverse the GUI projection so both views are newest-first
;; and directly comparable.
(define (gui-projected-artifacts st)
  (for/list ([msg (in-list (reverse (gui-state-messages st)))]
             #:when (hash-ref (gui-message-meta msg) 'artifact #f))
    (artifact-view (hash-ref (gui-message-meta msg) 'artifact))))

;; --------------------------------------------------------------------------
;; Scenario driver
;; --------------------------------------------------------------------------

;; run-scenario :: string events -> void
;; Runs BOTH production adapters over the event matrix, then asserts full
;; artifact parity (body + id + lifecycle) against each other and against the
;; expected specs.  When #:projection is a list, additionally asserts both
;; frontends projected exactly that artifact sequence (render order, most
;; recent first); when #:terminal?, asserts both frontends returned to rest.
(define (run-scenario name
                      events
                      #:thinking-body thinking-body
                      #:thinking-lifecycle thinking-lifecycle
                      #:assistant-body assistant-body
                      #:assistant-lifecycle assistant-lifecycle
                      #:projection projection-spec
                      #:terminal? terminal?)
  ;; Production wiring: the runtime publishes session.started before any turn,
  ;; establishing the session identity both adapters key their stream
  ;; activation and terminal handling on (without it, terminal reducers are
  ;; fail-closed by design).  The GUI renders it as a plain system message,
  ;; which carries no artifact meta and is filtered from projections.
  (define session-start (make-event* "session.started" (hasheq)))
  (define tui-st (run-tui (cons session-start events)))
  (define gui-st (run-gui (cons session-start events)))

  ;; Expected canonical views.
  (define expected-thinking
    (list 'thinking SESSION TURN EXPECTED-THINKING-ID thinking-body thinking-lifecycle))
  (define expected-assistant
    (and assistant-body
         (list 'assistant SESSION TURN EXPECTED-ASSISTANT-ID assistant-body assistant-lifecycle)))

  (test-case name
    ;; Both frontends reach the same thinking artifact...
    (check-equal? (tui-thinking tui-st) (gui-thinking gui-st) "frontend thinking artifacts diverged")
    (check-equal? (tui-thinking tui-st) expected-thinking "unexpected TUI thinking artifact")
    (check-equal? (gui-thinking gui-st) expected-thinking "unexpected GUI thinking artifact")
    ;; ...the same assistant artifact (present or absent together)...
    (check-equal? (tui-assistant tui-st)
                  (gui-assistant gui-st)
                  "frontend assistant artifacts diverged")
    (check-equal? (tui-assistant tui-st) expected-assistant "unexpected TUI assistant artifact")
    (check-equal? (gui-assistant gui-st) expected-assistant "unexpected GUI assistant artifact")
    ;; ...and stable ids across both frontends.
    (check-equal? (list-ref (tui-thinking tui-st) 3) (list-ref (gui-thinking gui-st) 3))
    (check-true (string=? (list-ref (tui-thinking tui-st) 3) EXPECTED-THINKING-ID)
                "thinking artifact id is not the canonical stable id")

    ;; Projection parity: what each frontend rendered for this turn must be
    ;; identical artifact projections (same bodies, ids, lifecycles).
    (when (list? projection-spec)
      (check-equal? (tui-projected-artifacts tui-st)
                    (gui-projected-artifacts gui-st)
                    "projected artifacts diverged between frontends")
      (check-equal? (gui-projected-artifacts gui-st) projection-spec "unexpected projection"))

    ;; Turn lifecycle parity for terminal scenarios: both frontends must be
    ;; back at rest (GUI idle, TUI not busy) regardless of event order.
    (when terminal?
      (check-equal? (gui-state-status gui-st) 'idle "GUI did not return to idle")
      (check-false (ui-state-active-turn-id tui-st) "TUI still has an active turn")
      (check-false (ui-state-active-model-turn-id tui-st) "TUI still has an active model turn"))))

;; --------------------------------------------------------------------------
;; The event-order matrix
;; --------------------------------------------------------------------------

;; (a) Normal order: thinking deltas, content deltas, stream completion,
;; assistant completion, turn completion.
(run-scenario "e2e: normal order - both frontends converge on identical retained artifacts"
              (normal-order-events)
              #:thinking-body "Plan: step one."
              #:thinking-lifecycle 'retained
              #:assistant-body "Hello world."
              #:assistant-lifecycle 'retained
              #:projection
              (list (list 'assistant SESSION TURN EXPECTED-ASSISTANT-ID "Hello world." 'retained)
                    (list 'thinking SESSION TURN EXPECTED-THINKING-ID "Plan: step one." 'retained))
              #:terminal? #t)

;; (b) Reversed terminals: assistant.message.completed BEFORE
;; model.stream.completed.
(run-scenario "e2e: assistant.message.completed before model.stream.completed"
              (reversed-terminal-events)
              #:thinking-body "Reason first. "
              #:thinking-lifecycle 'retained
              #:assistant-body "Answer second."
              #:assistant-lifecycle 'retained
              #:projection
              (list (list 'assistant SESSION TURN EXPECTED-ASSISTANT-ID "Answer second." 'retained)
                    (list 'thinking SESSION TURN EXPECTED-THINKING-ID "Reason first. " 'retained))
              #:terminal? #t)

;; (c) Thinking deltas interleaved after content deltas.
(run-scenario "e2e: thinking deltas interleaved after content deltas"
              (interleaved-thinking-events)
              #:thinking-body "t1 t2  t3"
              #:thinking-lifecycle 'retained
              #:assistant-body "one two three"
              #:assistant-lifecycle 'retained
              #:projection
              (list (list 'assistant SESSION TURN EXPECTED-ASSISTANT-ID "one two three" 'retained)
                    (list 'thinking SESSION TURN EXPECTED-THINKING-ID "t1 t2  t3" 'retained))
              #:terminal? #t)

;; (d) Duplicate completion events (3x stream completed, 2x assistant
;; completed): artifacts must be deduplicated, single-stable-id, retained.
(run-scenario "e2e: duplicate completion events do not duplicate or corrupt artifacts"
              (duplicate-completion-events)
              #:thinking-body "Reasoning body."
              #:thinking-lifecycle 'retained
              #:assistant-body "answer"
              #:assistant-lifecycle 'retained
              #:projection
              (list (list 'assistant SESSION TURN EXPECTED-ASSISTANT-ID "answer" 'retained)
                    (list 'thinking SESSION TURN EXPECTED-THINKING-ID "Reasoning body." 'retained))
              #:terminal? #t)

;; (e) Thinking-only turn: thinking retained; the canonical empty assistant
;; artifact is retained internally; NO phantom assistant row is projected by
;; either frontend.
(run-scenario
 "e2e: thinking-only turn - thinking retained, no phantom assistant projection"
 (thinking-only-events)
 #:thinking-body "Only thinking. No answer."
 #:thinking-lifecycle 'retained
 #:assistant-body ""
 #:assistant-lifecycle 'retained
 #:projection
 (list (list 'thinking SESSION TURN EXPECTED-THINKING-ID "Only thinking. No answer." 'retained))
 #:terminal? #t)

;; (f) Cancellation mid-thinking: no assistant artifact exists yet; thinking
;; artifact is present in BOTH frontends' reducers with the same body and
;; lifecycle.  (Mid-stream scenarios assert artifact parity only: projection
;; strategy during streaming is a frontend presentation concern, the shared
;; contract is the artifact state.)
(run-scenario "e2e: cancellation mid-thinking - partial thinking artifacts match across frontends"
              (cancelled-mid-thinking-events)
              #:thinking-body "Partial reasoning before cancel."
              #:thinking-lifecycle 'streaming
              #:assistant-body #f
              #:assistant-lifecycle #f
              #:projection #f
              #:terminal? #f)

;; (g) Runtime error mid-thinking.
(run-scenario "e2e: runtime error mid-thinking - partial thinking artifacts match across frontends"
              (error-mid-thinking-events)
              #:thinking-body "Partial reasoning before error."
              #:thinking-lifecycle 'streaming
              #:assistant-body #f
              #:assistant-lifecycle #f
              #:projection #f
              #:terminal? #f)
