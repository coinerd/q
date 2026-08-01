#lang racket/base

;; tui/commands/goal-bridge.rkt — adapter between goal-runner and TUI event bus
;;
;; Two functions:
;; 1. make-goal-event-bridge: maps goal-runner symbol events → typed event bus events
;; 2. make-goal-run-prompt!: wraps run-prompt! for goal-runner consumption

(require racket/match
         racket/string
         (only-in "../../runtime/agent-session.rkt" agent-session?)
         (only-in "../../runtime/session/session-lifecycle.rkt" run-prompt!)
         (only-in "../../runtime/session/session-store-goal-task.rkt"
                  load-goal-evaluations
                  load-goal-evidence)
         (only-in "../../runtime/goal/goal-state.rkt"
                  evaluation-result-achieved?
                  evaluation-result-reason
                  evaluation-result-model-used
                  evaluation-result-token-cost)
         (only-in "../../runtime/goal/goal-evidence.rkt"
                  evidence-provenance-evidence-id
                  evidence-provenance-kind
                  evidence-provenance-base-sha
                  evidence-provenance-tree-hash
                  evidence-provenance-captured-at
                  evidence-provenance-result
                  evidence-current?
                  evidence-stale?)
         "../../util/event/event-bus.rkt"
         "../../runtime/runtime-helpers.rkt")

(provide make-goal-event-bridge
         make-goal-run-prompt!
         render-goal-history
         render-goal-evidence)

;; ============================================================
;; Event bridge: goal-runner symbols → event bus strings
;; ============================================================

(define (make-goal-event-bridge event-bus session-id)
  ;; Maps goal-runner symbol events to typed event bus events.
  ;; event-bus: event-bus? — the live event bus from agent-session
  ;; session-id: string? — the session identifier
  ;; Returns: (-> symbol? hash? void?)
  (lambda (event-type payload)
    (define event-name
      (case event-type
        [(goal-started) "goal.started"]
        [(goal-turn-started) "goal.turn.started"]
        [(goal-turn-timed-out) "goal.turn.timed-out"]
        [(goal-evaluated) "goal.evaluated"]
        [(goal-check-completed) "goal.check.completed"]
        [(goal-achieved) "goal.achieved"]
        [(goal-failed) "goal.failed"]
        [else #f]))
    (when (and event-name event-bus)
      (emit-session-event! event-bus session-id event-name payload))))

;; ============================================================
;; run-prompt wrapper: adapts agent-session for goal-runner
;; ============================================================

(define (make-goal-run-prompt! sess)
  ;; Keep the latest session returned by run-prompt!. The session can be a new
  ;; value after lifecycle mutations; closing over the original would make each
  ;; later goal turn start from stale state.
  (define sess-box (box sess))
  (lambda (prompt)
    (define-values (updated-sess result) (run-prompt! (unbox sess-box) prompt))
    (when updated-sess
      (set-box! sess-box updated-sess))
    (values updated-sess result)))

;; ============================================================
;; /goal history — persisted evaluator decision trail (W1, G-8)
;; ============================================================

(define (render-goal-history log-path)
  ;; Renders the persisted evaluator decision trail from the session log.
  ;; log-path: (or/c path-string? #f) — session log; #f when no log is set.
  ;; Returns: string? — human-readable trail (turn, ok, reason, model, cost).
  (cond
    [(not log-path) "[goal] No session log available — no persisted evaluations."]
    [else
     (define entries (load-goal-evaluations log-path))
     (if (null? entries)
         "[goal] No evaluator decisions recorded yet."
         (string-join (cons "[goal] Evaluation history:"
                            (for/list ([e (in-list entries)])
                              (define turn (car e))
                              (define er (cdr e))
                              (format "  turn ~a | ok: ~a | ~a | model: ~a | cost: ~a"
                                      turn
                                      (if (evaluation-result-achieved? er) "yes" "no")
                                      (evaluation-result-reason er)
                                      (evaluation-result-model-used er)
                                      (evaluation-result-token-cost er))))
                      "\n"))]))

;; ============================================================
;; /goal evidence — persisted verification evidence provenance (W3, G-5)
;; ============================================================

(define (render-goal-evidence log-path base-sha tree-hash)
  ;; Renders the persisted verification evidence trail with current/stale
  ;; flags. Evidence is STALE when the base SHA or working-tree hash at
  ;; capture time differs from the current code state.
  ;; log-path: (or/c path-string? #f) — session log; #f when no log is set.
  ;; base-sha: (or/c string? #f) — current base SHA ("" or #f when unknown).
  ;; tree-hash: (or/c string? #f) — current working-tree hash.
  ;; Returns: string? — human-readable evidence trail.
  (define cur-base (or base-sha ""))
  (define cur-tree (or tree-hash ""))
  (cond
    [(not log-path) "[goal] No session log available — no persisted evidence."]
    [else
     (define entries (load-goal-evidence log-path))
     (if (null? entries)
         "[goal] No verification evidence recorded yet."
         (string-join
          (cons "[goal] Verification evidence:"
                (for/list ([ev (in-list entries)])
                  (define flag
                    (if (and (not (equal? cur-base ""))
                             (not (equal? cur-tree ""))
                             (evidence-current? ev cur-base cur-tree))
                        "CURRENT"
                        "STALE"))
                  (format "  ~a | id: ~a | kind: ~a | base: ~a | tree: ~a | at: ~a | result: ~a"
                          flag
                          (evidence-provenance-evidence-id ev)
                          (evidence-provenance-kind ev)
                          (evidence-provenance-base-sha ev)
                          (evidence-provenance-tree-hash ev)
                          (evidence-provenance-captured-at ev)
                          (evidence-provenance-result ev))))
          "\n"))]))
