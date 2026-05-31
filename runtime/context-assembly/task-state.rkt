#lang racket/base

;; runtime/context-assembly/task-state.rkt — Task-state FSM for context optimization
;; STABILITY: evolving
;;
;; Defines FSM states for tracking what the agent is doing (exploring,
;; planning, implementing, verifying, debugging) and uses that state to
;; optimize context assembly — replacing raw file contents with agent-authored
;; conclusions once exploration is complete.
;;
;; Uses define-fsm-machine macro from util/fsm.rkt (proven since v0.47.1).
;; 6 states, 8 events, 18 transitions (including explicit "any->X" expansions).

(require racket/contract
         (only-in "../../util/fsm.rkt"
                  define-fsm-machine
                  fsm-state
                  fsm-state-name
                  fsm-state?
                  fsm-event
                  fsm-event-name
                  fsm-event?
                  fsm?
                  fsm-states
                  fsm-events
                  fsm-transitions
                  fsm-lookup
                  fsm-valid-transition?))

;; ── Machine definition ──
;;
;; Transition graph:
;;   idle ──begin-explore──→ exploration ──begin-plan──→ planning
;;     │                        │                          │
;;     │                        └──begin-implement──→ implementation
;;     │                                                │
;;     │                           ┌──begin-verify──────┤
;;     │                           │                    └──begin-debug──┐
;;     │                           ▼                                     ▼
;;     │                      verification                         debugging
;;     │                           │                                    │
;;     └─────── task-complete ◄───┘      ┌──── begin-implement ────────┘
;;                              │        └──── begin-verify
;;                              └──begin-debug──→ debugging
;;
;;   revisit: any state → exploration
;;   task-complete: any state → idle
;;   force-transition: any state → any state (escape hatch)

(define-fsm-machine task
                    #:states (idle exploration planning implementation verification debugging)
                    #:events (begin-explore begin-plan
                                            begin-implement
                                            begin-verify
                                            begin-debug
                                            task-complete
                                            revisit
                                            force-transition)
                    #:transitions
                    ;; Core forward path
                    [(idle -> exploration) begin-explore]
                    [(exploration -> planning) begin-plan]
                    [(exploration -> implementation) begin-implement]
                    [(planning -> implementation) begin-implement]
                    [(implementation -> verification) begin-verify]
                    [(implementation -> debugging) begin-debug]
                    [(verification -> idle) task-complete]
                    [(verification -> debugging) begin-debug]
                    [(debugging -> implementation) begin-implement]
                    [(debugging -> verification) begin-verify]
                    ;; revisit: any → exploration (explicit for each state)
                    [(idle -> exploration) revisit]
                    [(exploration -> exploration) revisit]
                    [(planning -> exploration) revisit]
                    [(implementation -> exploration) revisit]
                    [(verification -> exploration) revisit]
                    [(debugging -> exploration) revisit]
                    ;; task-complete: any → idle (explicit for non-idle states)
                    [(exploration -> idle) task-complete]
                    [(planning -> idle) task-complete]
                    [(implementation -> idle) task-complete]
                    [(debugging -> idle) task-complete]
                    ;; force-transition: escape hatch for any state
                    [(idle -> exploration) force-transition]
                    [(idle -> planning) force-transition]
                    [(idle -> implementation) force-transition]
                    [(idle -> verification) force-transition]
                    [(idle -> debugging) force-transition]
                    [(exploration -> idle) force-transition]
                    [(exploration -> planning) force-transition]
                    [(exploration -> implementation) force-transition]
                    [(exploration -> verification) force-transition]
                    [(exploration -> debugging) force-transition]
                    [(planning -> idle) force-transition]
                    [(planning -> exploration) force-transition]
                    [(planning -> implementation) force-transition]
                    [(planning -> verification) force-transition]
                    [(planning -> debugging) force-transition]
                    [(implementation -> idle) force-transition]
                    [(implementation -> exploration) force-transition]
                    [(implementation -> planning) force-transition]
                    [(implementation -> verification) force-transition]
                    [(implementation -> debugging) force-transition]
                    [(verification -> idle) force-transition]
                    [(verification -> exploration) force-transition]
                    [(verification -> planning) force-transition]
                    [(verification -> implementation) force-transition]
                    [(verification -> debugging) force-transition]
                    [(debugging -> idle) force-transition]
                    [(debugging -> exploration) force-transition]
                    [(debugging -> planning) force-transition]
                    [(debugging -> implementation) force-transition]
                    [(debugging -> verification) force-transition])

;; ── Exports ──

;; FSM machine
(provide task-machine
         ;; State predicates
         task-state?
         ;; State singletons
         task-idle
         task-exploration
         task-planning
         task-implementation
         task-verification
         task-debugging
         ;; Event singletons
         task-begin-explore
         task-begin-plan
         task-begin-implement
         task-begin-verify
         task-begin-debug
         task-task-complete
         task-revisit
         task-force-transition
         ;; Transition helpers
         task-valid-transition?
         task-next-state
         ;; Contracts
         (contract-out [task-states-list (-> (listof symbol?))]
                       [task-events-list (-> (listof symbol?))]))

;; ── Convenience accessors ──

(define (task-states-list)
  (fsm-states task-machine))

(define (task-events-list)
  (fsm-events task-machine))
