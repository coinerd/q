#lang racket/base

;; runtime/context-assembly/ws-evolution.rkt — Working-set evolution on state transitions
;; v0.75.4 W0: Evolves working-set when task-state changes
;;
;; When the agent transitions from exploration to planning/implementation,
;; raw file contents in the working-set are replaced with conclusions,
;; reducing token count while preserving key information.

(require (only-in "../working-set.rkt"
                  working-set?
                  working-set-entries
                  working-set-reset!
                  working-set-entry-count
                  working-set-token-count
                  working-set-selective-remove!
                  ws-entry?
                  ws-entry-path)
         (only-in "task-state.rkt"
                  task-state?
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging)
         (only-in "task-conclusion.rkt" task-conclusion? task-conclusion-text)
         (only-in "../../util/fsm.rkt" fsm-state-name)
         racket/string)

(provide evolve-working-set-for-state
         working-set-selective-keep)

;; working-set-selective-keep : working-set? (-> any/c boolean?) → void?
;; Keep only entries matching predicate. Removes all others.
(define (working-set-selective-keep ws keep-pred?)
  (working-set-selective-remove! ws keep-pred?))

;; evolve-working-set-for-state : working-set? task-state? task-state? (listof task-conclusion?) → (listof task-conclusion?)
;;
;; Evolves the working-set based on state transition.
;; Returns the conclusions that should be injected.
;; old-state: the previous task state (or #f if unknown)
;; new-state: the new task state
;; conclusions: current conclusions list
(define (evolve-working-set-for-state ws old-state new-state conclusions)
  (define old-name (and old-state (fsm-state-name old-state)))
  (define new-name (and new-state (fsm-state-name new-state)))

  (cond
    ;; exploration → planning or implementation: clear ws, return conclusions
    [(and (eq? old-name 'exploration) (or (eq? new-name 'planning) (eq? new-name 'implementation)))
     (working-set-reset! ws)
     (filter task-conclusion? conclusions)]

    ;; implementation → debugging: keep error-related files
    [(and (eq? old-name 'implementation) (eq? new-name 'debugging))
     (working-set-selective-remove! ws
                                    (lambda (e)
                                      (define p (ws-entry-path e))
                                      (and (string? p)
                                           (or (string-contains? p "test")
                                               (string-contains? p "error")
                                               (string-contains? p "spec")))))
     (filter task-conclusion? conclusions)]

    ;; debugging → implementation: clear selective entries
    [(and (eq? old-name 'debugging) (eq? new-name 'implementation))
     (working-set-reset! ws)
     (filter task-conclusion? conclusions)]

    ;; any → idle: full reset
    [(eq? new-name 'idle)
     (working-set-reset! ws)
     '()]

    ;; No evolution needed for other transitions
    [else (filter task-conclusion? conclusions)]))
