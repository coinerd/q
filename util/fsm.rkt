#lang racket/base

;; util/fsm.rkt — Generic FSM transition library (F16)
;; STABILITY: evolving
;;
;; Shared transition-table machinery used by:
;;   runtime/iteration/fsm-types.rkt (iteration FSM)
;;   agent/loop-fsm.rkt (turn FSM)
;;   extensions/gsd/state-machine.rkt (GSD FSM)
;;
;; Each FSM is a data structure: (fsm states events transitions)
;; where transitions is a list of ((state-sym . event-sym) . next-state-sym).

(require racket/match)

(provide (struct-out fsm)
         make-fsm
         fsm-states
         fsm-events
         fsm-transitions
         fsm-lookup
         fsm-valid-transition?
         fsm-valid-targets
         fsm-find-path)

;; ── Struct ──

(struct fsm (states events transitions) #:transparent)

(define (make-fsm states events transitions)
  ;; states: (listof symbol?)
  ;; events: (listof symbol?)
  ;; transitions: (listof (cons (cons symbol? symbol?) symbol?))
  (fsm states events transitions))

;; ── Lookup ──

(define (fsm-lookup machine state-sym event-sym)
  ;; Returns next-state symbol or #f
  (for/or ([entry (in-list (fsm-transitions machine))])
    (match entry
      [`((,s . ,e) . ,next) (and (eq? s state-sym) (eq? e event-sym) next)]
      [_ #f])))

(define (fsm-valid-transition? machine state-sym event-sym)
  ;; Returns #t if transition exists, #f otherwise
  (and (fsm-lookup machine state-sym event-sym) #t))

(define (fsm-valid-targets machine state-sym)
  ;; Returns list of reachable next-state symbols from state-sym
  (for/list ([entry (in-list (fsm-transitions machine))]
             #:when (eq? (caar entry) state-sym))
    (cdr entry)))

;; ── BFS path finder ──

(define (fsm-find-path machine from-sym to-sym)
  ;; Returns list of event symbols to reach from → to, or #f if impossible
  (define visited (make-hash))
  ;; Queue entries: (cons node-sym path-events)
  (define q (list (cons from-sym '())))
  (let loop ([q q])
    (cond
      [(null? q) #f]
      [else
       (define node (car (car q)))
       (define path (cdr (car q)))
       (cond
         [(eq? node to-sym) path]
         [(hash-has-key? visited node) (loop (cdr q))]
         [else
          (hash-set! visited node #t)
          (define next-steps
            (for/list ([entry (in-list (fsm-transitions machine))]
                       #:when (eq? (caar entry) node)
                       #:unless (hash-has-key? visited (cdr entry)))
              (cons (cdr entry) (append path (list (cdar entry))))))
          (loop (append (cdr q) next-steps))])])))
