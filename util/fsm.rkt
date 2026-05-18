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
;;
;; v0.47.1: Added define-fsm-machine macro to eliminate boilerplate.

(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(require racket/contract)

(provide (struct-out fsm)
         (struct-out fsm-state)
         (struct-out fsm-event)
         fsm-states
         fsm-events
         fsm-transitions
         fsm-state-name
         fsm-event-name
         (contract-out [make-fsm
                        (-> (listof symbol?)
                            (listof symbol?)
                            (listof (cons/c (cons/c symbol? symbol?) symbol?))
                            fsm?)]
                       [fsm-lookup (-> fsm? symbol? symbol? (or/c symbol? #f))]
                       [fsm-valid-transition? (-> fsm? symbol? symbol? boolean?)]
                       [fsm-valid-targets (-> fsm? symbol? (listof symbol?))]
                       [fsm-find-path (-> fsm? symbol? symbol? (or/c (listof symbol?) #f))])
         define-fsm-machine)

;; ── Typed state/event wrappers ──

(struct fsm-state (name) #:transparent)
(struct fsm-event (name) #:transparent)

;; ── FSM struct ──

(struct fsm (states events transitions) #:transparent)

(define (make-fsm states events transitions)
  (fsm states events transitions))

;; ── Lookup ──

(define (fsm-lookup machine state-sym event-sym)
  (for/or ([entry (in-list (fsm-transitions machine))])
    (match entry
      [`((,s . ,e) . ,next) (and (eq? s state-sym) (eq? e event-sym) next)]
      [_ #f])))

(define (fsm-valid-transition? machine state-sym event-sym)
  (and (fsm-lookup machine state-sym event-sym) #t))

(define (fsm-valid-targets machine state-sym)
  (for/list ([entry (in-list (fsm-transitions machine))]
             #:when (eq? (caar entry) state-sym))
    (cdr entry)))

;; ── BFS path finder ──

(define (fsm-find-path machine from-sym to-sym)
  (define visited (make-hash))
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

;; ============================================================
;; define-fsm-machine macro
;; ============================================================

(begin-for-syntax
  (define-syntax-class transition-clause
    #:datum-literals (->)
    (pattern [(from-sym:id -> to-sym:id) event-sym:id])))

(define-syntax (define-fsm-machine stx)
  (syntax-parse stx
    [(_ prefix:id
        (~seq #:states (state:id ...))
        (~seq #:events (event:id ...))
        (~seq #:transitions t:transition-clause ...))
     (with-syntax ([prefix-state? (format-id #'prefix "~a-state?" #'prefix)]
                   [prefix-event? (format-id #'prefix "~a-event?" #'prefix)]
                   [prefix-machine (format-id #'prefix "~a-machine" #'prefix)]
                   [prefix-valid-transition? (format-id #'prefix "~a-valid-transition?" #'prefix)]
                   [prefix-next-state (format-id #'prefix "~a-next-state" #'prefix)]
                   [(state-constructor ...) (for/list ([s (syntax->list #'(state ...))])
                                              (format-id s "~a-~a" #'prefix s))]
                   [(event-constructor ...) (for/list ([e (syntax->list #'(event ...))])
                                              (format-id e "~a-~a" #'prefix e))]
                   [(state-name ...) (syntax->list #'(state ...))]
                   [(event-name ...) (syntax->list #'(event ...))]
                   [(trans-from ...) (syntax->list #'(t.from-sym ...))]
                   [(trans-event ...) (syntax->list #'(t.event-sym ...))]
                   [(trans-to ...) (syntax->list #'(t.to-sym ...))])
       #'(begin
           ;; State singletons
           (define state-constructor (fsm-state 'state-name)) ...
           ;; Event singletons
           (define event-constructor (fsm-event 'event-name)) ...
           ;; Predicates
           (define (prefix-state? v)
             (and (fsm-state? v) (member (fsm-state-name v) '(state ...)) #t))
           (define (prefix-event? v)
             (and (fsm-event? v) (member (fsm-event-name v) '(event ...)) #t))
           ;; Machine instance
           (define prefix-machine
             (make-fsm '(state ...)
                       '(event ...)
                       (list (cons (cons 'trans-from 'trans-event) 'trans-to) ...)))
           ;; Transition validator
           (define (prefix-valid-transition? state-obj event-obj)
             (fsm-valid-transition? prefix-machine
                                    (fsm-state-name state-obj)
                                    (fsm-event-name event-obj)))
           ;; Next-state lookup
           (define (prefix-next-state state-obj event-obj)
             (define next-sym
               (fsm-lookup prefix-machine (fsm-state-name state-obj) (fsm-event-name event-obj)))
             (and next-sym
                  (case next-sym
                    [(state-name) state-constructor] ...
                    [else #f])))))]))
