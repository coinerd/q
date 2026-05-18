#lang racket/base

;; tests/test-fsm-macro.rkt — Tests for define-fsm-machine macro
;;
;; TDD tests for the FSM machine definition macro in util/fsm.rkt.
;; The macro generates state constructors, transition validators,
;; guard accessors, and state predicates from a declarative spec.

(require rackunit
         "../util/fsm.rkt")

;; ============================================================
;; Basic machine definition
;; ============================================================

(test-case "define-fsm-machine generates state constructors"
  (define-fsm-machine test-mach
    #:states (idle running stopped)
    #:events (start stop reset)
    #:transitions
    [(idle -> running) start]
    [(running -> stopped) stop]
    [(stopped -> idle) reset])
  ;; Should generate constructor: test-mach-<state>
  (check-true (fsm-state? (test-mach-idle)))
  (check-true (fsm-state? (test-mach-running)))
  (check-true (fsm-state? (test-mach-stopped)))
  (check-equal? (fsm-state-name (test-mach-idle)) 'idle)
  (check-equal? (fsm-state-name (test-mach-running)) 'running)
  (check-equal? (fsm-state-name (test-mach-stopped)) 'stopped))

(test-case "define-fsm-machine generates event constructors"
  (define-fsm-machine test-mach
    #:states (idle running)
    #:events (start stop)
    #:transitions
    [(idle -> running) start]
    [(running -> idle) stop])
  (check-true (fsm-event? (test-mach-start)))
  (check-true (fsm-event? (test-mach-stop)))
  (check-equal? (fsm-event-name (test-mach-start)) 'start)
  (check-equal? (fsm-event-name (test-mach-stop)) 'stop))

(test-case "define-fsm-machine generates valid-transition? predicate"
  (define-fsm-machine test-mach
    #:states (idle running stopped)
    #:events (start stop reset)
    #:transitions
    [(idle -> running) start]
    [(running -> stopped) stop]
    [(stopped -> idle) reset])
  ;; valid transition
  (check-true (test-mach-valid-transition? (test-mach-idle) (test-mach-start)))
  ;; invalid transition
  (check-false (test-mach-valid-transition? (test-mach-idle) (test-mach-stop))))

(test-case "define-fsm-machine generates next-state lookup"
  (define-fsm-machine test-mach
    #:states (idle running stopped)
    #:events (start stop reset)
    #:transitions
    [(idle -> running) start]
    [(running -> stopped) stop]
    [(stopped -> idle) reset])
  (define next (test-mach-next-state (test-mach-idle) (test-mach-start)))
  (check-equal? (fsm-state-name next) 'running)
  ;; invalid transition returns #f
  (check-false (test-mach-next-state (test-mach-idle) (test-mach-stop))))

(test-case "define-fsm-machine generates machine instance"
  (define-fsm-machine test-mach
    #:states (idle running)
    #:events (start stop)
    #:transitions
    [(idle -> running) start]
    [(running -> idle) stop])
  (check-true (fsm? test-mach-machine))
  (check-equal? (fsm-states test-mach-machine) '(idle running))
  (check-equal? (fsm-events test-mach-machine) '(start stop)))

(test-case "define-fsm-machine generates state? predicate"
  (define-fsm-machine test-mach
    #:states (idle running)
    #:events (start)
    #:transitions
    [(idle -> running) start])
  (check-true (test-mach-state? (test-mach-idle)))
  (check-true (test-mach-state? (test-mach-running)))
  (check-false (test-mach-state? 'not-a-state)))

(test-case "define-fsm-machine generates event? predicate"
  (define-fsm-machine test-mach
    #:states (idle running)
    #:events (start stop)
    #:transitions
    [(idle -> running) start]
    [(running -> idle) stop])
  (check-true (test-mach-event? (test-mach-start)))
  (check-true (test-mach-event? (test-mach-stop)))
  (check-false (test-mach-event? 'not-an-event)))

(test-case "define-fsm-machine handles terminal/self-loop transitions"
  (define-fsm-machine test-mach
    #:states (idle done)
    #:events (start finish)
    #:transitions
    [(idle -> done) finish]
    [(done -> done) finish])
  ;; Self-loop
  (check-true (test-mach-valid-transition? (test-mach-done) (test-mach-finish)))
  (define next (test-mach-next-state (test-mach-done) (test-mach-finish)))
  (check-equal? (fsm-state-name next) 'done))

(test-case "define-fsm-machine with single transition"
  (define-fsm-machine simple
    #:states (a b)
    #:events (go)
    #:transitions
    [(a -> b) go])
  (check-true (simple-valid-transition? (simple-a) (simple-go)))
  (check-false (simple-valid-transition? (simple-b) (simple-go))))

(test-case "fsm-state-name returns correct symbol"
  (define-fsm-machine names
    #:states (alpha beta gamma)
    #:events (advance)
    #:transitions
    [(alpha -> beta) advance]
    [(beta -> gamma) advance])
  (check-equal? (fsm-state-name (names-alpha)) 'alpha)
  (check-equal? (fsm-state-name (names-beta)) 'beta)
  (check-equal? (fsm-state-name (names-gamma)) 'gamma))
