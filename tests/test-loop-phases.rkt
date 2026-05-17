#lang racket/base
;; BOUNDARY: pure

;; tests/test-loop-phases.rkt — Pure phase function tests (F1)

(require rackunit
         rackunit/text-ui
         "../agent/effect-types.rkt"
         "../agent/loop-phases.rkt"
         "../agent/loop-fsm.rkt"
         "../agent/event-bus.rkt"
         "../agent/state.rkt"
         "../util/message.rkt"
         (only-in "../llm/model.rkt" model-request?))

(define loop-phases-tests
  (test-suite "loop-phases"

    (test-case "phase-emit-start returns context and effects"
      (define-values (ctx effs) (phase-emit-start "sess" "turn" #f '()))
      (check-equal? ctx '())
      (check = (length effs) 2)
      (check-true (effect:emit-event? (car effs)))
      (check-true (effect:update-fsm? (cadr effs))))

    (test-case "phase-build-context returns raw messages and effects"
      (define bus (make-event-bus))
      (define st (make-loop-state "sess" "turn"))
      (define-values (raw effs)
        (phase-build-context bus
                             "sess"
                             "turn"
                             st
                             (list (message "id1" #f 'user 'user "hello" 0 (hash)))))
      (check-true (list? raw))
      (check = (length raw) 1)
      (check = (length effs) 1)
      (check-true (effect:update-fsm? (car effs))))

    (test-case "phase-build-request returns model request"
      (define-values (req effs) (phase-build-request '() #f #f))
      (check-true (model-request? req))
      (check-true (null? effs)))

    (test-case "phase-pre-hook returns #f without dispatcher"
      (define-values (result effs) (phase-pre-hook #f #f '() (hash) "sess" "turn"))
      (check-false result)
      (check-true (null? effs)))))

(run-tests loop-phases-tests)
