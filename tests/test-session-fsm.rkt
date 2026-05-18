#lang racket/base

;; tests/test-session-fsm.rkt — Session lifecycle FSM tests (W2 v0.47.1)
;; Tests the derived FSM overlay on agent-session boolean flags.

(require rackunit
         (only-in "../runtime/session-fsm.rkt"
                  session-lifecycle-machine
                  session-lifecycle-state?
                  session-lifecycle-event?
                  session-lifecycle-active
                  session-lifecycle-streaming
                  session-lifecycle-compacting
                  session-lifecycle-idle
                  session-lifecycle-terminated
                  session-current-state
                  session-current-state-name
                  session-valid-lifecycle?
                  session-can-transition?)
         (only-in "../runtime/session-types.rkt"
                  agent-session
                  agent-session-active?
                  agent-session-compacting?
                  agent-session-prompt-running?))

;; Minimal session struct for testing (no dependencies)
(define (make-test-session #:active? [active? #t]
                           #:compacting? [compacting? #f]
                           #:prompt-running? [prompt-running? #f])
  (agent-session "test-id" "/tmp/test" #f #f #f #f
                 #f '() #f #f #f active? 0
                 compacting? #f #f '() #f #f #f prompt-running?))

(test-case "session-lifecycle FSM machine has correct states"
  (check-not-false (session-lifecycle-state? session-lifecycle-active))
  (check-not-false (session-lifecycle-state? session-lifecycle-streaming))
  (check-not-false (session-lifecycle-state? session-lifecycle-compacting))
  (check-not-false (session-lifecycle-state? session-lifecycle-idle))
  (check-not-false (session-lifecycle-state? session-lifecycle-terminated)))

(test-case "session-current-state: active session returns active state"
  (define sess (make-test-session #:active? #t))
  (check-equal? (session-current-state-name sess) 'active))

(test-case "session-current-state: compacting session returns compacting state"
  (define sess (make-test-session #:active? #t #:compacting? #t))
  (check-equal? (session-current-state-name sess) 'compacting))

(test-case "session-current-state: streaming session returns streaming state"
  (define sess (make-test-session #:active? #t #:prompt-running? #t))
  (check-equal? (session-current-state-name sess) 'streaming))

(test-case "session-current-state: inactive session returns terminated state"
  (define sess (make-test-session #:active? #f))
  (check-equal? (session-current-state-name sess) 'terminated))

(test-case "session-valid-lifecycle? returns #t for all valid states"
  (check-true (session-valid-lifecycle? (make-test-session #:active? #t)))
  (check-true (session-valid-lifecycle? (make-test-session #:active? #f)))
  (check-true (session-valid-lifecycle? (make-test-session #:active? #t #:compacting? #t)))
  (check-true (session-valid-lifecycle? (make-test-session #:active? #t #:prompt-running? #t))))

(test-case "session-can-transition?: valid transitions"
  (define active-sess (make-test-session #:active? #t))
  (check-true (session-can-transition? active-sess 'stream-requested))
  (check-true (session-can-transition? active-sess 'compaction-needed))
  (check-true (session-can-transition? active-sess 'session-ended)))

(test-case "session-can-transition?: invalid transitions"
  (define active-sess (make-test-session #:active? #t))
  (check-false (session-can-transition? active-sess 'session-started)))

(test-case "compacting takes priority over streaming"
  ;; Edge case: both compacting? and prompt-running? are #t
  (define sess (make-test-session #:active? #t #:compacting? #t #:prompt-running? #t))
  (check-equal? (session-current-state-name sess) 'compacting))

(test-case "session-lifecycle-machine is valid FSM"
  (check-true (session-can-transition? (make-test-session #:active? #t) 'turn-complete))
  (define idle-sess (make-test-session #:active? #t))
  ;; After turn-complete, should transition to idle
  (check-true (session-can-transition? idle-sess 'turn-complete)))
