#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: integration

;;; test-agent-iteration-di.rkt — Verify that Agent iteration can execute
;;; with injected (fake) runtime operations, without importing the concrete
;;; Runtime turn orchestrator.

(require rackunit
         racket/file
         racket/list
         "../agent/iteration/loop-config.rkt"
         "../agent/iteration/loop-state.rkt"
         (prefix-in ml: "../agent/iteration/main-loop.rkt")
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         (only-in "../util/loop-result.rkt"
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata
                  make-loop-result)
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../runtime/session/session-config.rkt" hash->session-config)
         (only-in "../runtime/iteration/directive.rkt" directive-stop))

;; ============================================================
;; Fake implementations — no runtime/turn-orchestrator.rkt needed
;; =============================================================

;; Fake context assembler: returns the context unchanged
(define (fake-build-context ctx config ext-reg bus session-id iteration #:session [session #f])
  ctx)

;; Fake provider turn: returns a single assistant message and 'completed
(define (fake-run-provider-turn ctx prov bus reg ext-reg session-id turn-id token config)
  (define msg
    (make-message "fake-response"
                  #f
                  'assistant
                  'message
                  (list (make-text-part "This is a fake response."))
                  (current-seconds)
                  (hasheq)))
  (make-loop-result (list msg) 'completed (hasheq)))

;; Fake step executor: returns directive-stop with the result
(define (fake-interpret-step step-res result new-msgs infra snapshot)
  (directive-stop result))

;; ============================================================
;; Tests
;; ============================================================

(test-case "run-iteration-loop/v2 executes with fake injected operations"
  (define bus (make-event-bus))
  (define cfg
    (make-loop-config '() ; context (empty — fake provider ignores it)
                      #f ; provider (not needed for fake)
                      bus
                      #f ; registry
                      #f ; ext-registry
                      "/tmp/test-di-log"
                      "test-di-session"
                      10
                      #:config (hash->session-config (hash))
                      #:build-context-fn fake-build-context
                      #:run-provider-turn-fn fake-run-provider-turn
                      #:interpret-step-fn fake-interpret-step))
  (define result (ml:run-iteration-loop/v2 cfg))
  (check-equal? (loop-result-termination-reason result) 'completed)
  (check-true (pair? (loop-result-messages result))
              "Should have at least one message from the fake provider turn"))

(test-case "run-iteration-loop/v2 errors when build-context-fn not supplied"
  (define bus (make-event-bus))
  (define cfg (make-loop-config '() #f bus #f #f "/tmp/test-di-log" "test-di-session" 10))
  (check-exn exn:fail?
             (lambda () (ml:run-iteration-loop/v2 cfg))
             "Should error when build-context-fn is not supplied"))

(test-case "v0.99.85: main-loop.rkt does not import runtime orchestration"
  ;; Structural verification: the agent iteration module should not
  ;; import from the runtime orchestration layer
  (define src
    (file->string (build-path (current-directory) ".." "agent" "iteration" "main-loop.rkt")))
  (check-false (string-contains? src "turn-orchestrator")
               "main-loop.rkt must not import the runtime orchestration layer"))

(test-case "v0.99.86: agent/iteration/ no longer has step-interpreter.rkt"
  (define path (build-path (current-directory) ".." "agent" "iteration" "step-interpreter.rkt"))
  (check-false (file-exists? path)
               "step-interpreter.rkt must not exist in agent/iteration/ after relocation"))
