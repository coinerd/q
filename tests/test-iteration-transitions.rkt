#lang racket

;; tests/test-iteration-transitions.rkt — Wave 9: I1-I6 iteration pipeline tests
;;
;; Tests for tool call extraction, hook modification, scheduler result mapping,
;; and iteration control flow in run-iteration-loop.

(require rackunit
         rackunit/text-ui
         racket/list
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../agent/event-bus.rkt"
         "../agent/queue.rkt"
         "../runtime/iteration.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool make-tool-registry register-tool!
                  make-success-result make-error-result
                  tool-result-content tool-result-is-error?)
         (only-in "../extensions/hooks.rkt" dispatch-hooks)
         (only-in "../extensions/api.rkt"
                  make-extension-registry register-extension!
                  extension)
         (only-in "../util/ids.rkt" generate-id)
         "helpers/mock-provider.rkt")

;; ============================================================
;; Test helpers
;; ============================================================

(define (make-user-message text)
  (make-message (generate-id) #f 'user 'user
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define temp-log-path "/tmp/test-iteration-transitions.log")

(define (cleanup-temp-log!)
  (when (file-exists? temp-log-path)
    (delete-file temp-log-path)))

;; Create a mock extension-registry with hook handlers.
;; Arguments: alternating hook-point-symbol handler-proc
(define (make-mock-ext-reg . hook-dispatcher-pairs)
  (define reg (make-extension-registry))
  (define hooks-hash
    (let loop ([pairs hook-dispatcher-pairs] [h (hasheq)])
      (if (null? pairs) h
          (loop (cddr pairs) (hash-set h (car pairs) (cadr pairs))))))
  (define ext (extension "mock-ext" "0.1" "1.0" hooks-hash))
  (register-extension! reg ext)
  reg)

;; Make a read tool
(define (make-read-tool)
  (make-tool "read" "Read a file" (hasheq)
             (lambda (args ctx) (make-success-result "mock file content"))))

;; Make a bash tool
(define (make-bash-tool)
  (make-tool "bash" "Run bash" (hasheq)
             (lambda (args ctx) (make-success-result "bash output"))))

;; Provider responses for tool-call-then-text pattern
(define (tool-call-then-text-responses tool-name tool-args)
  (list (make-model-response
         (list (hash 'type "tool-call" 'id "tc-1" 'name tool-name
                     'arguments tool-args))
         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
         "mock" 'tool-calls)
        (make-model-response
         (list (hash 'type "text" 'text "Done"))
         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
         "mock" 'stop)))

;; Provider responses: always tool-calls (never terminates on its own)
(define (always-tool-call-responses tool-name tool-args)
  (list (make-model-response
         (list (hash 'type "tool-call" 'id "tc-1" 'name tool-name
                     'arguments tool-args))
         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
         "mock" 'tool-calls)))

(define iteration-transition-tests
  (test-suite
   "Iteration Pipeline Transition Tests"

   ;; ============================================================
   ;; I1: iteration loop with tool-calls-pending then text response
   ;; ============================================================
   (test-case
    "I1: iteration loop with tool-calls-pending then text response"
    (cleanup-temp-log!)
    (define bus (make-event-bus))
    (define prov (make-multi-mock-provider
                  (tool-call-then-text-responses "read" (hasheq 'path "/tmp/test"))))
    (define reg (make-tool-registry))
    (register-tool! reg (make-read-tool))

    (define result
      (run-iteration-loop
       (list (make-user-message "test"))
       prov bus reg #f temp-log-path "sess-i1" 5))
    (check-equal? (loop-result-termination-reason result) 'completed
                  "iteration completes after tool call then text")
    (cleanup-temp-log!))

   ;; ============================================================
   ;; I2: tool-call hook amends tool calls
   ;; ============================================================
   (test-case
    "I2: tool-call hook amends tool calls"
    (cleanup-temp-log!)
    (define bus (make-event-bus))
    (define events (box '()))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    (define (amending-hook payload)
      ;; The iteration loop dispatches 'tool-call with a list of tool-calls.
      ;; The scheduler dispatches 'tool-call-pre per individual tool-call.
      ;; We only amend at the iteration level (when payload is a list).
      (if (list? payload)
          (hook-amend (list (make-tool-call "tc-amended" "read" (hasheq 'path "/safe"))))
          (hook-pass)))

    (define ext-reg (make-mock-ext-reg 'tool-call amending-hook))
    (define prov (make-multi-mock-provider
                  (tool-call-then-text-responses "bash" (hasheq 'command "rm -rf /"))))
    (define reg (make-tool-registry))
    (register-tool! reg (make-read-tool))
    (register-tool! reg (make-bash-tool))

    (define result
      (run-iteration-loop
       (list (make-user-message "test"))
       prov bus reg ext-reg temp-log-path "sess-i2" 5))

    (check-equal? (loop-result-termination-reason result) 'completed
                  "iteration completes after amended tool call")
    (define evts (reverse (unbox events)))
    (define completed-events
      (filter (lambda (e) (equal? (event-event e) "tool.call.completed")) evts))
    (check-equal? (length completed-events) 1 "one tool call completed")
    (when (= (length completed-events) 1)
      (check-equal? (hash-ref (event-payload (car completed-events)) 'name #f) "read"
                    "amended tool 'read' was executed, not 'bash'"))
    (cleanup-temp-log!))

   ;; ============================================================
   ;; I3: tool-call hook removes all tool calls
   ;; ============================================================
   (test-case
    "I3: tool-call hook removes all tool calls"
    (cleanup-temp-log!)
    (define bus (make-event-bus))
    (define events (box '()))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    (define (removing-hook payload)
      (hook-amend '()))

    (define ext-reg (make-mock-ext-reg 'tool-call removing-hook))
    ;; First call returns tool call, second returns text
    (define prov (make-multi-mock-provider
                  (tool-call-then-text-responses "read" (hasheq 'path "/a"))))
    (define reg (make-tool-registry))
    (register-tool! reg (make-read-tool))

    (define result
      (run-iteration-loop
       (list (make-user-message "test"))
       prov bus reg ext-reg temp-log-path "sess-i3" 5))

    (check-equal? (loop-result-termination-reason result) 'completed
                  "iteration completes even when all tool calls removed")
    (define evts (reverse (unbox events)))
    (define tool-completed
      (filter (lambda (e) (equal? (event-event e) "tool.call.completed")) evts))
    (check-equal? (length tool-completed) 0
                  "no tool.call.completed when all tool calls removed")
    (cleanup-temp-log!))

   ;; ============================================================
   ;; I4: tool-result hook returns 'block — original messages preserved
   ;; ============================================================
   (test-case
    "I4: tool-result hook blocks — original messages preserved"
    (cleanup-temp-log!)
    (define bus (make-event-bus))
    (define events (box '()))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    (define (blocking-hook payload)
      (hook-block "blocked"))

    (define ext-reg (make-mock-ext-reg 'tool-result blocking-hook))
    (define prov (make-multi-mock-provider
                  (tool-call-then-text-responses "read" (hasheq 'path "/a"))))
    (define reg (make-tool-registry))
    (register-tool! reg (make-read-tool))

    (define result
      (run-iteration-loop
       (list (make-user-message "test"))
       prov bus reg ext-reg temp-log-path "sess-i4" 5))

    (check-equal? (loop-result-termination-reason result) 'completed
                  "iteration completes despite tool-result block")
    ;; tool.call.completed should still be emitted (scheduler ran)
    (define evts (reverse (unbox events)))
    (define tool-completed
      (filter (lambda (e) (equal? (event-event e) "tool.call.completed")) evts))
    (check-not-false (positive? (length tool-completed))
                     "tool.call.completed emitted despite hook block")
    (cleanup-temp-log!))

   ;; ============================================================
   ;; I5: max-iterations = 0 triggers max-iterations-exceeded
   ;; ============================================================
   (test-case
    "I5: max-iterations = 0 triggers max-iterations-exceeded on first tool call"
    (cleanup-temp-log!)
    (define bus (make-event-bus))
    (define events (box '()))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    (define prov (make-multi-mock-provider
                  (always-tool-call-responses "read" (hasheq 'path "/a"))))
    (define reg (make-tool-registry))
    (register-tool! reg (make-read-tool))

    (define result
      (run-iteration-loop
       (list (make-user-message "test"))
       prov bus reg #f temp-log-path "sess-i5" 0))

    (check-equal? (loop-result-termination-reason result) 'max-iterations-exceeded
                  "max-iterations=0 triggers max-iterations-exceeded")
    (define evts (reverse (unbox events)))
    (define error-events
      (filter (lambda (e) (equal? (event-event e) "runtime.error")) evts))
    (check-not-false (positive? (length error-events))
                     "runtime.error emitted for max-iterations-exceeded")
    (cleanup-temp-log!))

   ;; ============================================================
   ;; I6: Follow-up delivery mode 'one-at-a-time
   ;; ============================================================
   (test-case
    "I6: follow-up delivery mode one-at-a-time dequeues one at a time"
    (cleanup-temp-log!)
    (define bus (make-event-bus))
    (define queue (make-queue))
    (define events (box '()))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    (enqueue-followup! queue "follow-up 1")
    (enqueue-followup! queue "follow-up 2")

    (define prov (make-simple-mock-provider "R1" "R2" "R3"))

    (define result
      (run-iteration-loop
       (list (make-user-message "test"))
       prov bus #f #f temp-log-path "sess-i6" 10
       #:queue queue
       #:follow-up-delivery-mode 'one-at-a-time))

    (check-equal? (loop-result-termination-reason result) 'completed
                  "iteration completes with one-at-a-time follow-ups")
    (define evts (reverse (unbox events)))
    (define followup-events
      (filter (lambda (e) (equal? (event-event e) "followup.injected")) evts))
    (check-equal? (length followup-events) 2
                  "two followup.injected events for two follow-ups")
    (for ([fe (in-list followup-events)])
      (check-equal? (hash-ref (event-payload fe) 'count #f) 1
                    "each followup.injected has count=1"))
    (cleanup-temp-log!))))

(module+ main
  (run-tests iteration-transition-tests))

(module+ test
  (run-tests iteration-transition-tests))
