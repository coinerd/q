#lang racket

;; tests/test-soft-iteration.rkt — soft iteration limit tests
;;
;; Wave 1 (v0.14.1): Agent warns at soft limit, hard-stops at hard limit.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../util/ids.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry register-tool! make-tool make-error-result)
         "../runtime/iteration.rkt")

;; ============================================================
;; Test helpers
;; ============================================================

(define (make-user-message text)
  (make-message (generate-id) #f 'user 'user (list (make-text-part text)) (current-seconds) (hasheq)))

;; Create a mock provider that always returns a tool call, causing infinite looping.
;; The iteration loop will hit max-iterations limits.
(define (make-looping-tool-provider)
  ;; Stream chunks: one tool-call delta chunk, then done
  (define chunks
    (list
     (make-stream-chunk
      #f
      (hasheq 'index 0 'id "tc-loop-1" 'name "bash" 'arguments "{\"command\": \"echo test\"}")
      #f
      #f)
     (make-stream-chunk #f #f (hasheq 'prompt-tokens 10 'completion-tokens 10 'total-tokens 20) #t)))
  (make-mock-provider
   (make-model-response (list (hasheq 'type "text" 'text ""))
                        (hasheq 'prompt-tokens 10 'completion-tokens 10 'total-tokens 20)
                        "test-model"
                        'stop)
   #:name "test"
   #:stream-chunks chunks))

;; Collect events of a given type from the bus
(define (collect-events bus event-type)
  (define collected (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) event-type)
                  (set-box! collected (append (unbox collected) (list (event-payload evt))))))
              #:filter (lambda (evt) (equal? (event-ev evt) event-type)))
  collected)

;; Create a tool registry with a mock bash tool for tool-call loops
(define (make-test-registry)
  (define reg (make-tool-registry))
  (register-tool! reg
                  (make-tool "bash"
                             "mock bash for testing"
                             (hasheq 'type "object" 'properties (hasheq))
                             (lambda (args ctx) (make-tool-result "mock output" (hasheq) #f))))
  reg)

(define tmp-log "/tmp/q-test-soft-iteration.jsonl")

(define (cleanup!)
  (when (file-exists? tmp-log)
    (delete-file tmp-log)))

;; ============================================================
;; Tests
;; ============================================================

(define soft-iteration-tests
  (test-suite "soft-iteration-limit"

    (test-case "soft-warning event fires at soft limit"
      (cleanup!)
      (define bus (make-event-bus))
      (define warnings (collect-events bus "iteration.soft-warning"))
      (define provider (make-looping-tool-provider))
      (define ctx (list (make-user-message "test")))
      ;; soft=3, hard=6 — should warn at iterations 3, 4, 5 then stop at 6
      (run-iteration-loop ctx
                          provider
                          bus
                          (make-test-registry)
                          #f
                          tmp-log
                          "test-session"
                          3
                          #:config (hasheq 'max-iterations-hard 6))
      (define collected (unbox warnings))
      (check >= (length collected) 1 "Expected at least one soft-warning event")
      (cleanup!))

    (test-case "hard stop at hard limit"
      (cleanup!)
      (define bus (make-event-bus))
      (define provider (make-looping-tool-provider))
      (define ctx (list (make-user-message "test")))
      ;; soft=2, hard=4 — should stop at 4 even though soft limit is 2
      (define result
        (run-iteration-loop ctx
                            provider
                            bus
                            (make-test-registry)
                            #f
                            tmp-log
                            "test-session"
                            2
                            #:config (hasheq 'max-iterations-hard 4)))
      (check-equal? (loop-result-termination-reason result)
                    'max-iterations-exceeded
                    "Should hard-stop at max-iterations-hard")
      (cleanup!))

    (test-case "no warning when soft limit is not reached"
      (cleanup!)
      (define bus (make-event-bus))
      (define warnings (collect-events bus "iteration.soft-warning"))
      ;; Provider that completes immediately (no tool calls)
      (define provider
        (make-mock-provider (make-model-response
                             (list (hasheq 'type "text" 'text "done"))
                             (hasheq 'prompt-tokens 5 'completion-tokens 5 'total-tokens 10)
                             "test-model"
                             'stop)))
      (define ctx (list (make-user-message "test")))
      ;; soft=20, hard=50 — completes in 1 iteration, no warning
      (run-iteration-loop ctx
                          provider
                          bus
                          #f
                          #f
                          tmp-log
                          "test-session"
                          20
                          #:config (hasheq 'max-iterations-hard 50))
      (check-equal? (unbox warnings) '() "No soft-warning should fire when loop completes normally")
      (cleanup!))

    (test-case "soft warning payload has correct fields"
      (cleanup!)
      (define bus (make-event-bus))
      (define warnings (collect-events bus "iteration.soft-warning"))
      (define provider (make-looping-tool-provider))
      (define ctx (list (make-user-message "test")))
      (run-iteration-loop ctx
                          provider
                          bus
                          (make-test-registry)
                          #f
                          tmp-log
                          "test-session"
                          2
                          #:config (hasheq 'max-iterations-hard 5))
      (define collected (unbox warnings))
      (when (pair? collected)
        (define first-warning (car collected))
        (check-equal? (hash-ref first-warning 'soft-limit #f) 2)
        (check-equal? (hash-ref first-warning 'hard-limit #f) 5)
        (check-true (number? (hash-ref first-warning 'iteration #f))))
      (cleanup!))))

(run-tests soft-iteration-tests)
