#lang racket

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration

;; tests/test-tool-coordinator.rkt — tests for runtime/tool-coordinator.rkt
;;
;; Tests for the extracted tool coordinator (A-02, v0.16.1 Wave 4).
;; Covers: extract-tool-calls-from-messages, make-tool-result-messages.

(require rackunit
         rackunit/text-ui
         json
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  make-tool-call-part
                  message-id
                  message-role
                  message-content
                  message-meta
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  make-tool-result-part
                  tool-result-part-tool-call-id
                  tool-result-content
                  tool-result-is-error?
                  tool-result-part-is-error?
                  make-tool-call
                  tool-call-id
                  tool-call-name
                  tool-call-arguments)
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-registry
                  register-tool!
                  make-success-result
                  make-error-result)
         (only-in "../tools/permission-gate.rkt" make-permissive-permission-config)
         (only-in "../runtime/session/session-config.rkt" hash->session-config)
         (only-in "../util/capability.rkt" current-session-capabilities)
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         (only-in "../util/hook-types.rkt" hook-amend hook-block)
         (only-in "../extensions/hooks.rkt" current-hook-timeout-ms)
         (only-in "../extensions/api.rkt" make-extension-registry register-extension! extension)
         (only-in "../runtime/session/session-store.rkt" load-session-log)
         (only-in "../runtime/tool-coordinator.rkt"
                  extract-tool-calls-from-messages
                  make-tool-result-messages
                  handle-tool-calls-pending/outcome
                  capabilities-for-tool-execution
                  tool-batch-outcome?
                  tool-batch-outcome-updated-context
                  tool-batch-outcome-effective-current-calls
                  tool-batch-outcome-current-result-messages))

(define (result-call-id msg)
  (tool-result-part-tool-call-id (first (message-content msg))))

(define (make-read-tool [counter #f])
  (make-tool "read"
             "test read"
             (hasheq 'type "object" 'properties (hasheq 'path (hasheq 'type "string")))
             (lambda (args _ctx)
               (when counter
                 (set-box! counter (add1 (unbox counter))))
               (make-success-result (hash-ref args 'path "fresh contents")))))

(define (make-tool-call-extension handler)
  (define reg (make-extension-registry))
  (register-extension! reg (extension "coordinator-test" "0.1" "1.0" (hasheq 'tool-call handler)))
  reg)

(define-test-suite
 test-tool-coordinator-suite
 (test-case "extract-tool-calls-from-messages returns empty for text-only messages"
   (define msg (make-message "m1" #f 'user 'message (list (make-text-part "hello")) 1000 (hasheq)))
   (check-equal? (extract-tool-calls-from-messages (list msg)) '()))
 (test-case "extract-tool-calls-from-messages finds tool calls in assistant messages"
   (define msg
     (make-message "m1"
                   #f
                   'assistant
                   'message
                   (list (make-tool-call-part "tc1" "read_file" "{\"path\":\"/tmp/x\"}"))
                   1000
                   (hasheq)))
   (define result (extract-tool-calls-from-messages (list msg)))
   (check-equal? (length result) 1)
   (check-equal? (tool-call-name (first result)) "read_file"))
 (test-case "extract-tool-calls-from-messages skips user messages"
   (define user-msg
     (make-message "m1" #f 'user 'message (list (make-text-part "hello")) 1000 (hasheq)))
   (define asst-msg
     (make-message "m2"
                   "m1"
                   'assistant
                   'message
                   (list (make-tool-call-part "tc1" "bash" "{\"cmd\":\"ls\"}"))
                   1000
                   (hasheq)))
   (define result (extract-tool-calls-from-messages (list user-msg asst-msg)))
   (check-equal? (length result) 1)
   (check-equal? (tool-call-name (first result)) "bash"))
 (test-case "extract-tool-calls-from-messages returns empty for empty list"
   (check-equal? (extract-tool-calls-from-messages '()) '()))
 (test-case "make-tool-result-messages creates correct number of messages"
   (define tc (list (make-tool-call "tc1" "read" (hasheq 'path "/tmp/x"))))
   (define results (list (make-success-result "file contents")))
   (define msgs (make-tool-result-messages tc results "parent-1"))
   (check-equal? (length msgs) 1)
   (check-equal? (message-role (first msgs)) 'tool))
 (test-case "make-tool-result-messages preserves error flag"
   (define tc (list (make-tool-call "tc1" "bash" (hasheq 'cmd "bad"))))
   (define results (list (make-error-result "command failed")))
   (define msgs (make-tool-result-messages tc results "parent-1"))
   (check-equal? (length msgs) 1)
   ;; The tool-result-part has is-error? field directly
   (define msg (first msgs))
   (define part (first (message-content msg)))
   (check-true (tool-result-part-is-error? part)))
 (test-case "make-tool-result-messages pairs calls with results"
   (define tcs (list (make-tool-call "tc1" "read" (hasheq)) (make-tool-call "tc2" "write" (hasheq))))
   (define results (list (make-success-result "ok") (make-success-result "done")))
   (define msgs (make-tool-result-messages tcs results "p1"))
   (check-equal? (length msgs) 2))
 (test-case "successful planning reads are pinned with a reason only"
   (define cases
     (list (list (make-tool-call "named" "read" (hasheq 'path ".planning/PLAN-v1.2.3-W4.md"))
                 (make-success-result "plan")
                 #t)
           (list (make-tool-call "planning" "planning-read" (hasheq 'artifact "STATE"))
                 (make-success-result "state")
                 #t)
           (list (make-tool-call "failed" "read" (hasheq 'path ".planning/STATE-v1.2.3-W4.md"))
                 (make-error-result "missing")
                 #f)
           (list (make-tool-call "planning-failed" "planning-read" (hasheq 'artifact "PLAN"))
                 (make-error-result "missing")
                 #f)
           (list (make-tool-call "ordinary" "read" (hasheq 'path "src/main.rkt"))
                 (make-success-result "source")
                 #f)))
   (for ([case (in-list cases)])
     (define msg (first (make-tool-result-messages (list (first case)) (list (second case)) "p")))
     (define meta (message-meta msg))
     (if (third case)
         (begin
           (check-true (hash-ref meta 'gsd-pin #f))
           (check-true (hash-has-key? meta 'gsd-pin-reason))
           (check-not-false (hash-ref meta 'gsd-pin-reason)))
         (begin
           (check-false (hash-ref meta 'gsd-pin #f))
           (check-false (hash-has-key? meta 'gsd-pin-reason))))))
 (test-case "tool batch outcome separates current results from historical context"
   (define historical
     (make-message "historical-result"
                   #f
                   'tool
                   'tool-result
                   (list (make-tool-result-part "old-call" "old contents" #f))
                   1
                   (hasheq 'toolCallId "old-call" 'isError #f)))
   (define assistant
     (make-message "assistant-current"
                   #f
                   'assistant
                   'message
                   (list (make-tool-call-part "current-call" "read" (hasheq 'path "current.rkt")))
                   2
                   (hasheq)))
   (define reg (make-tool-registry))
   (register-tool! reg (make-read-tool))
   (define outcome
     (handle-tool-calls-pending/outcome (list assistant)
                                        (list historical)
                                        #f
                                        reg
                                        (make-event-bus)
                                        "tool-outcome-test"
                                        (format "/tmp/test-~a-tool-outcome.log" (random 1000000))
                                        #f
                                        (hash->session-config (hasheq))
                                        #:permission-config (make-permissive-permission-config)))
   (check-true (tool-batch-outcome? outcome))
   (check-equal? (map tool-call-id (tool-batch-outcome-effective-current-calls outcome))
                 '("current-call"))
   (check-equal? (map result-call-id (tool-batch-outcome-current-result-messages outcome))
                 '("current-call"))
   (check-false (member "historical-result"
                        (map message-id (tool-batch-outcome-current-result-messages outcome))))
   (check-equal? (map message-id
                      (filter (lambda (m) (eq? (message-role m) 'tool))
                              (tool-batch-outcome-updated-context outcome)))
                 (list "historical-result"
                       (message-id (first (tool-batch-outcome-current-result-messages outcome))))))
 (test-case "amended outcome executes and persists the effective call exactly once"
   (define counter (box 0))
   (define hook-counter (box 0))
   (define reg (make-tool-registry))
   (register-tool! reg (make-read-tool counter))
   (define assistant
     (make-message "assistant-amend"
                   #f
                   'assistant
                   'message
                   (list (make-tool-call-part "amended-call" "read" (hasheq 'path "old.rkt")))
                   2
                   (hasheq)))
   (define ext-reg
     (make-tool-call-extension
      (lambda (calls)
        (set-box! hook-counter (add1 (unbox hook-counter)))
        (define call (first calls))
        (hook-amend (list (make-tool-call (tool-call-id call) "read" (hasheq 'path "new.rkt")))))))
   (define log-path (format "/tmp/test-~a-tool-amend.log" (random 1000000)))
   (define outcome
     (parameterize ([current-hook-timeout-ms #f])
       (handle-tool-calls-pending/outcome (list assistant)
                                          '()
                                          ext-reg
                                          reg
                                          (make-event-bus)
                                          "amend-test"
                                          log-path
                                          #f
                                          (hash->session-config (hasheq))
                                          #:permission-config (make-permissive-permission-config))))
   (check-equal? (unbox counter) 1)
   (check-equal? (unbox hook-counter) 1)
   (check-equal?
    (hash-ref (tool-call-arguments (first (tool-batch-outcome-effective-current-calls outcome)))
              'path)
    "new.rkt")
   (check-equal? (length (tool-batch-outcome-current-result-messages outcome)) 1)
   (check-equal? (length (load-session-log log-path)) 1))
 (test-case "blocked outcome persists one error result without executing"
   (define counter (box 0))
   (define hook-counter (box 0))
   (define reg (make-tool-registry))
   (register-tool! reg (make-read-tool counter))
   (define assistant
     (make-message "assistant-block"
                   #f
                   'assistant
                   'message
                   (list (make-tool-call-part "blocked-call" "read" (hasheq 'path "blocked.rkt")))
                   2
                   (hasheq)))
   (define log-path (format "/tmp/test-~a-tool-block.log" (random 1000000)))
   (define outcome
     (handle-tool-calls-pending/outcome
      (list assistant)
      '()
      (make-tool-call-extension (lambda (_calls)
                                  (set-box! hook-counter (add1 (unbox hook-counter)))
                                  (hook-block "blocked by test")))
      reg
      (make-event-bus)
      "block-test"
      log-path
      #f
      (hash->session-config (hasheq))
      #:permission-config (make-permissive-permission-config)))
   (check-equal? (unbox counter) 0)
   (check-equal? (unbox hook-counter) 1)
   (check-equal? (length (tool-batch-outcome-current-result-messages outcome)) 1)
   (check-true (tool-result-part-is-error?
                (first (message-content (first (tool-batch-outcome-current-result-messages
                                                outcome))))))
   (check-equal? (length (load-session-log log-path)) 1))
 (test-case "configured capability authority takes precedence"
   (define config (hash->session-config (hasheq 'capabilities '(read-only))))
   (parameterize ([current-session-capabilities '(any)])
     (check-equal? (capabilities-for-tool-execution config) '(read-only))))
 (test-case "session capability authority is used when config has none"
   (define config (hash->session-config (hasheq)))
   (parameterize ([current-session-capabilities '(shell-exec)])
     (check-equal? (capabilities-for-tool-execution config) '(shell-exec)))))

(run-tests test-tool-coordinator-suite)
