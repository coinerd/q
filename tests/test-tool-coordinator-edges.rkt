#lang racket/base

;; tests/test-tool-coordinator-edges.rkt — Edge case tests for tool-coordinator (AXIS3-F24)
;; @speed fast  ;; @suite runtime

(require rackunit
         racket/list
         (only-in "../runtime/tool-coordinator.rkt"
                  extract-tool-calls-from-messages
                  classify-tool-results
                  build-blocked-tool-results)
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part make-tool-call-part)
         (only-in "../util/tool/tool-types.rkt" make-tool-call tool-call-id tool-call-name)
         (only-in "../tools/tool.rkt" make-success-result))

;; ── Helpers ────────────────────────────────────────────────

(define (make-assistant-msg-with-tool-calls calls)
  (define parts
    (for/list ([c (in-list calls)])
      (make-tool-call-part (car c) (cadr c) (caddr c))))
  (apply make-message
         "msg-id"
         #f
         'assistant
         'text
         (append parts (list (make-text-part "done")))
         (list 0 #f)))

(define (make-user-msg text)
  (make-message "user-id" #f 'user 'text (list (make-text-part text)) 0 #f))

(define call-1 '("tc-1" "read" "{\"path\":\"/tmp/a\"}"))
(define call-2 '("tc-2" "write" "{\"path\":\"/tmp/b\"}"))

;; ── Tests ──────────────────────────────────────────────────

(test-case "extract-tool-calls: skips user messages"
  (define msgs (list (make-user-msg "hello")))
  (define result (extract-tool-calls-from-messages msgs))
  (check-equal? result '()))

(test-case "extract-tool-calls: extracts from assistant message"
  (define msgs (list (make-assistant-msg-with-tool-calls (list call-1))))
  (define result (extract-tool-calls-from-messages msgs))
  (check-equal? (length result) 1)
  (check-equal? (tool-call-name (car result)) "read"))

(test-case "extract-tool-calls: multiple tool calls across messages"
  (define msgs
    (list (make-assistant-msg-with-tool-calls (list call-1 call-2)) (make-user-msg "results")))
  (define result (extract-tool-calls-from-messages msgs))
  (check-equal? (length result) 2))

(test-case "classify-tool-results: mismatched counts truncates to shorter"
  (define calls (list (make-tool-call "a" "read" (hasheq)) (make-tool-call "b" "write" (hasheq))))
  (define results (list (make-success-result "ok")))
  (define classified (classify-tool-results calls results))
  (check-equal? (length classified) 1))

(test-case "build-blocked-tool-results: creates error for each call"
  (define calls
    (list (make-tool-call "a" "read" (hasheq))
          (make-tool-call "b" "write" (hasheq))
          (make-tool-call "c" "bash" (hasheq))))
  (define blocked (build-blocked-tool-results calls))
  (check-equal? (length blocked) 3))

(test-case "extract-tool-calls: empty tool-call-id still extracted"
  (define msgs (list (make-assistant-msg-with-tool-calls '(("" "read" "{\"path\":\"/tmp\"}")))))
  (define result (extract-tool-calls-from-messages msgs))
  (check-equal? (length result) 1)
  (check-equal? (tool-call-id (car result)) ""))

(test-case "extract-tool-calls: empty tool-name still extracted"
  (define msgs (list (make-assistant-msg-with-tool-calls '(("id-1" "" "{}")))))
  (define result (extract-tool-calls-from-messages msgs))
  (check-equal? (length result) 1)
  (check-equal? (tool-call-name (car result)) ""))
