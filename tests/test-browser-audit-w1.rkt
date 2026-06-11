#lang racket

;; tests/test-browser-audit-w1.rkt — Tests for Wave 1 audit fixes
;; C3: make-success-result accepts image-part
;; C4/H7: strip-image-parts wired into context assembly
;; H6: result-content->string handles image hashes
;; M6: merge-consecutive-roles handles list content
;; M7: summarize-tool-result handles image-part

(require rackunit
         racket/string
         "../tools/tool.rkt"
         "../util/content/content-parts.rkt"
         "../util/content/content-helpers.rkt"
         "../util/message/message.rkt"
         (only-in "../agent/loop-messages.rkt" merge-consecutive-roles)
         (only-in "../runtime/context-assembly/session-walk.rkt" summarize-tool-result))

;; ---------------------------------------------------------------------------
;; C3: make-success-result accepts image-part
;; ---------------------------------------------------------------------------

(test-case "C3: make-success-result accepts image-part and serializes it"
  (define img (make-image-part "image/png" "base64data" "auto"))
  (define tr (make-success-result img))
  (check-true (tool-result? tr))
  (define content (tool-result-content tr))
  (check-true (hash? content))
  (check-equal? (hash-ref content 'type) "image")
  (check-equal? (hash-ref content 'mimeType) "image/png")
  (check-equal? (hash-ref content 'data) "base64data"))

(test-case "C3: make-success-result still accepts hash"
  (define tr (make-success-result (hasheq 'status "ok" 'data "hello")))
  (check-true (tool-result? tr))
  (check-equal? (hash-ref (tool-result-content tr) 'status) "ok"))

;; ---------------------------------------------------------------------------
;; H6: result-content->string handles image hashes
;; ---------------------------------------------------------------------------

(test-case "H6: result-content->string handles image hash in list"
  (define img-hash (hasheq 'type "image" 'mimeType "image/png" 'data "abcd1234" 'detail "auto"))
  (define result (result-content->string (list img-hash) #:handle-hash? #t))
  (check-true (string-contains? result "[image:"))
  (check-true (string-contains? result "image/png")))

(test-case "H6: result-content->string handles plain text in list"
  (check-equal? (result-content->string (list "hello" "world")) "hello\nworld"))

;; ---------------------------------------------------------------------------
;; M6: merge-consecutive-roles handles list content
;; ---------------------------------------------------------------------------

(test-case "M6: merge-consecutive-roles merges string content"
  (define msgs (list (hasheq 'role "user" 'content "hello")
                     (hasheq 'role "user" 'content "world")))
  (define merged (merge-consecutive-roles msgs))
  (check-equal? (length merged) 1)
  (check-equal? (hash-ref (car merged) 'content) "hello\n\nworld"))

(test-case "M6: merge-consecutive-roles preserves list content (image blocks)"
  (define msgs (list (hasheq 'role "user" 'content (list (hasheq 'type "image")))
                     (hasheq 'role "user" 'content (list (hasheq 'type "text" 'text "hi")))))
  (define merged (merge-consecutive-roles msgs))
  (check-equal? (length merged) 1)
  (define content (hash-ref (car merged) 'content))
  (check-true (list? content))
  (check-equal? (length content) 2))

;; ---------------------------------------------------------------------------
;; M7: summarize-tool-result handles image-part
;; ---------------------------------------------------------------------------

(test-case "M7: summarize-tool-result handles short image-part (returns entry)"
  (define msg (message "id" #f 'user 'tool-result
                       (list (make-image-part "image/png" "abcd" "auto"))
                       0 (hash)))
  (define result (summarize-tool-result msg))
  ;; Short content returned unchanged
  (check-true (message? result))
  (check-equal? (message-content result) (message-content msg)))

(test-case "M7: summarize-tool-result counts image-part in long mixed content"
  ;; Mix image-part with many long lines to trigger truncation (>40 lines AND >8000 chars)
  (define many-lines (string-join (for/list ([i 100]) (format "line ~a ~a" i (make-string 100 #\x))) "\n"))
  (define msg (message "id" #f 'user 'tool-result
                       (list (make-image-part "image/png" "data" "auto")
                             (make-text-part many-lines))
                       0 (hash)))
  (define result (summarize-tool-result msg))
  (check-true (message? result))
  (define parts (message-content result))
  (check-equal? (length parts) 1)
  (check-true (text-part? (car parts))))
