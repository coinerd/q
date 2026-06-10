#lang racket/base

;; tests/test-gapc-tool-result-summaries.rkt
;; GAP-C: Content summaries include tool-result-parts

(require rackunit
         rackunit/text-ui
         (only-in "../util/content/content-parts.rkt"
                  make-text-part
                  text-part-text
                  make-tool-result-part
                  tool-result-part?
                  tool-result-part-content
                  tool-result-part-is-error?)
         ;; v0.97.6 LF2: Import real extracted function instead of local mock
         (only-in "../runtime/context-assembly/turn-context.rkt" content-part->text))

(define suite
  (test-suite "gapc-tool-result-summaries"

    ;; text-part content extracted
    (test-case "text-part content extracted"
      (define tp (make-text-part "hello world"))
      (check-equal? (content-part->text tp) "hello world"))

    ;; tool-result-part with string content
    (test-case "tool-result-part string content"
      (define trp (make-tool-result-part "tool-1" "grep output here" #f))
      (check-equal? (content-part->text trp) "grep output here"))

    ;; tool-result-part with hash content
    (test-case "tool-result-part hash content"
      (define trp (make-tool-result-part "tool-2" (hash 'result "found" 'line 42) #f))
      (check-true (> (string-length (content-part->text trp)) 0)))

    ;; error tool-result-part returns empty string
    (test-case "error tool-result-part returns empty"
      (define trp (make-tool-result-part "tool-3" "error: timeout" #t))
      (check-equal? (content-part->text trp) ""))

    ;; long tool result content truncated to 500 chars
    (test-case "long tool result truncated to 500 chars"
      (define long-text (make-string 600 #\x))
      (define trp (make-tool-result-part "tool-4" long-text #f))
      (define result (content-part->text trp))
      (check-equal? (string-length result) 500)
      (check-equal? (substring result 497 500) "..."))))

(run-tests suite)
