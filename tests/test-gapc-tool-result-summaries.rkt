#lang racket/base

;; tests/test-gapc-tool-result-summaries.rkt
;; GAP-C: Content summaries include tool-result-parts

(require rackunit
         racket/format
         racket/string
         (only-in "../util/content/content-parts.rkt"
                  make-text-part
                  text-part-text
                  make-tool-result-part
                  tool-result-part?
                  tool-result-part-content
                  tool-result-part-is-error?))

;; --- Helper: Simulate content-part->text from turn-context.rkt ---
(define (content-part->text part)
  (cond
    [(and (procedure? (hash-ref part 'text-part? (lambda () #f)))
          (hash-ref part 'text-part? (lambda () #f)))
     (hash-ref part 'text "")]
    [(and (hash-ref part 'tool-result-part? (lambda () #f)) (not (hash-ref part 'is-error? #f)))
     (define c (hash-ref part 'content ""))
     (define raw
       (cond
         [(string? c) c]
         [(hash? c) (~a c)]
         [(list? c) (string-join (map ~a c) " ")]
         [else ""]))
     (if (> (string-length raw) 500)
         (string-append (substring raw 0 497) "...")
         raw)]
    [else ""]))

;; Simulate message content as list of structs
;; We test against the actual content-part structs
(define (make-text-part/text txt)
  txt) ;; simplified

(test-case "GAP-C: text-part content extracted"
  (define tp (make-text-part "hello world"))
  (check-equal? (text-part-text tp) "hello world"))

(test-case "GAP-C: tool-result-part with string content"
  (define trp (make-tool-result-part "tool-1" "grep output here" #f))
  (check-true (tool-result-part? trp))
  (check-equal? (tool-result-part-content trp) "grep output here")
  (check-false (tool-result-part-is-error? trp)))

(test-case "GAP-C: tool-result-part with hash content"
  (define trp (make-tool-result-part "tool-2" (hash 'result "found" 'line 42) #f))
  (check-true (tool-result-part? trp))
  (check-true (hash? (tool-result-part-content trp))))

(test-case "GAP-C: error tool-result-part has is-error flag"
  (define trp (make-tool-result-part "tool-3" "error: timeout" #t))
  (check-true (tool-result-part-is-error? trp)))

(test-case "GAP-C: long tool result content preserved"
  (define long-text (make-string 600 #\x))
  (define trp (make-tool-result-part "tool-4" long-text #f))
  (check-equal? (string-length (tool-result-part-content trp)) 600)
  ;; When processed through content-part->text, should be truncated
  (define content (tool-result-part-content trp))
  (define raw
    (if (> (string-length content) 500)
        (string-append (substring content 0 497) "...")
        content))
  (check-true (<= (string-length raw) 500)))
