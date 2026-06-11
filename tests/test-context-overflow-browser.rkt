#lang racket

;; @speed fast
;; @suite default

;; tests/test-context-overflow-browser.rkt — Regression tests for NF-11
;; context-overflow fixes in browser observations and session-walk truncation.

(require rackunit
         "../tools/builtins/browser-tools.rkt"
         "../browser/types.rkt"
         "../runtime/context-assembly/session-walk.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt")

;; ---------------------------------------------------------------------------
;; NF-11a: observation->hash truncates text fields to 4000 chars
;; ---------------------------------------------------------------------------

(define (make-big-observation text-len)
  (browser-observation "https://example.com"
                       "Title"
                       (make-string text-len #\x)
                       (make-string text-len #\y)
                       "dom"
                       #f
                       #f
                       #f
                       '()
                       '()
                       #f
                       '()
                       #f))

(test-case "observation->hash truncates text-content at 4000 chars"
  (define obs (make-big-observation 5000))
  (define h (observation->hash obs))
  (define tc (hash-ref h 'text-content))
  (check-true (string? tc))
  (check-equal? (string-length tc) 4016) ; 4000 + "\n... [truncated]"
  (check-true (string-suffix? tc "\n... [truncated]")))

(test-case "observation->hash truncates visible-text at 4000 chars"
  (define obs (make-big-observation 5000))
  (define h (observation->hash obs))
  (define vt (hash-ref h 'visible-text))
  (check-true (string? vt))
  (check-equal? (string-length vt) 4016)
  (check-true (string-suffix? vt "\n... [truncated]")))

(test-case "observation->hash preserves short text unchanged"
  (define obs (make-big-observation 100))
  (define h (observation->hash obs))
  (check-equal? (string-length (hash-ref h 'text-content)) 100)
  (check-equal? (string-length (hash-ref h 'visible-text)) 100))

;; ---------------------------------------------------------------------------
;; NF-11b: summarize-tool-result handles long single-line strings
;; ---------------------------------------------------------------------------

(define (make-tool-result-message text)
  (make-message 'id-1 #f 'user 'tool-result (list (make-text-part text)) (current-seconds) (hasheq)))

(test-case "summarize-tool-result truncates single-line text > 8000 chars"
  (define big-text (make-string 9000 #\z))
  (define entry (make-tool-result-message big-text))
  (define result (summarize-tool-result entry))
  (define result-text (text-part-text (first (message-content result))))
  (check-true (< (string-length result-text) 9000))
  (check-true (string-contains? result-text "... 1000 chars truncated ...")))

(test-case "summarize-tool-result preserves text <= 8000 chars"
  (define small-text (make-string 100 #\z))
  (define entry (make-tool-result-message small-text))
  (define result (summarize-tool-result entry))
  (check-equal? result entry))

(test-case "summarize-tool-result truncates multi-line text > 40 lines"
  ;; 50 lines × 200 chars = 10000 chars, exceeds 8000 limit
  (define many-lines
    (string-join (for/list ([i 50])
                   (format "line ~a ~a" i (make-string 190 #\-)))
                 "\n"))
  (define entry (make-tool-result-message many-lines))
  (define result (summarize-tool-result entry))
  (define result-text (text-part-text (first (message-content result))))
  (check-true (string-contains? result-text "... 30 lines truncated ...")))
