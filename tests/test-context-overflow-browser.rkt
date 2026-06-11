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
  (check-equal? (string-length tc) 4015) ; 4000 + "\n...(truncated)"
  (check-true (string-suffix? tc "\n...(truncated)")))

(test-case "observation->hash truncates visible-text at 4000 chars"
  (define obs (make-big-observation 5000))
  (define h (observation->hash obs))
  (define vt (hash-ref h 'visible-text))
  (check-true (string? vt))
  (check-equal? (string-length vt) 4015)
  (check-true (string-suffix? vt "\n...(truncated)")))

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

;; ---------------------------------------------------------------------------
;; NF-11d: dom-summary is also truncated (F-08)
;; ---------------------------------------------------------------------------

(test-case "observation->hash truncates dom-summary at 4000 chars"
  (define obs
    (browser-observation "https://example.com"
                         "Title"
                         "short"
                         "short"
                         (make-string 5000 #\d)
                         #f
                         #f
                         #f
                         '()
                         '()
                         #f
                         '()
                         #f))
  (define h (observation->hash obs))
  (define ds (hash-ref h 'dom-summary))
  (check-true (string? ds))
  (check-equal? (string-length ds) 4015)
  (check-true (string-suffix? ds "\n...(truncated)")))

(test-case "observation->hash preserves short dom-summary unchanged"
  (define obs
    (browser-observation "https://example.com"
                         "Title"
                         "short"
                         "short"
                         "small summary"
                         #f
                         #f
                         #f
                         '()
                         '()
                         #f
                         '()
                         #f))
  (define h (observation->hash obs))
  (check-equal? (hash-ref h 'dom-summary) "small summary"))

;; ---------------------------------------------------------------------------
;; F-07: Boundary tests for truncation
;; ---------------------------------------------------------------------------

(test-case "observation->hash: exact boundary — 4000 chars unchanged"
  (define obs (make-big-observation 4000))
  (define h (observation->hash obs))
  (check-equal? (string-length (hash-ref h 'text-content)) 4000)
  (check-false (string-contains? (hash-ref h 'text-content) "truncated")))

(test-case "observation->hash: exact boundary — 4001 chars triggers truncation"
  (define obs (make-big-observation 4001))
  (define h (observation->hash obs))
  (define tc (hash-ref h 'text-content))
  (check-true (< (string-length tc) 4020))
  (check-true (string-contains? tc "(truncated)")))

(test-case "observation->hash: empty string preserved"
  (define obs (make-big-observation 0))
  (define h (observation->hash obs))
  (check-equal? (hash-ref h 'text-content) ""))

(test-case "observation->hash: dom-summary #f preserved"
  (define obs
    (browser-observation "https://example.com" "Title" "short" "short" #f #f #f #f '() '() #f '() #f))
  (define h (observation->hash obs))
  (check-false (hash-ref h 'dom-summary)))

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
