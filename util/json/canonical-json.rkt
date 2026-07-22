#lang racket/base

;; Canonical restricted-JSON serializer extracted from scripts/gsd-milestone-truth.rkt
;; (v0.99.58 W1-4 / P1-G). Pure functions implementing the JCS-style restricted
;; integer-only canonicalization profile for stable hashes.

(require racket/list
         (only-in file/sha1 bytes->hex-string)
         openssl)

(provide canonical-json-bytes
         canonical-json-sha256)

;; JavaScript safe-integer range. Shared with util/json/strict-json.rkt —
;; keep in sync.
(define MAX-SAFE-INTEGER 9007199254740991)

(define (safe-integer? value)
  (and (exact-integer? value) (<= (- MAX-SAFE-INTEGER) value MAX-SAFE-INTEGER)))

(define (string->utf16-units value)
  (append-map (lambda (char)
                (define codepoint (char->integer char))
                (cond
                  [(<= #xd800 codepoint #xdfff)
                   (error 'canonical-json-bytes "string contains an invalid Unicode surrogate")]
                  [(<= codepoint #xffff) (list codepoint)]
                  [else
                   (define offset (- codepoint #x10000))
                   (list (+ #xd800 (quotient offset #x400)) (+ #xdc00 (remainder offset #x400)))]))
              (string->list value)))

(define (utf16-string<? left right)
  (let loop ([left-units (string->utf16-units left)]
             [right-units (string->utf16-units right)])
    (cond
      [(null? left-units) (pair? right-units)]
      [(null? right-units) #f]
      [(< (car left-units) (car right-units)) #t]
      [(> (car left-units) (car right-units)) #f]
      [else (loop (cdr left-units) (cdr right-units))])))

(define (canonical-json-bytes value)
  (define output (open-output-string))
  (define (write-string! value*)
    (write-char #\" output)
    (for ([char (in-string value*)])
      (define codepoint (char->integer char))
      (cond
        [(char=? char #\") (display "\\\"" output)]
        [(char=? char #\\) (display "\\\\" output)]
        [(char=? char #\backspace) (display "\\b" output)]
        [(char=? char #\page) (display "\\f" output)]
        [(char=? char #\newline) (display "\\n" output)]
        [(char=? char #\return) (display "\\r" output)]
        [(char=? char #\tab) (display "\\t" output)]
        [(< codepoint #x20)
         (define hex (number->string codepoint 16))
         (display "\\u" output)
         (display (make-string (- 4 (string-length hex)) #\0) output)
         (display hex output)]
        [(<= #xd800 codepoint #xdfff)
         (error 'canonical-json-bytes "string contains an invalid Unicode surrogate")]
        [else (write-char char output)]))
    (write-char #\" output))
  (define (write-value! current)
    (cond
      [(eq? current 'null) (display "null" output)]
      [(boolean? current) (display (if current "true" "false") output)]
      [(string? current) (write-string! current)]
      [(safe-integer? current) (display current output)]
      [(number? current) (error 'canonical-json-bytes "number is not a safe integer: ~e" current)]
      [(list? current)
       (write-char #\[ output)
       (for ([element (in-list current)]
             [index (in-naturals)])
         (unless (zero? index)
           (write-char #\, output))
         (write-value! element))
       (write-char #\] output)]
      [(hash? current)
       (unless (andmap string? (hash-keys current))
         (error 'canonical-json-bytes "object keys must all be strings"))
       (write-char #\{ output)
       (for ([key (in-list (sort (hash-keys current) utf16-string<?))]
             [index (in-naturals)])
         (unless (zero? index)
           (write-char #\, output))
         (write-string! key)
         (write-char #\: output)
         (write-value! (hash-ref current key)))
       (write-char #\} output)]
      [else (error 'canonical-json-bytes "value is outside the restricted JSON model: ~e" current)]))
  (write-value! value)
  (string->bytes/utf-8 (get-output-string output)))

(define (canonical-json-sha256 value)
  (bytes->hex-string (sha256-bytes (open-input-bytes (canonical-json-bytes value)))))
