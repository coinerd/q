#lang racket

;; tests/tui/utf8-accumulator.rkt — Tests for UTF-8 accumulator (#435)
;;
;; Tests utf8-accumulate-char, utf8-lead-byte-count,
;; utf8-continuation-byte?, utf8-high-byte?, and reassemble-utf8-chars.

(require rackunit
         rackunit/text-ui
         "../../tui/terminal-input.rkt")

(define utf8-tests
  (test-suite
   "utf8-accumulator"

   (test-case "utf8-lead-byte-count"
     (check-equal? (utf8-lead-byte-count 65) 1)   ; ASCII 'A'
     (check-equal? (utf8-lead-byte-count 127) 1)   ; DEL
     (check-equal? (utf8-lead-byte-count 128) 1)   ; continuation
     (check-equal? (utf8-lead-byte-count 192) 2)   ; 2-byte lead
     (check-equal? (utf8-lead-byte-count 224) 3)   ; 3-byte lead
     (check-equal? (utf8-lead-byte-count 240) 4)   ; 4-byte lead
     (check-equal? (utf8-lead-byte-count 248) 1))  ; invalid

   (test-case "utf8-continuation-byte?"
     (check-false (utf8-continuation-byte? 65))
     (check-false (utf8-continuation-byte? 127))
     (check-true (utf8-continuation-byte? 128))
     (check-true (utf8-continuation-byte? 191))
     (check-false (utf8-continuation-byte? 192)))

   (test-case "utf8-high-byte?"
     (check-false (utf8-high-byte? #\A))
     (check-false (utf8-high-byte? #\z))
     (check-true (utf8-high-byte? (integer->char 200))))

   (test-case "utf8-accumulate-char ASCII"
     (utf8-accumulator-reset!)
     (check-equal? (utf8-accumulator-length) 0)
     (define result (utf8-accumulate-char #\A))
     (check-equal? result #\A)
     (check-equal? (utf8-accumulator-length) 0))

   (test-case "utf8-accumulate-char 2-byte UTF-8"
     (utf8-accumulator-reset!)
     ;; 'ü' = U+00FC = 0xC3 0xBC
     (define r1 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r1) ; incomplete
     (check-equal? (utf8-accumulator-length) 1)
     (define r2 (utf8-accumulate-char (integer->char #xBC)))
     (check-equal? r2 #\ü)
     (check-equal? (utf8-accumulator-length) 0))

   (test-case "utf8-accumulate-char 3-byte UTF-8 (CJK)"
     (utf8-accumulator-reset!)
     ;; '日' = U+65E5 = 0xE6 0x97 0xA5
     (define r1 (utf8-accumulate-char (integer->char #xE6)))
     (check-false r1)
     (define r2 (utf8-accumulate-char (integer->char #x97)))
     (check-false r2)
     (check-equal? (utf8-accumulator-length) 2)
     (define r3 (utf8-accumulate-char (integer->char #xA5)))
     (check-equal? r3 #\日)
     (check-equal? (utf8-accumulator-length) 0))

   (test-case "reassemble-utf8-chars"
     (check-equal? (reassemble-utf8-chars (list #\A)) #\A)
     (check-equal? (reassemble-utf8-chars
                    (list (integer->char #xC3) (integer->char #xBC)))
                   #\ü))
   ))

(run-tests utf8-tests)
