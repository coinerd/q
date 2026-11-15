#lang racket/base

;; q/scripts/ci/elapsed-seconds.rkt — testing-framework refactor §11.5.
;;
;; Formats a millisecond duration as the seconds string the CI steps publish
;; in `$GITHUB_OUTPUT` (e.g. 1234 → "1.234"). This replaces the inline
;; `python3 -c "print(round(…, 3))"` arithmetic in ci.yml's eager-gate stamp:
;; the shell keeps the integer subtraction (no floating point in bash) and the
;; pinned Racket toolchain does the decimal formatting, so no interpreter
;; outside the toolchain is installed for a timing stamp.
;;
;; Contract: ALWAYS three decimal places, so a whole-second measurement still
;; looks like a duration ("1.000", never "1" or "1.0"), and a sub-second one
;; never loses precision to integer division ("0.004", never "0"). The value is
;; numerically identical to the previous `round(ms / 1000, 3)` output; only the
;; trailing-zero spelling is fixed-width.
;;
;; Usage:
;;   racket scripts/ci/elapsed-seconds.rkt --ms <non-negative integer>
;;
;; Writes one line to stdout: `seconds=<value>`, ready to be appended to
;; `$GITHUB_OUTPUT`. Exit 1 (no stdout) on a malformed or negative duration, so
;; a broken stamp can never publish a fabricated number.

(require racket/cmdline
         racket/format)

(provide ms->seconds-string)

;; Milliseconds (exact non-negative integer) → "S.mmm".
(define (ms->seconds-string ms)
  (unless (and (exact-integer? ms) (>= ms 0))
    (raise-argument-error 'ms->seconds-string "non-negative exact integer" ms))
  (real->decimal-string (exact->inexact (/ ms 1000)) 3))

(define (main argv)
  (define raw #f)
  (command-line
   #:program "elapsed-seconds.rkt"
   #:once-each
   [("--ms") value "Elapsed time in whole milliseconds" (set! raw value)]
   #:args ()
   (void))
  (cond
    [(not raw)
     (eprintf "elapsed-seconds: --ms <integer> is required\n")
     2]
    [else
     (define ms (string->number raw))
     (cond
       [(and ms (exact-integer? ms) (>= ms 0))
        (printf "seconds=~a\n" (ms->seconds-string ms))
        0]
       [else
        (eprintf "elapsed-seconds: --ms must be a non-negative integer, got ~s\n" raw)
        1])]))

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))
