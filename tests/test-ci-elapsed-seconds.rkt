#lang racket/base

;; @speed fast
;; @suite workflows
;; @boundary unit
;; @isolation temp-dir
;; @timeout 120

;; tests/test-ci-elapsed-seconds.rkt — testing-framework refactor §11.5.
;;
;; The ci.yml eager-gate stop stamp used to do its ms→seconds arithmetic in
;; `python3 -c "print(round(…, 3))"`. The shell keeps the integer subtraction
;; and scripts/ci/elapsed-seconds.rkt publishes the value, so this pins:
;;   - the value is a millisecond measurement in seconds, unchanged
;;     numerically (1234 ms ⇒ 1.234, never 1.2 and never a fraction),
;;   - it is ALWAYS three decimals (fixed width, so 1000 ms ⇒ 1.000 and a
;;     4 ms measurement ⇒ 0.004 instead of losing precision to 0.0),
;;   - the CLI writes exactly one `seconds=<value>` line ready for
;;     `$GITHUB_OUTPUT`, and refuses a malformed/negative duration (exit ≠ 0,
;;     nothing published).

(require rackunit
         racket/port
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         "../scripts/ci/elapsed-seconds.rkt")

(define-runtime-path here "test-ci-elapsed-seconds.rkt")
(define repo-root (simplify-path (build-path (path-only here) "..")))
(define script (build-path repo-root "scripts" "ci" "elapsed-seconds.rkt"))

(define (run-cli . args)
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port (open-output-nowhere)])
      (apply system*/exit-code (append (list (find-executable-path "racket")
                                             (path->string script))
                                       args))))
  (values code (get-output-string out)))

(define elapsed-seconds-suite
  (test-suite
   "scripts/ci/elapsed-seconds.rkt"

   (test-case "milliseconds become seconds with three decimals"
     (check-equal? (ms->seconds-string 1234) "1.234")
     (check-equal? (ms->seconds-string 1) "0.001")
     (check-equal? (ms->seconds-string 4) "0.004")
     (check-equal? (ms->seconds-string 999) "0.999")
     (check-equal? (ms->seconds-string 0) "0.000"))

   (test-case "whole seconds keep their width instead of collapsing"
     ;; The retired python form printed `1.0` here; the value is identical and
     ;; the fixed width keeps a duration readable and unambiguous.
     (check-equal? (ms->seconds-string 1000) "1.000")
     (check-equal? (ms->seconds-string 60000) "60.000"))

   (test-case "a non-duration is refused, never formatted"
     (check-exn exn:fail? (lambda () (ms->seconds-string -1)))
     (check-exn exn:fail? (lambda () (ms->seconds-string 1.5)))
     (check-exn exn:fail? (lambda () (ms->seconds-string "1234"))))

   (test-case "the CLI publishes exactly one seconds= line"
     (define-values (code out) (run-cli "--ms" "1234"))
     (check-equal? code 0 out)
     (check-equal? out "seconds=1.234\n"))

   (test-case "the CLI refuses a negative or malformed duration"
     (define-values (code out) (run-cli "--ms" "-5"))
     (check-not-equal? code 0)
     (check-equal? out "" "nothing may be published for a refused duration")
     (define-values (code2 out2) (run-cli "--ms" "abc"))
     (check-not-equal? code2 0)
     (check-equal? out2 "")
     (define-values (code3 _out3) (run-cli))
     (check-not-equal? code3 0))))

(module+ test
  (require rackunit/text-ui)
  (run-tests elapsed-seconds-suite))

(module+ main
  (require rackunit/text-ui)
  (exit (run-tests elapsed-seconds-suite)))
