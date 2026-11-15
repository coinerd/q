#lang racket

;; @speed slow
;; @suite default
;; @boundary integration  ;; @isolation process  ;; @mutates fs

;; q/tests/test-check-deps.rkt — tests for scripts/check-deps.rkt

(require rackunit
         racket/file
         racket/port)

(define q-dir
  (simplify-path
   (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                      (#%variable-reference))))
               "..")))
(define script-path (build-path q-dir "scripts" "check-deps.rkt"))
(define racket-path (find-executable-path "racket"))

(define (run-check root . args)
  (define out (open-output-string))
  (define exit-code
    (parameterize ([current-directory root]
                   [current-output-port out])
      (apply system*/exit-code racket-path script-path args)))
  (values exit-code (get-output-string out)))

(test-case "check-deps-passes-with-current-info-rkt"
  (define-values (exit-code out) (run-check q-dir))
  (check-equal? exit-code 0)
  (check-regexp-match #rx"All external packages declared" out)
  (check-false (regexp-match? #rx"x browser" out)
               "relative internal requires must not be classified as external browser package")
  (check-false (regexp-match? #rx"x wiring" out)
               "relative internal requires must not be classified as external wiring package"))

(test-case "check-deps-detects-missing-dep"
  (define info-path (build-path q-dir "info.rkt"))
  (define original (file->bytes info-path))
  (define root (make-temporary-file "check-deps-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (call-with-output-file
      (build-path root "info.rkt")
      (lambda (out) (display "#lang info\n(define deps '())\n(define build-deps '())\n" out)))
     (call-with-output-file
      (build-path root "fixture.rkt")
      (lambda (out)
        (display
         "#lang racket\n(require quickcheck \"../browser/adapter.rkt\" \"../wiring/example.rkt\")\n"
         out)))
     (define-values (exit-code out) (run-check root))
     (check-not-equal? exit-code 0 "Should fail when dep is missing")
     (check-regexp-match #rx"x quickcheck" out)
     (check-false (regexp-match #rx"x browser" out))
     (check-false (regexp-match #rx"x wiring" out)))
   (lambda () (delete-directory/files root)))
  (check-equal? (file->bytes info-path) original "Real info.rkt must remain unchanged"))

(test-case "check-deps-verbose-mode"
  (define-values (exit-code out) (run-check q-dir "-v"))
  (check-equal? exit-code 0)
  (check-regexp-match #rx"Scanned:" out)
  (check-regexp-match #rx"Declared deps:" out))
