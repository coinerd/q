#lang racket

;; q/tests/test-check-deps.rkt — tests for scripts/check-deps.rkt

(require rackunit
         racket/file
         racket/port)

(define q-dir
  (simplify-path
   (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                      (#%variable-reference))))
               "..")))

(test-case "check-deps-passes-with-current-info-rkt"
  (define cmd (format "cd ~a && racket scripts/check-deps.rkt" q-dir))
  (define out (with-output-to-string (lambda () (void (system cmd)))))
  (check-regexp-match #rx"All external packages declared" out))

(test-case "check-deps-detects-missing-dep"
  (define info-path (build-path q-dir "info.rkt"))
  (define original (file->string info-path))
  ;; Remove quickcheck from deps to simulate a gap
  (define broken (string-replace original "\"quickcheck\"" "\"REMOVED-FAKE\""))
  (dynamic-wind
   (lambda () (call-with-output-file info-path (lambda (o) (display broken o)) #:exists 'truncate))
   (lambda ()
     (define cmd (format "cd ~a && racket scripts/check-deps.rkt" q-dir))
     (define out (with-output-to-string (lambda () (void (system cmd)))))
     (define exit-code
       (parameterize ([current-directory q-dir])
         (system/exit-code "racket scripts/check-deps.rkt")))
     (check-not-equal? exit-code 0 "Should fail when dep is missing"))
   (lambda ()
     (call-with-output-file info-path (lambda (o) (display original o)) #:exists 'truncate))))

(test-case "check-deps-verbose-mode"
  (define cmd (format "cd ~a && racket scripts/check-deps.rkt -v" q-dir))
  (define out (with-output-to-string (lambda () (void (system cmd)))))
  (check-regexp-match #rx"Scanned:" out)
  (check-regexp-match #rx"Declared deps:" out))
