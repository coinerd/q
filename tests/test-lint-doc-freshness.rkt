#lang racket

;; @speed fast
;; @suite default

;; tests/test-lint-doc-freshness.rkt — Doc freshness lint tests (v0.54.6 W2)

(require rackunit
         rackunit/text-ui)

(define freshness-suite
  (test-suite "lint-doc-freshness tests"

    (test-case "version regex matches verified-against"
      (define m (regexp-match #rx"verified-against:[ ]*([0-9]+\\.[0-9]+\\.[0-9]+)"
                              "<!-- verified-against: 0.54.5 -->"))
      check-true m
      (check-equal? (cadr m) "0.54.5"))

    (test-case "version regex matches ## Version heading"
      (define m (regexp-match #rx"## Version[ \t]*\n+v?([0-9]+\\.[0-9]+\\.[0-9]+)"
                              "## Version\n\nv0.54.5\n"))
      check-true m
      (check-equal? (cadr m) "0.54.5"))

    (test-case "version regex matches inline Q version"
      (define m (regexp-match #rx"[Qq] ([0-9]+\\.[0-9]+\\.[0-9]+)"
                              "Complete reference for all event types in Q 0.54.5."))
      check-true m
      (check-equal? (cadr m) "0.54.5"))

    (test-case "script runs and exits 0"
      (define-values (sp out in err)
        (subprocess #f #f #f (find-executable-path "racket")
                    "scripts/lint-doc-freshness.rkt"))
      (close-output-port in)
      (subprocess-wait sp)
      (define code (subprocess-status sp))
      (close-input-port out)
      (close-input-port err)
      (check-equal? code 0))))

(run-tests freshness-suite)
