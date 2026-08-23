#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-lint-doc-freshness.rkt — Doc freshness lint tests (v0.54.6 W2)

(require rackunit
         rackunit/text-ui
         racket/port)

(define q-root (simplify-path (build-path (syntax-source #'here) ".." "..")))
(define script-path (build-path q-root "scripts" "lint-doc-freshness.rkt"))

(define freshness-suite
  (test-suite "lint-doc-freshness tests"

    (test-case "version regex matches verified-against"
      (define m
        (regexp-match #rx"verified-against:[ ]*([0-9]+\\.[0-9]+\\.[0-9]+)"
                      "<!-- verified-against: 0.54.5 -->"))
      (check-not-false m)
      (check-equal? (cadr m) "0.54.5"))

    (test-case "version regex matches ## Version heading"
      (define m
        (regexp-match #rx"## Version[ \t]*\n+v?([0-9]+\\.[0-9]+\\.[0-9]+)" "## Version\n\nv0.54.5\n"))
      (check-not-false m)
      (check-equal? (cadr m) "0.54.5"))

    (test-case "version regex matches inline Q version"
      (define m
        (regexp-match #rx"[Qq] ([0-9]+\\.[0-9]+\\.[0-9]+)"
                      "Complete reference for all event types in Q 0.54.5."))
      (check-not-false m)
      (check-equal? (cadr m) "0.54.5"))

    (test-case "agent-harness-runbook exists with version marker"
      (define runbook (build-path q-root "docs" "agent-harness-runbook.md"))
      (check-true (file-exists? runbook))
      (define text (call-with-input-file runbook port->string))
      (check-regexp-match #rx"verified-against:[ ]*[0-9]+\\.[0-9]+\\.[0-9]+" text))

    (test-case "agent-harness-runbook documents background-gate pattern"
      (define runbook (build-path q-root "docs" "agent-harness-runbook.md"))
      (define text (call-with-input-file runbook port->string))
      (check-regexp-match #rx"nohup" text)
      (check-regexp-match #rx"MUST (Run|run) in the (Background|background)" text)
      (check-regexp-match #rx"VERDICT:" text))

    (test-case "agent-harness-runbook documents exit-137 interpretation"
      (define runbook (build-path q-root "docs" "agent-harness-runbook.md"))
      (define text (call-with-input-file runbook port->string))
      (check-regexp-match #rx"137" text)
      (check-regexp-match #rx"SIGKILL" text)
      (check-regexp-match #rx"T`-state|SIGSTOP" text))

    (test-case "agent-harness-runbook documents post-W1 timeout behavior"
      (define runbook (build-path q-root "docs" "agent-harness-runbook.md"))
      (define text (call-with-input-file runbook port->string))
      (check-regexp-match #rx"SIGTERM" text)
      (check-regexp-match #rx"SIGKILL" text)
      (check-regexp-match #rx"foreground timeout now returns a result" text))

    (test-case "script runs and exits 0"
      (define-values (sp out in err)
        (parameterize ([current-directory q-root])
          (subprocess #f #f #f (find-executable-path "racket") (path->string script-path))))
      (close-output-port in)
      (subprocess-wait sp)
      (define code (subprocess-status sp))
      (close-input-port out)
      (close-input-port err)
      (check-equal? code 0))))

(run-tests freshness-suite)
