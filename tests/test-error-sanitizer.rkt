#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         (only-in "../util/error/error-sanitizer.rkt" sanitize-error-message))

(define home-str (path->string (find-system-path 'home-dir)))

;; ── Existing checks wrapped in test-case ──────────────────────────

(test-case "home-directory replaced with ~"
  (check-equal? (sanitize-error-message (format "File not found: ~afoo.txt" home-str))
                "File not found: ~/foo.txt"))

(test-case "non-path messages pass through unchanged"
  (check-equal? (sanitize-error-message "no paths here") "no paths here"))

(test-case "path in middle of message is sanitized"
  (check-equal? (sanitize-error-message
                 (format "Write error: cannot write to ~abar/baz.rkt: permission denied" home-str))
                "Write error: cannot write to ~/bar/baz.rkt: permission denied"))

(test-case "multiple occurrences are all replaced"
  (check-equal? (sanitize-error-message (format "~afoo and ~abar" home-str home-str))
                "~/foo and ~/bar"))

;; ── Edge cases (TH-14) ────────────────────────────────────────────

(test-case "empty string input returns empty string"
  (check-equal? (sanitize-error-message "") ""))

(test-case "bare home dir is replaced with ~"
  ;; When home-str already has trailing /, the sanitizer replaces it with ~/
  ;; so sanitize-error-message(home-str) = "~/"
  (check-equal? (sanitize-error-message home-str) "~/"))

(test-case "home dir with no trailing content becomes ~"
  ;; home-str already has trailing /, so it matches home-prefix exactly.
  (check-equal? (sanitize-error-message (string-append home-str)) "~/"))

(test-case "/tmp/ paths are now redacted (SEC-09)"
  (check-equal? (sanitize-error-message "/tmp/some/other/path error") "[REDACTED] error"))

(test-case "double-slash in path is handled"
  ;; If the message contains home-dir//something, the home-prefix is
  ;; replaced with ~/ , leaving the extra slashes intact.
  (check-equal? (sanitize-error-message (format "~a//double" home-str)) "~///double"))

;; ── SEC-09: API keys, /tmp/ paths, emails ─────────────────────────

(test-case "API key sk-... is redacted"
  (check-equal? (sanitize-error-message "key=sk-abc123 failed") "key=[REDACTED] failed"))

(test-case "API key ghp_... is redacted"
  (check-equal? (sanitize-error-message "token ghp_xyz789 is invalid") "token [REDACTED] is invalid"))

(test-case "API key xoxb-... is redacted"
  (check-equal? (sanitize-error-message "using xoxb-1234-5678 for slack")
                "using [REDACTED] for slack"))

(test-case "API key key-... is redacted"
  (check-equal? (sanitize-error-message "auth key-abc failed") "auth [REDACTED] failed"))

(test-case "email address is redacted"
  (check-equal? (sanitize-error-message "sent to user@example.com") "sent to [REDACTED]"))

(test-case "/tmp/ path is redacted"
  (check-equal? (sanitize-error-message "/tmp/secret-dir/file.txt not found") "[REDACTED] not found"))

(test-case "string without sensitive patterns passes through unchanged"
  (check-equal? (sanitize-error-message "just a normal error message") "just a normal error message"))

(test-case "multiple sensitive patterns are all redacted"
  (check-equal? (sanitize-error-message
                 (format "key=sk-abc email=user@example.com tmp=/tmp/secret/dir home=~astuff"
                         home-str))
                "key=[REDACTED] email=[REDACTED] tmp=[REDACTED] home=~/stuff"))

;; ── AUDIT-12: non-string input edge case ──────────────────────────

(test-case "sanitize-error-message handles non-string input gracefully"
  ;; If called with #f, should raise a clear error
  (check-exn exn:fail? (lambda () (sanitize-error-message #f))))
