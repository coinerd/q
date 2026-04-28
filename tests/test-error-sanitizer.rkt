#lang racket/base

(require rackunit
         (only-in "../util/error-sanitizer.rkt" sanitize-error-message))

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

(test-case "message with no home dir path passes through unchanged"
  (check-equal? (sanitize-error-message "/tmp/some/other/path error") "/tmp/some/other/path error"))

(test-case "double-slash in path is handled"
  ;; If the message contains home-dir//something, the home-prefix is
  ;; replaced with ~/ , leaving the extra slashes intact.
  (check-equal? (sanitize-error-message (format "~a//double" home-str)) "~///double"))
