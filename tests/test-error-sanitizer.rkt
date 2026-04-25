#lang racket/base

(require rackunit
         (only-in "../util/error-sanitizer.rkt" sanitize-error-message))

(define home-str (path->string (find-system-path 'home-dir)))

;; Home directory is replaced with ~
(check-equal? (sanitize-error-message (format "File not found: ~afoo.txt" home-str))
              "File not found: ~/foo.txt")

;; Non-path messages pass through unchanged
(check-equal? (sanitize-error-message "no paths here")
              "no paths here")

;; Path in middle of message is sanitized
(check-equal? (sanitize-error-message (format "Write error: cannot write to ~abar/baz.rkt: permission denied" home-str))
              "Write error: cannot write to ~/bar/baz.rkt: permission denied")

;; Multiple occurrences are all replaced
(check-equal? (sanitize-error-message (format "~afoo and ~abar" home-str home-str))
              "~/foo and ~/bar")
