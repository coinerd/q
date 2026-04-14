#lang racket

;; tests/tui/test-cursor-marker.rkt — Tests for IME cursor marker (#464)

(require rackunit
         rackunit/text-ui
         "../../../q/tui/terminal.rkt")

(define-test-suite test-cursor-marker

  (test-case "CURSOR-MARKER is a non-empty string"
    (check-pred string? CURSOR-MARKER)
    (check-true (> (string-length CURSOR-MARKER) 0)))

  (test-case "CURSOR-MARKER uses APC protocol"
    ;; Should start with ESC _ (APC introducer)
    (check-equal? (substring CURSOR-MARKER 0 2) "\x1b_")
    ;; Should end with BEL (0x07)
    (define last-char (string-ref CURSOR-MARKER (sub1 (string-length CURSOR-MARKER))))
    (check-equal? (char->integer last-char) 7))

  (test-case "strip-cursor-marker returns clean string when no marker"
    (define-values (clean pos) (strip-cursor-marker "hello world"))
    (check-equal? clean "hello world")
    (check-false pos))

  (test-case "strip-cursor-marker removes marker from string"
    (define-values (clean pos) (strip-cursor-marker (string-append "hello" CURSOR-MARKER)))
    (check-equal? clean "hello")
    (check-equal? pos (cons 0 5)))

  (test-case "strip-cursor-marker handles marker in middle"
    (define-values (clean pos) (strip-cursor-marker (string-append "abc" CURSOR-MARKER "def")))
    (check-equal? clean "abcdef")
    (check-equal? pos (cons 0 3)))

  (test-case "strip-cursor-marker handles marker in multi-line string"
    (define-values (clean pos)
      (strip-cursor-marker (string-append "line0\nline1" CURSOR-MARKER)))
    (check-equal? clean "line0\nline1")
    (check-equal? pos (cons 1 5)))

  (test-case "strip-cursor-marker handles only marker"
    (define-values (clean pos) (strip-cursor-marker CURSOR-MARKER))
    (check-equal? clean "")
    (check-equal? pos (cons 0 0))))

(run-tests test-cursor-marker)
