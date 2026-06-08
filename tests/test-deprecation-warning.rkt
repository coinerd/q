#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-deprecation-warning.rkt -- Tests for warn-deprecated! utility (S3)

(require rackunit
         rackunit/text-ui
         racket/logging
         (only-in "../util/error/errors.rkt" warn-deprecated!))

(define deprecation-suite
  (test-suite "Deprecation warning utility"

    (test-case "warn-deprecated! does not raise"
      (check-not-exn (lambda () (warn-deprecated! 'old-fn "v0.46.0" "Use new-fn instead"))))

    (test-case "warn-deprecated! works without extra notes"
      (check-not-exn (lambda () (warn-deprecated! 'another-old-fn "v0.50.0"))))

    ;; v0.44.4 (S11): Verify log message content
    (test-case "warn-deprecated! produces warning log message"
      (define messages '())
      (with-intercepted-logging
       (lambda (log-entry) (set! messages (cons (vector-ref log-entry 1) messages)))
       (lambda () (warn-deprecated! 'old-symbol "v0.46.0" "Use new-symbol instead"))
       'warning)
      (check-true (>= (length messages) 1) "Expected at least one log message")
      (define msg (car messages))
      (check-true (string-contains? msg "DEPRECATED"))
      (check-true (string-contains? msg "old-symbol"))
      (check-true (string-contains? msg "v0.46.0"))
      (check-true (string-contains? msg "Use new-symbol instead")))))

(run-tests deprecation-suite)
