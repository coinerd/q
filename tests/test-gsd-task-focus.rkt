#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-task-focus.rkt — v0.75.7 W3 tests
;; Task focus preservation: extract-task-summary

(require rackunit
         rackunit/text-ui
         racket/string
         "../extensions/gsd/command-handlers.rkt"
         "../extensions/gsd/plan-types.rkt")

(define-test-suite test-task-focus
                   (test-case "extract-task-summary with waves"
                     (define waves
                       (list (gsd-wave 0 "Setup" 'pending "" '() '() "" '())
                             (gsd-wave 1 "Implement" 'pending "" '() '() "" '())
                             (gsd-wave 2 "Verify" 'pending "" '() '() "" '())))
                     (define plan (gsd-plan waves "" '() '()))
                     (define summary (extract-task-summary plan))
                     (check-not-false (regexp-match? #rx"W0: Setup" summary))
                     (check-not-false (regexp-match? #rx"W1: Implement" summary))
                     (check-not-false (regexp-match? #rx"W2: Verify" summary))
                     (check-not-false (regexp-match? #rx"DO NOT FORGET" summary)))
                   (test-case "extract-task-summary with no waves"
                     (define plan (gsd-plan '() "" '() '()))
                     (define summary (extract-task-summary plan))
                     (check-equal? summary ""))
                   (test-case "extract-task-summary includes stay-focused reminder"
                     (define waves (list (gsd-wave 0 "Fix bug" 'pending "" '() '() "" '())))
                     (define plan (gsd-plan waves "" '() '()))
                     (define summary (extract-task-summary plan))
                     (check-not-false (regexp-match? #rx"Stay focused" summary))))

(run-tests test-task-focus)
