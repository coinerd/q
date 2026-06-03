#lang racket/base

;; tests/test-context-assembly-pure.rkt — Pure context assembly selection tests (F23)
;; BOUNDARY: pure

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/selection.rkt"
         "../util/message/message.rkt")

(define (make-test-msg id text)
  (message id #f 'user 'user text 0 (hash)))

(define (const-estimate n)
  (lambda (msg) n))

(define context-assembly-pure-tests
  (test-suite "context-assembly-pure"

    (test-case "select-messages fits within budget"
      (define msgs
        (for/list ([i (in-range 10)])
          (make-test-msg (format "msg~a" i) (format "text ~a" i))))
      (define-values (selected excluded) (select-messages '() msgs 50 (const-estimate 10)))
      ;; Budget 50 / 10 per msg = 5 messages max
      (check-true (<= (length selected) 5))
      (check-true (> (length excluded) 0)))

    (test-case "select-messages with pinned messages"
      (define pinned (list (make-test-msg "p1" "pinned")))
      (define removable
        (for/list ([i (in-range 5)])
          (make-test-msg (format "r~a" i) (format "rem ~a" i))))
      (define-values (selected excluded) (select-messages pinned removable 30 (const-estimate 10)))
      ;; pinned takes 10, remaining 20 = 2 more
      (check-not-false (member (car pinned) selected))
      (check-true (>= (length selected) 3))) ;; at least pinned + some

    (test-case "select-messages all fit"
      (define msgs (list (make-test-msg "a" "one") (make-test-msg "b" "two")))
      (define-values (selected excluded) (select-messages '() msgs 1000 (const-estimate 10)))
      (check-equal? (length selected) 2)
      (check-equal? (length excluded) 0))

    (test-case "select-messages nothing fits"
      (define msgs
        (for/list ([i (in-range 5)])
          (make-test-msg (format "b~a" i) (format "big ~a" i))))
      (define-values (selected excluded) (select-messages '() msgs 5 (const-estimate 10)))
      (check-equal? (length selected) 0)
      (check-equal? (length excluded) 5))))

(run-tests context-assembly-pure-tests)
