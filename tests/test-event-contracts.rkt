#lang racket

;; @speed fast
;; @suite default

(require rackunit
         rackunit/text-ui
         "../util/event/event-classes.rkt")

(define event-contract-tests
  (test-suite "event-classes contracts"
    (test-case "stream constructors accept hash payloads"
      (check-true (stream-text-event? (make-stream-text-event "s" (hasheq 'text "hi"))))
      (check-true (stream-tool-call-event? (make-stream-tool-call-event "s" (hasheq 'id "tc1"))))
      (check-true (stream-thinking-event? (make-stream-thinking-event "s"
                                                                      (hasheq 'text "thinking")))))

    (test-case "stream constructors reject non-hash payloads at contract boundary"
      (check-exn exn:fail:contract? (lambda () (make-stream-text-event "s" "not-a-hash")))
      (check-exn exn:fail:contract? (lambda () (make-stream-tool-call-event "s" "not-a-hash")))
      (check-exn exn:fail:contract? (lambda () (make-stream-thinking-event "s" "not-a-hash"))))))

(module+ main
  (run-tests event-contract-tests))
