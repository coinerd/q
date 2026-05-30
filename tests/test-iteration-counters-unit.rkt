#lang racket
;; BOUNDARY: pure
;; BOUNDARY: unit
;; tests/test-iteration-counters-unit.rkt -- Counter computation tests (T-1a)
;;
;; Tests compute-next-counters with real message structures.
;; check-cancellation is impure (emits events) -- integration-level test needed.
;; Documented gap: check-cancellation requires event bus mock.

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/counters.rkt"
         "../runtime/iteration/loop-state.rkt"
         (only-in "../util/message.rkt" make-message)
         (only-in "../util/content-parts.rkt" make-tool-call-part))

;; Helper: create a message struct with tool-call content parts
(define (make-tool-msg tool-names)
  (define content (for/list ([n (in-list tool-names)])
                    (make-tool-call-part (format "tc-~a" n) n (hasheq))))
  (make-message "mid" #f 'assistant 'tool-call content 0 (hasheq)))

(define base-counters
  (make-initial-counters))

(define counters-suite
  (test-suite "compute-next-counters"

    (test-case "empty messages leaves counters mostly unchanged"
      (define result (compute-next-counters base-counters '()))
      (check-equal? (loop-counters-consecutive-tool-count result)
                    (loop-counters-consecutive-tool-count base-counters))
      (check-equal? (loop-counters-explore-count result)
                    (loop-counters-explore-count base-counters))
      (check-equal? (loop-counters-implement-count result)
                    (loop-counters-implement-count base-counters)))

    (test-case "message with bash tool call (non-read) does not increment consecutive-tool-count"
      ;; consecutive-tool-count only increments for read-tools with new paths
      (define msgs (list (make-tool-msg '("bash"))))
      (define result (compute-next-counters base-counters msgs))
      (check-equal? (loop-counters-consecutive-tool-count result) 0))

    (test-case "explore tools (read) increment explore-count"
      (define msgs (list (make-tool-msg '("read"))))
      (define result (compute-next-counters base-counters msgs))
      (check-equal? (loop-counters-explore-count result) 1)
      (check-equal? (loop-counters-implement-count result) 0))

    (test-case "implement tools (edit) increment implement-count"
      (define msgs (list (make-tool-msg '("edit"))))
      (define result (compute-next-counters base-counters msgs))
      (check-equal? (loop-counters-implement-count result) 1)
      (check-equal? (loop-counters-explore-count result) 0))

    (test-case "non-explore non-implement tools don't increment explore/implement"
      (define msgs (list (make-tool-msg '("bash"))))
      (define result (compute-next-counters base-counters msgs))
      (check-equal? (loop-counters-explore-count result) 0)
      (check-equal? (loop-counters-implement-count result) 0))

    (test-case "multiple tool calls increment all relevant counts"
      (define msgs (list (make-tool-msg '("read" "edit"))))
      (define result (compute-next-counters base-counters msgs))
      (check-equal? (loop-counters-explore-count result) 1)
      (check-equal? (loop-counters-implement-count result) 1)
      ;; "edit" is non-read, so consecutive-tool-count does not increment
      (check-equal? (loop-counters-consecutive-tool-count result) 0))

    (test-case "recent-tool-names tracks tools"
      (define msgs (list (make-tool-msg '("bash"))))
      (define result (compute-next-counters base-counters msgs))
      (check-equal? (loop-counters-recent-tool-names result) '("bash")))

    (test-case "iteration counter stays at base value"
      (define result (compute-next-counters base-counters '()))
      (check-equal? (loop-counters-iteration result)
                    (loop-counters-iteration base-counters)))
    ))

(run-tests counters-suite 'verbose)
