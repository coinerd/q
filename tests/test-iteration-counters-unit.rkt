#lang racket

;; @speed fast  ;; @suite runtime
;; BOUNDARY: pure
;; BOUNDARY: unit
;; tests/test-iteration-counters-unit.rkt -- Counter computation tests (T-1a)
;;
;; Tests compute-next-counters with real message structures.
;; check-cancellation is impure (emits events) -- integration-level test needed.
;; Documented gap: check-cancellation requires event bus mock.

(require rackunit
         rackunit/text-ui
         "../agent/iteration/counters.rkt"
         "../agent/iteration/loop-state.rkt"
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/content/content-parts.rkt" make-tool-call-part))

;; Helper: create a message struct with tool-call content parts
(define (make-tool-msg tool-names)
  (define content
    (for/list ([n (in-list tool-names)])
      (make-tool-call-part (format "tc-~a" n) n (hasheq))))
  (make-message "mid" #f 'assistant 'tool-call content 0 (hasheq)))

;; Helper: create a tool-call message where each spec is (name args-hash)
(define (make-tool-msg/args . specs)
  (define content
    (for/list ([spec (in-list specs)])
      (match-define (list n args) spec)
      (make-tool-call-part (format "tc-~a" n) n args)))
  (make-message "mid-args" #f 'assistant 'tool-call content 0 (hasheq)))

(define (make-text-turn)
  (make-message "mid-text" #f 'assistant 'message '() 0 (hasheq)))

(define base-counters (make-initial-counters))

(define counters-suite
  (test-suite "compute-next-counters"

    (test-case "a turn without tool calls resets the consecutive-tool count"
      (define seeded (struct-copy loop-counters base-counters [consecutive-tool-count 4]))
      (define result (compute-next-counters seeded (list (make-text-turn))))
      (check-equal? (loop-counters-consecutive-tool-count result) 0)
      (check-equal? (loop-counters-explore-count result) (loop-counters-explore-count base-counters))
      (check-equal? (loop-counters-implement-count result)
                    (loop-counters-implement-count base-counters)))

    (test-case "every tool-only turn increments consecutive-tool-count once"
      (define seeded (struct-copy loop-counters base-counters [consecutive-tool-count 4]))
      (define msgs (list (make-tool-msg '("bash"))))
      (define result (compute-next-counters seeded msgs))
      (check-equal? (loop-counters-consecutive-tool-count result) 5))

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
      ;; Count consecutive tool-only turns, not the number or class of calls.
      (check-equal? (loop-counters-consecutive-tool-count result) 1))

    (test-case "repeated reads of the same path still count as consecutive tool turns"
      (define first (compute-next-counters base-counters (list (make-tool-msg '("read")))))
      (define second (compute-next-counters first (list (make-tool-msg '("read")))))
      (check-equal? (loop-counters-consecutive-tool-count second) 2))

    (test-case "BUG-0016: editing a NEW distinct file resets the consecutive-tool count"
      ;; A bulk-migration turn that edits a file not yet edited in the streak is
      ;; implementation progress, not circling — the streak must reset so the
      ;; wave is never policy-killed mid-migration.
      (define seeded (struct-copy loop-counters base-counters [consecutive-tool-count 190]))
      (define msgs (list (make-tool-msg/args (list "edit" (hasheq 'path "/tmp/a.rkt")))))
      (define result (compute-next-counters seeded msgs))
      (check-equal? (loop-counters-consecutive-tool-count result) 0)
      (check-equal? (loop-counters-edited-paths result) '("/tmp/a.rkt")))

    (test-case "BUG-0016: editing the SAME file again does NOT reset the streak"
      ;; Re-editing an already-edited file is a potential circle on one file; the
      ;; breaker must keep counting so a same-file loop still fails closed.
      (define seeded
        (struct-copy loop-counters
                     base-counters
                     [consecutive-tool-count 190]
                     [edited-paths '("/tmp/a.rkt")]))
      (define msgs (list (make-tool-msg/args (list "edit" (hasheq 'path "/tmp/a.rkt")))))
      (define result (compute-next-counters seeded msgs))
      (check-equal? (loop-counters-consecutive-tool-count result) 191)
      (check-equal? (loop-counters-edited-paths result) '("/tmp/a.rkt")))

    (test-case "BUG-0016: multiple distinct-file edits in one turn reset once"
      (define seeded (struct-copy loop-counters base-counters [consecutive-tool-count 300]))
      (define msgs
        (list (make-tool-msg/args (list "edit" (hasheq 'path "/tmp/a.rkt"))
                                  (list "write" (hasheq 'path "/tmp/b.rkt")))))
      (define result (compute-next-counters seeded msgs))
      (check-equal? (loop-counters-consecutive-tool-count result) 0)
      (check-equal? (loop-counters-edited-paths result) '("/tmp/a.rkt" "/tmp/b.rkt")))

    (test-case "recent-tool-names tracks tools"
      (define msgs (list (make-tool-msg '("bash"))))
      (define result (compute-next-counters base-counters msgs))
      (check-equal? (loop-counters-recent-tool-names result) '("bash")))

    (test-case "iteration counter stays at base value"
      (define result (compute-next-counters base-counters '()))
      (check-equal? (loop-counters-iteration result) (loop-counters-iteration base-counters)))))

(run-tests counters-suite 'verbose)
