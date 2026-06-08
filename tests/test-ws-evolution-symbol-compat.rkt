#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-ws-evolution-symbol-compat.rkt
;; Regression test: evolve-working-set-for-state must accept raw symbols
;; (not just fsm-state? structs) because the runtime stores task-fsm-state
;; as raw symbols in agent-session.
;;
;; Bug: fsm-state-name contract violation when given 'exploration instead of
;; (fsm-state 'exploration).

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/working-set.rkt"
                  make-working-set
                  working-set?
                  working-set-entries
                  working-set-add!
                  working-set-reset!
                  ws-entry-path)
         (only-in "../runtime/context-assembly/ws-evolution.rkt"
                  evolve-working-set-for-state
                  evolve-working-set-for-state/result
                  evolution-result?
                  evolution-result-kept-entries
                  evolution-result-archived-entries
                  evolution-result-evicted-conclusions)
         (only-in "../runtime/context-assembly/task-conclusion.rkt" task-conclusion task-conclusion?))

(define (populate-ws ws paths)
  (for ([p (in-list paths)])
    (working-set-add! ws p (format "msg-~a" p) 500)))

(define conclusions
  (list (task-conclusion "c1" "Use struct for data" 'fact 'idle '() (current-seconds) '() '())
        (task-conclusion "c2" "Tests in tests/" 'fact 'idle '() (current-seconds) '() '())))

(define suite
  (test-suite
   "ws-evolution symbol compat (regression)"

   ;; RAW SYMBOLS — exactly what turn-orchestrator passes
   (test-case "evolve-working-set-for-state: raw symbol exploration→planning"
     (define ws (make-working-set))
     (populate-ws ws '("/tmp/a.rkt" "/tmp/b.rkt" "/tmp/c.rkt"))
     ;; Pass raw symbols, NOT fsm-state? structs
     (define result (evolve-working-set-for-state ws 'exploration 'planning conclusions))
     (check-true (list? result))
     (check-true (andmap task-conclusion? result)))

   (test-case "evolve-working-set-for-state: raw symbol exploration→implementation"
     (define ws (make-working-set))
     (populate-ws ws '("/tmp/a.rkt" "/tmp/b.rkt"))
     (define result (evolve-working-set-for-state ws 'exploration 'implementation conclusions))
     (check-true (list? result)))

   (test-case "evolve-working-set-for-state: raw symbol idle→exploration"
     (define ws (make-working-set))
     (populate-ws ws '("/tmp/a.rkt"))
     ;; idle→exploration: no evolution needed, return empty
     (define result (evolve-working-set-for-state ws 'idle 'exploration conclusions))
     (check-true (list? result)))

   (test-case "evolve-working-set-for-state: raw symbol implementation→debugging"
     (define ws (make-working-set))
     (populate-ws ws '("/tmp/a.rkt" "/tmp/test-foo.rkt" "/tmp/bar.rkt"))
     (define result (evolve-working-set-for-state ws 'implementation 'debugging conclusions))
     (check-true (list? result)))

   (test-case "evolve-working-set-for-state: #f old-state (unknown)"
     (define ws (make-working-set))
     (populate-ws ws '("/tmp/a.rkt"))
     ;; old-state = #f (unknown, first turn)
     (define result (evolve-working-set-for-state ws #f 'exploration conclusions))
     (check-true (list? result)))

   (test-case "evolve-working-set-for-state/result: raw symbols"
     (define ws (make-working-set))
     (populate-ws ws '("/tmp/a.rkt" "/tmp/b.rkt" "/tmp/c.rkt"))
     (define result (evolve-working-set-for-state/result ws 'exploration 'planning conclusions))
     (check-true (evolution-result? result))
     (check-true (list? (evolution-result-kept-entries result)))
     (check-true (list? (evolution-result-archived-entries result)))
     (check-true (list? (evolution-result-evicted-conclusions result))))

   (test-case "evolve-working-set-for-state/result: #f→exploration (first turn)"
     (define ws (make-working-set))
     (populate-ws ws '("/tmp/a.rkt"))
     (define result (evolve-working-set-for-state/result ws #f 'exploration conclusions))
     (check-true (evolution-result? result)))))

(run-tests suite)
