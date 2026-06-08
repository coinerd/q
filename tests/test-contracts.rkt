#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: contract

;; tests/test-contracts.rkt — Boundary contract tests
;;
;; Verifies that sdk-public.rkt provides the correct contracted API
;; and that contracts are enforced at the SDK boundary.

(require rackunit
         "../interfaces/sdk-public.rkt"
         (only-in "../extensions/api.rkt" extension-registry?)
         (only-in "../tools/tool.rkt" tool-registry?))

;; ============================================================
;; SDK boundary smoke tests
;; ============================================================

(test-case "sdk-public: make-runtime requires #:provider"
  (check-exn exn:fail:contract?
             (lambda ()
               ;; Missing #:provider should trigger contract error
               (make-runtime))))

(test-case "sdk-public: run-prompt! requires runtime? and string?"
  (check-exn exn:fail:contract?
             (lambda ()
               ;; Passing wrong types should trigger contract error
               (run-prompt! "not-a-runtime" "hello"))))

(test-case "sdk-public: make-event-bus returns event-bus?"
  (define bus (make-event-bus))
  (check-pred event-bus? bus))

(test-case "sdk-public: make-extension-registry returns registry"
  (define reg (make-extension-registry))
  (check-pred extension-registry? reg))

(test-case "sdk-public: make-tool-registry returns registry"
  (define reg (make-tool-registry))
  (check-pred tool-registry? reg))

(test-case "sdk-public: make-mock-provider is exported and procedure?"
  (check-true (procedure? make-mock-provider)))

(test-case "sdk-public: cancellation-token contract"
  (define tok (make-cancellation-token))
  (check-pred cancellation-token? tok)
  (check-false (cancellation-token-cancelled? tok))
  (cancel-token! tok)
  (check-true (cancellation-token-cancelled? tok)))

;; ============================================================
;; Struct export checks
;; ============================================================

(test-case "sdk-public: runtime-config? predicate exported"
  (check-true (procedure? runtime-config?)))

(test-case "sdk-public: navigate-result? predicate exported"
  (check-true (procedure? navigate-result?)))

(test-case "sdk-public: compaction-result? predicate exported"
  (check-true (procedure? compaction-result?)))

(test-case "sdk-public: context-usage? predicate exported"
  (check-true (procedure? context-usage?)))

;; ============================================================
;; Enriched API aliases exported
;; ============================================================

(test-case "sdk-public: enriched API aliases are procedures"
  (for ([proc (in-list (list q:create-session
                             q:session-send
                             q:session-subscribe
                             q:session-interrupt
                             q:session-fork
                             q:session-compact
                             q:session-info
                             q:session-branch
                             q:session-navigate
                             q:session-tree-info))])
    (check-true (procedure? proc) (format "Expected procedure, got ~a" proc))))

;; ============================================================
;; GSD API exported
;; ============================================================

(test-case "sdk-public: GSD API procedures exported"
  (check-true (procedure? q:plan))
  (check-true (procedure? q:go))
  (check-true (procedure? q:gsd-status))
  (check-true (procedure? q:reset-gsd!)))

;; ============================================================
;; Negative contract-rejection tests (SDK-02)
;; ============================================================

(test-case "sdk-public: open-session rejects non-runtime"
  (check-exn exn:fail:contract? (lambda () (open-session "not-a-runtime"))))

(test-case "sdk-public: interrupt! rejects non-runtime"
  (check-exn exn:fail:contract? (lambda () (interrupt! 42))))

(test-case "sdk-public: fork-session! rejects non-runtime"
  (check-exn exn:fail:contract? (lambda () (fork-session! 'nope))))

(test-case "sdk-public: steer! rejects non-runtime"
  (check-exn exn:fail:contract? (lambda () (steer! "not-runtime" "prompt"))))

(test-case "sdk-public: dispatch-command! rejects non-runtime"
  (check-exn exn:fail:contract? (lambda () (dispatch-command! #f "cmd" "arg"))))

(test-case "sdk-public: subscribe-events! rejects non-procedure callback"
  (check-exn exn:fail:contract? (lambda () (subscribe-events! "not-runtime" 42))))

(test-case "sdk-public: session:set-thinking-level! rejects invalid level"
  (check-exn exn:fail:contract? (lambda () (session:set-thinking-level! 'not-a-session 'invalid))))

(test-case "sdk-public: get-context-usage rejects non-integer"
  (check-exn exn:fail:contract? (lambda () (get-context-usage "not-a-number" -1))))
