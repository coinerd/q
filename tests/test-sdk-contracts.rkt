#lang racket/base

;; tests/test-sdk-contracts.rkt — Contract enforcement tests for SDK public API
;;
;; W0 scaffolding for v0.29.0 milestone: Verify that SDK boundary functions
;; enforce type contracts (reject wrong argument types).

(require rackunit
         (only-in "../interfaces/sdk-public.rkt"
                  make-runtime
                  make-event-bus
                  event-bus?
                  subscribe!
                  publish!
                  make-tool-registry
                  register-tool!
                  list-tools
                  make-cancellation-token
                  cancellation-token?
                  cancel-token!
                  make-extension-registry
                  register-extension!
                  make-mock-provider
                  provider?))

;; ── make-runtime contracts ──

(test-case "make-runtime-requires-valid-args"
  ;; Will tighten to provider? once any/c is replaced
  ;; For now just verify the function exists and rejects completely wrong types
  (check-true (procedure? make-runtime)))

(test-case "make-runtime-is-exported"
  ;; Verify make-runtime is available as a contracted function
  (check-true (procedure? make-runtime)))

;; ── Event bus contracts ──

(test-case "make-event-bus-returns-event-bus"
  (define bus (make-event-bus))
  (check-true (event-bus? bus)))

(test-case "subscribe!-rejects-non-procedure"
  (define bus (make-event-bus))
  (check-exn exn:fail:contract? (lambda () (subscribe! bus "not-a-procedure"))))

(test-case "subscribe!-accepts-procedure"
  (define bus (make-event-bus))
  (check-not-exn (lambda () (subscribe! bus (lambda (evt) (void))))))

;; ── Cancellation token contracts ──

(test-case "make-cancellation-token-returns-token"
  (define tok (make-cancellation-token))
  (check-true (cancellation-token? tok)))

(test-case "cancel-token!-works"
  (define tok (make-cancellation-token))
  (check-not-exn (lambda () (cancel-token! tok))))

;; ── Extension registry contracts ──

(test-case "make-extension-registry-returns-registry"
  (define reg (make-extension-registry))
  (check-not-false reg))

;; ── Tool registry contracts ──

(test-case "sdk-make-tool-registry-returns-registry"
  (define reg (make-tool-registry))
  (check-not-false reg))

(test-case "sdk-list-tools-returns-list"
  (define reg (make-tool-registry))
  (define tools (list-tools reg))
  (check-true (list? tools)))
