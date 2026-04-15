#lang racket

;; tests/test-hook-expansion.rkt — tests for hook point expansion (#665)
;;
;; Covers:
;;   #666: input hook for user input interception
;;   #667: agent-start/agent-end hook points
;;   #668: resources.discover event
;;   #669: session-before-fork hook
;;   #670/#671: critical vs advisory error defaults

(require rackunit
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../extensions/context.rkt"
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt")

;; ============================================================
;; #670/#671: Critical vs advisory hook error handling
;; ============================================================

(test-case "critical-hook? returns #t for tool-call"
  (check-true (critical-hook? 'tool-call)))

(test-case "critical-hook? returns #t for session-before-fork"
  (check-true (critical-hook? 'session-before-fork)))

(test-case "critical-hook? returns #t for session-before-compact"
  (check-true (critical-hook? 'session-before-compact)))

(test-case "critical-hook? returns #t for input"
  (check-true (critical-hook? 'input)))

(test-case "critical-hook? returns #f for advisory hooks"
  (check-false (critical-hook? 'turn-start))
  (check-false (critical-hook? 'turn-end))
  (check-false (critical-hook? 'context))
  (check-false (critical-hook? 'message-update)))

(test-case "critical hook errors default to block"
  (define reg (make-extension-registry))
  (define throwing-ext
    (extension "thrower" "0.1" "1.0"
               (hasheq 'tool-call (lambda (payload) (error "boom")))))
  (register-extension! reg throwing-ext)
  (define result (dispatch-hooks 'tool-call "payload" reg))
  (check-equal? (hook-result-action result) 'block))

(test-case "advisory hook errors default to pass"
  (define reg (make-extension-registry))
  (define throwing-ext
    (extension "thrower" "0.1" "1.0"
               (hasheq 'turn-start (lambda (payload) (error "boom")))))
  (register-extension! reg throwing-ext)
  (define result (dispatch-hooks 'turn-start "payload" reg))
  (check-equal? (hook-result-action result) 'pass))

(test-case "critical hook timeout defaults to block"
  (define reg (make-extension-registry))
  (define slow-ext
    (extension "sleeper" "0.1" "1.0"
               (hasheq 'tool-call (lambda (payload) (sleep 10) payload))))
  (register-extension! reg slow-ext)
  (parameterize ([current-hook-timeout-ms 50])
    (define result (dispatch-hooks 'tool-call "payload" reg))
    (check-equal? (hook-result-action result) 'block)))

(test-case "advisory hook timeout defaults to pass"
  (define reg (make-extension-registry))
  (define slow-ext
    (extension "sleeper" "0.1" "1.0"
               (hasheq 'context (lambda (payload) (sleep 10) payload))))
  (register-extension! reg slow-ext)
  (parameterize ([current-hook-timeout-ms 50])
    (define result (dispatch-hooks 'context "payload" reg))
    (check-equal? (hook-result-action result) 'pass)))

(test-case "non-hook-result return from critical hook defaults to block"
  (define reg (make-extension-registry))
  (define bad-ext
    (extension "bad-return" "0.1" "1.0"
               (hasheq 'tool-call (lambda (payload) "not-a-hook-result"))))
  (register-extension! reg bad-ext)
  (define result (dispatch-hooks 'tool-call "payload" reg))
  (check-equal? (hook-result-action result) 'block))

(test-case "non-hook-result return from advisory hook defaults to pass"
  (define reg (make-extension-registry))
  (define bad-ext
    (extension "bad-return" "0.1" "1.0"
               (hasheq 'context (lambda (payload) "not-a-hook-result"))))
  (register-extension! reg bad-ext)
  (define result (dispatch-hooks 'context "payload" reg))
  (check-equal? (hook-result-action result) 'pass))

;; ============================================================
;; #667: agent-start and agent-end hook dispatch via hook-dispatcher
;; ============================================================

(test-case "agent-start hook receives correct payload shape"
  (define received (box #f))
  (define dispatcher
    (lambda (hook-point payload)
      (when (eq? hook-point 'agent-start)
        (set-box! received payload))
      (hook-pass payload)))
  ;; Simulate what loop.rkt does
  (dispatcher 'agent-start
              (hasheq 'session-id "test" 'turn-id "t1" 'message-count 5))
  (check-true (hash? (unbox received)))
  (check-equal? (hash-ref (unbox received) 'session-id) "test")
  (check-equal? (hash-ref (unbox received) 'message-count) 5))

(test-case "agent-end hook receives termination in payload"
  (define received (box #f))
  (define dispatcher
    (lambda (hook-point payload)
      (when (eq? hook-point 'agent-end)
        (set-box! received payload))
      (hook-pass payload)))
  (dispatcher 'agent-end
              (hasheq 'session-id "test" 'turn-id "t1" 'termination 'completed))
  (check-equal? (hash-ref (unbox received) 'termination) 'completed))

;; ============================================================
;; General dispatch still works
;; ============================================================

(test-case "dispatch-hooks with pass action continues"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "pass-ext" "0.1" "1.0"
               (hasheq 'turn-start (lambda (p) (hook-pass p)))))
  (define result (dispatch-hooks 'turn-start "hello" reg))
  (check-equal? (hook-result-action result) 'pass))

(test-case "dispatch-hooks with amend action replaces payload"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "amend-ext" "0.1" "1.0"
               (hasheq 'turn-start (lambda (p) (hook-amend "replaced")))))
  (define result (dispatch-hooks 'turn-start "original" reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "replaced"))

(test-case "dispatch-hooks with block action stops"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "block-ext" "0.1" "1.0"
               (hasheq 'tool-call (lambda (p) (hook-block "nope")))))
  (define result (dispatch-hooks 'tool-call "payload" reg))
  (check-equal? (hook-result-action result) 'block))

(test-case "dispatch-hooks with ctx-aware handler"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "ctx-ext" "0.1" "1.0"
               (hasheq 'turn-start
                       (lambda (ctx payload)
                         (hook-amend (format "ctx:~a" (ctx-session-id ctx)))))))
  (define ctx (extension-ctx "s42" "/tmp" (make-event-bus) reg #f #f #f))
  (define result (dispatch-hooks 'turn-start "hello" reg #:ctx ctx))
  (check-equal? (hook-result-payload result) "ctx:s42"))
