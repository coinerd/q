#lang racket

;; tests/test-extension-context.rkt — tests for extension-ctx and ctx-aware dispatch
;;
;; Covers:
;;   1. context.rkt — extension-ctx struct and accessor functions
;;   2. context.rkt — make-extension-ctx constructor with keyword args
;;   3. hooks.rkt   — dispatch-hooks with #:ctx passes ctx as first arg to handlers
;;   4. hooks.rkt   — backward compat: handlers that don't accept ctx still work

(require rackunit
         racket/match
         "../extensions/context.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../agent/event-bus.rkt"
         "../util/cancellation.rkt")

;; ============================================================
;; 1. extension-ctx struct — construction and accessors
;; ============================================================

(test-case "make-extension-ctx creates context with all fields"
  (define bus (make-event-bus))
  (define reg (make-extension-registry))
  (define tok (make-cancellation-token))
  (define ctx
    (make-extension-ctx #:session-id "sess-001"
                        #:session-dir "/tmp/sessions/sess-001"
                        #:event-bus bus
                        #:extension-registry reg
                        #:model-name "gpt-4o"
                        #:cancellation-token tok
                        #:working-directory (build-path "/tmp" "project")))
  (check-equal? (ctx-session-id ctx) "sess-001")
  (check-equal? (ctx-session-dir ctx) "/tmp/sessions/sess-001")
  (check-eq? (ctx-event-bus ctx) bus)
  (check-eq? (ctx-extension-registry ctx) reg)
  (check-equal? (ctx-model ctx) "gpt-4o")
  (check-eq? (ctx-signal ctx) tok)
  (check-equal? (ctx-cwd ctx) (build-path "/tmp" "project")))

(test-case "make-extension-ctx allows optional fields as #f"
  (define bus (make-event-bus))
  (define reg (make-extension-registry))
  (define ctx
    (make-extension-ctx #:session-id "sess-002"
                        #:session-dir "/tmp/sessions/sess-002"
                        #:event-bus bus
                        #:extension-registry reg))
  (check-equal? (ctx-session-id ctx) "sess-002")
  (check-false (ctx-model ctx))
  (check-false (ctx-signal ctx))
  (check-false (ctx-cwd ctx)))

(test-case "extension-ctx? predicate works"
  (define ctx
    (make-extension-ctx #:session-id "s1"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-true (extension-ctx? ctx))
  (check-false (extension-ctx? 42))
  (check-false (extension-ctx? "not-a-ctx")))

(test-case "extension-ctx is transparent (equal? works)"
  (define bus (make-event-bus))
  (define reg (make-extension-registry))
  (define ctx1
    (make-extension-ctx #:session-id "s1"
                        #:session-dir "/tmp"
                        #:event-bus bus
                        #:extension-registry reg))
  (define ctx2
    (make-extension-ctx #:session-id "s1"
                        #:session-dir "/tmp"
                        #:event-bus bus
                        #:extension-registry reg))
  (check-equal? ctx1 ctx2))

(test-case "ctx-session-id returns the session ID string"
  (define ctx
    (make-extension-ctx #:session-id "abc-123"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-equal? (ctx-session-id ctx) "abc-123"))

(test-case "ctx-session-dir returns the session directory path"
  (define ctx
    (make-extension-ctx #:session-id "s"
                        #:session-dir (build-path "/tmp" "sessions" "s")
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-equal? (ctx-session-dir ctx) (build-path "/tmp" "sessions" "s")))

(test-case "ctx-model returns model name or #f"
  (define ctx-with-model
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:model-name "claude-3.5-sonnet"))
  (check-equal? (ctx-model ctx-with-model) "claude-3.5-sonnet")
  (define ctx-no-model
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-false (ctx-model ctx-no-model)))

(test-case "ctx-signal returns cancellation token or #f"
  (define tok (make-cancellation-token))
  (define ctx-with-tok
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:cancellation-token tok))
  (check-eq? (ctx-signal ctx-with-tok) tok)
  (check-false (cancellation-token-cancelled? (ctx-signal ctx-with-tok)))
  (define ctx-no-tok
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-false (ctx-signal ctx-no-tok)))

(test-case "ctx-cwd returns working directory or #f"
  (define ctx-with-cwd
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:working-directory (build-path "/tmp" "project")))
  (check-equal? (ctx-cwd ctx-with-cwd) (build-path "/tmp" "project"))
  (define ctx-no-cwd
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-false (ctx-cwd ctx-no-cwd)))

(test-case "ctx-event-bus returns the event bus"
  (define bus (make-event-bus))
  (define ctx
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus bus
                        #:extension-registry (make-extension-registry)))
  (check-eq? (ctx-event-bus ctx) bus))

(test-case "ctx-extension-registry returns the registry"
  (define reg (make-extension-registry))
  (define ctx
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg))
  (check-eq? (ctx-extension-registry ctx) reg))

;; ============================================================
;; 2. dispatch-hooks with ctx — new-style handlers receive (ctx payload)
;; ============================================================

(test-case "dispatch-hooks with #:ctx passes ctx as first arg to handler"
  (define reg (make-extension-registry))
  (define received-ctx (box #f))
  (define received-payload (box #f))
  (define ctx
    (make-extension-ctx #:session-id "sess-ctx-test"
                        #:session-dir "/tmp/s"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg
                        #:model-name "gpt-4o"))
  (register-extension! reg
                       (extension "ctx-aware"
                                  "1"
                                  "1"
                                  (hasheq 'tool-call
                                          (λ (c p)
                                            (set-box! received-ctx c)
                                            (set-box! received-payload p)
                                            (hook-pass p)))))
  (define result (dispatch-hooks 'tool-call "my-payload" reg #:ctx ctx))
  (check-eq? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "my-payload")
  (check-true (extension-ctx? (unbox received-ctx)))
  (check-equal? (ctx-session-id (unbox received-ctx)) "sess-ctx-test")
  (check-equal? (unbox received-payload) "my-payload"))

(test-case "dispatch-hooks with #:ctx and amend uses ctx"
  (define reg (make-extension-registry))
  (define ctx
    (make-extension-ctx #:session-id "amend-sess"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg
                        #:model-name "claude-3"))
  (register-extension!
   reg
   (extension "ctx-amender"
              "1"
              "1"
              (hasheq 'before-send
                      (λ (c p)
                        (hook-amend (string-append p " [model=" (or (ctx-model c) "unknown") "]"))))))
  (define result (dispatch-hooks 'before-send "hello" reg #:ctx ctx))
  (check-eq? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "hello [model=claude-3]"))

(test-case "dispatch-hooks with #:ctx and block handler receives ctx"
  (define reg (make-extension-registry))
  (define ctx
    (make-extension-ctx #:session-id "block-sess"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg))
  (register-extension!
   reg
   (extension "ctx-blocker"
              "1"
              "1"
              (hasheq 'tool-call (λ (c p) (hook-block (format "blocked by ~a" (ctx-session-id c)))))))
  (define result (dispatch-hooks 'tool-call "payload" reg #:ctx ctx))
  (check-eq? (hook-result-action result) 'block)
  (check-equal? (hook-result-payload result) "blocked by block-sess"))

;; ============================================================
;; 3. dispatch-hooks backward compat — old-style (payload) still works
;; ============================================================

(test-case "dispatch-hooks with #:ctx: 1-arg handler still works (backward compat)"
  (define reg (make-extension-registry))
  (define ctx
    (make-extension-ctx #:session-id "compat-sess"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg))
  (register-extension! reg
                       (extension "old-style"
                                  "1"
                                  "1"
                                  (hasheq 'tool-call
                                          (λ (p) (hook-amend (string-append p " [old]"))))))
  (define result (dispatch-hooks 'tool-call "payload" reg #:ctx ctx))
  (check-eq? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "payload [old]"))

(test-case "dispatch-hooks without #:ctx: 1-arg handler works as before"
  (define reg (make-extension-registry))
  (register-extension! reg
                       (extension "no-ctx"
                                  "1"
                                  "1"
                                  (hasheq 'tool-call
                                          (λ (p) (hook-amend (string-append p " [no-ctx]"))))))
  (define result (dispatch-hooks 'tool-call "payload" reg))
  (check-eq? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "payload [no-ctx]"))

(test-case "dispatch-hooks with #:ctx: mixed 1-arg and 2-arg handlers"
  (define reg (make-extension-registry))
  (define ctx
    (make-extension-ctx #:session-id "mixed-sess"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg
                        #:model-name "test-model"))
  ;; Old-style handler (1 arg)
  (register-extension!
   reg
   (extension "old-ext" "1" "1" (hasheq 'tool-call (λ (p) (hook-amend (string-append p " +old"))))))
  ;; New-style handler (2 args: ctx, payload)
  (register-extension!
   reg
   (extension "new-ext"
              "1"
              "1"
              (hasheq 'tool-call
                      (λ (c p) (hook-amend (string-append p " +new[model=" (ctx-model c) "]"))))))
  (define result (dispatch-hooks 'tool-call "start" reg #:ctx ctx))
  (check-eq? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "start +old +new[model=test-model]"))

(test-case "dispatch-hooks with #:ctx: exception in 2-arg handler is isolated"
  (define reg (make-extension-registry))
  (define ctx
    (make-extension-ctx #:session-id "err-sess"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg))
  (register-extension!
   reg
   (extension "crasher" "1" "1" (hasheq 'tool-call (λ (c p) (error "boom in ctx handler")))))
  (parameterize ([current-hook-timeout-ms #f])
    (define result (dispatch-hooks 'tool-call "payload" reg #:ctx ctx))
    (check-eq? (hook-result-action result) 'block)
    (check-equal? (hook-result-payload result) "handler crasher failed for critical hook tool-call")))

(test-case "dispatch-hooks with #:ctx: cancelled token accessible via ctx-signal"
  (define reg (make-extension-registry))
  (define tok (make-cancellation-token))
  (cancel-token! tok)
  (define ctx
    (make-extension-ctx #:session-id "cancelled-sess"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry reg
                        #:cancellation-token tok))
  (define checked-cancelled (box #f))
  (register-extension! reg
                       (extension "cancel-check"
                                  "1"
                                  "1"
                                  (hasheq 'tool-call
                                          (λ (c p)
                                            (set-box! checked-cancelled
                                                      (cancellation-token-cancelled? (ctx-signal c)))
                                            (hook-pass p)))))
  (parameterize ([current-hook-timeout-ms #f])
    (dispatch-hooks 'tool-call "payload" reg #:ctx ctx)
    (check-true (unbox checked-cancelled))))
