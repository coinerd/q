#lang racket

;; tests/test-agent-session-hooks.rkt — hook dispatch, blocking, error isolation
;;
;; Split from test-agent-session.rkt (T3-01, v0.16.1).

;; tests/test-agent-session.rkt — tests for runtime/agent-session.rkt
;;
;; Covers:
;;   1. Session creation
;;   2. Text-only prompt
;;   3. History accumulation across multiple prompts
;;   4. Session resume
;;   5. Session fork (full + partial)
;;   6. Tool-call execution loop
;;   7. Max-iteration guard
;;   8. Event emission order
;;   9. Session active/close
;;  10. Message struct input to run-prompt!
;;  11. Provider exception handling (BUG-34)

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../util/protocol-types.rkt"
                  message
                  message?
                  message-id
                  message-role
                  message-content
                  message-parent-id
                  message-kind
                  make-message
                  make-text-part
                  make-tool-result-part
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-result-part?
                  tool-result-part-content
                  tool-result-part-is-error?
                  make-loop-result
                  loop-result?
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata
                  event
                  event-event
                  event-payload
                  event-ev
                  make-event
                  content-part->jsexpr)
         "../agent/event-bus.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool make-tool-registry register-tool! make-success-result)
         (only-in "../extensions/api.rkt"
                  extension-registry?
                  make-extension-registry
                  register-extension!
                  extension)
         (only-in "../extensions/hooks.rkt" hook-pass hook-amend hook-block)
         "../runtime/agent-session.rkt"
         (only-in "../runtime/session-store.rkt" append-entry! load-session-log)
         (only-in "../runtime/compactor.rkt"
                  compaction-strategy
                  compaction-result->message-list
                  compact-history
                  build-tiered-context
                  tiered-context
                  tiered-context?
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c
                  tiered-context->message-list)
         (only-in "../runtime/token-compaction.rkt" token-compaction-config)
         (only-in "helpers/mock-provider.rkt"
                  make-multi-mock-provider
                  make-test-config
                  make-simple-mock-provider
                  make-tool-call-mock-provider)
         (only-in "../util/cancellation.rkt"
                  make-cancellation-token
                  cancellation-token?
                  cancellation-token-cancelled?
                  cancel-token!))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-agent-session-test-~a" 'directory))

(define (make-event-collector bus)
  (define collected (box '()))
  (subscribe! bus (λ (evt) (set-box! collected (append (unbox collected) (list evt)))))
  collected)

(define (event-names collected-box)
  (map event-event (unbox collected-box)))

(define (text-of msg)
  (text-part-text (first (message-content msg))))

;; Helper: parse tool-call arguments (streaming produces JSON strings,
;; but test tools expect Racket hashes)
(define (parse-tool-args args)
  (cond
    [(hash? args) args]
    [(string? args)
     (with-handlers ([exn:fail? (lambda (_) (hash))])
       (define cleaned (string-trim args))
       (if (or (string=? cleaned "") (string=? cleaned "{}"))
           (hash)
           (for/hash ([m (in-list (regexp-match* #rx"\"([^\"]+)\"\\s*:\\s*\"([^\"]*)\""
                                                 cleaned
                                                 #:match-select values))])
             (values (string->symbol (cadr m)) (caddr m)))))]
    [else (hash)]))

;; ============================================================
;; Test suite
;; ============================================================

(define-test-suite
 test-agent-session-hooks-suite
 (test-case "run-prompt! dispatches 'turn-start hook when extension-registry present"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define hook-called? (box #f))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
   (define ext-reg (make-extension-registry))
   (register-extension! ext-reg
                        (extension "test-ext"
                                   "1.0"
                                   "0.1"
                                   (hasheq 'turn-start
                                           (lambda (ctx)
                                             (set-box! hook-called? #t)
                                             (hook-pass ctx)))))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'extension-registry ext-reg)))
   (define-values (s result) (run-prompt! sess "hello"))
   (check-true (unbox hook-called?) "turn-start hook should have been called")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "run-prompt! dispatches 'turn-end hook on completed turn"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define turn-end-called? (box #f))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "done")) (hash) "mock" 'stop)))
   (define ext-reg (make-extension-registry))
   (register-extension! ext-reg
                        (extension "test-ext"
                                   "1.0"
                                   "0.1"
                                   (hasheq 'turn-end
                                           (lambda (result)
                                             (set-box! turn-end-called? #t)
                                             (hook-pass result)))))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'extension-registry ext-reg)))
   (define-values (s result) (run-prompt! sess "hello"))
   (check-true (unbox turn-end-called?) "turn-end hook should have been called")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "run-prompt! skips hooks when extension-registry is #f"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "ok")) (hash) "mock" 'stop)))
   ;; No extension-registry — default is #f
   (define sess (make-agent-session (make-test-config dir bus prov)))
   ;; Should complete without error even though no registry exists
   (define-values (s result) (run-prompt! sess "hello"))
   (check-pred loop-result? result)
   (check-equal? (loop-result-termination-reason result) 'completed)
   (delete-directory/files dir #:must-exist? #f))
 (test-case "tool-result hook amending with non-message values are filtered"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define reg (make-tool-registry))
   (register-tool!
    reg
    (make-tool "echo"
               "Echoes input"
               (hasheq 'type "object" 'properties (hasheq 'msg (hasheq 'type "string")) 'required '())
               (lambda (args ctx) (make-success-result (list (hasheq 'type "text" 'text "echoed"))))))
   (define prov
     (make-multi-mock-provider
      (list (make-model-response
             (list (hash 'type "tool-call" 'id "tc-1" 'name "echo" 'arguments (hash 'msg "hi")))
             (hash)
             "mock"
             'tool-calls)
            (make-model-response (list (hash 'type "text" 'text "Done")) (hash) "mock" 'stop))))
   (define ext-reg (make-extension-registry))
   (register-extension! ext-reg
                        (extension "dirty-ext"
                                   "1.0"
                                   "0.1"
                                   (hasheq 'tool-result
                                           (lambda (msgs)
                                             ;; Amend with a mix of valid messages and junk
                                             (hook-amend (append msgs (list "junk-string" 42)))))))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov reg) 'extension-registry ext-reg)))
   (define-values (s result) (run-prompt! sess "Echo hi"))
   (check-equal? (loop-result-termination-reason result) 'completed)
   ;; History: user, assistant(tool-call), tool-result, assistant(text)
   ;; The junk values should be filtered out — still 4 entries
   (define hist (session-history s))
   (check-equal? (length hist) 4)
   (check-equal? (message-role (list-ref hist 0)) 'user)
   (check-equal? (message-role (list-ref hist 1)) 'assistant)
   (check-equal? (message-role (list-ref hist 2)) 'tool)
   (check-equal? (message-role (list-ref hist 3)) 'assistant)
   (delete-directory/files dir #:must-exist? #f))
 (test-case "turn-start hook 'block skips the turn"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "should not appear")) (hash) "mock" 'stop)))
   (define ext-reg (make-extension-registry))
   (register-extension! ext-reg
                        (extension "block-ext"
                                   "1.0"
                                   "0.1"
                                   (hasheq 'turn-start
                                           (lambda (ctx) (hook-block "blocked for testing")))))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'extension-registry ext-reg)))
   (define-values (s result) (run-prompt! sess "hello"))
   (check-equal? (loop-result-termination-reason result) 'completed)
   (check-equal? (hash-ref (loop-result-metadata result) 'reason) "extension-block")
   ;; History should only have user message (no model call)
   (define hist (session-history s))
   (check-equal? (length hist) 1)
   (check-equal? (message-role (first hist)) 'user)
   ;; turn.blocked event emitted
   (check-not-false (member "turn.blocked" (event-names evts)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "hooks fire on 'turn-start and 'tool-call (BLUEPRINT names)"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define turn-start-called? (box #f))
   (define tool-call-called? (box #f))
   (define reg (make-tool-registry))
   (register-tool!
    reg
    (make-tool "echo"
               "Echoes input"
               (hasheq 'type "object" 'properties (hasheq 'msg (hasheq 'type "string")) 'required '())
               (lambda (args ctx) (make-success-result (list (hasheq 'type "text" 'text "echoed"))))))
   (define prov
     (make-multi-mock-provider
      (list (make-model-response
             (list (hash 'type "tool-call" 'id "tc-1" 'name "echo" 'arguments (hash 'msg "hi")))
             (hash)
             "mock"
             'tool-calls)
            (make-model-response (list (hash 'type "text" 'text "Done")) (hash) "mock" 'stop))))
   (define ext-reg (make-extension-registry))
   (register-extension! ext-reg
                        (extension "blueprint-names-ext"
                                   "1.0"
                                   "0.1"
                                   (hasheq 'turn-start
                                           (lambda (ctx)
                                             (set-box! turn-start-called? #t)
                                             (hook-pass ctx))
                                           'tool-call
                                           (lambda (tcs)
                                             (set-box! tool-call-called? #t)
                                             (hook-pass tcs)))))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov reg) 'extension-registry ext-reg)))
   (define-values (s result) (run-prompt! sess "Echo hi"))
   (check-true (unbox turn-start-called?) "'turn-start hook should have been called")
   (check-true (unbox tool-call-called?) "'tool-call hook should have been called")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "hook exception during dispatch is isolated and session continues"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
   (define ext-reg (make-extension-registry))
   (register-extension!
    ext-reg
    (extension "crashing-ext"
               "1.0"
               "0.1"
               (hasheq 'turn-start (lambda (ctx) (error "crashing-hook" "intentional test error")))))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'extension-registry ext-reg)))
   ;; The crashing handler should be isolated — session should NOT raise
   (check-not-exn (lambda () (run-prompt! sess "hello"))
                  "exception in hook should be isolated, not propagate")
   (delete-directory/files dir #:must-exist? #f)))

(run-tests test-agent-session-hooks-suite)
