#lang racket

;; tests/test-agent-session-extensions.rkt — extension-registry and model-name
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
 test-agent-session-extensions-suite
 (test-case "extension-registry and model-name default to #f"
   (let ([tmpdir (make-temporary-file "q-session-~a" 'directory)]
         [bus (make-event-bus)]
         [reg (make-tool-registry)]
         [prov (make-mock-provider
                (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop))])
     (define sess
       (make-agent-session
        (hasheq 'provider prov 'tool-registry reg 'event-bus bus 'session-dir (path->string tmpdir))))
     (check-false (agent-session-extension-registry sess)
                  "agent-session: extension-registry defaults to #f")
     (check-false (agent-session-model-name sess) "agent-session: model-name defaults to #f")))
 (test-case "extension-registry is stored when provided"
   (let ([tmpdir (make-temporary-file "q-session-~a" 'directory)]
         [bus (make-event-bus)]
         [reg (make-tool-registry)]
         [prov (make-mock-provider
                (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop))]
         [ext-reg (make-extension-registry)])
     (define sess
       (make-agent-session (hasheq 'provider
                                   prov
                                   'tool-registry
                                   reg
                                   'event-bus
                                   bus
                                   'session-dir
                                   (path->string tmpdir)
                                   'extension-registry
                                   ext-reg)))
     (check-pred extension-registry?
                 (agent-session-extension-registry sess)
                 "agent-session: extension-registry stored when provided")))
 (test-case "model-name is stored when provided"
   (let ([tmpdir (make-temporary-file "q-session-~a" 'directory)]
         [bus (make-event-bus)]
         [reg (make-tool-registry)]
         [prov (make-mock-provider
                (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop))])
     (define sess
       (make-agent-session (hasheq 'provider
                                   prov
                                   'tool-registry
                                   reg
                                   'event-bus
                                   bus
                                   'session-dir
                                   (path->string tmpdir)
                                   'model-name
                                   "gpt-4o")))
     (check-equal? (agent-session-model-name sess)
                   "gpt-4o"
                   "agent-session: model-name stored when provided")))
 (test-case "Both extension-registry and model-name together"
   (let ([tmpdir (make-temporary-file "q-session-~a" 'directory)]
         [bus (make-event-bus)]
         [reg (make-tool-registry)]
         [prov (make-mock-provider
                (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop))]
         [ext-reg (make-extension-registry)])
     (define sess
       (make-agent-session (hasheq 'provider
                                   prov
                                   'tool-registry
                                   reg
                                   'event-bus
                                   bus
                                   'session-dir
                                   (path->string tmpdir)
                                   'extension-registry
                                   ext-reg
                                   'model-name
                                   "claude-3")))
     (check-true (extension-registry? (agent-session-extension-registry sess)))
     (check-equal? (agent-session-model-name sess) "claude-3")))
 (test-case "resume-agent-session preserves extension-registry and model-name"
   (let ([tmpdir (make-temporary-file "q-session-~a" 'directory)]
         [bus (make-event-bus)]
         [reg (make-tool-registry)]
         [prov (make-mock-provider
                (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop))]
         [ext-reg (make-extension-registry)])
     ;; Create a session first
     (define sess
       (make-agent-session (hasheq 'provider
                                   prov
                                   'tool-registry
                                   reg
                                   'event-bus
                                   bus
                                   'session-dir
                                   (path->string tmpdir)
                                   'extension-registry
                                   ext-reg
                                   'model-name
                                   "test-model")))
     ;; #771: ensure-persisted! now creates directory and version header eagerly
     (define sid (session-id sess))
     ;; Resume it
     (define resumed
       (resume-agent-session sid
                             (hasheq 'provider
                                     prov
                                     'tool-registry
                                     reg
                                     'event-bus
                                     bus
                                     'session-dir
                                     (path->string tmpdir)
                                     'extension-registry
                                     ext-reg
                                     'model-name
                                     "test-model")))
     (check-true (extension-registry? (agent-session-extension-registry resumed)))
     (check-equal? (agent-session-model-name resumed) "test-model")))
 (test-case "resume-agent-session preserves system-instructions"
   (let ([tmpdir (make-temporary-file "q-session-~a" 'directory)]
         [bus (make-event-bus)]
         [reg (make-tool-registry)]
         [prov (make-mock-provider
                (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop))]
         [instrs '("System instruction A")])
     (define sess
       (make-agent-session (hasheq 'provider
                                   prov
                                   'tool-registry
                                   reg
                                   'event-bus
                                   bus
                                   'session-dir
                                   (path->string tmpdir)
                                   'system-instructions
                                   instrs)))
     ;; #771: ensure-persisted! now creates directory and version header eagerly
     (define sid (session-id sess))
     ;; Resume it
     (define resumed
       (resume-agent-session sid
                             (hasheq 'provider
                                     prov
                                     'tool-registry
                                     reg
                                     'event-bus
                                     bus
                                     'session-dir
                                     (path->string tmpdir)
                                     'system-instructions
                                     instrs)))
     (check-equal? (agent-session-system-instructions resumed)
                   instrs
                   "resume-agent-session preserves system-instructions")))
 (test-case "fork-session copies extension-registry and model-name"
   (let ([tmpdir (make-temporary-file "q-session-~a" 'directory)]
         [bus (make-event-bus)]
         [reg (make-tool-registry)]
         [prov (make-mock-provider
                (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop))]
         [ext-reg (make-extension-registry)])
     (define sess
       (make-agent-session (hasheq 'provider
                                   prov
                                   'tool-registry
                                   reg
                                   'event-bus
                                   bus
                                   'session-dir
                                   (path->string tmpdir)
                                   'extension-registry
                                   ext-reg
                                   'model-name
                                   "fork-model")))
     (define forked (fork-session sess))
     (check-true (extension-registry? (agent-session-extension-registry forked)))
     (check-equal? (agent-session-model-name forked) "fork-model"))))

(run-tests test-agent-session-extensions-suite)
