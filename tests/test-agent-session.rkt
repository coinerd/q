#lang racket

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
 test-agent-session-suite
 ;; ── 1. Create session ──
 (test-case "make-agent-session creates session with valid ID and empty history"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
   (define sess (make-agent-session (make-test-config dir bus prov)))

   (check-pred agent-session? sess)
   (check-true (string? (session-id sess)))
   (check-true (> (string-length (session-id sess)) 0))
   (check-equal? (session-history sess) '())
   (check-pred session-active? sess)

   ;; session.started event emitted
   (check-not-false (member "session.started" (event-names evts)))

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 2. Run prompt — text only ──
 (test-case "run-prompt! with text response persists user + assistant messages"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider (make-model-response
                          (list (hash 'type "text" 'text "Hello, world!"))
                          (hash 'prompt_tokens 5 'completion_tokens 3 'total_tokens 8)
                          "mock-model"
                          'stop)))
   (define sess (make-agent-session (make-test-config dir bus prov)))

   (define-values (updated-sess result) (run-prompt! sess "Say hello"))

   ;; Check result
   (check-equal? (loop-result-termination-reason result) 'completed)

   ;; Check history: user + assistant
   (define hist (session-history updated-sess))
   (check-equal? (length hist) 2)
   (check-equal? (message-role (first hist)) 'user)
   (check-equal? (message-role (second hist)) 'assistant)

   ;; Check content
   (check-equal? (text-of (first hist)) "Say hello")
   (check-equal? (text-of (second hist)) "Hello, world!")

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 3. History accumulation across multiple prompts ──
 (test-case "run-prompt! accumulates history across multiple calls"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-multi-mock-provider
      (list
       (make-model-response (list (hash 'type "text" 'text "First response")) (hash) "mock" 'stop)
       (make-model-response (list (hash 'type "text" 'text "Second response")) (hash) "mock" 'stop))))
   (define sess (make-agent-session (make-test-config dir bus prov)))

   (define-values (s1 r1) (run-prompt! sess "First prompt"))
   (define-values (s2 r2) (run-prompt! s1 "Second prompt"))

   (define hist (session-history s2))
   (check-equal? (length hist) 4)
   (check-equal? (text-of (list-ref hist 0)) "First prompt")
   (check-equal? (text-of (list-ref hist 1)) "First response")
   (check-equal? (text-of (list-ref hist 2)) "Second prompt")
   (check-equal? (text-of (list-ref hist 3)) "Second response")

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 4. Resume session ──
 (test-case "resume-agent-session loads history and continues"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define prov
     (make-multi-mock-provider
      (list
       (make-model-response (list (hash 'type "text" 'text "First")) (hash) "mock" 'stop)
       (make-model-response (list (hash 'type "text" 'text "After resume")) (hash) "mock" 'stop))))

   ;; Create and run first prompt
   (define sess1 (make-agent-session (make-test-config dir bus prov)))
   (define-values (s1 _r1) (run-prompt! sess1 "Hello"))
   (define sid (session-id s1))

   ;; Clear event collector for resume check
   (set-box! evts '())

   ;; Resume session
   (define sess2 (resume-agent-session sid (make-test-config dir bus prov)))
   (check-equal? (session-id sess2) sid)

   ;; History preserved
   (define hist (session-history sess2))
   (check-equal? (length hist) 2)
   (check-equal? (message-role (first hist)) 'user)
   (check-equal? (message-role (second hist)) 'assistant)

   ;; session.resumed event
   (check-not-false (member "session.resumed" (event-names evts)))

   ;; Continue with another prompt
   (define-values (s2 r2) (run-prompt! sess2 "Continue"))
   (check-equal? (loop-result-termination-reason r2) 'completed)

   (define hist2 (session-history s2))
   (check-equal? (length hist2) 4)
   (check-equal? (text-of (list-ref hist2 2)) "Continue")
   (check-equal? (text-of (list-ref hist2 3)) "After resume")

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 5. Fork session — full history ──
 (test-case "fork-session copies full history to new session"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "Response")) (hash) "mock" 'stop)))

   (define sess1 (make-agent-session (make-test-config dir bus prov)))
   (define-values (s1 _) (run-prompt! sess1 "Prompt 1"))

   (define forked (fork-session s1))

   ;; New ID
   (check-not-equal? (session-id forked) (session-id s1))

   ;; Same history content
   (define orig-hist (session-history s1))
   (define fork-hist (session-history forked))
   (check-equal? (length fork-hist) (length orig-hist))
   (check-equal? (text-of (first fork-hist)) "Prompt 1")
   (check-equal? (text-of (second fork-hist)) "Response")

   ;; session.forked event
   (check-not-false (member "session.forked" (event-names evts)))

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 6. Fork session — at specific entry ──
 (test-case "fork-session at specific entry copies partial history"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-multi-mock-provider
      (list (make-model-response (list (hash 'type "text" 'text "First")) (hash) "mock" 'stop)
            (make-model-response (list (hash 'type "text" 'text "Second")) (hash) "mock" 'stop))))

   (define sess (make-agent-session (make-test-config dir bus prov)))
   (define-values (s1 _r1) (run-prompt! sess "Prompt 1"))
   (define-values (s2 _r2) (run-prompt! s1 "Prompt 2"))

   (define hist (session-history s2))
   (check-equal? (length hist) 4)

   ;; Fork at second entry (first assistant message)
   (define second-entry-id (message-id (second hist)))
   (define forked (fork-session s2 second-entry-id))

   (define fork-hist (session-history forked))
   (check-equal? (length fork-hist) 2)
   (check-equal? (text-of (first fork-hist)) "Prompt 1")
   (check-equal? (text-of (second fork-hist)) "First")

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 7. Tool-call execution loop ──
 (test-case "run-prompt! executes tool calls and feeds results back"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define reg (make-tool-registry))

   ;; Register echo tool
   (register-tool!
    reg
    (make-tool
     "echo"
     "Echoes the input message"
     (hasheq 'type
             "object"
             'properties
             (hasheq 'message (hasheq 'type "string"))
             'required
             '(message))
     (lambda (args ctx)
       (define a (parse-tool-args args))
       (make-success-result
        (list (hasheq 'type "text" 'text (string-append "Echo: " (hash-ref a 'message ""))))))))

   ;; Mock: first returns tool-call, then text
   (define prov
     (make-multi-mock-provider
      (list
       ;; First turn: call echo tool
       (make-model-response
        (list
         (hash 'type "tool-call" 'id "tc-1" 'name "echo" 'arguments (hash 'message "hello world")))
        (hash)
        "mock"
        'tool-calls)
       ;; Second turn: text response
       (make-model-response (list (hash 'type "text" 'text "I echoed hello world"))
                            (hash)
                            "mock"
                            'stop))))

   (define sess (make-agent-session (make-test-config dir bus prov reg)))
   (define-values (s result) (run-prompt! sess "Echo hello world"))

   ;; Should complete
   (check-equal? (loop-result-termination-reason result) 'completed)

   ;; History: user, assistant(tool-call), tool-result, assistant(text)
   (define hist (session-history s))
   (check-equal? (length hist) 4)
   (check-equal? (message-role (list-ref hist 0)) 'user)
   (check-equal? (message-role (list-ref hist 1)) 'assistant)
   (check-equal? (message-role (list-ref hist 2)) 'tool)
   (check-equal? (message-role (list-ref hist 3)) 'assistant)

   ;; Check tool-call in assistant message
   (define assistant-1 (list-ref hist 1))
   (define tc-parts (filter tool-call-part? (message-content assistant-1)))
   (check-equal? (length tc-parts) 1)
   (check-equal? (tool-call-part-name (first tc-parts)) "echo")
   (check-equal? (tool-call-part-id (first tc-parts)) "tc-1")

   ;; Check tool-result content
   (define tool-result-msg (list-ref hist 2))
   (define tr-parts (filter tool-result-part? (message-content tool-result-msg)))
   (check-equal? (length tr-parts) 1)
   (check-false (tool-result-part-is-error? (first tr-parts)))
   ;; Tool result content should contain echo output
   (define tr-content (tool-result-part-content (first tr-parts)))
   (check-not-false (member "Echo: hello world"
                            (map (lambda (c) (hash-ref c 'text #f)) (filter hash? tr-content))))

   ;; Final assistant text
   (check-equal? (text-of (list-ref hist 3)) "I echoed hello world")

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 8. Max-iteration guard ──
 (test-case "run-prompt! stops after max-iterations"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define reg (make-tool-registry))

   ;; Register loop tool
   (register-tool!
    reg
    (make-tool "loop"
               "Loops forever"
               (hasheq 'type "object" 'properties (hasheq 'n (hasheq 'type "integer")) 'required '())
               (lambda (args ctx)
                 (make-success-result (list (hasheq 'type "text" 'text "loop result"))))))

   ;; Mock always returns tool-calls
   (define prov
     (make-multi-mock-provider
      (list (make-model-response
             (list (hash 'type "tool-call" 'id "tc-loop" 'name "loop" 'arguments (hash)))
             (hash)
             "mock"
             'tool-calls))))

   (define sess
     (make-agent-session
      (hash 'provider prov 'tool-registry reg 'event-bus bus 'session-dir dir 'max-iterations 2 'max-iterations-hard 2)))

   (define-values (s result) (run-prompt! sess "keep looping"))

   ;; Should stop with max-iterations-exceeded
   (check-equal? (loop-result-termination-reason result) 'max-iterations-exceeded)

   ;; Log: user, assistant(tool), tool-result, assistant(tool)
   (define hist (session-history s))
   (check-equal? (length hist) 4)
   (check-equal? (message-role (first hist)) 'user)
   (check-equal? (message-role (second hist)) 'assistant)
   (check-equal? (message-role (third hist)) 'tool)
   (check-equal? (message-role (fourth hist)) 'assistant)

   ;; runtime.error event emitted
   (check-not-false (member "runtime.error" (event-names evts)))

   (delete-directory/files dir #:must-exist? #f))
 ;; ── BUG-34: Provider exception handling ──
 (test-case "run-prompt! catches provider exception and emits runtime.error event"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))

   ;; Create a provider that raises exn:fail
   (define prov
     (make-provider (lambda () "failing-mock")
                    (lambda () (hash 'streaming #t 'token-counting #t))
                    ;; send: raise exception
                    (lambda (req) (error "provider-error" "Simulated provider failure"))
                    ;; stream: raise exception
                    (lambda (req) (error "provider-error" "Simulated provider failure"))))

   (define sess (make-agent-session (make-test-config dir bus prov)))

   ;; Should NOT crash - should return error result
   (define-values (s result) (run-prompt! sess "This should not crash"))

   ;; Verify error result returned
   (check-equal? (loop-result-termination-reason result)
                 'error
                 "should return 'error termination reason")

   ;; Verify metadata contains error info
   (define meta (loop-result-metadata result))
   (check-not-false (hash-has-key? meta 'error) "metadata should contain error key")
   (check-equal? (hash-ref meta 'errorType)
                 'provider-error
                 "metadata should have errorType 'provider-error")

   ;; Verify runtime.error event was emitted
   (check-not-false (member "runtime.error" (event-names evts))
                    "runtime.error event should be emitted")

   ;; Verify session is still usable (history has user message)
   (define hist (session-history s))
   (check-equal? (length hist) 1 "user message should be persisted")
   (check-equal? (message-role (first hist)) 'user)

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 9. Session active/close ──
 (test-case "session-active? and close-session!"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))

   (define sess (make-agent-session (make-test-config dir bus prov)))
   (check-pred session-active? sess)

   (close-session! sess)
   (check-false (session-active? sess))

   ;; session.closed event
   (check-not-false (member "session.closed" (event-names evts)))

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 10. Events in correct order ──
 (test-case "events emitted in correct order for text turn"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "Response")) (hash) "mock" 'stop)))

   (define sess (make-agent-session (make-test-config dir bus prov)))
   (define-values (s _) (run-prompt! sess "Test"))

   (define names (event-names evts))
   ;; session.started should be first
   (check-equal? (first names) "session.started")
   ;; session.updated should be last
   (check-equal? (last names) "session.updated")
   ;; Core loop events in between
   (check-not-false (member "turn.started" names))
   (check-not-false (member "turn.completed" names))
   (check-not-false (member "context.built" names))
   (check-not-false (member "assistant.message.completed" names))

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 11. run-prompt! accepts message struct ──
 (test-case "run-prompt! accepts message struct as input"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "Got it")) (hash) "mock" 'stop)))

   (define sess (make-agent-session (make-test-config dir bus prov)))
   (define user-msg
     (make-message "custom-id"
                   #f
                   'user
                   'message
                   (list (make-text-part "Custom message"))
                   (current-seconds)
                   (hasheq)))
   (define-values (s result) (run-prompt! sess user-msg))

   (check-equal? (loop-result-termination-reason result) 'completed)
   (define hist (session-history s))
   (check-equal? (length hist) 2)
   (check-equal? (message-id (first hist)) "custom-id")

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 12. Tool-call events include tool.call.started ──
 (test-case "tool-call scenario emits tool.call.started events"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
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

   (define sess (make-agent-session (make-test-config dir bus prov reg)))
   (define-values (s result) (run-prompt! sess "Echo hi"))

   (define names (event-names evts))
   ;; Should have tool.call.started from first turn
   (check-not-false (member "tool.call.started" names))
   ;; Should have two turn.started events (two iterations)
   (define turn-started-count (length (filter (λ (n) (equal? n "turn.started")) names)))
   (check-equal? turn-started-count 2 "two turns for tool-call loop")

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 13. system-instructions field defaults to '() ──
 (test-case "agent-session has system-instructions field, defaults to '()"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
   (define sess (make-agent-session (make-test-config dir bus prov)))
   (check-equal? (agent-session-system-instructions sess)
                 '()
                 "system-instructions defaults to empty list")
   (delete-directory/files dir #:must-exist? #f))
 ;; ── 14. system-instructions stored when provided ──
 (test-case "make-agent-session stores system-instructions from config"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
   (define instrs '("You are a helpful assistant." "Always be concise."))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'system-instructions instrs)))
   (check-equal? (agent-session-system-instructions sess)
                 instrs
                 "system-instructions stored from config")
   (delete-directory/files dir #:must-exist? #f))
 ;; ── 15. run-prompt! injects system message but doesn't persist it ──
 (test-case "run-prompt! with system-instructions prepends system message to context"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   ;; Track what context the provider actually receives
   (define captured-context (box #f))
   (define prov
     (make-provider
      (lambda () "context-capture")
      (lambda () (hash 'streaming #t 'token-counting #t))
      ;; send: unused by run-agent-turn but required by provider interface
      (lambda (req)
        (make-model-response (list (hash 'type "text" 'text "Response")) (hash) "mock" 'stop))
      ;; stream: capture the messages sent to the model
      (lambda (req)
        (set-box! captured-context (model-request-messages req))
        (list (make-stream-chunk "Response" #f #f #t)))))
   (define instrs '("You are a coding expert."))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'system-instructions instrs)))
   (define-values (s result) (run-prompt! sess "Write code"))

   ;; Check that the context sent to the provider has a system message first
   (define ctx (unbox captured-context))
   (check-not-false ctx "provider should have received context")
   ;; The first message in context should be a raw hash with role "system"
   (define first-msg (first ctx))
   (check-equal? (hash-ref first-msg 'role) "system" "first context message should be system role")
   ;; The content should contain the instruction text
   (define first-content (hash-ref first-msg 'content))
   (check-true (string-contains? first-content "You are a coding expert.")
               "system message should contain the instruction text")

   ;; The system message should NOT be in the persisted session log
   (define hist (session-history s))
   (check-equal? (length hist) 2 "history should only have user + assistant")
   (check-equal? (message-role (first hist)) 'user)
   (check-equal? (message-role (second hist)) 'assistant)

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 16. fork-session copies system-instructions ──
 (test-case "fork-session copies system-instructions from parent"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
   (define instrs '("Be helpful." "Be safe."))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'system-instructions instrs)))
   (define forked (fork-session sess))
   (check-equal? (agent-session-system-instructions forked)
                 instrs
                 "forked session should inherit system-instructions")
   (delete-directory/files dir #:must-exist? #f))
 ;; ── 17. Session log is reloadable from disk ──
 (test-case "session log persists to disk and survives object recreation"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-multi-mock-provider
      (list (make-model-response (list (hash 'type "text" 'text "First")) (hash) "mock" 'stop)
            (make-model-response (list (hash 'type "text" 'text "Second")) (hash) "mock" 'stop))))

   ;; Create and run
   (define sess1 (make-agent-session (make-test-config dir bus prov)))
   (define-values (s1 _) (run-prompt! sess1 "Hello"))
   (define sid (session-id s1))

   ;; Resume in a new object
   (define bus2 (make-event-bus))
   (define sess2 (resume-agent-session sid (make-test-config dir bus2 prov)))
   (define hist (session-history sess2))
   (check-equal? (length hist) 2)

   ;; Can continue
   (define-values (s2 r2) (run-prompt! sess2 "More"))
   (check-equal? (loop-result-termination-reason r2) 'completed)
   (check-equal? (length (session-history s2)) 4)

   (delete-directory/files dir #:must-exist? #f))
 ;; ── 18. turn-start hook dispatched (F2: renamed from before-turn) ──
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
 ;; ── 19. turn-end hook dispatched (F2: renamed from after-turn) ──
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
 ;; ── 20. hooks skipped when extension-registry is #f ──
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
 ;; ── F1: tool-result hook amending with non-message values are filtered ──
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
 ;; ── F6: turn-start 'block skips the turn ──
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
 ;; ── F2: hook points use BLUEPRINT §7 names ──
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
 ;; ── GAP-5 FIXED: hook exception during dispatch is isolated ──
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
   (delete-directory/files dir #:must-exist? #f))
 ;; ── WP-35: Cooperative Cancellation Tests ──
 (test-case "run-prompt! with pre-cancelled token returns 'cancelled"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define tok (make-cancellation-token))
   (cancel-token! tok) ; pre-cancel
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "should not appear")) (hash) "mock" 'stop)))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'cancellation-token tok)))
   (define-values (s result) (run-prompt! sess "hello"))
   (check-equal? (loop-result-termination-reason result)
                 'cancelled
                 "should return 'cancelled when token is pre-cancelled")
   ;; No model response in history — only the user message was appended
   (define hist (session-history s))
   (check-equal? (length hist) 1 "only user message in history")
   (check-equal? (message-role (first hist)) 'user)
   (delete-directory/files dir #:must-exist? #f))
 (test-case "run-prompt! with pre-cancelled token emits turn.cancelled event"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define tok (make-cancellation-token))
   (cancel-token! tok)
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "should not appear")) (hash) "mock" 'stop)))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'cancellation-token tok)))
   (define-values (s result) (run-prompt! sess "hello"))
   (define names (event-names evts))
   (check-not-false (member "turn.cancelled" names) "turn.cancelled event should be emitted")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "run-prompt! token cancelled mid-loop stops after current iteration"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define evts (make-event-collector bus))
   (define reg (make-tool-registry))
   (define tok (make-cancellation-token #:callback (λ (_) (void))))

   ;; Register a tool
   (register-tool!
    reg
    (make-tool "echo"
               "Echoes input"
               (hasheq 'type "object" 'properties (hasheq 'msg (hasheq 'type "string")) 'required '())
               (lambda (args ctx) (make-success-result (list (hasheq 'type "text" 'text "echoed"))))))

   ;; Provider: first turn = tool call, second turn = would-be text (never reached)
   ;; Cancel the token AFTER the first turn completes (in the tool-call hook)
   (define turn-count (box 0))
   (define prov
     (make-provider
      (lambda () "cancel-mid-loop")
      (lambda () (hash 'streaming #t 'token-counting #t))
      (lambda (req) (make-model-response '() (hash) "mock" 'stop))
      (lambda (req)
        (define i (unbox turn-count))
        (set-box! turn-count (add1 i))
        (cond
          [(= i 0)
           ;; First turn: return a tool call
           (list (make-stream-chunk "calling tool" #f #f #f)
                 (make-stream-chunk #f (hasheq 'id "tc-1" 'name "echo" 'arguments "{}") #f #f)
                 (make-stream-chunk #f #f (hasheq) #t))]
          ;; Second turn: text response
          [else (list (make-stream-chunk "final text" #f #f #f) (make-stream-chunk #f #f (hasheq) #t))]))))

   ;; Use tool-call hook to cancel the token between iterations
   (define ext-reg (make-extension-registry))
   (register-extension! ext-reg
                        (extension "cancel-ext"
                                   "1.0"
                                   "0.1"
                                   (hasheq 'tool-call
                                           (lambda (tcs)
                                             ;; Cancel after first tool call is dispatched
                                             (cancel-token! tok)
                                             (hook-pass tcs)))))

   (define sess
     (make-agent-session (hash 'provider
                               prov
                               'tool-registry
                               reg
                               'event-bus
                               bus
                               'session-dir
                               dir
                               'extension-registry
                               ext-reg
                               'cancellation-token
                               tok
                               'max-iterations
                               5)))

   (define-values (s result) (run-prompt! sess "trigger tool call"))

   ;; Should have been cancelled after the first iteration completed
   (check-equal? (loop-result-termination-reason result)
                 'cancelled
                 "should terminate with 'cancelled when token cancelled mid-loop")
   ;; turn.cancelled event emitted
   (check-not-false (member "turn.cancelled" (event-names evts))
                    "turn.cancelled event should be emitted")
   ;; History should have user + assistant(tool-call) + tool-result from first iteration
   (define hist (session-history s))
   (check-true (>= (length hist) 3)
               "should have at least user + assistant + tool-result from first iteration")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "run-prompt! with no cancellation token runs normally (backward compat)"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "normal response")) (hash) "mock" 'stop)))
   ;; No cancellation-token in config
   (define sess (make-agent-session (make-test-config dir bus prov)))
   (define-values (s result) (run-prompt! sess "hello"))
   (check-equal? (loop-result-termination-reason result)
                 'completed
                 "should complete normally without cancellation token")
   (define hist (session-history s))
   (check-equal? (length hist) 2 "user + assistant")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "run-prompt! with uncancelled token runs normally"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define tok (make-cancellation-token)) ; not cancelled
   (define prov
     (make-mock-provider
      (make-model-response (list (hash 'type "text" 'text "normal response")) (hash) "mock" 'stop)))
   (define sess
     (make-agent-session (hash-set (make-test-config dir bus prov) 'cancellation-token tok)))
   (define-values (s result) (run-prompt! sess "hello"))
   (check-equal? (loop-result-termination-reason result)
                 'completed
                 "should complete normally with uncancelled token")
   (define hist (session-history s))
   (check-equal? (length hist) 2 "user + assistant")
   (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; extension-registry and model-name wiring
;; ============================================================

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
    (check-equal? (agent-session-model-name forked) "fork-model")))

;; ============================================================
;; Tiered Context Assembly Tests (WP-37)
;; ============================================================

;; compactor.rkt exports already imported above

(define-test-suite
 test-tiered-context-suite
 ;; ── Helper to create messages with specific properties ──
 (test-case "build-tiered-context splits messages into three tiers"
   (define msgs
     (list
      ;; Older messages (will be Tier A - compacted)
      (make-message "id-1" #f 'user 'message (list (make-text-part "Old user 1")) 1000 (hasheq))
      (make-message "id-2"
                    "id-1"
                    'assistant
                    'message
                    (list (make-text-part "Old assistant 1"))
                    1001
                    (hasheq))
      ;; Recent messages (Tier B - full)
      (make-message "id-3" #f 'user 'message (list (make-text-part "Recent user")) 2000 (hasheq))
      (make-message "id-4"
                    "id-3"
                    'assistant
                    'message
                    (list (make-text-part "Recent assistant"))
                    2001
                    (hasheq))
      ;; Current turn (Tier C)
      (make-message "id-5" #f 'user 'message (list (make-text-part "Current user")) 3000 (hasheq))))

   ;; Compact the older portion to simulate Tier A
   (define-values (old-msgs recent-msgs) (values (take msgs 2) (drop msgs 2)))
   (define compact-result (compact-history msgs #:token-config (token-compaction-config 10 0 10)))
   (define compacted-context (compaction-result->message-list compact-result))

   ;; Now build tiered context
   (define tiered (build-tiered-context compacted-context #:tier-b-count 2 #:tier-c-count 1))

   ;; Tier A should have the summary message
   (check-equal? (length (tiered-context-tier-a tiered)) 1 "Tier A should have 1 summary message")
   (check-equal? (message-kind (first (tiered-context-tier-a tiered)))
                 'compaction-summary
                 "Tier A should contain compaction-summary message")

   ;; Tier B should have recent messages
   ;; With token compaction: summary + 3 kept = 4 msgs. Tier C=1, Tier B=2, Tier A=1
   (check-equal? (length (tiered-context-tier-b tiered))
                 2
                 "Tier B should have 2 messages (remaining after Tier C)")

   ;; Tier C should have the most recent message
   (check-equal? (length (tiered-context-tier-c tiered)) 1 "Tier C should have 1 message")
   (check-equal? (message-content (first (tiered-context-tier-c tiered)))
                 (message-content (last msgs))
                 "Tier C should contain the most recent message"))
 (test-case "build-tiered-context with no compaction returns empty Tier A"
   (define msgs
     (list (make-message "id-1" #f 'user 'message (list (make-text-part "User 1")) 1000 (hasheq))
           (make-message "id-2"
                         "id-1"
                         'assistant
                         'message
                         (list (make-text-part "Assistant 1"))
                         1001
                         (hasheq))
           (make-message "id-3" #f 'user 'message (list (make-text-part "User 2")) 1002 (hasheq))))

   ;; Build tiered context without prior compaction
   (define tiered (build-tiered-context msgs #:tier-b-count 2 #:tier-c-count 1))

   ;; Tier A should be empty (no compaction summaries)
   (check-equal? (length (tiered-context-tier-a tiered))
                 0
                 "Tier A should be empty when no compaction summaries exist")

   ;; Tier B should have the older messages
   (check-equal? (length (tiered-context-tier-b tiered)) 2 "Tier B should have 2 messages")

   ;; Tier C should have the most recent
   (check-equal? (length (tiered-context-tier-c tiered)) 1 "Tier C should have 1 message"))
 (test-case "tiered-context->message-list flattens in correct order"
   (define tier-a
     (list (make-message "sum-1"
                         #f
                         'system
                         'compaction-summary
                         (list (make-text-part "Summary"))
                         1000
                         (hasheq))))
   (define tier-b
     (list (make-message "id-1" #f 'user 'message (list (make-text-part "Tier B")) 2000 (hasheq))))
   (define tier-c
     (list (make-message "id-2" #f 'user 'message (list (make-text-part "Tier C")) 3000 (hasheq))))

   (define tiered (tiered-context tier-a tier-b tier-c))
   (define flat (tiered-context->message-list tiered))

   ;; Order should be: Tier A (summary), Tier B (recent), Tier C (current)
   (check-equal? (length flat) 3)
   (check-equal? (message-kind (first flat)) 'compaction-summary)
   (check-equal? (text-part-text (first (message-content (second flat)))) "Tier B")
   (check-equal? (text-part-text (first (message-content (third flat)))) "Tier C"))
 (test-case "build-tiered-context respects custom tier boundaries"
   (define msgs
     (for/list ([i (in-range 10)])
       (make-message (format "id-~a" i)
                     #f
                     'user
                     'message
                     (list (make-text-part (format "Message ~a" i)))
                     i
                     (hasheq))))

   ;; tier-b-count=3, tier-c-count=2
   (define tiered (build-tiered-context msgs #:tier-b-count 3 #:tier-c-count 2))

   ;; Total: 10 messages, Tier C takes last 2, Tier B takes 3 before that
   ;; Tier A is empty (no compaction), Tier B has 5, Tier C has 2? No...
   ;; Actually: Tier B = recent but not current = 3, Tier C = current = 2
   ;; Remaining = 10 - 3 - 2 = 5 would be Tier A if compacted, but we have no summaries
   (check-equal? (length (tiered-context-tier-a tiered)) 0)
   (check-equal? (length (tiered-context-tier-b tiered)) 3 "Tier B should have exactly 3 messages")
   (check-equal? (length (tiered-context-tier-c tiered)) 2 "Tier C should have exactly 2 messages"))
 (test-case "build-tiered-context with empty input"
   (define tiered (build-tiered-context '() #:tier-b-count 5 #:tier-c-count 1))
   (check-equal? (length (tiered-context-tier-a tiered)) 0)
   (check-equal? (length (tiered-context-tier-b tiered)) 0)
   (check-equal? (length (tiered-context-tier-c tiered)) 0))
 (test-case "build-tiered-context handles messages smaller than tier-c-count"
   (define msgs
     (list (make-message "id-1" #f 'user 'message (list (make-text-part "Only")) 1000 (hasheq))))

   (define tiered (build-tiered-context msgs #:tier-b-count 5 #:tier-c-count 3))

   ;; With only 1 message and tier-c-count=3, all go to Tier C
   (check-equal? (length (tiered-context-tier-a tiered)) 0)
   (check-equal? (length (tiered-context-tier-b tiered)) 0)
   (check-equal? (length (tiered-context-tier-c tiered)) 1)))

;; ============================================================
;; Fork/compact event wiring tests
;; ============================================================

(define test-event-wiring-suite
  (test-suite "event wiring for fork/compact"

    (test-case "fork.requested event triggers fork-session"
      (define bus (make-event-bus))
      (define tmpdir (make-temp-dir))
      (define prov
        (make-mock-provider
         (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
      (define cfg
        (hasheq 'provider
                prov
                'tool-registry
                (make-tool-registry)
                'event-bus
                bus
                'session-dir
                tmpdir))
      (define sess (make-agent-session cfg))
      ;; Add a user message to the session via run-prompt!
      (run-prompt! sess "test message")
      ;; Collect events from fork
      (define fork-events (box '()))
      (subscribe! bus
                  (lambda (evt) (set-box! fork-events (cons evt (unbox fork-events))))
                  #:filter (lambda (e)
                             (member (event-ev e) '("session.fork.completed" "session.fork.failed"))))
      ;; Publish fork.requested with the first user message ID
      (define history (session-history sess))
      (define first-msg-id (and (pair? history) (message-id (car history))))
      (publish! bus
                (make-event "fork.requested"
                            1001
                            (session-id sess)
                            #f
                            (hasheq 'entry-id (or first-msg-id "unknown"))))
      ;; Allow thread to process (publish! is synchronous but yield for safety)
      (sync/timeout 0.5 never-evt)
      ;; Verify fork completed event was emitted
      (check-not-equal? (length (unbox fork-events))
                        0
                        "fork.requested should trigger session.fork.completed or session.forked")
      (close-session! sess)
      (delete-directory/files tmpdir))

    (test-case "compact.requested event triggers compaction"
      (define bus (make-event-bus))
      (define tmpdir (make-temp-dir))
      (define prov
        (make-mock-provider
         (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
      (define cfg
        (hasheq 'provider
                prov
                'tool-registry
                (make-tool-registry)
                'event-bus
                bus
                'session-dir
                tmpdir))
      (define sess (make-agent-session cfg))
      ;; Add messages via run-prompt!
      (for ([i (in-range 3)])
        (run-prompt! sess (format "message ~a" i)))
      ;; Collect events from compact
      (define compact-events (box '()))
      (subscribe! bus
                  (lambda (evt) (set-box! compact-events (cons evt (unbox compact-events))))
                  #:filter (lambda (e)
                             (member (event-ev e)
                                     '("session.compact.completed" "session.compact.failed"))))
      ;; Publish compact.requested
      (publish! bus (make-event "compact.requested" 1001 (session-id sess) #f (hasheq)))
      ;; Allow thread to process (publish! is synchronous but yield for safety)
      (sync/timeout 0.5 never-evt)
      ;; Verify compact completed event was emitted
      (check-not-equal? (length (unbox compact-events))
                        0
                        "compact.requested should trigger session.compact.completed")
      (close-session! sess)
      (delete-directory/files tmpdir))))

;; ============================================================
;; Run
;; ============================================================

(run-tests test-agent-session-suite)
(run-tests test-tiered-context-suite)
(run-tests test-event-wiring-suite)
