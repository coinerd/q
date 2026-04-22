#lang racket

;; tests/test-agent-session-basic.rkt — session creation, text prompts, tool calls, lifecycle
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
 test-agent-session-basic-suite
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
     (make-agent-session (hash 'provider
                               prov
                               'tool-registry
                               reg
                               'event-bus
                               bus
                               'session-dir
                               dir
                               'max-iterations
                               2
                               'max-iterations-hard
                               2)))

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

   (delete-directory/files dir #:must-exist? #f)))

(run-tests test-agent-session-basic-suite)
