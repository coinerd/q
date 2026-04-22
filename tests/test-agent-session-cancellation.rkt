#lang racket

;; tests/test-agent-session-cancellation.rkt — cancellation token handling
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
 test-agent-session-cancellation-suite
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
          [else
           (list (make-stream-chunk "final text" #f #f #f) (make-stream-chunk #f #f (hasheq) #t))]))))

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

(run-tests test-agent-session-cancellation-suite)
