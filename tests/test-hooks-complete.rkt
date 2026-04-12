#lang racket

;; tests/test-hooks-complete.rkt - Tests for all 20 hook dispatch points (R2-7 + Wave 2)

(require rackunit
         rackunit/text-ui
         racket/file
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../agent/loop.rkt"
         "../agent/state.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../runtime/iteration.rkt"
         "../runtime/agent-session.rkt"
         "../skills/types.rkt"
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result)
         (only-in "../tools/tool.rkt"
                  make-tool make-tool-registry make-exec-context
                  register-tool! tool-name tool-schema tool-execute
                  tool-call tool-call? make-tool-call
                  tool-call-id tool-call-name tool-call-arguments
                  make-error-result make-success-result)
         "../util/ids.rkt"
         (only-in "../llm/provider.rkt" make-mock-provider)
         (only-in "../llm/model.rkt" make-model-response))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-user-message text)
  (make-message (generate-id) #f 'user 'user
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define (make-test-extension-registry . point-handler-pairs)
  (define reg (make-extension-registry))
  (for ([pair (in-list point-handler-pairs)])
    (define point (car pair))
    (define handler (cadr pair))
    (define ext (extension (format "test-ext-~a" point)
                           "0.1.0"
                           "1.0"
                           (hasheq point handler)))
    (register-extension! reg ext))
  reg)

(define (make-hook-dispatcher reg)
  (lambda (hook-point payload)
    (dispatch-hooks hook-point payload reg)))

(define (make-event-collector bus)
  (define collected (box '()))
  (subscribe! bus
              (lambda (evt)
                (set-box! collected
                          (append (unbox collected) (list evt)))))
  collected)

;; ============================================================
;; Test suite
;; ============================================================

(define hooks-tests
  (test-suite
   "Hook Dispatch Points (R2-7)"

   ;; ---- Hook 1: model-request-pre (in agent loop) ----
   (test-case "model-request-pre hook is dispatched before streaming"
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-payload payload)
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))

     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))

     (define ctx (list (make-user-message "Hello")))
     (run-agent-turn ctx mock-prov bus
                     #:session-id "test"
                     #:turn-id "t1"
                     #:hook-dispatcher hook-dispatcher)

     (check-true (unbox hook-called?) "model-request-pre hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'message-count 0) 1
                   "payload should contain message count"))

   ;; ---- Hook 2: model-response-post (in agent loop) ----
   (test-case "model-response-post hook is dispatched after response"
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define reg (make-test-extension-registry
                  (list 'model-response-post
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-payload payload)
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))

     (define content-parts (list (hasheq 'type "text" 'text "Response text")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))

     (define ctx (list (make-user-message "Hello")))
     (run-agent-turn ctx mock-prov bus
                     #:session-id "test"
                     #:turn-id "t1"
                     #:hook-dispatcher hook-dispatcher)

     (check-true (unbox hook-called?) "model-response-post hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'response-content "")
                   "Response text"
                   "payload should contain response content"))

   ;; ---- Hook 3: tool-call-pre (in scheduler) ----
   (test-case "tool-call-pre hook is dispatched before tool execution"
     (define hook-called? (box #f))
     (define captured-name (box #f))
     (define reg (make-test-extension-registry
                  (list 'tool-call-pre
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-name (hash-ref payload 'tool-name #f))
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))

     (define test-tool
       (make-tool "test-tool" "A test tool"
                  (hasheq 'type "object" 'properties (hasheq))
                  (lambda (args exec-ctx)
                    (make-success-result "tool output"))))

     (define tc (make-tool-call "tc-1" "test-tool" (hasheq)))
     (define registry (make-tool-registry))
     (register-tool! registry test-tool)

     (define results (run-tool-batch (list tc) registry #:hook-dispatcher hook-dispatcher))

     (check-true (unbox hook-called?) "tool-call-pre hook should be called")
     (check-equal? (unbox captured-name) "test-tool" "payload should contain tool name"))

   ;; ---- Hook 4: tool-result-post (in scheduler) ----
   (test-case "tool-result-post hook is dispatched after tool execution"
     (define hook-called? (box #f))
     (define captured-result (box #f))
     (define reg (make-test-extension-registry
                  (list 'tool-result-post
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-result (hash-ref payload 'result #f))
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))

     (define test-tool
       (make-tool "result-tool" "A test tool"
                  (hasheq 'type "object" 'properties (hasheq))
                  (lambda (args exec-ctx)
                    (make-success-result "done"))))

     (define tc (make-tool-call "tc-2" "result-tool" (hasheq)))
     (define registry (make-tool-registry))
     (register-tool! registry test-tool)

     (define results (run-tool-batch (list tc) registry #:hook-dispatcher hook-dispatcher))

     (check-true (unbox hook-called?) "tool-result-post hook should be called")
     (check-not-false (unbox captured-result) "payload should contain result"))

   ;; ---- Hook 5: hook-pass action works correctly ----
   (test-case "hook-pass action allows normal execution"
     (define bus (make-event-bus))
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload) (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))

     (define content-parts (list (hasheq 'type "text" 'text "OK")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))

     (define result
       (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))

     (check-equal? (loop-result-termination-reason result) 'completed
                   "pass action should allow normal completion"))

   ;; ---- Hook 6: hook-amend modifies payload ----
   (test-case "hook-amend action modifies payload"
     (define amended? (box #f))
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload)
                          (set-box! amended? #t)
                          (hook-amend (hash-set payload 'amended #t))))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define result (hook-dispatcher 'model-request-pre (hasheq 'x 1)))
     (check-true (unbox amended?) "handler was called")
     (check-equal? (hook-result-action result) 'amend "action is amend"))

   ;; ---- Hook 7: hook-block stops dispatch ----
   (test-case "hook-block action stops dispatch"
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload)
                          (hook-block "blocked for testing")))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define result (hook-dispatcher 'model-request-pre (hasheq)))
     (check-equal? (hook-result-action result) 'block "action is block")
     (check-equal? (hook-result-payload result) "blocked for testing"
                   "block reason preserved"))

   ;; ---- Hook 9: hook-block with no registry returns payload unchanged ----
   (test-case "maybe-dispatch-hooks with #f registry returns payload unchanged"
     (define-values (payload result)
       (maybe-dispatch-hooks #f 'turn-start (hasheq 'x 1)))
     (check-equal? payload (hasheq 'x 1) "payload unchanged when no registry")
     (check-false result "hook result is #f when no registry"))

   ;; ---- Hook: session-before-compact ----
   (test-case "session-before-compact hook is dispatched before compaction"
     ;; Create a session with a very low token-budget-threshold to trigger compaction.
     ;; Register a session-before-compact hook and verify it fires.
     (define dir (make-temporary-file "q-session-compact-~a" 'directory))
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define ext-reg (make-test-extension-registry
                      (list 'session-before-compact
                            (lambda (payload)
                              (set-box! hook-called? #t)
                              (set-box! captured-payload payload)
                              (hook-pass payload)))))
     ;; Provider that returns a simple text response
     (define prov (make-mock-provider
                   (make-model-response
                    (list (hash 'type "text" 'text "Response"))
                    (hash) "mock" 'stop)))
     ;; Use a very low token-budget-threshold (1) to guarantee compaction triggers
     (define sess (make-agent-session
                   (hash 'provider prov
                         'tool-registry (make-tool-registry)
                         'event-bus bus
                         'session-dir dir
                         'extension-registry ext-reg
                         'token-budget-threshold 1)))
     (define-values (s result) (run-prompt! sess "Hello world this is a test"))
     (check-true (unbox hook-called?) "session-before-compact hook should be called")
     (check-not-false (unbox captured-payload) "payload should be captured")
     (check-equal? (hash-ref (unbox captured-payload) 'session-id) (session-id s)
                   "payload should contain session-id")
     (delete-directory/files dir #:must-exist? #f))

   (test-case "session-before-compact hook 'block prevents compaction"
     ;; When the hook blocks, compaction should NOT run even though threshold is exceeded.
     (define dir (make-temporary-file "q-session-compact-block-~a" 'directory))
     (define bus (make-event-bus))
     (define evts (make-event-collector bus))
     (define ext-reg (make-test-extension-registry
                      (list 'session-before-compact
                            (lambda (payload)
                              (hook-block "compaction blocked for testing")))))
     (define prov (make-mock-provider
                   (make-model-response
                    (list (hash 'type "text" 'text "Response"))
                    (hash) "mock" 'stop)))
     (define sess (make-agent-session
                   (hash 'provider prov
                         'tool-registry (make-tool-registry)
                         'event-bus bus
                         'session-dir dir
                         'extension-registry ext-reg
                         'token-budget-threshold 1)))
     (define-values (s result) (run-prompt! sess "Hello world this is a test"))
     ;; Should still complete (just without compaction)
     (check-equal? (loop-result-termination-reason result) 'completed)
     ;; No compaction.warning event should be emitted (blocked before it)
     (define names (map event-event (unbox evts)))
     (check-false (member "compaction.warning" names)
                  "compaction.warning should NOT be emitted when blocked")
     (delete-directory/files dir #:must-exist? #f))

   ;; ---- Hook: before-agent-start ----
   (test-case "before-agent-start hook is dispatched before first iteration"
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define ext-reg (make-test-extension-registry
                      (list 'before-agent-start
                            (lambda (payload)
                              (set-box! hook-called? #t)
                              (set-box! captured-payload payload)
                              (hook-pass payload)))))
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define ctx (list (make-user-message "Hello")))
     ;; Need a writable log-path for run-iteration-loop
     (define tmpdir (make-temporary-file "q-iteration-test-~a" 'directory))
     (define log-path (build-path tmpdir "session.jsonl"))
     (run-iteration-loop ctx mock-prov bus #f ext-reg log-path "test-session" 10)
     (check-true (unbox hook-called?) "before-agent-start hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'session-id) "test-session"
                   "payload should contain session-id")
     (check-equal? (hash-ref (unbox captured-payload) 'max-iterations) 10
                   "payload should contain max-iterations")
     (check-equal? (hash-ref (unbox captured-payload) 'context-message-count) 1
                   "payload should contain context-message-count")
     (delete-directory/files tmpdir #:must-exist? #f))

   (test-case "before-agent-start hook 'block prevents iteration loop"
     (define bus (make-event-bus))
     (define evts (make-event-collector bus))
     (define ext-reg (make-test-extension-registry
                      (list 'before-agent-start
                            (lambda (payload)
                              (hook-block "agent start blocked")))))
     (define content-parts (list (hasheq 'type "text" 'text "Should not appear")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define ctx (list (make-user-message "Hello")))
     (define tmpdir (make-temporary-file "q-iteration-block-~a" 'directory))
     (define log-path (build-path tmpdir "session.jsonl"))
     (define result (run-iteration-loop ctx mock-prov bus #f ext-reg log-path "test-session" 10))
     (check-equal? (loop-result-termination-reason result) 'completed
                   "blocked before-agent-start should return 'completed")
     (check-equal? (hash-ref (loop-result-metadata result) 'reason) "extension-block"
                   "metadata should contain extension-block reason")
     ;; agent.blocked event emitted
     (define names (map event-event (unbox evts)))
     (check-not-false (member "agent.blocked" names)
                      "agent.blocked event should be emitted")
     (delete-directory/files tmpdir #:must-exist? #f))

   ;; ---- Hook 8: no hook dispatcher - no errors ----
   (test-case "missing hook dispatcher does not cause errors"
     (define bus (make-event-bus))
     (define content-parts (list (hasheq 'type "text" 'text "OK")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))

     (define result
       (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher #f))

     (check-equal? (loop-result-termination-reason result) 'completed
                   "no hook dispatcher should work normally"))

   ;; ---- Hook: message-start ----
   (test-case "message-start hook is dispatched before streaming"
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define reg (make-test-extension-registry
                  (list 'message-start
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-payload payload)
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                     #:session-id "test" #:turn-id "t1"
                     #:hook-dispatcher hook-dispatcher)
     (check-true (unbox hook-called?) "message-start hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'message-count 0) 1
                   "payload should contain message count"))

   (test-case "message-start hook block aborts turn"
     (define bus (make-event-bus))
     (define reg (make-test-extension-registry
                  (list 'message-start
                        (lambda (payload)
                          (hook-block "blocked by test")))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define result
       (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))
     (check-equal? (loop-result-termination-reason result) 'hook-blocked
                   "message-start block should abort with hook-blocked")
     (check-equal? (hash-ref (loop-result-metadata result) 'hook #f) 'message-start
                   "metadata should identify message-start hook"))

   ;; ---- Hook: message-update ----
   (test-case "message-update hook receives each text delta"
     (define bus (make-event-bus))
     (define deltas (box '()))
     (define reg (make-test-extension-registry
                  (list 'message-update
                        (lambda (payload)
                          (when (hash-ref payload 'delta-text #f)
                            (set-box! deltas
                                      (append (unbox deltas)
                                              (list (hash-ref payload 'delta-text)))))
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define content-parts (list (hasheq 'type "text" 'text "Hello world")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define result
       (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))
     (check-equal? (loop-result-termination-reason result) 'completed
                   "message-update pass should allow normal completion")
     (check-not-false (unbox deltas) "message-update hook should have been called")
     ;; The mock provider sends the full text as one chunk
     (check-equal? (unbox deltas) '("Hello world")
                   "message-update should receive text delta"))

   (test-case "message-update hook block stops streaming"
     (define bus (make-event-bus))
     (define call-count (box 0))
     (define reg (make-test-extension-registry
                  (list 'message-update
                        (lambda (payload)
                          (set-box! call-count (+ 1 (unbox call-count)))
                          (hook-block "stop streaming")))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define content-parts (list (hasheq 'type "text" 'text "Hello world")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define result
       (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))
     ;; message-update block stops streaming but the turn still completes
     ;; (stream-blocked flag halts the loop, accumulated text is partial)
     (check-equal? (loop-result-termination-reason result) 'completed
                   "message-update block should still complete the turn"))

   ;; ---- Hook: message-end ----
   (test-case "message-end hook is dispatched after response"
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define reg (make-test-extension-registry
                  (list 'message-end
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-payload payload)
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define content-parts (list (hasheq 'type "text" 'text "Response text")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define result
       (run-agent-turn (list (make-user-message "Hello")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))
     (check-true (unbox hook-called?) "message-end hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'content #f) "Response text"
                   "payload should contain accumulated content")
     (check-equal? (loop-result-termination-reason result) 'completed
                   "pass should complete normally"))

   (test-case "message-end hook amend modifies content"
     (define bus (make-event-bus))
     (define reg (make-test-extension-registry
                  (list 'message-end
                        (lambda (payload)
                          (hook-amend (hash-set payload 'content "AMENDED CONTENT"))))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define content-parts (list (hasheq 'type "text" 'text "Original text")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define result
       (run-agent-turn (list (make-user-message "Hello")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))
     (check-equal? (loop-result-termination-reason result) 'completed
                   "amend should complete normally")
     ;; Check the result messages contain amended text
     (define msgs (loop-result-messages result))
     (define assistant-msg (findf (lambda (m) (eq? (message-role m) 'assistant)) msgs))
     (check-not-false assistant-msg "should have an assistant message")
     (when assistant-msg
       (define text-parts (filter text-part? (message-content assistant-msg)))
       (check-equal? (text-part-text (car text-parts)) "AMENDED CONTENT"
                     "amended content should replace original")))

   (test-case "message-end hook block returns hook-blocked"
     (define bus (make-event-bus))
     (define reg (make-test-extension-registry
                  (list 'message-end
                        (lambda (payload)
                          (hook-block "suppressed by test")))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define content-parts (list (hasheq 'type "text" 'text "Response text")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     (define result
       (run-agent-turn (list (make-user-message "Hello")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))
     (check-equal? (loop-result-termination-reason result) 'hook-blocked
                   "message-end block should return hook-blocked")
     (check-equal? (hash-ref (loop-result-metadata result) 'hook #f) 'message-end
                   "metadata should identify message-end hook"))

   ;; ---- Hook: resources-discover ----
   (test-case "resources-discover hook receives base-dir"
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define reg (make-test-extension-registry
                  (list 'resources-discover
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-payload payload)
                          (hook-pass payload)))))
     ;; Create a temp dir with a .q subdir to scan
     (define tmp-dir (make-temporary-file "q-test-resources-~a" 'directory))
     (define q-dir (build-path tmp-dir ".q"))
     (make-directory* q-dir)
     (define result (load-global-resources tmp-dir #:hook-dispatcher (make-hook-dispatcher reg)))
     (check-true (unbox hook-called?) "resources-discover hook should be called")
     (check-not-false (hash-ref (unbox captured-payload) 'base-dir #f)
                      "payload should contain base-dir")
     (delete-directory/files tmp-dir))

   (test-case "resources-discover hook block returns empty resource-set"
     (define reg (make-test-extension-registry
                  (list 'resources-discover
                        (lambda (payload) (hook-block "blocked")))))
     (define tmp-dir (make-temporary-file "q-test-resources-~a" 'directory))
     (define q-dir (build-path tmp-dir ".q"))
     (make-directory* q-dir)
     ;; Even with a real dir, block should return empty
     (define result (load-global-resources tmp-dir #:hook-dispatcher (make-hook-dispatcher reg)))
     (check-equal? result (empty-resource-set)
                   "blocked resources-discover should return empty resource-set")
     (delete-directory/files tmp-dir))

   (test-case "resources-discover hook amend provides custom resources"
     (define custom-resources (resource-set "custom instructions" '() (hash) (hash)))
     (define reg (make-test-extension-registry
                  (list 'resources-discover
                        (lambda (payload)
                          (hook-amend custom-resources)))))
     (define tmp-dir (make-temporary-file "q-test-resources-~a" 'directory))
     (define q-dir (build-path tmp-dir ".q"))
     (make-directory* q-dir)
     (define result (load-global-resources tmp-dir #:hook-dispatcher (make-hook-dispatcher reg)))
     (check-equal? result custom-resources
                   "amended resources-discover should return custom resource-set")
     (delete-directory/files tmp-dir))

   ;; ---- Hook: session-before-switch ----
   (test-case "session-before-switch hook dispatched on resume"
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define ext-reg (make-test-extension-registry
                      (list 'session-before-switch
                            (lambda (payload)
                              (set-box! hook-called? #t)
                              (set-box! captured-payload payload)
                              (hook-pass payload)))))
     (define dir (make-temporary-file "q-session-switch-~a" 'directory))
     (define bus (make-event-bus))
     (define prov (make-mock-provider
                   (make-model-response
                    (list (hash 'type "text" 'text "Response"))
                    (hash) "mock" 'stop)))
     ;; Create a session, close it, then resume it
     (define sess (make-agent-session
                   (hash 'provider prov
                         'tool-registry (make-tool-registry)
                         'event-bus bus
                         'session-dir dir
                         'extension-registry ext-reg)))
     (close-session! sess)
     (define resumed (resume-agent-session
                      (session-id sess)
                      (hash 'provider prov
                            'tool-registry (make-tool-registry)
                            'event-bus bus
                            'session-dir dir
                            'extension-registry ext-reg)))
     (check-true (unbox hook-called?) "session-before-switch hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'session-id) (session-id sess)
                   "payload should contain session-id")
     (check-equal? (hash-ref (unbox captured-payload) 'operation) 'resume
                   "payload should contain 'resume operation")
     (delete-directory/files dir #:must-exist? #f))

   (test-case "session-before-switch hook block prevents resume"
     (define ext-reg (make-test-extension-registry
                      (list 'session-before-switch
                            (lambda (payload) (hook-block "blocked")))))
     (define dir (make-temporary-file "q-session-block-~a" 'directory))
     (define bus (make-event-bus))
     (define prov (make-mock-provider
                   (make-model-response
                    (list (hash 'type "text" 'text "Response"))
                    (hash) "mock" 'stop)))
     ;; Create a session, close it, then attempt resume (should error)
     (define sess (make-agent-session
                   (hash 'provider prov
                         'tool-registry (make-tool-registry)
                         'event-bus bus
                         'session-dir dir
                         'extension-registry ext-reg)))
     (close-session! sess)
     (check-exn
      exn:fail?
      (lambda ()
        (resume-agent-session
         (session-id sess)
         (hash 'provider prov
               'tool-registry (make-tool-registry)
               'event-bus bus
               'session-dir dir
               'extension-registry ext-reg)))
      "blocked session-before-switch should raise an error")
     (delete-directory/files dir #:must-exist? #f))

   ;; ---- FUNC-06 (#104): Non-hook-result return values don't crash dispatch ----
   (test-case "handler returning void does not crash dispatch-hooks"
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload) (void)))))
     (define result (dispatch-hooks 'model-request-pre (hasheq 'x 1) reg))
     (check-true (hook-result? result) "void return should produce hook-result")
     (check-equal? (hook-result-action result) 'pass "void return should default to pass"))

   (test-case "handler returning string does not crash dispatch-hooks"
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload) "bad"))))
     (define result (dispatch-hooks 'model-request-pre (hasheq 'x 1) reg))
     (check-true (hook-result? result) "string return should produce hook-result")
     (check-equal? (hook-result-action result) 'pass "string return should default to pass"))

   (test-case "handler returning #f does not crash dispatch-hooks"
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload) #f))))
     (define result (dispatch-hooks 'model-request-pre (hasheq 'x 1) reg))
     (check-true (hook-result? result) "#f return should produce hook-result")
     (check-equal? (hook-result-action result) 'pass "#f return should default to pass"))

   (test-case "handler returning proper hook-result still works after fix"
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload) (hook-amend (hash-set payload 'ok #t))))))
     (define result (dispatch-hooks 'model-request-pre (hasheq 'x 1) reg))
     (check-true (hook-result? result) "proper hook-result should still work")
     (check-equal? (hook-result-action result) 'amend "action should be amend")
     (check-equal? (hash-ref (hook-result-payload result) 'ok) #t "payload should contain amendment"))
   ))

;; Run tests
(module+ main
  (run-tests hooks-tests))

(module+ test
  (run-tests hooks-tests))
