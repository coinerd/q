#lang racket

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration

;; tests/test-iteration-observability.rkt — Working set observability + read-spiral detection

(require rackunit
         rackunit/text-ui
         racket/list
         racket/file
         "../runtime/working-set.rkt"
         (only-in "helpers/iteration-loop.rkt" run-iteration-loop)
         "../runtime/turn-orchestrator.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         "../util/message/protocol-types.rkt"
         "../util/event/event-bus.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         "../util/ids.rkt"
         (only-in "../tools/tool.rkt" make-tool make-tool-registry register-tool! make-success-result)
         (only-in "../tools/permission-gate.rkt" make-permissive-permission-config)
         (only-in "../util/hook-types.rkt" hook-amend)
         (only-in "../extensions/api.rkt" make-extension-registry register-extension! extension)
         (only-in "helpers/mock-provider.rkt"
                  make-multi-mock-provider
                  make-tool-call-mock-provider
                  make-test-config)
         (only-in "../runtime/session/session-config.rkt" hash->session-config))

;; ── Helpers ──

(define (make-temp-dir)
  (make-temporary-file "q-obs-test-~a" 'directory))

(define (make-event-collector bus)
  (define collected (box '()))
  (subscribe! bus (lambda (evt) (set-box! collected (append (unbox collected) (list evt)))))
  collected)

(define (event-names collected-box)
  (map event-ev (unbox collected-box)))

(define (events-with-name collected-box name)
  (filter (lambda (e) (equal? (event-ev e) name)) (unbox collected-box)))

(define read-dummy-tool
  (make-tool
   "read"
   "read files"
   (hasheq 'type "object" 'properties (hasheq 'path (hasheq 'type "string")) 'required '("path"))
   (lambda (args ctx) (make-success-result (list "read-content") (hasheq)))))

(define edit-dummy-tool
  (make-tool "edit"
             "edit files"
             (hasheq 'type
                     "object"
                     'properties
                     (hasheq 'path
                             (hasheq 'type "string")
                             'old-text
                             (hasheq 'type "string")
                             'new-text
                             (hasheq 'type "string"))
                     'required
                     '("path" "old-text" "new-text"))
             (lambda (args ctx) (make-success-result (list "edited") (hasheq)))))

(define (make-read-response path)
  (make-model-response
   (list (hash 'type "tool-call" 'id (generate-id) 'name "read" 'arguments (hasheq 'path path)))
   (hasheq 'prompt_tokens 10 'completion_tokens 5 'total_tokens 15)
   "mock"
   'tool-calls))

(define text-response
  (make-model-response (list (hash 'type "text" 'text "done"))
                       (hasheq 'prompt_tokens 10 'completion_tokens 5 'total_tokens 15)
                       "mock"
                       'stop))

(define (make-edit-response path)
  (make-model-response (list (hash 'type
                                   "tool-call"
                                   'id
                                   (generate-id)
                                   'name
                                   "edit"
                                   'arguments
                                   (hasheq 'path path 'old-text "old" 'new-text "new")))
                       (hasheq 'prompt_tokens 10 'completion_tokens 5 'total_tokens 15)
                       "mock"
                       'tool-calls))

;; ── Test Suite ──

(define iteration-observability-tests
  (test-suite "Iteration Observability + Read-Spiral Detection"

    ;; ── T01: context.assembled includes working-set diagnostics ──
    (test-case "T01: build-assembled-context includes working-set diagnostics"
      (define bus (make-event-bus))
      (define collected (make-event-collector bus))
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/a.rkt")))
                           (list (make-message "m1"
                                               #f
                                               'tool
                                               'tool-result
                                               (list (make-text-part "content"))
                                               (current-seconds)
                                               (hasheq)))
                           message-id
                           (lambda (m) 20))
      (define config
        (hash->session-config
         (hasheq 'working-set ws 'tier-b-count 5 'tier-c-count 1 'max-tokens 10000)))
      (define ctx
        (list (make-message "u1"
                            #f
                            'user
                            'message
                            (list (make-text-part "hello"))
                            (current-seconds)
                            (hasheq))))
      (define result (build-assembled-context ctx config #f bus "sess-1" 0))
      (check-pred list? result)
      (define assembled-events (events-with-name collected "context.assembled"))
      (check-equal? (length assembled-events) 1)
      (define payload (event-payload (first assembled-events)))
      (check-true (hash-has-key? payload 'workingSetEntries))
      (check-true (hash-has-key? payload 'workingSetTokens))
      (check-equal? (hash-ref payload 'workingSetEntries) (working-set-entry-count ws))
      (check-equal? (hash-ref payload 'workingSetTokens) (working-set-token-count ws))
      (check-true (<= (hash-ref payload 'workingSetTokens)
                      (compute-working-set-budget (hash-ref payload 'tokenCount)))))

    (test-case "final context hook amendment reconciles WS state and provider telemetry"
      (define bus (make-event-bus))
      (define collected (make-event-collector bus))
      (define ws (make-working-set))
      (define ws-msg
        (make-message "ws-final-hook"
                      #f
                      'tool
                      'tool-result
                      (list (make-tool-result-part "ws-call" "working data" #f))
                      (current-seconds)
                      (hasheq 'toolCallId "ws-call" 'isError #f)))
      (working-set-add! ws "/tmp/ws.rkt" (message-id ws-msg) 20)
      (define user-msg
        (make-message "user-final-hook"
                      #f
                      'user
                      'message
                      (list (make-text-part "hello"))
                      (current-seconds)
                      (hasheq)))
      (define ext-reg (make-extension-registry))
      (register-extension! ext-reg
                           (extension "final-context-test"
                                      "0.1"
                                      "1.0"
                                      (hasheq 'context
                                              (lambda (messages)
                                                (hook-amend (filter (lambda (m)
                                                                      (not (equal? (message-id m)
                                                                                   "ws-final-hook")))
                                                                    messages))))))
      (define config
        (hash->session-config (hasheq 'working-set
                                      ws
                                      'project-dir
                                      "/tmp/project"
                                      'tier-b-count
                                      5
                                      'tier-c-count
                                      1
                                      'max-tokens
                                      10000)))
      (define final
        (build-assembled-context (list ws-msg user-msg) config ext-reg bus "final-hook" 0))
      (check-false (member "ws-final-hook" (map message-id final)))
      (check-equal? (working-set-entry-count ws) 0)
      (define detail (event-payload (first (events-with-name collected "context.assembly.detail"))))
      (define assembled (event-payload (first (events-with-name collected "context.assembled"))))
      (check-equal? (hash-ref detail 'wsEntryCount) 0)
      (check-equal? (hash-ref detail 'wsTokens) 0)
      (check-equal? (hash-ref assembled 'workingSetEntries) 0)
      (check-equal? (hash-ref assembled 'workingSetTokens) 0)
      (check-equal? (hash-ref assembled 'assembledMessages) (length final))
      (check-equal? (hash-ref assembled 'tokenCount) (estimate-context-tokens final)))

    (test-case "final hook omission retains GSD-pinned WS authority state"
      (define bus (make-event-bus))
      (define ws (make-working-set))
      (define gsd-msg
        (make-message "gsd-authority"
                      #f
                      'tool
                      'tool-result
                      (list (make-tool-result-part "gsd-call" "plan" #f))
                      (current-seconds)
                      (hasheq 'gsd-pin #t 'toolCallId "gsd-call" 'isError #f)))
      (working-set-add! ws "/tmp/PLAN.md" (message-id gsd-msg) 5)
      (define user-msg
        (make-message "gsd-user"
                      #f
                      'user
                      'message
                      (list (make-text-part (make-string 1000 #\u)))
                      (current-seconds)
                      (hasheq)))
      (define ext-reg (make-extension-registry))
      (register-extension! ext-reg
                           (extension "gsd-authority-test"
                                      "0.1"
                                      "1.0"
                                      (hasheq 'context
                                              (lambda (messages)
                                                (hook-amend (filter (lambda (m)
                                                                      (not (equal? (message-id m)
                                                                                   "gsd-authority")))
                                                                    messages))))))
      (define config (hash->session-config (hasheq 'working-set ws 'max-tokens 10000)))
      (define final (build-assembled-context (list gsd-msg user-msg) config ext-reg bus "gsd-hook" 0))
      (check-false (member "gsd-authority" (map message-id final)))
      (check-equal? (map ws-entry-message-id (working-set-entries ws)) '("gsd-authority")))

    ;; ── T02: working-set.update event after tool execution ──
    (test-case "T02: working-set.update emitted after read tool execution"
      (define dir (make-temp-dir))
      (define bus (make-event-bus))
      (define collected (make-event-collector bus))
      (define reg (make-tool-registry))
      (register-tool! reg read-dummy-tool)
      (define prov (make-tool-call-mock-provider "read" (hasheq 'path "/tmp/a.rkt") "done"))
      (define cfg (make-test-config dir bus prov reg))
      (define sess (make-agent-session cfg))
      (run-prompt! sess "hello" #:max-iterations 10)
      (define update-events (events-with-name collected "working-set.update"))
      (check-true (>= (length update-events) 1)
                  "working-set.update should be emitted after read tool execution")
      (define payload (event-payload (first update-events)))
      (check-true (hash-has-key? payload 'entry-count))
      (check-true (hash-has-key? payload 'token-count))
      (check-true (hash-has-key? payload 'paths))
      (check-true (hash-has-key? payload 'budget-actions))
      (delete-directory/files dir))

    ;; ── T03: read-spiral detected on consecutive same-path reads ──
    (test-case "T03: read-spiral detected when same path is read twice"
      (define dir (make-temp-dir))
      (define bus (make-event-bus))
      (define collected (make-event-collector bus))
      (define reg (make-tool-registry))
      (register-tool! reg read-dummy-tool)
      ;; Provider: read /tmp/a.rkt, then read /tmp/a.rkt again, then text
      (define prov
        (make-multi-mock-provider
         (list (make-read-response "/tmp/a.rkt") (make-read-response "/tmp/a.rkt") text-response)))
      (define cfg (make-test-config dir bus prov reg))
      (define sess (make-agent-session cfg))
      (run-prompt! sess "hello" #:max-iterations 10)
      (define spiral-events (events-with-name collected "working-set.read-spiral-detected"))
      (check-true (>= (length spiral-events) 1)
                  "read-spiral should be detected on second read of same path")
      (define payload (event-payload (first spiral-events)))
      (check-true (hash-has-key? payload 'paths))
      (check-true (hash-has-key? payload 'count))
      (define update-events (events-with-name collected "working-set.update"))
      (define final-actions (hash-ref (event-payload (last update-events)) 'budget-actions '()))
      (check-true (for/or ([action (in-list final-actions)])
                    (eq? (hash-ref action 'action #f) 'superseded)))
      (delete-directory/files dir))

    ;; ── T04: edit removes entry and prevents read-spiral ──
    (test-case "T04: edit removes working-set entry and avoids false spiral"
      (define dir (make-temp-dir))
      (define bus (make-event-bus))
      (define collected (make-event-collector bus))
      (define reg (make-tool-registry))
      (register-tool! reg read-dummy-tool)
      (register-tool! reg edit-dummy-tool)
      ;; Provider: read /tmp/a.rkt, then edit /tmp/a.rkt, then text
      (define prov
        (make-multi-mock-provider
         (list (make-read-response "/tmp/a.rkt") (make-edit-response "/tmp/a.rkt") text-response)))
      (define cfg
        (hash-set (make-test-config dir bus prov reg)
                  'permission-config
                  (make-permissive-permission-config)))
      (define sess (make-agent-session cfg))
      (run-prompt! sess "hello" #:max-iterations 10)
      (define spiral-events (events-with-name collected "working-set.read-spiral-detected"))
      ;; No read-spiral should be detected because successful edit consumed the entry
      (check-equal? (length spiral-events)
                    0
                    "edit should remove ws entry and prevent read-spiral detection")
      (define update-events (events-with-name collected "working-set.update"))
      ;; Should have at least 2 updates (after read and after edit)
      (check-true (>= (length update-events) 2))
      ;; After edit, entry count should be 0
      (define last-update-payload (event-payload (last update-events)))
      (check-equal? (hash-ref last-update-payload 'entry-count 999) 0)
      (delete-directory/files dir))))

(module+ main
  (run-tests iteration-observability-tests))
