#lang racket

;; BOUNDARY: integration

;; tests/test-iteration-observability.rkt — Working set observability + read-spiral detection

(require rackunit
         rackunit/text-ui
         racket/list
         racket/file
         "../runtime/working-set.rkt"
         (only-in "../runtime/iteration/main-loop.rkt" run-iteration-loop)
         "../runtime/turn-orchestrator.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session-types.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../util/ids.rkt"
         (only-in "../tools/tool.rkt" make-tool make-tool-registry register-tool! make-success-result)
         (only-in "helpers/mock-provider.rkt"
                  make-multi-mock-provider
                  make-tool-call-mock-provider
                  make-test-config))

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
   (hasheq 'type "object" 'properties (hasheq 'path (hasheq 'type "string")) 'required '(path))
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
                     '(path old-text new-text))
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
      (define config (hasheq 'working-set ws 'tier-b-count 5 'tier-c-count 1 'max-tokens 10000))
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
      (check-true (hash-has-key? payload 'working-set-entries))
      (check-true (hash-has-key? payload 'working-set-tokens))
      (check-equal? (hash-ref payload 'working-set-entries) 1)
      (check-true (>= (hash-ref payload 'working-set-tokens) 0)))

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
      (define cfg (make-test-config dir bus prov reg))
      (define sess (make-agent-session cfg))
      (run-prompt! sess "hello" #:max-iterations 10)
      (define spiral-events (events-with-name collected "working-set.read-spiral-detected"))
      ;; No read-spiral should be detected because edit consumed the entry
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
(module+ test
  (run-tests iteration-observability-tests))
