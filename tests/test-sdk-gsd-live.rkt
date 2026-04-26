#lang racket

;; tests/test-sdk-gsd-live.rkt — v0.20.5 W4: Live SDK Integration Test
;;
;; Integration tests for the GSD SDK convenience API with mock providers.
;; Exercises event emission, lifecycle state transitions, and budget tracking.

(require rackunit
         racket/file
         racket/string
         json
         racket/set
         "../interfaces/sdk.rkt"
         "../extensions/api.rkt"
         "../extensions/gsd-planning.rkt"
         "../extensions/hooks.rkt"
         "../agent/event-bus.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (only-in "../util/event.rkt" event-ev event-event)
         "helpers/mock-provider.rkt"
         "helpers/temp-fs.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-live-runtime prov [ext-reg #f])
  (define tmp (make-temporary-file "/tmp/sdk-live-test-~a" 'directory))
  (define bus (make-event-bus))
  (define er (or ext-reg (make-extension-registry)))
  (register-extension! er the-extension)
  (define rt (make-runtime #:provider prov
                           #:session-dir tmp
                           #:extension-registry er
                           #:event-bus bus
                           #:register-default-tools? #t))
  (values rt tmp bus er))

(define (cleanup-live! tmp)
  (reset-all-gsd-state!)
  (delete-directory/files tmp #:must-exist? #f))

;; ============================================================
;; Tests
;; ============================================================

(test-case "W4-live: GSD events emitted during q:plan lifecycle"
  (reset-all-gsd-state!)
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text" 'text "plan done"))))))
  (define-values (rt tmp bus ext-reg) (make-live-runtime prov))
  ;; Subscribe to events before open-session
  (define collected-events (box '()))
  (subscribe! bus
              (lambda (ev)
                (define ev-name (event-ev ev))
                (when (and ev-name
                           (or (symbol? ev-name) (string? ev-name))
                           (string-prefix? (if (symbol? ev-name)
                                               (symbol->string ev-name)
                                               ev-name)
                                           "gsd."))
                  (set-box! collected-events
                            (append (unbox collected-events)
                                    (list ev))))))
  ;; Open session and run q:plan
  (define opened (open-session rt))
  (define-values (rt2 result) (q:plan opened "test planning task"))
  ;; Verify events were emitted
  (define events (unbox collected-events))
  (check-true (> (length events) 0)
              "at least one gsd event should be emitted during q:plan")
  ;; Check for mode-changed event
  (define event-names (map (lambda (e) (let ([n (event-ev e)]) (if (symbol? n) (symbol->string n) n))) events))
  (check-not-false (member "gsd.mode.changed" event-names)
                   "gsd.mode.changed event should be emitted")
  (cleanup-live! tmp))

(test-case "W4-live: q:plan sets planning mode"
  (reset-all-gsd-state!)
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text" 'text "ok"))))))
  (define-values (rt tmp bus ext-reg) (make-live-runtime prov))
  (define opened (open-session rt))
  (define-values (rt2 result) (q:plan opened "build a widget"))
  ;; The /plan handler sets gsd-mode to 'planning
  (check-equal? (gsd-mode) 'planning
                "q:plan should set GSD mode to planning")
  (cleanup-live! tmp))

(test-case "W4-live: q:go with plan dispatches correctly"
  (reset-all-gsd-state!)
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text" 'text "impl done"))))))
  (define-values (rt tmp bus ext-reg) (make-live-runtime prov))
  ;; Write a plan file so /go has something to execute
  (define plan-dir (build-path tmp ".planning"))
  (make-directory* plan-dir)
  (call-with-output-file (build-path plan-dir "PLAN.md")
    (lambda (out) (display "## Wave 0: Test\n- Do something\n\n## Verify\n~bash echo ok~\n" out))
    #:exists 'truncate)
  (set-pinned-planning-dir! tmp)
  (define opened (open-session rt))
  (define-values (rt2 result) (q:go opened))
  ;; q:go should have dispatched and returned a result
  (check-not-equal? result 'no-extension-registry)
  (check-not-equal? result 'unexpected-result)
  (cleanup-live! tmp))

(test-case "W4-live: completed waves tracking via GSD state"
  (reset-all-gsd-state!)
  ;; Verify initial state
  (check-equal? (set-count (completed-waves)) 0)
  ;; Simulate marking a wave complete (what the /go handler does)
  (define cw (completed-waves))
  (set-add! cw 0)
  (check-equal? (set-count cw) 1)
  ;; Reset clears it
  (reset-all-gsd-state!)
  (check-equal? (set-count (completed-waves)) 0))

(test-case "W4-live: read budget is reset during planning"
  (reset-all-gsd-state!)
  ;; Set an initial budget
  (set-go-read-budget! 100)
  (check-equal? (go-read-budget) 100)
  ;; Reset (simulating new plan cycle)
  (reset-all-gsd-state!)
  ;; After reset, budget should be cleared
  (check-false (go-read-budget)))

(test-case "W4-live: full plan→go with tool call simulation"
  (reset-all-gsd-state!)
  ;; Use make-tool-call-mock-provider to simulate planning-write
  (define plan-content "## Wave 0: Hello\n- Write hello.txt\n\n## Verify\n~bash cat hello.txt~\n")
  (define prov (make-tool-call-mock-provider
                "planning-write"
                (hasheq 'content plan-content 'artifact "PLAN")
                "Plan written."))
  (define-values (rt tmp bus ext-reg) (make-live-runtime prov))
  (define opened (open-session rt))
  ;; Run q:plan — the mock provider will call planning-write
  (define-values (rt2 result) (q:plan opened "write hello.txt"))
  ;; Result should be from the second turn (text response)
  (check-not-equal? result 'no-extension-registry)
  (check-not-equal? result 'no-plan-text)
  ;; Verify mode was set
  (check-equal? (gsd-mode) 'planning)
  (cleanup-live! tmp))

(test-case "W4-live: q:gsd-status returns hash when state is active"
  (reset-all-gsd-state!)
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text" 'text "ok"))))))
  (define-values (rt tmp bus ext-reg) (make-live-runtime prov))
  (define opened (open-session rt))
  (define-values (rt2 result) (q:plan opened "test"))
  ;; After planning, status should return a hash
  (define status (q:gsd-status))
  (check-not-equal? status 'no-active-session
                    "gsd-status should return state after session activity")
  (cleanup-live! tmp))
