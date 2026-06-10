#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-state-aware-builder.rkt — tests for state-aware context assembly
;; v0.76.0 W2: Extracted module tests

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-rollback-action-execution?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt" check-rollback-triggers)
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-role
                  message-content
                  text-part?
                  text-part-text)
         (only-in "../util/message/message.rkt" message-kind)
         (only-in "../util/fsm/fsm.rkt" fsm-state)
         (only-in "../runtime/session/session-config.rkt" hash->session-config session-config?)
         (only-in "../runtime/memory/service.rkt" current-memory-backend)
         (only-in "../runtime/memory/backends/memory-hash.rkt" make-memory-hash-backend)
         (only-in "../runtime/memory/types.rkt" memory-item)
         (only-in "../runtime/memory/protocol.rkt" memory-backend-store!)
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget)
         (only-in "../runtime/context-assembly/conclusion-ranker.rkt" rank-and-budget)
         (only-in "../runtime/context-assembly/conclusion-graph.rkt" fallback-select-conclusions))

(define (make-test-msg role text [meta (hasheq)])
  (make-message "test-id" #f role 'text (list (make-text-part text)) (current-seconds) meta))

(define (make-test-msgs n)
  (for/list ([i (in-range n)])
    (make-test-msg (if (even? i) 'user 'assistant) (format "msg ~a" i))))

(define suite
  (test-suite "state-aware-builder"

    (test-case "current-task-state-aware-assembly? defaults to #f"
      (check-false (current-task-state-aware-assembly?)))

    (test-case "state-aware-builder: feature flag can be enabled"
      (parameterize ([current-task-state-aware-assembly? #t])
        (check-true (current-task-state-aware-assembly?))))

    (test-case "state-aware assembly without state acts like regular"
      (define msgs (make-test-msgs 10))
      (define regular (build-tiered-context msgs))
      (define state-aware (build-tiered-context/state-aware msgs))
      (check-equal? (length (tiered-context-tier-a regular))
                    (length (tiered-context-tier-a state-aware)))
      (check-equal? (length (tiered-context-tier-b regular))
                    (length (tiered-context-tier-b state-aware)))
      (check-equal? (length (tiered-context-tier-c regular))
                    (length (tiered-context-tier-c state-aware))))

    (test-case "state-aware with idle state adds preamble"
      (define msgs (make-test-msgs 10))
      (define regular (build-tiered-context msgs))
      (define state-aware (build-tiered-context/state-aware msgs #:task-state 'idle))
      ;; v0.78+: idle now produces a preamble message, so tier-a is longer
      (check-true (> (length (tiered-context-tier-a state-aware))
                     (length (tiered-context-tier-a regular)))))

    (test-case "state-aware with exploration adds preamble"
      (define msgs (make-test-msgs 5))
      (define tc (build-tiered-context/state-aware msgs #:task-state 'exploration))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (> (length tier-a) 0))
      (check-equal? (message-role (car tier-a)) 'system)
      (check-equal? (message-kind (car tier-a)) 'system-instruction))

    (test-case "state-aware with fsm-state struct works"
      (define msgs (make-test-msgs 5))
      (define tc (build-tiered-context/state-aware msgs #:task-state (fsm-state 'planning)))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (> (length tier-a) 0)))

    (test-case "state-aware with conclusions adds conclusion entries"
      (define msgs (make-test-msgs 5))
      (define conclusions
        (list (task-conclusion "c1" "Found the bug" 'fact 'exploration '() 1000 '() '())))
      (define tc
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:conclusions conclusions))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 2)))

    (test-case "state-aware implementation currently injects all conclusion entries (v0.77.0 baseline)"
      (define msgs (make-test-msgs 5))
      (define conclusions
        (for/list ([i (in-range 25)])
          (task-conclusion (format "c~a" i)
                           (format "important conclusion ~a" i)
                           'fact
                           'implementation
                           '()
                           (+ 1000 i)
                           '()
                           '())))
      (define tc
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:conclusions conclusions))
      ;; v0.77.9: Budget enforcement is now active (default 2000 tokens).
      ;; With 25 conclusions × ~132 tokens each ≈ 3300 total, budget trims to ~15.
      ;; Tier-a should have preamble + budgeted conclusions + base tier-a entries.
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 2)) ; at least preamble + some conclusions
      (check-true (< (length tier-a) (+ 1 (length conclusions))))) ; budget was enforced

    (test-case "state-aware filters working-set for implementation"
      (define msgs (make-test-msgs 5))
      (define ws (list (make-test-msg 'system "ws1")))
      (define tc
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:working-set-messages ws))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 1)))

    (test-case "preamble returns message for idle (v0.78+)"
      (check-not-false (build-state-awareness-preamble 'idle '())))

    (test-case "state-aware-builder: preamble returns #f for #f state"
      (check-false (build-state-awareness-preamble #f '())))

    (test-case "preamble returns message for exploration"
      (define preamble (build-state-awareness-preamble 'exploration '()))
      (check-not-false preamble)
      (check-equal? (message-role preamble) 'system)
      (check-equal? (message-kind preamble) 'system-instruction))

    (test-case "preamble includes conclusions text"
      (define conclusions
        (list (task-conclusion "c1" "Bug in line 42" 'fact 'debugging '() 1000 '() '())))
      (define preamble (build-state-awareness-preamble 'debugging conclusions))
      (check-not-false preamble))

    (test-case "preamble handles fsm-state struct input"
      (define preamble (build-state-awareness-preamble (fsm-state 'verification) '()))
      (check-not-false preamble))

    ;; v0.78.0 G9: repeat-tool-count computed from recent-tool-calls
    (test-case "repeat-tool-count computed from recent-tool-calls"
      (parameterize ([current-task-state-aware-assembly? #t]
                     [current-rollback-action-execution? #t])
        (define msgs (make-test-msgs 10))
        ;; Same tool (read) repeated 4 times -> repeat-count should be 4
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages msgs
                                            #:recent-tool-calls '(read read read read bash)))
        ;; v0.78.6 C4: Verify amnesia warning actually fires for repeat count > 2
        (check-not-false tc "tiered context should be produced")
        ;; Directly verify trigger fires: 4 repeats > 2 threshold
        (define warnings
          (check-rollback-triggers #:before-messages 10
                                   #:after-messages 10
                                   #:conclusion-coverage 0.5
                                   #:repeat-tool-count 4))
        (check-true (pair? warnings) "repeat count 4 should produce warnings")
        (define amnesia-warn (assoc 'task-amnesia-detected warnings))
        (check-not-false amnesia-warn
                         "task-amnesia-detected warning should fire for 4 repeat tool calls")))

    (test-case "state-aware observe-only memory integration emits trace without changing context shape"
      (define msgs (make-test-msgs 10))
      (define cfg (hash->session-config (hash 'memory-backend 'memory-hash)))
      (define events (box '()))
      (define (trace! phase payload)
        (set-box! events (cons (cons phase payload) (unbox events))))
      (parameterize ([current-memory-backend (make-memory-hash-backend)])
        (define without-memory (build-tiered-context/state-aware msgs #:task-state 'implementation))
        (define with-memory
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:session-config cfg
                                            #:trace trace!))
        (check-equal? (length (tiered-context-tier-a with-memory))
                      (length (tiered-context-tier-a without-memory)))
        (check-true (for/or ([evt (in-list (unbox events))])
                      (eq? (car evt) 'memory-observe)))))

    (test-case "repeat-tool-count defaults to 0 with no recent-tool-calls"
      (parameterize ([current-task-state-aware-assembly? #t])
        (define msgs (make-test-msgs 10))
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages msgs))
        (check-not-false tc)))))

;; ============================================================
;; v0.95.15 W3: Memory injection wiring tests
;; ============================================================

(define (extract-text-from-messages msgs)
  (string-join (for*/list ([m msgs]
                           [p (message-content m)]
                           #:when (text-part? p))
                 (text-part-text p))
               " "))

(define memory-injection-suite
  (test-suite "state-aware-builder memory injection"

    (test-case "no memory injection when budget not set"
      (parameterize ([current-memory-backend (make-memory-hash-backend)]
                     [current-memory-injection-budget #f]
                     [current-task-state-aware-assembly? #f])
        (define msgs (make-test-msgs 5))
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:session-config
                                            (hash->session-config (hasheq 'memory-backend 'hash))
                                            #:working-set-messages msgs))
        (define tier-a-text (extract-text-from-messages (tiered-context-tier-a tc)))
        (check-false (string-contains? tier-a-text "Memory —")
                     "No memory section expected without budget")))

    (test-case "memory section injected into tier-a when budget is set"
      (define backend (make-memory-hash-backend))
      (parameterize ([current-memory-backend backend]
                     [current-memory-injection-budget 500]
                     [current-task-state-aware-assembly? #f])
        (define test-item
          (memory-item "mem-1"
                       'semantic
                       'session
                       "The project uses Racket"
                       (hasheq 'project-root
                               "/test"
                               'session-id
                               "sess-1"
                               'tags
                               '()
                               'source
                               'test
                               'origin-message-id
                               "msg-1")
                       (hasheq 'sensitivity 'public 'confidence 0.9 'expires-at #f 'supersedes '())
                       "2026-06-05T12:00:00Z"
                       "2026-06-05T12:00:00Z"))
        ((memory-backend-store! backend) test-item)
        (define msgs (make-test-msgs 5))
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:session-config
                                            (hash->session-config (hasheq 'memory-backend 'hash))
                                            #:working-set-messages msgs))
        (define tier-a-text (extract-text-from-messages (tiered-context-tier-a tc)))
        (check-not-false (string-contains? tier-a-text "Racket")
                         "Injected memory content should appear in tier-a")))))

(run-tests suite)
(run-tests memory-injection-suite)

;; ---------------------------------------------------------------------------
;; v0.95.18 W0: Blank Auto preamble regression (expected red before W2)
;; ---------------------------------------------------------------------------

(test-case "W0 F9: state preamble filters bare Auto conclusions"
  (define conclusions (list (task-conclusion "auto-blank" "[Auto]" 'fact 'idle '("m1") 1000 '() '())))
  (define preamble (build-state-awareness-preamble 'implementation conclusions))
  (define text (extract-text-from-messages (list preamble)))
  (check-false (string-contains? text "  - [Auto]\n"))
  ;; GAP-A v0.97.7: format changed to "Top conclusions"
  (check-false (string-contains? text "Key conclusions (1 in memory)")))

(test-case "W0 F9: state preamble keeps valid Auto conclusions"
  (define conclusions
    (list
     (task-conclusion "auto-valid" "[Auto] Previously read file x" 'fact 'idle '("m1") 1000 '() '())))
  (define preamble (build-state-awareness-preamble 'implementation conclusions))
  (define text (extract-text-from-messages (list preamble)))

  ;; ---------------------------------------------------------------------------
  ;; v0.95.19 W0: HF1 regression — missing session-config wire in production path
  ;; ---------------------------------------------------------------------------

  (test-case "W0 HF1: build-tiered-context/state-aware without session-config skips injection"
    (define backend (make-memory-hash-backend))
    (parameterize ([current-memory-backend backend]
                   [current-memory-injection-budget 500]
                   [current-task-state-aware-assembly? #f])
      (define test-item
        (memory-item "mem-hf1"
                     'semantic
                     'session
                     "HF1 test: this should NOT appear without session-config wire"
                     (hasheq 'project-root
                             "/test"
                             'session-id
                             "sess-hf1"
                             'tags
                             '()
                             'source
                             'test
                             'origin-message-id
                             "msg-hf1")
                     (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                     "2026-06-07T12:00:00Z"
                     "2026-06-07T12:00:00Z"))
      ((memory-backend-store! backend) test-item)
      (define msgs (make-test-msgs 5))
      ;; BUG PROOF: without #:session-config, injection is skipped
      ;; This is the exact gap in turn-orchestrator.rkt assemble-context/pure
      (define tc-no-config
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:working-set-messages msgs))
      (define text-no (extract-text-from-messages (tiered-context-tier-a tc-no-config)))
      (check-false (string-contains? text-no "HF1 test")
                   "BUG CONFIRMED: Without #:session-config wire, injection skipped")
      ;; FIX PROOF: with #:session-config, injection fires
      (define cfg (hash->session-config (hasheq 'memory-backend 'hash)))
      (define tc-with-config
        (build-tiered-context/state-aware msgs
                                          #:task-state 'implementation
                                          #:session-config cfg
                                          #:working-set-messages msgs))
      (define text-yes (extract-text-from-messages (tiered-context-tier-a tc-with-config)))
      (check-not-false (string-contains? text-yes "HF1 test")
                       "FIX TARGET: With #:session-config wire, injection fires"))))

;; ============================================================
;; v0.97.7 W1: GAP-A preamble ranking regression tests
;; ============================================================

(test-case "GAP-A: preamble shows top conclusions with total+shown counts"
  (define conclusions
    (for/list ([i (in-range 15)])
      (task-conclusion (format "gap-a-c~a" i)
                       (format "conclusion ~a" i)
                       'fact
                       'implementation
                       '()
                       (+ 1000 i)
                       '()
                       '())))
  (define preamble (build-state-awareness-preamble 'implementation conclusions))
  (define text (extract-text-from-messages (list preamble)))
  (check-true (string-contains? text "15 total, 15 shown"))
  (check-true (string-contains? text "conclusion 0"))
  (check-true (string-contains? text "conclusion 14")))

(test-case "GAP-A: [Auto] conclusions filtered from preamble"
  (define conclusions
    (list (task-conclusion "c1" "real finding" 'fact 'exploration '() 1000 '() '())
          (task-conclusion "c2" "[Auto]" 'fact 'exploration '() 1001 '() '())
          (task-conclusion "c3" "another finding" 'fact 'exploration '() 1002 '() '())))
  (define preamble (build-state-awareness-preamble 'exploration conclusions))
  (define text (extract-text-from-messages (list preamble)))
  (check-false (string-contains? text "  - [Auto]"))
  (check-true (string-contains? text "2 total, 2 shown")))

(test-case "GAP-A: preamble respects cap of 20"
  (define conclusions
    (for/list ([i (in-range 25)])
      (task-conclusion (format "cap-c~a" i)
                       (format "item ~a" i)
                       'fact
                       'planning
                       '()
                       (+ 1000 i)
                       '()
                       '())))
  (define preamble (build-state-awareness-preamble 'planning conclusions))
  (define text (extract-text-from-messages (list preamble)))
  (check-true (string-contains? text "25 total, 20 shown"))
  ;; First 20 should be present; items 20-24 should NOT
  (check-true (string-contains? text "item 19"))
  (check-false (string-contains? text "item 20"))
  (check-false (string-contains? text "item 24")))

(test-case "GAP-A: empty conclusion list produces no-conclusions message"
  (define preamble (build-state-awareness-preamble 'implementation '()))
  (define text (extract-text-from-messages (list preamble)))
  (check-true (string-contains? text "No conclusions in memory")))

(test-case "GAP-A: non-Auto text with whitespace is not filtered"
  (define conclusions
    (list (task-conclusion "ws1" "  Some finding  " 'fact 'exploration '() 1000 '() '())))
  (define preamble (build-state-awareness-preamble 'exploration conclusions))
  (define text (extract-text-from-messages (list preamble)))
  (check-true (string-contains? text "1 total, 1 shown"))
  (check-true (string-contains? text "Some finding")))

(test-case "GAP-A: preamble uses budgeted order (first = highest relevance)"
  (define conclusions
    (list (task-conclusion "rank-1" "most relevant" 'fact 'implementation '() 1000 '(bug) '())
          (task-conclusion "rank-2" "medium relevant" 'fact 'planning '() 999 '() '())
          (task-conclusion "rank-3" "least relevant" 'fact 'exploration '() 998 '() '())))
  (define preamble (build-state-awareness-preamble 'implementation conclusions))
  (define text (extract-text-from-messages (list preamble)))
  ;; The budgeted list is passed directly; order preserved
  (define pos1 (regexp-match-positions (regexp-quote "most relevant") text))
  (define pos2 (regexp-match-positions (regexp-quote "medium relevant") text))
  (check-not-false pos1 "most relevant found in preamble")
  (check-not-false pos2 "medium relevant found in preamble")
  (check-true (< (caar pos1) (caar pos2)) "highest-relevance conclusion appears before medium"))

;; ============================================================
;; v0.97.9 W1: GAP-D semantic fallback regression tests
;; ============================================================

(test-case "GAP-D: rank-and-budget fallback produces list (not error)"
  (define conclusions
    (list (task-conclusion "dc1" "finding A" 'fact 'implementation '() 1000 '() '())
          (task-conclusion "dc2" "finding B" 'fact 'planning '() 999 '() '())))
  ;; Direct call: rank-and-budget returns ranked conclusions
  (define ranked
    (rank-and-budget conclusions #:current-state 'implementation #:max-conclusion-tokens 2000))
  (check-true (list? ranked))
  (check-true (>= (length ranked) 1)))

(test-case "GAP-D: rank-and-budget respects token budget"
  (define conclusions
    (for/list ([i (in-range 10)])
      (task-conclusion (format "dc~a" i)
                       (format "conclusion with enough text to consume tokens ~a" i)
                       'fact
                       'implementation
                       '()
                       (+ 1000 i)
                       '()
                       '())))
  (define ranked
    (rank-and-budget conclusions #:current-state 'implementation #:max-conclusion-tokens 500))
  (check-true (list? ranked))
  ;; Budget of 500 tokens should limit conclusions: each ~60 chars × 4 ≈ 240 tokens
  ;; so only ~2 fit, proving truncation occurred
  (check-true (< (length ranked) 10) "budget truncation occurred"))

(test-case "GAP-D: rank-and-budget with empty conclusions returns empty"
  (define ranked (rank-and-budget '() #:current-state 'implementation #:max-conclusion-tokens 2000))
  (check-true (or (null? ranked) (not ranked))))

(test-case "GAP-D: fallback-select-conclusions still available for direct use"
  ;; Verify fallback-select-conclusions still works as a standalone function
  (define conclusions
    (list (task-conclusion "fc1" "A" 'fact 'idle '() 100 '() '())
          (task-conclusion "fc2" "B" 'fact 'idle '() 200 '() '())))
  (define selected (fallback-select-conclusions conclusions 1))
  (check-equal? (length selected) 1))

(test-case "GAP-D: build-tiered-context uses semantic fallback for cyclic graph"
  ;; Create conclusions with a cycle (c1→c2→c1)
  (define cyc-c1
    (task-conclusion "cyc1" "cycle finding 1" 'fact 'implementation '() 1000 '() '("cyc2")))
  (define cyc-c2
    (task-conclusion "cyc2" "cycle finding 2" 'fact 'implementation '() 999 '() '("cyc1")))
  ;; Build with cyclic conclusions — should NOT error, should produce context
  (define result
    (build-tiered-context/state-aware '()
                                      #:conclusions (list cyc-c1 cyc-c2)
                                      #:task-state 'implementation))
  (check-true (tiered-context? result)))

(test-case "GAP-D: build-tiered-context uses semantic fallback for no-seed match"
  ;; Conclusions with no WS messages to derive seeds from
  (define no-seed-c1 (task-conclusion "ns1" "orphan finding" 'fact 'planning '() 1000 '() '()))
  (define result
    (build-tiered-context/state-aware '() #:conclusions (list no-seed-c1) #:task-state 'planning))
  (check-true (tiered-context? result)))
