#lang racket/base

;; @speed fast
;; @suite arch

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string)

(define-runtime-path tests-dir ".")
(define root (simplify-path (build-path tests-dir "..")))
(define ledger-path (build-path root "docs" "architecture" "session-lifecycle-trace-v0.99.92.rktd"))
(define report-path
  (build-path root "docs" "reports" "SESSION-LIFECYCLE-TRACE-RESPONSIBILITY-MAP-v0.99.92.md"))

(define expected-families '(normal error cancel close retry compaction))
(define expected-responsibilities '(orchestration pure-preparation persistence eventing fsm wiring))
(define expected-path-ids
  '(normal-success handled-error
                   error-then-index-failure
                   cancel-pre-iteration
                   cancel-midstream
                   close-normal
                   close-repeated
                   close-active-prompt
                   retry-success
                   retry-exhausted
                   retry-exhausted-partial
                   retry-held-circuit
                   retry-progressive-circuit
                   retry-health-gate
                   retry-adaptive
                   retry-partial-recovery
                   compact-auto-success
                   compact-auto-hook-block
                   compact-auto-start-failure
                   compact-midturn
                   compact-manual-completed
                   compact-manual-nothing
                   compact-manual-failed
                   compact-manual-tracer-failure
                   compact-manual-contention
                   hook-input-block
                   hook-before-agent-block
                   hook-turn-start-block
                   hook-model-request-block
                   hook-message-start-block
                   hook-message-end-block))
(define expected-consumer-ids
  '(sdk-run goal-run
            tui-campaign
            tui-normal
            cli-runners
            rpc-prompt
            rpc-default-prompt
            gui-campaign
            gui-submit
            gui-new-session
            gui-input
            prompt-internal
            prompt-context
            prompt-dispatch
            dispatch-loop
            rpc-close
            gui-slash-close
            gui-close
            provider-retry
            auto-retry
            policy-retry
            subagent-retry
            prompt-compaction
            midturn-compaction
            durable-compaction-event
            sdk-compat
            goal-callback))
(define expected-exit-ids
  '(closed-guard busy-event-failure
                 begin-turn-failure
                 outer-start-failure
                 input-hook-failure
                 input-hook-block
                 context-persistence-failure
                 context-event-failure
                 model-select-failure
                 tracer-construction-failure
                 tracer-start-failure
                 provider-generator-failure
                 dispatch-handler-failure
                 normal-tracer-stop-failure
                 index-rebuild-failure
                 session-updated-failure
                 rollback-save-back-failure
                 finish-turn-failure
                 release-prompt-failure
                 acknowledgement-tracer-failure
                 cleanup-terminal-failure
                 emergency-persist-failure
                 interrupt-accepted-publication-failure
                 interrupt-signal-failure
                 retry-callback-failure
                 retry-sleep-break
                 retry-partial-metadata-loss
                 auto-compaction-start-event-failure
                 auto-compaction-hook-block
                 auto-compaction-body-failure
                 auto-compaction-cleanup-failure
                 manual-compaction-contention-event-failure
                 manual-compaction-tracer-construction-failure
                 manual-compaction-failed-event-failure
                 repeated-close
                 active-prompt-close
                 close-cleanup-group-failure
                 non-exn-break-kill))

(define (read-one path)
  (call-with-input-file path
                        (lambda (in)
                          (define datum (read in))
                          (check-true (eof-object? (read in))
                                      "lifecycle ledger must contain exactly one datum")
                          datum)))

(define (locator-parts locator)
  (string-split locator ":" #:trim? #f))

(define (locator-path locator)
  (car (locator-parts locator)))

(define (locator-anchor locator)
  (string-join (cdr (locator-parts locator)) ":"))

(define (anchor-positions text anchor)
  (regexp-match-positions* (regexp (regexp-quote anchor)) text))

(define (check-locator id locator #:unique? [unique? #f])
  (check-true (string? locator) (format "~a locator must be a string" id))
  (define path (build-path root (locator-path locator)))
  (check-true (file-exists? path) (format "~a evidence file absent: ~a" id locator))
  (define anchor (locator-anchor locator))
  (check-false (string=? anchor "") (format "~a locator needs an anchor" id))
  (define positions (anchor-positions (file->string path) anchor))
  (check-true (pair? positions) (format "~a evidence anchor absent: ~a" id locator))
  (when unique?
    (check-equal? (length positions) 1 (format "~a anchor must be unique: ~a" id locator))))

(define (exact-id-bijection label expected entries)
  (define ids (map (lambda (entry) (hash-ref entry 'id)) entries))
  (check-equal? (sort ids symbol<?) (sort expected symbol<?) label)
  (check-equal? (length ids) (length (remove-duplicates ids)) (format "~a IDs must be unique" label)))

(define (first-anchor-position locator)
  (define text (file->string (build-path root (locator-path locator))))
  (caar (anchor-positions text (locator-anchor locator))))

(test-case "W0-1: ledger freezes evidence-only scope and exact expanded path taxonomy"
  (define ledger (read-one ledger-path))
  (check-equal? (hash-ref ledger 'schema-version) 2)
  (check-eq? (hash-ref ledger 'milestone) 'v0.99.92)
  (check-eq? (hash-ref ledger 'wave) 'W0)
  (check-equal? (hash-ref ledger 'baseline) "a4b85569ff0dbe7971c3fec12babdb3fccbdd329")
  (check-eq? (hash-ref ledger 'scope) 'characterization-only)
  (check-false (hash-ref ledger 'production-change))
  (define paths (hash-ref ledger 'paths))
  (exact-id-bijection "explicit path variants" expected-path-ids paths)
  (check-equal? (sort (remove-duplicates (map (lambda (path) (hash-ref path 'family)) paths))
                      symbol<?)
                (sort expected-families symbol<?)))

(test-case "W0-2: every path is non-vacuous and all effect anchors resolve"
  (for ([path (in-list (hash-ref (read-one ledger-path) 'paths))])
    (define id (hash-ref path 'id))
    (define trace (hash-ref path 'trace))
    (check-true (>= (length trace) 3) (format "~a trace is non-vacuous" id))
    (check-equal? (length (map (lambda (effect) (hash-ref effect 'effect)) trace))
                  (length (remove-duplicates (map (lambda (effect) (hash-ref effect 'effect)) trace)))
                  (format "~a effect names must be unique" id))
    (for ([effect (in-list trace)])
      (check-locator id (hash-ref effect 'anchor)))))

(test-case "W0-3: unique structural anchors preserve control-flow source order"
  (define groups
    '(("runtime/session/session-lifecycle.rkt:try-claim-prompt! sess"
       "runtime/session/session-lifecycle.rkt:define active-turn-id"
       "runtime/session/session-lifecycle.rkt:maybe-dispatch-hooks ext-reg 'input"
       "runtime/session/session-lifecycle.rkt:current-prompt-operation-session sess"
       "runtime/session/session-lifecycle.rkt:run-prompt-internal sess
                                                                   effective-input"
       "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st! (agent-session-lifecycle sess)"
       "runtime/session/session-lifecycle.rkt:finish-session-turn! sess"
       "runtime/session/session-lifecycle.rkt:release-prompt! sess")
      ("runtime/session/session-lifecycle.rkt:    (build-session-context-for-prompt sess user-message ensure-persisted!-fn"
       "runtime/session/session-lifecycle.rkt:maybe-compact-context sess context-with-system"
       "runtime/session/session-lifecycle.rkt:ensure-persisted!-fn sess"
       "runtime/session/session-lifecycle.rkt:dispatch-iteration sess context-after-compact"
       "runtime/session/session-lifecycle.rkt:;; 5. Rebuild index\n  (guarded-set-index! sess (build-index!"
       "runtime/session/session-lifecycle.rkt:\"session.updated\"")
      ("runtime/agent-session.rkt:guarded-set-closed! sess #t"
       "runtime/agent-session.rkt:exn-message e)))])\n      (ensure-persisted! sess"
       "runtime/agent-session.rkt:session-shutdown-event \"session.closed\""
       "runtime/agent-session.rkt:'session-shutdown"
       "runtime/agent-session.rkt:persist-high-value-conclusions! (agent-session-task-conclusions sess)"
       "runtime/agent-session.rkt:guarded-set-active! sess #f"
       "runtime/agent-session.rkt:close-session-repository! repo")))
  (for ([group (in-list groups)]
        [index (in-naturals 1)])
    (for ([locator (in-list group)])
      (check-locator (format "order group ~a" index) locator #:unique? #t))
    (define positions (map first-anchor-position group))
    (check-equal? positions (sort positions <) (format "source order group ~a drifted" index))))

(test-case "W0-4: responsibilities and exact direct/transitive consumer edges are anchored"
  (define ledger (read-one ledger-path))
  (check-equal? (hash-ref ledger 'responsibility-taxonomy) expected-responsibilities)
  (define units (hash-ref ledger 'units))
  (define observed
    (remove-duplicates (append* (map (lambda (unit) (hash-ref unit 'responsibilities)) units))))
  (check-equal? (sort observed symbol<?) (sort expected-responsibilities symbol<?))
  (for ([unit (in-list units)])
    (check-locator (hash-ref unit 'id) (hash-ref unit 'owner) #:unique? #t))
  (define edges (hash-ref ledger 'consumer-edges))
  (exact-id-bijection "consumer edges" expected-consumer-ids edges)
  (for ([edge (in-list edges)])
    (check-not-false (member (hash-ref edge 'kind) '(direct transitive)))
    (check-locator (hash-ref edge 'id) (hash-ref edge 'anchor))))

(test-case "W0-5: exceptional boundary inventory is exhaustive, accountable and anchored"
  (define exits (hash-ref (read-one ledger-path) 'exceptional-exits))
  (exact-id-bijection "exceptional exits" expected-exit-ids exits)
  (for ([exit (in-list exits)])
    (define id (hash-ref exit 'id))
    (check-not-false (member (hash-ref exit 'classification) '(IN_SCOPE DEFERRED SEPARATE_MILESTONE)))
    (check-not-false (member (hash-ref exit 'severity) '(low medium high critical)))
    (check-true (symbol? (hash-ref exit 'owner)))
    (check-true (string-contains? (hash-ref exit 'follow-up) "#")
                (format "~a must name an accountable issue" id))
    (check-locator id (hash-ref exit 'anchor))))

(test-case "W0-6: parameter scopes pin exceptional rollback save-back timing"
  (define scopes (hash-ref (read-one ledger-path) 'parameter-scopes))
  (exact-id-bijection "parameter scopes"
                      '(current-prompt-operation-session current-rollback-state)
                      scopes)
  (define rollback (findf (lambda (scope) (eq? (hash-ref scope 'id) 'current-rollback-state)) scopes))
  (check-eq? (hash-ref rollback 'save-back) 'rollback-dynamic-wind-after-before-parameter-unwind)
  (check-eq? (hash-ref rollback 'unwind) 'after-save-back)
  (for ([scope (in-list scopes)])
    (check-locator (hash-ref scope 'id) (hash-ref scope 'anchor))))

(test-case "W0-7: behavior evidence and report agree with the machine oracle"
  (define ledger (read-one ledger-path))
  (for ([probe (in-list (hash-ref ledger 'behavioral-evidence))])
    (check-locator (hash-ref probe 'id) (hash-ref probe 'anchor)))
  (define report (file->string report-path))
  (for ([path (in-list (hash-ref ledger 'paths))])
    (check-true (string-contains? report (symbol->string (hash-ref path 'id)))
                (format "report omits path ~a" (hash-ref path 'id))))
  (for ([finding (in-list (hash-ref ledger 'findings))])
    (define id (hash-ref finding 'id))
    (check-true (string-contains? report (symbol->string id)))
    (check-true (string-contains? report (hash-ref finding 'follow-up)))))
