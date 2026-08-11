#hasheq((baseline . "a4b85569ff0dbe7971c3fec12babdb3fccbdd329")
        (behavioral-evidence
         .
         (#hasheq((anchor . "tests/test-agent-session-basic.rkt:run-prompt")
                  (id . normal-error-close)
                  (mode . behavioral)
                  (paths . (normal-success handled-error close-normal)))
          #hasheq((anchor
                   .
                   "tests/test-session-lifecycle-guards.rkt:close-session! called twice")
                  (id . close-repeat)
                  (mode . behavioral)
                  (paths . (close-repeated)))
          #hasheq((anchor
                   .
                   "tests/test-agent-session-cancellation.rkt:pre-cancelled token")
                  (id . direct-cancellation)
                  (mode . behavioral)
                  (paths . (cancel-pre-iteration)))
          #hasheq((anchor
                   .
                   "tests/test-loop-cancellation.rkt:stream.turn.cancelled")
                  (id . direct-midstream)
                  (mode . behavioral)
                  (paths . (cancel-midstream-direct)))
          #hasheq((anchor
                   .
                   "tests/test-interrupt-lifecycle.rkt:interrupt.accepted")
                  (id . correlated-interrupt)
                  (mode . behavioral)
                  (paths . (cancel-pre-iteration-correlated cancel-midstream)))
          #hasheq((anchor
                   .
                   "tests/test-session-compaction-lifecycle.rkt:session.compact")
                  (id . manual-compaction)
                  (mode . behavioral)
                  (paths
                   .
                   (compact-manual-completed
                    compact-manual-nothing
                    compact-manual-failed
                    compact-manual-contention)))
          #hasheq((anchor
                   .
                   "tests/test-hooks-complete.rkt:session-before-compact")
                  (id . auto-compaction)
                  (mode . behavioral)
                  (paths . (compact-auto-success compact-auto-hook-block)))
          #hasheq((anchor
                   .
                   "tests/test-mid-turn-compaction-integration.rkt:mid-turn")
                  (id . midturn-compaction)
                  (mode . behavioral)
                  (paths . (compact-midturn)))
          #hasheq((anchor . "tests/test-auto-retry.rkt:retry")
                  (id . retry-policy)
                  (mode . behavioral)
                  (paths
                   .
                   (retry-success
                    retry-exhausted
                    retry-held-circuit
                    retry-progressive-circuit
                    retry-health-gate)))
          #hasheq((anchor . "tests/test-adaptive-retry.rkt:adaptive")
                  (id . retry-adaptive)
                  (mode . behavioral)
                  (paths . (retry-adaptive)))
          #hasheq((anchor
                   .
                   "tests/test-partial-result-preservation.rkt:partial")
                  (id . retry-partial)
                  (mode . behavioral)
                  (paths . (retry-exhausted-partial retry-partial-recovery)))
          #hasheq((anchor . "tests/test-hooks-complete.rkt:hook 'block")
                  (id . hook-agent)
                  (mode . behavioral)
                  (paths
                   .
                   (hook-before-agent-block
                    hook-message-start-block
                    hook-message-end-block)))
          #hasheq((anchor
                   .
                   "tests/test-agent-session-hooks.rkt:turn-start hook 'block")
                  (id . hook-turn)
                  (mode . behavioral)
                  (paths . (hook-turn-start-block)))
          #hasheq((anchor
                   .
                   "tests/test-session-lifecycle-characterization.rkt:unique structural anchors")
                  (id . source-only-failures)
                  (mode . source-only)
                  (paths
                   .
                   (error-then-index-failure
                    compact-auto-start-failure
                    compact-manual-tracer-failure
                    close-active-prompt
                    hook-input-block
                    hook-model-request-block)))))
        (consumer-edges
         .
         (#hasheq((anchor
                   .
                   "interfaces/sdk-core.rkt:session:run-prompt! sess prompt")
                  (from . interfaces/sdk-core)
                  (id . sdk-run)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "tui/commands/goal-bridge.rkt:run-prompt! (unbox sess-box) prompt")
                  (from . tui/commands/goal-bridge)
                  (id . goal-run)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "tui/tui-init.rkt:run-prompt! campaign-sess prompt")
                  (from . tui/tui-init)
                  (id . tui-campaign)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor . "tui/tui-init.rkt:run-prompt! sess prompt")
                  (from . tui/tui-init)
                  (id . tui-normal)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "wiring/run-interactive.rkt:run-prompt! sess prompt")
                  (from . wiring/run-interactive)
                  (id . cli-runners)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor . "wiring/run-json-rpc.rkt:run-prompt! sess text")
                  (from . wiring/run-json-rpc)
                  (id . rpc-prompt)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "wiring/run-json-rpc.rkt:run-prompt! default-sess msg")
                  (from . wiring/run-json-rpc)
                  (id . rpc-default-prompt)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "gui/slash-commands.rkt:run-prompt! campaign-session prompt")
                  (from . gui/slash-commands)
                  (id . gui-campaign)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "gui/slash-commands.rkt:run-prompt! sess (hash-ref payload 'submit)")
                  (from . gui/slash-commands)
                  (id . gui-submit)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "gui/slash-commands.rkt:run-prompt! sess (hash-ref payload 'new-session)")
                  (from . gui/slash-commands)
                  (id . gui-new-session)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor . "gui/main.rkt:run-prompt! sess val")
                  (from . gui/main)
                  (id . gui-input)
                  (kind . direct)
                  (to . run-prompt!))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:run-prompt-internal sess")
                  (from . run-prompt!)
                  (id . prompt-internal)
                  (kind . direct)
                  (to . run-prompt-internal))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:build-session-context-for-prompt sess user-message")
                  (from . run-prompt-internal)
                  (id . prompt-context)
                  (kind . direct)
                  (to . build-session-context-for-prompt))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:dispatch-iteration sess context-after-compact")
                  (from . run-prompt-internal)
                  (id . prompt-dispatch)
                  (kind . direct)
                  (to . dispatch-iteration))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:run-iteration-loop/v2")
                  (from . dispatch-iteration)
                  (id . dispatch-loop)
                  (kind . direct)
                  (to . run-iteration-loop/v2))
          #hasheq((anchor . "wiring/run-json-rpc.rkt:close-session! sess")
                  (from . wiring/run-json-rpc)
                  (id . rpc-close)
                  (kind . direct)
                  (to . close-session!))
          #hasheq((anchor . "gui/slash-commands.rkt:close-session! sess")
                  (from . gui/slash-commands)
                  (id . gui-slash-close)
                  (kind . direct)
                  (to . close-session!))
          #hasheq((anchor . "gui/main.rkt:close-session! sess")
                  (from . gui/main)
                  (id . gui-close)
                  (kind . direct)
                  (to . close-session!))
          #hasheq((anchor
                   .
                   "runtime/turn-orchestrator.rkt:call-with-provider-retry")
                  (from . run-provider-turn)
                  (id . provider-retry)
                  (kind . direct)
                  (to . call-with-provider-retry))
          #hasheq((anchor . "runtime/provider-retry.rkt:with-auto-retry")
                  (from . call-with-provider-retry)
                  (id . auto-retry)
                  (kind . direct)
                  (to . with-auto-retry))
          #hasheq((anchor . "runtime/auto-retry.rkt:with-auto-retry thunk")
                  (from . with-retry-policy)
                  (id . policy-retry)
                  (kind . direct)
                  (to . with-auto-retry))
          #hasheq((anchor
                   .
                   "tools/builtins/spawn-execution.rkt:with-auto-retry")
                  (from . run-subagent-loop)
                  (id . subagent-retry)
                  (kind . direct)
                  (to . with-auto-retry))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:maybe-compact-context sess context-with-system")
                  (from . run-prompt-internal)
                  (id . prompt-compaction)
                  (kind . direct)
                  (to . maybe-compact-context))
          #hasheq((anchor
                   .
                   "runtime/compaction/session-compaction.rkt:maybe-compact-context sess context budget-threshold")
                  (from . compact-context-mid-turn)
                  (id . midturn-compaction)
                  (kind . direct)
                  (to . maybe-compact-context))
          #hasheq((anchor
                   .
                   "runtime/session/session-events.rkt:compact-session-durably! sess #:request-id request-id")
                  (from . wire-session-event-handlers!)
                  (id . durable-compaction-event)
                  (kind . direct)
                  (to . compact-session-durably!))
          #hasheq((anchor . "interfaces/sdk-compat.rkt:q:session-send")
                  (from . interfaces/sdk-compat)
                  (id . sdk-compat)
                  (kind . transitive)
                  (to . interfaces/sdk-core))
          #hasheq((anchor . "runtime/goal/goal-runner.rkt:run-prompt-fn!")
                  (from . runtime/goal/goal-runner)
                  (id . goal-callback)
                  (kind . transitive)
                  (to . goal-run))
          #hasheq((anchor
                   .
                   "runtime/agent-session.rkt:only-in \"session/session-lifecycle.rkt\"")
                  (from . runtime/agent-session)
                  (id . runtime-facade-reexport)
                  (kind . transitive)
                  (to . run-prompt!))
          #hasheq((anchor . "main.rkt:run-prompt!")
                  (from . main)
                  (id . main-run-reexport)
                  (kind . transitive)
                  (to . run-prompt!))
          #hasheq((anchor . "main.rkt:close-session!")
                  (from . main)
                  (id . main-close-reexport)
                  (kind . transitive)
                  (to . close-session!))
          #hasheq((anchor . "interfaces/sdk.rkt:run-prompt!")
                  (from . interfaces/sdk)
                  (id . sdk-reexport)
                  (kind . transitive)
                  (to . interfaces/sdk-core))
          #hasheq((anchor . "interfaces/sdk-public.rkt:run-prompt!")
                  (from . interfaces/sdk-public)
                  (id . sdk-public-reexport)
                  (kind . transitive)
                  (to . interfaces/sdk))
          #hasheq((anchor . "interfaces/sdk-compat.rkt:q:session-send")
                  (from . interfaces/sdk-compat)
                  (id . sdk-compat-send)
                  (kind . transitive)
                  (to . interfaces/sdk-core))
          #hasheq((anchor . "runtime/agent-session.rkt:maybe-compact-context")
                  (from . runtime/agent-session)
                  (id . runtime-facade-compaction-reexport)
                  (kind . transitive)
                  (to . maybe-compact-context))))
        (consumer-scope
         .
         #hasheq((excluded
                  .
                  (tests
                   scripts
                   benchmarks
                   generated
                   import-only-without-reexport))
                 (included . tracked-production-call-and-reexport-modules)))
        (exceptional-exits
         .
         (#hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:unless (agent-session-active? sess")
                  (classification . IN_SCOPE)
                  (cleanup . none)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . closed-guard)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . pre-claim)
                  (rollback-save-back . not-entered)
                  (severity . low)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:Prompt already running")
                  (classification . DEFERRED)
                  (cleanup . none)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . busy-event-failure)
                  (outcome . masks-busy-exception)
                  (owner . runtime-session)
                  (phase . claim-denied)
                  (rollback-save-back . not-entered)
                  (severity . medium)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:define active-turn-id")
                  (classification . DEFERRED)
                  (cleanup . ownership-not-released)
                  (follow-up . "W1 #9243 preserve; W4 #9246 decide")
                  (id . begin-turn-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . post-claim-pre-outer-wind)
                  (rollback-save-back . not-entered)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                  (classification . DEFERRED)
                  (cleanup . ownership-not-released)
                  (follow-up . "W1 #9243 preserve; W4 #9246 decide")
                  (id . outer-start-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . post-claim-pre-outer-wind)
                  (rollback-save-back . not-entered)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:maybe-dispatch-hooks ext-reg 'input")
                  (classification . IN_SCOPE)
                  (cleanup . finish+release+cleanup-terminal+emergency-persist)
                  (follow-up . "W1 #9243 equivalence oracle")
                  (id . input-hook-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . outer-wind)
                  (rollback-save-back . not-entered)
                  (severity . medium)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:\"input.blocked\"")
                  (classification . IN_SCOPE)
                  (cleanup . finish+release+cleanup-terminal)
                  (follow-up . "W1 #9243 equivalence oracle")
                  (id . input-hook-block)
                  (outcome . returns-completed)
                  (owner . runtime-session)
                  (phase . outer-wind)
                  (rollback-save-back . not-entered)
                  (severity . low)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:buffer-or-append!-fn sess user-msg")
                  (classification . IN_SCOPE)
                  (cleanup . rollback-save+finish+release+cleanup-terminal)
                  (follow-up . "W1 #9243 equivalence oracle")
                  (id . context-persistence-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . rollback+outer-winds)
                  (rollback-save-back . saved-before-unwind)
                  (severity . high)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:make-context-event")
                  (classification . DEFERRED)
                  (cleanup . rollback-save+finish+release+cleanup-terminal)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . context-event-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . rollback+outer-winds)
                  (rollback-save-back . saved-before-unwind)
                  (severity . medium)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:'model-select")
                  (classification . DEFERRED)
                  (cleanup . rollback-save+finish+release+cleanup-terminal)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . model-select-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . rollback+outer-winds)
                  (rollback-save-back . saved-before-unwind)
                  (severity . medium)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:make-trace-logger bus session-dir")
                  (classification . DEFERRED)
                  (cleanup . rollback-save+finish+release+cleanup-terminal)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . tracer-construction-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . rollback+outer-winds)
                  (rollback-save-back . saved-before-unwind)
                  (severity . high)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:start-trace-logger! tracer")
                  (classification . DEFERRED)
                  (cleanup . rollback-save+finish+release+cleanup-terminal)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . tracer-start-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . rollback+outer-winds)
                  (rollback-save-back . saved-before-unwind)
                  (severity . high)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor . "agent/stream-runner.rkt:provider-stream")
                  (classification . DEFERRED)
                  (cleanup
                   .
                   partial-persist+error-event+terminal+trace-stop+index-rebuild+outer-release)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . provider-generator-failure)
                  (outcome . returns-error-result)
                  (owner . agent-stream)
                  (phase . dispatch-handler)
                  (rollback-save-back . saved-before-unwind)
                  (severity . medium)
                  (terminal . turn.completed/error))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:emit-session-event! bus sid \"runtime.error\" payload")
                  (classification . DEFERRED)
                  (cleanup . outer-cleanup-best-effort)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . dispatch-handler-failure)
                  (outcome . propagates-handler-error)
                  (owner . runtime-session)
                  (phase . dispatch-error-handler)
                  (rollback-save-back . saved-before-unwind)
                  (severity . high)
                  (terminal . possibly-turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:Stop trace logger on normal completion")
                  (classification . DEFERRED)
                  (cleanup . error-handler-runs+second-stop-may-fail)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . normal-tracer-stop-failure)
                  (outcome . error-result-or-propagates)
                  (owner . runtime-session)
                  (phase . dispatch-handler)
                  (rollback-save-back . saved-before-unwind)
                  (severity . medium)
                  (terminal . turn.completed/error))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                  (classification . DEFERRED)
                  (cleanup . rollback-save+finish+release+cleanup-terminal)
                  (follow-up . "W2 #9244 boundary equivalence")
                  (id . index-rebuild-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . post-dispatch)
                  (rollback-save-back . saved-before-unwind)
                  (severity . high)
                  (terminal . second-turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                  (classification . DEFERRED)
                  (cleanup . rollback-save+finish+release+cleanup-terminal)
                  (follow-up . "W2 #9244 boundary equivalence")
                  (id . session-updated-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . post-index)
                  (rollback-save-back . saved-before-unwind)
                  (severity . medium)
                  (terminal . turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                  (classification . DEFERRED)
                  (cleanup . outer-finish+release)
                  (follow-up . "W2 #9244 ownership gate")
                  (id . rollback-save-back-failure)
                  (outcome . propagates)
                  (owner . runtime-session-state)
                  (phase . rollback-after)
                  (rollback-save-back . save-back-failed)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                  (classification . DEFERRED)
                  (cleanup . subsequent-cleanup-suppressed)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . finish-turn-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . outer-after)
                  (rollback-save-back . already-saved)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                  (classification . DEFERRED)
                  (cleanup . acknowledgement+terminal+persist-suppressed)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . release-prompt-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . outer-after)
                  (rollback-save-back . already-saved)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:acknowledgement-tracer")
                  (classification . DEFERRED)
                  (cleanup . cleanup-terminal+persist-suppressed)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . acknowledgement-tracer-failure)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . outer-after)
                  (rollback-save-back . already-saved)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:cleanup turn.completed failed")
                  (classification . IN_SCOPE)
                  (cleanup . logged+continue-emergency-persist)
                  (follow-up . "W1 #9243 equivalence oracle")
                  (id . cleanup-terminal-failure)
                  (outcome . swallowed-exn-fail)
                  (owner . runtime-session)
                  (phase . outer-after)
                  (rollback-save-back . already-saved)
                  (severity . low)
                  (terminal . attempted-turn.completed/cleanup))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:emergency persist failed")
                  (classification . IN_SCOPE)
                  (cleanup . logged)
                  (follow-up . "W1 #9243 equivalence oracle")
                  (id . emergency-persist-failure)
                  (outcome . swallowed-exn-fail)
                  (owner . runtime-session)
                  (phase . outer-after)
                  (rollback-save-back . already-saved)
                  (severity . medium)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-events.rkt:\"interrupt.accepted\"")
                  (classification . DEFERRED)
                  (cleanup . request-recorded+token-not-signalled)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . interrupt-accepted-publication-failure)
                  (outcome . propagates-from-publish)
                  (owner . runtime-interruption)
                  (phase . event-subscriber)
                  (rollback-save-back . not-applicable)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-events.rkt:signal-session-interrupt!")
                  (classification . DEFERRED)
                  (cleanup . accepted-event-already-emitted)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . interrupt-signal-failure)
                  (outcome . swallowed-by-subscriber-handler)
                  (owner . runtime-interruption)
                  (phase . event-subscriber)
                  (rollback-save-back . not-applicable)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor . "runtime/provider-retry.rkt:on-retry")
                  (classification . DEFERRED)
                  (cleanup . no-sleep+no-reattempt)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . retry-callback-failure)
                  (outcome . propagates-to-dispatch)
                  (owner . runtime-retry)
                  (phase . retry-handler)
                  (rollback-save-back . unchanged)
                  (severity . medium)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/auto-retry.rkt:sleep (/ next-delay 1000.0)")
                  (classification . DEFERRED)
                  (cleanup . dynamic-winds-only)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . retry-sleep-break)
                  (outcome . non-exn-break-propagates)
                  (owner . runtime-retry)
                  (phase . retry-handler)
                  (rollback-save-back . unchanged)
                  (severity . medium)
                  (terminal . none))
          #hasheq((anchor . "runtime/provider-retry.rkt:exn:fail:stream-error")
                  (classification . DEFERRED)
                  (cleanup . partial-persist+dispatch-error)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . retry-partial-metadata-loss)
                  (outcome . returns-error-without-retry-metadata)
                  (owner . runtime-retry)
                  (phase . retry-exhaustion)
                  (rollback-save-back . saved-before-unwind)
                  (severity . medium)
                  (terminal . turn.completed/error))
          #hasheq((anchor
                   .
                   "runtime/compaction/session-compaction.rkt:\"budget-exceeded\"")
                  (classification . DEFERRED)
                  (cleanup . ownership-not-released)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . auto-compaction-start-event-failure)
                  (outcome . propagates)
                  (owner . runtime-compaction)
                  (phase . post-claim-pre-wind)
                  (rollback-save-back . unchanged)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/compaction/session-compaction.rkt:hook-result-action compact-hook-res")
                  (classification . IN_SCOPE)
                  (cleanup . release+cooldown+complete-event)
                  (follow-up . "W1 #9243 preserve ordering")
                  (id . auto-compaction-hook-block)
                  (outcome . returns-original-context)
                  (owner . runtime-compaction)
                  (phase . compaction-body)
                  (rollback-save-back . unchanged)
                  (severity . low)
                  (terminal . compaction/compaction-complete))
          #hasheq((anchor
                   .
                   "runtime/compaction/session-compaction.rkt:compact-history")
                  (classification . DEFERRED)
                  (cleanup . release+cooldown+complete-event)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . auto-compaction-body-failure)
                  (outcome . propagates-original-or-cleanup-error)
                  (owner . runtime-compaction)
                  (phase . compaction-body)
                  (rollback-save-back . unchanged)
                  (severity . medium)
                  (terminal . compaction/compaction-complete))
          #hasheq((anchor
                   .
                   "runtime/compaction/session-compaction.rkt:release-compaction!")
                  (classification . DEFERRED)
                  (cleanup . later-cooldown/event-may-be-suppressed)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . auto-compaction-cleanup-failure)
                  (outcome . propagates)
                  (owner . runtime-compaction)
                  (phase . compaction-after)
                  (rollback-save-back . unchanged)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-events.rkt:\"session.compact.already-running\"")
                  (classification . DEFERRED)
                  (cleanup . none)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . manual-compaction-contention-event-failure)
                  (outcome . propagates)
                  (owner . runtime-compaction)
                  (phase . claim-denied)
                  (rollback-save-back . not-applicable)
                  (severity . medium)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-events.rkt:make-trace-logger")
                  (classification . DEFERRED)
                  (cleanup . ownership-not-released)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . manual-compaction-tracer-construction-failure)
                  (outcome . propagates)
                  (owner . runtime-compaction)
                  (phase . post-claim-pre-handler)
                  (rollback-save-back . not-applicable)
                  (severity . high)
                  (terminal . none))
          #hasheq((anchor
                   .
                   "runtime/session/session-events.rkt:\"session.compact.failed\"")
                  (classification . DEFERRED)
                  (cleanup . release+trace-stop-may-be-suppressed)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . manual-compaction-failed-event-failure)
                  (outcome . propagates-handler-error)
                  (owner . runtime-compaction)
                  (phase . manual-handler)
                  (rollback-save-back . not-applicable)
                  (severity . high)
                  (terminal . attempted-session.compact.failed))
          #hasheq((anchor . "runtime/agent-session.rkt:already-closed session")
                  (classification . DEFERRED)
                  (cleanup . repeats-cleanup-groups)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . repeated-close)
                  (outcome . continues-not-terminal)
                  (owner . runtime-session)
                  (phase . close-guard)
                  (rollback-save-back . not-applicable)
                  (severity . medium)
                  (terminal . warning+possible-session.closed))
          #hasheq((anchor
                   .
                   "runtime/agent-session.rkt:when (session-active? sess")
                  (classification . DEFERRED)
                  (cleanup . closes-repository-while-prompt-may-write)
                  (follow-up
                   .
                   "W4 #9246 assign separate concurrency milestone")
                  (id . active-prompt-close)
                  (outcome . race-dependent)
                  (owner . runtime-session)
                  (phase . concurrent-close)
                  (rollback-save-back . saved-by-prompt-if-reached)
                  (severity . high)
                  (terminal . session.closed+later-session.updated))
          #hasheq((anchor . "runtime/agent-session.rkt:Cleanup steps")
                  (classification . IN_SCOPE)
                  (cleanup . logged+continues-next-group)
                  (follow-up . "W3 #9245 locality assessment")
                  (id . close-cleanup-group-failure)
                  (outcome . swallowed-exn-fail)
                  (owner . runtime-session)
                  (phase . close-step)
                  (rollback-save-back . not-applicable)
                  (severity . medium)
                  (terminal . session.closed-if-emission-reached))
          #hasheq((anchor . "runtime/session/session-lifecycle.rkt:[exn:fail?")
                  (classification . DEFERRED)
                  (cleanup . dynamic-winds-only)
                  (follow-up . "W4 #9246 terminal reassessment")
                  (id . non-exn-break-kill)
                  (outcome . propagates)
                  (owner . runtime-session)
                  (phase . any-exn-fail-only-boundary)
                  (rollback-save-back . save-back-if-inner-entered)
                  (severity . high)
                  (terminal . cleanup-terminal-if-outer-after-reached))))
        (findings
         .
         (#hasheq((classification . DEFERRED)
                  (follow-up . "W1 #9243 preserve; W4 #9246 terminal decision")
                  (id . W0-F1)
                  (owner . runtime-session)
                  (severity . high)
                  (summary
                   .
                   "Prompt ownership is claimed before outer dynamic-wind protection."))
          #hasheq((classification . DEFERRED)
                  (follow-up . "W1 #9243 preserve; W4 #9246 terminal decision")
                  (id . W0-F2)
                  (owner . runtime-session)
                  (severity . medium)
                  (summary
                   .
                   "Normal, error and correlated cancellation use different terminal identities/events."))
          #hasheq((classification . DEFERRED)
                  (follow-up
                   .
                   "W4 #9246 assign separate concurrency milestone")
                  (id . W0-F3)
                  (owner . runtime-session)
                  (severity . high)
                  (summary
                   .
                   "Close does not coordinate with an active prompt or repository writer."))
          #hasheq((classification . DEFERRED)
                  (follow-up
                   .
                   "W3 #9245 locality assessment; W4 #9246 decision")
                  (id . W0-F4)
                  (owner . runtime-compaction)
                  (severity . medium)
                  (summary
                   .
                   "Automatic compaction completion/cooldown runs after hook block or body error, while start-event failure leaks ownership."))
          #hasheq((classification . DEFERRED)
                  (follow-up . "W4 #9246 terminal decision")
                  (id . W0-F5)
                  (owner . runtime-retry)
                  (severity . medium)
                  (summary
                   .
                   "Retry sleep is not cancellation-aware and partial wrapping can hide retry metadata."))))
        (milestone . v0.99.92)
        (parameter-scopes
         .
         (#hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:current-prompt-operation-session sess")
                  (enter . before-run-prompt-internal)
                  (id . current-prompt-operation-session)
                  (save-back . none)
                  (unwind . automatic))
          #hasheq((anchor
                   .
                   "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                  (enter . from-session-lifecycle-state)
                  (id . current-rollback-state)
                  (save-back
                   .
                   rollback-dynamic-wind-after-before-parameter-unwind)
                  (unwind . after-save-back))))
        (paths
         .
         (#hasheq((family . normal)
                  (id . normal-success)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:try-claim-prompt! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:begin-session-turn! sess")
                            (effect . begin-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                            (effect . outer-turn-started))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:maybe-dispatch-hooks ext-reg 'input")
                            (effect . input-hook))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:'last-user-prompt effective-input")
                            (effect . last-prompt-mutation))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:current-prompt-operation-session sess")
                            (effect . parameterize-session-state))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-config!")
                            (effect . working-set-config))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:buffer-or-append!-fn sess user-msg")
                            (effect . user-index-persistence))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:build-session-context-for-prompt sess user-message")
                            (effect . context-build))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:maybe-compact-context sess context-with-system")
                            (effect . advisory-compaction))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:check-context-pressure sess token-count")
                            (effect . context-pressure))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:make-context-event")
                            (effect . context-built))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:ensure-persisted!-fn sess")
                            (effect . ensure-persisted))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:dispatch-iteration sess context-after-compact")
                            (effect . tracer-and-model-iteration))
                    #hasheq((anchor
                             .
                             "agent/loop-stream.rkt:make-stream-turn-completed-event")
                            (effect . model-terminal))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                            (effect . rollback-save-back))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt)))))
          #hasheq((family . error)
                  (id . handled-error)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:append-session-entries! sess partial-msgs")
                            (effect . partial-persist))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:emit-session-event! bus sid \"runtime.error\" payload")
                            (effect . runtime-error))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:turn-end-event \"turn.completed\"")
                            (effect . error-terminal))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:stop-trace-logger! tracer")
                            (effect . trace-stop))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:lastTurnTermination")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . save-back-release)))))
          #hasheq((family . error)
                  (id . error-then-index-failure)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:turn-end-event \"turn.completed\"")
                            (effect . error-terminal))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:build-index! log-path idx-path")
                            (effect . build-index-raises))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                            (effect . rollback-save-back))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:emit-cleanup-turn-completed?")
                            (effect . cleanup-terminal)))))
          #hasheq((family . cancel)
                  (id . cancel-pre-iteration)
                  (precondition . token-pre-cancelled-without-request)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "tests/test-agent-session-cancellation.rkt:cancel-token! tok")
                            (effect . token-already-cancelled))
                    #hasheq((anchor
                             .
                             "agent/iteration/counters.rkt:(define (check-cancellation")
                            (effect . cancellation-check))
                    #hasheq((anchor
                             .
                             "agent/iteration/counters.rkt:turn-cancelled-event \"turn.cancelled\"")
                            (effect . uncorrelated-turn-cancelled))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:lastTurnTermination")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-interruption.rkt:make-cancellation-token")
                            (effect . rotate-token))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:when interrupt-request-id")
                            (effect . no-correlated-terminal)))))
          #hasheq((family . cancel)
                  (id . cancel-midstream)
                  (precondition . accepted-interrupt-request-midstream)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "agent/stream-runner.rkt:when (stream-chunk-done? chunk")
                            (effect . chunk-processed))
                    #hasheq((anchor
                             .
                             "agent/stream-runner.rkt:cancellation-token-cancelled?")
                            (effect . token-observed))
                    #hasheq((anchor
                             .
                             "agent/stream-runner.rkt:make-stream-turn-cancelled-event")
                            (effect . stream-cancelled))
                    #hasheq((anchor
                             .
                             "agent/loop-stream.rkt:make-stream-turn-completed-event")
                            (effect . stream-completed))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:lastTurnTermination")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"turn.cancelled\"")
                            (effect . correlated-turn-cancelled)))))
          #hasheq((family . close)
                  (id . close-normal)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/agent-session.rkt:agent-session-closed? sess")
                            (effect . closed-check))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:guarded-set-closed! sess #t")
                            (effect . mark-closed))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:current-browser-service #f")
                            (effect . browser-clear))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:ensure-persisted! sess")
                            (effect . ensure-persisted))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:session-shutdown-event \"session.closed\"")
                            (effect . session-closed))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:'session-shutdown")
                            (effect . shutdown-hook))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:persist-high-value-conclusions!")
                            (effect . persist-conclusions))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:guarded-set-active! sess #f")
                            (effect . mark-inactive))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:stop-blackboard-subscriber!")
                            (effect . stop-blackboard))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:set-session-active! #f")
                            (effect . registry-inactive))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:set-hot-swap-enabled! #f")
                            (effect . disable-hot-swap))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:stop-registry-watcher!")
                            (effect . stop-watcher))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:close-session-repository!")
                            (effect . close-repository)))))
          #hasheq((family . close)
                  (id . close-repeated)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/agent-session.rkt:already-closed session")
                            (effect . warning))
                    #hasheq((anchor . "runtime/agent-session.rkt:(void)")
                            (effect . void-not-return))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:guarded-set-closed! sess #t")
                            (effect . mark-closed-again))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:Cleanup steps")
                            (effect . repeat-cleanup-groups))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:close-session-repository!")
                            (effect . repository-close-again)))))
          #hasheq((family . close)
                  (id . close-active-prompt)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/agent-session.rkt:(define (close-session! sess)")
                            (effect . no-prompt-claim-check))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:when (session-active? sess")
                            (effect . close-while-active))
                    #hasheq((anchor
                             .
                             "runtime/agent-session.rkt:close-session-repository!")
                            (effect . repository-close))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . prompt-index-rebuild-can-follow))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                            (effect . prompt-session-updated-can-follow)))))
          #hasheq((family . retry)
                  (id . retry-success)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:wrapped-attempt")
                            (effect . failed-attempt))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:retryable-error? e")
                            (effect . retry-decision))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:make-auto-retry-start-event")
                            (effect . retry-event))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:sleep (/ next-delay 1000.0)")
                            (effect . sleep))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:loop (add1 attempt)")
                            (effect . reattempt))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:record-success! health")
                            (effect . success-health-reset)))))
          #hasheq((family . retry)
                  (id . retry-exhausted)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:< current-type-count type-budget")
                            (effect . budget-denies))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:retry-exhausted (format")
                            (effect . retry-exhausted-raise))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:retries-attempted")
                            (effect . retry-metadata))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:emit-session-event! bus sid \"runtime.error\" payload")
                            (effect . runtime-error))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:make-loop-result context-with-system 'error")
                            (effect . error-path)))))
          #hasheq((family . retry)
                  (id . retry-exhausted-partial)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:retry-exhausted (format")
                            (effect . exhaustion))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:exn:fail:stream-error")
                            (effect . partial-wrap))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:append-session-entries! sess partial-msgs")
                            (effect . partial-persist))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:if (retry-exhausted? e)")
                            (effect . metadata-hidden)))))
          #hasheq((family . retry)
                  (id . retry-held-circuit)
                  (trace
                   .
                   (#hasheq((anchor . "runtime/auto-retry.rkt:held-request? e")
                            (effect . held-detected))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:on-circuit-break")
                            (effect . circuit-callback))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:\"circuit-break.tripped\"")
                            (effect . circuit-event))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:not (held-request? exn)")
                            (effect . no-retry)))))
          #hasheq((family . retry)
                  (id . retry-progressive-circuit)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:consecutive-stalls")
                            (effect . stall-count))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:stall-max-consecutive")
                            (effect . threshold))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:\"circuit-break.tripped\"")
                            (effect . circuit-event))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:retry-exhausted (format")
                            (effect . exhaustion)))))
          #hasheq((family . retry)
                  (id . retry-health-gate)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:record-failure! health")
                            (effect . record-failure))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:provider-healthy?")
                            (effect . health-check))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:\"provider.health-gate\"")
                            (effect . health-event))
                    #hasheq((anchor . "runtime/provider-retry.rkt:#f")
                            (effect . deny)))))
          #hasheq((family . retry)
                  (id . retry-adaptive)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:make-auto-retry-start-event")
                            (effect . retry-event))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:adapt-provider-request")
                            (effect . adapt))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:\"provider.adaptive-retry\"")
                            (effect . adaptive-event))
                    #hasheq((anchor
                             .
                             "runtime/auto-retry.rkt:loop (add1 attempt)")
                            (effect . reattempt)))))
          #hasheq((family . retry)
                  (id . retry-partial-recovery)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:partial-text-box")
                            (effect . capture-partial))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:partial-min-chars")
                            (effect . threshold))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:make-text-part continuation-text")
                            (effect . continuation-context))
                    #hasheq((anchor
                             .
                             "runtime/provider-retry.rkt:\"provider.partial-recovery\"")
                            (effect . recovery-event)))))
          #hasheq((family . compaction)
                  (id . compact-auto-success)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:try-claim-compaction!")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:\"budget-exceeded\"")
                            (effect . start-event))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:'session-before-compact")
                            (effect . hook))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:compact-history")
                            (effect . compact))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:\"compaction.completed\"")
                            (effect . completed))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:release-compaction!")
                            (effect . release)))))
          #hasheq((family . compaction)
                  (id . compact-auto-hook-block)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:try-claim-compaction!")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:hook-result-action compact-hook-res")
                            (effect . hook-block))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:context-with-system")
                            (effect . original-context))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:\"compaction-complete\"")
                            (effect . release-cooldown-complete)))))
          #hasheq((family . compaction)
                  (id . compact-auto-start-failure)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:try-claim-compaction!")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:emit-typed-event!")
                            (effect . start-publication-raises))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:dynamic-wind")
                            (effect . before-dynamic-wind))
                    #hasheq((anchor
                             .
                             "runtime/compaction/session-compaction.rkt:release-compaction!")
                            (effect . ownership-leaks)))))
          #hasheq((family . compaction)
                  (id . compact-midturn)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/iteration/step-executor.rkt:stop-soft-limit")
                            (effect . soft-limit-branch))
                    #hasheq((anchor
                             .
                             "runtime/iteration/retry-policy.rkt:\"context.mid-turn-over-budget\"")
                            (effect . over-budget-event))
                    #hasheq((anchor
                             .
                             "runtime/iteration/step-executor.rkt:compact-context-mid-turn")
                            (effect . compact-midturn))
                    #hasheq((anchor
                             .
                             "runtime/iteration/step-executor.rkt:directive-recurse ctx-after-budget")
                            (effect . recurse)))))
          #hasheq((family . compaction)
                  (id . compact-manual-completed)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:try-claim-compaction! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:make-trace-logger")
                            (effect . tracer))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:\"session.compact.started\"")
                            (effect . started))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:compact-proc")
                            (effect . persist-summary))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:build-index! log-path")
                            (effect . rebuild-index))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:\"session.compact.completed\"")
                            (effect . completed))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:release-compaction! sess")
                            (effect . release-stop)))))
          #hasheq((family . compaction)
                  (id . compact-manual-nothing)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:\"session.compact.started\"")
                            (effect . claim-start))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:null? history")
                            (effect . history-check))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:\"session.compact.nothing-to-compact\"")
                            (effect . nothing-terminal))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:release-compaction! sess")
                            (effect . release-stop)))))
          #hasheq((family . compaction)
                  (id . compact-manual-failed)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:[exn:fail?")
                            (effect . handler))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:\"session.compact.failed\"")
                            (effect . failed-event))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:release-compaction! sess")
                            (effect . release))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:stop-compact-tracer! tracer")
                            (effect . trace-stop)))))
          #hasheq((family . compaction)
                  (id . compact-manual-tracer-failure)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:try-claim-compaction! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:make-trace-logger")
                            (effect . tracer-construction-raises))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:with-handlers")
                            (effect . before-handler))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:release-compaction! sess")
                            (effect . ownership-leaks)))))
          #hasheq((family . compaction)
                  (id . compact-manual-contention)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:define claimed? (try-claim-compaction! sess)")
                            (effect . claim-denied))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:\"session.compact.already-running\"")
                            (effect . already-running))
                    #hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:'already-running")
                            (effect . return)))))
          #hasheq((base-path . normal-success)
                  (cleanup-resumes-at . outer-prompt-cleanup)
                  (deviation-at . hook-input-block)
                  (family . normal)
                  (id . hook-input-block)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:try-claim-prompt! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:begin-session-turn! sess")
                            (effect . begin-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                            (effect . outer-turn-started))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:maybe-dispatch-hooks ext-reg 'input")
                            (effect . input-hook))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"input.blocked\"")
                            (effect . input-blocked))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"input-blocked\"")
                            (effect . completed-result))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:emit-cleanup-turn-completed?")
                            (effect . cleanup-terminal))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt)))))
          #hasheq((base-path . normal-success)
                  (cleanup-resumes-at
                   .
                   dispatch-trace-stop+index-rebuild+session-updated+rollback+outer-cleanup)
                  (deviation-at . hook-before-agent-block)
                  (family . normal)
                  (id . hook-before-agent-block)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:try-claim-prompt! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:begin-session-turn! sess")
                            (effect . begin-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                            (effect . outer-turn-started))
                    #hasheq((anchor
                             .
                             "agent/iteration/main-loop.rkt:'before-agent-start")
                            (effect . agent-hook))
                    #hasheq((anchor
                             .
                             "agent/iteration/main-loop.rkt:\"agent.blocked\"")
                            (effect . agent-blocked))
                    #hasheq((anchor
                             .
                             "agent/iteration/main-loop.rkt:\"extension-block\"")
                            (effect . completed-result))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:stop-trace-logger! tracer")
                            (effect . trace-stop))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                            (effect . rollback-save-back))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt)))))
          #hasheq((base-path . normal-success)
                  (cleanup-resumes-at
                   .
                   dispatch-trace-stop+index-rebuild+session-updated+rollback+outer-cleanup)
                  (deviation-at . hook-turn-start-block)
                  (family . normal)
                  (id . hook-turn-start-block)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:try-claim-prompt! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:begin-session-turn! sess")
                            (effect . begin-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                            (effect . outer-turn-started))
                    #hasheq((anchor
                             .
                             "agent/iteration/loop-phases.rkt:'turn-start")
                            (effect . hook))
                    #hasheq((anchor
                             .
                             "agent/iteration/main-loop.rkt:\"turn.blocked\"")
                            (effect . turn-blocked))
                    #hasheq((anchor
                             .
                             "agent/iteration/main-loop.rkt:turn blocked at iteration")
                            (effect . completed-result))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:stop-trace-logger! tracer")
                            (effect . trace-stop))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                            (effect . rollback-save-back))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt)))))
          #hasheq((base-path . normal-success)
                  (cleanup-resumes-at
                   .
                   dispatch-trace-stop+index-rebuild+session-updated+rollback+outer-cleanup)
                  (deviation-at . hook-model-request-block)
                  (family . normal)
                  (id . hook-model-request-block)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:try-claim-prompt! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:begin-session-turn! sess")
                            (effect . begin-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                            (effect . outer-turn-started))
                    #hasheq((anchor
                             .
                             "agent/loop.rkt:'model-request-pre")
                            (effect . hook))
                    #hasheq((anchor
                             .
                             "agent/loop.rkt:make-model-request-blocked-event")
                            (effect . blocked-event))
                    #hasheq((anchor . "agent/loop.rkt:make-turn-end-event")
                            (effect . turn-terminal))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:stop-trace-logger! tracer")
                            (effect . trace-stop))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                            (effect . rollback-save-back))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt)))))
          #hasheq((base-path . normal-success)
                  (cleanup-resumes-at
                   .
                   dispatch-trace-stop+index-rebuild+session-updated+rollback+outer-cleanup)
                  (deviation-at . hook-message-start-block)
                  (family . normal)
                  (id . hook-message-start-block)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:try-claim-prompt! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:begin-session-turn! sess")
                            (effect . begin-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                            (effect . outer-turn-started))
                    #hasheq((anchor
                             .
                             "agent/loop-dispatch.rkt:'message-start")
                            (effect . hook))
                    #hasheq((anchor
                             .
                             "agent/loop-dispatch.rkt:make-message-blocked-event")
                            (effect . blocked-event))
                    #hasheq((anchor
                             .
                             "agent/loop-dispatch.rkt:make-turn-end-event")
                            (effect . turn-terminal))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:stop-trace-logger! tracer")
                            (effect . trace-stop))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                            (effect . rollback-save-back))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt)))))
          #hasheq((base-path . normal-success)
                  (cleanup-resumes-at
                   .
                   dispatch-trace-stop+index-rebuild+session-updated+rollback+outer-cleanup)
                  (deviation-at . hook-message-end-block)
                  (family . normal)
                  (id . hook-message-end-block)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:try-claim-prompt! sess")
                            (effect . claim))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:begin-session-turn! sess")
                            (effect . begin-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:(make-event \"turn.started\"")
                            (effect . outer-turn-started))
                    #hasheq((anchor
                             .
                             "agent/loop-stream.rkt:'message-end")
                            (effect . hook))
                    #hasheq((anchor
                             .
                             "agent/loop-stream.rkt:make-stream-turn-completed-event")
                            (effect . stream-terminal))
                    #hasheq((anchor
                             .
                             "agent/loop-stream.rkt:'hook-blocked")
                            (effect . hook-blocked))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:stop-trace-logger! tracer")
                            (effect . trace-stop))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:guarded-set-index! sess (build-index!")
                            (effect . index-rebuild))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"session.updated\"")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:set-lifecycle-state-rollback-st!")
                            (effect . rollback-save-back))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-turn))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt)))))
          #hasheq((family . cancel)
                  (id . cancel-pre-iteration-correlated)
                  (precondition . accepted-interrupt-request-before-checkpoint)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "runtime/session/session-events.rkt:\"interrupt.accepted\"")
                            (effect . interrupt-accepted))
                    #hasheq((anchor
                             .
                             "runtime/session/session-interruption.rkt:cancel-token! token")
                            (effect . token-signal))
                    #hasheq((anchor
                             .
                             "agent/iteration/counters.rkt:turn-cancelled-event \"turn.cancelled\"")
                            (effect . uncorrelated-turn-cancelled))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:lastTurnTermination")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:finish-session-turn! sess")
                            (effect . finish-rotate))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:release-prompt! sess")
                            (effect . release-prompt))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:\"turn.cancelled\"")
                            (effect . correlated-turn-cancelled)))))
          #hasheq((family . cancel)
                  (id . cancel-midstream-direct)
                  (precondition . token-cancelled-without-recorded-request)
                  (trace
                   .
                   (#hasheq((anchor
                             .
                             "agent/stream-runner.rkt:when (stream-chunk-done? chunk")
                            (effect . chunk-processed))
                    #hasheq((anchor
                             .
                             "agent/stream-runner.rkt:cancellation-token-cancelled?")
                            (effect . token-observed))
                    #hasheq((anchor
                             .
                             "agent/stream-runner.rkt:make-stream-turn-cancelled-event")
                            (effect . stream-cancelled))
                    #hasheq((anchor
                             .
                             "agent/loop-stream.rkt:make-stream-turn-completed-event")
                            (effect . stream-completed))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:lastTurnTermination")
                            (effect . session-updated))
                    #hasheq((anchor
                             .
                             "runtime/session/session-lifecycle.rkt:when interrupt-request-id")
                            (effect . release-no-correlated-terminal)))))))
        (production-change . #f)
        (responsibility-taxonomy
         .
         (orchestration pure-preparation persistence eventing fsm wiring))
        (schema-version . 2)
        (scope . characterization-only)
        (units
         .
         (#hasheq((id . run-prompt!)
                  (owner
                   .
                   "runtime/session/session-lifecycle.rkt:(define (run-prompt!")
                  (responsibilities . (orchestration eventing fsm)))
          #hasheq((id . run-prompt-internal)
                  (owner
                   .
                   "runtime/session/session-lifecycle.rkt:(define (run-prompt-internal")
                  (responsibilities . (orchestration persistence eventing)))
          #hasheq((id . build-session-context-for-prompt)
                  (owner
                   .
                   "runtime/session/session-lifecycle.rkt:(define (build-session-context-for-prompt")
                  (responsibilities
                   .
                   (pure-preparation persistence orchestration)))
          #hasheq((id . dispatch-iteration)
                  (owner
                   .
                   "runtime/session/session-lifecycle.rkt:(define (dispatch-iteration")
                  (responsibilities . (orchestration eventing wiring)))
          #hasheq((id . run-iteration-loop/v2)
                  (owner
                   .
                   "agent/iteration/main-loop.rkt:(define (run-iteration-loop/v2")
                  (responsibilities . (orchestration fsm)))
          #hasheq((id . close-session!)
                  (owner . "runtime/agent-session.rkt:(define (close-session!")
                  (responsibilities
                   .
                   (orchestration persistence eventing fsm wiring)))
          #hasheq((id . call-with-provider-retry)
                  (owner
                   .
                   "runtime/provider-retry.rkt:(define (call-with-provider-retry")
                  (responsibilities . (orchestration eventing)))
          #hasheq((id . with-auto-retry)
                  (owner . "runtime/auto-retry.rkt:(define (with-auto-retry")
                  (responsibilities . (orchestration eventing fsm)))
          #hasheq((id . maybe-compact-context)
                  (owner
                   .
                   "runtime/compaction/session-compaction.rkt:(define (maybe-compact-context sess")
                  (responsibilities . (orchestration eventing fsm)))
          #hasheq((id . compact-session-durably!)
                  (owner
                   .
                   "runtime/session/session-events.rkt:(define (compact-session-durably!")
                  (responsibilities
                   .
                   (orchestration persistence eventing fsm wiring)))))
        (wave . W0))
