#hasheq((baseline . "3773e6f8933fbccf26c35642f8d1ebd1f6aaed54")
        (decision . terminal-reassessment)
        (findings
         .
         (#hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/reports/ARCHITECTURE-BASELINE-v0.99.87.md"
                    "scripts/architecture-baseline.rkt"))
                  (id . MA-01)
                  (resolved-by . "v0.99.87 W0/W4")
                  (title . "Analysis metrics partially stale"))
          #hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/architecture/dependency-policy.rktd"
                    "tests/test-extension-exception-fitness.rkt"))
                  (id . MA-02)
                  (resolved-by . "v0.99.88 W2")
                  (title . "Extension exceptions can go stale/too broad"))
          #hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/architecture/dependency-policy.rktd"
                    "docs/reports/EXTENSION-HOST-SERVICE-PROTOCOL-v0.99.88.md"))
                  (id . MA-03)
                  (resolved-by . "v0.99.88 W0-W2")
                  (title . "extensions/context.rkt session-type coupling"))
          #hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/architecture/dependency-policy.rktd"
                    "docs/reports/EXT-PACKAGE-MANAGER-ISOLATION-v0.99.88.md"))
                  (id . MA-04)
                  (resolved-by . "v0.99.88 W3")
                  (title . "ext-package-manager.rkt Runtime coupling"))
          #hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/architecture/dependency-policy.rktd"
                    "tests/test-extension-exception-fitness.rkt"))
                  (id . MA-05)
                  (resolved-by . "v0.99.88 W4")
                  (title . "Extension-TUI bridges"))
          #hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/reports/GSD-RESPONSIBILITY-EFFECT-INVENTORY-v0.99.87.md"
                    "docs/reports/PORT-INVENTORY-COMPOSITION-ROOT-v0.99.90.md"
                    "docs/reports/GSD-GOLDEN-TRACES-v0.99.89.md"
                    "docs/reports/FACADE-THINNING-RELEASE-v0.99.89.md"))
                  (id . MA-06)
                  (resolved-by . "v0.99.89 W0-W4 + v0.99.90 W0-W5")
                  (title . "GSD domain and effects hard to separate"))
          #hasheq((disposition . PARTIAL)
                  (evidence
                   .
                   ("docs/reports/PLAN-STATE-PROJECTION-KERNEL-v0.99.89.md"
                    "tests/test-gsd-atomic-projection-transaction.rkt"
                    "tests/test-gsd-campaign-repository.rkt"))
                  (follow-up
                   .
                   "W5 #9247 projection-hygiene sweep before release")
                  (id . MA-07)
                  (resolved-by
                   .
                   "v0.99.89 W2 + v0.99.90 W1/W2/W5; residual HANDOFF/STATE drift requires a W5 hygiene sweep")
                  (title . "Tracking projections can drift"))
          #hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/reports/PORT-INVENTORY-COMPOSITION-ROOT-v0.99.90.md"
                    "tests/test-gsd-end-to-end-recovery.rkt"
                    "tests/helpers/gsd-port-fakes.rkt"))
                  (id . MA-08)
                  (resolved-by . "v0.99.90 W0-W5")
                  (title
                   .
                   "GSD external effects hard to test deterministically"))
          #hasheq((disposition . CLOSED)
                  (evidence
                   .
                   ("docs/architecture/provider-hardening-terminal-v0.99.91.rktd"
                    "docs/reports/PROVIDER-HARDENING-REASSESSMENT-v0.99.91.md"))
                  (id . MA-09)
                  (resolved-by . "v0.99.91 Path B W0-W4-B")
                  (title . "Provider co-change / redundancy"))
          #hasheq((closure-proof
                   .
                   "trace-equivalent pure extraction measurably improved locality AND evidence-backed rejection closed the residual candidate")
                  (disposition . CLOSED)
                  (evidence
                   .
                   ("runtime/session/session-prompt-preparation.rkt"
                    "runtime/session/session-context-boundary.rkt"
                    "docs/architecture/orchestration-surface-reduction-v0.99.93.rktd"
                    "docs/reports/PROMPT-PREPARATION-EXTRACTION-v0.99.93.md"
                    "docs/reports/CONTEXT-BUILD-BOUNDARY-v0.99.93.md"))
                  (id . MA-10)
                  (metrics
                   .
                   #hasheq((changed-commits . 13)
                           (fan-out . 38)
                           (hotspot . 7358)
                           (loc . 566)
                           (pure-modules-testable-without-session . #t)))
                  (resolved-by . "v0.99.93 W0-W3; terminal decision at W4")
                  (title
                   .
                   "session-lifecycle.rkt bundles multiple responsibilities"))
          #hasheq((disposition . GUARDED)
                  (evidence
                   .
                   ("tests/test-arch-fitness.rkt"
                    "tests/test-arch-parameters.rkt"))
                  (id . MA-11)
                  (resolved-by . "closed invariant; re-verified W4/W5")
                  (title . "Agent-iteration / Runtime coupling"))
          #hasheq((disposition . GUARDED)
                  (evidence
                   .
                   ("tests/test-arch-parameters.rkt"
                    "tests/test-session-owned-cross-turn-state.rkt"))
                  (id . MA-12)
                  (resolved-by . "closed invariant; re-verified W2/W4/W5")
                  (title . "Hidden cross-turn state"))))
        (milestone . v0.99.93)
        (production-change . #f)
        (schema-version . 1)
        (scope . repository-wide-maintainability-reassessment)
        (wave . W4)
        (wave-findings
         .
         (#hasheq((disposition . DEFERRED)
                  (follow-up . "#9276")
                  (id . W0-F1)
                  (owner . runtime-session)
                  (severity . high)
                  (summary
                   .
                   "Prompt ownership claimed before outer dynamic-wind protection."))
          #hasheq((disposition . DEFERRED)
                  (follow-up . "#9277")
                  (id . W0-F2)
                  (owner . runtime-session)
                  (severity . medium)
                  (summary
                   .
                   "Three distinct prompt terminal identities/events."))
          #hasheq((disposition . SEPARATE_MILESTONE)
                  (follow-up . "#9278")
                  (id . W0-F3)
                  (owner . runtime-session)
                  (severity . high)
                  (summary
                   .
                   "Close does not coordinate with an active prompt."))
          #hasheq((disposition . DEFERRED)
                  (follow-up . "#9279")
                  (id . W0-F4)
                  (owner . runtime-compaction)
                  (severity . medium)
                  (summary
                   .
                   "Auto compaction start-event ownership leak; block/error completion."))
          #hasheq((disposition . DEFERRED)
                  (follow-up . "#9280")
                  (id . W0-F5)
                  (owner . runtime-retry)
                  (severity . medium)
                  (summary
                   .
                   "Retry sleep not cancellation-aware; partial metadata hidden."))
          #hasheq((disposition . CLOSED)
                  (follow-up . "none")
                  (id . W3-F1)
                  (owner . runtime-session)
                  (severity . low)
                  (summary
                   .
                   "Remaining session-lifecycle complexity is orchestration glue by design; rejected with evidence."))
          #hasheq((disposition . DEFERRED)
                  (follow-up . "#9281")
                  (id . W3-F2)
                  (owner . runtime-session)
                  (severity . low)
                  (summary
                   .
                   "Rollback prompt-scope wrapper extraction deferred (oracle regeneration required).")))))
