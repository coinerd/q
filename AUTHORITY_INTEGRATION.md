# Authority integration

`owned-review-authority` retains the legacy authority and adds `evidence-revision` (0 for legacy). A registered baseline probe adds `evidence-context`, `evidence-registration`, and `evidence-registration-sha256` (registration canonical digest); revision is 1. Copy every relative path in `publication-files`, including the probe objects.bundle, into the publication. Do not synthesize an owned-RED proof reference.

`red.reference` is the typed baseline-probe reference supplied by the engine. `red.origin` is `posthoc-reproduction`; `red.relation` is `baseline-with-test-overlay-v1`. `red.command`, `adapter-argv`, `focused-selection`, `failure-summary`, and `owner-job-id` describe retained execution, not a preimplementation run. Preserve original failure bytes.

Reviewer `red-reference` must equal this typed reference exactly. Reviewer `red-ack` is `{ "proof-sha256": <red.reference.sha256>, "raw-output-read": true }`. The expected SHA is the probe reference digest, NOT the GREEN or registration digest. Audit binding must copy `evidence-revision` and `evidence-registration-sha256`; these participate in the binding digest and reject stale acknowledgements. Legacy revision 0 omits registration hash and preserves its binding shape.

Offline validation reloads the portable GREEN and probe bundle, receipt and context, source identities, registration digest/revision, operation, focused command selection, baseline failure and candidate pass. It needs neither capture worktrees nor the owned store.

Capability API caveat: use the exact `context.repo` representation when authorizing registration (string versus path identity matters). Ledger operations and register-owned-evidence! use the campaign base; probe execution uses context.repo.

Validation logs: original /tmp/autoevidence-authority.log preserved; subsequent genuine failure logs /tmp/authority-retry.log and /tmp/authority-fixed*.log preserved. Full legacy aggregate exceeded 240 seconds; do not report it as passing.
