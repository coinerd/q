# Release Backfill Policy — v0.99.40

## Context

GitHub Releases stopped appearing after v0.32.9 due to CI failures
in the `setup-racket` composite action (375/375 consecutive failures).
Tags continued to be created, but no GitHub Release objects were
published. This document establishes the policy for backfilling
missing releases.

## Historical Gap

- **Last successful release**: v0.32.9
- **Affected tags**: All tags after v0.32.9 through v0.99.38
- **Root cause**: `raco pkg install` during setup-racket compiled
  package-visible modules with errors not caught by `raco make main.rkt`
- **Remediation**: v0.99.39 (#826) fixed the F1–F4 compile errors and
  added `ci-package-setup.rkt` as a local CI-equivalent gate

## Classification

### Tier 1: Current / Latest Approved Release (Priority: Immediate)

The most recent milestone release (e.g., v0.99.40) should be published
first once the release workflow is verified working.

- Must pass all current release gates
- Must have tarball + manifest assets
- Must be verified via milestone-gate.rkt

### Tier 2: Important Milestone Boundary Tags (Priority: Candidate)

Tags representing significant milestone completions (e.g., major
feature milestones, architecture roadmap completions).

- Publish via release-repair.yml workflow in `publish` mode
- Must pass version consistency check
- Must have CHANGELOG entry
- Historical test gates are NOT rerun — release is marked as
  "backfilled" not "verified-current"

### Tier 3: Intermediate Superseded Tags (Priority: Skip)

Tags that were intermediate releases, now superseded by a later
release covering the same functionality.

- Document as superseded in CHANGELOG
- Do not backfill unless specifically requested
- No assets published

### Tier 4: Tags with Failing Historical Gates (Priority: Do Not Publish)

Tags that were created when CI was known to be failing.

- Do NOT backfill
- Do NOT mark as verified
- If backfill is needed, must rerun all gates from scratch first

## Repair Procedure

### Using release-repair.yml (GitHub Actions)

1. Navigate to Actions → Release Repair workflow
2. Click "Run workflow"
3. Enter the tag (e.g., `v0.99.40`)
4. Select mode:
   - `dry-run` (default) — verify readiness without publishing
   - `publish` — create GitHub Release if absent
   - `repair-assets` — upload missing assets to existing release
5. Monitor workflow execution
6. Verify assets after completion

### Safety Guarantees

- **Default is dry-run**: No accidental publication
- **Version mismatch refusal**: Tag version must match `util/version.rkt`
- **No tag mutation**: Existing tags are never modified or deleted
- **CHANGELOG required**: Release notes cannot be generated without it
- **No historical gate claims**: Backfilled releases are explicitly
  marked as backfilled, not gate-verified

### Emergency Override

If the repair workflow cannot be used and a release must be created
manually:

1. Ensure all current release gates pass locally:
   ```bash
   cd q/
   racket scripts/release-dry-run.rkt
   racket scripts/lint-release-readiness.rkt --strict --context tag-publish
   ```
2. Create release manually via GitHub UI or `gh release create`
3. Upload tarball and manifest assets
4. Verify with `verify_milestone_release()` from gh_helpers.py
5. Document the override in the milestone issue

## Evidence Requirements

For the final audit (W10), the following evidence must be available:

- At least one successful end-to-end release via the standard
  tag-push trigger
- At least one successful dry-run via release-repair.yml
- Verification that `close_milestone_complete` requires release
- All policy documents published and version-consistent
