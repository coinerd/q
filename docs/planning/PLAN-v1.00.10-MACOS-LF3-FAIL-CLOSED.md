# Plan v1.00.10 — macOS LF3 Fail-Closed Repair and L4 Closure

**Status:** proposed remediation milestone
**Owner:** q maintainers
**Author:** Manus AI
**Prerequisite:** v1.00.09 merged as [PR #9418](https://github.com/coinerd/q/pull/9418); manual full regression [32479520248](https://github.com/coinerd/q/actions/runs/32479520248) completed with a macOS platform failure.

## 1. Purpose and release decision

v1.00.10 repairs a security regression in worker file-operation authorization and produces the missing clean L4 release evidence. The v1.00.09 all-lane aggregator behaved correctly: six Linux shards and the workflows lane passed, but the macOS platform lane failed and `run-summary.json` consequently reported `status: fail`. That release block remains valid until this milestone is complete.

> **Release rule:** v1.00.10 is not complete when the code compiles or Linux CI is green. It is complete only when a fresh manual `full-regression` run on the repair commit has a GitHub conclusion of `success`, a `run-summary.json.status` of `pass`, and passing evidence for all required lanes.

| Required result | v1.00.09 evidence | v1.00.10 target |
|---|---:|---:|
| Linux shard evidence | 6/6 passing | 6/6 passing |
| Workflow-suite evidence | passing | passing |
| macOS platform evidence | failing | passing |
| L4 run summary | `fail` | `pass` |
| GitHub full-regression conclusion | failure | success |

## 2. Confirmed root cause

The merged implementation in `sandbox/worker-tools.rkt` walks each existing component and stores:

```racket
(simplify-path (resolve-path candidate) #f)
```

as the next `resolved-prefix`. This is unsafe because Racket’s `resolve-path` may return a soft-link target **relative to the directory that owns the link**.[1] The state machine then treats that relative target as a complete prefix and builds subsequent components relative to the process current directory.

On the macOS runner, the temporary-directory ancestry includes compatibility-link topology. The path walk becomes relative before it reaches an attacker-controlled external link, `..` traversal, or broken-link component. The request and the allowed root are then both incorrectly represented beneath the same lexical prefix, and the final string-boundary comparison returns `#t`. This explains all six failed `check-false` assertions: external file links, external directory links with new tails, traversal, an alias-root escape, and broken links all become authorized.

The behavior was reproduced under Racket 8.10 with a relative symlinked ancestor. The current algorithm allowed four attack classes; rebasing the relative `resolve-path` result against the directory containing the link rejected all four while preserving an in-root relative link. This is a CWE-22 path-traversal issue and requires fail-closed remediation.[2] [3]

## 3. Scope and constraints

### In scope

| Area | Required outcome |
|---|---|
| `sandbox/worker-tools.rkt` | Every stored resolver prefix is complete; a relative soft-link target is rebased against the resolved link’s containing directory. |
| `tests/test-worker-security.rkt` | Deterministic relative-ancestor fixtures cover all v1.00.09 fail-open classes and the required legitimate in-root case. |
| Test architecture | A narrow pure/helper-level test proves the completeness invariant without needing a macOS runner. |
| CI validation | PR macOS platform check passes, followed by a manual all-lane full-regression run. |
| Governance | The regression issue/log records the failing and clean run URLs and preserves all retained artifacts. |

### Explicitly out of scope

The milestone must not change configured allowed-root policy, weaken the component-boundary guard, suppress the macOS lane, relabel failed evidence as acceptable, or introduce a case-folding rule for macOS. Racket treats Unix and macOS path case as filesystem-dependent; global lowercasing can conflate distinct names and does not repair a lost path root.[1]

## 4. Non-negotiable security invariants

| ID | Invariant | Acceptance condition |
|---|---|---|
| S1 | Resolver state is complete after every existing component. | `complete-path?` holds for every prefix carried into the next iteration. |
| S2 | Relative soft-link targets are interpreted relative to their owning directory. | A relative ancestor alias canonicalizes to the physical absolute path. |
| S3 | Missing or broken links fail closed. | A broken final link, or any resolver exception, yields `#f` from `path-allowed?`. |
| S4 | Authorization retains component boundaries. | `/allowed` never authorizes `/allowed-escape`. |
| S5 | External links and upward traversal are denied. | `/etc`-like links and `..` exits are rejected before any file operation. |
| S6 | Valid in-root relative links remain supported. | A link within the configured root plus a non-existent write tail is accepted. |
| S7 | All-lane L4 status remains conservative. | A failed, missing, malformed, cancelled, timed-out, or skipped required lane never yields `status: pass`. |

## 5. Implementation waves

### W0 — Characterize and lock the relative-target defect

Extract or add a narrow internal helper seam around the existing-component transition. Do not change authorization policy in this wave. The test fixture must use a **relative** target for the symlinked ancestor:

```racket
(make-file-or-directory-link (string->path "physical-parent") alias-parent)
```

This differs materially from an absolute-target alias: it forces `resolve-path` to return a non-complete target and is the missing condition that mirrored the macOS runner. Add an assertion or a helper-level test that documents the precondition and rejects any non-complete stored prefix.

**Deliverables:** a failing pre-fix characterization test, a reviewable trace comment beside the resolver, and a regression issue/log entry referencing run 32479520248 and its retained `platform.json`.

**Exit criterion:** the characterization fixture reproduces the old fail-open behavior before the repair and names the expected post-fix outcomes.

### W1 — Rebase relative `resolve-path` results

Introduce a small internal helper with exactly one responsibility:

```racket
(define (resolve-existing-component candidate)
  (define target (resolve-path candidate))
  (define complete-target
    (if (complete-path? target)
        target
        (let-values ([(base _name _must-be-dir?) (split-path candidate)])
          (path->complete-path target (or base (current-directory))))))
  (simplify-path complete-target #f))
```

Replace only the existing-component branch in `resolve-longest-prefix`:

```racket
[(or (file-exists? candidate) (directory-exists? candidate))
 (loop (cdr remaining) (resolve-existing-component candidate))]
```

Retain the existing `link-exists?` branch immediately after the ordinary existence checks. Racket distinguishes final-destination predicates from `link-exists?`; the latter is required to recognize a broken final link without following it.[4] Retain shared use of `resolve-longest-prefix` by both request and configured-root canonicalization.

**Guardrails:** do not use a string replacement, macOS-specific lowercasing, a blanket rejection of relative link targets, or a rollback to parent-only resolution. Each would either preserve the defect or reintroduce the original ancestor-alias false rejection.

**Exit criterion:** every resolver prefix that enters the next loop iteration is complete; current allowed-root semantics remain unchanged except for corrected resolution.

### W2 — Security regression matrix

Extend `tests/test-worker-security.rkt` with a single temporary fixture tree containing a relative ancestor alias, a physical allowed root, an in-root target, an outside sibling, external file/directory links, and a broken link. Parameterize `current-allowed-roots` with the lexical allowed root.

| Test case | Expected |
|---|---:|
| Relative ancestor alias + in-root relative directory link + non-existent tail | `#t` |
| Relative ancestor alias + external existing file link | `#f` |
| Relative ancestor alias + external directory link + non-existent tail | `#f` |
| Relative ancestor alias + `..` traversal into physical-parent sibling | `#f` |
| Relative ancestor alias + broken final link | `#f` |
| Direct link from allowed root to `/etc/passwd` | `#f` |
| Existing valid file below allowed root | `#t` |
| Separator-collision path (`allowed-escape`) | `#f` |

Retain the original LF3 tests in addition to these relative-target cases. The tests must clean up through `dynamic-wind` and must not rely on `/tmp` or `/var` having a specific spelling; the test-created relative alias is the portable mechanism.

**Exit criterion:** `raco test tests/test-worker-security.rkt` is green repeatedly on Racket 8.10; no test uses platform-case folding or a conditional macOS exemption.

### W3 — Layered validation and PR evidence

Run the following validation sequence before merging:

| Layer | Command or evidence | Required result |
|---|---|---|
| Compile | `raco make sandbox/worker-tools.rkt tests/test-worker-security.rkt` | success |
| Unit/security | `raco test tests/test-worker-security.rkt` | all assertions pass |
| Platform contract | `racket scripts/run-tests.rkt --suite platform --jobs 4 --profile local` | pass locally where supported |
| Workflow contract | `raco test tests/test-ci-workflows.rkt tests/test-full-regression-status.rkt` | pass |
| Repository lint | `racket scripts/lint-all.rkt` | no blocking failure |
| Pull request | macOS `test-platform` CI job | success |

The PR body must identify CWE-22 exposure, the relative-target mechanism, the no-case-folding decision, the test matrix, and the required post-merge full-regression gate. It must attach or link the failed run 32479520248 and the green replacement run once available.

**Exit criterion:** every blocking PR check is green, including the macOS platform check; the branch is then eligible for squash merge.

### W4 — Release-candidate L4 closure

After merge, dispatch:

```text
full-regression.yml
ref: main
suite: all
profile: ci
jobs-per-shard: 4
run-mutation-pilot: false
```

Inspect retained `run-summary.json`, `matrix-summary.json`, all six `results-shard-*` artifacts, `results-workflows`, and `results-platform`.

| Required all-lane field | Required value |
|---|---|
| GitHub workflow conclusion | `success` |
| `run-summary.json.schema_version` | `1.00.09` or a backwards-compatible successor |
| `run-summary.json.status` | `pass` |
| `required_lanes.linux_shards` | `collected_records: 6`, `status: pass` |
| `required_lanes.workflows` | `collected_records: 1`, `status: pass` |
| `required_lanes.platform` | `collected_records: 1`, `status: pass` |
| Platform result | `tests/test-worker-security.rkt` passes |

If any lane fails, times out, is cancelled, is skipped unexpectedly, is malformed, or is absent, the release remains blocked under the evidence-integrity protocol. Do not accept Linux-only success or manually amend `run-summary.json`.

**Exit criterion:** a clean all-lane artifact set is recorded in `docs/reports/test-regression-log.md`, linked from the remediation issue, and reviewed as the v1.00.10 closeout proof.

## 6. Sequencing and ownership

| Wave | Depends on | Primary code surface | Verification owner |
|---|---|---|---|
| W0 | none | test fixture/helper seam | security/test maintainer |
| W1 | W0 | `sandbox/worker-tools.rkt` | security maintainer |
| W2 | W1 | `tests/test-worker-security.rkt` | test maintainer |
| W3 | W1, W2 | CI checks and PR evidence | reviewer + CI |
| W4 | W3 and merge | `full-regression.yml` retained evidence | release owner |

W1 and W2 should be one atomic implementation PR because the defect is security-sensitive. W3 must not be compressed into a documentation-only review, and W4 must not be dispatched before the PR is merged to `main`.

## 7. Risks and mitigations

| Risk | Mitigation |
|---|---|
| The repair only fixes `/var`-style aliases but misses ordinary relative links. | Use a fixture-created relative ancestor alias; never depend on host `/var` semantics. |
| A fix accepts in-root links but silently permits broken or external links. | Keep all negative tests in the matrix; require `link-exists?` fail-closed coverage. |
| A macOS-only patch drifts from Linux behavior. | Make the rebasing logic platform-neutral and run it under Racket 8.10 locally plus macOS CI. |
| A green Linux run masks a platform failure. | Preserve the v1.00.09 all-lane status contract and require W4’s lane-by-lane artifact review. |
| A path string is damaged by display conversion. | Treat `path->string` as an authorization implementation detail only after complete canonicalization; evaluate a later `path->bytes` comparison improvement separately. |
| Test cleanup flakes on a platform. | Use test-owned temporary fixture roots and `dynamic-wind` cleanup; do not use fixed host paths. |

## 8. Definition of done

The v1.00.10 milestone is complete only if all statements below are true:

1. The resolver rebases a relative `resolve-path` result against the owning directory before retaining it as a prefix.
2. The six assertions that failed in run 32479520248 are green on macOS ARM64 without quarantine, skip, lowercasing, or a platform-specific bypass.
3. The portable relative-ancestor fixture proves both fail-closed external/broken/traversal rejection and valid in-root-link acceptance.
4. All blocking pull-request checks, including macOS `test-platform`, are green.
5. A new manual merged-main full-regression run has matching GitHub `success` and `run-summary.json.status: pass`, with six Linux records, one workflow record, and one macOS record all passing.
6. The regression log preserves the failed run, the repair PR, the clean run, and the lane-by-lane evidence location.

## References

[1]: https://docs.racket-lang.org/reference/Manipulating_Paths.html "Racket Reference — Manipulating Paths"
[2]: https://cwe.mitre.org/data/definitions/22.html "CWE-22: Improper Limitation of a Pathname to a Restricted Directory"
[3]: https://owasp.org/www-community/attacks/Path_Traversal "OWASP: Path Traversal"
[4]: https://docs.racket-lang.org/reference/Filesystem.html "Racket Reference — Filesystem"
