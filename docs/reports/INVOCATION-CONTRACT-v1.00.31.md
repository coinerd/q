# Invocation contract (workflow/action ↔ script) — v1.00.31 W1

Status: implemented; wave gates pending
Register: F1 (the v1.00.30 W4 producer declaration the called script could not execute)
Owner: v1.00.31 W1 (`campaign/v1.00.31-w1`, issue #9724)

## 1. What F1 actually was

`prepare-racket-environment/action.yml` declared:

```
racket scripts/ci/compiled-root.rkt build --out "$stage/q-compiled/trusted-root" \
       --checkout "$PWD" --trusted-label q-trusted-producer
```

`scripts/ci/compiled-root.rkt` accepted **no `--out` at all**. The producer job died with

```
compiled-root: unknown switch: --out        (exit 1, 0.7 s)
```

so the compiled-root lane could never activate, and v1.00.30 W4 was undeliverable for
weeks. The reason nobody saw it earlier is the interesting part: the only check on the
declaration was a **substring assertion**. It searched the action file for the text of the
invocation — which was present and correct. A substring assertion can only fail when the
action line changes; it cannot fail when the *script* changes. That asymmetry is the defect
class this wave removes.

Red-first evidence: `artifacts/wave-delivery-integrity/v1.00.31-w1/raw/w4-producer-repro-prefix.txt`
(the same command against the canonical checkout at the W4 head, exit 1).

## 2. What was changed

| Change | Where |
| --- | --- |
| Whole-checkout producer mode (`--out`), deriving the module list from the checkout itself, staged for atomic publish, cleaning up staging on failure, rejecting `--out` combinations with `--module`/`--final-dir` | `scripts/ci/compiled-root.rkt` |
| The declared invocation made real (guarded step, `compiled-root` input, `COMPILED_ROOT` env, publishing into the already-allowlisted `q-compiled/` prefix so no `known-safe-prefixes` widening was needed) | `.github/actions/prepare-racket-environment/action.yml` |
| Extractor/validator: declared `racket`/`raco` invocations parsed from `.github/**` and checked against each target's own `command-line` option set; fail-closed; JSON emitter for the evidence artifact | `scripts/ci/invocation-contract.rkt` |
| The F1 declaration is **executed verbatim** (argv taken from the action file, never retyped) against a scratch checkout; unknown switch / missing argument / conflicting mode are asserted fail-closed; per-module contract unchanged | `tests/test-compiled-root-workflow.rkt` |
| Every declaration in the repository is contracted, with anti-vacuity floors and an emitter-reproducibility check; wired into the `workflows` suite | `tests/test-workflow-invocation-contract.rkt` |

The CLI was extended rather than the action weakened: the producer's intent ("publish a
trusted root of the whole checkout") is the invariant, and the per-module `build`/`run`
contract, the freeze/identity refusal and the read-only publication are unchanged
(demonstrated below).

## 3. The verification model (and why it can fail)

`scripts/ci/invocation-contract.rkt` verifies each declaration in one of four honest tiers:

| Tier | Meaning | Count at this revision |
| --- | --- | --- |
| `option-set` | strong: every declared flag matched against the target's own `command-line` clauses | 11 |
| `literal-vocabulary` | weak: flag literals appearing in the target or its local requires | 44 |
| `external-provisioned` | target lives under a checkout step with `path:` (e.g. `tooling/`) | 7 |
| `not-a-script` | probes and subcommands (`racket --version`, `raco pkg …`) | 25 |

Measured matrix: **20 declaration files, 87 declarations, 0 unknown flags, 0 unverifiable
targets, 0 errors**. Tiers are recorded so that a weak verdict is never presented as a strong
one.

Three properties make the verifier falsifiable rather than decorative:

1. **Anti-vacuity floors** (`tests/test-workflow-invocation-contract.rkt`): the matrix must
   still contain ≥ 60 invocations, ≥ 8 strong-tier and ≥ 20 weak-tier verdicts. A regression
   that makes the extractor inert collapses those counts to zero while every individual
   check stays green — exactly the failure mode described in §4.
2. **Injection rehearsal** (`raw/injection-rehearsal.txt`): the F1 defect is injected into a
   throwaway copy of the CLI (the `--out` clause renamed). The contract must, and does,
   report `status=unknown-flag tier=option-set unknown-flags=(--out)`; the unmodified tree
   reports `ok` for all 87 declarations. A contract that cannot fail proves nothing.
3. **Non-escape rule**: a declaration that names a `.rkt` file may never be classified
   `not-a-script`, so the permissive tier cannot hide a script invocation.

## 4. Four silent-inertness bugs found inside this wave's own tooling

Recorded because the wave exists to remove precisely this class, and because each one looked
green:

1. **`file->list` on a `#lang` file raises.** The handler swallowed it, so the extractor
   honestly reported "no declarations" while the strong tier verified nothing. Fixed by
   skipping the shebang/`#lang` line and reading forms explicitly.
2. **`#rx` byte regexps ignore counted repetitions.** `#rx"^[0-9a-f]{64}$"` matches nothing
   (no error), so a digest assertion passed vacuously. Fixed (`#px`) and now forbidden
   mechanically by the "no silently-inert matchers" test, which scans the wave's own tooling
   for `#rx…{n…}` and `file->list`.
3. **A shebang defeats the codemod reader.** The injection rehearsal's first edit attempt
   silently found 0 matches on the copy (the CLI begins with `#!/usr/bin/env racket`).
   Worked around with the sanctioned narrow-edit fallback; reported here as a tooling
   limitation, not fixed in this wave (it is not a q runtime surface).
4. **Test metadata outside the parser's 30-line window is invisible.** Both new test files
   initially carried `@speed/@suite` below the `require` block; they then had *no tier at
   all* and the ownership matrix stayed at 997 rows while the suite grew by two files. Moving
   the tags into the opening lines made the tier-ownership check report
   `DRIFT (2)` — fail-closed drift detection working — after which the matrix was
   regenerated canonically (`999 rows`).

## 5. Evidence

### 5.0 Incident found while committing (repaired, guarded)

The repo's pre-commit hook exports `GIT_DIR` while it runs the affected tests. The git fixture in
`tests/test-compiled-root.rkt` inherited it, so its `git init` did not create a repository in the
fixture but **re-initialized the exported repository as bare** (`core.bare = true` in
`/home/user/src/q-agent/q/.git/config`), after which every worktree-bound git command in the
canonical checkout and in all worktrees failed with `this operation must be run in a work tree`.

Repaired and audited: `core.bare` true→false; canonical HEAD `ce13d038` unchanged; reflog
unchanged; **0 stray staged files**; all worktrees usable again
(`raw/hook-env-incident.txt`). Guarded in this wave: the test now sanitizes its process
environment (seven git plumbing variables removed) for the whole test, because the engine under
test shells out to git too — a fixture-only fix was measurably insufficient. Verified in both
modes: `raco test tests/test-compiled-root.rkt` ⇒ 11/11, and with `GIT_DIR`/`GIT_INDEX_FILE`
deliberately leaked ⇒ 11/11.

This is F1's class again — an inherited context silently changing what a command means — and it is
recorded rather than quietly fixed, because it is part of this wave's history.

Executed invocation (`raw/w4-producer-execution.txt`), with the argv taken from the action
file:

| Case | Result |
| --- | --- |
| dirty checkout (untracked compilation inputs) | refused, exit 1 — the freeze/identity guard still bites |
| clean scratch fixture (2 modules) | **exit 0**, `2 modules, reason: ok` |
| published manifest | `schema=q-compiled-root-manifest-1`, `label=q-trusted-producer`, `trusted=#t`, payload `sha256=49f694…092` |
| published root | read-only (no writable file), 0 leftover staging dirs beside the root |
| ill-formed declarations | `--outdir` → `unknown switch`; missing `--checkout` → `missing required option`; `--out` + `--module` → refused |

Artifacts: `artifacts/wave-delivery-integrity/v1.00.31-w1/invocation-matrix.json`
(generated by the tool, byte-identical for a relative and an absolute `--root`), the four raw
transcripts in `raw/`, and `SHA256SUMS`.

Reproduce:

```bash
# the whole contract, extracted and validated
racket scripts/ci/invocation-contract.rkt --root . --out /tmp/matrix.json

# the executed declaration + fail-closed shapes
raco test tests/test-compiled-root-workflow.rkt

# every declaration in the repository
raco test tests/test-workflow-invocation-contract.rkt

# the suite the wave wires this into
racket scripts/run-tests.rkt --suite workflows
```

## 6. Derived artifacts moved with this wave (checksum-pin discipline)

Making the declared invocation real changes files that other gates pin, so every pin was
moved the way the v1.00.29 W6 and v1.00.30 W2 waves moved theirs — with a mechanical proof
and a provenance note, never by relaxing an assertion:

| Pin | Old | New | Proof |
| --- | --- | --- | --- |
| `prepare_action_sha256` in `artifacts/ci-topology/v1.00.26-w2/dag-checkpoint.json` | `f0e46ee2…` | `608d93c5…` | `raw/action-pin-proof.txt`: deletion-free diff (+22, −0), pre-W1 file is an **ordered subsequence** of the post-W1 file, pinned dimensions unchanged, block inert by default |
| checkpoint file hash in `tests/test-ci-runtime-contract.rkt` | `e396efb0…` | `cb8efb20…` | same proof; a `w1_restamp` note records wave, old/new and reason |
| `prepare_action_sha256` literal in `tests/test-w9-ci-workflow-verification.rkt` | `f0e46ee2…` | `608d93c5…` | same, message extended with the re-stamp reason |
| `artifacts/ci-topology/v1.00.26-w2/SHA256SUMS` | `e396efb0…` | `cb8efb20…` | `sha256sum -c` OK |
| `artifacts/tier-ownership/v1.00.29-w0/ownership-matrix.json` + sibling `SHA256SUMS` | 1400 families | **1402 families** | canonical `inventory.rkt --ownership-map --tier-matrix …`; `run-tier-ownership-check` ⇒ `()` (no drift) |
| `tests/tier-ownership-matrix.json` | 997 rows | **999 rows** | canonical `scripts/tier-ownership-matrix.rkt generate`; `check` ⇒ green |
| `README.md` metrics | 187669 src lines | **188343 / 892 / 1504 / 277204 / 41964** | canonical `scripts/metrics.rkt --sync-all`; `--lint` ⇒ 5/5 |

`setup_action_sha256` is untouched: W1 does not modify `setup-racket/action.yml`.

The register harness itself moved with the wave that fixes the row it guards: F1's
`guard-status` is now produced by the shipped contract (`register-guard! "F1"` asks the
extractor + the CLI's own option set), the remaining rows stay `unguarded` with their
reproduction-liveness checks intact, and the W0-era "no guard is registered" assertion was
replaced by the per-wave statement it was always meant to become.

## 7. Residual

- `governance` (delivery ladder) remains structurally broken by the F6 resolver bug; owner is
  W4, not this wave.
- The extraction reads `command-line` clauses statically. A script that builds its option
  table dynamically would fall to the weak tier; the tier is recorded per declaration so such
  a case is visible rather than assumed strong.
- Metadata-window invisibility (§4.4) is a governance-visibility gap, not a correctness hole
  (the runner still executes the file); it is recorded as a candidate for the next legitimate
  plan revision rather than amending the frozen v1.00.31 plan.