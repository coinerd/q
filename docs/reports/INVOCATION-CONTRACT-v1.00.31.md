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
| Every declaration in the repository is contracted, with anti-vacuity floors and an emitter-reproducibility check; selected by the `workflows` suite because the suite now honours the tag its file declares | `tests/test-workflow-invocation-contract.rkt` |
| A `@suite workflows` tag was silently inert outside `tests/workflows/` — the one area predicate that ignored its own tag; it now routes, and a regression test holds the routing | `scripts/run-tests/classify-filters.rkt`, `tests/test-run-tests.rkt` |

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

The second git fixture added by this wave (`tests/test-compiled-root-workflow.rkt`) carried the same
latent exposure and did not inherit the guard, which the round-2 review caught: it too runs
`git init` inside a scratch fixture, so under the hook's full mode it could have re-initialised the
canonical checkout a second time. It now sanitizes its environment the same way, and the fix was
verified against a **decoy** `GIT_DIR` rather than the real repository: `raco test` ⇒ 11/11 in
direct and simulated-hook modes, with `core.bare` still `false` on both the canonical checkout and
the decoy afterwards (HEAD `ce13d038` unchanged).

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

Verification at the reviewed head (the wave's own verify chain, run in the worktree whose base-dir
resolves to itself):

| Gate | Command | Result |
| --- | --- | --- |
| focused | `raco test` on the eight touched/related files | **210 passed**, 0 failures |
| workflows suite | `racket scripts/run-tests.rkt --suite workflows` | **31 files / 180 tests passed** (was 29/162: the two new tests are now selected) |
| fast suite | `racket scripts/run-tests.rkt --suite fast` | **1203 files / 17909 tests passed**, 0 failures, 960 s |
| deps / lint | `scripts/check-deps.rkt`, `scripts/metrics.rkt --lint`, `scripts/tier-ownership-matrix.rkt check`, `inventory.rkt --ownership-map --check` | OK / 5 of 5 / green 999 rows / `PASS: no drift` |
| checksums | `sha256sum -c` on the three touched `SHA256SUMS` | 6/6, 1/1, 1/1 OK |

### 5.1 A defect only the real tree could show (found by this run, fixed, guarded)

The fixture-based contract test executes the declaration against a scratch checkout with two valid
modules and passed. The first run against **this** repository died:

```
load-handler: expected a `module` declaration in
  tests/metadata-discovery/fixture/tests/alpha-test.rkt
```

The checkout is full of `.rkt` files that are deliberately *not* modules — discovery-parity
fixtures such as a bare `(module+ test …)` fragment — and `raco make` aborts on them. The producer
now partitions derived inputs into compilation modules and non-module inputs (a `#lang` line,
optionally after a shebang, or an explicit `(module …)` form), skips the latter **and says how
many it skipped**, so the omission is visible rather than silent; the workflow test's fixture now
contains such a file and asserts exactly that.

This is the wave's own lesson applied to itself: an oracle that only ever sees a well-formed input
cannot tell you what happens on the real one.

### 5.2 Real-tree execution (the CI shape)

```
$ racket scripts/ci/compiled-root.rkt build --out /tmp/w1-exec/real-stage/q-compiled/trusted-root \
      --checkout <this checkout> --trusted-label q-trusted-producer
published whole-checkout compiled root: … (2386 modules, 10 non-module .rkt input(s) skipped, reason: ok)
exit code: 0 (wall clock: 269s)
producer : label=q-trusted-producer trusted=#t
sources  : 2389 module(s)
payload  : algorithm=sha256 digest=2b9f03498e37c909aa9b9b57c5025f6b8d25c0292fd57485d570b8b3da7bc338
read-only: #t (no writable file in the published root) · staging: 0 leftovers
```

`2389` in the manifest's `sources` versus `2386` derived modules is the harvest, not the build:
`collect-checkout-compilation-pairs` lists **every** `.zo` under the checkout (excluding `docs/`)
whose sibling source exists, so bytecode already produced by an earlier `raco make` travels into
the manifest with it. The derived count is what the producer was asked to compile; the manifest is
what the payload contains and what the digest covers. Named rather than smoothed over — and
harmless for identity, since every entry must still resolve to a source inside the checkout.

### 5.3 The same defect one layer up: a tag nobody consulted

Wiring the two new tests into the `workflows` suite exposed the identical failure shape a second
time. Every other area predicate (`security-file?`, `arch-file?`, `runtime-file?`,
`extensions-file?`) accepts the file's own `@suite` tag as well as a path pattern;
`workflows-file?` alone matched on the path only, so a test that declared `@suite workflows` while
living outside `tests/workflows/` selected nothing — it believed it was gated by a suite that never
ran it. That is F1 again: a declaration that looked meaningful and was inert.

The fix honours the tag for real test files (`tests/**/test-*.rkt`, so a tagged helper or fixture
still stays out) — deliberately **stricter** than its siblings, which accept the tag unconditionally —
and `tests/test-run-tests.rkt` now asserts the routing: the suite selects both new
tests and does not select the tagged helper. Measured: `--suite workflows` went from 29 to 31 files
and the tier-ownership drift gate independently recorded the change — both rows' `required_gates`
became `("fast" "workflows")` and `("slow/L4" "workflows")`, so the versioned baseline had to be
regenerated precisely because the tag had started to do something.

## 6. Derived artifacts moved with this wave (checksum-pin discipline)

Making the declared invocation real changes files that other gates pin, so every pin was
moved the way the v1.00.29 W6 and v1.00.30 W2 waves moved theirs — with a mechanical proof
and a provenance note, never by relaxing an assertion:

| Pin | Old | New | Proof |
| --- | --- | --- | --- |
| `prepare_action_sha256` in `artifacts/ci-topology/v1.00.26-w2/dag-checkpoint.json` | `f0e46ee2…` | `608d93c5…` | `raw/action-pin-proof.txt`: deletion-free diff (+22, −0), pre-W1 file is an **ordered subsequence** of the post-W1 file, pinned dimensions unchanged, block inert by default |
| checkpoint file hash in `tests/test-ci-runtime-contract.rkt` | `e396efb0…` | `bcdbc2c6…` | same proof; a `w1_restamp` note records wave, old/new and reason (the note's provenance wording was corrected in the same change, which is why the hash moved once more) |
| `prepare_action_sha256` literal in `tests/test-w9-ci-workflow-verification.rkt` | `f0e46ee2…` | `608d93c5…` | same, message extended with the re-stamp reason |
| `artifacts/ci-topology/v1.00.26-w2/SHA256SUMS` | `e396efb0…` | `bcdbc2c6…` | `sha256sum -c` OK |
| `artifacts/tier-ownership/v1.00.29-w0/ownership-matrix.json` + sibling `SHA256SUMS` | 1400 families | **1402 families** | canonical `inventory.rkt --ownership-map --tier-matrix …`; `run-tier-ownership-check` ⇒ `()` (no drift) |
| `tests/tier-ownership-matrix.json` | 997 rows | **999 rows** | canonical `scripts/tier-ownership-matrix.rkt generate`; `check` ⇒ green |
| `README.md` metrics | 187669 src lines | **188384 / 892 / 1504 / 277303 / 41971** | canonical `scripts/metrics.rkt --sync-all`; `--lint` ⇒ 5/5 |
| `artifacts/tier-ownership/v1.00.29-w0/SHA256SUMS` | `65b79131…` | `d3f04c56…` | sibling artifact regenerated by the canonical `inventory.rkt --ownership-map --tier-matrix` in the same change; `sha256sum -c` OK |

`setup_action_sha256` is untouched: W1 does not modify `setup-racket/action.yml`.

The register harness itself moved with the wave that fixes the row it guards, and the guard it
registers is a real one: `register-guard! "F1"` verifies the **live** declaration (the action's own
flags against the shipped CLI) *and* requires the contract to **refuse the defect** — it injects an
unknown flag into that same declaration and expects `unknown-flag`. Both halves must hold, so the
row fails if the CLI loses `--out` and equally if the validator stops refusing unknown flags, which
is the inertness F1 was made of. A falsification assertion in the same test hands the guard the
pre-fix declaration and asserts it reports `'not-refused`, so the guard's failure path is
observable rather than asserted. The remaining rows stay `unguarded` with their
reproduction-liveness checks intact, and the W0-era "no guard is registered" assertion was
replaced by the per-wave statement it was always meant to become.

## 7. Residual

- `governance` (delivery ladder) remains structurally broken by the F6 resolver bug; owner is
  W4, not this wave.
- The extraction reads `command-line` clauses statically. A script that builds its option
  table dynamically would fall to the weak tier; the tier is recorded per declaration so such
  a case is visible rather than assumed strong.
- Task 2 asked to ban substring-only invocation assertions. The one offender is replaced, and the
  repository-wide contract makes such an assertion worthless — it can no longer pass while the
  invocation is broken — but the ban is not mechanical: a newly written substring assertion would
  still be inert on its own. The property that is enforced is the contract, not the syntax of
  assertions; a lint over assertion *shapes* is a candidate for a later wave.
- Metadata-window invisibility (§4.4) is a governance-visibility gap, not a correctness hole
  (the runner still executes the file); it is recorded as a candidate for the next legitimate
  plan revision rather than amending the frozen v1.00.31 plan.
- The producer publishes into the already-allowlisted `q-compiled/` prefix, and
  `restore-racket-environment/action.yml` mirrors every `compiled/` directory it finds under
  `q-compiled/` — including the nested `q-compiled/trusted-root/**/compiled` this root would
  contain. Inert while `compiled-root` stays `'false'` (no workflow passes it `'true'`), but the
  activation wave (W4/W7) must narrow the `find` or move the root out of the mirrored prefix.
- `checkout-compilation-clean?` compares git's stdout and ignores its exit code. The live
  dirty-input refusal (§5.2) shows the check works in practice, but a git that fails before
  printing anything would read as "clean". The function predates this wave, so it is recorded for
  the hardening owner rather than changed here.