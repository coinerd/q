# impact-selector — static, explanation-only evaluator (v1.00.29 W7)

`evaluate.rkt` decides — statically, from a diff manifest and the W0 proof
graph — whether a change could be served by a narrow test selection or must
fall back to the broad tier. It **never executes tests** (there is no
test-executing code path: no subprocess, no system call, no thread, no sleep,
no network) and nothing consumes its output in CI this wave. Decisions are
shadow/replay recommendations only. See
`docs/reports/SELECTOR-CI-GOVERNANCE-v1.00.29.md` for the full design.

```text
racket scripts/impact-selector/evaluate.rkt \
  --diff-manifest F --graph F --source-sha S --out F [--budget-ms N]
```

- `--diff-manifest F` — `q.impact-diff/1`: `{"schema": "...", "source_sha":
  "...", "changes": [{"path", "change": "modified|added|deleted|renamed",
  "old_path"?}]}`
- `--graph F` — W0 proof graph `artifacts/proof-graph/v1.00.29-w0/graph.json`;
  the directory may also carry `claims.json` (normative claim inventory) and
  `selector-mapping.json` (`q.selector-mapping/1`: `coverage` entries
  test→covered sources, `area_rules` prefix→area, `uncertain_sources`
  markers). Missing claims ⇒ broaden; missing mapping entries ⇒ broaden.
- `--out F` — decision record (`q.impact-decision/1`), canonical sorted-key
  JSON. Exit 0 whenever a decision record was written; exit 2 only on bad
  *invocation* (unknown flag / missing value). Bad *data* is never a crash:
  it is a `fallback:broad` decision with a named reason.
- `--budget-ms N` — soft budget (default 60000, clamped to the 120000 ms hard
  timeout); exceedance at any stage checkpoint writes `fallback:broad` with
  reason `budget-exceeded` and `budget_ok: false`.

Dependencies: `racket/base` + `racket/contract` + `json` only. Deterministic:
byte-identical output modulo the measured `elapsed_ms` field.

## Budget

| Parameter | Value | On exceedance |
|---|---|---|
| selector/explanation p95 target | ≤ 60 s (`60000` ms) | `fallback:broad`, reason `budget-exceeded`, `budget_ok=false` — never "select less" |
| hard timeout | ≤ 120 s (`120000` ms; `--budget-ms` is clamped to this) | same as above |
| test execution in the evaluation phase | **zero** (`tests_run: 0` in every record; no executing code path exists) | n/a |
| observed on the 15-PR offline replay | p50 1 ms / p95 2 ms / max 2 ms | 0 violations |

## Fallback table (§10 classes → decisions)

First deterministic hit wins; every fallback carries the full §11.3
explanation (reason, source SHA, consumer, affected claim set,
zero-tests-run, elapsed-ms). Expected behavior under uncertainty: broaden /
fallback, **never narrower**.

| §10 class / uncertainty | Decision | `reason` |
|---|---|---|
| changed test helper | `fallback:broad` | `helper-change` |
| changed fixture helper | `fallback:broad` | `fixture-change` |
| changed runner | `fallback:broad` | `runner-change` (other scripts: `runner-helper-change`) |
| changed workflow | `fallback:broad` | `workflow-change` |
| package/dependency metadata | `fallback:broad` | `config-or-dependency-metadata` |
| dynamic require | `fallback:broad` | `dynamic-require` |
| macro-generated dependency | `fallback:broad` | `macro-generated-dependency` |
| reader/generated source | `fallback:broad` | `generated-source` |
| missing `@covers` / unknown mapping | `fallback:broad` | `missing-mapping-metadata` |
| malformed dependency graph | `fallback:broad` | `graph-parse-failure` |
| deleted / renamed source | `fallback:broad` | `deleted-or-renamed-source` |
| multi-area change | `fallback:broad` | `multi-area-change` |
| security-sensitive path (`security/`, `sandbox/`, credential/security lints) | `fallback:broad` | `security-sensitive-path` |
| platform-sensitive path (`gui`, `browser`, `interfaces`, `tui`) | `fallback:broad` | `platform-sensitive-path` |
| unknown path (no area rule) | `fallback:broad` | `unknown-path` |
| empty diff | `broad` | `empty-diff` |
| huge diff (>40 non-docs changes) | `fallback:broad` | `huge-diff` |
| timeout / resource-budget exceedance | `fallback:broad` (`budget_ok=false`) | `budget-exceeded` |
| malformed diff manifest | `fallback:broad` | `invalid-diff-manifest` |
| claims inventory unavailable | `fallback:broad` | `missing-claims-metadata` |
| docs-only change | `broad` | `docs-only-change` |

`selected` (reason `mapped-single-area`) exists only when the whole diff is a
single production area of mapped sources / changed `test-*` files with no
broadening hit; the recommended set is the union of covering test files plus
changed test files themselves.

## Threat-model coverage map (class → test case → expected outcome)

Suite: `tests/test-impact-selector-evaluate.rkt` (small inline fixture graph;
79 checks, green). Every case additionally asserts `tests_run = 0`,
`zero_test_execution = true`, budget fields, §11.3 fields, and non-selection.

| §10 class | Test case | Expected outcome |
|---|---|---|
| changed test helper | `case-helper.json` (`tests/helpers/db-util.rkt`) | `fallback:broad` / `helper-change` |
| changed fixture helper | `case-fixture.json` (`tests/fixtures/app/seed.rkt`) | `fallback:broad` / `fixture-change` |
| changed runner | `case-runner.json` (`scripts/run-tests.rkt`) | `fallback:broad` / `runner-change` |
| changed workflow | `case-workflow.json` (`.github/workflows/ci.yml`) | `fallback:broad` / `workflow-change` + workflow claim set |
| package/dependency metadata | `case-metadata.json` (`info.rkt`) | `fallback:broad` / `config-or-dependency-metadata` |
| dynamic require | `case-dynamic-require.json` (`runtime/dyn.rkt`, marker) | `fallback:broad` / `dynamic-require` |
| macro-generated dependency | `case-macro-generated.json` (`runtime/macro-gen.rkt`) | `fallback:broad` / `macro-generated-dependency` |
| reader/generated source | `case-generated.json` (`runtime/generated/expand.rkt`) | `fallback:broad` / `generated-source` |
| missing `@covers` | `case-missing-covers.json` (`runtime/unmapped.rkt`) | `fallback:broad` / `missing-mapping-metadata` |
| malformed dependency graph | `case-bad-graph.json` (corrupt graph bytes) | `fallback:broad` / `graph-parse-failure` |
| deleted source | `case-deleted.json` (`runtime/foo.rkt` deleted) | `fallback:broad` / `deleted-or-renamed-source` |
| renamed source | `case-renamed.json` (`runtime/foo.rkt` ← `runtime/old-foo.rkt`) | `fallback:broad` / `deleted-or-renamed-source` |
| multi-area change | `case-multi-area.json` (`runtime/` + `llm/`) | `fallback:broad` / `multi-area-change` |
| security-sensitive path | `case-security.json` (`security/credential-policy.rkt`) | `fallback:broad` / `security-sensitive-path` |
| platform-sensitive path | `case-platform.json` (`gui/window.rkt`) | `fallback:broad` / `platform-sensitive-path` |
| unknown path | `case-unknown.json` (`third_party/mystery/lib.xyz`) | `fallback:broad` / `unknown-path` |
| empty diff | `case-empty.json` | `broad` / `empty-diff` |
| huge diff | `case-huge.json` (50 mapped files) | `fallback:broad` / `huge-diff` |
| budget exceedance | `case-budget.json` (`--budget-ms 0`) | `fallback:broad` / `budget-exceeded`, `budget_ok=false` |
| (guard) hard-timeout clamp | `case-clamp.json` (`--budget-ms 999999`) | budget clamped to 120000; decision unchanged |
| (guard) missing claims inventory | `case-no-claims.json` (no `claims.json` sibling) | `fallback:broad` / `missing-claims-metadata` |
| (guard) docs-only change | `case-docs.json` | `broad` / `docs-only-change` |
| (control) mapped single-area source | `case-selected.json` (`runtime/foo.rkt` covered by two tests) | `selected` / `mapped-single-area`, tests = union of covering tests |
| (control) changed test file | `case-changed-test.json` | `selected` / self |
| determinism | same input ×3 | canonical output byte-identical modulo `elapsed_ms` |
| zero test execution | every record + static source scan | `tests_run = 0`, `zero_test_execution = true`; no banned identifiers in source |

## Replay evidence

`artifacts/proof-graph/v1.00.29-w7/replay-results.json` (+ `SHA256SUMS`):
15 merged squash-PRs from `origin/main` first-parent history, 177 changed
files — 1 × `selected` (6.7%), 14 × `fallback:broad` (93.3%), 0 × plain
broad, p50 1 ms / p95 2 ms, 0 budget violations, 0 tests executed.
