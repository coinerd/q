# BUG0087 validation

Implementation contract: [owner correction](../architecture/owner-correction.md).
Base: `475f95c9e3ddab853ab30d6934d540e77dc6c690`. Dedicated worktree only;
no commits or live campaign/README/authority changes. README metric regeneration
is parent-owned. All grants/applications exercised here are disposable fixtures.

## Executable RED → GREEN

Before correction implementation, the new production owned-receipt fixture
committed its legitimate frozen test delta and exact generated README counters.
`raco test tests/test-gsd-generated-metadata-correction.rkt` exited 1:

```
actual: "author-session-recovery: committed author path outside frozen FILES: \"README.md\""
expected: #t
1/1 test failures
```

This is the genuine old-scope behavioral RED. Initial fixture lint-section setup
errors and the previous child's exec admission denial are not RED evidence.

The GREEN suite uses actual owned full frozen Verify, including the real pinned
metrics `--lint`, publication, receipt supersession, confirmed-publication replay
and fresh-review routing. Critical negatives cover absent/fake/stale private
authority; journal/generation/receipt/oldproof/checkout/snapshot/fence/cancellation/
origin/branch/parent drift; wrong tree/commit/blob/mode/input/generator proposals;
prose, mixed/staged/untracked dirt; live lease and author phase; hidden index and
mode dirt; arbitrary equivalent-tree commits, nonterminal/merge corrections,
reverted forbidden history (including unchanged net tree); intent/CAS/index/
worktree crashes and source races; revocation at publication; and full Verify RED
preserving the old receipt/proof and spent allowance.

## Gates

Observed passing:

- Compilation of owner protocol, deterministic interpreter, recovery, CLI,
  integration/pure/inventory tests.
- Pure generated README + responsibility inventory + existing recovery bounds:
  **24 tests** in the final combined gate.
- Author phase hook/tool admission gates passed (**22 tests** combined with
  pure/inventory); existing owned-review-proof suite: **11 tests**.
- Test lint: 0 errors (pre-existing warnings/info).
- Format lint: 0 errors/warnings after fixture-string formatting fixes.
- Tier ownership regeneration/check: green, **1053 rows**.
- `git diff --check`: green.

Final full correction integration suite: **34 tests passed, exit 0**.
Existing production lost-author recovery suite: **27 tests passed, exit 0**.
The production GREEN case was also repeated successfully after the final
branch/mode guard tightening. CLI `--help` passed and exposes only the three
explicit mutually exclusive operations.

Local raw evidence is retained in `.planning/bug0087/green-parallel.log` and
`.planning/bug0087/recovery.log`. Independent fixture setup uses three bounded
workers; actual recovery retains its production serialization and owned registry.
Preliminary integration probes were interrupted to repair test nesting/add
security cases; one 600-second probe timed out. Three corrupted-journal fixture
assertions initially asked the strict production loader to read deliberately
invalid journals; assertions were corrected to compare raw preserved bytes,
targeted reruns passed, then the complete 34-case suite passed. None of the failed
or interrupted probes is claimed as a passing gate.

## Boundary decisions and remaining work

The operator command is exactly **`--authorize <proposal digest>`**. No invented
broker grant or model tool. Only two numeric generated table cells are owned by
this fixed protocol, never prose/markers or expanded frozen FILES. Private
immutable authority is outside project state; complete source/campaign/receipt/
fence/snapshot/commit binding is mandatory. Only deterministic interpretation of
immutable Git object identities is cached, never authority or currentness.
Private index + exact branch CAS + immutable intent support process-crash replay;
no broad staging/discard. Git replacement objects/grafts, inherited redirection,
hooks/signing and hidden index flags cannot supply correction authority.

Trust assumptions match owned review proofs: trusted installed owner process,
Git object identities, and private store/ancestor write boundary. Chmod does not
sandbox arbitrary same-UID host programs. The campaign lease excludes owned
author activity, not an uncooperative external filesystem writer. Existing atomic
rename retention is not a new power-loss/fsync guarantee. Ambiguity refuses.

Parent-owned: independent security review, README metrics synchronization, final
exact-head CI and stable full-fast, reconciliation of the canonical external spec.
No live authorization/apply or `/go` resume is permitted in this child.

## Canonical external `BLUEPRINT/GSD.md` paragraph proposal

A generated README correction is a separate owner operation, never an author
permission, Verify result, review, or ambient `/go` grant. A proposal binds its
exact independently derived generated-only byte delta and terminal one-parent
commit to the authenticated old execution repository/checkout/branch/origin,
old receipt and generation, frozen campaign/wave/snapshot, journal, successor
attempt and fences, parent/tree/blob/mode identities, and pinned generator and
input identities. Only an operator inspecting that exact proposal may authorize
its digest with **`--authorize <proposal digest>`**, recording the original user
directive in protected immutable storage outside project write authority. Apply
uses a private index, durable intent/replay and compare-and-swap branch update
without broad staging or checkout cleanup. Recovery exempts only that exact
authorized terminal commit; every other commit remains frozen-FILES checked,
including reverted forbidden edits. Authority and source/campaign bindings are
revalidated before owned Verify, publication, replay and certification. Progress
still requires full frozen owned Verify and fresh independent review through
existing boundaries. The original author commit, old proof/receipt and spent
allowance remain intact.

The external canonical file was not edited or claimed validated.
