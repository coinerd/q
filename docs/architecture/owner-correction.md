# Separately authorized generated README correction (BUG0087)

This is an additive owner protocol, **not** an expansion of frozen `FILES`, an
extra author allowance, a Verify result, a review, or an automatic `/go` grant.
There is no runtime/model tool or interactive approval broker for it.

## Trusted operator sequence

Run the installed, reviewed owner code, not code from the candidate checkout:

```
racket scripts/gsd-owner-correction.rkt --root CAMPAIGN --plan PLAN --wave N --propose
racket scripts/gsd-owner-correction.rkt --root CAMPAIGN --plan PLAN --wave N --authorize DIGEST
racket scripts/gsd-owner-correction.rkt --root CAMPAIGN --plan PLAN --wave N --apply DIGEST
```

`--propose` prints the complete immutable proposal, including exact before/after
README bytes, parent, terminal commit, modes, blobs and trees. Inspect that exact
diff before issuing **`--authorize <proposal digest>`** under the user's BUG0087
directive. Authorization does not apply, verify, publish, review, or run `/go`.
Each command takes the existing exclusive campaign lease and refuses an active
owner/author. Direct owner APIs also require a trusted session-liveness callback;
all entry points reject an author phase. Author tool admission excludes shell,
Git, owner CLI invocation, and private authority writes.

Authority lives in the trusted embedding's private owner store (default
`~/.q/agent/owner-corrections`), outside campaign and execution repositories.
The store is not a project setting or CLI argument. Symlinked paths and public
store permissions refuse. Addressed proposal, authorization (including the exact
user directive), and intent files are immutable/private. Arbitrary project JSON,
policy settings, and approval-looking files cannot authorize anything. As with
owned review proofs, trusted store configuration and its parent directories must
remain outside model write authority; chmod is not a sandbox against an arbitrary
same-UID host process.

## Fixed narrow delta

The owner independently interprets the pinned metrics generator's rules from
committed Git inputs; it never loads or executes project generator code. Only
`Test lines` and `Test assertions` numeric table cells in the actual `Test Suite`
section may change. The other three static metrics must already match. Ambiguous
rows, noncanonical formatting, prose/marker sync, new test-file/module counts,
unrecognized generators, mixed dirt, hidden index entries, mode changes and
unsupported nonregular inputs refuse rather than broadening this contract.
To match the pinned generator's file reads, a tracked `.rkt` symlink may refer
lexically to a committed regular blob in the same tree: relative, at most 4096
UTF-8 bytes / 256 components, and single-hop only. Its target's contents count
under the link's path, independently of the target's own entry. Absolute or
root-escaping targets, dangling targets, cycles/chains, directory targets and
linked/missing/non-directory traversal components refuse; no filesystem target
or external fallback is used. The complete immutable input tree already binds
both link and target paths/modes/blob identities, independently rederived during
validation. README must still be regular; its no-links/mode guards are unchanged.
Assertions count
**lines containing `(check-`**, not occurrences. README mode and all other bytes
are preserved. The actual frozen Verify still runs its declared metrics lint;
the correction validator cannot substitute its own passing gate.

The proposal binds campaign root/plan/wave, full retained journal and generation,
old receipt and authenticated old owned proof/execution checkout, frozen snapshot
and FILES, successor attempt/coordinator fence, repository/branch/origin,
execution Git directory/index, author parent/tree, exact before/after blobs and
modes, generator identities, complete input tree, resulting tree and exact
one-parent correction commit. All pre-correction commits are checked separately
against frozen FILES, including merge-parent and reverted forbidden changes.

## Apply, replay, and recovery

Preparation uses a private index and deterministic `commit-tree`, never broad
staging, stash, discard, or project executables. Apply retains immutable intent,
rechecks source/campaign/authority, CAS-updates only the bound branch from the
exact author parent, then reconciles only the exact README index entry/bytes.
Crashes after intent, CAS, index update, or worktree update replay only from the
exact parent or exact authorized commit; unrelated dirt or head drift refuses.
Process-crash durability follows the existing proof store's atomic rename model;
it is not a new power-loss/fsync guarantee. An external uncooperative filesystem
writer is outside the exclusive campaign lease, just as for existing owner Git
operations; guards never authorize broad cleanup to handle one.

Lost-author recovery exempts only that exact terminal, single-parent commit,
with exact tree/blob/mode delta. An arbitrary equivalent-tree commit, nonterminal
correction, extra path, merge or stale authority is not exempt. Authority and
source/campaign bindings are reread before owned Verify, publication, confirmed
publication replay, and certification. Only the deterministic interpretation of
immutable Git object identities is cached; no authority or source-currentness
verdict is cached. Git replacement objects, grafts, inherited Git redirection,
hooks and signing are excluded from the correction transaction.

The original author commit, old receipt/proof, frozen FILES and spent allowance
remain intact. Verify RED leaves the running marker and old receipt unchanged.
Successful full frozen owned Verify must still pass existing publication and
receipt certification and route to a **fresh independent review**. The correction
itself makes no delivery or approval claim.
