# Safe Racket Editing Rules

<!-- verified-against: 1.00.31 -->

This runbook is the canonical operating guide for editing Racket-family source
files in q. It applies to `.rkt`, `.rktl`, and `.scrbl` files; q's edit-time
reader validation also protects `.rktd` files.

## Required workflow

1. Read the target file immediately before editing and copy `old-text`
   verbatim. Do not reconstruct text from memory.
2. Apply one narrow exact edit. For a multi-level nested form, use the
   whole-form replacement rules below.
3. After every successful edit, run:

   ```bash
   raco make <file>
   ```

   A focused test that compiles the edited module is an acceptable stronger
   check. Do not edit dependent files until this verification passes.
4. If the edit changed behavior, run the focused regression test before a
   broader suite.

The edit tool validates reader syntax before writing Racket-family files, but
that guard does not perform module expansion, dependency resolution, or full
compilation. `raco make <file>` remains mandatory.

## Shell scratch output

The trusted session scratch root is
`<platform-temp>/q-agent-scratch/<session-token>/`
(`/tmp/q-agent-scratch/<session-token>/` on POSIX). That session directory is
created owner-only by the runtime (`tools/scratch.rkt`) and is the same root
the structured `write`/`edit` tools and the executor prompt use, so the
absolute path the prompt advertises is the path you may redirect to. The
allocation container itself, sibling sessions, arbitrary `/tmp` paths,
repository paths and system paths are outside the sanctioned root.

The direct (in-process) bash tool treats a redirect outside that root as a
destructive command: the default `warn` policy reports a warning in the tool
result but still runs the command, while destructive blocking (the `block`
execution policy or safe mode) refuses it. The sandbox worker has no approval
channel and always refuses. Refusals identify the classifier reason, the
target and the triggering command segment, and name the sanctioned root.

```bash
racket scripts/pre-commit.rkt >/tmp/q-agent-scratch/<session-token>/pre-commit.log 2>&1
tail -20 /tmp/q-agent-scratch/<session-token>/pre-commit.log
```

Substitute the session token the prompt reported; do not invent a different
directory, and do not clean up other sessions' scratch. Do not use the scratch
exception for source edits or release evidence; move reviewed results through
the relevant structured file tool.

This exception never applies in safe mode. Safe mode is read-only: `bash`,
`edit`, `write` and `delete-lines` are blocked by its allowlist, and
`read`/`write`/`edit`/`delete-lines` all refuse the scratch root there
(project-root-bounded access only). There is no "writable scratch" in a
`--safe` run.

Remote executor requests carry no trusted scratch root (the transport does not
include it), so a remote worker has no sanctioned scratch destination at all.
That is unavailable by omission, not a fallback to the allocation container,
`/tmp`, or a coordinator-local root.

## Executor scratch source files (GSD /go)

Structured source-file probes during a /go campaign wave use that same single
trusted session root, not the repository. The prompt reports one absolute
path; executors create diagnostic scripts there with the structured `write`
tool (missing parents are created automatically after authorization) and may
run them with read-only bash. The token is allocated once per session:
retries and worker requests reuse the same root, fresh sessions get their own.

The root lives outside the checkout, so scratch never appears in delivery
evidence. A leftover `.planning/scratch/<token>/` directory from an older
session is no longer advertised or evidence-exempt by the executor;
q never deletes another session's scratch. Arbitrary `/tmp` source writes and
destructive cleanup remain blocked, and bash output redirection stays
restricted to the session scratch root described above.

The structured tools refuse this root under safe mode (see the safe-mode note
above); scratch source probes require a normal, non-safe-mode session.

## Whole-form and structural edits

When replacing a clause or other form containing nested `if`, `begin`, `case`,
`cond`, `match`, or similar forms:

- Replace the **whole form** in one edit.
- For a whole-form replacement longer than the default 2000-character
  `old-text` limit, pass `max-old-text-len` explicitly. The safe per-call
  ceiling is 2000.
- If a structural edit tool supports the operation, use it instead of manually
  rewriting enclosing delimiters.
- Never split a form's head from its tail. In particular, do not split a nested
  form into partial edits merely to fit the default size limit.

A depth-change warning is routing guidance, while the edit-time reader check is
the authoritative write guard. Re-read the enclosing form and choose either a
whole-form replacement or the structural edit tool before retrying.

## Fast recovery from an unparseable file

If an edit or another operation leaves a tracked file unparseable:

1. Stop. Do not attempt repeated manual closing-delimiter repair.
2. Review staged and unstaged changes in that file. The recovery command below
   discards both from the named path.
3. Restore the worktree and index explicitly from `HEAD`:

   ```bash
   git restore --source=HEAD --staged --worktree -- <file>
   ```

   The shorter `git restore <file>` reads from the index, not necessarily from
   `HEAD`, and therefore is safe only when the index already matches `HEAD`.
4. Re-read the restored file.
5. Re-apply the intended change as one whole-form replacement or with the
   structural edit tool.
6. Run `raco make <file>` immediately, followed by the focused regression test.

If the file contains other uncommitted changes that must be preserved, save a
separate copy or patches for both staged and unstaged changes before running the
explicit restore command; never sacrifice unrelated work silently.

## Examples

### Large whole-form replacement

Use the edit tool's explicit limit override rather than splitting the form:

```text
path: tools/example.rkt
old-text: <the complete enclosing form copied from read>
new-text: <the complete replacement form>
max-old-text-len: 2000
```

### Verification sequence

```bash
raco make tools/example.rkt
racket scripts/run-tests.rkt tests/test-example.rkt
```

## Implementation and regression anchors

- Edit implementation: `tools/builtins/edit.rkt`
- Reader and balance validation: `util/racket-source-validation.rkt`
- Balance/limit tests: `tests/test-edit-balance-guard.rkt`
- Guidance/recovery tests: `tests/test-edit-guidance-doc.rkt`
