# Safe Racket Editing Rules

<!-- verified-against: 1.00.03 -->

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

## Whole-form and structural edits

When replacing a clause or other form containing nested `if`, `begin`, `case`,
`cond`, `match`, or similar forms:

- Replace the **whole form** in one edit.
- For a whole-form replacement longer than the default 500-character
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
max-old-text-len: 1200
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
