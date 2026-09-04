# session-template — immutable minimal session fixture (W6)

Minimum valid durable-session shape used by
`tests/helpers/private-fixture-templates.rkt`:

    session/tmpl-seed-0001/session.jsonl   one v2 session-info header line

Rules:

- The template root and every file under it are read-only (enforced by the
  helper after first use and asserted by
  tests/test-private-fixture-templates.rkt).
- Consumers get a private, writable COPY per test instance; the template
  itself is never handed out for mutation.
- `session.index` is intentionally absent: it is derived state rebuilt by the
  runtime on resume (`build-index!`).
- The header line must satisfy `runtime/session-store/versioning.rkt`
  (session-info entry whose meta.version is CURRENT-SESSION-VERSION, i.e. 2).
