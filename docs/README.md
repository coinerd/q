# q/docs — Canonical Documentation Source

This directory (`q/docs/`) is the **canonical source** for all q documentation.

## Why two doc trees?

| Tree | Purpose |
|------|---------|
| `q/docs/` | **Canonical source of truth.** Updated directly. Lives alongside the code it documents. |
| `docs/` | Doc-site build input. May lag behind `q/docs/`. Files here carry a `<!-- NOTE -->` header pointing to the canonical version. |

### Rules

1. **Always edit `q/docs/` first.** Changes in `docs/` should be back-ported from here.
2. Files in `docs/` that mirror `q/docs/` content carry a header comment pointing here.
3. If a file exists only in `docs/` (e.g., site-specific layout pages), it is maintained independently.

## Directory Layout

```
q/docs/
├── README.md          ← you are here
├── install.md         ← installation guide (canonical)
├── releasing.md       ← release process
├── security.md        ← security model
├── style-guide.md     ← code style conventions
├── trust-model.md     ← trust boundaries
├── why-q.md           ← motivation and packaging roadmap
├── adr/               ← architecture decision records
│   ├── README.md
│   ├── 0001-event-first-architecture.md
│   ├── 0002-provider-dispatch-pattern.md
│   ├── 0003-safe-mode-design.md
│   └── 0004-session-journal-append-only.md
├── demos/             ← demo scripts and walkthroughs
└── tutorials/         ← builder and team setup guides
    ├── builder-tutorials.md
    └── team-setup.md
```
