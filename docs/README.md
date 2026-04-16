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
├── api-stability.md   ← API stability tier definitions
├── install.md         ← installation guide (canonical)
├── releasing.md       ← release process
├── sdk-rpc-catalog.md ← SDK/RPC interface reference
├── security.md        ← security model
├── style-guide.md     ← code style conventions
├── trust-model.md     ← trust boundaries
├── why-q.md           ← motivation and packaging roadmap
├── adr/               ← architecture decision records
│   ├── README.md
│   ├── 0001-small-trusted-core.md
│   ├── 0002-append-only-jsonl-session-log.md
│   ├── 0003-event-bus-architecture.md
│   ├── 0004-provider-abstraction.md
│   ├── 0005-interface-separation.md
│   ├── 0006-extension-hook-model.md
│   └── 0007-sandboxing-boundary.md
├── demos/             ← demo scripts and walkthroughs
├── migration/         ← migration guides for version bumps
│   └── TEMPLATE.md    ← template for new migration guides
└── tutorials/         ← builder and team setup guides
    ├── builder-tutorials.md
    └── team-setup.md
```
