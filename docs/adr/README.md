# Architecture Decision Records (ADRs)

This directory records significant architectural decisions for the q project.

Each ADR describes a decision that was made, the context that led to it, and its consequences. ADRs are immutable once accepted — if a decision is superseded, a new ADR is created that references the original.

## Index

| ADR | Title | Status |
|-----|-------|--------|
| [0001](0001-small-trusted-core.md) | Small trusted core | Accepted |
| [0002](0002-append-only-jsonl-session-log.md) | Append-only JSONL session log | Accepted |
| [0003](0003-event-bus-architecture.md) | Event bus architecture | Accepted |
| [0004](0004-provider-abstraction.md) | Provider abstraction | Accepted |
| [0005](0005-interface-separation.md) | Interface separation | Accepted |
| [0006](0006-extension-hook-model.md) | Extension hook model | Accepted |
| [0007](0007-sandboxing-boundary.md) | Sandboxing boundary | Accepted |

## Conventions

- **Status**: `Proposed` → `Accepted` → `Deprecated` (superseded by a new ADR)
- **Format**: Title, Status, Context, Decision, Consequences
- **Template**: See [TEMPLATE.md](TEMPLATE.md)
- **Naming**: `NNNN-kebab-case-title.md`
- ADRs are not design docs — they record *why*, not *what*. For *what*, see `docs/architecture/`.
