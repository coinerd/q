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
| [0008](0008-safe-mode-enforcement.md) | Safe-mode enforcement | Accepted |
| [0009](0009-credential-redaction.md) | Credential redaction | Accepted |
| [0010](0010-streaming-port-lifecycle.md) | Streaming port lifecycle | Accepted |
| [0011](0011-gsd-state-machine-rewrite.md) | GSD state machine & architecture | Accepted |
| [0012](0012-context-manager-strategy.md) | Context manager strategy | Accepted |
| [0013](0013-typed-racket-optional-args.md) | Typed Racket optional positional arguments | Accepted |
| [0014](0014-typed-racket-migration-strategy.md) | Typed Racket migration strategy | Active |
| [0015](0015-tr-boundary-callback-pattern.md) | TR boundary callback pattern for opaque values | Accepted |
| [0016](0016-native-tui-architecture.md) | Native TUI architecture | Accepted |
| [0017](0017-gui-architecture.md) | GUI architecture | Proposed |

## Conventions

- **Status**: `Proposed` → `Accepted` → `Deprecated` (superseded by a new ADR)
- **Format**: Title, Status, Context, Decision, Consequences
- **Template**: See [TEMPLATE.md](TEMPLATE.md)
- **Naming**: `NNNN-kebab-case-title.md`
- ADRs are not design docs — they record *why*, not *what*. For *what*, see the README and source code.
