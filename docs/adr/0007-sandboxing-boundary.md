# ADR-0007: Sandboxing Boundary

## Status
Accepted

## Context
q tools execute arbitrary operations: shell commands, file reads and writes,
code evaluation. Without containment, a malicious or misbehaving prompt could
instruct the agent to damage the host system. Defense in depth is essential.

## Decision
q enforces a sandboxing boundary around tool execution:

- **Racket sandbox:** User-facing code evaluation runs in Racket's built-in
  sandbox, which restricts available modules and resource limits.
- **Subprocess limits:** Shell commands execute through a managed subprocess
  with configurable timeouts, working directory restrictions, and optional
  allow/deny lists for commands.
- **File scope:** File read/write tools operate within a configurable project
  root and refuse path traversal outside it.

The sandbox boundary is enforced at the tool layer, not at the core, keeping
the core simple and delegating containment to the appropriate level.

## Consequences
**Easier:** Defense in depth reduces the blast radius of unintended actions.
Sandbox parameters are configurable per-project. Tool authors have a clear
security contract.

**Harder:** Sandbox escape is still possible with sufficient effort—this is a
mitigation, not a guarantee. Overly strict sandboxes may break legitimate
workflows, requiring careful tuning of permissions.
