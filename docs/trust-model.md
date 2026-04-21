<!-- verified-against: v0.14.2 -->
# Trust Model

## Trust Levels

| Level      | Description | Tools Available                     |
|------------|-------------|-------------------------------------|
| Full       | Default mode | All tools                          |
| Restricted | Safe mode (`--safe`, `Q_SAFE_MODE=1`, or config `safe-mode: true`) | read, ls, grep, find only |
| Sandbox    | Future use | Same as Restricted                  |

## Activation

Safe mode activates when **ANY** of:

1. `--safe` CLI flag
2. `Q_SAFE_MODE=1` environment variable
3. `safe-mode: true` in config.json

## What's Blocked

- `bash` — shell command execution
- `edit` — file editing
- `write` — file creation/overwrite
- `firecrawl` — network access
- `extension:*` — all extension-provided tools

Additionally, file read operations are restricted to the project directory only.

## Use Cases

- Auditing unfamiliar codebases
- CI/CD pipelines
- Code review assistance
- Exploring packages before trusting them

## Implementation

The safe mode module (`q/runtime/safe-mode.rkt`) provides **query functions only**.
It does not enforce restrictions itself — callers (tool dispatch, extension loader)
check `allowed-tool?` and `allowed-path?` before executing operations.

### Key Functions

- `safe-mode?` → check if safe mode is active
- `allowed-tool?` → check if a specific tool is permitted
- `allowed-path?` → check if a file path is accessible
- `trust-level` → current trust level symbol (`'full`, `'restricted`, `'sandbox`)
- `safe-mode-config` → hash summarizing current state
