<!-- verified-against: 0.25.1 --># Security Considerations

## API Key Storage

API keys are stored as plaintext in `~/.q/credentials.json` with `0600` (owner-only) permissions.

### Risks
- Keys are readable by any process running as the same user
- Not encrypted at rest
- Backup tools may copy the file

### Mitigations
- File permissions restrict access to owner only
- Environment variable alternatives available for CI/CD
- Project-level configs support per-project credentials

### Recommendations
- Use environment variables (`ANTHROPIC_API_KEY`, etc.) in CI/CD
- Avoid sharing or backing up the credentials file
- Rotate keys if the file is accidentally exposed

## Command Execution Policy (v0.25.1)

Three execution policy modes control how commands are handled:

| Mode | Behavior |
|------|----------|
| `warn` | (Default) Warn on destructive commands, execute all |
| `block` | Block destructive commands entirely (safe-mode default) |
| `allowlist` | Only pre-approved commands execute; all others blocked |

### High-Risk Pattern Detection

A subset of destructive patterns (`rm -rf`, `mkfs`, `dd of=/dev/`, `/etc/passwd` overwrites) are classified as **high-risk**. When in warn-only mode, high-risk matches inject a `[SECURITY NOTICE]` prefix into tool output.

## Environment Variable Scrubbing (v0.25.1)

All subprocess environments are sanitized before execution:

- **Default denylist**: Patterns matching `API_KEY`, `SECRET`, `TOKEN`, `PASSWORD`, `CREDENTIAL`, `AUTH`, `GH_PAT`
- **Extra denylist**: Configurable via `current-secret-scrub-denylist` — extends the default patterns
- **Allowlist**: Configurable via `current-secret-scrub-allowlist` — vars matching these patterns are preserved even if they match the denylist. Allowlist takes priority.
- **Audit logging**: Scrubbed variable names are logged at INFO level
