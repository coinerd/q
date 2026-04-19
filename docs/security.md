<!-- verified-against: v0.11.2 -->
# Security Considerations

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
