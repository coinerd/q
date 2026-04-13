# Troubleshooting

Use this page as the quick index for common setup and runtime issues.

## Common categories

- **Install failures** — Check Racket version (8.10+), verify `raco` is on PATH
- **Provider configuration** — Verify `~/.q/config.json` has correct `base-url` and `api-key-env`
- **Interface-specific issues** — TUI requires a terminal; JSON mode needs valid JSON on stdin
- **Extension behavior** — Run `q doctor` to diagnose extension loading issues
- **Sandbox/trust model** — Check `.q/safe-mode.rkt` settings and extension trust tiers

For canonical setup details, always check the [main repository docs](https://github.com/coinerd/q/tree/main/docs) first.
