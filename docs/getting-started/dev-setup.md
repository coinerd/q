# Developer Setup

## Prerequisites

- Racket 8.10+
- Git
- GNU Make (optional)

## Quick Start

```bash
git clone https://github.com/coinerd/q.git
cd q
raco pkg install --auto
raco make main.rkt
```

## Running Tests

```bash
# Full suite (~1m10s on 12 cores)
racket scripts/run-tests.rkt

# Fast subset (skip slow sandbox tests)
racket scripts/run-tests.rkt --suite fast

# Single file
raco test tests/test-event-bus.rkt
```

## Useful Scripts

| Script | Purpose |
|--------|---------|
| `scripts/run-tests.rkt` | Parallel test runner |
| `scripts/lint-all.rkt` | Lint checks |
| `scripts/metrics.rkt` | Code metrics |
| `scripts/sync-version.rkt` | Version sync across docs |
| `scripts/contract-metrics.rkt` | Contract coverage report |

## Project Structure

See `BLUEPRINT/ARCHITECTURE.md` (outside repo) for the 5-layer architecture.

## Submitting Changes

1. Work in feature branches
2. Run `raco make main.rkt` before committing
3. Ensure tests pass: `racket scripts/run-tests.rkt --suite fast`
4. Open PR against `main`
