# Contributing to q

Thank you for your interest in contributing to q!

## Getting Started

### Prerequisites
- Racket 8.10+
- Basic familiarity with Racket and functional programming

### Setup
```bash
git clone https://github.com/coinerd/q.git
cd q
raco pkg install --auto
```

### Running Tests
```bash
raco test tests/
```

## Development Workflow

1. Fork the repository
2. Create a feature branch: `git checkout -b my-feature`
3. Make changes with tests
4. Ensure all tests pass: `raco test tests/`
5. Submit a pull request

## Code Style

See [docs/style-guide.md](docs/style-guide.md) for detailed conventions.

Key points:
- `#lang racket/base` for library modules, `#lang racket` for entry points
- Hyphenated names, `?` for predicates, `!` for mutators
- 2-space indentation, max 120 chars per line
- `contract-out` at module boundaries only

## Testing Expectations

- Every source module should have a corresponding test file
- Use `rackunit` with `test-case` blocks
- Tests go in `q/tests/`
- Naming: `test-<module-name>.rkt`
- New features MUST include tests
- Bug fixes SHOULD include a regression test

## Pull Request Process

1. Ensure tests pass
2. Update documentation if needed
3. Add CHANGELOG.md entry under `[Unreleased]`
4. Keep PRs focused — one concern per PR
5. Be responsive to review feedback

## Reporting Issues

Use GitHub Issues. Please include:
- q version (`racket main.rkt --version`)
- Racket version (`racket --version`)
- Steps to reproduce
- Expected vs actual behavior
