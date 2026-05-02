# ADR 0014: Typed Racket Migration Strategy

## Status: Active

## Context

q is a large Racket codebase (~200 modules). Full Typed Racket migration is impractical, but strategic typing of stable protocol modules catches bugs at compile time and documents interfaces.

## Decision

Migrate stable, struct-heavy utility modules to `#lang typed/racket` following these rules:

1. **Only migrate stable modules** — ones that haven't changed significantly in 2+ milestones
2. **Prefer struct-heavy modules** — TR adds most value for struct field type enforcement
3. **Avoid modules with heavy untyped library interaction** — `call-with-output-file`, `json`, `with-handlers` create excessive friction
4. **Use `(->* ...)` for optional args** — TR doesn't support keyword args in type annotations cleanly
5. **Use `(cast ...)` for `hash-ref` results** — TR can't infer return types from `(HashTable Symbol Any)`
6. **Use lambda defaults for `hash-ref`** — 3-arg `hash-ref` doesn't type-check; use `(hash-ref h key (lambda () default))`
7. **Use `Any` for event name fields** — callers pass both strings and symbols

## Migrated modules (v0.28.9)

- `util/event.rkt` — event envelope struct + serialization
- `util/hook-types.rkt` — hook result types + validation schemas
- `extensions/gsd/plan-types.rkt` (v0.22.8) — GSD plan/task/wave types
- `extensions/gsd/plan-validator.rkt` (v0.22.8) — plan validation

## Modules NOT migrated (and why)

- `util/errors.rkt` — extends `exn:fail` struct (TR doesn't support well)
- `util/json-helpers.rkt` — heavy `call-with-output-file` + `json` untyped interaction
- `util/event-codec.rkt` — predicate-based dispatch on untyped struct types
- `util/event-payloads.rkt` — already `#lang typed/racket` (migrated earlier)

## Consequences

- TR boundary generates auto-contracts for untyped consumers
- `make-event` enforces argument types at call sites
- Hook validation uses typed schemas
- Compile time catches increase for typed modules
