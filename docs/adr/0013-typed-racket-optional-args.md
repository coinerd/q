# ADR-0013: Typed Racket Optional Positional Arguments

## Status: Accepted

## Context

Typed Racket does not support optional positional arguments in the same way as
untyped Racket. `(define (f [x 10]) x)` compiles in `#lang racket` but NOT in
`#lang typed/racket`.

This limitation affects struct constructors in `util/event-payloads.rkt` and
`extensions/gsd/plan-types.rkt` where backward-compatible constructors need
optional trailing arguments.

## Decision

Use one of two patterns depending on context:

### Pattern 1: Rest-arg with default extraction

```racket
(: make-thing : String String * -> thing)
(define (make-thing name . rest)
  (thing name (if (null? rest) "default" (car rest))))
```

### Pattern 2: Keyword argument with default

```racket
(: make-thing : String #:done [String] -> thing)
(define (make-thing name #:done [done ""]) ...)
```

Prefer keyword args (Pattern 2) for new APIs. Use rest-arg (Pattern 1) only
when matching an existing untyped Racket signature.

## Consequences

- Callers can use `(make-thing "foo")` or `(make-thing "foo" "bar")`
- Type signature shows `String *` rather than `String String` — less self-documenting
- Rest-arg pattern only works for at most one optional arg at the end
- Keyword args are more explicit but change the calling convention
- Both patterns enforce type safety at the TR boundary

## Examples in q

- `util/event-payloads.rkt`: All constructors use required fields (no optional args needed)
- `extensions/gsd/plan-types.rkt`: `make-gsd-task` uses keyword args for optional fields
- `util/version.rkt`: Simple `(define q-version "0.22.9")` — no constructors affected
