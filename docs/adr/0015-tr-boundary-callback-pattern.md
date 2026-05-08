# ADR 0015: Typed Racket Boundary — Callback Pattern for Opaque Values

## Status: Accepted

## Context

q uses `#lang typed/racket` (TR) for stable utility modules (see ADR-0014). TR generates automatic contracts at module boundaries. When an untyped module passes a value to a typed module, TR wraps it with `any-wrap/c` — a recursive contract that tries to protect the value by checking its type.

The problem: `any-wrap/c` **cannot protect opaque values**. An opaque value is one whose type predicate is not externally inspectable — e.g. `event-bus?`, `session-config?`, or any struct whose constructor is not exported. When `any-wrap/c` encounters an opaque value, it raises:

```
any-wrap/c: Unable to protect opaque value passed as Any
```

This happened in v0.33.0 W2 when `emit-typed-event!` (untyped) got a `contract-out` with `event-bus?` as its first argument. The typed module `retry-policy.rkt` was importing `emit-session-event!` as `(-> Any String String Any Any)`. The `event-bus?` value crossed the TR boundary as `Any`, then the new `contract-out` on `emit-typed-event!` tried to check it against `event-bus?`, which failed because the struct internals were opaque to the contract system.

### Why not `#:opaque`?

TR provides `(require/typed ... [#:opaque TypeName predicate?])` for importing opaque types. This works for simple cases — the predicate crosses the boundary and the typed module can use it. However:

1. It still requires the **value itself to cross the boundary**
2. It creates a **bidirectional contract** that can fail in subtle ways
3. It doesn't scale — every new opaque type needs its own `#:opaque` declaration
4. It doesn't help when the opaque value is nested inside a hash or list

## Decision

Use the **callback pattern** for all TR boundary crossings where opaque values are involved.

Instead of passing an opaque value across the TR boundary, the **untyped caller creates a closure** that captures the opaque value and passes the **closure** (a plain procedure) to the typed module. The typed module calls the closure with simple, serializable arguments. The closure executes in untyped code, where the opaque value is fully accessible.

### Pattern structure

```racket
;; === Untyped caller ===
(define (do-something-with-bus bus session-id)
  ;; Create a closure that captures the opaque bus
  (define emit-event
    (lambda (name payload)
      (emit-session-event! bus session-id name payload)))
  ;; Pass the closure (a procedure) to the typed module
  (maybe-compact-mid-turn sess ctx session-id config
                          #:emit-event emit-event))

;; === Typed module ===
(: maybe-compact-mid-turn
   (->* (Any (Listof Any) (U String #f) (HashTable Symbol Any))
        (#:emit-event (U (-> String Any Any) #f))
        (Listof Any)))
(define (maybe-compact-mid-turn sess ctx session-id config
                                #:emit-event [emit-event #f])
  ;; ... logic ...
  (when (and emit-event session-id)
    ;; Call the callback — no opaque value crossing here
    (emit-event "context.mid-turn-over-budget" payload)))
```

### Key rules

1. **Typed functions accept callbacks, not opaque values** — Every function that needs to emit events, access a registry, or manipulate an opaque struct accepts a callback parameter
2. **Callback type is always a simple procedure** — `(-> String Any Any)` not `(-> EventBus String Any Any)`. The bus is invisible to the typed module
3. **Callers create closures at the boundary** — The closure is created in untyped code, captures the opaque value, and exposes only simple operations
4. **Test callers pass mock callbacks** — Tests can pass `(lambda (name payload) (set! events (cons (list name payload) events)))` without creating real event buses

### Canonical example: `retry-policy.rkt`

All four public functions in `runtime/iteration/retry-policy.rkt` use this pattern:

| Function | Callback | Purpose |
|----------|----------|---------|
| `estimate-mid-turn-tokens` | `#:emit-event` | Emit "context.mid-turn-over-budget" event |
| `maybe-compact-mid-turn` | `#:emit-event` | Emit event + compact context via `#:compact-proc` |
| `check-mid-turn-budget!` | `#:emit-event` | Wrapper around the two above |
| `call-with-overflow-recovery` | `#:emit-event` | Emit "context.overflow.detected" event |

```racket
;; In iteration.rkt (untyped caller)
(define (run-iteration-loop ...)
  ;; ...
  (define emit-event
    (lambda (name payload)
      (emit-typed-event! bus (make-session-event ... name payload))))
  (define compact-proc
    (lambda (ctx)
      (compact-history ctx)))
  ;; Both callbacks are plain procedures — no opaque crossing
  (maybe-compact-mid-turn sess ctx session-id config
                          #:emit-event emit-event
                          #:compact-proc compact-proc))
```

### Why this is better than `#:opaque`

| Aspect | `#:opaque` | Callback pattern |
|--------|-----------|------------------|
| Value crossing | Yes (opaque struct) | No (plain procedure) |
| Contract complexity | Bidirectional, recursive | Simple `->` type |
| Testability | Must create real struct | Mock with lambda |
| Extensibility | Per-type declaration | Works for any opaque type |
| Nested values | Fails for `hash?` containing opaque | No problem |
| Performance | Contract wrapping overhead | Procedure call overhead only |

## Consequences

### Positive

- **No `any-wrap/c` failures** — Opaque values never cross the TR boundary
- **Simpler contracts** — Callbacks are plain procedures with simple type signatures
- **Better testability** — Tests inject mock callbacks without creating real domain objects
- **Decoupled architecture** — Typed modules don't depend on untyped struct definitions
- **Extensible** — Same pattern works for any future opaque type

### Negative

- **More parameters** — Functions gain `#:emit-event` or similar callback params
- **Indirection** — Every event emission goes through a closure call
- **Callback lifecycle** — Callers must ensure closures don't outlive captured values (not an issue in q's synchronous loop)
- **Not obvious** — New contributors may not understand why we pass callbacks instead of values

### Mitigations

- Document this ADR in module headers near TR boundaries
- Use consistent naming: `#:emit-event`, `#:compact-proc`, `#:resolve-*`
- Add type aliases for common callback shapes: `(define-type Emit-Event-Fn (-> String Any Any))`

## Lessons learned

- **`any-wrap/c` is a signal, not an error to suppress** — When you see it, redesign the boundary, don't patch around it
- **Closure capture is idiomatic Racket** — This pattern leverages Racket's first-class functions rather than fighting TR's type system
- **Run `raco test` on ALL test files after TR boundary changes** — The v0.33.0 W2 regression only showed up in 4 test files; a full suite run would have caught it before merge
- **Check both source and test files before unexporting** — v0.33.7 W0a unexported `make-injection-message` without checking test consumers; the test file compiled but failed at runtime

## Related ADRs

- ADR-0014: Typed Racket Migration Strategy — When and how to use TR in q
- ADR-0003: Event Bus Architecture — The event bus that triggered the original `any-wrap/c` issue

## References

- v0.33.5 W0a: Initial callback pattern implementation (`e196fdb`)
- v0.33.6 A-02: `raise-arguments-error` guard when `#:compact-proc` is missing (`2b40bf5`)
- v0.33.7 W0a: Cleanup dead import, add `#:compact-proc` passthrough (`fe11824`)
- Racket docs: [`any-wrap/c`](https://docs.racket-lang.org/reference/any-wrap.html)
- Racket docs: [`require/typed` opaque types](https://docs.racket-lang.org/ts-reference/special-forms.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fprims..rkt%29._require%2Ftyped%29%29)
