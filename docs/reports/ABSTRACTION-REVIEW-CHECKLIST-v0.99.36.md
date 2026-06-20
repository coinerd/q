# Abstraction Review Checklist — v0.99.36

**Companion to:** `scripts/abstraction-audit.rkt`  
**Manual reference:** Racket Abstraction Instruction Manual

## Purpose

This checklist translates the red-flag signals from the abstraction audit
into actionable manual review questions. The audit tool provides objective
evidence; this checklist guides the human judgment.

## How to Use

1. Run `racket scripts/abstraction-audit.rkt --root .` to generate the report
2. For each module in the report, review the relevant checklist questions below
3. Document findings in the wave report or audit document

## Signal-to-Question Mapping

### struct-out Exports

**Signal:** Module uses `(struct-out ...)` in provide form.

**Review questions:**
- Does every consumer need `struct-copy` or field mutation? If not, consider explicit provides.
- Are mutable fields exposed that should be hidden behind accessor functions?
- Is the struct used as a public protocol type (justifies struct-out) or an internal implementation detail?
- Could a constructor + accessors provide the same utility without exposing the representation?

**Manual references:** §7 (Deep Modules), §9 (Information Hiding), §17 (Interfaces)

### all-defined-out Usage

**Signal:** Module uses `(all-defined-out)`.

**Review questions:**
- Is every definition in this module truly part of the public API?
- Could future internal definitions accidentally become public API?
- Would explicit provides document the intended API surface?

**Manual references:** §9 (Information Hiding), §13 (Comments)

### Parameter Usage (make-parameter / parameterize)

**Signal:** Module defines parameters or uses parameterize.

**Review questions:**
- Is the parameter used for legitimate dynamic extent (testing, configuration)?
- Or is it hiding ordinary data flow that would be clearer as function arguments?
- Does the parameter have a clear owner (the module that defines and documents it)?
- Is the default value tested?

**Manual references:** §29 (Different Layer, Different Abstraction)

### Error/Raise Density

**Signal:** Module has high `error`/`raise` call count.

**Review questions:**
- Are errors raised at the right boundary (close to the cause)?
- Do error messages include enough context for diagnosis?
- Is there a result-based alternative (return structured failure instead of raising)?
- Are expected failures (file-not-found, parse-error) handled with results rather than exceptions?

**Manual references:** §25 (Errors as Data), §26 (Partial Success)

### Handler Density (with-handlers)

**Signal:** Module has high `with-handlers` count.

**Review questions:**
- Are handlers recovering gracefully or just suppressing errors?
- Do handlers log or report the original error before recovery?
- Is the handler at the right level (not too broad)?
- Could result-based error handling replace some handler patterns?

**Manual references:** §25 (Errors as Data), §31 (Exceptions)

### I/O Mixed With Logic

**Signal:** Module mixes file/process I/O with pure function definitions.

**Review questions:**
- Can the formatting/validation logic be separated from the I/O?
- Would string-port testing reduce reliance on filesystem fixtures?
- Is the I/O at the module boundary (justified) or interleaved with logic?

**Manual references:** §16 (Shallow Module), §27 (Ports)

### Macro Usage (define-syntax / syntax-parse)

**Signal:** Module defines macros.

**Review questions:**
- Is a macro truly necessary, or would a higher-order function suffice?
- Are there expansion tests for complex macros (syntax-parse)?
- Are hygiene and introduced identifiers documented?
- What happens when the macro is used incorrectly — is the error message helpful?

**Manual references:** §31 (DSLs), §34 (Macro Hygiene), §36 (S-expression Liberation)

### Serialization Hotspots (->jsexpr / jsexpr->)

**Signal:** Module has serialization/deserialization code.

**Review questions:**
- Is serialization at the module boundary?
- Are round-trip properties tested?
- Could serialization be delegated to a shared helper?

**Manual references:** §19 (Consistency)

## Strict Mode Thresholds

| Threshold | Limit | Rationale |
|-----------|-------|-----------|
| max-lines | 800 | Modules over 800 lines likely mix concerns |
| max-exports | 40 | High export count suggests a shallow interface |
| max-require-count | 30 | High fan-out suggests too many dependencies |
| max-handler-count | 12 | High handler density may indicate error-as-control-flow |
| max-error-count | 20 | High error density may indicate missing result types |

**Note:** These thresholds are intentionally generous. The audit is advisory.
