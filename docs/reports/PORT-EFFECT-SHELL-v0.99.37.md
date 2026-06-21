# Port/Effect-Shell II for Audit Writer Path — v0.99.37

## W6 (#8446): Port abstraction for abstraction-audit.rkt

### Problem

`scripts/abstraction-audit.rkt` mixes I/O with analysis. The `audit-module`
function calls `file->lines` directly, making it impossible to test analysis
logic without creating temporary files on disk. The existing tests in
`test-abstraction-audit.rkt` create real fixture directories for every test
case — slow and fragile.

### Pattern Applied

Following the `lint-version-io.rkt` pattern (v0.99.36 W6), we apply the
port/effect-shell pattern to the audit scanner:

1. **Pure core extraction**: `audit-content` takes `(path text)` and returns
   a finding hash. No I/O, no side effects. All existing analysis functions
   (count-exports, count-io-effects, etc.) are already pure — they operate
   on text strings.

2. **I/O parameter injection**: A `current-audit-file->lines` parameter
   wraps `file->lines`. Default is real file I/O; tests substitute mock
   readers.

3. **Thin shell**: `audit-module` becomes a thin I/O wrapper that reads
   the file via the parameter and delegates to `audit-content`.

### Benefits

- **Testability**: Analysis logic tested without filesystem I/O
- **Speed**: Pure tests are instant vs. temp-directory setup/teardown
- **Isolation**: I/O failures don't cascade into analysis logic
- **Consistency**: Same pattern as lint-version-io.rkt (v0.99.36)

### API Surface

```racket
;; Pure: analyze text content (no I/O)
(audit-content path text) → (or/c module-finding? #f)

;; Parameterized I/O (default: file->lines)
(current-audit-file->lines) → parameter

;; Thin shell: read file via parameter, delegate to pure core
(audit-module path) → (or/c module-finding? #f)
```
