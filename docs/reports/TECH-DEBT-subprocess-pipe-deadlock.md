# Tech Debt: Subprocess Pipe-Buffer Deadlock

**Date**: 2026-06-20  
**Discovered**: v0.99.32 W5 (#8356, PR #8362)  
**Severity**: Medium (production impact under large stdout)  
**Tracking**: v0.99.33 W1 (#8372)

## Problem

`sandbox/subprocess.rkt` `run-subprocess` (lines 260–316) deadlocks when a
child process writes more than the OS pipe buffer (~64KB on Linux) to stdout
or stderr before exiting.

### Root Cause

The execution flow is:

1. **Line 260**: `(sync/timeout effective-timeout sp)` — blocks waiting for
   the subprocess to exit.
2. **Lines 305–316**: Only *after* the subprocess exits, reads stdout/stderr
   via `read-available-bounded`.

When the child writes >64KB to stdout, the OS pipe buffer fills. The child
process blocks on its next `write()` call, waiting for the parent to drain
the pipe. But the parent is blocked on `sync/timeout` waiting for the child
to exit. Neither can make progress → **deadlock**.

### Reproduction

```bash
# Any command producing >64KB of output triggers the deadlock:
echo "bash tool call with: ls /tmp"  # /tmp on this VPS has ~2634 entries (~81KB)
```

In v0.99.32 W5, the test `test-spawn-subagent-serialization.rkt` used a mock
provider that returned a `bash` tool call with command `ls /tmp`. The test
hung for 120s (runner timeout). The workaround was changing the command to
`echo done` (9 bytes output).

### Impact

- Any user tool call (`bash`) that produces >64KB of stdout will hang the
  agent until the subprocess timeout, then return partial output with a
  timeout error code (-9).
- This affects real-world usage (e.g., `find / -name '*.log'`, large build
  output, `cat` on large files).

## Proposed Fix

Drain stdout/stderr **concurrently** with waiting for process exit, instead
of sequentially. The pattern:

```racket
;; Spawn a reader thread for stdout and stderr BEFORE syncing on process exit
(define stdout-ch (make-channel))
(define stderr-ch (make-channel))
(thread (λ () (channel-put stdout-ch (read-available-bounded stdout-in max-output))))
(thread (λ () (channel-put stderr-ch (read-available-bounded stderr-in max-output))))

;; Now wait for process exit — the reader threads can drain pipes concurrently
(define evt-result (sync/timeout effective-timeout sp))

;; Collect output from channels
(define out-str (channel-get stdout-ch))
(define err-str (channel-get stderr-ch))
```

Alternatively, use `sync` on multiple events (process + ports) to drain as
data becomes available.

### Considerations

- `read-available-bounded` uses `port-try-peek?` (non-blocking), so a thread
  wrapping it needs to poll or use `sync` on the port.
- Must handle the case where the subprocess exits quickly (readers may get
  empty reads if ports are closed by custodian shutdown).
- Timeout path must also cancel/kill reader threads.

## Workaround (Applied in v0.99.32 W5)

Changed the test mock command from `ls /tmp` (81KB) to `echo done` (9 bytes).
This avoids the deadlock for the test but does **not** fix the production bug.

## Related Issues

- v0.99.32 W5 (#8356, PR #8362) — discovered and worked around in test
- v0.99.33 W1 (#8372) — this tracking report
- `sandbox/subprocess.rkt:255-316` — the buggy code path
