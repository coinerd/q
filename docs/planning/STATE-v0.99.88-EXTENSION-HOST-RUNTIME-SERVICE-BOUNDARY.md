# State: v0.99.88 — Extension Host/Runtime Service Boundary

**Campaign status:** ACTIVE — W2 implemented; review + gates pending
**Plan-ID / Hash:** generated at campaign start
**Baseline SHA:** `0e78c51f` (v0.99.87 release)
**Current main:** `8214d7cd` (W1 merge)
**Current wave:** W2 — Provider-Registry-Service aus `extensions/context.rkt` isolieren (#9223)
**Next:** Broad gate + Fast + independent review; PR/CI/merge; then W3 `ext-package-manager.rkt` isolation

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE | #9249 | `61fc7057` | ✅ APPROVED (2 passes) | characterization 12/12; focused 257/257; Fast 15369 |
| W1 | ✅ DONE | #9250 | `8214d7cd` | ✅ APPROVED | protocol 11/11; focused 76/76; Arch 231/231; Fast 15382 |
| W2 | REVIEW | — | — | PENDING | dual-run 7/7; focused 149/149; Arch 232/232 |
| W3 | PENDING | — | — | — | — |
| W4 | PENDING | — | — | — | — |

## Finding state

MA-02 CLOSED (W2: context.rkt runtime import removed; exception removed);
MA-03 CLOSED (W2: provider-registry service isolated behind neutral
provider-host-service; adapter owns concrete import); MA-04 OPEN; MA-05 OPEN.

## Projection consistency

Tracked scaffolds live in `q/docs/planning/`; executor mirror in `.planning/`.
STATE/VALIDATION/wave files advance together on every transition.
