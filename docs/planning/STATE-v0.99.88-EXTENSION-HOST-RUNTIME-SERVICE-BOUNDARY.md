# State: v0.99.88 — Extension Host/Runtime Service Boundary

**Campaign status:** ACTIVE — W3 implemented; review + gates pending
**Plan-ID / Hash:** generated at campaign start
**Baseline SHA:** `0e78c51f` (v0.99.87 release)
**Current main:** `0fb0779d` (W2 merge; W3 branch `feature/v09988-w3-ext-package-manager-isolation`)
**Current wave:** W3 — `ext-package-manager.rkt` isolieren (#9224)
**Next:** independent review; PR/CI/merge; then W4 TUI exceptions + release

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE | #9249 | `61fc7057` | ✅ APPROVED (2 passes) | characterization 12/12; focused 257/257; Fast 15369 |
| W1 | ✅ DONE | #9250 | `8214d7cd` | ✅ APPROVED | protocol 11/11; focused 76/76; Arch 231/231; Fast 15382 |
| W2 | ✅ DONE | #9251 | `0fb0779d` | ✅ APPROVED | dual-run 8/8; focused 150/150; Arch 232/232; Broad 1241/17657; Fast 15390 |
| W3 | REVIEW | — | — | PENDING | E1–E7 9/9; package 51/51; Security 63/702; Arch 21/235; Fast 1056/15403 |
| W4 | PENDING | — | — | — | — |

## Finding state

MA-02 CLOSED (W2: context.rkt runtime import removed; exception removed);
MA-03 CLOSED (W2: provider-registry service isolated behind neutral
provider-host-service; adapter owns concrete import);
MA-04 CLOSED (W3: ext-package-manager isolated behind neutral
package-host-service; adapter owns runtime/package.rkt; exception removed);
MA-05 OPEN (W4: TUI bridges).

## Projection consistency

Tracked scaffolds live in `q/docs/planning/`; executor mirror in `.planning/`.
STATE/VALIDATION/wave files advance together on every transition.
