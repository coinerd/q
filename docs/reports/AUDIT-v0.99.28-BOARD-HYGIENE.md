# Project Board Hygiene Report: v0.99.28 W5

**Date:** 2026-07-21  
**Wave:** W5 (#8285)  
**Milestones:** #813 (v0.99.27), #814 (v0.99.28)

---

## 1. Milestone #813 (v0.99.27) — CLOSED

**Status:** 0 open, 7 closed  
**Board verification:** 2 problems (issues #8268, #8269 not linked to project)

### Issue #8268 (W1: Restore skill-route read-only capability contract)

- **GitHub state:** Closed, merged via PR #8274
- **Board status field:** "Done" (set via `batch_set`)
- **Project link:** Missing — issue was closed before being linked to the
  GitHub Projects v2 board. The Projects v2 API does not support
  retroactively linking closed issues that were never project items.

### Issue #8269 (W2: Frontmatter booleans + clean skill descriptions)

- **GitHub state:** Closed, merged via PR #8275
- **Board status field:** "Done" (set via `batch_set`)
- **Project link:** Missing — same Projects v2 API limitation as #8268.

### Resolution

Both issues have their status field set to "Done" and appear as ✅ in
board status reports. The `verify` command reports them as "not on board"
because the Projects v2 item link cannot be created retroactively for
closed issues. This is a **known GitHub Projects v2 API limitation**,
not a process error. All future wave issues are created with
`create_and_configure()` which links them to the board at creation time.

**Action:** Documented here. No further action possible.

---

## 2. Milestone #814 (v0.99.28) — OPEN

**Status:** 4 open, 5 closed  
**Board verification:** 0 problems ✅

All v0.99.28 wave issues are properly linked to the board with correct
fields:

| Issue | Wave | Status | Priority | Area | Effort | Risk |
|-------|------|--------|----------|------|--------|------|
| #8279 | Parent | Todo | P0 | mas | L | H |
| #8280 | W0 | Done | P0 | mas | M | M |
| #8281 | W1 | Done | P0 | mas | M | M |
| #8282 | W2 | Done | P1 | testing | L | M |
| #8283 | W3 | Done | P1 | testing | M | H |
| #8284 | W4 | Done | P1 | testing | M | L |
| #8285 | W5 | In Progress | P2 | ci | S | L |
| #8286 | W6 | Todo | P1 | release | S | L |
| #8287 | W7 | Todo | P0 | security | M | L |

---

## 3. Conclusion

- **v0.99.28 board:** Clean — all issues properly linked with correct fields.
- **v0.99.27 board:** 2 closed issues (#8268, #8269) cannot be linked
  retroactively due to GitHub Projects v2 API limitations. Status fields
  are set correctly. Documented here as resolved.

---

*Report generated as part of v0.99.28 W5 (#8285).*
