# Abstraction Audit Rubric — v0.99.36

**Date:** 2026-06-20

## Scoring Dimensions

### Reward (0–3 each, max 18)

| Dimension | 0 | 1 | 2 | 3 |
|-----------|---|---|---|---|
| Change amplification | No callers | Few callers | Many callers | Cross-system |
| Cognitive load | Trivial | Moderate | Complex domain | Multiple concerns interleaved |
| Boundary clarity | Already clear | Minor leak | Mixed concerns | No clear boundary |
| Testability | Already pure | Easy to test | Needs setup | Requires full runtime |
| Invalid-use prevention | Strong contracts | Some checks | Ad hoc strings | No validation |
| Release reliability | Non-critical | Important path | Critical path | Release blocker |

### Risk (0–3 each, max 18)

| Dimension | 0 | 1 | 2 | 3 |
|-----------|---|---|---|---|
| Behavior risk | No change | Internal only | Public API | Wire protocol |
| API risk | Private | Internal | Public surface | SDK ABI |
| Dependency risk | No deps | Self-contained | Cross-module | Cross-system |
| Test fragility | Deterministic | Stable | Integration | Environment-dependent |
| Scope risk | One file | Two files | Multiple dirs | Whole project |
| Rollback cost | Trivial | Easy | Moderate | Hard |

## Classification Thresholds

- **GREEN:** reward ≥ 9 and risk ≤ 7 → implement
- **YELLOW:** reward ≥ 12 and risk 8–11 → implement behind strong tests
- **RED:** risk ≥ 12 or unclear behavior → audit/document only

## Strict Mode Thresholds (tool --strict)

| Signal | Threshold |
|--------|-----------|
| Max lines per module | 800 |
| Max exports per module | 40 |
| Max requires per module | 30 |

These thresholds are advisory defaults. Override with `--threshold-lines`, `--threshold-exports`, `--threshold-requires` if needed.

## Tool Signals → Manual Principles Mapping

| Tool Signal | Manual Principle | Action |
|-------------|------------------|--------|
| High line count | Deep modules (§6) | Consider splitting if multiple concerns |
| High export count | Information hiding (§7) | Review if all exports are needed |
| High parameter usage | Pure core/effect shell (§20) | Check if state should be explicit |
| Macro usage | Data representation (§21) | Verify function wouldn't suffice |
| struct-out | Information hiding (§7) | Consider explicit exports |
| I/O mixed with logic | Pure core/effect shell (§20) | Extract pure functions |
| Serialization hotspots | Serialization boundaries (§22) | Verify round-trip tests exist |
| Handler density | Result values vs exceptions (§16) | Consider result-based error handling |
| High require count | Dependency management | Review coupling |
