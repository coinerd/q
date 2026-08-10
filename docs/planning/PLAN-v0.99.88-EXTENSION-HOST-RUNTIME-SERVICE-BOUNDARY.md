# Plan: v0.99.88 — Extension Host/Runtime Service Boundary

**Status:** ACTIVE — W4 in progress (decisions made; gates + release pending)
**Authority:** `.planning/Racket-Wartbarkeits-und-Architekturanalyse-q-Coding-Agent.md`
**Freeze contract:** `q/docs/architecture/maintainability-roadmap-freeze-v0.99.87.rktd`
**Plan-ID / Plan-Hash:** generated at campaign start from this immutable manifest
**Dependency:** v0.99.87 released and verified
**GitHub:** milestone #875; waves #9221–#9225
**Findings:** MA-02, MA-03, MA-04, MA-05 (MA-02/03/04 CLOSED; MA-05 CLOSED pending release)

## Goal

Isolate concrete Runtime provider/package services from Extension contexts while preserving dynamic loading, public `ctx-*` compatibility, and evidence-backed TUI bridges.

## Immutable wave map

| Wave | Title | Scope | Required gate / acceptance |
|---|---|---|---|
| W0 | Characterization and re-baselining | Characterize context, registry consumers, lifecycle, resume, GSD, dynamic loading, failures; no production change | focused Extension/Registry/GSD + Fast; all consumers classified |
| W1 | Neutral Extension Host Service Protocol | Minimal capability-oriented registry service + runtime adapter; preserve facade | contracts + registry + dynamic-require + Arch + Fast |
| W2 | Isolate Provider Registry Service | Remove direct Runtime registry import and exception | Arch + Broad + negative exception probe; behavior equivalent |
| W3 | Isolate ext-package-manager | Package lifecycle behind host/wiring adapter; no install logic in util | package + Security + Arch + Fast |
| W4 | Decide TUI exceptions and release | Decide dialog-api/ui-surface/widget-lifecycle individually | Broad + Arch + Security + Smoke + Release + review; all five old exceptions terminal and none expired |

## Broad schedule

Mandatory after W2 and W4. Fast and independent review remain required for every wave.

## Amendment policy

Wave IDs/order/goals/scope/gates are immutable after campaign start. Substantive change requires dated amendment, new plan hash, controlled campaign migration, and retained MA history.
