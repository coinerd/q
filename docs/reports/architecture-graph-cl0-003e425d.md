# Architecture Graph Analysis

Pinned revision: `003e425d2f1206436eb9c0b84353604ce85bb082`

Derived from the architecture-baseline raw snapshot; static internal
requires only — dynamic requires and macro-expanded bindings are
unresolved by construction and are reported separately, never inferred.

## Summary

- production-modules: 977
- production-edges: 3483
- dependency-cycles: 0

## Dependency cycles

None detected in static requires.

## Closure hubs (largest transitive fan-out)

| Path | Transitive reach | Direct fan-out |
| ---- | ---------------- | -------------- |
| `main.rkt` | 617 | 12 |
| `scripts/run-benchmark.rkt` | 563 | 5 |
| `scripts/benchmark/compare.rkt` | 562 | 3 |
| `scripts/benchmark/baseline.rkt` | 561 | 1 |
| `scripts/benchmark/report.rkt` | 560 | 2 |
| `scripts/benchmark/scorer.rkt` | 559 | 4 |
| `scripts/benchmark/executor.rkt` | 558 | 17 |
| `interfaces/tui.rkt` | 521 | 3 |
| `tui/tui-init.rkt` | 520 | 28 |
| `tui/tui-render-loop.rkt` | 508 | 33 |
| `tui/message-dispatch.rkt` | 494 | 7 |
| `tui/tui-keybindings.rkt` | 491 | 18 |
| `scripts/sdk-gsd-integration-test.rkt` | 489 | 13 |
| `extensions/gsd-planning.rkt` | 484 | 25 |
| `extensions/gsd/command-handlers.rkt` | 482 | 39 |
| `tui/commands.rkt` | 478 | 31 |
| `benchmarks/run.rkt` | 471 | 11 |
| `interfaces/sdk-public.rkt` | 469 | 8 |
| `examples/sdk/04-tools.rkt` | 469 | 2 |
| `examples/sdk/05-extensions.rkt` | 469 | 2 |
| `examples/sdk/01-minimal.rkt` | 469 | 1 |
| `examples/sdk/02-custom-model.rkt` | 469 | 1 |
| `examples/sdk/03-custom-prompt.rkt` | 469 | 1 |
| `examples/sdk/07-tree.rkt` | 469 | 1 |
| `interfaces/sdk.rkt` | 468 | 2 |

## Closure dependents (largest transitive fan-in)

| Path | Transitive dependents | Direct fan-in |
| ---- | --------------------- | ------------- |
| `util/error/deprecation.rkt` | 366 | 1 |
| `util/error/errors.rkt` | 365 | 44 |
| `util/event/event.rkt` | 289 | 49 |
| `util/content/content-parts.rkt` | 286 | 69 |
| `util/telemetry.rkt` | 286 | 3 |
| `util/error/error-helpers.rkt` | 285 | 55 |
| `agent/event-structs/base.rkt` | 279 | 19 |
| `util/event/event-macro.rkt` | 278 | 18 |
| `agent/event-structs/tool-events.rkt` | 261 | 4 |
| `agent/event-structs/iteration-events.rkt` | 260 | 8 |
| `agent/event-structs/session-events.rkt` | 260 | 7 |
| `agent/event-structs/hook-events.rkt` | 260 | 4 |
| `agent/event-structs/stream-events.rkt` | 260 | 4 |
| `agent/event-structs/message-events.rkt` | 260 | 2 |
| `agent/event-structs/provider-events.rkt` | 260 | 2 |
| `agent/event-structs/turn-events.rkt` | 260 | 2 |
| `browser/events.rkt` | 259 | 2 |
| `agent/event-json-helpers.rkt` | 258 | 1 |
| `agent/event-structs/typed-event-predicates.rkt` | 258 | 1 |
| `agent/event-structs.rkt` | 257 | 4 |
| `agent/event-json.rkt` | 257 | 1 |
| `util/event/event-access.rkt` | 257 | 1 |
| `agent/event-types.rkt` | 256 | 2 |
| `util/event/event-bus.rkt` | 255 | 52 |
| `util/json/checksum.rkt` | 248 | 28 |

## Top cross-subsystem edges

| From subsystem | To subsystem | Edges |
| -------------- | ------------ | ----- |
| runtime | util | 284 |
| agent | util | 118 |
| tools | util | 94 |
| extensions | util | 90 |
| tui | util | 62 |
| tui | runtime | 45 |
| runtime | llm | 44 |
| wiring | runtime | 43 |
| gui | ui-core | 38 |
| runtime | agent | 38 |
| scripts | util | 37 |
| scripts | extensions | 34 |
| tui | ui-core | 33 |
| examples | extensions | 32 |
| interfaces | util | 25 |
| cli | util | 24 |
| tools | runtime | 23 |
| extensions | runtime | 22 |
| tui | extensions | 22 |
| agent | llm | 21 |
| scripts | runtime | 21 |
| interfaces | runtime | 20 |
| extensions | sandbox | 14 |
| extensions | scripts | 13 |
| extensions | tools | 13 |

## Subsystem summary

| Subsystem | Modules | Internal edges | Outbound | Inbound |
| --------- | ------- | -------------- | -------- | ------- |
| agent | 66 | 156 | 150 | 69 |
| benchmarks | 3 | 3 | 9 | 0 |
| browser | 11 | 16 | 10 | 12 |
| ci | 5 | 4 | 0 | 0 |
| cli | 12 | 10 | 28 | 6 |
| examples | 25 | 0 | 49 | 0 |
| extensions | 174 | 704 | 166 | 129 |
| gui | 26 | 24 | 64 | 0 |
| interfaces | 11 | 4 | 71 | 27 |
| llm | 21 | 56 | 8 | 81 |
| pkg | 1 | 0 | 3 | 1 |
| root | 3 | 0 | 12 | 0 |
| runtime | 170 | 396 | 385 | 197 |
| sandbox | 11 | 14 | 12 | 29 |
| scripts | 166 | 106 | 109 | 13 |
| skills | 9 | 7 | 7 | 7 |
| tools | 61 | 155 | 130 | 50 |
| tui | 80 | 246 | 168 | 9 |
| ui-core | 21 | 9 | 3 | 73 |
| util | 90 | 59 | 8 | 796 |
| wiring | 11 | 13 | 109 | 2 |

