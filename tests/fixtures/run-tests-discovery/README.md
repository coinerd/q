# run-tests-discovery fixture root (v1.00.24 W5)

Tiny explicit fixture tree shaped like a repository root: a `tests/` subtree
whose members carry controlled classification metadata.

Purpose (W5): collector/classifier/sharding unit assertions in
`tests/test-run-tests-shard.rkt` operate on THIS tree via the explicit-root
seam `collect-test-files #:test-root` (alias `#:root`) — never by crawling the
live repository. The one scheduled repository-scale discovery smoke lives in
`tests/test-run-tests-repository-discovery.rkt` (slow/L4).

Members and the behaviors each one pins:

| Fixture path | Metadata | Behavior pinned |
|---|---|---|
| `tests/zeta-fast-test.rkt` | `@suite unit @speed fast @boundary unit` | explicit fast/unit-fast selection (name has no classifier pattern) |
| `tests/alpha-heuristic-test.rkt` | none | heuristic fast selection of a clean filename; classification `'heuristic` |
| `tests/theta-slow-quietly-test.rkt` | `@speed slow @suite default @boundary integration @isolation process` | slow gate via metadata on a name with NO slow heuristic pattern |
| `tests/gamma-platform-test.rkt` | `@suite platform @speed fast @boundary integration @requires fs` | platform inclusion via `@suite platform` metadata (absent from curated lists) |
| `tests/iota-tui-named-test.rkt` | `@suite tui @speed fast @boundary integration` | tui inclusion via metadata; basename misses the `test-tui-` heuristic prefix |
| `tests/eps-mutating-probe.rkt` | `@suite testing @speed fast @boundary e2e @mutates temp @isolation process` | mutating-family selection via declared process isolation |
| `tests/nested/eta-nested-test.rkt` | `@suite default @speed fast` | nested-directory discovery (one level) |
| `tests/nested/deep/zeta-deep-nested-test.rkt` | `@suite default @speed fast` | deep nested discovery (two levels), deterministic path shape |
| `tests/helpers/event-simulator.rkt` | none | `/helpers/` support-module exclusion |
| `tests/fixtures/data-fixture.rkt` | none | `/fixtures/` support-module exclusion |
| `tests/zulu-not-test.rkt` | `@not-test true` + malformed `@flarb` + invalid `@speed ultra` | `@not-test` exclusion from collection and lint; malformed/edge metadata asserted via `validate-file`/`get-file-metadata`, never collected |

Escape cases (directory symlinks, `..`-bearing containment, missing roots,
`compiled/` bytecode) are constructed at test time in throwaway temp copies of
this tree — they cannot be committed.

`tests/fixtures/` is a support path (`support-test-module?`), so no file under
this tree is ever collected or executed from the real repository root; the
non-`@not-test` members carry schema-valid metadata so the enforced
repository-wide metadata lint stays clean.
