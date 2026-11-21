# CL3 Wave 2 — edit-to-correct-result workflow comparison (#9980 / #9911)

Evaluation-only artifact at revision `aaa8217f65bc93fc54a582e14738f456a8d6d3e1`. Raw samples:
[`CL3-WAVE2-aaa8217f.json`](CL3-WAVE2-aaa8217f.json). No agent-facing Racket tool, permission policy,
test-runner change or CI-policy change was made; CL4's contract-driven pilot is untouched.

## Question and method

Question: does a persistent Racket worker materially reduce the time for a coding agent to obtain
a **correct** result after editing real q code?

Three real q modules were copied into a disposable git worktree, given a small probe binding,
and edited from one returned integer to another:

| Task | Module | Before → after |
| --- | --- | --- |
| Small pure utility | `util/hash-utils.rkt` | 41 → 42 |
| Typed dependency chain | `runtime/iteration/retry-policy.rkt` | 7 → 8 |
| GSD decision module | `extensions/gsd/campaign-decision.rkt` | 3 → 4 |

Three approaches ran the same full workflow n = 10 times per task: startup, load, baseline
evaluation, source edit, recovery/reload, successful re-evaluation and cleanup. The oracle is
independent: the expected integer is written into the source fixture by the driver and every
evaluated value must equal it; error responses or stale values cannot count as success.

Quantiles use CL0-compatible linear interpolation at rank `q*(n-1)`, rounded to 0.001 ms.
Memory is `(current-memory-use 'cumulative)` (an allocation proxy, **not** OS peak RSS).
Process count is direct Racket processes started by the workflow; descendants are not enumerated.

## Full-workflow latency (ms, n = 10 per cell)

| Task | Approach | p50 | p90 | max | Processes | All correct |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| small pure | one-shot | 2201.7 | 2304.3 | 2306.2 | 2 | yes |
| small pure | persistent + reload | **791.5** | 840.7 | 859.2 | 1 | yes |
| small pure | process replacement | 1382.7 | 1441.5 | 1463.2 | 2 | yes |
| typed chain | one-shot | 7009.5 | 7431.5 | 7598.0 | 2 | yes |
| typed chain | persistent + reload | **5010.3** | 5553.3 | 5907.9 | 1 | yes |
| typed chain | process replacement | 6781.3 | 6996.1 | 7055.2 | 2 | yes |
| GSD decision | one-shot | 7961.0 | 8224.0 | 8310.0 | 2 | yes |
| GSD decision | persistent + reload | **6622.5** | 7106.0 | 7134.9 | 1 | yes |
| GSD decision | process replacement | 6923.3 | 7276.9 | 7525.1 | 2 | yes |

Reload reduces p50 by 64.0% (small), 28.5% (typed) and 16.8% (GSD) versus one-shot. Clean
process replacement reduces p50 by 37.2%, 3.3% and 13.0% respectively. The clean option's
advantage over one-shot is therefore modest on the larger components.

## Memory, compilation and persistent-state observations

| Task | One-shot memory p50 (bytes) | Reload memory p50 | Replacement memory p50 | Cold `raco make` (ms) | Warm `raco make` (ms) |
| --- | ---: | ---: | ---: | ---: | ---: |
| small pure | 348,915,640 | 265,638,976 | 232,739,728 | 983.2 | 670.0 |
| typed chain | 1,251,225,920 | 1,945,957,056 | 1,195,579,984 | 13,101.6 | 827.0 |
| GSD decision | 1,527,635,664 | 2,622,584,696 | 1,424,137,112 | 4,187.0 | 690.2 |

- Reload retained the **pre-edit value in all 30 reload trials**. The correct post-edit value
  required a namespace reset plus a fresh module load in the same worker. This is a reliable,
  reproducible stale-state observation, not an intermittent race.
- Process replacement returned the correct post-edit value in all 30 trials and never relied on
  in-process reload semantics.
- Reload's allocation proxy grew to 1.56× (typed) and 1.72× (GSD) of one-shot memory, consistent
  with a long-lived process retaining module instances.
- Compilation cache effects are large: cold-to-warm `raco make` fell 13,101.6 → 827.0 ms for the
  typed chain and 4,187.0 → 690.2 ms for the GSD module. Targeted compilation improvements may
  rival or exceed the clean worker's end-to-end gain.

## Correctness and safety boundary

- No trial counted an error response as success; mismatches would abort the benchmark. All 90
  workflows passed the oracle, so the latency figures describe correct results only.
- The benchmark edited only a disposable git worktree, which was removed after measurement.
- The Wave 1 process-tree-cleanup limitation remains: only the direct child was killed and
  reaped; descendants were not enumerated or proven cleaned up. This is an explicit blocker for
  any future production implementation.
- Output/value caps and end-to-end request deadlines are Wave 1 prototype protections, not a
  sandbox. A namespace/custodian is not a security boundary, and no permission policy was added.

## Decision: REVISE

**Not GO.** Reload is faster but silently stale in every trial; process replacement is clean but
only modestly faster on the larger components, and process-tree cleanup plus resource isolation
remain unresolved. These facts do not justify a separately reviewed production design yet.

**Not REJECT.** The clean replacement path did reduce full-workflow p50 on all three components,
and the reload path shows a real latency opportunity. The experiment still has value.

**REVISE.** Retain the worker experiment and improve the identified weak points: process-tree
cleanup, memory retention across reloads, and an explicit reload-vs-replacement state policy.
Compare future improvements against targeted compilation-cache work on the same real modules.
Production promotion remains blocked until lifecycle/security gates are independently reviewed.
CL0 fresh-context cognitive-load trials remain assigned to CL5 (#9982).
