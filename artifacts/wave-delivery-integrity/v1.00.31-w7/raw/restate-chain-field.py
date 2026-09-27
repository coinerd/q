#!/usr/bin/env python3
"""Restate supporting-verification.chain in the W7 release-preflight artifact.

Round 11 finding W7-R11-F1. The field asserted that the 16-path range was the
delta between the chain head and the preflight commit, and named an endpoint
(28f39f30d) as "the head named inside verify-chain.txt". Neither is true:

  * verify-chain.txt names exactly one SHA, e9af72fee, and cannot name the commit
    that carries it.
  * b0f19d6bd (this file's own `commit` field) is a DESCENDANT of the chain head,
    not the other way round, and the delta between them is 5 paths.
  * 28f39f30d is neither the chain head nor this artifact's commit nor the content
    head: it is an intermediate commit on the way, the round-9 remediation.

So the field mixed three ranges and the count happened to survive, because 28f39f30d
sits on the P..C segment and P..C is also sixteen paths.

The restatement below deliberately does NOT name the content head. Naming it would
recreate the trap this wave has fallen into repeatedly: the artifact is bound by
SHA256SUMS, its digest feeds the evidence binding, and committing it moves the head it
names. It instead names the two anchors that are stable - the base of the range and
this artifact's own commit - gives the count, and points at the field that is the
authority for the live head.
"""
import json
import io
import os

PATH = "artifacts/wave-delivery-integrity/v1.00.31-w7/release-preflight.json"
CHAIN_HEAD = "e9af72fee9c652b16a017d37219d15e7eb5f9f0b"
PREFLIGHT_COMMIT = "b0f19d6bd22ee9ee72d9dc02eead67d8e4272a05"
STALE_ENDPOINT = "28f39f30d1ade0d74635561ff2cc02a28a29059e"

NEW = (
    "raw/verify-chain.txt is the single W7 verification capture. It names exactly one "
    "commit, {chain}, and cannot name the commit that carries it, so the two are stated "
    "separately rather than conflated: each capture names the tree it was actually run "
    "against. This artifact's own `commit` field is {pf}, which is a DESCENDANT of the "
    "chain head, not an ancestor - the chain ran first. Measured, at this head: "
    "`git diff --name-only {chain}..{pf}` is 5 paths, and the wave's total change from "
    "the chain head to the W7 content head is 17. "
    "The sixteen-path figure quoted in earlier revisions of this field is NOT the delta "
    "between those two commits - that delta is 5. It is the delta from this artifact's "
    "own commit {pf} forward to the W7 content head, and the paths are: "
    ".chainlogs/w7-version-derive-tests.log (deleted), .gitignore, README.md, "
    "artifacts/wave-delivery-integrity/v1.00.31-w7/SHA256SUMS, "
    "raw/focused-release-tests.txt, raw/format-compile-canonicality.txt, "
    "raw/milestone-gate-recheck.txt, raw/preflight-locally-runnable.txt, "
    "raw/verify-chain.txt, raw/version-derivation-tests.log, release-preflight.json, "
    "docs/reports/WAVE-DELIVERY-INTEGRITY-RELEASE-v1.00.31.md, the three "
    "docs/reports/gsd-wave-* records, and tests/test-milestone-gate.rkt. "
    "Relative to the earlier 13-path enumeration of this field, the three paths added by "
    "the round-9 remediation of W7-R9-F4 are tests/test-milestone-gate.rkt (comment-only), "
    "raw/milestone-gate-recheck.txt (the capture re-running it), and README.md (whose "
    "metrics marker the reword forced stale, regenerated canonically by scripts/metrics.rkt). "
    "Of all sixteen, exactly one is a source or test path - tests/test-milestone-gate.rkt - "
    "so the chain's subject matter is unaffected and the count moved for a comment; the "
    "other fifteen are captures, reports and records, none of which any test reads. "
    "This field deliberately does not name the content head: doing so would make the "
    "artifact claim a head that committing it moves. The live content head is bound in "
    "docs/reports/gsd-wave-evidence/v1.00.31-w7.rktd under `implementation-sha` and "
    "`merge-authorization.head`, and those fields are the authority for it, not this one. "
    "Three earlier revisions of this field each restated a count that was wrong at the head "
    "carrying it: a prose-and-JSON glob that excluded the .rktd records, then twelve paths "
    "followed by fourteen named files, then a correction aimed at the pre-remediation "
    "endpoint 9841fed8f, then this one - which kept the right count while naming the wrong "
    "commit as its endpoint and calling that commit the chain head. A count that survives "
    "four revisions is not thereby correct, and the defect that survived longest here was "
    "never arithmetic: it was a sentence that mixed two ranges and a stale endpoint, and "
    "no gate reads prose. Review round 1 finding 4 asked for one coherent head, and the "
    "honest form of that is a named anchor plus an explicit account of the evidence-only "
    "commits that follow the content head, not a claim that two runs at different commits "
    "happened at the same commit."
).format(chain=CHAIN_HEAD, pf=PREFLIGHT_COMMIT)

with io.open(PATH, encoding="utf-8") as fh:
    raw = fh.read()
doc = json.loads(raw)

before = doc["supporting-verification"]["chain"]
assert before != NEW, "chain field already restated"
assert STALE_ENDPOINT in before, "expected the stale endpoint in the old field"
# The old field reached the chain head only by reference ("the head named inside
# verify-chain.txt") and never named it, which is part of why the two ranges got
# mixed: nothing in the sentence pinned which commit the diff ran from.
assert CHAIN_HEAD not in before, "old field already named the chain head"
assert "the head named inside verify-chain.txt" in before

doc["supporting-verification"]["chain"] = NEW

# Canonical JSON: sorted keys, one-space indent, ASCII escaping, trailing newline.
out = json.dumps(doc, indent=1, sort_keys=True, ensure_ascii=True) + "\n"
with io.open(PATH, "w", encoding="utf-8") as fh:
    fh.write(out)

print("restated supporting-verification.chain")
print("  old chars:", len(before), " new chars:", len(NEW))
print("  stale endpoint present after restatement:",
      STALE_ENDPOINT in json.loads(out)["supporting-verification"]["chain"])
print("  chain head named as the endpoint:",
      json.loads(out)["supporting-verification"]["chain"].count(CHAIN_HEAD))
