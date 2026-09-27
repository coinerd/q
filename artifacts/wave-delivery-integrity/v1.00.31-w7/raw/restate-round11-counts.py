#!/usr/bin/env python3
"""Restate every live W7 surface whose counts moved, in one pass.

Round 11 added the restatement generators to the W7 manifest (W6 set the precedent
that raw/*.py generators are tracked AND bound), so the W7 manifest holds 14 entries
and the three manifests 21. That moves the totals quoted on three live surfaces plus
one path enumeration in each of two of them.

The script is IDEMPOTENT and keyed on the shapes rather than the old digits, because
restating the counts writes another generator into the manifest, which moves the
counts again. Re-running after the manifest settles therefore converges instead of
failing on a stale expectation - which is the round-9 lesson about deriving a count
before the last append, applied to the restatement itself.

The historical figures (9/9, 11/11, 16/16, 18/18) are deliberately preserved: they
are what the superseded runs and the retained chain capture actually recorded, and
round 10's remediation correctly labelled rather than deleted them.
"""
import io
import json
import re
import subprocess

REPORT = "docs/reports/WAVE-DELIVERY-INTEGRITY-RELEASE-v1.00.31.md"
PREFLIGHT = "artifacts/wave-delivery-integrity/v1.00.31-w7/release-preflight.json"
VALIDATION = "docs/reports/gsd-wave-validation/v1.00.31-w7.rktd"
W7_MANIFEST = "artifacts/wave-delivery-integrity/v1.00.31-w7/SHA256SUMS"
BASE = "50bdf7334a9161e9e930b40be0870c52a498a0b9"
PREFLIGHT_COMMIT = "b0f19d6bd22ee9ee72d9dc02eead67d8e4272a05"
CHAIN_HEAD = "e9af72fee9c652b16a017d37219d15e7eb5f9f0b"


def sh(*args):
    return subprocess.run(args, capture_output=True, text=True, check=True).stdout


def path_count(a, b=None):
    """Number of paths differing, measured against the working tree."""
    rng = f"{a}..{b}" if b else a
    out = sh("git", "diff", "--name-only", rng)
    return len([l for l in out.splitlines() if l.strip()])


# --- measured, not transcribed -------------------------------------------------
w7_entries = len([l for l in io.open(W7_MANIFEST, encoding="utf-8") if l.strip()])
w6_entries = len([l for l in io.open(
    "artifacts/wave-delivery-integrity/v1.00.31-w6/SHA256SUMS", encoding="utf-8") if l.strip()])
w0_entries = 1
total = w7_entries + w6_entries + w0_entries
w7_raw = len([l for l in io.open(W7_MANIFEST, encoding="utf-8") if "/raw/" in l])

# Enumerations measured against the working tree: the content commit is not yet made,
# so `git diff <a>` compares to exactly the tree that commit will carry.
n_pf_to_content = path_count(PREFLIGHT_COMMIT)
n_chain_to_content = path_count(CHAIN_HEAD)

print(f"measured: W7 {w7_entries} (raw {w7_raw}), W6 {w6_entries}, W0 {w0_entries}, "
      f"total {total}")
print(f"measured: {PREFLIGHT_COMMIT[:9]}..content = {n_pf_to_content} paths")
print(f"measured: {CHAIN_HEAD[:9]}..content = {n_chain_to_content} paths")

# --- 1. release report ----------------------------------------------------------
s = io.open(REPORT, encoding="utf-8").read()
s, n1 = re.subn(r"6/6 W6, \d+/\d+ W7,\n1/1 tier-ownership, \d+/\d+ entries",
                f"6/6 W6, {w7_entries}/{w7_entries} W7,\n1/1 tier-ownership, {total}/{total} entries",
                s, count=1)
s, n2 = re.subn(r"which returns \d+/\d+ OK at this commit",
                f"which returns {total}/{total} OK at this commit", s, count=1)
assert n1 == 1 and n2 == 1, f"release report shape changed (n1={n1}, n2={n2})"
assert not re.search(r"\b12/12 W7\b|\b19/19\b", s), "stale totals remain in the report"
io.open(REPORT, "w", encoding="utf-8").write(s)
print("release report restated")

# --- 2. preflight artifact ------------------------------------------------------
with io.open(PREFLIGHT, encoding="utf-8") as fh:
    doc = json.load(fh)
sv = doc["supporting-verification"]
ac = sv["artifact-checksums"]
ac, n3 = re.subn(r"6/6 W6, \d+/\d+ W7, 1/1 tier-ownership, \d+/\d+ entries",
                 f"6/6 W6, {w7_entries}/{w7_entries} W7, 1/1 tier-ownership, {total}/{total} entries",
                 ac, count=1)
ac, n4 = re.subn(r"sha256sum -c returns \d+/\d+ OK at this commit",
                 f"sha256sum -c returns {total}/{total} OK at this commit", ac, count=1)
ac, n5 = re.subn(
    r"(?:and )?to twelve when the round-9 remediation added the milestone-gate recheck capture, "
    r"(?:and to \d+ when the round-11 remediation bound the (?:restatement generator|two restatement generators) "
    r"that produced the corrected chain field, )?so the earlier 9/9, 11/11, 16/16 and 18/18 figures "
    r"are superseded here and in the release report\.",
    f"to twelve when the round-9 remediation added the milestone-gate recheck capture, and to "
    f"{w7_entries} when the round-11 remediation bound the two restatement generators that "
    f"produced the corrected chain field, so the earlier 9/9, 11/11, 16/16 and 18/18 figures "
    f"are superseded here and in the release report.", ac, count=1)
assert n3 == n4 == n5 == 1, f"preflight checksum shape changed ({n3},{n4},{n5})"
sv["artifact-checksums"] = ac

# The chain field's own enumeration grew by the generators, which live in raw/.
ch = sv["chain"]
ch, n6 = re.subn(r"forward to the W7 content head\. At the head that carries it that range is "
                 r"\d+ paths: ",
                 f"forward to the W7 content head. At the head that carries it that range is "
                 f"{n_pf_to_content} paths: ", ch, count=1)
ch, n6b = re.subn(r"the wave's total change from the chain head to the W7 content head is \d+\.",
                  f"the wave's total change from the chain head to the W7 content head is "
                  f"{n_chain_to_content}.", ch, count=1)
assert n6 == 1 and n6b == 1, f"chain enumeration shape changed ({n6},{n6b})"
for gen in ("restate-chain-field.py", "restate-round11-counts.py"):
    if gen not in ch:
        anchor = "raw/version-derivation-tests.log, release-preflight.json, "
        note = (f"{gen} (a restatement generator bound so the correction is reproducible), ")
        ch = ch.replace(anchor, anchor.replace("release-preflight.json, ", f"release-preflight.json, {note}"), 1)
ch, n7 = re.subn(r"Of all \d+, exactly one is a source or test path",
                 f"Of all {n_pf_to_content}, exactly one is a source or test path", ch, count=1)
ch, n8 = re.subn(r"the other \d+ are captures",
                 f"the other {n_pf_to_content - 1} are captures", ch, count=1)
assert n7 == 1 and n8 == 1, f"chain tail shape changed ({n7},{n8})"
sv["chain"] = ch

out = json.dumps(doc, indent=1, sort_keys=True, ensure_ascii=True) + "\n"
io.open(PREFLIGHT, "w", encoding="utf-8").write(out)
print("preflight restated")

# --- 3. validation record (a record, not a content file) ------------------------
t = io.open(VALIDATION, encoding="utf-8").read()
t, n9 = re.subn(
    r"artifacts/wave-delivery-integrity/v1\.00\.31-w7/SHA256SUMS \(\d+ (?:captures?|entries)[^)]*\)",
    f"artifacts/wave-delivery-integrity/v1.00.31-w7/SHA256SUMS ({w7_entries} entries: "
    f"{w7_raw} raw captures and bound restatement generators, plus the preflight, the "
    f"w4-recovery record and the release report)", t, count=1)
assert n9 == 1, "validation checksum shape changed"
io.open(VALIDATION, "w", encoding="utf-8").write(t)
print("validation restated")
