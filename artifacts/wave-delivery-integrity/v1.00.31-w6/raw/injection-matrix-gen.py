#!/usr/bin/env python3
"""v1.00.31 W6 injection-matrix generator (deterministic, re-runnable).

Runs the rehearsal harness (scripts/ci/inject-wave-defect.rkt), verifies the
result is canonical, writes artifacts/wave-delivery-integrity/v1.00.31-w6/
injection-matrix.json byte-identically, and regenerates SHA256SUMS over the
wave's bound files (repository-root-relative, sorted, two spaces, trailing
newline — the convention the W5 provenance lint enforces for the current wave).

The harness is deterministic: fixtures use fixed git identities and dates,
scratch paths are scrubbed to <scratch>, and the rehearsal head is the
generation-time tip (git rev-parse HEAD) — a committed artifact cannot carry its
own commit hash.

OPERATIONAL RULE (v1.00.31 W7): the recorded head must name a commit that
survives publication. A pre-squash branch tip is unfetchable once the delivery
branch is deleted, which breaks the rehearsal test's recorded-head check on every
later CI run. Re-run this generator on a published commit (or re-stamp the head in
the matrix AND this report together, then rebind SHA256SUMS) so the observation
anchor stays resolvable; the matrix content is bound by rehearsal-inputs-digest, not
by the head. The generator also keeps the human report's head literal in sync
with the matrix, so the two cannot drift. Re-running this generator on an
unchanged tree therefore reproduces the same bytes apart from the advancing head.
"""
import hashlib
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ARTDIR = os.path.dirname(HERE)
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(ARTDIR)))
HARNESS = os.path.join(ROOT, "scripts", "ci", "inject-wave-defect.rkt")
MATRIX = os.path.join(ARTDIR, "injection-matrix.json")
SUMS = os.path.join(ARTDIR, "SHA256SUMS")
BOUND = [
    "artifacts/wave-delivery-integrity/v1.00.31-w6/injection-matrix.json",
    "artifacts/wave-delivery-integrity/v1.00.31-w6/raw/extend-register-f12-f13.py",
    "artifacts/wave-delivery-integrity/v1.00.31-w6/raw/injection-matrix-gen.py",
    "artifacts/wave-delivery-integrity/v1.00.31-w6/raw/red-first-harness.txt",
    "artifacts/wave-delivery-integrity/v1.00.31-w6/raw/red-first-register-f12-f13.txt",
    "docs/reports/WAVE-INTEGRITY-REHEARSAL-v1.00.31.md",
]


def canon(obj):
    return json.dumps(obj, indent=1, sort_keys=True, ensure_ascii=True) + "\n"


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def main():
    tmp = MATRIX + ".fresh"
    result = subprocess.run(["racket", HARNESS, "--root", ROOT, "--out", tmp],
                            capture_output=True, text=True)
    if not os.path.exists(tmp):
        sys.stderr.write(result.stdout + result.stderr)
        raise SystemExit("harness produced no matrix")
    fresh = open(tmp, encoding="utf-8").read()
    parsed = json.loads(fresh)
    if fresh != canon(parsed):
        raise SystemExit("harness output is not canonical JSON")
    with open(MATRIX, "w", encoding="utf-8") as handle:
        handle.write(fresh)
    os.unlink(tmp)
    # The report prints the rehearsal head; keep it in lockstep with the matrix
    # (the test asserts the two agree, so drift fails rather than ships).
    report = os.path.join(ROOT, "docs/reports/WAVE-INTEGRITY-REHEARSAL-v1.00.31.md")
    with open(report, encoding="utf-8") as handle:
        report_text = handle.read()
    import re
    synced = re.sub(r"(Rehearsal head `)[0-9a-f]{40}(`)",
                    r"\g<1>" + parsed["rehearsal-head"] + r"\g<2>", report_text, count=1)
    if synced != report_text:
        with open(report, "w", encoding="utf-8") as handle:
            handle.write(synced)
    missing = [rel for rel in BOUND if not os.path.exists(os.path.join(ROOT, rel))]
    if missing:
        raise SystemExit("bound file is missing: %s" % ", ".join(missing))
    entries = []
    for rel in sorted(BOUND):
        entries.append("%s  %s" % (sha256_file(os.path.join(ROOT, rel)), rel))
    with open(SUMS, "w", encoding="utf-8") as handle:
        handle.write("\n".join(entries) + "\n")
    print("verdict: %s; rows: %d; refused: %d; bound files: %d"
          % (parsed["verdict"], len(parsed["rows"]),
             len(parsed["refused-rows"]), len(entries)))


if __name__ == "__main__":
    main()