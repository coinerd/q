#!/usr/bin/env python3
"""v1.00.31 W5 artifact generator (deterministic, re-runnable).

Modes:
  --emit-f7-fixture DIR   write the F7 red fixture (the v1.00.30-w4
                          rollback-drill inconsistency shape) into DIR as a
                          declared artifact JSON; the provenance lint must
                          refuse it (typed provenance-drift).
  (default)               regenerate provenance-matrix.json and SHA256SUMS
                          for this artifact directory byte-identically.

Determinism contract: sorted keys, 1-space indent, trailing newline; no
timestamps; recorded git identities are stable inputs. Measured/timing
values never appear in the digested structure — the fixture that carries
timing values is evidence (raw/ or the emitted fixture), not the matrix.
"""
import argparse
import hashlib
import json
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ARTDIR = os.path.dirname(HERE)
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(ARTDIR)))


def canon(obj):
    return json.dumps(obj, indent=1, sort_keys=True, ensure_ascii=True) + "\n"


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True, check=True).stdout.strip()


F7_FIXTURE = {
    "schema": "prepared-env-rollback-drill@1",
    "milestone": 895,
    "wave": "W4",
    "issue": 9690,
    "note": "F7 red fixture: reconstructs the v1.00.30-w4 rollback-drill "
            "internal inconsistency (blocked-wave artifacts never landed); "
            "prose phase timings 260/245 ms contradict eager-fallback-ms "
            "247/256 ms",
    "steps": [
        {"step": 1, "name": "eager-boundary-fallback",
         "observed": "fallback resolved in 260 ms on the first attempt",
         "verdict": "pass"},
        {"step": 2, "name": "restore-path-relink",
         "observed": "restore relink completed in 245 ms",
         "verdict": "pass"}
    ],
    "timing": {
        "eager-fallback-ms": [247, 256],
        "restore-relink-ms": [245, 245]
    },
    "recorded-head": "8299409c9cf6330a85e66cbe5a9ba445bf739d27"
}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--emit-f7-fixture", metavar="DIR")
    args = ap.parse_args()

    if args.emit_f7_fixture:
        os.makedirs(args.emit_f7_fixture, exist_ok=True)
        out = os.path.join(args.emit_f7_fixture, "rollback-drill.json")
        with open(out, "w") as f:
            f.write(canon(F7_FIXTURE))
        print(out)
        return 0

    # Regenerate the matrix + SHA256SUMS for the W5 artifact directory.
    raw_dir = HERE
    raw_files = sorted(
        f for f in os.listdir(raw_dir)
        if os.path.isfile(os.path.join(raw_dir, f)) and f != "matrix-gen.py"
    )
    raw_entries = []
    for f in raw_files:
        p = os.path.join(raw_dir, f)
        raw_entries.append({
            "bytes": os.path.getsize(p),
            "path": os.path.relpath(p, ROOT),
            "sha256": sha256_file(p),
        })

    # R2: the recorded head is a PINNED OBSERVATION, not a live input.
    # Regeneration reads it from the committed matrix (when present) so the
    # artifact stays byte-identical at any later commit; a fresh matrix
    # records the generating head once.
    matrix_path_check = os.path.join(ARTDIR, "provenance-matrix.json")
    if os.path.exists(matrix_path_check):
        with open(matrix_path_check) as f:
            existing = json.load(f)
        head = existing["recorded-head"]
    else:
        head = git("rev-parse", "HEAD")
    matrix = {
        "schema": "artifact-provenance-matrix/1",
        "plan": "fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75",
        "milestone": 896,
        "wave": "W5",
        "issue": 9728,
        "recorded-head": head,
        "checks": {
            "sha256sums-binding": "per-line digest comparison plus directory coverage",
            "provenance-heads": "current-wave head/sha/commit/tree fields must resolve and be ancestors of the wave tip",
            "canonical-json": "sorted keys, 1-space indent, trailing newline; byte-identity required for the current wave",
            "cross-artifact": "prose <n> ms values inside timing-bearing artifacts must equal the structured timing.*-ms values"
        },
        "artifacts": raw_entries,
        "generator": {
            "path": os.path.relpath(os.path.join(raw_dir, "matrix-gen.py"), ROOT),
            "sha256": sha256_file(os.path.join(raw_dir, "matrix-gen.py"))
        }
    }
    matrix_path = os.path.join(ARTDIR, "provenance-matrix.json")
    with open(matrix_path, "w") as f:
        f.write(canon(matrix))

    report_rel = "docs/reports/ARTIFACT-PROVENANCE-v1.00.31.md"
    sums_targets = [matrix_path, os.path.join(raw_dir, "matrix-gen.py")] + [
        os.path.join(raw_dir, f) for f in raw_files
    ]
    if os.path.exists(os.path.join(ROOT, report_rel)):
        sums_targets.append(os.path.join(ROOT, report_rel))
    sums_lines = []
    for p in sorted(sums_targets):
        rel = os.path.relpath(p, ROOT)
        sums_lines.append(f"{sha256_file(p)}  {rel}")
    with open(os.path.join(ARTDIR, "SHA256SUMS"), "w") as f:
        f.write("\n".join(sums_lines) + "\n")
    print(matrix_path)
    return 0


if __name__ == "__main__":
    sys.exit(main())
