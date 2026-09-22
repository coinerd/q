#!/usr/bin/env python3
"""Canonical generator for precondition-matrix.json (v1.00.31 W3).

Parses raw/red-first-evidence.txt (the verbatim typed verdicts captured by
raw/capture-probe.rkt.txt) and emits one JSON field per evidence line — no
field is renamed, dropped or summarised. Run from the repository root:

    python3 artifacts/wave-delivery-integrity/v1.00.31-w3/raw/matrix-gen.py
"""
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
RAW = HERE / "red-first-evidence.txt"
OUT = HERE.parent / "precondition-matrix.json"


def parse(raw: str):
    sections = {}
    current = None
    for line in raw.splitlines():
        m = re.match(r"=== (\S+) ===", line)
        if m:
            current = m.group(1)
            sections[current] = {}
            continue
        if current and ": " in line:
            key, value = line.split(": ", 1)
            sections[current][key] = value
    return sections


def main():
    raw = RAW.read_text()
    sections = parse(raw)
    matrix = {
        "plan-id": "fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75",
        "wave": "W3",
        "issue": 9726,
        "captured-from": "raw/red-first-evidence.txt",
        "generator": "raw/matrix-gen.py (this file); every field below is the verbatim typed verdict line",
    }
    for name, fields in sections.items():
        matrix[name] = fields
    OUT.write_text(json.dumps(matrix, indent=2) + "\n")
    print(f"wrote {OUT} ({sum(len(v) for v in sections.values())} verdict fields)")
    if len(sections) < 6:
        print("ERROR: expected >= 6 sections", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
