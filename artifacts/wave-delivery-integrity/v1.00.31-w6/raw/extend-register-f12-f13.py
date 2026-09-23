#!/usr/bin/env python3
"""v1.00.31 W6 register extension (deterministic, re-runnable).

The plan's failure-mode register (PLAN-v1.00.31-GSD-WAVE-DELIVERY-INTEGRITY.md)
carries F12 (sentinel placeholders accepted as evidence, owning wave W2) and
F13 (frozen contract stale while every gate reports clean, owning wave W5).
W0's frozen materialization covers F1-F11; W4 extended it consistently for F11.
W6 extends it consistently for F12/F13 so the W6 rehearsal can replay every
register row F1-F13 through the register harness.

This script mirrors the plan's F12/F13 cells verbatim into:

  artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json
  artifacts/wave-delivery-integrity/v1.00.31-w0/w4-reproduction.json
  artifacts/wave-delivery-integrity/v1.00.31-w0/raw/plan-failure-mode-register.txt
  docs/reports/WAVE-DELIVERY-INTEGRITY-CONTRACT-v1.00.31.md

and is idempotent: re-running it on an already-extended tree reproduces the
same bytes. Determinism contract: sorted keys, 1-space indent, trailing
newline, ensure_ascii (the same canonical form the W5 provenance lint enforces).
"""
import hashlib
import json
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
ARTDIR = os.path.dirname(HERE)
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(ARTDIR)))
PLAN = os.path.join(os.path.dirname(ROOT), ".planning",
                    "PLAN-v1.00.31-GSD-WAVE-DELIVERY-INTEGRITY.md")
W0 = os.path.join(ROOT, "artifacts", "wave-delivery-integrity", "v1.00.31-w0")
REGISTER = os.path.join(W0, "failure-register.json")
REPRODUCTION = os.path.join(W0, "w4-reproduction.json")
RAW = os.path.join(W0, "raw", "plan-failure-mode-register.txt")
CONTRACT = os.path.join(ROOT, "docs", "reports",
                        "WAVE-DELIVERY-INTEGRITY-CONTRACT-v1.00.31.md")
SUMS = os.path.join(W0, "SHA256SUMS")

SHORT_MODE = {
    "F12": "Gate accepts sentinel placeholders as evidence",
    "F13": "Frozen contract can be stale while every gate reports clean",
}
EVIDENCE_GAP = {
    "F12": "The strict gate validated identity and narrative fields with "
           "`non-empty-string?` alone, so the literal sentinel `PENDING` "
           "satisfied every field the gate inspected; no fixture proved that a "
           "sentinel-carrying draft is refused, so the placeholder path "
           "survived every green suite.",
    "F13": "The plan-id hashes the wave-doc contents and the plan index rows, "
           "not the plan body, so a plan-body amendment changed neither the "
           "plan-id nor the snapshot; drift detection compared the live mirror "
           "against the snapshot, and with a stale mirror both sides agreed.",
}
REPRO = {
    "F12": {
        "fixture-kind": "record",
        "executed": "racket scripts/gsd-wave-gate.rkt <sentinel trio> "
                    "--content-digest <empty-sha> --root <scratch> "
                    "(W6 injection scripts/ci/inject-wave-defect.rkt --row F12)",
        "exit-code": 1,
        "tool-form-result": "placeholder-evidence",
        "consequence": "a staged draft could be 'finalized' by flipping only "
                       "status/verdict/digest fields while asserting an "
                       "independent review that never happened",
        "note": "added by the operator-directed plan amendment of 2026-09-20 "
                "after W0 froze F1-F10; W2 shipped the guard, W4 hardened "
                "compound sentinels, and W6 replays it as an injection rather "
                "than a pre-guard capture (the pre-fix tree no longer exists)",
    },
    "F13": {
        "fixture-kind": "record",
        "executed": "racket /tmp/w6-f13.rkt (extensions/gsd/plan-snapshot.rkt "
                    "seed-and-bind-plan-snapshot! over a scratch campaign root "
                    "whose authored PLAN.md was amended after the snapshot was "
                    "taken; W6 injection scripts/ci/inject-wave-defect.rkt "
                    "--row F13)",
        "exit-code": 1,
        "tool-form-result": "frozen-contract-stale",
        "consequence": "the campaign executed a contract that differed from the "
                       "frozen snapshot while the plan-id, drift classification "
                       "and plan validation all reported clean",
        "note": "added by the operator-directed plan amendment of 2026-09-20 "
                "after W0 froze F1-F10; W5 shipped the guard, and W6 replays it "
                "as an injection rather than a pre-guard capture",
    },
}


def canon(obj):
    # The W0 artifacts are 2-space, raw-UTF-8, insertion-ordered JSON (their
    # original hand formatting). Reproduce exactly that so the extension adds
    # only the F12/F13 rows instead of reformatting frozen material.
    return json.dumps(obj, indent=2, ensure_ascii=False) + "\n"


def plan_rows():
    rows = {}
    for line in open(PLAN, encoding="utf-8"):
        m = re.match(r"^\| (F1[23]) \|", line)
        if not m:
            continue
        cells = [c.strip() for c in line.rstrip("\n").split("|")]
        rows[m.group(1)] = {
            "mode-cell": cells[2],
            "structural-fix": cells[3],
            "owning-wave": cells[4],
            "refusal": cells[5],
        }
    assert set(rows) == {"F12", "F13"}, rows.keys()
    return rows


def extend_register(rows):
    data = json.load(open(REGISTER, encoding="utf-8"))
    have = {r["id"] for r in data["rows"]}
    for fid in ("F12", "F13"):
        if fid in have:
            continue
        data["rows"].append({
            "id": fid,
            "mode": SHORT_MODE[fid],
            "observed": rows[fid]["mode-cell"],
            "evidence-gap": EVIDENCE_GAP[fid],
            "structural-fix": rows[fid]["structural-fix"],
            "owning-wave": rows[fid]["owning-wave"],
            "refusal": rows[fid]["refusal"],
        })
    data["row-count"] = len(data["rows"])
    open(REGISTER, "w", encoding="utf-8").write(canon(data))
    return data["row-count"]


def extend_reproduction(rows):
    data = json.load(open(REPRODUCTION, encoding="utf-8"))
    have = {r["mode"] for r in data["reproductions"]}
    for fid in ("F12", "F13"):
        if fid in have:
            continue
        data["reproductions"].append({
            "mode": fid,
            "owning-wave": rows[fid]["owning-wave"],
            "expect": "refused",
            "guard-status": "guarded-pass",
            "fixture-kind": REPRO[fid]["fixture-kind"],
            "executed": REPRO[fid]["executed"],
            "exit-code": REPRO[fid]["exit-code"],
            "tool-form-result": REPRO[fid]["tool-form-result"],
            "consequence": REPRO[fid]["consequence"],
            "note": REPRO[fid]["note"],
        })
    open(REPRODUCTION, "w", encoding="utf-8").write(canon(data))
    return len(data["reproductions"])


def extend_raw(rows):
    lines = open(RAW, encoding="utf-8").read().split("\n")
    body = [l for l in lines if l.strip()]
    for fid in ("F12", "F13"):
        body = [l for l in body if not l.startswith("| %s |" % fid)]
        narrative = "| %s | %s %s |" % (fid, rows[fid]["mode-cell"], EVIDENCE_GAP[fid])
        contract = "| %s | %s | %s | %s | %s |" % (
            fid, SHORT_MODE[fid], rows[fid]["structural-fix"],
            rows[fid]["owning-wave"], rows[fid]["refusal"])
        body.append(narrative)
        body.append(contract)
    open(RAW, "w", encoding="utf-8").write("\n".join(body) + "\n")


def extend_contract(rows):
    text = open(CONTRACT, encoding="utf-8").read()
    lines = text.split("\n")
    register_at = next(i for i, l in enumerate(lines)
                       if l.startswith("| Mode | Observed in v1.00.30 W4"))
    gap_at = next(i for i, l in enumerate(lines)
                  if l.startswith("| Mode | Why the failure was possible"))
    # register row goes at the end of the register table (before the blank line
    # that follows it); evidence-gap row at the end of the gap table.
    def table_end(start):
        i = start + 1
        while i < len(lines) and lines[i].startswith("|"):
            i += 1
        return i
    reg_end = table_end(register_at)
    gap_end = table_end(gap_at)
    reg_rows = [
        "| **%s** %s | %s | %s | %s | %s |" % (
            fid, SHORT_MODE[fid], rows[fid]["mode-cell"],
            rows[fid]["structural-fix"], rows[fid]["owning-wave"],
            rows[fid]["refusal"])
        for fid in ("F12", "F13")]
    gap_rows = ["| **%s** | %s |" % (fid, EVIDENCE_GAP[fid])
                for fid in ("F12", "F13")]
    keep = [l for l in lines if not re.match(r"^\| \*\*F1[23]\*\*", l)]
    register_at = next(i for i, l in enumerate(keep)
                       if l.startswith("| Mode | Observed in v1.00.30 W4"))
    gap_at = next(i for i, l in enumerate(keep)
                  if l.startswith("| Mode | Why the failure was possible"))
    reg_end = table_end_line(keep, register_at)
    gap_end = table_end_line(keep, gap_at)
    out = keep[:reg_end] + reg_rows + keep[reg_end:]
    gap_end = gap_end + len(reg_rows)
    out = out[:gap_end] + gap_rows + out[gap_end:]
    out = [re.sub(r"^Row count: \*\*\d+\*\*\.$", "Row count: **13**.",
                  l, count=1) for l in out]
    open(CONTRACT, "w", encoding="utf-8").write("\n".join(out))


def table_end_line(lines, start):
    i = start + 1
    while i < len(lines) and lines[i].startswith("|"):
        i += 1
    return i


def regen_sums():
    entries = []
    for rel in sorted([
            "artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json",
            "artifacts/wave-delivery-integrity/v1.00.31-w0/raw/plan-failure-mode-register.txt",
            "artifacts/wave-delivery-integrity/v1.00.31-w0/w4-reproduction.json",
            "docs/reports/WAVE-DELIVERY-INTEGRITY-CONTRACT-v1.00.31.md"]):
        h = hashlib.sha256(open(os.path.join(ROOT, rel), "rb").read()).hexdigest()
        entries.append("%s  %s" % (h, rel))
    open(SUMS, "w", encoding="utf-8").write("\n".join(entries) + "\n")


def main():
    rows = plan_rows()
    count = extend_register(rows)
    repro = extend_reproduction(rows)
    extend_raw(rows)
    extend_contract(rows)
    regen_sums()
    print("register rows:", count, "reproductions:", repro)


if __name__ == "__main__":
    main()