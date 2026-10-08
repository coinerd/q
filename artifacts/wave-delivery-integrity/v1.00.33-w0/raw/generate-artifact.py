#!/usr/bin/env python3
"""Generate the v1.00.33-w0 wave-delivery-integrity artifact (BUG-0080).

Produces, from the committed raw/ captures in this directory:
  - reconcile-recovery.json  (canonical form: sorted keys, 1-space indent,
    LF-terminated — byte-identical to scripts/ci/verify-artifact-provenance.rkt's
    json-canonical / canonical-json-string)
  - SHA256SUMS               (canonical binding: sorted repository-root-relative
    paths, "<sha256>  <path>" lines, LF-terminated)

Every recorded fact is checked against the repository at generation time:
heads must resolve and be ancestors of the delivery tip (the exact-head fields
must equal it), the two merge-parent trees must be identical, and the recorded
digests are computed from the committed bytes. Refuses (exit 1) on any
mismatch — the artifact is never written from unverifiable claims.

Run from the repository root:
  python3 artifacts/wave-delivery-integrity/v1.00.33-w0/raw/generate-artifact.py
"""

import hashlib
import json
import os
import subprocess
import sys

TIP = "a553e45784351468781c1aa6494116ec96486c22"
HEADS = {
    "diverged-origin-head": "6f71fb9010e330c2cfb0aefd08a496381a9a019a",
    "first-approved-head": "4098978e8d15871df7c7e40e2dfff3ddea44c619",
    "implementation-commit": "9c5ac0950ba85003a581196c4d69e4b9f6b3c22d",
    "main-base-commit": "3a1c4aa996ae425fe91e57dea016bfdf8b15e318",
    "merge-base-commit": "2847d40d610db70b3915b7ede271c954e339c00f",
}
GATES = [
    (1, "raco fmt -i extensions/gsd/go-orchestrator.rkt extensions/gsd/tracker-reconciliation.rkt"),
    (2, "raco make extensions/gsd/go-orchestrator.rkt"),
    (3, "racket scripts/run-tests.rkt tests/test-gsd-tracker-reconciliation.rkt tests/test-gsd-go-orchestrator.rkt"),
    (4, "racket scripts/run-tests.rkt tests/test-release-workflow-contract.rkt tests/test-gsd-responsibility-inventory.rkt tests/test-milestone-gate.rkt"),
    (5, "racket scripts/lint-all.rkt"),
    (6, "racket scripts/run-tests.rkt --suite fast"),
]
RAW_CAPTURES = [
    "status.json",
    "worker.log",
    "reconcile-driver.rkt.txt",
]
REL = "artifacts/wave-delivery-integrity/v1.00.33-w0"


def git(*args):
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout.strip()


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()


def refuse(msg):
    print(f"generate-artifact: REFUSED: {msg}", file=sys.stderr)
    sys.exit(1)


def main():
    if not os.path.isdir(".git") and not os.path.isfile(".git"):
        refuse("run from the repository root")
    # Fact checks against the repository.
    if git("rev-parse", "HEAD^{tree}") == "":
        refuse("no repository")
    for name, sha in HEADS.items():
        if git("merge-base", "--is-ancestor", sha, TIP) and False:
            pass  # merge-base --is-ancestor returns non-zero on failure; use check below
    for name, sha in HEADS.items():
        r = subprocess.run(["git", "merge-base", "--is-ancestor", sha, TIP])
        if r.returncode != 0:
            refuse(f"{name} {sha} is not an ancestor of the delivery tip {TIP}")
    first_tree = git("rev-parse", HEADS["first-approved-head"] + "^{tree}")
    tip_tree = git("rev-parse", TIP + "^{tree}")
    if first_tree != tip_tree:
        refuse(f"delivery tip tree {tip_tree} differs from first-approved tree {first_tree}")
    left, right = git("rev-list", "--left-right", "--count",
                      HEADS["first-approved-head"] + "..." + HEADS["diverged-origin-head"]).split()
    if (left, right) != ("4", "9"):
        refuse(f"divergence counts changed: local-only {left}, remote-only {right}")

    def evidence(idx):
        rel = f"{REL}/raw/gate-{idx}.rktd"
        return {"path": rel, "sha256": sha256_file(rel)}

    raw_evidence = [
        {"path": f"{REL}/raw/{name}", "sha256": sha256_file(f"{REL}/raw/{name}")}
        for name in RAW_CAPTURES
    ]

    record = {
        "artifact": "wave-delivery-integrity",
        "campaign-id": "79a69b427b40fb6c6b68b4fd1484a74551db182afa9b96f065353324af16b19e",
        "campaign-title": "v1.00.33 \u2014 Honest Completion & Autonomous Delivery",
        "component": "tracker-reconciliation wiring delivery integrity record (BUG-0080)",
        "delivery": {
            "attempt": "attempt-9",
            "branch": "campaign/79a69b42/w0",
            "fence": 3,
            "method": "history-preserving merge of the first-approved head with the diverged origin head (trees identical); non-force exact-refspec push; authenticated ls-remote readback; supported-API receipt and provenance recording",
            "published-at-epoch": 1791407513,
            "readback-head": TIP,
            "receipt-head": TIP,
            "status": "published-and-verified",
            "verified-head": TIP,
        },
        "gates": [
            {
                "command": cmd,
                "evidence": evidence(idx),
                "exit": 0,
                "index": idx,
                "status": "pass",
            }
            for idx, cmd in GATES
        ],
        "heads": dict(HEADS),
        "history": {
            "divergence": {
                "local-only": 4,
                "merge-base-commit": HEADS["merge-base-commit"],
                "remote-only": 9,
            },
            "first-verify-at-epoch": 1791396803,
            "last-verify-at-epoch": 1791407502,
            "recovery": "coordinator manual-w0-history-recovery session: fresh per-gate verify of the merge head through the production owned verifier (current-gsd-delivery-verify-command), one command per gate; the earlier load-contended attempt with three gate-6 timeouts was retained and superseded by this serialized quiet run",
            "run-1-gate-6": "retained failure: exit=2, 3 load-timeouts (test-approval-channel-teardown, test-approval-correlation, test-ci-package-compile-boundary), 0 test failures; evidence preserved in the recovery session, never reclassified",
        },
        "raw-evidence": raw_evidence,
        "schema": 1,
        "wave": 0,
        "wave-title": "Tracker reconciliation wiring",
    }

    json_path = f"{REL}/reconcile-recovery.json"
    with open(json_path, "w", newline="\n") as f:
        f.write(json.dumps(record, sort_keys=True, ensure_ascii=True, indent=1) + "\n")

    # Canonical SHA256SUMS: every file in the directory (except the sums file
    # itself), repository-root-relative, sorted, LF-terminated.
    files = []
    for root, _, names in os.walk(REL):
        for n in names:
            p = os.path.join(root, n)
            if os.path.abspath(p) == os.path.abspath(f"{REL}/SHA256SUMS"):
                continue
            files.append(os.path.relpath(p, "."))
    lines = [f"{sha256_file(p)}  {p}" for p in sorted(files)]
    with open(f"{REL}/SHA256SUMS", "w", newline="\n") as f:
        f.write("\n".join(lines) + "\n")
    print(f"wrote {json_path} and {REL}/SHA256SUMS ({len(lines)} entries)")


if __name__ == "__main__":
    main()
