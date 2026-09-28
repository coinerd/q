#!/usr/bin/env python3
"""Build the v1.00.30 R2 cohort record from raw GitHub API captures.

Deterministic and re-runnable: every field is derived from the JSON under
raw/. No value is hand-entered. Emits cohort.json and report.json for
artifacts/ci-recovery/v1.00.30-r2/.

Frozen eligibility rules applied (PLAN-v1.00.30-PR-CI-RECOVERY.md, W5/W7):
  - candidate must be created strictly AFTER the W4 final activation
  - candidate head SHA must be unique within the cohort
  - docs-only / no-execution-change PRs do NOT fill a performance cohort
    (they take the trusted reporter's NOT_APPLICABLE result instead)
  - campaign implementation/review heads DO count, provided they are genuine,
    complete, strictly inside the window and comparability-consistent
  - heads may not be reused from the v1.00.28 / v1.00.29 series
"""

import json
import os
import sys
from datetime import datetime, timezone

HERE = os.path.dirname(os.path.abspath(__file__))
ART = os.path.dirname(HERE)

# Frozen W4 final activation. Source of record: the merged activation commit
# itself, not a hand-copied timestamp.
#
#   cc25b3663362d032b9a23a9d22f9dd5edbe637d8
#   "W4 delivery: campaign/v1.00.30-w4 (#9752)"
#   committer date 2026-09-25T04:10:14+02:00  ==  2026-09-25T02:10:14Z
#
# The commit carries a +02:00 offset, so the UTC instant is 02:10:14Z. Recording
# the local wall-clock time as if it were UTC would shift the cohort window two
# hours later and silently exclude any genuine PR created in that gap.
W4_ACTIVATION_COMMIT = "cc25b3663362d032b9a23a9d22f9dd5edbe637d8"
W4_ACTIVATION = "2026-09-25T02:10:14Z"
W4_ACTIVATION_LOCAL = "2026-09-25T04:10:14+02:00"

# Files whose presence alone cannot change executable behaviour. A PR touching
# ONLY these is "no execution change" and is exempt from the performance cohort.
NON_EXECUTION_PREFIXES = ("docs/reports/",)
NON_EXECUTION_EXACT = ("CHANGELOG.md",)

REQUIRED = 20


def load(name):
    with open(os.path.join(HERE, name), encoding="utf-8") as fh:
        return json.load(fh)


def parse_ts(value):
    return datetime.strptime(value, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=timezone.utc)


def classify_files(filenames):
    """Return (execution_affecting_files, doc_only_files)."""
    exec_files, doc_files = [], []
    for name in filenames:
        if name in NON_EXECUTION_EXACT or name.startswith(NON_EXECUTION_PREFIXES):
            doc_files.append(name)
        else:
            exec_files.append(name)
    return exec_files, doc_files


def main():
    pulls = load("post-w4-pulls-raw.json")
    cutoff = parse_ts(W4_ACTIVATION)

    considered, excluded_window = [], []
    for pr in pulls:
        if parse_ts(pr["created_at"]) <= cutoff:
            continue
        considered.append(pr)
    considered.sort(key=lambda p: p["number"])

    candidates = []
    for pr in considered:
        num = pr["number"]
        files_doc = load(f"pr-{num}-files.json")
        runs_doc = load(f"pr-{num}-runs.json")

        filenames = [f["filename"] for f in files_doc]
        exec_files, doc_files = classify_files(filenames)

        runs = runs_doc.get("workflow_runs", [])
        conclusions = sorted({r.get("conclusion") or r.get("status") for r in runs})

        reasons = []
        if not exec_files:
            reasons.append(
                "no-execution-change: touches only %s, so the trusted reporter "
                "issues NOT_APPLICABLE and it cannot fill a performance cohort"
                % ", ".join(sorted(doc_files))
            )

        candidates.append(
            {
                "pr": num,
                "title": pr["title"],
                "created_at": pr["created_at"],
                "merged_at": pr.get("merged_at"),
                "head_sha": pr["head"]["sha"],
                "base_repo": pr["base"]["repo"]["full_name"],
                "author": pr["user"]["login"],
                "is_internal_repo_pr": pr["head"]["repo"]["full_name"] == pr["base"]["repo"]["full_name"],
                "files_total": len(filenames),
                "execution_affecting_files": sorted(exec_files),
                "doc_only_files": sorted(doc_files),
                "ci_runs_observed": len(runs),
                "ci_run_conclusions": conclusions,
                "eligible": not reasons,
                "exclusion_reasons": reasons,
            }
        )

    eligible = [c for c in candidates if c["eligible"]]
    eligible_shas = [c["head_sha"] for c in eligible]
    unique_shas = sorted(set(eligible_shas))

    all_unique = len(unique_shas) == len(eligible_shas)
    cohort_open = len(unique_shas) < REQUIRED

    cohort = {
        "schema": 1,
        "record": "v1.00.30 R2 baseline recovery cohort",
        "status": "COHORT OPEN" if cohort_open else "CLOSED",
        "no_secrets": True,
        "generated_by": "raw/build-cohort.py",
        "w4_final_activation": W4_ACTIVATION,
        "w4_final_activation_commit": W4_ACTIVATION_COMMIT,
        "w4_final_activation_committer_date": W4_ACTIVATION_LOCAL,
        "window_rule": "created_at strictly after the W4 final activation merge",
        "required_heads": REQUIRED,
        "candidate_prs_after_w4": len(candidates),
        "eligible_heads": len(eligible),
        "unique_eligible_heads": len(unique_shas),
        "heads_unique": all_unique,
        "shortfall": max(0, REQUIRED - len(unique_shas)),
        "r2_evaluated": False,
        "r2_verdict": None,
        "blocking_reason": (
            "population is insufficient: %d unique eligible post-W4 PR head SHAs "
            "against a required %d. A missing population is COHORT OPEN, not a "
            "completed wave, and never a pass." % (len(unique_shas), REQUIRED)
        )
        if cohort_open
        else None,
        "no_samples_manufactured": True,
        "candidates": candidates,
        "unique_eligible_head_shas": unique_shas,
        "comparability_caveats": [
            "PR #9754 (head 49d9bb088) is a W4-era test-harness fix, created 68 "
            "minutes after the activation merge. It is strictly inside the window "
            "and execution-affecting, so it is counted, but it is campaign "
            "maintenance rather than representative feature traffic and its timing "
            "may not be comparable to ordinary PR load.",
            "PRs #9755 and #9758 are campaign implementation work (69 and 3 changed "
            "files). Their run-time profile is dominated by that change's own size, "
            "not by repository-wide cost, so neither is a clean latency sample.",
            "PR #9757 changes .github/workflows/release.yml only; it exercises the "
            "CI definition rather than the codebase.",
            "The window cutoff is the activation commit's UTC instant "
            "(2026-09-25T02:10:14Z). The commit's own date carries a +02:00 offset; "
            "using the local wall-clock time as if it were UTC would silently drop "
            "PRs #9753 and #9754 from the candidate set.",
        ],
    }

    report = {
        "schema": 1,
        "record": "v1.00.30 R2 performance report",
        "no_secrets": True,
        "generated_by": "raw/build-cohort.py",
        "status": "COHORT OPEN",
        "measured": False,
        "reason": (
            "No performance statistics are reported. The cohort never reached the "
            "required %d eligible heads, so p50/p95, trigger/queue time, max "
            "fast-shard time and runner-minutes have no valid population. Reporting "
            "statistics over %d heads would misrepresent the measurement."
            % (REQUIRED, len(unique_shas))
        ),
        "baselines_for_context_only": {
            "prior_clean_p50_seconds": 820.5,
            "prior_clean_p95_seconds": 850.3,
            "v1_00_29_p50_seconds": 2558.0,
            "v1_00_29_p95_seconds": 3023.0,
            "note": (
                "Shown for orientation only. These are NOT achieved values and were "
                "not recomputed; R2 was never evaluated."
            ),
        },
        "r2_thresholds_not_evaluated": {
            "closed_cohort_p50_must_be_at_most": 820.5,
            "evaluated": False,
        },
        "eligible_heads": unique_shas,
        "excluded_candidates": [
            {"pr": c["pr"], "head_sha": c["head_sha"], "reasons": c["exclusion_reasons"]}
            for c in candidates
            if not c["eligible"]
        ],
        "next_authorized_action": (
            "operator resumption or a separately reviewed plan amendment; deadlines "
            "never authorize a pass"
        ),
    }

    with open(os.path.join(ART, "cohort.json"), "w", encoding="utf-8") as fh:
        json.dump(cohort, fh, indent=2)
        fh.write("\n")
    with open(os.path.join(ART, "report.json"), "w", encoding="utf-8") as fh:
        json.dump(report, fh, indent=2)
        fh.write("\n")

    print("  status:            %s" % cohort["status"])
    print("  candidates >W4:    %d" % len(candidates))
    print("  eligible heads:    %d (unique %d, unique=%s)" % (len(eligible), len(unique_shas), all_unique))
    print("  required:          %d" % REQUIRED)
    print("  shortfall:         %d" % cohort["shortfall"])
    print("  r2_evaluated:      %s" % cohort["r2_evaluated"])
    for c in candidates:
        print(
            "    #%d %-8s exec=%d doc=%d  %s"
            % (c["pr"], "ELIGIBLE" if c["eligible"] else "EXCLUDED",
               len(c["execution_affecting_files"]), len(c["doc_only_files"]),
               c["head_sha"][:9])
        )
    return 0


if __name__ == "__main__":
    sys.exit(main())
