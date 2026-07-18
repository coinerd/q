"""Security and recovery tests for the protected-wave finalizer."""

from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import unittest


MODULE_PATH = Path(__file__).with_name("wave-finalizer.py")
SPEC = importlib.util.spec_from_file_location("wave_finalizer", MODULE_PATH)
assert SPEC and SPEC.loader
wf = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = wf
SPEC.loader.exec_module(wf)

HEAD = "1" * 40
MERGE_SHA = "2" * 40
APP_ID = 15368
MERGED_AT = "2025-01-02T12:00:00Z"
RUN_AT = "2025-01-02T12:01:00Z"


def trusted_config(**changes):
    value = {
        "schema_version": 1,
        "repository": "acme/widgets",
        "base_ref": "main",
        "merge_method": "squash",
        "attestation_issuer": "trusted-wave-launcher",
        "required_workflows": [{
            "context": "W0 / protected",
            "workflow": ".github/workflows/ci.yml",
            "ref": "feature/w0",
            "event": "pull_request",
            "app": {"slug": "github-actions", "id": APP_ID},
        }],
        "merge_workflows": [{
            "context": "W0 / merge",
            "workflow": ".github/workflows/merge-ci.yml",
            "ref": "main",
            "event": "push",
            "app": {"slug": "github-actions", "id": APP_ID},
        }],
        "wave_issue_numbers": [40, 41],
        "board": {
            "project_id": "PVT_1", "item_id": "PVTI_1", "field_id": "PVTF_1",
            "value": "Done",
        },
    }
    value.update(changes)
    return value


def envelope(**projection_changes):
    projection = {"pull_number": 17, "head_sha": HEAD}
    projection.update(projection_changes)
    value = {
        "schema_version": 1,
        "issuer": "trusted-wave-launcher",
        "projection": projection,
        "evidence": {"kind": "artifact", "id": 7001},
    }
    rendered = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False)
    value["canonical_digest"] = {
        "algorithm": "sha256",
        "value": hashlib.sha256(rendered.encode()).hexdigest(),
    }
    return value


def projection_digest(value):
    rendered = json.dumps(value["projection"], sort_keys=True, separators=(",", ":"), ensure_ascii=False)
    return hashlib.sha256(rendered.encode()).hexdigest()


def protection(app_id=APP_ID, **changes):
    value = {
        "required_status_checks": {
            "strict": True,
            "checks": [{"context": "W0 / protected", "app_id": app_id}],
            "contexts": [],
        },
        "enforce_admins": {"enabled": True},
        "required_linear_history": {"enabled": True},
        "allow_force_pushes": {"enabled": False},
        "allow_deletions": {"enabled": False},
    }
    value.update(changes)
    return value


def pull(*, merged=False, state="open", head=HEAD, merge_sha=None, merged_at=None):
    return {
        "number": 17, "state": state, "merged": merged,
        "merge_commit_sha": merge_sha, "merged_at": merged_at,
        "base": {"ref": "main", "repo": {"full_name": "acme/widgets"}},
        "head": {"sha": head, "ref": "feature/w0", "repo": {"full_name": "acme/widgets"}},
    }


def workflow_run(*, merge=False, sha=None, attempt=1, conclusion="success", event=None,
                 ref=None, suite=None, run_id=None, created_at=None, path=None):
    sha = sha or (MERGE_SHA if merge else HEAD)
    return {
        "id": run_id or (200 if merge else 100) + attempt,
        "workflow_id": 8 if merge else 7,
        "path": path or (".github/workflows/merge-ci.yml" if merge else ".github/workflows/ci.yml"),
        "repository": {"full_name": "acme/widgets"},
        "head_sha": sha,
        "head_branch": ref or ("main" if merge else "feature/w0"),
        "event": event or ("push" if merge else "pull_request"),
        "run_number": 9 if merge else 8,
        "run_attempt": attempt,
        "check_suite_id": suite or (190 if merge else 90),
        "status": "completed", "conclusion": conclusion,
        "created_at": created_at or (RUN_AT if merge else "2025-01-01T12:00:00Z"),
    }


def check_run(*, merge=False, sha=None, conclusion="success", suite=None, app_id=APP_ID,
              slug="github-actions"):
    return {
        "id": 600 if merge else 500,
        "name": "W0 / merge" if merge else "W0 / protected",
        "head_sha": sha or (MERGE_SHA if merge else HEAD),
        "status": "completed", "conclusion": conclusion,
        "check_suite": {"id": suite or (190 if merge else 90)},
        "app": {"slug": slug, "id": app_id},
    }


class FakeGitHub:
    def __init__(self):
        self.prs = [pull()]
        self.protection = protection()
        self.workflow_runs = {HEAD: [workflow_run()]}
        self.check_runs = {HEAD: [check_run()]}
        self.merge_response = {"merged": True, "sha": MERGE_SHA}
        self.attestation_overrides = {}
        self.issues = {40: "open", 41: "open"}
        self.board_value = "Todo"
        self.calls = []
        self.close_fail_after_apply = set()
        self.close_without_apply = set()
        self.board_fail_after_apply = False
        self.board_without_apply = False
        self.merge_fail_after_apply = False

    def verify_github_attested_evidence(self, repository, reference):
        self.calls.append(("attestation", repository, copy.deepcopy(reference)))
        env = envelope()
        result = {
            "verified": True, "reference": copy.deepcopy(reference),
            "repository": repository, "pull_number": 17, "head_sha": HEAD,
            "projection_digest": projection_digest(env),
            "issuer": "trusted-wave-launcher", "latest": True,
        }
        result.update(self.attestation_overrides)
        return result

    def get_pull_request(self, repository, pull_number):
        self.calls.append(("get_pr", repository, pull_number))
        return copy.deepcopy(self.prs.pop(0) if len(self.prs) > 1 else self.prs[0])

    def get_branch_protection(self, repository, branch):
        self.calls.append(("protection", repository, branch))
        return copy.deepcopy(self.protection)

    def list_workflow_runs(self, repository, sha):
        self.calls.append(("workflow_runs", repository, sha))
        return copy.deepcopy(self.workflow_runs.get(sha, []))

    def list_check_runs(self, repository, sha):
        self.calls.append(("check_runs", repository, sha))
        return copy.deepcopy(self.check_runs.get(sha, []))

    def merge_pull_request(self, repository, pull_number, *, sha, merge_method):
        self.calls.append(("merge", repository, pull_number, sha, merge_method))
        if self.merge_fail_after_apply:
            raise ConnectionError("response lost")
        return copy.deepcopy(self.merge_response)

    def get_issue(self, repository, issue_number):
        self.calls.append(("get_issue", repository, issue_number))
        return {"number": issue_number, "state": self.issues[issue_number]}

    def close_issue(self, repository, issue_number):
        self.calls.append(("close_issue", repository, issue_number))
        if issue_number not in self.close_without_apply:
            self.issues[issue_number] = "closed"
        if issue_number in self.close_fail_after_apply:
            self.close_fail_after_apply.remove(issue_number)
            raise ConnectionError("response lost")

    def get_project_field_value(self, repository, project_id, item_id, field_id):
        self.calls.append(("get_board", repository, project_id, item_id, field_id))
        return copy.deepcopy(self.board_value)

    def update_project_field_value(self, repository, project_id, item_id, field_id, value):
        self.calls.append(("update_board", repository, project_id, item_id, field_id, value))
        if not self.board_without_apply:
            self.board_value = copy.deepcopy(value)
        if self.board_fail_after_apply:
            self.board_fail_after_apply = False
            raise ConnectionError("response lost")


def ready_client(*, merge_sha=MERGE_SHA):
    client = FakeGitHub()
    client.prs = [pull(), pull(), pull(merged=True, state="closed", merge_sha=merge_sha, merged_at=MERGED_AT)]
    client.workflow_runs[merge_sha] = [workflow_run(merge=True, sha=merge_sha)]
    client.check_runs[merge_sha] = [check_run(merge=True, sha=merge_sha)]
    return client


class ConfigurationAndEnvelopeTests(unittest.TestCase):
    def test_envelope_digest_is_only_canonical_self_consistency(self):
        verified = wf.verify_external_envelope(envelope())
        self.assertTrue(verified.canonical_self_consistent)
        self.assertFalse(hasattr(verified, "integrity_verified"))
        changed = envelope()
        changed["projection"]["pull_number"] = 99
        with self.assertRaises(wf.EnvelopeError):
            wf.verify_external_envelope(changed)

    def test_envelope_cannot_supply_trusted_policy_or_mutations(self):
        value = envelope()
        value["repository"] = "evil/repo"
        with self.assertRaises(wf.EnvelopeError):
            wf.prepare_finalization(FakeGitHub(), trusted_config(), value)
        with self.assertRaises(wf.ConfigurationError):
            wf.load_trusted_configuration(trusted_config(merge_workflows=[]))

    def test_board_value_and_issue_configuration_are_type_strict(self):
        with self.assertRaises(wf.ConfigurationError):
            wf.load_trusted_configuration(trusted_config(wave_issue_numbers=[40, True]))
        bad = trusted_config()
        bad["board"]["value"] = float("nan")
        with self.assertRaises(wf.ConfigurationError):
            wf.load_trusted_configuration(bad)


class PrepareTests(unittest.TestCase):
    def test_attestation_is_verified_before_github_facts_and_is_exact(self):
        client = FakeGitHub()
        prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        self.assertEqual("attestation", client.calls[0][0])
        self.assertEqual(HEAD, prepared.head_sha)
        self.assertTrue(prepared.attestation_digest)
        self.assertFalse(any(c[0] in {"merge", "close_issue", "update_board"} for c in client.calls))

    def test_wrong_attestation_binding_or_unverified_or_stale_rejects(self):
        cases = [
            {"verified": False}, {"repository": "evil/repo"}, {"pull_number": 18},
            {"head_sha": "3" * 40}, {"projection_digest": "4" * 64},
            {"issuer": "attacker"}, {"latest": False},
            {"reference": {"kind": "artifact", "id": 999}},
        ]
        for override in cases:
            with self.subTest(override=override):
                client = FakeGitHub()
                client.attestation_overrides = override
                with self.assertRaises(wf.AttestationError):
                    wf.prepare_finalization(client, trusted_config(), envelope())
                self.assertFalse(any(c[0] == "get_pr" for c in client.calls))

    def test_fresh_post_merge_prepare_cannot_authorize_closure(self):
        client = FakeGitHub()
        client.prs = [pull(merged=True, state="closed", merge_sha=MERGE_SHA, merged_at=MERGED_AT)]
        with self.assertRaises(wf.MergeError):
            wf.prepare_finalization(client, trusted_config(), envelope())
        self.assertFalse(any(c[0] in {"merge", "close_issue", "update_board"} for c in client.calls))

    def test_branch_policy_rejects_legacy_unpinned_wrong_app_and_uncertainty(self):
        policies = [
            protection(app_id=None), protection(app_id=999),
            protection(required_status_checks={"strict": True, "checks": [], "contexts": ["W0 / protected"]}),
            protection(required_status_checks=None),
        ]
        for policy in policies:
            with self.subTest(policy=policy):
                client = FakeGitHub(); client.protection = policy
                with self.assertRaises((wf.PolicyError, wf.ApiUncertainty)):
                    wf.prepare_finalization(client, trusted_config(), envelope())

    def test_wrong_workflow_ref_event_sha_app_and_latest_attempt_reject(self):
        mutations = [
            ("path", ".github/workflows/other.yml"), ("head_branch", "other"),
            ("event", "push"), ("head_sha", "3" * 40),
        ]
        for field, value in mutations:
            with self.subTest(field=field):
                client = FakeGitHub(); client.workflow_runs[HEAD][0][field] = value
                with self.assertRaises(wf.CheckError):
                    wf.prepare_finalization(client, trusted_config(), envelope())
        client = FakeGitHub(); client.check_runs[HEAD][0]["app"]["id"] = 999
        with self.assertRaises(wf.CheckError):
            wf.prepare_finalization(client, trusted_config(), envelope())
        client = FakeGitHub()
        client.workflow_runs[HEAD] = [workflow_run(attempt=1), workflow_run(attempt=2, conclusion="failure", suite=91)]
        client.check_runs[HEAD] = [check_run()]
        with self.assertRaises(wf.CheckError):
            wf.prepare_finalization(client, trusted_config(), envelope())


class FinalizeTests(unittest.TestCase):
    def test_merge_ci_is_mandatory_exact_push_base_sha_and_after_merge(self):
        client = ready_client()
        prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        result = wf.finalize_prepared(client, prepared)
        self.assertTrue(result.merge_ci_verified)
        merge_i = next(i for i, c in enumerate(client.calls) if c[0] == "merge")
        merge_ci_i = next(i for i, c in enumerate(client.calls) if c[:3] == ("workflow_runs", "acme/widgets", MERGE_SHA))
        close_i = next(i for i, c in enumerate(client.calls) if c[0] == "close_issue")
        self.assertLess(merge_i, merge_ci_i); self.assertLess(merge_ci_i, close_i)

    def test_merge_ci_wrong_ref_event_sha_failure_or_premerge_run_reuse_rejects_without_sync(self):
        variants = [
            workflow_run(merge=True, ref="feature/w0"),
            workflow_run(merge=True, event="pull_request"),
            workflow_run(merge=True, sha=HEAD),
            workflow_run(merge=True, conclusion="failure"),
            workflow_run(merge=True, run_id=101),
            workflow_run(merge=True, created_at="2025-01-02T11:59:59Z"),
        ]
        for run in variants:
            with self.subTest(run=run):
                client = ready_client(); client.workflow_runs[MERGE_SHA] = [run]
                prepared = wf.prepare_finalization(client, trusted_config(), envelope())
                with self.assertRaises(wf.CheckError):
                    wf.finalize_prepared(client, prepared)
                self.assertFalse(any(c[0] in {"close_issue", "update_board"} for c in client.calls))

    def test_merge_ci_cannot_reuse_pull_request_evidence_when_merge_sha_equals_head(self):
        client = ready_client(merge_sha=HEAD)
        client.merge_response = {"merged": True, "sha": HEAD}
        client.workflow_runs[HEAD] = [workflow_run()]  # only the pre-merge pull_request run
        client.check_runs[HEAD] = [check_run()]
        prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        with self.assertRaises(wf.CheckError):
            wf.finalize_prepared(client, prepared)
        self.assertFalse(any(c[0] == "close_issue" for c in client.calls))

    def test_merged_false_never_runs_post_merge_mutations(self):
        client = FakeGitHub()
        prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.merge_response = {"merged": False, "sha": MERGE_SHA}
        client.prs = [pull()]
        with self.assertRaises(wf.MergeError):
            wf.finalize_prepared(client, prepared)
        self.assertFalse(any(c[0] in {"close_issue", "update_board"} for c in client.calls))

    def test_prepared_is_immutable_revalidated_and_not_handcraftable(self):
        client = ready_client()
        prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        with self.assertRaises(Exception):
            prepared.head_sha = "3" * 40
        forged = wf.PreparedFinalization(
            config_json=prepared.config_json, envelope_json=prepared.envelope_json,
            config_digest=prepared.config_digest, envelope_digest=prepared.envelope_digest,
            attestation_digest=prepared.attestation_digest, head_sha=prepared.head_sha,
            policy=prepared.policy, premerge_run_ids=prepared.premerge_run_ids,
            already_merged=prepared.already_merged, seal="forged",
        )
        with self.assertRaises(wf.ValidationError):
            wf.finalize_prepared(client, forged)
        self.assertFalse(any(c[0] == "merge" for c in client.calls))

        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.attestation_overrides = {"issuer": "attacker"}
        with self.assertRaises(wf.AttestationError):
            wf.finalize_prepared(client, prepared)
        self.assertFalse(any(c[0] == "merge" for c in client.calls))

    def test_only_configured_issues_are_closed_never_pull_request_and_confirmed(self):
        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        wf.finalize_prepared(client, prepared)
        closed = [c[2] for c in client.calls if c[0] == "close_issue"]
        self.assertEqual([40, 41], closed)
        self.assertNotIn(17, closed)
        self.assertTrue(all(client.issues[n] == "closed" for n in (40, 41)))

    def test_issue_failure_stops_board_and_retry_recovers_partial_close(self):
        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.close_fail_after_apply = {40}
        # Response loss is recovered immediately by refetch.
        wf.finalize_prepared(client, prepared)
        self.assertEqual("Done", client.board_value)
        first_close_count = len([c for c in client.calls if c[0] == "close_issue" and c[2] == 40])
        wf.finalize_prepared(client, prepared)
        self.assertEqual(first_close_count, len([c for c in client.calls if c[0] == "close_issue" and c[2] == 40]))

        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.issues.pop(41)
        with self.assertRaises(wf.ApiUncertainty):
            wf.finalize_prepared(client, prepared)
        self.assertEqual("Todo", client.board_value)

    def test_board_type_strict_confirmation_idempotency_and_response_lost_recovery(self):
        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.board_fail_after_apply = True
        wf.finalize_prepared(client, prepared)
        updates = len([c for c in client.calls if c[0] == "update_board"])
        wf.finalize_prepared(client, prepared)
        self.assertEqual(updates, len([c for c in client.calls if c[0] == "update_board"]))

        client = ready_client(); config = trusted_config()
        config["board"]["value"] = 1
        client.board_value = True  # equal under Python, but not type-equal
        prepared = wf.prepare_finalization(client, config, envelope())
        wf.finalize_prepared(client, prepared)
        self.assertIs(type(client.board_value), int)

        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.board_without_apply = True
        with self.assertRaises(wf.ApiUncertainty):
            wf.finalize_prepared(client, prepared)

    def test_unconfirmed_issue_close_fails_and_merge_response_loss_is_recovered(self):
        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.close_without_apply = {40}
        with self.assertRaises(wf.ApiUncertainty):
            wf.finalize_prepared(client, prepared)
        self.assertEqual("Todo", client.board_value)

        client = ready_client(); prepared = wf.prepare_finalization(client, trusted_config(), envelope())
        client.merge_fail_after_apply = True
        # The queued refetch is merged=true, so the lost response is safely recovered.
        result = wf.finalize_prepared(client, prepared)
        self.assertEqual(MERGE_SHA, result.merge_sha)


if __name__ == "__main__":
    unittest.main()
