#!/usr/bin/env python3
"""Small fail-closed controller for finalizing a protected GitHub wave.

Authorization comes only from separately supplied, trusted configuration.  The
external envelope contains a PR projection and an evidence reference; its
SHA-256 is only a canonical self-consistency check, not integrity,
authenticity, or authorization.  Before preparation, the injected adapter must
verify GitHub-attested evidence bound to the exact projection.

This W0 module is intentionally adapter-only.  The external launcher is
responsible for verifying the deployed finalizer digest and then supplying an
authenticated, correctly scoped GitHub adapter.  The controller never invokes
``git`` or ``gh`` and never derives issue numbers from PR text.
"""

import argparse
import copy
from datetime import datetime, timezone
import hashlib
import hmac
import json
import re
import secrets
import weakref
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping, Optional, Protocol, Sequence, Tuple, Union

_SHA_RE = re.compile(r"^[0-9a-fA-F]{40}$")
_DIGEST_RE = re.compile(r"^[0-9a-f]{64}$")
_PREPARED_KEY = secrets.token_bytes(32)


class FinalizerError(RuntimeError):
    pass


class EnvelopeError(FinalizerError):
    pass


class ConfigurationError(FinalizerError):
    pass


class AttestationError(FinalizerError):
    pass


class ApiUncertainty(FinalizerError):
    pass


class ValidationError(FinalizerError):
    pass


class PolicyError(ValidationError):
    pass


class CheckError(ValidationError):
    pass


class MergeError(FinalizerError):
    pass


class GitHubClient(Protocol):
    """Authenticated adapter supplied by the digest-verifying launcher.

    List methods must return a complete list/tuple, or
    ``{"items": [...], "complete": true}``.  Uncertainty must be raised rather
    than represented as an empty or partial result.
    """

    def verify_github_attested_evidence(
        self, repository: str, reference: Mapping[str, Any]
    ) -> Mapping[str, Any]: ...
    def get_pull_request(self, repository: str, pull_number: int) -> Mapping[str, Any]: ...
    def get_branch_protection(self, repository: str, branch: str) -> Mapping[str, Any]: ...
    def list_workflow_runs(self, repository: str, sha: str) -> Sequence[Mapping[str, Any]]: ...
    def list_check_runs(self, repository: str, sha: str) -> Sequence[Mapping[str, Any]]: ...
    def merge_pull_request(
        self, repository: str, pull_number: int, *, sha: str, merge_method: str
    ) -> Mapping[str, Any]: ...
    def get_issue(self, repository: str, issue_number: int) -> Mapping[str, Any]: ...
    def close_issue(self, repository: str, issue_number: int) -> Any: ...
    def get_project_field_value(
        self, repository: str, project_id: str, item_id: str, field_id: str
    ) -> Any: ...
    def update_project_field_value(
        self, repository: str, project_id: str, item_id: str, field_id: str, value: Any
    ) -> Any: ...


@dataclass(frozen=True)
class WorkflowContract:
    context: str
    workflow: Union[str, int]
    ref: str
    event: str
    app_slug: str
    app_id: int


@dataclass(frozen=True)
class BoardSync:
    project_id: str
    item_id: str
    field_id: str
    value_json: str

    def value(self) -> Any:
        return json.loads(self.value_json)


@dataclass(frozen=True)
class TrustedConfiguration:
    repository: str
    base_ref: str
    merge_method: str
    attestation_issuer: str
    required_workflows: Tuple[WorkflowContract, ...]
    merge_workflows: Tuple[WorkflowContract, ...]
    wave_issue_numbers: Tuple[int, ...]
    board: BoardSync
    canonical_json: str
    digest: str


@dataclass(frozen=True)
class ExternalProjection:
    pull_number: int
    head_sha: str


@dataclass(frozen=True)
class VerifiedEnvelope:
    projection: ExternalProjection
    issuer: str
    evidence_kind: str
    evidence_id: int
    digest: str
    projection_digest: str
    canonical_json: str
    canonical_self_consistent: bool = True

    def evidence_reference(self) -> Mapping[str, Any]:
        return {"kind": self.evidence_kind, "id": self.evidence_id}


@dataclass(frozen=True)
class ProtectionPolicy:
    required_contexts: Tuple[str, ...]
    app_ids: Tuple[Tuple[str, int], ...]
    strict: bool = True
    admins_enforced: bool = True
    linear_history: bool = True
    force_pushes_allowed: bool = False
    deletions_allowed: bool = False

    def app_id_for(self, context: str) -> int:
        return dict(self.app_ids)[context]


@dataclass(frozen=True)
class PreparedFinalization:
    """Sealed immutable snapshot; only ``prepare_finalization`` creates valid values."""

    config_json: str
    envelope_json: str
    config_digest: str
    envelope_digest: str
    attestation_digest: str
    head_sha: str
    policy: ProtectionPolicy
    premerge_run_ids: Tuple[int, ...]
    already_merged: bool
    seal: str


@dataclass(frozen=True)
class FinalizationResult:
    repository: str
    pull_number: int
    head_sha: str
    merge_sha: str
    already_merged: bool
    merge_ci_verified: bool
    post_merge_synchronized: bool
    canonical_self_consistency_verified: bool = True
    github_attestation_verified: bool = True


# Seals detect field mutation; identity registration additionally rejects
# dataclasses.replace/directly handcrafted lookalikes within this process.
_ISSUED_PREPARED: dict[int, weakref.ReferenceType[PreparedFinalization]] = {}


def _register_prepared(value: PreparedFinalization) -> None:
    identity = id(value)
    _ISSUED_PREPARED[identity] = weakref.ref(
        value, lambda unused, identity=identity: _ISSUED_PREPARED.pop(identity, None)
    )


def _canonical_json(value: Any, label: str, error_type: type[FinalizerError]) -> str:
    try:
        return json.dumps(
            value, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False
        )
    except (TypeError, ValueError) as exc:
        raise error_type(f"{label} is not canonical JSON: {exc}") from exc


def _mapping(value: Any, label: str, error_type: type[FinalizerError]) -> Mapping[str, Any]:
    if not isinstance(value, Mapping):
        raise error_type(f"{label} must be an object")
    return value


def _exact_keys(
    value: Mapping[str, Any], required: set[str], optional: set[str], label: str,
    error_type: type[FinalizerError],
) -> None:
    missing = required - set(value)
    unknown = set(value) - required - optional
    if missing or unknown:
        parts = []
        if missing:
            parts.append(f"missing {sorted(missing)}")
        if unknown:
            parts.append(f"unknown {sorted(unknown)}")
        raise error_type(f"{label} has {', '.join(parts)}")


def _string(value: Any, label: str, error_type: type[FinalizerError]) -> str:
    if not isinstance(value, str) or not value or value != value.strip():
        raise error_type(f"{label} must be a nonempty, trimmed string")
    return value


def _positive_int(value: Any, label: str, error_type: type[FinalizerError]) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise error_type(f"{label} must be a positive integer")
    return value


def _sha(value: Any, label: str, error_type: type[FinalizerError]) -> str:
    if not isinstance(value, str) or _SHA_RE.fullmatch(value) is None:
        raise error_type(f"{label} must be a full 40-character hexadecimal SHA")
    return value


def _workflow(value: Any, label: str) -> WorkflowContract:
    item = _mapping(value, label, ConfigurationError)
    _exact_keys(item, {"context", "workflow", "ref", "event", "app"}, set(), label, ConfigurationError)
    identity = item["workflow"]
    if isinstance(identity, bool) or not isinstance(identity, (str, int)):
        raise ConfigurationError(f"{label}.workflow must be a path or positive workflow id")
    identity = (_string(identity, f"{label}.workflow", ConfigurationError)
                if isinstance(identity, str)
                else _positive_int(identity, f"{label}.workflow", ConfigurationError))
    app = _mapping(item["app"], f"{label}.app", ConfigurationError)
    _exact_keys(app, {"slug", "id"}, set(), f"{label}.app", ConfigurationError)
    return WorkflowContract(
        context=_string(item["context"], f"{label}.context", ConfigurationError),
        workflow=identity,
        ref=_string(item["ref"], f"{label}.ref", ConfigurationError),
        event=_string(item["event"], f"{label}.event", ConfigurationError),
        app_slug=_string(app["slug"], f"{label}.app.slug", ConfigurationError),
        app_id=_positive_int(app["id"], f"{label}.app.id", ConfigurationError),
    )


def _workflow_list(value: Any, label: str) -> Tuple[WorkflowContract, ...]:
    if not isinstance(value, list) or not value:
        raise ConfigurationError(f"{label} must be a nonempty array")
    result = tuple(_workflow(item, f"{label}[{index}]") for index, item in enumerate(value))
    contexts = tuple(item.context for item in result)
    if len(contexts) != len(set(contexts)):
        raise ConfigurationError(f"{label} has duplicate contexts")
    return result


def load_trusted_configuration(value: Mapping[str, Any]) -> TrustedConfiguration:
    """Parse the launcher's separate, trusted and immutable policy snapshot."""
    raw = _mapping(value, "trusted configuration", ConfigurationError)
    required = {
        "schema_version", "repository", "base_ref", "merge_method", "attestation_issuer",
        "required_workflows", "merge_workflows", "wave_issue_numbers", "board",
    }
    _exact_keys(raw, required, set(), "trusted configuration", ConfigurationError)
    if type(raw["schema_version"]) is not int or raw["schema_version"] != 1:
        raise ConfigurationError("unsupported trusted configuration schema_version")
    repository = _string(raw["repository"], "repository", ConfigurationError)
    if repository.count("/") != 1 or any(not part for part in repository.split("/")):
        raise ConfigurationError("repository must be owner/name")
    base_ref = _string(raw["base_ref"], "base_ref", ConfigurationError)
    merge_method = _string(raw["merge_method"], "merge_method", ConfigurationError)
    if merge_method not in {"squash", "rebase"}:
        raise ConfigurationError("merge_method must preserve linear history")
    required_workflows = _workflow_list(raw["required_workflows"], "required_workflows")
    merge_workflows = _workflow_list(raw["merge_workflows"], "merge_workflows")
    if any(item.event != "pull_request" for item in required_workflows):
        raise ConfigurationError("pre-merge workflows must use the pull_request event")
    if any(item.event != "push" or item.ref != base_ref for item in merge_workflows):
        raise ConfigurationError("merge CI must use push on the exact base_ref")

    issue_values = raw["wave_issue_numbers"]
    if not isinstance(issue_values, list) or not issue_values:
        raise ConfigurationError("wave_issue_numbers must be a nonempty array")
    issues = tuple(
        _positive_int(item, f"wave_issue_numbers[{index}]", ConfigurationError)
        for index, item in enumerate(issue_values)
    )
    if len(issues) != len(set(issues)):
        raise ConfigurationError("wave_issue_numbers must be unique")

    board_raw = _mapping(raw["board"], "board", ConfigurationError)
    _exact_keys(board_raw, {"project_id", "item_id", "field_id", "value"}, set(), "board", ConfigurationError)
    value_json = _canonical_json(board_raw["value"], "board.value", ConfigurationError)
    board = BoardSync(
        project_id=_string(board_raw["project_id"], "board.project_id", ConfigurationError),
        item_id=_string(board_raw["item_id"], "board.item_id", ConfigurationError),
        field_id=_string(board_raw["field_id"], "board.field_id", ConfigurationError),
        value_json=value_json,
    )
    canonical = _canonical_json(copy.deepcopy(dict(raw)), "trusted configuration", ConfigurationError)
    digest = hashlib.sha256(canonical.encode("utf-8")).hexdigest()
    return TrustedConfiguration(
        repository=repository, base_ref=base_ref, merge_method=merge_method,
        attestation_issuer=_string(raw["attestation_issuer"], "attestation_issuer", ConfigurationError),
        required_workflows=required_workflows, merge_workflows=merge_workflows,
        wave_issue_numbers=issues, board=board, canonical_json=canonical, digest=digest,
    )


def compute_envelope_digest(envelope: Mapping[str, Any]) -> str:
    """Compute only the envelope's canonical self-consistency digest."""
    raw = _mapping(envelope, "envelope", EnvelopeError)
    covered = {key: copy.deepcopy(item) for key, item in raw.items() if key != "canonical_digest"}
    return hashlib.sha256(_canonical_json(covered, "envelope", EnvelopeError).encode("utf-8")).hexdigest()


def verify_external_envelope(value: Mapping[str, Any]) -> VerifiedEnvelope:
    raw = _mapping(value, "envelope", EnvelopeError)
    _exact_keys(raw, {"schema_version", "issuer", "projection", "evidence", "canonical_digest"}, set(), "envelope", EnvelopeError)
    if type(raw["schema_version"]) is not int or raw["schema_version"] != 1:
        raise EnvelopeError("unsupported envelope schema_version")
    projection_raw = _mapping(raw["projection"], "projection", EnvelopeError)
    _exact_keys(projection_raw, {"pull_number", "head_sha"}, set(), "projection", EnvelopeError)
    projection = ExternalProjection(
        pull_number=_positive_int(projection_raw["pull_number"], "projection.pull_number", EnvelopeError),
        head_sha=_sha(projection_raw["head_sha"], "projection.head_sha", EnvelopeError),
    )
    evidence = _mapping(raw["evidence"], "evidence", EnvelopeError)
    _exact_keys(evidence, {"kind", "id"}, set(), "evidence", EnvelopeError)
    kind = _string(evidence["kind"], "evidence.kind", EnvelopeError)
    if kind not in {"artifact", "check"}:
        raise EnvelopeError("evidence.kind must be artifact or check")
    digest = _mapping(raw["canonical_digest"], "canonical_digest", EnvelopeError)
    _exact_keys(digest, {"algorithm", "value"}, set(), "canonical_digest", EnvelopeError)
    if digest["algorithm"] != "sha256":
        raise EnvelopeError("canonical_digest.algorithm must be sha256")
    supplied = digest["value"]
    if not isinstance(supplied, str) or _DIGEST_RE.fullmatch(supplied) is None:
        raise EnvelopeError("canonical_digest.value must be lowercase SHA-256")
    calculated = compute_envelope_digest(raw)
    if not hmac.compare_digest(supplied, calculated):
        raise EnvelopeError("envelope canonical self-consistency mismatch")
    projection_json = _canonical_json(projection_raw, "projection", EnvelopeError)
    canonical = _canonical_json(copy.deepcopy(dict(raw)), "envelope", EnvelopeError)
    return VerifiedEnvelope(
        projection=projection,
        issuer=_string(raw["issuer"], "issuer", EnvelopeError),
        evidence_kind=kind,
        evidence_id=_positive_int(evidence["id"], "evidence.id", EnvelopeError),
        digest=supplied,
        projection_digest=hashlib.sha256(projection_json.encode("utf-8")).hexdigest(),
        canonical_json=canonical,
    )


def _api_call(label: str, operation: Any, *args: Any, **kwargs: Any) -> Any:
    try:
        return operation(*args, **kwargs)
    except FinalizerError:
        raise
    except Exception as exc:
        raise ApiUncertainty(f"GitHub API uncertainty during {label}: {exc}") from exc


def _api_mapping(value: Any, label: str) -> Mapping[str, Any]:
    if not isinstance(value, Mapping):
        raise ApiUncertainty(f"{label} response is not an object")
    return value


def _api_items(value: Any, label: str) -> Sequence[Mapping[str, Any]]:
    if isinstance(value, Mapping):
        if set(value) != {"items", "complete"} or value.get("complete") is not True:
            raise ApiUncertainty(f"{label} collection is not explicitly complete")
        value = value["items"]
    if not isinstance(value, (list, tuple)):
        raise ApiUncertainty(f"{label} is not a complete sequence")
    result = []
    for index, item in enumerate(value):
        if not isinstance(item, Mapping):
            raise ApiUncertainty(f"{label}[{index}] is malformed")
        result.append(item)
    return result


def _nested(value: Mapping[str, Any], path: Tuple[str, ...], label: str) -> Any:
    current: Any = value
    for key in path:
        if not isinstance(current, Mapping) or key not in current:
            raise ApiUncertainty(f"{label} is missing {'.'.join(path)}")
        current = current[key]
    return current


def _api_bool(value: Any, label: str) -> bool:
    if type(value) is not bool:
        raise ApiUncertainty(f"{label} is not boolean")
    return value


def _api_positive_int(value: Any, label: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise ApiUncertainty(f"{label} is not a positive integer")
    return value


def _time(value: Any, label: str) -> datetime:
    if not isinstance(value, str):
        raise ApiUncertainty(f"{label} is not a timestamp")
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as exc:
        raise ApiUncertainty(f"{label} is malformed") from exc
    if parsed.tzinfo is None:
        raise ApiUncertainty(f"{label} has no timezone")
    return parsed.astimezone(timezone.utc)


def _verify_attestation(
    client: GitHubClient, config: TrustedConfiguration, envelope: VerifiedEnvelope
) -> str:
    if envelope.issuer != config.attestation_issuer:
        raise AttestationError("envelope issuer is not the configured issuer")
    response = _api_call(
        "verify GitHub-attested evidence", client.verify_github_attested_evidence,
        config.repository, copy.deepcopy(envelope.evidence_reference()),
    )
    evidence = _api_mapping(response, "attested evidence")
    required = {
        "verified", "reference", "repository", "pull_number", "head_sha",
        "projection_digest", "issuer", "latest",
    }
    if set(evidence) != required:
        raise AttestationError("attested evidence response has missing or unknown fields")
    expected = {
        "verified": True,
        "reference": envelope.evidence_reference(),
        "repository": config.repository,
        "pull_number": envelope.projection.pull_number,
        "head_sha": envelope.projection.head_sha,
        "projection_digest": envelope.projection_digest,
        "issuer": config.attestation_issuer,
        "latest": True,
    }
    if not _typed_equal(dict(evidence), expected):
        raise AttestationError("GitHub-attested evidence is unverified, stale, or not exactly bound")
    canonical = _canonical_json(dict(evidence), "attested evidence", AttestationError)
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


def _validate_pr(value: Any, config: TrustedConfiguration, projection: ExternalProjection) -> Mapping[str, Any]:
    pr = _api_mapping(value, "pull request")
    number = _nested(pr, ("number",), "pull request")
    if isinstance(number, bool) or number != projection.pull_number:
        raise ValidationError("pull request number does not match the attested projection")
    state = _nested(pr, ("state",), "pull request")
    if state not in {"open", "closed"}:
        raise ApiUncertainty("pull request state is malformed")
    merged = _api_bool(_nested(pr, ("merged",), "pull request"), "pull request.merged")
    if not merged and state != "open":
        raise ValidationError("unmerged pull request is not open")
    if _nested(pr, ("base", "ref"), "pull request") != config.base_ref:
        raise ValidationError("pull request base ref does not match trusted configuration")
    if _nested(pr, ("base", "repo", "full_name"), "pull request") != config.repository:
        raise ValidationError("pull request repository does not match trusted configuration")
    head_sha = _nested(pr, ("head", "sha"), "pull request")
    if head_sha != projection.head_sha:
        raise ValidationError("pull request head does not match attested projection")
    _sha(head_sha, "pull request head", ApiUncertainty)
    head_ref = _nested(pr, ("head", "ref"), "pull request")
    _nested(pr, ("head", "repo", "full_name"), "pull request")
    if any(item.ref != head_ref for item in config.required_workflows):
        raise CheckError("configured workflow ref does not exactly match pull request head ref")
    if projection.pull_number in config.wave_issue_numbers:
        raise ConfigurationError("wave issue numbers must never include the pull request number")
    if merged:
        _sha(pr.get("merge_commit_sha"), "pull request merge SHA", ApiUncertainty)
        _time(pr.get("merged_at"), "pull request.merged_at")
    return pr


def _enabled(protection: Mapping[str, Any], key: str) -> bool:
    return _api_bool(_nested(protection, (key, "enabled"), "branch protection"), f"{key}.enabled")


def _parse_policy(value: Any) -> ProtectionPolicy:
    protection = _api_mapping(value, "branch protection")
    required = _nested(protection, ("required_status_checks",), "branch protection")
    if not isinstance(required, Mapping):
        raise ApiUncertainty("required_status_checks is malformed")
    if _api_bool(required.get("strict"), "required_status_checks.strict") is not True:
        raise PolicyError("required status checks must be strict")
    contexts = required.get("contexts")
    checks = required.get("checks")
    if not isinstance(contexts, list) or not isinstance(checks, list):
        raise ApiUncertainty("required status check contexts/checks are malformed")
    declared = set()
    for context in contexts:
        if not isinstance(context, str) or not context:
            raise ApiUncertainty("legacy required status context is malformed")
        declared.add(context)
    app_ids: dict[str, int] = {}
    for check in checks:
        if not isinstance(check, Mapping) or set(check) != {"context", "app_id"}:
            raise ApiUncertainty("required status check is malformed")
        context = check["context"]
        if not isinstance(context, str) or not context:
            raise ApiUncertainty("required status check context is malformed")
        app_id = check["app_id"]
        if app_id is None:
            raise PolicyError("legacy unpinned required status checks are forbidden")
        app_id = _api_positive_int(app_id, "required status check app_id")
        if context in app_ids and app_ids[context] != app_id:
            raise PolicyError("required context has conflicting App IDs")
        app_ids[context] = app_id
        declared.add(context)
    if not declared:
        raise PolicyError("required status check policy must be nonempty")
    if any(context not in app_ids for context in declared):
        raise PolicyError("every required context must have a non-null App ID")
    if not _enabled(protection, "enforce_admins"):
        raise PolicyError("branch protection must include administrators")
    if not _enabled(protection, "required_linear_history"):
        raise PolicyError("linear history must be required")
    if _enabled(protection, "allow_force_pushes"):
        raise PolicyError("force pushes must be disabled")
    if _enabled(protection, "allow_deletions"):
        raise PolicyError("branch deletion must be disabled")
    return ProtectionPolicy(tuple(sorted(declared)), tuple(sorted(app_ids.items())))


def _identity_matches(run: Mapping[str, Any], workflow: Union[str, int]) -> bool:
    return run.get("workflow_id") == workflow if isinstance(workflow, int) else run.get("path") == workflow


def _verify_checks(
    client: GitHubClient, config: TrustedConfiguration, sha: str,
    contracts: Tuple[WorkflowContract, ...], policy: Optional[ProtectionPolicy],
    *, forbidden_run_ids: Tuple[int, ...] = (), not_before: Optional[datetime] = None,
) -> Tuple[int, ...]:
    workflows = _api_items(
        _api_call("list workflow runs", client.list_workflow_runs, config.repository, sha),
        "workflow runs",
    )
    checks = _api_items(
        _api_call("list check runs", client.list_check_runs, config.repository, sha),
        "check runs",
    )
    contexts = {item.context for item in contracts}
    if policy is not None and contexts != set(policy.required_contexts):
        raise PolicyError("trusted workflow contracts must exactly cover protected contexts")
    selected_ids = []
    for contract in contracts:
        candidates = [run for run in workflows if _identity_matches(run, contract.workflow)]
        if not candidates:
            raise CheckError(f"required workflow {contract.workflow!r} has no run")
        ranked = []
        for run in candidates:
            rank = (
                _api_positive_int(run.get("run_number"), "workflow run_number"),
                _api_positive_int(run.get("run_attempt"), "workflow run_attempt"),
            )
            ranked.append((rank, run))
        latest_rank = max(rank for rank, unused in ranked)
        latest = [run for rank, run in ranked if rank == latest_rank]
        if len(latest) != 1:
            raise ApiUncertainty("latest workflow attempt is ambiguous")
        run = latest[0]
        actual = {
            "repository": _nested(run, ("repository", "full_name"), "workflow run"),
            "ref": run.get("head_branch"), "event": run.get("event"), "SHA": run.get("head_sha"),
        }
        expected = {
            "repository": config.repository, "ref": contract.ref,
            "event": contract.event, "SHA": sha,
        }
        if actual != expected:
            raise CheckError("latest workflow attempt provenance mismatch")
        if run.get("status") != "completed" or run.get("conclusion") != "success":
            raise CheckError("latest workflow attempt is not successful")
        run_id = _api_positive_int(run.get("id"), "workflow run id")
        if run_id in forbidden_run_ids:
            raise CheckError("merge CI reused pre-merge workflow evidence")
        if not_before is not None and _time(run.get("created_at"), "workflow run.created_at") < not_before:
            raise CheckError("merge CI predates the confirmed merge")
        suite_id = _api_positive_int(run.get("check_suite_id"), "workflow check_suite_id")
        matching = []
        for check in checks:
            if check.get("name") != contract.context:
                continue
            suite = check.get("check_suite")
            if not isinstance(suite, Mapping):
                raise ApiUncertainty("check run check_suite is malformed")
            if suite.get("id") == suite_id:
                matching.append(check)
        if len(matching) != 1:
            raise CheckError("latest attempt must have exactly one required check run")
        check = matching[0]
        if check.get("head_sha") != sha:
            raise CheckError("required check is not for the exact SHA")
        if check.get("status") != "completed" or check.get("conclusion") != "success":
            raise CheckError("required check is not successful")
        app = check.get("app")
        if not isinstance(app, Mapping):
            raise ApiUncertainty("required check App provenance is missing")
        if app.get("slug") != contract.app_slug or app.get("id") != contract.app_id:
            raise CheckError("required check App provenance mismatch")
        if policy is not None and policy.app_id_for(contract.context) != contract.app_id:
            raise PolicyError("workflow App does not match pinned branch protection App")
        selected_ids.append(run_id)
    return tuple(sorted(selected_ids))


def _observe(
    client: GitHubClient, config: TrustedConfiguration, projection: ExternalProjection,
    *, require_unmerged: bool = False,
) -> Tuple[Mapping[str, Any], ProtectionPolicy, Tuple[int, ...]]:
    pr = _validate_pr(
        _api_call("get pull request", client.get_pull_request, config.repository, projection.pull_number),
        config, projection,
    )
    if require_unmerged and pr["merged"] is True:
        raise MergeError("cannot prepare finalization for an already merged pull request")
    policy = _parse_policy(_api_call(
        "get branch protection", client.get_branch_protection, config.repository, config.base_ref
    ))
    run_ids = _verify_checks(client, config, projection.head_sha, config.required_workflows, policy)
    return pr, policy, run_ids


def _policy_dict(policy: ProtectionPolicy) -> Mapping[str, Any]:
    return {
        "required_contexts": list(policy.required_contexts),
        "app_ids": [list(item) for item in policy.app_ids],
        "strict": policy.strict, "admins_enforced": policy.admins_enforced,
        "linear_history": policy.linear_history,
        "force_pushes_allowed": policy.force_pushes_allowed,
        "deletions_allowed": policy.deletions_allowed,
    }


def _prepared_material(
    config_json: str, envelope_json: str, config_digest: str, envelope_digest: str,
    attestation_digest: str, head_sha: str, policy: ProtectionPolicy,
    premerge_run_ids: Tuple[int, ...], already_merged: bool,
) -> bytes:
    value = {
        "config_json": config_json, "envelope_json": envelope_json,
        "config_digest": config_digest, "envelope_digest": envelope_digest,
        "attestation_digest": attestation_digest, "head_sha": head_sha,
        "policy": _policy_dict(policy), "premerge_run_ids": list(premerge_run_ids),
        "already_merged": already_merged,
    }
    return _canonical_json(value, "prepared finalization", ValidationError).encode("utf-8")


def _seal(*args: Any) -> str:
    return hmac.new(_PREPARED_KEY, _prepared_material(*args), hashlib.sha256).hexdigest()


def prepare_finalization(
    client: GitHubClient, trusted_configuration: Mapping[str, Any], envelope: Mapping[str, Any]
) -> PreparedFinalization:
    """Authenticate evidence and read all pre-merge facts without mutation."""
    config = load_trusted_configuration(trusted_configuration)
    verified = verify_external_envelope(envelope)
    attestation_digest = _verify_attestation(client, config, verified)
    _, policy, run_ids = _observe(
        client, config, verified.projection, require_unmerged=True
    )
    fields = (
        config.canonical_json, verified.canonical_json, config.digest, verified.digest,
        attestation_digest, verified.projection.head_sha, policy, run_ids, False,
    )
    prepared = PreparedFinalization(*fields, seal=_seal(*fields))
    _register_prepared(prepared)
    return prepared


def _validate_prepared(
    prepared: PreparedFinalization,
) -> Tuple[TrustedConfiguration, VerifiedEnvelope]:
    if type(prepared) is not PreparedFinalization:
        raise ValidationError("finalize requires an authentic PreparedFinalization")
    if prepared.already_merged is not False:
        raise ValidationError("PreparedFinalization must originate from an unmerged pull request")
    issued = _ISSUED_PREPARED.get(id(prepared))
    if issued is None or issued() is not prepared:
        raise ValidationError("PreparedFinalization was not issued by prepare_finalization")
    fields = (
        prepared.config_json, prepared.envelope_json, prepared.config_digest,
        prepared.envelope_digest, prepared.attestation_digest, prepared.head_sha,
        prepared.policy, prepared.premerge_run_ids, prepared.already_merged,
    )
    expected_seal = _seal(*fields)
    if not isinstance(prepared.seal, str) or not hmac.compare_digest(prepared.seal, expected_seal):
        raise ValidationError("PreparedFinalization seal is invalid")
    try:
        config_raw = json.loads(prepared.config_json)
        envelope_raw = json.loads(prepared.envelope_json)
    except (TypeError, json.JSONDecodeError) as exc:
        raise ValidationError("PreparedFinalization snapshots are malformed") from exc
    config = load_trusted_configuration(config_raw)
    envelope = verify_external_envelope(envelope_raw)
    if config.digest != prepared.config_digest or envelope.digest != prepared.envelope_digest:
        raise ValidationError("PreparedFinalization snapshot digest mismatch")
    if envelope.projection.head_sha != prepared.head_sha:
        raise ValidationError("PreparedFinalization head binding mismatch")
    return config, envelope


def _confirmed_merge(pr: Mapping[str, Any]) -> Tuple[str, datetime]:
    if pr.get("merged") is not True:
        raise MergeError("refetched pull request is not merged")
    return (
        _sha(pr.get("merge_commit_sha"), "refetched merge SHA", MergeError),
        _time(pr.get("merged_at"), "refetched pull request.merged_at"),
    )


def _typed_equal(left: Any, right: Any) -> bool:
    if type(left) is not type(right):
        return False
    if isinstance(left, dict):
        return set(left) == set(right) and all(_typed_equal(left[key], right[key]) for key in left)
    if isinstance(left, list):
        return len(left) == len(right) and all(_typed_equal(a, b) for a, b in zip(left, right))
    return left == right


def _validated_issue(value: Any, number: int) -> Mapping[str, Any]:
    issue = _api_mapping(value, f"issue {number}")
    if issue.get("number") != number or isinstance(issue.get("number"), bool):
        raise ApiUncertainty(f"issue {number} identity is malformed")
    if "pull_request" in issue:
        raise ValidationError(f"configured issue {number} is a pull request")
    if issue.get("state") not in {"open", "closed"}:
        raise ApiUncertainty(f"issue {number} state is malformed")
    return issue


def _read_issue(client: GitHubClient, config: TrustedConfiguration, number: int) -> Mapping[str, Any]:
    return _validated_issue(
        _api_call(f"get issue {number}", client.get_issue, config.repository, number), number
    )


def _sync_issues(client: GitHubClient, config: TrustedConfiguration) -> None:
    for number in config.wave_issue_numbers:
        issue = _read_issue(client, config, number)
        if issue["state"] == "open":
            try:
                _api_call(f"close issue {number}", client.close_issue, config.repository, number)
            except ApiUncertainty as close_error:
                # A lost mutation response is recoverable only if a fresh read
                # proves the exact configured issue reached the desired state.
                confirmed = _read_issue(client, config, number)
                if confirmed["state"] != "closed":
                    raise close_error
            confirmed = _read_issue(client, config, number)
            if confirmed["state"] != "closed":
                raise ApiUncertainty(f"issue {number} close was not confirmed")


def _sync_board(client: GitHubClient, config: TrustedConfiguration) -> None:
    board = config.board
    desired = board.value()
    args = (config.repository, board.project_id, board.item_id, board.field_id)
    current = _api_call("read project field", client.get_project_field_value, *args)
    if _typed_equal(current, desired):
        return
    try:
        _api_call(
            "update project field", client.update_project_field_value,
            *args, copy.deepcopy(desired),
        )
    except ApiUncertainty as update_error:
        confirmed = _api_call("confirm project field after uncertain update", client.get_project_field_value, *args)
        if not _typed_equal(confirmed, desired):
            raise update_error
    confirmed = _api_call("confirm project field", client.get_project_field_value, *args)
    if not _typed_equal(confirmed, desired):
        raise ApiUncertainty("project field synchronization was not type-strictly confirmed")


def finalize_prepared(client: GitHubClient, prepared: PreparedFinalization) -> FinalizationResult:
    """Revalidate the sealed snapshot, merge, verify merge CI, then synchronize."""
    config, envelope = _validate_prepared(prepared)
    attestation_digest = _verify_attestation(client, config, envelope)
    if not hmac.compare_digest(attestation_digest, prepared.attestation_digest):
        raise AttestationError("attestation changed after prepare")
    fresh_pr, fresh_policy, fresh_run_ids = _observe(client, config, envelope.projection)
    if fresh_policy != prepared.policy:
        raise PolicyError("branch protection changed after prepare")
    if fresh_run_ids != prepared.premerge_run_ids:
        raise CheckError("pre-merge workflow evidence changed after prepare")

    already_merged = fresh_pr["merged"] is True
    if already_merged:
        confirmed_pr = fresh_pr
        merge_sha, merged_at = _confirmed_merge(confirmed_pr)
    else:
        response: Optional[Mapping[str, Any]] = None
        merge_uncertainty: Optional[ApiUncertainty] = None
        try:
            response = _api_mapping(_api_call(
                "merge pull request", client.merge_pull_request,
                config.repository, envelope.projection.pull_number,
                sha=envelope.projection.head_sha, merge_method=config.merge_method,
            ), "merge response")
        except ApiUncertainty as exc:
            merge_uncertainty = exc
        if response is not None and response.get("merged") is not True:
            raise MergeError("GitHub did not confirm merged=true")
        response_sha = None if response is None else response.get("sha")
        if response is not None:
            response_sha = _sha(response_sha, "merge response SHA", MergeError)
        refetched = _validate_pr(
            _api_call("refetch merged pull request", client.get_pull_request,
                      config.repository, envelope.projection.pull_number),
            config, envelope.projection,
        )
        if refetched.get("merged") is not True and merge_uncertainty is not None:
            raise merge_uncertainty
        confirmed_pr = refetched
        merge_sha, merged_at = _confirmed_merge(confirmed_pr)
        if response_sha is not None and not hmac.compare_digest(response_sha, merge_sha):
            raise MergeError("merge response SHA differs from refetched pull request")

    _verify_checks(
        client, config, merge_sha, config.merge_workflows, None,
        forbidden_run_ids=prepared.premerge_run_ids, not_before=merged_at,
    )
    # No issue or project mutation is reachable before confirmed merge + merge CI.
    _sync_issues(client, config)
    _sync_board(client, config)
    return FinalizationResult(
        repository=config.repository, pull_number=envelope.projection.pull_number,
        head_sha=envelope.projection.head_sha, merge_sha=merge_sha,
        already_merged=already_merged, merge_ci_verified=True,
        post_merge_synchronized=True,
    )


def finalize(
    client: GitHubClient, trusted_configuration: Mapping[str, Any], envelope: Mapping[str, Any]
) -> FinalizationResult:
    return finalize_prepared(client, prepare_finalization(client, trusted_configuration, envelope))


def main(argv: Optional[Sequence[str]] = None, *, client: Optional[GitHubClient] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--trusted-config", required=True, type=Path)
    parser.add_argument("--envelope", required=True, type=Path)
    parser.add_argument("--prepare-only", action="store_true")
    args = parser.parse_args(argv)
    if client is None:
        parser.error("the digest-verifying launcher must inject an authenticated GitHub adapter")
    try:
        with args.trusted_config.open("r", encoding="utf-8") as stream:
            config = json.load(stream)
        with args.envelope.open("r", encoding="utf-8") as stream:
            envelope_value = json.load(stream)
        prepared = prepare_finalization(client, config, envelope_value)
        if args.prepare_only:
            output = {
                "prepared": True, "head_sha": prepared.head_sha,
                "canonical_self_consistency_verified": True,
                "github_attestation_verified": True,
                "config_digest": prepared.config_digest,
                "attestation_digest": prepared.attestation_digest,
            }
        else:
            result = finalize_prepared(client, prepared)
            output = {
                "repository": result.repository, "pull_number": result.pull_number,
                "head_sha": result.head_sha, "merge_sha": result.merge_sha,
                "already_merged": result.already_merged,
                "merge_ci_verified": result.merge_ci_verified,
                "post_merge_synchronized": result.post_merge_synchronized,
                "canonical_self_consistency_verified": True,
                "github_attestation_verified": True,
            }
        print(json.dumps(output, sort_keys=True))
        return 0
    except (OSError, json.JSONDecodeError, FinalizerError) as exc:
        parser.exit(1, f"wave-finalizer: {exc}\n")
        return 1


if __name__ == "__main__":
    main()
