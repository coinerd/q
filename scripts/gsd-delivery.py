#!/usr/bin/env python3
"""Coordinator-owned delivery preparation/readback. Never merges or approves PRs.

status: exact protected-main proof and current-checkout readback (JSON).
prepare: produce a deliberately gate-red schema-2 binding trio in a new directory.
review: validate the durable independent implementation review at the receipt
  identity (binding-named evidence trio at the receipt head through the
  unchanged strict gate, plus the reviewed-sha receipt-binding clause);
  outcomes reviewed / awaiting-review; never generates or approves a review.
sync: explicit, clean-tree, expected-branch, fast-forward-only; never switches branches.

Remediation contract (controller review findings):
  * total process budget stays strictly under the 240 s runtime caller; every
    subprocess/API call is clamped to the remaining budget and fails closed;
  * GitHub API reads are cached per process (check pages per SHA, run details)
    so verifying 13 required checks never re-issues requests;
  * malformed/nullable API fields fail closed (safe accessors, never a crash);
    authentication failures surface an explicit reason without ever echoing
    credential material from commands or the environment;
  * prepare only accepts a source trio that exactly equals the frozen wave's
    declared output (immutable campaign plan snapshot verified through the
    trusted Racket plan-snapshot module); with no declared outputs only the
    exact campaign-hash-named source is accepted. Wave index or mtime alone is
    never sufficient; an explicit foreign plan-id in the source is always
    refused; re-binding a published wave to a different implementation is
    refused and output directories are never overwritten;
  * the exact PR head ref is fetched and the fetched SHA asserted, and the
    implementation merge tree must match the PR head tree (squash semantics);
  * status enforces implementation-sha == merge-sha, single-parent publication
    and implementation commits, publication through exactly one merged
    same-repo PR, branch protection configuring the expected required-check
    name snapshot, all required checks green at the exact implementation head
    under the authenticated Actions identity, an actual governance run at the
    publication commit, and a changed-evidence list of exactly the current
    binding (green-skip rule-out). Native GitHub approval counts are
    deliberately NOT required: the mandatory independent review is the
    schema-2 review artifact, which the unchanged strict gate enforces.
"""
import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path, PurePosixPath

HERE = Path(__file__).resolve().parent
EXCLUDES = [':(exclude)docs/reports/gsd-wave-' + d + '/**'
            for d in ('evidence', 'reviews', 'validation')]
GOVERNANCE_CHECK = 'gsd-governance'
EXCLUDED_PREFIXES = tuple('docs/reports/gsd-wave-' + d + '/'
                          for d in ('evidence', 'reviews', 'validation'))
ACTIONS_APP_ID = 15368
RUNTIME_CALLER_BUDGET = 240.0  # delivery-handoff run-subprocess timeout (seconds)
EMPTY_SHA = hashlib.sha256(b'').hexdigest()
# Register F12: sentinel placeholders that never constitute reviewer identity.
SENTINEL_TEXT = re.compile(
    r'(?:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx|n/a|na)[:.;!,?]*',
    re.IGNORECASE)
# Compound sentinels ("PENDING-INDEPENDENT-REVIEW") are not whole-string
# placeholders, but they still encode placeholder semantics in an identity
# field. Boundary-anchored so genuine names never match (no short tokens
# like "na" here; they remain covered by SENTINEL_TEXT.fullmatch).
SENTINEL_COMPOUND = re.compile(
    r'(?i)(?:^|[^a-z])(?:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx)'
    r'(?:[^a-z]|$)')

class Pending(RuntimeError):
    pass

def require(ok, reason):
    if not ok:
        raise Pending(reason)

def _budget_setting():
    try:
        value = float(os.environ.get('GSD_DELIVERY_BUDGET_SEC', ''))
    except ValueError:
        return 200.0
    return value if 10.0 <= value <= RUNTIME_CALLER_BUDGET - 5.0 else 200.0

PROCESS_BUDGET_SEC = _budget_setting()
_start = time.monotonic()

def remaining_budget():
    return PROCESS_BUDGET_SEC - (time.monotonic() - _start)

_AUTH_ERROR = re.compile(
    r'gh auth|GITHUB_TOKEN|bad credentials|authentication required|HTTP 40[13]',
    re.IGNORECASE)

# W3 register F8: typed failure classification. Every non-auth subprocess
# failure carries its class, the failing command and the exit code — never a
# bare "exit 128". Raw stderr is never echoed (it may contain credential
# URLs or remote text); only the classifier's own typed summary is exposed.
_REMOTE_REF_MISSING = re.compile(r"couldn't find remote ref|does not (?:appear to|exist)", re.IGNORECASE)
_NON_FAST_FORWARD = re.compile(r'non-fast-forward|\[rejected\]|fetch first|denied.*push', re.IGNORECASE)
_UNKNOWN_REF = re.compile(r'unknown revision|bad revision|ambiguous argument', re.IGNORECASE)
_REFSPEC_RE = re.compile(r'refs/heads/[A-Za-z0-9._/=-]+')

def classify_failure(args, name, exit_code, stderr):
    """Map exit codes + stderr shapes to typed, actionable refusals."""
    text = stderr if isinstance(stderr, str) else stderr.decode('utf-8', errors='replace')
    if _AUTH_ERROR.search(text):
        # Dedicated credential-free refusal; the raw shape never surfaces.
        return ('GitHub authentication failed (check: gh auth status); '
                'credentials are never logged or exposed')
    if _REMOTE_REF_MISSING.search(text):
        match = _REFSPEC_RE.search(text)
        if not match:
            match = next((m for a in args if isinstance(a, str)
                          for m in [_REFSPEC_RE.search(a)] if m), None)
        if match:
            return ('remote-ref-missing: %s is not published on origin; '
                    'push the verified head first (%s, exit %s)'
                    % (match.group(0), name, exit_code))
        return ('remote-ref-missing: the requested ref is not published on origin; '
                'push the verified head first (%s, exit %s)' % (name, exit_code))
    if _NON_FAST_FORWARD.search(text):
        return ('non-fast-forward: the remote ref diverged; fetch, reanchor '
                'the verified head and retry (%s, exit %s)' % (name, exit_code))
    if _UNKNOWN_REF.search(text):
        return ('unknown-ref: %s referenced an unknown revision; inspect the '
                'repository refs locally before retrying (%s, exit %s)'
                % (name, name, exit_code))
    return ('delivery command failed (%s, exit %s); inspect authentication, '
            'repository refs and required artifacts locally' % (name, exit_code))

def command(args, cwd=None, timeout=45, raw=False):
    """Bounded fail-closed subprocess. Each call is clamped to the remaining
    process budget so the total can never exceed the 240 s runtime caller."""
    left = remaining_budget()
    require(left > 1.0,
            'delivery process budget exhausted (%.0fs); rerun the action separately'
            % PROCESS_BUDGET_SEC)
    bounded = min(float(timeout), max(0.5, left - 0.5))
    try:
        result = subprocess.run(args, cwd=cwd, capture_output=True, timeout=bounded)
    except subprocess.TimeoutExpired as error:
        raise Pending('delivery command timed out after %.0fs (budget %.0fs): %s'
                      % (bounded, PROCESS_BUDGET_SEC, args[0])) from error
    except (OSError, ValueError) as error:
        raise Pending('delivery command unavailable: ' + str(error)) from error
    if result.returncode != 0:
        stderr = result.stderr if isinstance(result.stderr, bytes) else b''
        # git/gh/racket errors may contain credential-bearing URLs or remote
        # text. Only the typed classifier summary is surfaced, never raw output.
        raise Pending(classify_failure(args, Path(args[0]).name, result.returncode, stderr))
    return result.stdout if raw else result.stdout.decode('utf-8', errors='replace')

def git(repo, *args, raw=False):
    return command(['git', '-C', str(repo), *args], raw=raw)

def full_sha(value):
    return isinstance(value, str) and re.fullmatch('[0-9a-f]{40}', value) is not None

def dig(value, *keys):
    """Safe nullable-field traversal: any non-dict along the path yields None."""
    for key in keys:
        if not isinstance(value, dict):
            return None
        value = value.get(key)
    return value

def binding_path(plan, wave):
    require(isinstance(plan, str) and re.fullmatch('[0-9a-f]{64}', plan), 'invalid campaign id')
    require(type(wave) is int and wave >= 0, 'invalid wave index')
    return f'docs/reports/gsd-wave-evidence/{plan}-w{wave}.rktd'

def artifact_path(value, directory):
    require(isinstance(value, str), 'missing artifact path')
    path = PurePosixPath(value)
    require(not path.is_absolute() and '..' not in path.parts and
            value.startswith('docs/reports/' + directory + '/') and
            path.suffix == '.rktd', 'invalid artifact path')
    return value

def repository(repo):
    # Read the *configured* origin URL: url.<x>.insteadOf rewrites are
    # transport conveniences and must never redefine repository identity.
    origin = git(repo, 'config', '--get', 'remote.origin.url').strip()
    match = re.fullmatch(r'(?:https://github\.com/|git@github\.com:)([\w.-]+/[\w.-]+?)(?:\.git)?', origin)
    require(match, 'origin must identify a GitHub repository (no arbitrary API host)')
    return match.group(1)

_API_CACHE = {}
_CHECKS_CACHE = {}
POLICY_PATH = 'scripts/required-pr-checks.policy'

def api(slug, route, paginate=False, _refresh=False):
    key = (slug, route, paginate)
    if not _refresh and key in _API_CACHE:
        return _API_CACHE[key]
    args = ['gh', 'api', '--hostname', 'github.com', 'repos/' + slug + '/' + route]
    if paginate:
        args += ['--paginate', '--slurp']
    try:
        data = json.loads(command(args, timeout=60))
    except (ValueError, TypeError) as error:
        raise Pending('malformed GitHub response for ' + route) from error
    require(isinstance(data, (dict, list)), 'malformed GitHub response for ' + route)
    _API_CACHE[key] = data
    return data

def read_datum(path):
    try:
        return json.loads(command(['racket', str(HERE / 'gsd-binding-data.rkt'), 'read', str(path)],
                                  timeout=60))
    except ValueError as error:
        raise Pending('malformed binding datum: ' + str(path)) from error

def materialize(repo, ref, relative, root):
    mode = git(repo, 'ls-tree', ref, '--', relative).split(' ', 1)[0]
    require(mode == '100644', 'artifact is absent, executable, or a symlink: ' + relative)
    target = root / relative
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(git(repo, 'show', f'{ref}:{relative}', raw=True))
    return target

def digest(repo, base, head):
    return hashlib.sha256(git(repo, 'diff', '--binary', base + '...' + head,
                              '--', '.', *EXCLUDES, raw=True)).hexdigest()

def blob_or_none(repo, ref, path):
    try:
        return git(repo, 'show', f'{ref}:{path}', raw=True)
    except Pending:
        return None

def policy_names(repo, ref):
    """Expected required-check name snapshot at a ref (trusted local object)."""
    data = blob_or_none(repo, ref, 'scripts/required-pr-checks.policy')
    require(data is not None, 'required-check policy is absent at ' + ref)
    with tempfile.NamedTemporaryFile(prefix='q-delivery-policy-', delete=False) as handle:
        handle.write(data)
        name = handle.name
    try:
        names = json.loads(command(['racket', str(HERE / 'gsd-binding-data.rkt'),
                                    'read-any', name], timeout=60))
    except ValueError as error:
        raise Pending('malformed required-check policy at ' + ref) from error
    finally:
        os.unlink(name)
    require(isinstance(names, list) and names and
            all(isinstance(item, str) and item.strip() for item in names) and
            len(set(names)) == len(names), 'malformed required-check policy at ' + ref)
    return names

def validate_trio(repo, ref, relative, base):
    """Run the existing, unchanged strict gate on committed blobs and real diff.

    Returns the evidence and review data so callers can enforce the amended
    approval contract against the very artifacts the strict gate accepted."""
    with tempfile.TemporaryDirectory(prefix='q-delivery-validate-') as temp:
        root = Path(temp)
        evidence = read_datum(materialize(repo, ref, relative, root))
        require(isinstance(evidence, dict) and evidence.get('schema-version') == 2,
                'delivery requires a schema-2 hash trio')
        review = artifact_path(evidence.get('review-artifact'), 'gsd-wave-reviews')
        validation = artifact_path(evidence.get('validation-artifact'), 'gsd-wave-validation')
        for path in (review, validation, 'scripts/required-pr-checks.policy'):
            materialize(repo, ref, path, root)
        actual = digest(repo, base, ref)
        command(['racket', str(HERE / 'gsd-wave-gate.rkt'), str(root / relative),
                 '--content-digest', actual, '--root', str(root), '--policy',
                 str(root / 'scripts/required-pr-checks.policy')], timeout=60)
        return evidence, read_datum(root / review), [relative, review, validation]

def validate_merge(pr, commit, slug, merge, head):
    require(isinstance(pr, dict) and isinstance(commit, dict), 'malformed PR/commit response')
    require(pr_is_merged(pr), 'implementation PR is not merged')
    require(dig(pr, 'merge_commit_sha') == merge and full_sha(merge),
            'implementation merge SHA mismatch')
    require(dig(pr, 'head', 'sha') == head and full_sha(head),
            'implementation head SHA mismatch')
    require(dig(pr, 'base', 'ref') == 'main' and
            dig(pr, 'base', 'repo', 'full_name') == slug and
            dig(pr, 'head', 'repo', 'full_name') == slug,
            'implementation repository/default branch mismatch')
    parents = commit.get('parents')
    require(commit.get('sha') == merge and isinstance(parents, list) and len(parents) == 1,
            'delivery must have single-parent squash provenance')

def validate_pr_identity(pr, slug):
    """Same-repo branch identity: the implementation PR must target main of
    the very same repository on both sides (no forks, no alternate bases)."""
    require(isinstance(pr, dict), 'malformed PR response')
    require(dig(pr, 'base', 'ref') == 'main' and
            dig(pr, 'base', 'repo', 'full_name') == slug and
            dig(pr, 'head', 'repo', 'full_name') == slug,
            'implementation PR must be a same-repository main-base pull request')

def merge_authorization(evidence, wave, head, base, repo):
    """Recorded operator authorization from the evidence record's
    `merge-authorization` object (amended approval contract, F11).

    The authorization must name the operator, this exact wave, the exact
    authorized head and the authorized action, and must cite its source. The
    authorized head is the verified implementation head recorded by the same
    evidence (its `implementation-sha`): a committed record cannot contain its
    own commit SHA, so the exact-head binding is enforced where it is both
    meaningful and satisfiable — against the receipt head, and against the
    merged tip via content equality. The excluded-paths digest from the PR
    base to the authorized receipt must equal the digest from the base to the
    merge tip (which the strict gate just verified against the recorded
    content-digest), so the merge delivers exactly the authorized content and
    nothing else. Refuses `no-operator-authorization` in every
    under-specified direction — including an authorization naming another
    head, another wave, or citing no source."""
    auth = evidence.get('merge-authorization') if isinstance(evidence, dict) else None
    require(isinstance(auth, dict),
            'no-operator-authorization: evidence record carries no merge-authorization object')
    for field in ('operator', 'wave', 'head', 'action', 'source'):
        value = auth.get(field)
        require(isinstance(value, str) and value.strip(),
                'no-operator-authorization: merge-authorization.%s is missing or empty' % field)
    require(auth.get('wave') == wave,
            'no-operator-authorization: merge-authorization names wave %r, not %r'
            % (auth.get('wave'), wave))
    receipt = evidence.get('implementation-sha')
    require(full_sha(receipt),
            'no-operator-authorization: evidence record has no implementation head to authorize')
    require(auth.get('head') == receipt,
            'no-operator-authorization: merge-authorization authorizes head %s, not the verified implementation head %s'
            % (auth.get('head'), receipt))
    authorized_digest = digest(repo, base, receipt)
    require(authorized_digest == evidence.get('content-digest') and
            digest(repo, base, head) == evidence.get('content-digest'),
            'no-operator-authorization: the merge tip no longer carries the authorized implementation content')
    return auth


def reviewed_head_approval(review, evidence, author):
    """APPROVED independent non-author review artifact bound to the exact
    verified head.

    The schema-2 review artifact is the repository's mandatory independent
    review (the strict gate already enforces its integrity); the merge gate
    additionally requires that it is APPROVED, produced by a reviewer other
    than the PR author, and bound to the exact authorized implementation head
    (the F3 head-binding rule). This replaces the unsatisfiable second-account
    GitHub review requirement (F11) without weakening any other merge
    semantics."""
    require(isinstance(review, dict),
            'no-review-artifact: the review artifact is missing or malformed')
    require(isinstance(author, str) and author.strip(),
            'no-review-artifact: PR author identity is missing or malformed; '
            'the non-author property is unverifiable')
    require(review.get('verdict') == 'APPROVED',
            'no-review-artifact: review verdict is %r, not APPROVED'
            % (review.get('verdict'),))
    reviewer = review.get('reviewer')
    require(isinstance(reviewer, str) and reviewer.strip(),
            'no-review-artifact: review has no reviewer identity')
    require(reviewer.strip().casefold() != author.strip().casefold(),
            'no-review-artifact: review has no non-author reviewer identity')
    receipt = evidence.get('implementation-sha') if isinstance(evidence, dict) else None
    require(review.get('reviewed-sha') == receipt,
            'head-binding-mismatch: review reviewed-sha %s does not bind the verified implementation head %s'
            % (review.get('reviewed-sha'), receipt))
    return review

def gh_put(slug, route, fields):
    """Authenticated GitHub mutation (PUT) through the trusted gh CLI. The
    controller is the single deliberate credential boundary; no shell, no
    token material ever leaves the process."""
    args = ['gh', 'api', '--hostname', 'github.com', '-X', 'PUT',
            'repos/' + slug + '/' + route]
    for key in sorted(fields):
        args += ['-f', f'{key}={fields[key]}']
    data = json.loads(command(args, timeout=60))
    require(isinstance(data, dict), 'malformed GitHub mutation response')
    return data

def gh_post(slug, route, fields):
    """Authenticated GitHub mutation (POST) through the trusted gh CLI. This
    mirrors gh_put so PR creation remains a single credential boundary."""
    args = ['gh', 'api', '--hostname', 'github.com', '-X', 'POST',
            'repos/' + slug + '/' + route]
    for key in sorted(fields):
        args += ['-f', f'{key}={fields[key]}']
    data = json.loads(command(args, timeout=60))
    require(isinstance(data, dict), 'malformed GitHub mutation response')
    return data

def merge(repo, plan, wave, number, expected_head, expected_branch, source):
    """Deterministic protected squash merge of the implementation PR.

    Every gate runs BEFORE any mutation; `expected_head` is the validated PR
    tip (or the receipt head when no evidence-only review commits followed it),
    never model output. Order:
      1. same-repo main-base identity
      2. exact expected head (both the PR claim and the actually-fetched refs/pull/N/head)
      3. already-merged at the expected head -> idempotent, no second merge
      4. fresh main (PR based on current origin/main)
      5. unchanged strict trio/digest preflight at PR base...head, then the
         amended approval contract (F11) on those exact artifacts: a recorded
         operator authorization naming the exact head plus an APPROVED
         non-author review artifact bound to the exact head
      6. every required check green at that exact head (policy snapshot + protection)
      7. squash-only PUT to the PR merge endpoint (never admin, never main push)
      8. post-merge single-parent squash provenance via validate_merge
    Returns {'status':'merged'|'already-merged', ...}; model or journal
    completion alone never sets delivery."""
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    pr = api(slug, f'pulls/{number}')
    require(isinstance(pr, dict), 'malformed PR response')
    validate_pr_identity(pr, slug)
    head = dig(pr, 'head', 'sha')
    require(head == expected_head and full_sha(expected_head),
            'implementation PR head does not match the verified expected head')
    if pr_is_merged(pr):
        # Idempotent resume after a lost merge response: the mutation already
        # happened, so it is not repeated — but the same pre-merge proofs still
        # run against the PR head. A merge that happened outside this gate,
        # without the amended approval contract or green required checks, is
        # refused instead of being accepted as delivered (review R3 finding).
        merge_sha = dig(pr, 'merge_commit_sha')
        validate_merge(pr, api(slug, f'commits/{merge_sha}'), slug, merge_sha, head)
        fetched = fetch_head(repo, number)
        require(fetched == head, 'fetched implementation head does not match expected head')
        evidence, review, _paths = validate_trio(repo, head, source,
                                                 dig(pr, 'base', 'sha'))
        author = dig(pr, 'user', 'login') or ''
        require(isinstance(author, str) and author.strip(),
                'malformed PR author identity; refusing the non-author review comparison')
        merge_authorization(evidence, 'W%d' % wave, head, dig(pr, 'base', 'sha'), repo)
        reviewed_head_approval(review, evidence, author)
        names = policy_names(repo, main)
        protection(slug, names)
        for name in names:
            trusted_check(slug, head, name, 'pull_request', expected_branch)
        return {'status': 'already-merged', 'merge-sha': merge_sha, 'plan-id': plan,
                'wave': wave, 'head': head, 'pr': number}
    require(pr.get('state') == 'open', 'implementation PR is neither open nor already merged')
    require(dig(pr, 'base', 'sha') == main,
            'implementation PR is not based on fresh origin/main; reanchor and re-verify')
    fetched = fetch_head(repo, number)
    require(fetched == head, 'fetched implementation head does not match expected head')
    evidence, review, _paths = validate_trio(repo, head, source, dig(pr, 'base', 'sha'))
    author = dig(pr, 'user', 'login') or ''
    require(isinstance(author, str) and author.strip(),
            'malformed PR author identity; refusing the non-author review comparison')
    merge_authorization(evidence, 'W%d' % wave, head, dig(pr, 'base', 'sha'), repo)
    reviewed_head_approval(review, evidence, author)
    names = policy_names(repo, main)
    protection(slug, names)
    for name in names:
        trusted_check(slug, head, name, 'pull_request', expected_branch)
    result = gh_put(slug, f'pulls/{number}/merge', {'merge_method': 'squash'})
    require(result.get('merged') is True and full_sha(result.get('sha')),
            'GitHub did not confirm the protected squash merge')
    merge_sha = result.get('sha')
    # Refetch WITHOUT the per-process cache: a stage-1 read must never satisfy
    # a post-mutation proof.
    merged_pr = api(slug, f'pulls/{number}', _refresh=True)
    validate_merge(merged_pr, api(slug, f'commits/{merge_sha}'), slug, merge_sha, head)
    return {'status': 'merged', 'merge-sha': merge_sha, 'plan-id': plan, 'wave': wave,
            'head': head, 'pr': number}


def checks(slug, sha):
    key = (slug, sha)
    if key in _CHECKS_CACHE:
        return _CHECKS_CACHE[key]
    # gh 2.45 has no `--paginate --slurp`; page manually in-process so every
    # gh version returns the same fail-closed result.
    result = []
    page = 1
    while True:
        data = api(slug, 'commits/%s/check-runs?per_page=100&page=%d' % (sha, page))
        require(isinstance(data, dict) and isinstance(data.get('check_runs'), list),
                'invalid check result')
        runs = data['check_runs']
        require(all(isinstance(run, dict) for run in runs), 'invalid check run entry')
        result.extend(runs)
        total = data.get('total_count')
        require(isinstance(total, int) and total >= 0, 'malformed check-total')
        if not runs or len(result) >= total:
            break
        page += 1
    require(len(result) >= total, 'check pagination incomplete')
    _CHECKS_CACHE[key] = result
    return result

def trusted_check(slug, sha, name, event, branch=None):
    candidates = [c for c in checks(slug, sha) if c.get('name') == name]
    require(candidates, 'missing check: ' + name)
    check = max(candidates, key=lambda c: c.get('id', 0))
    require(check.get('status') == 'completed' and check.get('conclusion') == 'success' and
            check.get('head_sha') == sha and dig(check, 'app', 'id') == ACTIONS_APP_ID,
            'pending, failed, or untrusted check: ' + name)
    match = re.fullmatch(r'https://github\.com/' + re.escape(slug) +
                         r'/actions/runs/(\d+)/job/\d+', check.get('details_url', ''))
    require(match, 'check has no authenticated Actions run: ' + name)
    run = api(slug, 'actions/runs/' + match.group(1))
    require(isinstance(run, dict) and run.get('status') == 'completed' and
            run.get('conclusion') == 'success' and
            run.get('head_sha') == sha and run.get('event') == event and
            dig(run, 'repository', 'full_name') == slug and
            run.get('path') == '.github/workflows/ci.yml' and
            (branch is None or run.get('head_branch') == branch),
            'check workflow/ref/event mismatch: ' + name)

def protection(slug, expected_names):
    """Branch protection must configure the expected required-check name
    snapshot. Newer additive names are allowed; an empty or missing context
    list (native checks not actually required) fails closed. Historical
    bindings stay verifiable because only the publication-time snapshot is
    demanded, not every newer current check."""
    require(isinstance(expected_names, list) and expected_names, 'invalid expected check names')
    policy = api(slug, 'branches/main/protection')
    require(isinstance(policy, dict), 'malformed branch protection response')
    contexts = dig(policy, 'required_status_checks', 'contexts')
    require(dig(policy, 'enforce_admins', 'enabled') is True and
            dig(policy, 'required_status_checks', 'strict') is True and
            dig(policy, 'allow_force_pushes', 'enabled') is False and
            dig(policy, 'allow_deletions', 'enabled') is False,
            'default branch protection is absent or weakened')
    require(isinstance(contexts, list) and contexts and
            all(isinstance(item, str) for item in contexts),
            'default branch protection requires configured native checks')
    missing = [name for name in expected_names if name not in contexts]
    require(not missing, 'branch protection does not require expected checks: ' + ', '.join(missing))
    return policy

def pr_is_merged(pr):
    """Return whether a PR response is authoritatively merged.

    GitHub's commit-association response can report ``merged: null`` for a
    real squash merge, while the pulls endpoint exposes either ``merged: true``
    or a closed PR with ``merged_at`` set. Accept both established shapes and
    nothing weaker."""
    return (isinstance(pr, dict) and
            (pr.get('merged') is True or
             (pr.get('state') == 'closed' and pr.get('merged_at') is not None)))


def branch_owner(slug):
    """Owner login of an `owner/repo` slug.

    GitHub's pulls head filter is `{owner}:{branch}` (F6); the
    `{owner}/{repo}:{branch}` form matches nothing, so both open-PR
    resolution and merged-PR recovery would silently return none and the
    `governance` ladder action could never succeed."""
    require(isinstance(slug, str) and slug.count('/') == 1,
            'malformed repository slug: ' + str(slug))
    owner, name = slug.split('/')
    require(bool(owner) and bool(name), 'malformed repository slug: ' + slug)
    return owner


def resolve_prs_for_branch(slug, branch, state):
    """Resolve PRs for a branch at a requested GitHub state.

    A branch may have at most one candidate in the returned set. The open-set
    query is used for resolve-before-create; the all-state query is used only
    by the idempotent merged-PR recovery path."""
    require(isinstance(branch, str) and branch.strip(), 'invalid branch identity')
    candidates = api(slug, f'pulls?state={state}&head={branch_owner(slug)}:{branch}')
    require(isinstance(candidates, list), 'malformed pull-request response')
    require(len(candidates) <= 1,
            'multiple pull requests already target branch ' + branch)
    return candidates[0] if candidates else None


def resolve_existing_pr(slug, branch):
    """Resolve-before-create: at most one open PR may already target the
    exact head branch. Returns it, or None when none exists; multiple open
    PRs for one branch fail closed (never silently pick one to reuse)."""
    return resolve_prs_for_branch(slug, branch, 'open')


def resolve_merged_pr(slug, branch):
    """Find one closed, merged PR for a branch for idempotent resume.

    This is deliberately separate from open-PR resolution: a lost merge response
    must not make the next delivery attempt invent a new PR or stall forever."""
    candidates = api(slug, f'pulls?state=all&head={branch_owner(slug)}:{branch}')
    require(isinstance(candidates, list), 'malformed pull-request response')
    merged = [pr for pr in candidates
              if isinstance(pr, dict) and pr.get('state') == 'closed'
              and pr_is_merged(pr) and dig(pr, 'head', 'ref') == branch]
    require(len(merged) <= 1,
            'multiple merged pull requests already target branch ' + branch)
    return merged[0] if merged else None

def refresh(repo):
    git(repo, 'fetch', '--no-tags', 'origin',
        'refs/heads/main:refs/remotes/origin/main')

def fetch_head(repo, number):
    """Fetch the exact PR head ref and assert what was actually fetched."""
    require(type(number) is int and number > 0, 'invalid implementation PR number')
    git(repo, 'fetch', '--no-tags', 'origin', f'refs/pull/{number}/head')
    fetched = git(repo, 'rev-parse', 'FETCH_HEAD').strip()
    require(full_sha(fetched), 'malformed fetched PR head')
    return fetched

def tree_of(repo, ref):
    tree = git(repo, 'rev-parse', ref + '^{tree}').strip()
    require(re.fullmatch('[0-9a-f]{40}', tree) is not None, 'malformed tree for ' + ref)
    return tree

def changed_evidence(repo, publication):
    out = git(repo, 'diff-tree', '--no-commit-id', '--name-only', '--no-renames',
              '-r', publication, '--', 'docs/reports/gsd-wave-evidence/')
    return [line.strip() for line in out.splitlines() if line.strip()]

def publication_pr(slug, publication):
    prs = api(slug, f'commits/{publication}/pulls')
    require(isinstance(prs, list) and prs, 'binding publication is not associated with a PR')
    same = [pr for pr in prs if isinstance(pr, dict) and
            dig(pr, 'base', 'ref') == 'main' and
            dig(pr, 'base', 'repo', 'full_name') == slug and
            dig(pr, 'head', 'repo', 'full_name') == slug and
            pr.get('merge_commit_sha') == publication]
    require(len(same) == 1,
            'binding publication must be exactly one merged same-repo PR')
    # GitHub's COMMIT-ASSOCIATION endpoint reports `merged: null` even for a
    # genuinely merged squash PR (only the pulls/{number} endpoint and the
    # `merged_at`/`state` fields reflect the merge authoritatively). Accept
    # the old `merged is True` shape (unit fakes), the association shape
    # (state == 'closed' AND merged_at set), and otherwise fail closed via
    # the authoritative pulls/{number} endpoint.
    pr = same[0]
    merged = (pr.get('merged') is True or
              (pr.get('state') == 'closed' and pr.get('merged_at') is not None))
    if not merged:
        number = pr.get('number')
        if isinstance(number, int):
            merged = pr_is_merged(api(slug, f'pulls/{number}'))
    require(merged, 'binding publication must be exactly one merged same-repo PR')
    return same[0]

def snapshot_facts(campaign_root, plan, wave):
    """Verify the immutable campaign plan snapshot for this plan/wave through
    the trusted Racket plan-snapshot module (full hash validation)."""
    root = Path(campaign_root) if campaign_root is not None else None
    require(root is not None and root.is_dir() and not root.is_symlink(),
            'prepare requires --campaign-root with the campaign working tree')
    binding_path(plan, wave)
    out = command(['racket', str(HERE / 'gsd-binding-data.rkt'), 'snapshot',
                   str(root), plan, str(wave)], timeout=60)
    try:
        facts = json.loads(out)
    except ValueError as error:
        raise Pending('malformed campaign snapshot facts') from error
    require(isinstance(facts, dict) and facts.get('status') == 'ok' and
            facts.get('plan-id') == plan and facts.get('wave') == wave,
            'campaign snapshot does not verify for this plan/wave')
    return facts

DECLARED_DIRS = {'evidence': 'gsd-wave-evidence',
                 'review': 'gsd-wave-reviews',
                 'validation': 'gsd-wave-validation'}


def declared_outputs(facts, repo):
    """Frozen declared trio outputs, or None when the wave declares none.
    A leading q/ path prefix is normalized only when the resolved repository
    directory itself is named q; anything else fails closed."""
    declared = facts.get('declared')
    if not isinstance(declared, dict):
        return None
    groups = {}
    for kind in ('evidence', 'review', 'validation'):
        raw = declared.get(kind)
        if raw is None:
            groups[kind] = []
        elif isinstance(raw, list):
            groups[kind] = raw
        else:
            raise Pending('malformed declared wave output for ' + kind)
    if not any(groups.values()):
        return None
    require(all(len(value) == 1 for value in groups.values()),
            'frozen wave document must declare exactly one evidence, review and '
            'validation output; filename guessing is refused')
    repo_name = Path(repo).resolve().name
    result = {}
    for kind, value in groups.items():
        path = value[0]
        require(isinstance(path, str) and path.strip(), 'malformed declared wave output')
        if path.startswith('q/'):
            require(repo_name == 'q',
                    'declared output uses a q/ prefix but the resolved repo is not q')
            path = path[2:]
        result[kind] = artifact_path(path, DECLARED_DIRS[kind])
    return result

def preflight(repo, plan, wave, expected_head, expected_branch):
    """Register F5 read-only preflight at the handoff seam (v1.00.31 W3).

    Before the first ladder action, verify: (1) the verified branch is
    published at origin, (2) the published tip equals the verified receipt
    head (or carries only excluded-evidence drift), and (3) the required-
    check policy object exists at origin/main. Mutates nothing; every
    refusal is typed with its exact remedy."""
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'preflight requires --expected-branch')
    require(isinstance(expected_head, str) and full_sha(expected_head),
            'preflight requires --expected-head (the durable receipt head)')
    binding_path(plan, wave)
    slug = repository(repo)
    refresh(repo)
    remote_ref = 'refs/heads/' + expected_branch
    tracking_ref = 'refs/remotes/origin/' + expected_branch
    try:
        git(repo, 'fetch', '--no-tags', 'origin', remote_ref + ':' + tracking_ref)
    except Pending as error:
        raise Pending('branch-not-published: branch %s is not published on origin; '
                      'push the verified head %s... first (underlying: %s)'
                      % (expected_branch, expected_head[:12], error)) from error
    tip = git(repo, 'rev-parse', tracking_ref).strip()
    require(full_sha(tip), 'malformed published branch tip for ' + expected_branch)
    require_receipt_tip(repo, expected_head, tip, 'preflight')
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    policy_names(repo, main)
    return {'status': 'ready', 'plan-id': plan, 'wave': wave,
            'branch': expected_branch, 'head': tip,
            'receipt-head': expected_head}

def status(repo, plan, wave):
    relative = binding_path(plan, wave)
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    publication = git(repo, 'log', '-1', '--format=%H', main, '--', relative).strip()
    require(publication, f'W{wave} verified; delivery pending: protected campaign binding is absent')
    require(full_sha(publication), 'malformed publication SHA')
    parents = git(repo, 'rev-list', '--parents', '-n', '1', publication).split()
    require(len(parents) == 2 and parents[0] == publication,
            'binding publication is not a single-parent protected commit')
    parent = parents[1]
    evidence, _review, paths = validate_trio(repo, publication, relative, parent)
    require(evidence.get('plan-id') == plan and evidence.get('wave') == f'W{wave}' and
            evidence.get('merge-method') == 'squash', 'campaign/wave/merge identity mismatch')
    merge, head, number = (evidence.get(k) for k in ('merge-sha', 'delivery-head-sha', 'delivery-pr'))
    wave_branch = evidence.get('wave-branch')
    require(full_sha(merge) and full_sha(head), 'binding must carry full implementation SHAs')
    require(evidence.get('implementation-sha') == merge,
            'binding implementation-sha differs from merge-sha (squash semantics violated)')
    require(type(number) is int and number > 0, 'missing implementation PR identity')
    require(isinstance(wave_branch, str) and wave_branch.strip(), 'missing implementation wave branch')
    require(changed_evidence(repo, publication) == [relative],
            'publication must change exactly the current binding evidence '
            '(possible governance green-skip refused)')
    publication_pr(slug, publication)
    pr = api(slug, f'pulls/{number}')
    validate_merge(pr, api(slug, f'commits/{merge}'), slug, merge, head)
    fetched = fetch_head(repo, number)
    require(fetched == head, 'fetched implementation head does not match binding delivery-head-sha')
    require(tree_of(repo, merge) == tree_of(repo, fetched),
            'implementation merge tree differs from PR head tree (not a clean squash)')
    names = evidence.get('required-checks')
    require(isinstance(names, list) and names and
            all(isinstance(name, str) and name.strip() for name in names),
            'binding required-checks are malformed')
    protection(slug, names)
    for name in names:
        trusted_check(slug, head, name, 'pull_request', wave_branch)
    # Governance for this exact published binding validates its reviewed trio.
    trusted_check(slug, publication, GOVERNANCE_CHECK, 'push', 'main')
    for path in paths:
        published = git(repo, 'show', f'{publication}:{path}', raw=True)
        require(git(repo, 'show', f'{main}:{path}', raw=True) == published,
                'binding trio changed since governance publication')
        require(git(repo, 'show', f'HEAD:{path}', raw=True) == published,
                'delivery sync pending: current checkout does not contain published trio')
        local = Path(repo) / path
        require(local.is_file() and not local.is_symlink() and local.read_bytes() == published,
                'delivery sync pending: local artifact differs from committed proof')
    git(repo, 'merge-base', '--is-ancestor', merge, publication)
    git(repo, 'merge-base', '--is-ancestor', publication, 'HEAD')
    return {'status':'delivered', 'merge-sha':merge, 'publication-sha':publication,
            'plan-id':plan, 'wave':wave}

def draft_request(source, plan, wave, pr, branch, required_checks):
    binding_path(plan, wave)
    require(source.get('wave') == f'W{wave}', 'source trio belongs to another wave')
    require(isinstance(required_checks, list) and required_checks and
            all(isinstance(name, str) and name.strip() for name in required_checks),
            'binding draft requires the exact required-check policy snapshot')
    return {'plan-id':plan, 'wave':f'W{wave}', 'milestone':source.get('milestone'),
            'issue':source.get('issue'), 'status':'pending-review',
            'merge-sha':pr['merge_commit_sha'], 'delivery-pr':pr['number'],
            'delivery-head-sha':pr['head']['sha'], 'wave-branch':pr['head']['ref'],
            'merged-at':pr['merged_at'], 'branch':branch,
            'required-pr-checks':list(required_checks)}

def binding_branch(plan, wave):
    binding_path(plan, wave)
    require(type(wave) is int and wave >= 0, 'invalid wave index')
    return 'binding/' + plan[:12] + f'-w{wave}'


def binding_staging_path(campaign_root, plan, wave):
    binding_path(plan, wave)
    root = Path(campaign_root)
    require(root.is_dir() and not root.is_symlink(),
            'binding staging requires a real campaign root')
    # Reject a symlink or traversal in any existing parent component. The
    # resolved path is still compared below so a later symlink cannot redirect
    # the durable staging directory.
    current = root
    for part in ('.planning', 'campaigns', plan, f'binding-w{wave}'):
        current = current / part
        require(not current.is_symlink(), 'binding staging path contains a symlink')
    expected = (root.resolve() / '.planning' / 'campaigns' / plan /
                f'binding-w{wave}').resolve()
    return expected


def require_binding_output(output, campaign_root, plan, wave):
    expected = binding_staging_path(campaign_root, plan, wave)
    raw = Path(output)
    require(not raw.is_symlink(), 'binding output path must not be a symlink')
    actual = raw.resolve()
    require(actual == expected,
            'binding output must be the deterministic campaign staging directory %s' % expected)
    return expected


def read_staged_trio(repo, plan, wave, output, campaign_root, expected_branch):
    expected = require_binding_output(output, campaign_root, plan, wave)
    if not expected.exists():
        return {'status': 'awaiting-review', 'output': str(expected),
                'branch': binding_branch(plan, wave)}
    require(expected.is_dir() and not expected.is_symlink(),
            'binding staging output is not a regular directory')
    evidence_path = expected / 'docs/reports/gsd-wave-evidence' / f'{plan}-w{wave}.rktd'
    review_path = expected / 'docs/reports/gsd-wave-reviews' / f'{plan}-w{wave}.rktd'
    validation_path = expected / 'docs/reports/gsd-wave-validation' / f'{plan}-w{wave}.rktd'
    for path in (evidence_path, review_path, validation_path):
        require(path.is_file() and not path.is_symlink(),
                'binding staging trio is incomplete or unsafe: ' + path.name)
    evidence = read_datum(evidence_path)
    review = read_datum(review_path)
    validation = read_datum(validation_path)
    require(isinstance(evidence, dict) and evidence.get('schema-version') == 2,
            'staged binding must contain a schema-2 evidence datum')
    require(evidence.get('plan-id') == plan and evidence.get('wave') == f'W{wave}',
            'staged binding plan/wave identity mismatch')
    require(type(evidence.get('milestone')) is int and evidence['milestone'] > 0,
            'staged binding milestone is invalid')
    require(type(evidence.get('issue')) is int and evidence['issue'] > 0,
            'staged binding issue is invalid')
    merge = evidence.get('merge-sha')
    head = evidence.get('delivery-head-sha')
    require(full_sha(merge) and full_sha(head),
            'staged binding must carry full implementation SHAs')
    require(evidence.get('implementation-sha') == merge,
            'staged binding implementation-sha differs from merge-sha')
    require(evidence.get('merge-method') == 'squash',
            'staged binding must use squash publication')
    require(type(evidence.get('delivery-pr')) is int and evidence['delivery-pr'] > 0,
            'staged binding has no implementation PR identity')
    require(isinstance(evidence.get('merged-at'), str) and evidence['merged-at'],
            'staged binding has no merge provenance')
    branch = binding_branch(plan, wave)
    require(evidence.get('branch') == branch,
            'staged binding branch identity mismatch')
    require(expected_branch is None or evidence.get('wave-branch') == expected_branch,
            'staged binding implementation branch does not match the durable receipt')
    require(isinstance(evidence.get('required-pr-checks'), list) and evidence['required-pr-checks'],
            'staged binding has no required-check policy snapshot')
    expected_review = f'docs/reports/gsd-wave-reviews/{plan}-w{wave}.rktd'
    expected_validation = f'docs/reports/gsd-wave-validation/{plan}-w{wave}.rktd'
    require(evidence.get('review-artifact') == expected_review and
            evidence.get('validation-artifact') == expected_validation,
            'staged binding artifact paths do not match plan/wave identity')
    require(isinstance(review, dict) and isinstance(validation, dict),
            'staged binding review/validation artifacts are malformed')

    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    require(evidence.get('required-pr-checks') == policy_names(repo, main),
            'staged binding required-check snapshot differs from the current policy')
    published = blob_or_none(repo, main, binding_path(plan, wave))
    if published is not None:
        with tempfile.TemporaryDirectory(prefix='q-delivery-rebind-') as temp:
            path = Path(temp) / 'binding.rktd'
            path.write_bytes(published)
            try:
                bound = read_datum(path)
            except Pending as error:
                raise Pending('origin/main binding is malformed; refusing rebind') from error
        require(isinstance(bound, dict) and full_sha(bound.get('merge-sha')),
                'origin/main binding is malformed; refusing rebind')
        require(bound.get('merge-sha') == merge and
                bound.get('delivery-head-sha') == head and
                bound.get('implementation-sha') == merge,
                'origin/main already binds this wave to a different implementation; refusing rebind')

    status = evidence.get('status')
    if status == 'pending-review':
        # The draft is deliberately not a second approval channel. Its honest
        # placeholders are validated as data; publication remains blocked until
        # a genuine independent reviewer finalizes the staged trio.
        require(review.get('verdict') == 'PENDING' and
                review.get('reviewed-sha') == merge and
                review.get('content-digest') == 'PENDING',
                'staged binding review placeholders do not match the pending draft contract')
        require(validation.get('status') == 'pending' and
                validation.get('implementation-sha') == merge and
                validation.get('content-digest') == 'PENDING' and
                validation.get('review-artifact') == expected_review,
                'staged binding validation placeholders do not match the pending draft contract')
        for field in ('red-first', 'focused-tests', 'format-compile', 'lint', 'fast'):
            require(isinstance(validation.get(field), dict),
                    'staged binding validation gate evidence is malformed: ' + field)
        return {'status': 'pending-review', 'output': str(expected),
                'binding': binding_path(plan, wave), 'branch': branch,
                'merge-sha': merge, 'delivery-head-sha': head,
                'implementation-sha': merge}

    require(status == 'ready-for-merge',
            'staged trio status must be pending-review or ready-for-merge')
    require(evidence.get('content-digest') == EMPTY_SHA,
            'finalized binding evidence must use the excluded-diff digest')
    # Register F12: a finalized review asserts an independent reviewer that
    # existed. The draft-stage placeholder contract is honest for drafts, but
    # a finalized record carrying the same sentinels must be refused here as
    # well as in the strict gate.
    reviewer = review.get('reviewer')
    require(isinstance(reviewer, str) and reviewer.strip()
            and not SENTINEL_TEXT.search(reviewer.strip())
            and not SENTINEL_COMPOUND.search(reviewer.strip()),
            'finalized binding review has no genuine reviewer identity')
    require(review.get('verdict') == 'APPROVED' and
            review.get('reviewed-sha') == merge and
            review.get('content-digest') == EMPTY_SHA,
            'finalized binding review is not bound to the publication evidence')
    require(validation.get('status') == 'current' and
            validation.get('implementation-sha') == merge and
            validation.get('content-digest') == EMPTY_SHA and
            validation.get('branch') == branch and
            validation.get('review-artifact') == expected_review and
            validation.get('planning-sync') == 'current',
            'finalized binding validation is not bound to the publication evidence')

    # The publication commit changes only excluded evidence directories, so its
    # changed-content digest is the SHA-256 of an empty diff. Validate the
    # finalized staged trio with the same gate used for implementation evidence.
    with tempfile.TemporaryDirectory(prefix='q-delivery-binding-gate-') as temp:
        gate_root = Path(temp)
        for kind, source in (('evidence', evidence_path),
                             ('reviews', review_path),
                             ('validation', validation_path)):
            target = gate_root / 'docs/reports' / ('gsd-wave-' + kind) / f'{plan}-w{wave}.rktd'
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(source.read_bytes())
        policy = blob_or_none(repo, main, str(POLICY_PATH))
        require(policy is not None, 'binding gate policy is unavailable')
        policy_path = gate_root / 'scripts/required-pr-checks.policy'
        policy_path.parent.mkdir(parents=True, exist_ok=True)
        policy_path.write_bytes(policy)
        gate = command(['racket', str(HERE / 'gsd-wave-gate.rkt'),
                        str(gate_root / 'docs/reports/gsd-wave-evidence' /
                            f'{plan}-w{wave}.rktd'),
                        '--content-digest', EMPTY_SHA,
                        '--root', str(gate_root), '--policy', str(policy_path)],
                       cwd=str(gate_root))
        require('GSD wave evidence PASS' in gate,
                'finalized binding trio failed the strict wave gate')
    return {'status': 'reviewed', 'output': str(expected), 'binding': binding_path(plan, wave),
            'branch': branch, 'merge-sha': merge,
            'delivery-head-sha': head, 'implementation-sha': merge}


def binding_review(repo, plan, wave, output, campaign_root=None, expected_branch=None):
    """Validate a durable staged binding draft without manufacturing review."""
    binding_path(plan, wave)
    root = Path(campaign_root) if campaign_root is not None else Path(repo)
    return read_staged_trio(repo, plan, wave, output, root, expected_branch)


def binding_publish(repo, plan, wave, output, campaign_root=None):
    """Publish a validated binding draft on a fresh-main binding branch."""
    staged = binding_review(repo, plan, wave, output, campaign_root)
    require(staged['status'] == 'reviewed', 'binding staging is not ready to publish')
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    branch = binding_branch(plan, wave)
    remote_ref = 'refs/remotes/origin/' + branch
    refspec = 'refs/heads/' + branch
    # Validate an existing branch before considering it. A branch may be
    # reused only when it is a fresh-main single-parent commit whose three
    # binding artifacts match the validated staged trio semantically (datum
    # equality; the strict digest gate still binds the publication content).
    try:
        remote_tip = git(repo, 'rev-parse', remote_ref).strip()
    except Pending:
        remote_tip = None
    if remote_tip is not None:
        require(full_sha(remote_tip), 'malformed existing binding branch tip')
        parents = git(repo, 'rev-list', '--parents', '-n', '1', remote_tip).split()
        require(len(parents) == 2 and parents[1] == main,
                'existing binding branch is not based on fresh origin/main')
        binding = binding_path(plan, wave)
        with tempfile.TemporaryDirectory(prefix='q-delivery-existing-binding-') as temp:
            for kind in ('evidence', 'reviews', 'validation'):
                artifact = f'{plan}-w{wave}.rktd'
                relative = f'docs/reports/gsd-wave-{kind}/{artifact}'
                blob = blob_or_none(repo, remote_tip, relative)
                require(blob is not None, 'existing binding branch is missing ' + relative)
                path = Path(temp) / (kind + '.rktd')
                path.write_bytes(blob)
                remote_datum = read_datum(path)
                staged_path = output / 'docs/reports' / ('gsd-wave-' + kind) / artifact
                require(remote_datum == read_datum(staged_path),
                        'existing binding branch does not match the staged trio')
        commit = remote_tip
    else:
        with tempfile.TemporaryDirectory(prefix='q-delivery-binding-') as temp:
            worktree = Path(temp)
            try:
                git(repo, 'worktree', 'add', '--detach', str(worktree), main)
                for kind in ('evidence', 'reviews', 'validation'):
                    artifact = f'{plan}-w{wave}.rktd'
                    source = output / 'docs/reports' / ('gsd-wave-' + kind) / artifact
                    require(source.is_file() and not source.is_symlink(),
                            'staged binding trio is not a regular file')
                    (worktree / 'docs/reports' / ('gsd-wave-' + kind)).mkdir(
                        parents=True, exist_ok=True)
                    target = worktree / 'docs/reports' / ('gsd-wave-' + kind) / artifact
                    target.write_bytes(source.read_bytes())
                git(worktree, 'add', '-A')
                git(worktree, 'commit', '--no-gpg-sign', '-m',
                    f'W{wave} binding draft: {plan}-w{wave}')
                commit = git(worktree, 'rev-parse', 'HEAD').strip()
                parents = git(worktree, 'rev-list', '--parents', '-n', '1', commit).split()
                require(len(parents) == 2 and parents[0] == commit and parents[1] == main,
                        'binding publication commit is not based on fresh origin/main')
            finally:
                try:
                    git(repo, 'worktree', 'remove', '--force', str(worktree))
                except Pending:
                    pass
    git(repo, 'push', '--no-tags', 'origin', f'{commit}:refs/heads/{branch}')
    # Re-read the remote branch after the non-force push. This also proves the
    # branch was created or remained at the exact commit being published.
    git(repo, 'fetch', '--no-tags', 'origin', f'{refspec}:{remote_ref}')
    tip = git(repo, 'rev-parse', remote_ref).strip()
    require(tip == commit, 'binding branch did not publish at the verified commit')

    existing = resolve_existing_pr(slug, branch)
    if existing is None:
        merged = resolve_merged_pr(slug, branch)
        if merged is not None:
            return {'status': 'already-published', 'pr': merged.get('number'),
                    'branch': branch, 'head': commit}
        title = f'W{wave} binding publication: {branch}'
        body = ('Plan: %s\nWave: W%d\nHead: %s\nBranch: %s\n'
                'Binding evidence: %s\n'
                'Review and approval: genuine independent human review required at the exact binding head.\n') % (
                    plan, wave, commit, branch, binding_path(plan, wave))
        created = gh_post(slug, 'pulls',
                          {'title': title, 'body': body, 'head': branch, 'base': 'main'})
        require(isinstance(created, dict) and type(created.get('number')) is int and
                created['number'] > 0, 'GitHub pull-request creation response has no usable number')
        number = created['number']
        pr = api(slug, f'pulls/{number}', _refresh=True)
        status = 'opened'
    else:
        number = existing.get('number')
        require(type(number) is int and number > 0,
                'resolved binding pull request has no usable number')
        pr = api(slug, f'pulls/{number}', _refresh=True)
        status = 'exists'
    require(isinstance(pr, dict) and pr.get('state') == 'open',
            'binding pull request is not open')
    validate_pr_identity(pr, slug)
    require(dig(pr, 'head', 'ref') == branch and dig(pr, 'head', 'sha') == commit,
            'binding pull request does not match the verified publication branch/head')
    return {'status': status, 'pr': number, 'branch': branch, 'head': commit}


def binding_resolve_pr(repo, expected_branch, plan=None, wave=None):
    """Resolve a binding PR by its deterministic branch without receipt ancestry.

    Binding publication branches are fresh-main publication commits, so the
    implementation receipt's excluded-diff ancestry rule must not be applied.
    The exact fetched branch tip and same-repository PR identity remain strict.
    Merged-PR recovery resolves before any branch fetch so a deleted head
    branch (common post-merge hygiene) cannot block idempotent resume after a
    lost merge response; a missing branch before first publication yields the
    typed none result instead of an opaque fetch failure.
    """
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'binding PR resolution requires --expected-branch')
    slug = repository(repo)
    existing = resolve_existing_pr(slug, expected_branch)
    refresh(repo)
    refspec = 'refs/heads/' + expected_branch
    remote_ref = 'refs/remotes/origin/' + expected_branch
    if existing is not None:
        git(repo, 'fetch', '--no-tags', 'origin', '+' + refspec + ':' + remote_ref)
        tip = git(repo, 'rev-parse', remote_ref).strip()
        require(full_sha(tip), 'malformed fetched binding branch tip')
        number = existing.get('number')
        require(type(number) is int and number > 0,
                'resolved binding pull request has no usable number')
        pr = api(slug, f'pulls/{number}', _refresh=True)
        require(isinstance(pr, dict) and pr.get('state') == 'open',
                'resolved binding pull request is not open')
        validate_pr_identity(pr, slug)
        head = dig(pr, 'head', 'sha')
        require(isinstance(head, str) and full_sha(head) and head == tip,
                'binding pull request head does not match the fetched branch tip')
        require(dig(pr, 'head', 'ref') == expected_branch,
                'binding pull request branch does not match expected branch')
        return {'status': 'resolved', 'pr': number, 'branch': expected_branch,
                'head': head, 'plan-id': plan, 'wave': wave}
    merged = resolve_merged_pr(slug, expected_branch)
    if merged is not None:
        number = merged.get('number')
        require(type(number) is int and number > 0,
                'resolved merged binding pull request has no usable number')
        pr = api(slug, f'pulls/{number}', _refresh=True)
        require(pr_is_merged(pr), 'resolved binding pull request is not merged')
        validate_pr_identity(pr, slug)
        head = dig(pr, 'head', 'sha')
        require(isinstance(head, str) and full_sha(head),
                'resolved merged binding pull request has no full head SHA')
        require(dig(pr, 'head', 'ref') == expected_branch,
                'resolved merged binding pull request branch does not match expected branch')
        fetched = fetch_head(repo, number)
        require(fetched == head, 'merged binding pull request head does not match fetched ref')
        # Defense-in-depth: when the deterministic branch still exists, its
        # tip must be the merged PR head — a recreated branch must never
        # inherit a stale merged PR identity. A deleted branch must not block
        # resume, so a failed fetch here is tolerated; the refs/pull/N/head
        # equality above stays authoritative either way. The tip equality
        # itself is enforced OUTSIDE the fetch-failure tolerance: a divergent
        # recreated branch must fail closed, not be swallowed as a deletion.
        branch_tip = None
        try:
            git(repo, 'fetch', '--no-tags', 'origin', '+' + refspec + ':' + remote_ref)
            branch_tip = git(repo, 'rev-parse', remote_ref).strip()
        except Pending:
            branch_tip = None
        if branch_tip is not None:
            require(head == branch_tip,
                    'resolved merged binding pull request head does not match fetched branch tip')
        return {'status': 'already-merged', 'pr': number, 'branch': expected_branch,
                'head': head, 'merge-sha': dig(pr, 'merge_commit_sha'),
                'plan-id': plan, 'wave': wave}
    return {'status': 'none', 'branch': expected_branch}


def binding_ci(repo, number, expected_branch, expected_head):
    """Check a binding PR at its exact head without implementation ancestry rules."""
    require(type(number) is int and number > 0, 'binding-ci requires --pr')
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'binding-ci requires --expected-branch')
    require(isinstance(expected_head, str) and full_sha(expected_head),
            'binding-ci requires --expected-head')
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    pr = api(slug, f'pulls/{number}', _refresh=True)
    require(isinstance(pr, dict) and pr.get('state') == 'open',
            'binding pull request is not open')
    validate_pr_identity(pr, slug)
    head = dig(pr, 'head', 'sha')
    require(isinstance(head, str) and full_sha(head) and head == expected_head,
            'binding pull request head does not match the verified expected head')
    require(dig(pr, 'head', 'ref') == expected_branch,
            'binding pull request branch does not match expected branch')
    fetched = fetch_head(repo, number)
    require(fetched == head, 'binding pull-request head does not match fetched ref')
    names = policy_names(repo, main)
    protection(slug, names)
    for name in names:
        trusted_check(slug, head, name, 'pull_request', expected_branch)
    return {'status': 'green', 'pr': number, 'branch': expected_branch, 'head': head}


def binding_merge(repo, plan, wave, number, expected_head, expected_branch, source):
    """Run the unchanged protected merge gate for finalized binding evidence."""
    return merge(repo, plan, wave, number, expected_head, expected_branch, source)


# Explicit action names used by the coordinator's stage-to-action map.
binding_pr = binding_publish


def prepare(repo, plan, wave, number, relative, campaign_root, output, expected_branch=None):
    binding = binding_path(plan, wave)
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    if number is None:
        require(isinstance(expected_branch, str) and expected_branch.strip(),
                'prepare --pr requires --expected-branch for durable PR self-resolution')
        pr = resolve_merged_pr(slug, expected_branch)
        require(isinstance(pr, dict),
                'prepare cannot self-resolve exactly one merged implementation PR for branch ' + expected_branch)
        number = pr.get('number')
    else:
        pr = api(slug, f'pulls/{number}')
    require(isinstance(pr, dict), 'malformed PR response')
    validate_pr_identity(pr, slug)
    merge, head = dig(pr, 'merge_commit_sha'), dig(pr, 'head', 'sha')
    require(pr_is_merged(pr), 'implementation PR must be independently reviewed and merged first')
    if number is None:
        require(pr.get('state') == 'closed', 'self-resolved implementation PR is not closed')
    validate_merge(pr, api(slug, f'commits/{merge}'), slug, merge, head)
    fetched = fetch_head(repo, number)
    require(fetched == head, 'fetched implementation head does not match PR head SHA')
    require(tree_of(repo, merge) == tree_of(repo, fetched),
            'implementation merge tree differs from fetched PR head tree')
    # Register F4: the wave's evidence record must be authored in an
    # evidence-only commit on the wave branch (the receipt head's history,
    # not the squash merge). The shared Racket tool inspects the newest
    # base..head commit touching the evidence directory and prints
    # `impure-record-commit: <paths>` for a mixed record commit; any tool
    # failure raises Pending, so the ladder fails closed before any stage.
    purity = command(['racket', str(HERE / 'gsd-evidence-bind.rkt'),
                      'record-commit', '--repo', str(repo),
                      '--base', dig(pr, 'base', 'sha'), '--head', head],
                     cwd=str(repo), timeout=60)
    # The tool's decided verdict already carries the typed refusal code.
    require(purity.startswith('pure'), purity.strip())
    names = policy_names(repo, main)
    protection(slug, names)
    git(repo, 'merge-base', '--is-ancestor', merge, 'refs/remotes/origin/main')
    # Frozen provenance: the immutable snapshot must verify for this plan and
    # wave, and the source trio must be exactly the wave's declared output.
    facts = snapshot_facts(campaign_root, plan, wave)
    declared = declared_outputs(facts, repo)
    artifact_path(relative, 'gsd-wave-evidence')
    if declared is not None:
        require(relative == declared['evidence'],
                'source evidence is not the frozen wave output for this plan/wave')
    else:
        require(relative == binding,
                'wave declares no outputs; only the exact campaign-hash source is accepted')
    with tempfile.TemporaryDirectory(prefix='q-delivery-source-') as temp:
        source = read_datum(materialize(repo, head, relative, Path(temp)))
    require(isinstance(source, dict) and source.get('schema-version') == 2,
            'source must be a schema-2 hash trio')
    require(source.get('wave') == f'W{wave}', 'source trio belongs to another wave')
    if source.get('plan-id') is not None:
        require(source.get('plan-id') == plan,
                'source trio explicitly names another campaign; refusing')
    if declared is not None:
        require(source.get('review-artifact') == declared['review'],
                'source review artifact is not the frozen wave output')
        require(source.get('validation-artifact') == declared['validation'],
                'source validation artifact is not the frozen wave output')
    if facts.get('issue'):
        require(source.get('issue') == facts['issue'],
                'source issue differs from frozen plan metadata')
    if facts.get('milestone'):
        require(source.get('milestone') == facts['milestone'],
                'source milestone differs from frozen plan metadata')
    # Rebinding: an already PUBLISHED binding may only be redrafted for the
    # identical verified implementation; a different one is refused outright.
    # A pre-binding trio (e.g. the hash-named source itself, which carries no
    # merge identity) is not a publication and is superseded by the reviewed
    # binding PR; the unchanged strict gate still guards every publication.
    existing = blob_or_none(repo, 'refs/remotes/origin/main', binding)
    if existing is not None:
        with tempfile.TemporaryDirectory(prefix='q-delivery-rebind-') as temp:
            path = Path(temp) / 'binding.rktd'
            path.write_bytes(existing)
            try:
                bound = read_datum(path)
            except Pending:
                bound = None
        published = (isinstance(bound, dict) and
                     all(bound.get(key) is not None for key in
                         ('merge-sha', 'delivery-head-sha', 'delivery-pr',
                          'implementation-sha')))
        require(not published or
                (bound.get('merge-sha') == merge and
                 bound.get('delivery-head-sha') == head and
                 bound.get('implementation-sha') == bound.get('merge-sha')),
                'origin/main already binds this wave to a different implementation; '
                'refusing rebind')
    validate_trio(repo, head, relative, dig(pr, 'base', 'sha'))
    request = draft_request(source, plan, wave, pr, 'binding/' + plan[:12] + f'-w{wave}', names)
    output = Path(output).resolve()
    require(not output.exists(), 'output directory already exists; refusing to overwrite')
    output.mkdir(parents=True)
    payload = output / 'request.json'
    payload.write_text(json.dumps(request))
    command(['racket', str(HERE / 'gsd-binding-data.rkt'), 'prepare', str(payload), str(output)],
            timeout=60)
    payload.unlink()
    return {'status':'pending-review', 'output':str(output),
            'binding':binding, 'source':relative,
            'next':'Copy trio into a dedicated fresh-main worktree; complete genuine binding review and gates; '
                   'publish through protected squash PR; wait for main governance; sync and resume /go.'}

def scratch_exempt_porcelain(repo):
    """sandbox-write-scratch-parity W1: UNTRACKED files under
    .planning/scratch/ are disposable executor probes (created by the
    structured-write tool for diagnostic scripts) and must not block delivery
    synchronization. Only bare-untracked ('??') lines are exempt: a COMMITTED
    scratch file's local modification or deletion is real working-tree dirt
    and still refuses, as does any edit outside the scratch root. Handles
    porcelain v1 quoting (core.quotePath wraps non-ASCII paths in double
    quotes) and rename pairs `R  old -> new`."""
    def exempt(path):
        path = path.strip().strip('"')
        return path.startswith('.planning/scratch/')
    kept = []
    # -uall expands collapsed untracked directories: a fresh `.planning/`
    # would otherwise report as one `?? .planning/` line and hide whether the
    # dirt is scratch-only. Tracked-file modifications are unaffected.
    for line in git(repo, 'status', '--porcelain', '--untracked-files=all').splitlines():
        if not line.strip():
            continue
        body = line[3:] if len(line) > 3 else ''
        code = line[:2]
        if code == '??':
            # An untracked path is one path even when its literal name contains
            # " -> "; splitting it would let a foreign path masquerade as scratch.
            if exempt(body):
                continue
        elif code[0] in ('R', 'C') and ' -> ' in body:
            old, new = body.split(' -> ', 1)
            if exempt(old) and exempt(new):
                continue
        kept.append(line)
    return '\n'.join(kept)

def sync(repo, expected_branch):
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'sync requires --expected-branch')
    try:
        current = git(repo, 'symbolic-ref', '--quiet', '--short', 'HEAD').strip()
    except Pending:
        raise Pending('detached HEAD; refusing synchronization')
    require(current == expected_branch,
            'synchronization refused: HEAD is %r, expected %r (unrelated branch changes not accepted)'
            % (current, expected_branch))
    require(not scratch_exempt_porcelain(repo).strip(), 'dirty checkout; refusing synchronization')
    refresh(repo)
    git(repo, 'merge', '--ff-only', 'origin/main')
    return {'status':'synchronized', 'head':git(repo, 'rev-parse', 'HEAD').strip(),
            'branch':current}


def resolve_pr(repo, plan, wave, branch, expected_head=None):
    """Resolve the exact implementation PR for a durable head branch.

    Open PRs are resolved for normal progression. When a durable receipt head
    is supplied and no open PR exists, one closed merged PR is also accepted so
    a lost merge response can resume idempotently; zero or multiple candidates
    still fail closed. The returned PR number feeds the protected merge; model
    or journal completion never substitutes for it.

    When a durable receipt head is supplied, fetch the PR ref and require the
    actual PR tip to descend from that receipt through excluded-evidence-only
    commits. This preserves the review-stage tip semantics while keeping
    merge's exact-head contract."""
    slug = repository(repo)
    refresh(repo)
    pr = resolve_existing_pr(slug, branch)
    merged_resume = False
    if pr is None and expected_head is not None:
        # A squash merge may have completed while the response was lost. The
        # closed PR remains the durable identity; re-resolve it and let merge()
        # perform its normal already-merged proof rather than opening a new PR.
        pr = resolve_merged_pr(slug, branch)
        merged_resume = pr is not None
    if pr is None:
        return {'status': 'none', 'plan-id': plan, 'wave': wave, 'branch': branch}
    number = pr.get('number') if isinstance(pr, dict) else None
    require(type(number) is int and number > 0,
            'resolved pull request has no usable number')
    head = dig(pr, 'head', 'sha') if isinstance(pr, dict) else None
    if expected_head is not None:
        require(isinstance(pr, dict), 'malformed pull-request response')
        require(pr.get('state') == ('closed' if merged_resume else 'open'),
                'resolved pull request has an unexpected state')
        require(isinstance(head, str) and full_sha(head),
                'resolved pull request has no full head SHA')
        require(dig(pr, 'head', 'ref') == branch,
                'resolved pull request branch does not match expected branch')
        validate_pr_identity(pr, slug)
        fetched = fetch_head(repo, number)
        require(fetched == head, 'resolved pull-request head does not match fetched ref')
        require_receipt_tip(repo, expected_head, head, 'merge')
    return {'status': 'resolved', 'pr': number, 'plan-id': plan, 'wave': wave,
            'branch': branch, 'head': head}


def require_receipt_tip(repo, expected_head, tip, action='delivery'):
    """Pin a branch/PR tip to the durable receipt identity when supplied.

    The implementation-review stage permits only excluded evidence commits
    after the receipt head. Carry that same ancestry/drift rule into PR and CI
    readback so a later source rewrite cannot silently advance the wave."""
    if expected_head is None:
        return
    require(isinstance(expected_head, str) and full_sha(expected_head),
            'expected-head must be a full SHA')
    common = git(repo, 'merge-base', expected_head, tip).strip()
    require(common == expected_head,
            'receipt head %s is not an ancestor of tip %s; the wave branch may '
            'have been rewritten; re-verify before %s'
            % (expected_head[:12], tip[:12], action))
    drifted = git(repo, 'diff', '--name-only', '--no-renames',
                  expected_head + '..' + tip, '--')
    foreign = [line.strip() for line in drifted.splitlines()
               if line.strip() and not line.strip().startswith(EXCLUDED_PREFIXES)]
    require(not foreign,
            'receipt head %s is verified but later wave-branch commits touch '
            'non-evidence paths: %s; re-verify before %s'
            % (expected_head[:12], ', '.join(foreign[:3]), action))


def open_pr(repo, plan, wave, expected_branch, expected_head=None):
    """Resolve-before-create an implementation PR from the fetched branch tip.

    The branch name is durable input, but the PR head is read from the remote
    branch after a refresh; the current checkout is never used as the head.
    Creation is deterministic, and the returned identity is re-read before it
    is exposed to the coordinator."""
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'open-pr requires --expected-branch')
    slug = repository(repo)
    existing = resolve_existing_pr(slug, expected_branch)
    refresh(repo)
    refspec = 'refs/heads/' + expected_branch
    remote_ref = 'refs/remotes/origin/' + expected_branch
    # Allow a rewritten wave branch to refresh the remote-tracking ref. The
    # subsequently re-read PR identity still pins the exact fetched tip.
    git(repo, 'fetch', '--no-tags', 'origin', '+' + refspec + ':' + remote_ref)
    tip = git(repo, 'rev-parse', remote_ref).strip()
    require(full_sha(tip), 'malformed fetched implementation branch tip')
    require_receipt_tip(repo, expected_head, tip)

    if existing is None:
        title = 'W%d delivery: %s' % (wave, expected_branch)
        evidence = binding_path(plan, wave)
        body = ('Plan: %s\nWave: W%d\nHead: %s\nBranch: %s\n'
                'Binding evidence: %s\n'
                'Review: genuine independent review required at the exact implementation head.\n') % (
                    plan, wave, tip, expected_branch, evidence)
        created = gh_post(slug, 'pulls',
                          {'title': title, 'body': body, 'head': expected_branch, 'base': 'main'})
        require(isinstance(created, dict), 'malformed GitHub pull-request creation response')
        created_number = created.get('number')
        require(type(created_number) is int and created_number > 0,
                'GitHub pull-request creation response has no usable number')
        pr = api(slug, 'pulls/%s' % created_number, _refresh=True)
    else:
        existing_number = existing.get('number')
        require(type(existing_number) is int and existing_number > 0,
                'resolved pull request has no usable number')
        pr = api(slug, 'pulls/%s' % existing_number, _refresh=True)

    require(isinstance(pr, dict), 'malformed pull-request response')
    require(pr.get('state') == 'open', 'pull request is not open')
    validate_pr_identity(pr, slug)
    number = pr.get('number')
    head = dig(pr, 'head', 'sha')
    require(type(number) is int and number > 0, 'created pull request has no usable number')
    require(full_sha(head), 'created pull request has no full head SHA')
    require(head == tip, 'fetched implementation branch tip differs from pull-request head')
    require(dig(pr, 'head', 'ref') == expected_branch,
            'pull request branch does not match expected branch')
    return {'status': 'opened', 'pr': number, 'branch': expected_branch, 'head': head}


def pr_ci(repo, number, expected_branch, expected_head=None):
    """Evaluate the exact PR head against the current required-check policy.

    Every required check must be completed successfully, trusted to the
    Actions app, and tied to the same pull-request event and branch. Any
    pending, failed, missing, or untrusted check is a typed stop naming that
    check."""
    require(type(number) is int and number > 0, 'pr-ci requires --pr')
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'pr-ci requires --expected-branch')
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    pr = api(slug, 'pulls/%s' % number, _refresh=True)
    require(isinstance(pr, dict), 'malformed pull-request response')
    require(pr.get('state') == 'open', 'pull request is not open')
    validate_pr_identity(pr, slug)
    head = dig(pr, 'head', 'sha')
    require(full_sha(head), 'pull request has no full head SHA')
    require(dig(pr, 'head', 'ref') == expected_branch,
            'pull request branch does not match expected branch')
    fetched = fetch_head(repo, number)
    require(fetched == head, 'pull-request head does not match fetched ref')
    require_receipt_tip(repo, expected_head, head)
    names = policy_names(repo, main)
    protection(slug, names)
    for name in names:
        trusted_check(slug, head, name, 'pull_request', expected_branch)
    return {'status': 'green', 'pr': number, 'branch': expected_branch, 'head': head}


def review(repo, plan, wave, expected_head, expected_branch):
    """implementation-review: validate the durable independent review at the
    receipt identity. Pure git-object readback plus the unchanged strict
    gate — no PR, no network mutation, and deliberately no checkout HEAD
    requirement (the head comes from the durable receipt, never from a
    working tree that may have moved).

    The wave branch TIP is validated when the receipt head is its ancestor:
    the receipt freezes the verified implementation head, and the evidence
    trio + review are produced AFTER Verify by a genuine independent
    reviewer. Every commit between the receipt head and the tip must touch
    only excluded evidence directories — any source drift after the receipt
    refuses. The reviewed-sha receipt-binding clause then refuses a review
    that never covered the implementation."""
    require(isinstance(expected_head, str) and full_sha(expected_head),
            'review requires --expected-head (the durable receipt head)')
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'review requires --expected-branch (the durable receipt branch)')
    relative = binding_path(plan, wave)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    # Materialize the receipt head's objects: fetch the receipt branch tip and
    # require the receipt head to be on it (a rewritten/force-pushed branch
    # that no longer carries the verified head refuses here).
    git(repo, 'fetch', '--no-tags', 'origin', 'refs/heads/' + expected_branch)
    tip = git(repo, 'rev-parse', 'FETCH_HEAD').strip()
    require(full_sha(tip), 'malformed fetched receipt branch tip')
    require_receipt_tip(repo, expected_head, tip, 'review')
    # Validation reference: the fetched tip (the trio and review are produced
    # after Verify); the excluded-diff digest binds it to the receipt's tree.
    if blob_or_none(repo, tip, relative) is None:
        raise Pending('binding trio %s is absent on branch %s (tip %s); a genuine '
                      'independent reviewer must commit it after Verify'
                      % (relative, expected_branch, tip[:12]))
    with tempfile.TemporaryDirectory(prefix='q-delivery-review-') as temp:
        evidence = read_datum(materialize(repo, tip, relative, Path(temp)))
        require(isinstance(evidence, dict) and evidence.get('schema-version') == 2,
                'review requires a schema-2 evidence record at the branch tip')
        require(evidence.get('wave') == 'W%d' % wave,
                'evidence record belongs to another wave (expected W%d)' % wave)
        declared_plan = evidence.get('plan-id')
        require(declared_plan is None or declared_plan == plan,
                'evidence record explicitly names another campaign')
        review_rel = artifact_path(evidence.get('review-artifact'), 'gsd-wave-reviews')
    if blob_or_none(repo, tip, review_rel) is None:
        return {'status': 'awaiting-review', 'plan-id': plan, 'wave': wave,
                'head': expected_head, 'review-artifact': review_rel,
                'reason': 'review artifact absent at the receipt identity; a genuine '
                          'independent non-author reviewer must produce it'}
    validate_trio(repo, tip, relative, main)
    with tempfile.TemporaryDirectory(prefix='q-delivery-review-bind-') as temp:
        review_datum = read_datum(materialize(repo, tip, review_rel, Path(temp)))
    reviewed = review_datum.get('reviewed-sha') if isinstance(review_datum, dict) else None
    require(isinstance(reviewed, str) and full_sha(reviewed),
            'review record carries no reviewed-sha')
    if reviewed != expected_head:
        require_receipt_tip(repo, reviewed, expected_head, 'review')
    return {'status': 'reviewed', 'plan-id': plan, 'wave': wave, 'head': expected_head,
            'reviewed-sha': reviewed, 'review-artifact': review_rel}

def governance(repo, plan, wave, expected_branch):
    """Validate the merged binding publication and its protected-main governance run."""
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'governance requires --expected-branch')
    require(expected_branch == binding_branch(plan, wave),
            'governance branch does not match the deterministic binding branch')
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    pr = resolve_merged_pr(slug, expected_branch)
    require(isinstance(pr, dict),
            'governance cannot self-resolve exactly one merged binding PR for branch ' + expected_branch)
    number = pr.get('number')
    merge = dig(pr, 'merge_commit_sha')
    head = dig(pr, 'head', 'sha')
    require(type(number) is int and number > 0 and full_sha(merge) and full_sha(head),
            'merged binding PR has incomplete identity')
    require(dig(pr, 'head', 'ref') == expected_branch,
            'merged binding PR branch does not match expected branch')
    validate_merge(pr, api(slug, f'commits/{merge}'), slug, merge, head)
    git(repo, 'merge-base', '--is-ancestor', merge, 'refs/remotes/origin/main')
    trusted_check(slug, merge, GOVERNANCE_CHECK, 'push', 'main')
    return {'status': 'governed', 'pr': number, 'branch': expected_branch,
            'publication-sha': merge}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=['status','prepare','sync','merge','resolve-pr','review',
                                           'open-pr','pr-ci','binding-review','binding-publish',
                                           'binding-resolve-pr','binding-pr','binding-ci','binding-merge',
                                           'governance','preflight'])
    parser.add_argument('--repo', type=Path, required=True)
    parser.add_argument('--plan')
    parser.add_argument('--wave', type=int)
    parser.add_argument('--pr', type=int)
    parser.add_argument('--evidence')
    parser.add_argument('--expected-branch',
                        help='PR head branch / sync branch; never switches branches')
    parser.add_argument('--expected-head',
                        help='exact verified implementation head (from the durable receipt)')
    parser.add_argument('--output', type=Path)
    parser.add_argument('--campaign-root', type=Path,
                        help='campaign working tree holding .planning/campaigns/<plan-id>/plan-snapshot')
    args = parser.parse_args()
    try:
        if args.action == 'sync':
            result = sync(args.repo, args.expected_branch)
        elif args.action == 'status':
            result = status(args.repo, args.plan, args.wave)
        elif args.action == 'merge':
            require(args.pr and args.expected_head and args.expected_branch and args.evidence,
                    'merge requires --pr, --expected-head, --expected-branch and --evidence')
            result = merge(args.repo, args.plan, args.wave, args.pr, args.expected_head,
                           args.expected_branch, args.evidence)
        elif args.action == 'resolve-pr':
            require(args.expected_branch,
                    'resolve-pr requires --expected-branch')
            result = resolve_pr(args.repo, args.plan, args.wave, args.expected_branch,
                                args.expected_head)
        elif args.action == 'review':
            require(args.expected_head and args.expected_branch,
                    'review requires --expected-head and --expected-branch')
            result = review(args.repo, args.plan, args.wave, args.expected_head,
                            args.expected_branch)
        elif args.action == 'open-pr':
            require(args.expected_branch,
                    'open-pr requires --expected-branch')
            result = open_pr(args.repo, args.plan, args.wave, args.expected_branch,
                             args.expected_head)
        elif args.action == 'pr-ci':
            require(args.pr and args.expected_branch,
                    'pr-ci requires --pr and --expected-branch')
            result = pr_ci(args.repo, args.pr, args.expected_branch, args.expected_head)
        elif args.action == 'binding-review':
            require(args.output and args.campaign_root,
                    'binding-review requires --output and --campaign-root')
            result = binding_review(args.repo, args.plan, args.wave, args.output,
                                    args.campaign_root, args.expected_branch)
        elif args.action == 'binding-publish':
            require(args.output and args.campaign_root,
                    'binding-publish requires --output and --campaign-root')
            result = binding_publish(args.repo, args.plan, args.wave, args.output,
                                     args.campaign_root)
        elif args.action == 'binding-pr':
            require(args.output and args.campaign_root,
                    'binding-pr requires --output and --campaign-root')
            result = binding_pr(args.repo, args.plan, args.wave, args.output,
                                args.campaign_root)
        elif args.action == 'binding-resolve-pr':
            require(args.expected_branch,
                    'binding-resolve-pr requires --expected-branch')
            result = binding_resolve_pr(args.repo, args.expected_branch,
                                          args.plan, args.wave)
        elif args.action == 'binding-ci':
            require(args.pr and args.expected_branch and args.expected_head,
                    'binding-ci requires --pr, --expected-branch and --expected-head')
            result = binding_ci(args.repo, args.pr, args.expected_branch, args.expected_head)
        elif args.action == 'binding-merge':
            require(args.pr and args.expected_branch and args.expected_head and args.evidence,
                    'binding-merge requires --pr, --expected-branch, --expected-head and --evidence')
            result = binding_merge(args.repo, args.plan, args.wave, args.pr,
                                   args.expected_head, args.expected_branch, args.evidence)
        elif args.action == 'preflight':
            require(args.expected_head and args.expected_branch,
                    'preflight requires --expected-head and --expected-branch')
            result = preflight(args.repo, args.plan, args.wave,
                               args.expected_head, args.expected_branch)
        elif args.action == 'governance':
            require(args.expected_branch, 'governance requires --expected-branch')
            result = governance(args.repo, args.plan, args.wave, args.expected_branch)
        else:
            require(args.evidence and args.output and args.campaign_root,
                    'prepare requires --evidence, --output and --campaign-root')
            if args.pr is None:
                require(args.expected_branch,
                        'prepare without --pr requires --expected-branch')
            result = prepare(args.repo, args.plan, args.wave, args.pr, args.evidence,
                             args.campaign_root, args.output, args.expected_branch)
        print(json.dumps(result))
        return 0
    except (Pending, ValueError, KeyError, TypeError, OSError, AttributeError) as error:
        print(json.dumps({'status':'delivery-pending', 'reason':str(error)}))
        return 2

if __name__ == '__main__':
    sys.exit(main())
