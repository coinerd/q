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

def binding_path(plan, wave, generation=0):
    require(isinstance(plan, str) and re.fullmatch('[0-9a-f]{64}', plan), 'invalid campaign id')
    require(type(wave) is int and wave >= 0, 'invalid wave index')
    require(type(generation) is int and generation >= 0, 'invalid binding generation')
    suffix = '' if generation == 0 else f'-r{generation}'
    return f'docs/reports/gsd-wave-evidence/{plan}-w{wave}{suffix}.rktd'


def campaign_journal_root(repo, campaign_root=None):
    if campaign_root is not None:
        root = Path(campaign_root).absolute()
        require(not any(p.is_symlink() for p in (root, *root.parents)),
                'symlinked campaign root refused')
        return root.resolve()
    repo = Path(repo).resolve()
    if repo.name == 'q' and (repo.parent / '.planning').is_dir():
        require(not (repo / '.planning').exists(),
                'ambiguous campaign roots; explicit --campaign-root required')
        return repo.parent
    return repo

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

def validate_trio(repo, ref, relative, base, owned_proof_authority=None):
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
        if owned_proof_authority is not None:
            bundle = owned_proof_authority['bundle']
            refs = [owned_proof_authority['reference'],
                    owned_proof_authority['log-reference']]
            if (isinstance(owned_proof_authority.get('red'), dict) and
                    owned_proof_authority['red'].get('relation') != 'baseline-with-test-overlay-v1'):
                refs += [owned_proof_authority['red']['reference'],
                         owned_proof_authority['red']['log-reference']]
            portable = [bundle + '/' + entry['path'] for entry in refs]
            portable += owned_proof_authority.get('publication-files', [])
            for path in dict.fromkeys(portable):
                parsed = PurePosixPath(path)
                require(not parsed.is_absolute() and '..' not in parsed.parts and
                        path.startswith(bundle + '/'), 'unsafe portable proof path')
                materialize(repo, ref, path, root)
            _owned_publication_files(root, owned_proof_authority)
        actual = digest(repo, base, ref)
        gate = ['racket', str(HERE / 'gsd-wave-gate.rkt'), str(root / relative),
                '--content-digest', actual, '--root', str(root), '--policy',
                str(root / 'scripts/required-pr-checks.policy')]
        if owned_proof_authority is not None:
            payload = root / 'owned-proof-authority.json'
            payload.write_text(json.dumps(owned_proof_authority), encoding='utf-8')
            gate += ['--owned-proof-authority', str(payload),
                     '--owned-source-repo', str(Path(repo).resolve())]
        command(gate, timeout=60)
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


def resolve_merged_pr(slug, branch, expected_head=None):
    """Find one closed, merged PR for a branch for idempotent resume.

    This is deliberately separate from open-PR resolution: a lost merge response
    must not make the next delivery attempt invent a new PR or stall forever."""
    require(expected_head is None or full_sha(expected_head),
            'merged resolution requires a full expected-head SHA')
    candidates = api(slug, f'pulls?state=all&head={branch_owner(slug)}:{branch}')
    require(isinstance(candidates, list), 'malformed pull-request response')
    merged = [pr for pr in candidates
              if isinstance(pr, dict) and pr.get('state') == 'closed'
              and pr_is_merged(pr) and dig(pr, 'head', 'ref') == branch]
    if expected_head is not None:
        exact = [pr for pr in merged if dig(pr, 'head', 'sha') == expected_head]
        if exact:
            merged = exact
        # No exact candidate retains the previous fail-closed uniqueness
        # and receipt ancestry/excluded-drift checks; never guess among
        # historical PRs or weaken the single-PR resume contract.
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

def review_tracker_facts(repo, campaign_root, plan, wave):
    """Resolve only a missing milestone from the exact verified frozen issue.

    Fully numeric snapshot metadata keeps its existing behavior. Legacy facts
    need the unique verified PLAN title and an authenticated same-repo issue;
    prose numbers, mutable plans and global tracker defaults are not inputs.
    """
    facts = snapshot_facts(campaign_root, plan, wave)
    issue = facts.get('issue')
    require(type(issue) is int and issue > 0,
            'the frozen wave facts carry no positive issue for the review trio')
    if 'milestone' in facts:
        milestone = facts['milestone']
        require(type(milestone) is int and milestone > 0,
                'the frozen wave facts carry no positive milestone for the review trio')
        return facts
    title = facts.get('frozen-plan-title')
    require(isinstance(title, str) and title and title == title.strip(),
            'missing unique verified frozen PLAN title for milestone resolution')
    slug = repository(repo)
    data = api(slug, f'issues/{issue}')
    require(isinstance(data, dict) and 'pull_request' not in data and
            type(data.get('number')) is int and data['number'] == issue and
            data.get('html_url') == f'https://github.com/{slug}/issues/{issue}',
            'authenticated tracker response is not the exact frozen same-repo issue')
    milestone = data.get('milestone')
    require(isinstance(milestone, dict) and
            type(milestone.get('number')) is int and milestone['number'] > 0,
            'the authenticated frozen issue has no positive milestone')
    number = milestone['number']
    require(milestone.get('url') == f'https://api.github.com/repos/{slug}/milestones/{number}' and
            milestone.get('title') == title,
            'authenticated milestone repository/number/title differs from the verified frozen PLAN')
    return dict(facts, milestone=number)


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


def repair_generation_path(relative, generation):
    require(type(generation) is int and generation >= 0, 'invalid binding generation')
    if generation == 0:
        return relative
    path = PurePosixPath(relative)
    require(path.suffix == '.rktd' and path.name.endswith('.rktd'),
            'multiple/conflicting declared output path')
    return str(path.with_name(path.name[:-5] + f'-r{generation}.rktd'))


def active_declared_outputs(facts, repo, plan, wave, generation):
    declared = declared_outputs(facts, repo)
    if declared is None:
        # Zero-declared waves keep ONE deterministic path family (the
        # hash-named trio); a repair generation derives it by the same -rN
        # rule used for declared outputs. Deterministic, never ambiguous,
        # and gen0 paths are never reused for an active repair generation.
        suffix = '' if generation == 0 else f'-r{generation}'
        return {'evidence': binding_path(plan, wave, generation),
                'review': f'docs/reports/gsd-wave-reviews/{plan}-w{wave}{suffix}.rktd',
                'validation': f'docs/reports/gsd-wave-validation/{plan}-w{wave}{suffix}.rktd'}
    if generation == 0:
        return dict(declared)
    return {kind: repair_generation_path(path, generation)
            for kind, path in declared.items()}

def publish_head(repo, expected_branch, expected_head, expected_origin=None, expected_tree=None,
                 plan=None, wave=None):
    """Publish the exact approved implementation head with authenticated readback.

    This is the only implementation-head publication producer used by /go:
    non-force push of <expected-head>:refs/heads/<expected-branch>, followed by
    an exact parsed ls-remote readback. Existing exact publication is
    idempotent; an ancestor remote tip may fast-forward; divergence, malformed
    or ambiguous readback, protected branches, and missing local objects refuse.
    """
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'publish-head requires --expected-branch')
    require(expected_branch not in ('main', 'master'),
            'publish-head refuses protected branch ' + expected_branch)
    # Branch names are path components under refs/heads, never raw refs, tags,
    # main/master aliases, or shell-ish shorthand.  Production calls bind them
    # to campaign/<plan8>/w<wave>; unit fixtures may omit plan/wave for legacy
    # direct helper coverage.
    normalized_branch = git(repo, 'check-ref-format', '--branch', expected_branch).strip()
    require(normalized_branch == expected_branch and not expected_branch.startswith('refs/'),
            'publish-head refuses malformed branch ' + expected_branch)
    if plan is not None or wave is not None:
        require(isinstance(plan, str) and re.fullmatch('[0-9a-f]{64}', plan or '') is not None,
                'publish-head requires exact campaign plan id')
        require(type(wave) is int and wave >= 0, 'publish-head requires exact wave index')
        require(expected_branch == 'campaign/%s/w%d' % (plan[:8], wave),
                'publish-head branch is not bound to campaign/wave')
    require(isinstance(expected_head, str) and full_sha(expected_head),
            'publish-head requires --expected-head')
    origin = git(repo, 'config', '--get', 'remote.origin.url').strip()
    require(origin, 'publish-head requires configured origin')
    if expected_origin is not None:
        require(origin == expected_origin, 'origin drifted before publication')
    local = git(repo, 'rev-parse', expected_head + '^{commit}').strip()
    require(local == expected_head, 'approved head object is missing or ambiguous')
    local_ref = git(repo, 'rev-parse', 'refs/heads/' + expected_branch).strip()
    require(local_ref == expected_head, 'local branch ref drifted before publication')
    tree = git(repo, 'rev-parse', expected_head + '^{tree}').strip()
    if expected_tree is not None:
        require(tree == expected_tree, 'approved tree drifted before publication')
    remote_ref = 'refs/heads/' + expected_branch

    def read_remote():
        out = git(repo, 'ls-remote', '--heads', 'origin', remote_ref)
        lines = [line.strip() for line in out.splitlines() if line.strip()]
        require(len(lines) <= 1, 'ambiguous remote readback for ' + remote_ref)
        if not lines:
            return None
        parts = lines[0].split()
        require(len(parts) == 2 and parts[1] == remote_ref and full_sha(parts[0]),
                'malformed remote readback for ' + remote_ref)
        return parts[0]

    def verify_local_unchanged(when):
        # BUG-0079 REVIEW-2 item 6: the ls-remote readback is itself an
        # external-effect window. The origin/local-ref/tree guards run again
        # before ANY return — including the already-published idempotent
        # path — and again AFTER the final readback, not merely between the
        # push and the readback.
        require(git(repo, 'config', '--get', 'remote.origin.url').strip() == origin,
                'origin drifted ' + when)
        require(git(repo, 'rev-parse', 'refs/heads/' + expected_branch).strip() == expected_head,
                'local branch ref drifted ' + when)
        if expected_tree is not None:
            require(git(repo, 'rev-parse', expected_head + '^{tree}').strip() == tree,
                    'approved tree drifted ' + when)

    before = read_remote()
    if before == expected_head:
        verify_local_unchanged('during the already-published readback')
        return {'status': 'already-published', 'branch': expected_branch, 'head': expected_head}
    if before is not None:
        try:
            git(repo, 'merge-base', '--is-ancestor', before, expected_head)
        except Pending as error:
            raise Pending('remote branch diverged from approved head; refusing publication') from error
    git(repo, 'push', '--no-tags', 'origin', expected_head + ':' + remote_ref)
    verify_local_unchanged('during publication')
    after = read_remote()
    require(after == expected_head,
            'remote readback did not confirm exact approved head after publication')
    verify_local_unchanged('after the final readback')
    return {'status': 'published', 'branch': expected_branch, 'head': expected_head}


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

def status(repo, plan, wave, campaign_root=None):
    root = campaign_journal_root(repo, campaign_root)
    generation = journal_generation(repo, plan, wave, campaign_root=root)
    receipt = None
    if campaign_root is not None:
        path = root / '.planning/campaigns' / plan / f'coordinator-w{wave}.json'
        require(path.is_file() and not path.is_symlink(),
                'delivery proof requires the authoritative coordinator journal')
        data = campaign_journal(repo, plan, wave, root)
        require(isinstance(data, dict), 'delivery proof requires a valid coordinator journal')
        require(data.get('plan-id') == plan and data.get('wave') == wave,
                'delivery journal campaign/wave mismatch')
        receipt = data.get('receipt')
        require(isinstance(receipt, dict) and full_sha(receipt.get('head')) and
                isinstance(receipt.get('branch'), str) and receipt['branch'] and
                receipt['branch'] not in ('main', 'master') and
                isinstance(receipt.get('attempt-id'), str) and receipt['attempt-id'] and
                type(receipt.get('attempt-fence')) is int and receipt['attempt-fence'] >= 0,
                'delivery proof requires an attempt-bound Verify receipt')
    relative = binding_path(plan, wave, generation)
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
    if receipt is not None:
        require(head == receipt['head'] and wave_branch == receipt['branch'],
                'published binding does not authenticate the current receipt head/branch')
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
            'plan-id':plan, 'wave':wave, 'delivery-head-sha':head,
            'delivery-branch':wave_branch, 'binding-generation':generation,
            'attempt-id':receipt.get('attempt-id') if receipt is not None else None,
            'attempt-fence':receipt.get('attempt-fence') if receipt is not None else None}

def draft_request(source, plan, wave, pr, branch, required_checks, generation=0):
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
            'required-pr-checks':list(required_checks), 'binding-generation':generation}

def binding_branch(plan, wave, generation=0):
    binding_path(plan, wave)
    require(type(wave) is int and wave >= 0, 'invalid wave index')
    require(type(generation) is int and generation >= 0,
            'invalid binding generation')
    base = 'binding/' + plan[:12] + f'-w{wave}'
    # v1.00.33 W0 repair-tail protocol (audit §21.4): a republication for the
    # same wave (after a reconciled repair-tail receipt) MUST NOT reuse the
    # original binding branch — governance admits exactly one merged PR per
    # binding branch — so generation n >= 1 publishes under an -r<n> branch.
    return base if generation == 0 else base + f'-r{generation}'


def journal_generation(repo, plan, wave, campaign_root=None):
    """Repair-tail republication generation for the wave: the number of PRIOR
    immutable receipts (the append-only receipt-history length) in the live
    coordinator journal. 0 = first publication. A missing journal or a
    journal without history is generation 0; a malformed history fails
    closed rather than guessing a branch that governance would later reject."""
    require(isinstance(plan, str) and re.fullmatch('[0-9a-f]{64}', plan),
            'invalid campaign id')
    require(type(wave) is int and wave >= 0, 'invalid wave index')
    data = campaign_journal(repo, plan, wave, campaign_root)
    return len(data.get('receipt-history', [])) if data is not False else 0


def campaign_journal(repo, plan, wave, campaign_root=None):
    # One trusted loader owns schema, receipt/history and symlink validation.
    # Missing is distinct from malformed: the latter raises, never generation 0.
    root = campaign_journal_root(repo, campaign_root)
    data = json.loads(command(['racket', str(HERE / 'gsd-binding-data.rkt'),
                               'journal', str(root), plan, str(wave)]))
    require(data is False or isinstance(data, dict), 'invalid coordinator journal response')
    return data


def require_binding_receipt(data, head, branch):
    # Standalone generation-0 authoring predates journals; preserve that
    # transport contract, but every receipt-bearing production campaign is
    # pinned before authoring/review/publish, not merely at final readback.
    if data is not False:
        receipt = data['receipt']
        require(full_sha(receipt.get('head')) and head == receipt['head'] and
                branch == receipt.get('branch') and branch not in ('main', 'master') and
                isinstance(receipt.get('attempt-id'), str) and receipt['attempt-id'] and
                type(receipt.get('attempt-fence')) is int and receipt['attempt-fence'] >= 0,
                'binding must match the current attempt-bound receipt head/branch')


def binding_staging_path(campaign_root, plan, wave, generation=0):
    binding_path(plan, wave)
    require(type(generation) is int and generation >= 0,
            'invalid binding generation')
    root = Path(campaign_root)
    require(root.is_dir() and not root.is_symlink(),
            'binding staging requires a real campaign root')
    staging = f'binding-w{wave}' if generation == 0 else f'binding-w{wave}-r{generation}'
    # Reject a symlink or traversal in any existing parent component. The
    # resolved path is still compared below so a later symlink cannot redirect
    # the durable staging directory.
    current = root
    for part in ('.planning', 'campaigns', plan, staging):
        current = current / part
        require(not current.is_symlink(), 'binding staging path contains a symlink')
    expected = (root.resolve() / '.planning' / 'campaigns' / plan /
                staging).resolve()
    return expected


def require_binding_output(output, campaign_root, plan, wave, generation=0):
    expected = binding_staging_path(campaign_root, plan, wave, generation)
    raw = Path(output)
    require(not raw.is_symlink(), 'binding output path must not be a symlink')
    actual = raw.resolve()
    require(actual == expected,
            'binding output must be the deterministic campaign staging directory %s' % expected)
    return expected


def read_staged_trio(repo, plan, wave, output, campaign_root, expected_branch):
    # A staging directory may belong to ANY deterministic generation: the
    # journal-derived generation names where a NEW draft would be staged,
    # but a staging created before a repair tail (whose journal now carries
    # receipt-history) remains valid at its own generation — the strict
    # finalized-trio gates below do all the semantic work either way.
    generation = journal_generation(repo, plan, wave, campaign_root)
    resolved = Path(output).resolve()
    candidates = [generation]
    expected = None
    for gen in candidates:
        try:
            if require_binding_output(output, campaign_root, plan, wave, gen) == resolved:
                expected = require_binding_output(output, campaign_root, plan, wave, gen)
                break
        except Pending:
            continue
    require(expected is not None,
            'binding output must be a deterministic campaign staging directory for '
            'generation %d' % generation)
    if not expected.exists():
        return {'status': 'awaiting-review', 'output': str(expected),
                'branch': binding_branch(plan, wave, generation)}
    require(expected.is_dir() and not expected.is_symlink(),
            'binding staging output is not a regular directory')
    # Staged drafts belong to the selected immutable publication generation.
    artifact = Path(binding_path(plan, wave, generation)).name
    evidence_path = expected / 'docs/reports/gsd-wave-evidence' / artifact
    review_path = expected / 'docs/reports/gsd-wave-reviews' / artifact
    validation_path = expected / 'docs/reports/gsd-wave-validation' / artifact
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
    require_binding_receipt(campaign_journal(repo, plan, wave, campaign_root),
                            head, evidence.get('wave-branch'))
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
    branch = binding_branch(plan, wave, generation)
    require(evidence.get('branch') == branch,
            'staged binding branch identity mismatch')
    require(expected_branch is None or evidence.get('wave-branch') == expected_branch,
            'staged binding implementation branch does not match the durable receipt')
    require(isinstance(evidence.get('required-pr-checks'), list) and evidence['required-pr-checks'],
            'staged binding has no required-check policy snapshot')
    expected_review = f'docs/reports/gsd-wave-reviews/{artifact}'
    expected_validation = f'docs/reports/gsd-wave-validation/{artifact}'
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
    published = blob_or_none(repo, main, binding_path(plan, wave, generation))
    if published is not None:
        with tempfile.TemporaryDirectory(prefix='q-delivery-rebind-') as temp:
            path = Path(temp) / 'binding.rktd'
            path.write_bytes(published)
            try:
                bound = read_datum(path)
            except Pending as error:
                raise Pending('origin/main binding is malformed; refusing rebind') from error
        # Zero-declared repair topology: the implementation squash carries the
        # hash-named source trio to main AT the binding path without merge
        # identity. That pre-binding source is not a publication (the same
        # exemption prepare applies) and the finalized staged binding PR
        # supersedes it; only a bound record with full merge identity
        # constrains a republication.
        is_publication = (isinstance(bound, dict) and
                          all(bound.get(key) is not None for key in
                              ('merge-sha', 'delivery-head-sha', 'delivery-pr',
                               'implementation-sha')))
        require(isinstance(bound, dict) and (not is_publication or
                                             full_sha(bound.get('merge-sha'))),
                'origin/main binding is malformed; refusing rebind')
        require(not is_publication or
                (bound.get('merge-sha') == merge and
                 bound.get('delivery-head-sha') == head and
                 bound.get('implementation-sha') == merge),
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
                'binding': binding_path(plan, wave, generation), 'branch': branch,
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
            and not SENTINEL_TEXT.fullmatch(reviewer.strip())
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
            target = gate_root / 'docs/reports' / ('gsd-wave-' + kind) / artifact
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(source.read_bytes())
        # An audit-mode staged trio validates through the SAME discovered
        # bundle law as a committed one: mirror the controller-exported
        # owned-proofs directory from the staging output into the gate root.
        proofs_src = expected / 'docs/reports/gsd-wave-validation/owned-proofs'
        if proofs_src.is_dir() and not proofs_src.is_symlink():
            for payload in sorted(proofs_src.rglob('*')):
                if payload.is_file() and not payload.is_symlink():
                    relative = payload.relative_to(proofs_src)
                    target = gate_root / 'docs/reports/gsd-wave-validation/owned-proofs' / relative
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(payload.read_bytes())
        policy = blob_or_none(repo, main, str(POLICY_PATH))
        require(policy is not None, 'binding gate policy is unavailable')
        policy_path = gate_root / 'scripts/required-pr-checks.policy'
        policy_path.parent.mkdir(parents=True, exist_ok=True)
        policy_path.write_bytes(policy)
        gate = command(['racket', str(HERE / 'gsd-wave-gate.rkt'),
                        str(gate_root / 'docs/reports/gsd-wave-evidence' / artifact),
                        '--content-digest', EMPTY_SHA,
                        '--root', str(gate_root), '--policy', str(policy_path)],
                       cwd=str(gate_root))
        require('GSD wave evidence PASS' in gate,
                'finalized binding trio failed the strict wave gate')
    return {'status': 'reviewed', 'output': str(expected), 'binding': binding_path(plan, wave, generation),
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
    generation = journal_generation(repo, plan, wave, campaign_root)
    branch = binding_branch(plan, wave, generation)
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
        # Already-published resume: the binding PR for this branch is merged —
        # the publication completed. Idempotent return with the merge
        # identity; no republish and no fresh-main-base demand (that governs
        # REUSE of an unpublished branch, not a completed publication whose
        # protected main has lawfully advanced since).
        merged_publication = resolve_merged_pr(slug, branch)
        if merged_publication is not None:
            return {'status': 'already-published', 'pr': merged_publication.get('number'),
                    'branch': branch, 'head': remote_tip,
                    'merge-sha': dig(merged_publication, 'merge_commit_sha')}
        parents = git(repo, 'rev-list', '--parents', '-n', '1', remote_tip).split()
        require(len(parents) == 2 and parents[1] == main,
                'existing binding branch is not based on fresh origin/main')
        binding = binding_path(plan, wave, generation)
        bundle_files = _binding_bundle_files(output)
        with tempfile.TemporaryDirectory(prefix='q-delivery-existing-binding-') as temp:
            for kind in ('evidence', 'reviews', 'validation'):
                artifact = Path(binding_path(plan, wave, generation)).name
                relative = f'docs/reports/gsd-wave-{kind}/{artifact}'
                blob = blob_or_none(repo, remote_tip, relative)
                require(blob is not None, 'existing binding branch is missing ' + relative)
                path = Path(temp) / (kind + '.rktd')
                path.write_bytes(blob)
                remote_datum = read_datum(path)
                staged_path = output / 'docs/reports' / ('gsd-wave-' + kind) / artifact
                require(remote_datum == read_datum(staged_path),
                        'existing binding branch does not match the staged trio')
            # An audit binding publication carries its owned-proof bundle:
            # reuse demands byte-identical bundle files on the branch.
            for relative, source in bundle_files.items():
                blob = blob_or_none(repo, remote_tip, relative)
                require(blob is not None,
                        'existing binding branch is missing the audit bundle: ' + relative)
                require(blob == source.read_bytes(),
                        'existing binding branch audit bundle differs from staging: ' + relative)
        commit = remote_tip
    else:
        with tempfile.TemporaryDirectory(prefix='q-delivery-binding-') as temp:
            worktree = Path(temp)
            try:
                git(repo, 'worktree', 'add', '--detach', str(worktree), main)
                for kind in ('evidence', 'reviews', 'validation'):
                    artifact = Path(binding_path(plan, wave, generation)).name
                    source = output / 'docs/reports' / ('gsd-wave-' + kind) / artifact
                    require(source.is_file() and not source.is_symlink(),
                            'staged binding trio is not a regular file')
                    (worktree / 'docs/reports' / ('gsd-wave-' + kind)).mkdir(
                        parents=True, exist_ok=True)
                    target = worktree / 'docs/reports' / ('gsd-wave-' + kind) / artifact
                    target.write_bytes(source.read_bytes())
                # Mirror the staged owned-proof bundle (manifest, raw log,
                # authority payload) so the binding PR validates offline.
                for relative, source in _binding_bundle_files(output).items():
                    target = worktree / Path(relative)
                    require(not target.is_symlink(),
                            'staged audit bundle path is unsafe: ' + relative)
                    target.parent.mkdir(parents=True, exist_ok=True)
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
                    plan, wave, commit, branch, binding_path(plan, wave, generation))
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


def prepare(repo, plan, wave, number, relative, campaign_root, output,
            expected_branch=None, expected_head=None):
    """expected_head (the durable receipt head) disambiguates self-resolution
    when the delivery branch carries MULTIPLE merged PRs (lawful for repair
    generations: each republication squashes its own PR); resolve_merged_pr
    then picks the PR whose head IS the receipt head instead of refusing."""
    generation = journal_generation(repo, plan, wave, campaign_root)
    binding = binding_path(plan, wave, generation)
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    if number is None:
        require(isinstance(expected_branch, str) and expected_branch.strip(),
                'prepare --pr requires --expected-branch for durable PR self-resolution')
        pr = resolve_merged_pr(slug, expected_branch, expected_head)
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
    active = active_declared_outputs(facts, repo, plan, wave, generation)
    artifact_path(relative, 'gsd-wave-evidence')
    if declared is not None:
        if generation >= 1 and relative == declared['evidence']:
            raise Pending('generation-0 source refused for active repair generation; use ' +
                          active['evidence'])
        require(relative == active['evidence'],
                ('source evidence is not the frozen wave output for this plan/wave'
                 if generation == 0 else
                 'source evidence is not the active frozen wave output for this plan/wave'))
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
        require(source.get('review-artifact') == active['review'],
                ('source review artifact is not the frozen wave output'
                 if generation == 0 else
                 'source review artifact is not the active frozen wave output'))
        require(source.get('validation-artifact') == active['validation'],
                ('source validation artifact is not the frozen wave output'
                 if generation == 0 else
                 'source validation artifact is not the active frozen wave output'))
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
    require_binding_receipt(campaign_journal(repo, plan, wave, campaign_root),
                            head, dig(pr, 'head', 'ref'))
    request = draft_request(source, plan, wave, pr,
                            binding_branch(plan, wave, generation),
                            names, generation)
    output = Path(output).resolve()
    if output.exists():
        # Idempotent resume: a staging directory that already holds the
        # binding draft for THIS exact identity (same merge, PR head and
        # number, binding branch and generation) is already-prepared — the
        # ladder may proceed to binding-review on it. A staged draft for any
        # OTHER identity is still refused: staging is never overwritten and
        # never silently accepted across identities.
        staged = output / binding
        require(staged.is_file() and not staged.is_symlink(),
                'binding staging output exists without the staged binding draft')
        existing = read_datum(staged)
        require(isinstance(existing, dict),
                'existing staged binding draft is malformed')
        require(existing.get('merge-sha') == merge and
                existing.get('delivery-head-sha') == head and
                existing.get('delivery-pr') == number and
                existing.get('branch') == binding_branch(plan, wave, generation) and
                existing.get('wave-branch') == dig(pr, 'head', 'ref'),
                'existing binding staging binds a different delivery identity; '
                'refusing to reuse it for this preparation')
        return {'status': 'already-staged', 'output': str(output),
                'binding': binding, 'branch': binding_branch(plan, wave, generation),
                'merge-sha': merge, 'delivery-head-sha': head,
                'implementation-sha': merge}
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

def porcelain(repo):
    """Working-tree dirt report for delivery synchronization.

    W2 retirement (mirrors extensions/gsd/delivery-verifier.rkt): executor
    scratch moved OUT of the repository to one trusted session root below the
    platform temporary directory, so there is no in-tree `.planning/scratch/`
    evidence exemption any more. Any untracked or modified path inside the
    checkout is ordinary working-tree dirt and blocks synchronization; a
    genuinely disposable probe belongs under the session scratch root, which is
    outside this checkout and therefore invisible to git.
    """
    # -uall keeps the report explicit per file instead of collapsing a fresh
    # directory into one line. It no longer changes the verdict (every dirt
    # entry refuses) but keeps the diagnostic specific.
    return git(repo, 'status', '--porcelain', '--untracked-files=all')

def sync(repo, expected_branch):
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'sync requires --expected-branch')
    try:
        current = git(repo, 'symbolic-ref', '--quiet', '--short', 'HEAD').strip()
    except Pending:
        raise Pending('detached HEAD; refusing synchronization')
    # The shared main checkout (the controller's own repo context) may
    # carry the published bytes too: accept the main checkout alongside the
    # wave branch itself. Anything else is an unrelated branch and refuses;
    # the fast-forward below never switches branches.
    require(current == expected_branch or current == 'main',
            'synchronization refused: HEAD is %r, expected %r (unrelated branch changes not accepted)'
            % (current, expected_branch))
    require(not porcelain(repo).strip(), 'dirty checkout; refusing synchronization')
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
        pr = resolve_merged_pr(slug, branch, expected_head)
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
    head = dig(pr, 'head', 'sha')
    # Merged-resume (resolve_pr's idempotent path): a closed MERGED PR at the
    # exact durable receipt head keeps its trusted required checks at that
    # head, so a resumed campaign can still evaluate its CI stage without
    # opening a redundant PR. Everything else closed is refused: the open-PR
    # contract is only ever loosened by an explicit receipt-bound identity.
    merged_resume = (expected_head is not None and pr.get('state') == 'closed'
                     and pr_is_merged(pr) and head == expected_head)
    require(pr.get('state') == 'open' or merged_resume,
            'pull request is not open')
    validate_pr_identity(pr, slug)
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


def review(repo, plan, wave, expected_head, expected_branch, campaign_root=None, generation=None):
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
    if campaign_root is not None:
        actual_generation = journal_generation(repo, plan, wave, campaign_root)
        if generation is not None:
            require(type(generation) is int and generation >= 0 and generation == actual_generation,
                    'review binding generation does not match the authoritative journal')
        facts = snapshot_facts(campaign_root, plan, wave)
        relative = active_declared_outputs(facts, repo, plan, wave, actual_generation)['evidence']
    else:
        require(generation in (None, 0),
                'review --generation requires --campaign-root for authoritative resolution')
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


# =====================================================================
# Automatic independent review dispatch (BUG_PLAN automatic-review slice B)
# =====================================================================
#
# The auto-review port pairs the coordinator's fresh-reviewer hook with the
# trusted controller. `prepare` decides — through the UNCHANGED validation
# paths above — whether a genuine independent review is MISSING (dispatchable)
# or the state is already valid/irreparably refused; when dispatchable it
# stages a deliberately gate-red template trio plus a typed request.
# `finalize` re-validates the pinned identity, structurally validates the
# reviewer's TYPED output, binds every digest itself (reviewer prose is
# never authority), and publishes through an evidence-only commit + pinned
# non-force push + exact readback. Only the unchanged strict gates can turn
# an approval into `reviewed`; an explicit head-bound REQUEST_CHANGES
# persists its findings through a trusted artifact commit and surfaces as
# `review-rejected` (post-merge binding rejections are typed separately for
# artifact repair). Malformed output, transport failures and divergence are
# `review-infra` — never a rejection and never an approval.

class _ReviewInfra(Exception):
    """Typed reviewer-infrastructure failure: never a rejection or approval."""


MIN_REVIEW_NARRATIVE = 64
MIN_RED_FIRST_FAILURE = 32
MAX_REVIEW_FINDINGS = 50
MAX_FINDING_CHARS = 2000
AUTO_REVIEW_STAGES = ('implementation-review', 'binding-review')


def _iso_now():
    return time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())


def write_rktd(datum, path):
    """Serialize one JSON-shaped hash datum as a .rktd artifact through the
    trusted binding-data module (never string-built Racket syntax here)."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(prefix='q-datum-', suffix='.json', delete=False) as handle:
        handle.write(json.dumps(datum).encode())
        payload = Path(handle.name)
    try:
        command(['racket', str(HERE / 'gsd-binding-data.rkt'), 'write-datum',
                 str(payload), str(path)], timeout=60)
    finally:
        try:
            payload.unlink()
        except OSError:
            pass
    return path


def _reviewer_identity(value):
    require(isinstance(value, str) and value.strip() and
            not SENTINEL_TEXT.fullmatch(value.strip()) and
            not SENTINEL_COMPOUND.search(value.strip()),
            'the reviewer output carries no genuine reviewer identity')
    return value.strip()


def _reviewer_narrative(value, label, minimum):
    require(isinstance(value, str) and value.strip() and
            not SENTINEL_TEXT.fullmatch(value.strip()),
            'the reviewer output %s is missing or a placeholder' % label)
    require(len(value.strip()) >= minimum,
            'the reviewer output %s carries fewer than %d characters of content'
            % (label, minimum))
    return value.strip()


def validate_reviewer_output(output, request=None):
    """Structural validation of the reviewer's TYPED result (the coordinator
    pre-checks the same shape; this is the trusted boundary). Trusts the
    ROLE, never prose: verdicts, digests and head bindings are stamped by the
    controller below, never taken from this output. Every malformed shape is
    a typed reviewer-infrastructure failure — NEVER a rejection and NEVER an
    approval."""
    def infra(reason):
        raise _ReviewInfra(reason)
    if not isinstance(output, dict):
        infra('the reviewer output is not an object')
    kind = output.get('kind')
    if kind not in ('approved', 'request-changes', 'review-failed'):
        infra('the reviewer output kind is unknown')
    if kind == 'review-failed':
        reason = output.get('reason')
        if not (isinstance(reason, str) and reason.strip()):
            infra('the reviewer session failure carries no reason')
        raise _ReviewInfra('the reviewer session failed: %s' % reason.strip()[:300])
    if not (isinstance(output.get('reviewer'), str) and output['reviewer'].strip() and
            not SENTINEL_TEXT.fullmatch(output['reviewer'].strip()) and
            not SENTINEL_COMPOUND.search(output['reviewer'].strip())):
        infra('the reviewer output carries no genuine reviewer identity')
    result = {'kind': kind, 'reviewer': output['reviewer'].strip()}
    for label, key in (('review scope', 'scope'), ('review report', 'report')):
        value = output.get(key)
        if not (isinstance(value, str) and value.strip() and
                not SENTINEL_TEXT.fullmatch(value.strip())):
            infra('the reviewer output %s is missing or a placeholder' % label)
        if len(value.strip()) < MIN_REVIEW_NARRATIVE:
            infra('the reviewer output %s carries fewer than %d characters of content'
                  % (label, MIN_REVIEW_NARRATIVE))
        result[key] = value.strip()
    if request and request.get('review-mode') == OWNED_AUDIT_MODE:
        ack = output.get('audit-ack')
        if not (isinstance(ack, dict) and ack.get('mode') == OWNED_AUDIT_MODE and
                ack.get('verification-origin') == 'audited-owned' and
                ack.get('source-read') is True and ack.get('raw-output-read') is True and
                _exact_audit_binding(ack.get('binding'), _audit_binding(request))):
            infra('missing or stale owned-proof audit acknowledgement')
        if 'validation' in output:
            infra('model-authored gate facts are forbidden in owned-proof audit mode')
        # The peer must meaningfully acknowledge a LINKED behavioral RED — the
        # exact retained red identity plus its actual raw failure output — and
        # can never acknowledge a red the owner has not linked.
        authority = request.get('owned-authority') if isinstance(request, dict) else None
        red = authority.get('red') if isinstance(authority, dict) else None
        if isinstance(red, dict):
            red_sha = (red.get('reference') or {}).get('sha256')
            if not (isinstance(ack.get('red-ack'), dict) and
                    ack['red-ack'].get('proof-sha256') == red_sha and
                    ack['red-ack'].get('raw-output-read') is True):
                infra('missing or stale owned behavioral RED acknowledgement')
        elif 'red-ack' in ack:
            infra('a behavioral RED was acknowledged without a linked owned red proof')
        result['audit-ack'] = ack
        if kind == 'approved':
            return result
    if kind == 'request-changes':
        findings = output.get('findings')
        if not (isinstance(findings, list) and 1 <= len(findings) <= MAX_REVIEW_FINDINGS and
                all(isinstance(item, str) and item.strip() and
                    len(item) <= MAX_FINDING_CHARS for item in findings)):
            infra('the REQUEST_CHANGES findings are missing or malformed')
        result['findings'] = list(findings)
        return result
    validation = output.get('validation')
    if not isinstance(validation, dict):
        infra('the approved review carries no validation gate facts')
    red = validation.get('red-first')
    if not isinstance(red, dict):
        infra('the red-first gate evidence is incomplete')
    command = red.get('command')
    if not (isinstance(command, str) and command.strip() and
            not SENTINEL_TEXT.fullmatch(command.strip())):
        infra('the red-first command fact is missing or a placeholder')
    failure = red.get('failure')
    if not (isinstance(failure, str) and failure.strip() and
            not SENTINEL_TEXT.fullmatch(failure.strip())):
        infra('the red-first observed failure is missing or a placeholder')
    if len(failure.strip()) < MIN_RED_FIRST_FAILURE:
        infra('the red-first observed failure carries fewer than %d characters of content'
              % MIN_RED_FIRST_FAILURE)
    gates = {'red-first': {'command': command.strip(), 'failure': failure.strip()}}
    for gate in ('focused-tests', 'format-compile', 'lint', 'fast'):
        facts = validation.get(gate)
        if not (isinstance(facts, dict) and facts.get('result') == 'passed' and
                isinstance(facts.get('command'), str) and facts['command'].strip()):
            infra('the %s gate facts are missing or not passed' % gate)
        gates[gate] = {'result': 'passed', 'command': facts['command'].strip()}
    remaining = validation.get('remaining-items', [])
    if not isinstance(remaining, list):
        infra('remaining-items must be a list')
    gates['remaining-items'] = remaining
    if validation.get('planning-sync') != 'current':
        infra('the reviewer planning-sync fact is not current')
    result['validation'] = gates
    return result


def load_reviewer_output(path, request=None):
    path = Path(path)
    if not (path.is_file() and not path.is_symlink()):
        raise _ReviewInfra('the reviewer output is absent at %s' % path)
    try:
        output = json.loads(path.read_text())
    except ValueError as error:
        raise _ReviewInfra('the reviewer output is not valid JSON: %s' % error) from error
    return validate_reviewer_output(output, request)


def implementation_review_staging(campaign_root, plan, wave, generation, staging_root):
    """The deterministic campaign-local, generation-scoped implementation
    review staging directory (never another generation's staging)."""
    root = Path(campaign_root).resolve()
    staging = Path(staging_root).resolve()
    expected = root / '.planning' / 'campaigns' / plan / f'review-w{wave}-g{generation}'
    require(staging == expected,
            'the review dispatch staging must be the deterministic campaign directory %s'
            % expected)
    current = root
    for part in ('.planning', 'campaigns', plan):
        current = current / part
        require(not current.is_symlink(), 'the review staging path contains a symlink')
    return staging


def _stage_template(staging, relative, datum):
    """Write one gate-red template artifact idempotently: an existing file
    must be byte-identical (a different dispatch already staged here and is
    never silently overwritten; old generations are separate directories)."""
    target = staging / relative
    with tempfile.TemporaryDirectory(prefix='q-template-') as temp:
        candidate = Path(temp) / 'template.rktd'
        write_rktd(datum, candidate)
        payload = candidate.read_bytes()
    if target.exists():
        require(target.is_file() and not target.is_symlink() and target.read_bytes() == payload,
                'the review staging already holds a different dispatch for ' + relative)
    else:
        write_rktd(datum, target)


def _stage_request(staging, request):
    """The controller-owned dispatch request; rewritten on every prepare so
    it always reflects the CURRENT pinned identity and controller context."""
    require(isinstance(staging, Path) and staging.is_dir(),
            'the review staging directory is missing')
    payload = staging / 'request.json'
    payload.write_text(json.dumps(request, sort_keys=True))
    return payload


def _implementation_templates(plan, wave, facts, names, head, branch, paths):
    """Deliberately gate-red trio: honest PENDING placeholders, no verdict,
    no acceptance, no fabricated digest (the finalize phase stamps those)."""
    artifact = {'schema-version': 2, 'plan-id': plan, 'wave': f'W{wave}',
                'milestone': facts['milestone'], 'issue': facts['issue'],
                'status': 'pending-review', 'implementation-sha': head,
                'content-digest': 'PENDING', 'branch': branch,
                'required-checks': list(names), 'required-pr-checks': list(names),
                'review-artifact': paths['review'],
                'validation-artifact': paths['validation'],
                'review-stage': 'fresh-independent-review-required'}
    review = {'reviewer': 'PENDING', 'verdict': 'PENDING', 'timestamp': 'PENDING',
              'reviewed-sha': head, 'content-digest': 'PENDING',
              'scope': 'PENDING', 'report': 'PENDING'}
    validation = {'status': 'pending', 'planning-sync': 'pending',
                  'milestone': facts['milestone'], 'wave': f'W{wave}',
                  'issue': facts['issue'], 'branch': branch,
                  'implementation-sha': head, 'content-digest': 'PENDING',
                  'red-first': {'command': 'PENDING', 'failure': 'PENDING'},
                  'focused-tests': {'result': 'PENDING', 'command': 'PENDING'},
                  'format-compile': {'result': 'PENDING', 'command': 'PENDING'},
                  'lint': {'result': 'PENDING', 'command': 'PENDING'},
                  'fast': {'result': 'PENDING', 'command': 'PENDING'},
                  'review-artifact': paths['review'],
                  'remaining-items': [
                      {'classification': 'deferred-noncritical', 'owner': 'PENDING',
                       'rationale': 'PENDING: a genuine independent non-author review is '
                                    'required before finalization'}]}
    return artifact, review, validation


def _review_prompt(stage, plan, wave, head, branch, paths, staging, digest_value, names):
    if stage == 'implementation-review':
        return ('Fresh independent non-author review of campaign %s wave W%d at the pinned '
                'receipt head %s on branch %s.\n'
                'A genuinely gate-red template trio (honest PENDING placeholders, no verdict) '
                'is staged at %s (evidence %s, review %s, validation %s).\n'
                'The controller-computed excluded-diff content digest is %s; the required '
                'checks are: %s.\n'
                'AUDIT the pinned implementation and authenticated owned manifest plus actual '
                'raw outputs using read/bash. Do not rerun owned full fast, focused, format, '
                'compile, lint or RED gates; never claim you executed them. When the pinned '
                'owned authority links a behavioral RED proof, read its manifest and actual '
                'raw failure output and acknowledge it with red-ack '
                '{"proof-sha256": <linked red sha256>, "raw-output-read": true}. '
                'Return an exact audit-ack and genuine verdict/findings, not gate facts. '
                'Missing authenticated behavioral RED, canary or F11 stays unproven. '
                'The review deadline is 600 seconds, including inspection; do not start long replay jobs.'
                % (plan, wave, head, branch, staging, paths['evidence'], paths['review'],
                   paths['validation'], digest_value, ', '.join(names)))
    return ('Fresh independent non-author finalization of the staged post-merge binding trio '
            'for campaign %s wave W%d (implementation receipt head %s, branch %s).\n'
            'The staged draft is pending-review (honest placeholders; no approval inferred). '
            'Staging: %s (evidence %s, review %s, validation %s). The publication commit '
            'changes only excluded evidence directories, so the finalized digest is the '
            'empty-diff digest %s.\n'
            'AUDIT the original execution proof and implementation with read/bash. Do not rerun '
            'owned full fast, focused, format, compile, lint or RED gates; never claim you ran them. '
            'Return audit-ack and genuine findings/verdict, not gate facts. A merged implementation '
            'is NOT an executed head: an unsupported execution/merge relation must stop. '
            'The review deadline remains 600 seconds.'
            % (plan, wave, head, branch, staging, paths['evidence'], paths['review'],
               paths['validation'], EMPTY_SHA))


def _receipt_for_review(repo, plan, wave, campaign_root, expected_head, expected_branch):
    journal = campaign_journal(repo, plan, wave, campaign_root)
    receipt = journal['receipt'] if isinstance(journal, dict) else None
    require(isinstance(receipt, dict) and receipt.get('head') == expected_head and
            receipt.get('branch') == expected_branch and
            isinstance(receipt.get('attempt-id'), str) and receipt['attempt-id'] and
            type(receipt.get('attempt-fence')) is int,
            'the dispatch identity does not match the durable receipt; rerun prepare')
    return receipt


def auto_review_prepare(repo, plan, wave, stage, expected_head, expected_branch,
                         campaign_root, generation, staging_root, manual_compatibility=False):
    require(stage in AUTO_REVIEW_STAGES, 'unknown auto-review stage ' + str(stage))
    if stage == 'binding-review':
        return _prepare_binding(repo, plan, wave, expected_head, expected_branch,
                                campaign_root, generation, staging_root, manual_compatibility)
    facts = review_tracker_facts(repo, campaign_root, plan, wave)
    milestone, issue = facts['milestone'], facts['issue']
    actual_generation = journal_generation(repo, plan, wave, campaign_root)
    require(generation == actual_generation,
            'the review binding generation does not match the authoritative journal')
    paths = active_declared_outputs(facts, repo, plan, wave, actual_generation)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    git(repo, 'fetch', '--no-tags', 'origin', 'refs/heads/' + expected_branch)
    tip = git(repo, 'rev-parse', 'FETCH_HEAD').strip()
    require(full_sha(tip), 'malformed fetched receipt branch tip')
    require_receipt_tip(repo, expected_head, tip, 'review')
    _require_review_evidence_tail(repo, expected_head, tip)
    receipt = _receipt_for_review(repo, plan, wave, campaign_root,
                                  expected_head, expected_branch)
    # Publication can precede the journal checkpoint. Recover committed,
    # receipt-bound rejection evidence BEFORE deciding whether to dispatch.
    recovery_authority = None
    if (not manual_compatibility and receipt.get('owned-proof') and
            blob_or_none(repo, tip, paths['review']) is not None):
        recovery_staging = implementation_review_staging(
            campaign_root, plan, wave, actual_generation, staging_root)
        recovery_staging.mkdir(parents=True, exist_ok=True)
        recovery_authority = _owned_authority(campaign_root, plan, wave, recovery_staging)
    rejected = _recover_rejected_artifact(repo, tip, paths['review'], plan, wave,
                                          actual_generation, receipt, recovery_authority)
    if rejected is not None:
        return rejected
    # Dispatchable ONLY when the trio or its review artifact is genuinely
    # missing. An existing-but-invalid trio is a REAL refusal (Pending), and
    # an already-valid one passes through the unchanged review path.
    if blob_or_none(repo, tip, paths['evidence']) is not None:
        result = review(repo, plan, wave, expected_head, expected_branch,
                        campaign_root, generation)
        if result['status'] == 'reviewed':
            if not manual_compatibility:
                raise _ReviewInfra('legacy manual approval is not production owned-proof audit approval')
            return result
    receipt = _receipt_for_review(repo, plan, wave, campaign_root,
                                  expected_head, expected_branch)
    names = policy_names(repo, main)
    digest_value = digest(repo, main, tip)
    staging = implementation_review_staging(campaign_root, plan, wave,
                                            actual_generation, staging_root)
    staging.mkdir(parents=True, exist_ok=True)
    artifact, review_datum, validation = _implementation_templates(
        plan, wave, facts, names, expected_head, expected_branch, paths)
    _stage_template(staging, paths['evidence'], artifact)
    _stage_template(staging, paths['review'], review_datum)
    _stage_template(staging, paths['validation'], validation)
    request = {'kind': 'implementation-review', 'plan-id': plan, 'wave': wave,
               'generation': actual_generation, 'head': expected_head,
               'branch': expected_branch, 'tip': tip,
               'tree': receipt['tree'],
               'attempt-id': receipt['attempt-id'],
               'attempt-fence': receipt['attempt-fence'],
               'evidence': paths['evidence'], 'review': paths['review'],
               'validation': paths['validation'], 'content-digest': digest_value,
               'required-checks': list(names), 'milestone': milestone, 'issue': issue,
               'staging': str(staging),
               'reviewer-output': str(staging / 'reviewer-output.json'),
               'fresh-session-required': True, 'non-author-review-required': True,
               'prompt': _review_prompt(stage, plan, wave, expected_head, expected_branch,
                                        paths, str(staging), digest_value, names)}
    _bind_owned_request(request, campaign_root, plan, wave, staging, manual_compatibility)
    _stage_request(staging, request)
    return {'status': 'awaiting-review', 'stage': 'implementation-review',
            'request': request}


def _prepare_binding(repo, plan, wave, expected_head, expected_branch,
                     campaign_root, generation, staging_root, manual_compatibility=False):
    staging = require_binding_output(staging_root, campaign_root, plan, wave, generation)
    staged = binding_review(repo, plan, wave, staging, campaign_root, expected_branch)
    if staged['status'] == 'reviewed':
        if not manual_compatibility:
            raise _ReviewInfra('existing manual binding approval is not production audit approval')
        return staged
    require(staged['status'] == 'pending-review',
            'the binding staging is %s; only a pending-review draft may dispatch a fresh '
            'reviewer (run binding-prepared first)' % staged['status'])
    receipt = _receipt_for_review(repo, plan, wave, campaign_root,
                                  expected_head, expected_branch)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    names = policy_names(repo, main)
    artifact = Path(staged['binding']).name
    paths = {'evidence': staged['binding'],
             'review': f'docs/reports/gsd-wave-reviews/{artifact}',
             'validation': f'docs/reports/gsd-wave-validation/{artifact}'}
    request = {'kind': 'binding-review', 'plan-id': plan, 'wave': wave,
               'generation': generation, 'head': expected_head,
               'branch': expected_branch, 'merge-sha': staged['merge-sha'],
               'delivery-head-sha': staged['delivery-head-sha'],
               'binding-branch': staged['branch'], 'tree': receipt['tree'],
               'attempt-id': receipt['attempt-id'],
               'attempt-fence': receipt['attempt-fence'],
               'evidence': paths['evidence'], 'review': paths['review'],
               'validation': paths['validation'], 'content-digest': EMPTY_SHA,
               'required-checks': list(names), 'staging': str(staging),
               'reviewer-output': str(staging / 'reviewer-output.json'),
               'fresh-session-required': True, 'non-author-review-required': True,
               'prompt': _review_prompt('binding-review', plan, wave, expected_head,
                                        expected_branch, paths, str(staging),
                                        EMPTY_SHA, names)}
    _bind_owned_request(request, campaign_root, plan, wave, staging, manual_compatibility)
    if not manual_compatibility and staged['merge-sha'] != expected_head:
        raise _ReviewInfra('unsupported-postmerge-owned-proof-relation: execution head is not merged head')
    _stage_request(staging, request)
    return {'status': 'awaiting-review', 'stage': 'binding-review', 'request': request}


OWNED_AUDIT_MODE = 'owned-proof-audit-v1'
AUDIT_KEYS = ('plan-id', 'wave', 'generation', 'head', 'tree', 'branch',
              'attempt-id', 'attempt-fence', 'content-digest', 'proof-sha256')


def _audit_binding(request):
    binding = {key: request.get(key) for key in AUDIT_KEYS}
    authority = request.get('owned-authority', {})
    reference = authority.get('reference', {})
    promotion = request.get('supplemental-promotion-sha256')
    expected = authority.get('supplemental-promotion-sha256')
    if 'supplemental-contract' in reference or promotion is not None or expected is not None:
        if not (isinstance(promotion, str) and re.fullmatch(r'[0-9a-f]{64}', promotion)
                and promotion == expected):
            raise _ReviewInfra('missing or mismatched supplemental promotion binding')
        binding['supplemental-promotion-sha256'] = promotion
    revision = request.get('evidence-revision', 0)
    if revision:
        binding['evidence-revision'] = revision
        binding['evidence-registration-sha256'] = request['evidence-registration-sha256']
    return binding


def _exact_audit_binding(actual, expected):
    return (isinstance(actual, dict) and actual.keys() == expected.keys() and
            all(type(actual[key]) is type(value) and actual[key] == value
                for key, value in expected.items()))


def _owned_authority(campaign_root, plan, wave, staging):
    try:
        return json.loads(command(['racket', str(HERE / 'gsd-binding-data.rkt'),
                                   'owned-review', str(campaign_root), plan, str(wave),
                                   str(staging)], timeout=60))
    except (Pending, ValueError) as error:
        raise _ReviewInfra('missing or invalid authenticated owned proof: %s' % error) from error


def _bind_owned_request(request, campaign_root, plan, wave, staging, manual=False):
    if manual:
        request['review-mode'] = 'manual-compatibility'
        return
    authority = _owned_authority(campaign_root, plan, wave, staging)
    receipt = authority['receipt']
    if (authority['generation'] != request['generation'] or
            any(request.get(key) != receipt.get(key) for key in
                ('head', 'tree', 'branch', 'attempt-id', 'attempt-fence'))):
        raise _ReviewInfra('stale owned-proof receipt/generation')
    request['review-mode'] = OWNED_AUDIT_MODE
    request['proof-sha256'] = authority['reference']['sha256']
    request['evidence-revision'] = authority.get('evidence-revision', 0)
    if request['evidence-revision']:
        request['evidence-registration-sha256'] = authority['evidence-registration-sha256']
    else:
        request.pop('evidence-registration-sha256', None)
    if 'supplemental-promotion-sha256' in authority:
        request['supplemental-promotion-sha256'] = authority['supplemental-promotion-sha256']
    else:
        request.pop('supplemental-promotion-sha256', None)
    request['owned-authority'] = authority
    authority['audit-binding'] = _audit_binding(request)
    request['prompt'] += ('\nPinned audit binding: '
                          + json.dumps(_audit_binding(request), sort_keys=True)
                          + '\nOwned manifest/raw log bundle: ' + str(staging / authority['bundle'])
                          + '\nRead the manifest AND actual raw outputs and source before acknowledging.')
    if isinstance(authority.get('red'), dict):
        request['prompt'] += ('\nLinked behavioral RED proof (separate authentic negative execution, '
                              'same bundle directory): read its manifest and actual raw failure '
                              'output, then acknowledge it with red-ack {"proof-sha256": '
                              '"%s", "raw-output-read": true}. '
                              % authority['red']['reference']['sha256'])
    else:
        request['prompt'] += ('\nNo behavioral RED proof is linked to this green execution: '
                              'acknowledge none and report the gap honestly.')


def _reauthenticate_owned_request(request, campaign_root, plan, wave, staging, manual):
    if manual:
        if request.get('review-mode') != 'manual-compatibility':
            raise _ReviewInfra('manual compatibility cannot consume a production audit request')
        return None
    if request.get('review-mode') != OWNED_AUDIT_MODE:
        raise _ReviewInfra('production review requires owned-proof audit mode')
    fresh = _owned_authority(campaign_root, plan, wave, staging)
    if (fresh['generation'] != request['generation'] or
            any(request.get(key) != fresh['receipt'].get(key) for key in
                ('head', 'tree', 'branch', 'attempt-id', 'attempt-fence'))):
        raise _ReviewInfra('stale owned receipt/generation before publication')
    if (request.get('evidence-revision', 0) != fresh.get('evidence-revision', 0) or
            request.get('evidence-registration-sha256') !=
            fresh.get('evidence-registration-sha256') or
            request.get('supplemental-promotion-sha256') !=
            fresh.get('supplemental-promotion-sha256')):
        raise _ReviewInfra('evidence registration drifted since prepare')
    fresh['audit-binding'] = _audit_binding(request)
    if fresh != request.get('owned-authority'):
        changed = sorted(key for key in set(fresh) | set(request.get('owned-authority', {}))
                         if fresh.get(key) != request.get('owned-authority', {}).get(key))
        detail = ''
        if 'publication-files' in changed:
            detail = '; publication-files prepared=%s fresh=%s' % (
                json.dumps(request['owned-authority'].get('publication-files', [])),
                json.dumps(fresh.get('publication-files', [])))
        raise _ReviewInfra('owned proof or receipt drifted since prepare: ' + ', '.join(changed) + detail)
    if request.get('proof-sha256') != fresh['reference']['sha256']:
        raise _ReviewInfra('owned proof SHA drifted since prepare')
    return fresh


def _owned_approval_gates(model_gates, authority):
    # Manual compatibility keeps the legacy injected fixtures. Production
    # owned audit fills EVERY gate fact from the authenticated owner record:
    # green execution facts plus, when linked, the separate authentic
    # behavioral RED facts. There is deliberately no model-prose fallback;
    # a missing linked RED is a typed stop, never approval or rejection.
    if authority is None:
        return model_gates
    red = authority.get('red')
    if not isinstance(red, dict):
        raise _ReviewInfra('missing-meaningful-red-proof: authenticated behavioral RED unavailable; '
                           'owned green and independent approval are not delivery approval')
    green = authority['green-gates']
    return {'red-first': {'command': red['command'],
                          'failure': red['failure-summary'],
                          'result': 'passed',
                          'verification-origin': 'audited-owned',
                          'owner-job-id': red['owner-job-id'],
                          **({'origin': red['origin'], 'relation': red['relation']}
                             if red.get('relation') == 'baseline-with-test-overlay-v1' else {})},
            'focused-tests': green['focused-tests'],
            'format-compile': green['format-compile'],
            'lint': green['lint'],
            'fast': green['fast'],
            'remaining-items': []}


def _load_dispatch_request(staging, plan, wave, generation, expected_head,
                           expected_branch, stage):
    path = Path(staging) / 'request.json'
    require(path.is_file() and not path.is_symlink(),
            'no review dispatch request at %s; run prepare first' % path)
    try:
        request = json.loads(path.read_text())
    except ValueError as error:
        raise Pending('the review dispatch request is malformed: %s' % error) from error
    require(isinstance(request, dict) and
            request.get('kind') == stage and
            request.get('plan-id') == plan and
            request.get('wave') == wave and
            request.get('generation') == generation and
            request.get('head') == expected_head and
            request.get('branch') == expected_branch,
            'the dispatch identity does not match the durable receipt; rerun prepare')
    return request


def _owned_publication_files(staging, authority):
    if authority is None:
        return {}
    bundle = authority['bundle']
    refs = [authority['reference'], authority['log-reference']]
    red = authority.get('red')
    if isinstance(red, dict) and red.get('relation') != 'baseline-with-test-overlay-v1':
        refs += [red['reference'], red['log-reference']]
    files = {bundle + '/' + ref['path']: Path(staging) / bundle / ref['path']
             for ref in refs}
    for relative in authority.get('publication-files', []):
        path = PurePosixPath(relative)
        require(not path.is_absolute() and '..' not in path.parts and
                relative.startswith(bundle + '/'), 'unsafe portable proof path')
        source = Path(staging) / path
        require(source.is_file() and not source.is_symlink(),
                'missing portable owned proof: ' + relative)
        if source.suffix == '.json' and re.fullmatch('[0-9a-f]{64}', source.stem):
            require(hashlib.sha256(source.read_bytes()).hexdigest() == source.stem,
                    'portable proof digest mismatch: ' + relative)
        if source.name == 'objects.bundle':
            metadata = json.loads((source.parent / 'bundle.json').read_text())
            require(hashlib.sha256(source.read_bytes()).hexdigest() ==
                    metadata['git-bundle-sha256'], 'portable Git bundle digest mismatch')
        files[relative] = source
    # The committed authority payload lets push CI (and the staged binding
    # gate) validate the audit trio with the exact trusted authority the
    # controller used: same excluded directory, same trust level as the
    # trio itself. Written from the FINAL reauthenticated authority, so the
    # committed bytes match the publication exactly.
    payload = Path(staging) / bundle / 'authority.json'
    payload.parent.mkdir(parents=True, exist_ok=True)
    payload.write_text(json.dumps(authority, sort_keys=True), encoding='utf-8')
    files[bundle + '/authority.json'] = payload
    return files


def _binding_bundle_files(staging):
    """Controller-authored owned-proof bundle files staged beside an audit
    binding trio. They are mirrored into the binding publication so the
    binding PR's governance gate can validate the audit trio through the
    discovered committed authority even when the implementation review was
    manual-compatibility and the bundle never reached main through it."""
    proofs = Path(staging) / 'docs/reports/gsd-wave-validation/owned-proofs'
    files = {}
    if proofs.is_dir() and not proofs.is_symlink():
        for payload in sorted(proofs.rglob('*')):
            if payload.is_file() and not payload.is_symlink():
                relative = payload.relative_to(proofs.parent)
                require(not relative.is_absolute() and '..' not in relative.parts,
                        'unsafe staged owned-proof bundle path: ' + str(payload))
                files['docs/reports/gsd-wave-validation/' + relative.as_posix()] = payload
    return files


def _commit_candidate(repo, base_tip, datums, message, raw_files=None):
    """Create a local evidence-only candidate; NEVER change remote refs."""
    require(isinstance(datums, dict) and datums, 'the review publication has no artifacts')
    raw_files = raw_files or {}
    for relative in (*datums, *raw_files):
        require(isinstance(relative, str) and relative.startswith(EXCLUDED_PREFIXES) and
                '..' not in PurePosixPath(relative).parts,
                'the review publication may only touch excluded evidence paths: ' + relative)
    with tempfile.TemporaryDirectory(prefix='q-auto-review-') as temp:
        worktree = Path(temp) / 'wt'
        git(repo, 'worktree', 'add', '--detach', str(worktree), base_tip)
        try:
            for relative, datum in datums.items():
                target = worktree / relative
                require(not target.is_symlink(), 'the candidate artifact is a symlink')
                if target.exists():
                    require(target.is_file(), 'the candidate artifact is not a file')
                    target.unlink()
                write_rktd(datum, target)
            for relative, source in raw_files.items():
                target = worktree / relative
                require(source.is_file() and not source.is_symlink(), 'unsafe raw proof source')
                for parent in target.parents:
                    require(not parent.is_symlink(), 'symlinked candidate proof directory')
                    if parent == worktree:
                        break
                target.parent.mkdir(parents=True, exist_ok=True)
                require(not target.is_symlink(), 'candidate raw proof is a symlink')
                data = source.read_bytes()
                if target.exists():
                    require(target.read_bytes() == data, 'published addressed proof differs')
                else:
                    target.write_bytes(data)
            git(worktree, 'add', '--', *sorted((*datums, *raw_files)))
            git(worktree, '-c', 'user.name=gsd-delivery-controller',
                '-c', 'user.email=gsd-delivery@invalid',
                'commit', '--no-gpg-sign', '-m', message)
            commit = git(worktree, 'rev-parse', 'HEAD').strip()
        finally:
            try:
                git(repo, 'worktree', 'remove', '--force', str(worktree))
            except Pending:
                pass
    return commit


def _publish_candidate(repo, branch, base_tip, commit, guard=None):
    """Publish an already validated candidate with pinned non-force readback."""
    git(repo, 'fetch', '--no-tags', 'origin', 'refs/heads/' + branch)
    remote = git(repo, 'rev-parse', 'FETCH_HEAD').strip()
    if remote != base_tip:
        raise _ReviewInfra(
            'the remote branch diverged from the prepared review tip during review '
            '(%s != %s); the dispatch is stale and was not published'
            % (remote[:12], base_tip[:12]))
    if guard is not None:
        guard()  # proof bytes, receipt/generation and frozen authority AFTER fetch, BEFORE push
    try:
        git(repo, 'push', '--no-tags', 'origin', f'{commit}:refs/heads/{branch}')
        git(repo, 'fetch', '--no-tags', 'origin',
            f'refs/heads/{branch}:refs/remotes/origin/{branch}')
        readback = git(repo, 'rev-parse', 'refs/remotes/origin/' + branch).strip()
    except Pending as error:
        raise _ReviewInfra('publishing the reviewed artifacts failed: %s' % error) from error
    require(readback == commit,
            'the reviewed publication readback does not match the published commit')
    return commit


def _require_review_evidence_tail(repo, head, tip):
    """Every tail commit must be evidence-only, not just its aggregate diff."""
    require_receipt_tip(repo, head, tip, 'review')
    for commit in git(repo, 'rev-list', head + '..' + tip).splitlines():
        changed = git(repo, 'diff-tree', '-m', '--no-commit-id', '--name-only',
                      '--no-renames', '-r', commit).splitlines()
        require(all(path.startswith(EXCLUDED_PREFIXES) for path in changed),
                'the review ancestry contains non-evidence paths')


def _recover_rejected_artifact(repo, tip, relative, plan, wave, generation, receipt,
                               authority=None):
    blob = blob_or_none(repo, tip, relative)
    if blob is None:
        return None
    with tempfile.TemporaryDirectory(prefix='q-rejected-') as temp:
        path = Path(temp) / 'rejected.rktd'
        path.write_bytes(blob)
        datum = read_datum(path)
    require(isinstance(datum, dict), 'the committed review is malformed')
    if datum.get('verdict') == 'APPROVED':
        return None  # Only the unchanged strict trio gate can approve.
    require(datum.get('verdict') == 'REQUEST_CHANGES',
            'the committed review has an invalid verdict')
    require(tip != receipt['head'] and
            blob != blob_or_none(repo, receipt['head'], relative),
            'the rejected review is not a published evidence tail')
    identity = {'stage': 'implementation-review', 'plan-id': plan,
                'wave': f'W{wave}', 'generation': generation,
                'head': receipt['head'], 'reviewed-sha': receipt['head'],
                'branch': receipt['branch'], 'attempt-id': receipt['attempt-id'],
                'attempt-fence': receipt['attempt-fence']}
    require(all(datum.get(key) == value and type(datum.get(key)) is type(value)
                for key, value in identity.items()),
            'the committed rejection does not match the exact receipt/generation')
    require(isinstance(datum.get('timestamp'), str) and
            re.fullmatch(r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z', datum['timestamp']),
            'the committed rejection timestamp is malformed')
    request = None
    if 'review-mode' in datum:
        require(datum['review-mode'] == OWNED_AUDIT_MODE, 'unknown committed review mode')
        ref = receipt.get('owned-proof')
        require(isinstance(ref, dict) and datum.get('owned-proof') == ref,
                'recovered rejection has stale owned execution proof')
        require(isinstance(authority, dict),
                'recovered owned rejection requires fresh authenticated authority')
        require(type(authority.get('generation')) is type(generation) and
                authority['generation'] == generation and
                _exact_audit_binding(authority.get('receipt'), receipt) and
                authority.get('reference') == ref,
                'recovered rejection has stale owned receipt/generation/proof')
        request = dict(receipt, **{'plan-id': plan, 'wave': wave, 'generation': generation,
                                  'proof-sha256': ref['sha256'], 'review-mode': OWNED_AUDIT_MODE,
                                  'content-digest': digest(repo, 'refs/remotes/origin/main', receipt['head'])})
        request['evidence-revision'] = authority.get('evidence-revision', 0)
        if request['evidence-revision']:
            request['evidence-registration-sha256'] = authority['evidence-registration-sha256']
        if 'supplemental-promotion-sha256' in authority:
            request['supplemental-promotion-sha256'] = authority['supplemental-promotion-sha256']
        request['owned-authority'] = authority
        red = authority.get('red')
        require(datum.get('owned-red') ==
                (red.get('reference') if isinstance(red, dict) else None),
                'recovered rejection has stale owned behavioral RED proof')
        require(_exact_audit_binding(datum.get('audit-binding'), _audit_binding(request)),
                'recovered rejection has stale audit binding')
    try:
        output = validate_reviewer_output(dict(datum, kind='request-changes'), request)
    except _ReviewInfra as error:
        raise Pending('the committed rejection is malformed: %s' % error) from error
    return {'status': 'review-rejected', 'stage': 'implementation-review',
            'plan-id': plan, 'wave': wave, 'generation': generation,
            'head': receipt['head'], 'branch': receipt['branch'],
            'attempt-id': receipt['attempt-id'], 'attempt-fence': receipt['attempt-fence'],
            'postmerge': False, 'findings': output['findings'], 'tip': tip}


def _verify_rejected_artifact(repo, tip, relative, expected_head, findings):
    """SEPARATE structural rejection validation (the strict APPROVED gate is
    never weakened to carry a rejection verdict through)."""
    blob = blob_or_none(repo, tip, relative)
    require(blob is not None, 'the rejected review artifact is absent at the published tip')
    with tempfile.TemporaryDirectory(prefix='q-rejected-') as temp:
        path = Path(temp) / 'rejected.rktd'
        path.write_bytes(blob)
        datum = read_datum(path)
    require(isinstance(datum, dict) and datum.get('verdict') == 'REQUEST_CHANGES' and
            datum.get('reviewed-sha') == expected_head and
            datum.get('head') == expected_head and datum.get('findings') == findings,
            'the committed rejection failed its structural validation')


def _finalize_implementation(repo, plan, wave, expected_head, expected_branch,
                             campaign_root, generation, staging_root, reviewer_output,
                             manual_compatibility=False):
    staging = implementation_review_staging(campaign_root, plan, wave, generation,
                                            staging_root)
    request = _load_dispatch_request(staging, plan, wave, generation, expected_head,
                                     expected_branch, 'implementation-review')
    receipt = _receipt_for_review(repo, plan, wave, campaign_root,
                                  expected_head, expected_branch)
    require(all(request.get(key) == receipt.get(key)
                for key in ('head', 'branch', 'attempt-id', 'attempt-fence')),
            'the dispatch identity does not match the exact durable receipt; rerun prepare')
    facts = review_tracker_facts(repo, campaign_root, plan, wave)
    milestone, issue = facts['milestone'], facts['issue']
    require(all(type(request.get(key)) is int and request[key] == facts[key]
                for key in ('milestone', 'issue')),
            'the tracker binding changed after review dispatch; rerun prepare')
    paths = active_declared_outputs(facts, repo, plan, wave, generation)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    git(repo, 'fetch', '--no-tags', 'origin', 'refs/heads/' + expected_branch)
    tip = git(repo, 'rev-parse', 'FETCH_HEAD').strip()
    require(full_sha(tip), 'malformed fetched receipt branch tip')
    require_receipt_tip(repo, expected_head, tip, 'review')
    if tip != request.get('tip'):
        raise _ReviewInfra('the receipt branch moved after the review dispatch was '
                           'prepared; rerun prepare')
    authority = _reauthenticate_owned_request(request, campaign_root, plan, wave, staging,
                                              manual_compatibility)
    output = load_reviewer_output(reviewer_output, request)
    if output['kind'] == 'request-changes':
        rejected = {'stage': 'implementation-review', 'plan-id': plan, 'wave': f'W{wave}',
                    'verdict': 'REQUEST_CHANGES', 'reviewer': output['reviewer'],
                    'timestamp': _iso_now(), 'reviewed-sha': expected_head,
                    'head': expected_head, 'branch': expected_branch,
                    'attempt-id': receipt['attempt-id'],
                    'attempt-fence': receipt['attempt-fence'], 'generation': generation,
                    'findings': output['findings'], 'scope': output['scope'],
                    'report': output['report']}
        if authority is not None:
            rejected.update({'review-mode': OWNED_AUDIT_MODE,
                             'owned-proof': authority['reference'],
                             'audit-binding': _audit_binding(request), 'audit-ack': output['audit-ack']})
            if isinstance(authority.get('red'), dict):
                rejected['owned-red'] = authority['red']['reference']
        candidate = _commit_candidate(repo, tip, {paths['review']: rejected},
                                      f'W{wave} review rejected: {plan}-w{wave}',
                                      _owned_publication_files(staging, authority))
        _require_review_evidence_tail(repo, expected_head, candidate)
        _verify_rejected_artifact(repo, candidate, paths['review'], expected_head,
                                  output['findings'])
        _recover_rejected_artifact(repo, candidate, paths['review'], plan, wave,
                                   generation, receipt, authority)
        _reauthenticate_owned_request(request, campaign_root, plan, wave, staging,
                                      manual_compatibility)
        if authority is None:
            tip_now = _publish_candidate(repo, expected_branch, tip, candidate)
        else:
            tip_now = _publish_candidate(
                repo, expected_branch, tip, candidate,
                lambda: _reauthenticate_owned_request(request, campaign_root, plan, wave, staging, False))
        return _recover_rejected_artifact(repo, tip_now, paths['review'], plan, wave,
                                          generation, receipt, authority)
    # approved: the controller binds every digest and head; the reviewer's
    # prose never becomes authority. Only the strict gate below can produce
    # the reviewed verdict.
    content = digest(repo, main, tip)
    names = policy_names(repo, main)
    common = {}
    if authority is not None:
        common = {'review-mode': OWNED_AUDIT_MODE,
                  'owned-proof': authority['reference'],
                  'audit-binding': _audit_binding(request)}
        if isinstance(authority.get('red'), dict):
            common['owned-red'] = authority['red']['reference']
    evidence = dict(common, **{'schema-version': 2, 'plan-id': plan, 'wave': f'W{wave}',
                               'milestone': milestone, 'issue': issue,
                               'status': 'ready-for-merge',
                               'implementation-sha': expected_head,
                               'content-digest': content,
                               'branch': expected_branch, 'required-checks': list(names),
                               'required-pr-checks': list(names),
                               'review-artifact': paths['review'],
                               'validation-artifact': paths['validation']})
    review_datum = dict(common, **{'reviewer': output['reviewer'], 'verdict': 'APPROVED',
                                   'timestamp': _iso_now(), 'reviewed-sha': expected_head,
                                   'content-digest': content, 'scope': output['scope'],
                                   'report': output['report']})
    if authority is not None:
        review_datum['audit-ack'] = output['audit-ack']
    gates = _owned_approval_gates(output.get('validation'), authority)
    validation = dict(common, **{'status': 'current', 'planning-sync': 'current',
                                 'milestone': milestone, 'wave': f'W{wave}', 'issue': issue,
                                 'branch': expected_branch,
                                 'implementation-sha': expected_head,
                                 'content-digest': content, 'red-first': gates['red-first'],
                                 'focused-tests': gates['focused-tests'],
                                 'format-compile': gates['format-compile'],
                                 'lint': gates['lint'],
                                 'fast': gates['fast'],
                                 'review-artifact': paths['review'],
                                 'remaining-items': gates['remaining-items']})
    candidate = _commit_candidate(repo, tip,
                                   {paths['evidence']: evidence,
                                    paths['review']: review_datum,
                                    paths['validation']: validation},
                                   f'W{wave} independent review: {plan}-w{wave}',
                                   _owned_publication_files(staging, authority))
    try:
        _require_review_evidence_tail(repo, expected_head, candidate)
        validate_trio(repo, candidate, paths['evidence'], main,
                      owned_proof_authority=authority)
    except Pending as error:
        raise _ReviewInfra('the local candidate failed the unchanged strict gate: %s'
                           % error) from error
    new_tip = _publish_candidate(
        repo, expected_branch, tip, candidate,
        None if authority is None else
        (lambda: _reauthenticate_owned_request(request, campaign_root, plan, wave,
                                               staging, manual_compatibility)))
    _require_review_evidence_tail(repo, expected_head, new_tip)
    validate_trio(repo, new_tip, paths['evidence'], main, owned_proof_authority=authority)
    return {'status': 'reviewed', 'plan-id': plan, 'wave': wave, 'head': expected_head,
            'reviewed-sha': expected_head, 'review-artifact': paths['review'],
            'tip': new_tip}


def _finalize_binding(repo, plan, wave, expected_head, expected_branch,
                      campaign_root, generation, staging_root, reviewer_output,
                      manual_compatibility=False):
    staging = require_binding_output(staging_root, campaign_root, plan, wave, generation)
    staged = binding_review(repo, plan, wave, staging, campaign_root, expected_branch)
    if staged['status'] == 'reviewed':
        if not manual_compatibility:
            raise _ReviewInfra('existing manual binding approval is not production audit approval')
        return dict(staged, stage='binding-review', head=expected_head,
                    branch=expected_branch, postmerge=True)
    require(staged['status'] == 'pending-review',
            'the binding staging is %s; only a pending-review draft can be finalized'
            % staged['status'])
    request = _load_dispatch_request(staging, plan, wave, generation, expected_head,
                                     expected_branch, 'binding-review')
    receipt = _receipt_for_review(repo, plan, wave, campaign_root,
                                  expected_head, expected_branch)
    authority = _reauthenticate_owned_request(request, campaign_root, plan, wave, staging,
                                              manual_compatibility)
    output = load_reviewer_output(reviewer_output, request)
    if authority is not None and staged['merge-sha'] != expected_head:
        raise _ReviewInfra('unsupported-postmerge-owned-proof-relation')
    artifact = Path(staged['binding']).name
    review_rel = f'docs/reports/gsd-wave-reviews/{artifact}'
    evidence_path = staging / 'docs/reports/gsd-wave-evidence' / artifact
    review_path = staging / 'docs/reports/gsd-wave-reviews' / artifact
    validation_path = staging / 'docs/reports/gsd-wave-validation' / artifact
    evidence = read_datum(evidence_path)
    if output['kind'] == 'request-changes':
        rejected = {'stage': 'binding-review', 'plan-id': plan, 'wave': f'W{wave}',
                    'verdict': 'REQUEST_CHANGES', 'reviewer': output['reviewer'],
                    'timestamp': _iso_now(), 'reviewed-sha': staged['merge-sha'],
                    'head': expected_head, 'branch': staged['branch'],
                    'findings': output['findings'], 'scope': output['scope'],
                    'report': output['report']}
        write_rktd(rejected, staging / 'review-rejected.rktd')
        return {'status': 'review-rejected', 'stage': 'binding-review',
                'plan-id': plan, 'wave': wave, 'head': expected_head,
                'branch': expected_branch, 'attempt-id': receipt['attempt-id'],
                'attempt-fence': receipt['attempt-fence'], 'postmerge': True,
                'findings': output['findings']}
    gates = _owned_approval_gates(output.get('validation'), authority)
    common = {}
    if authority is not None:
        # Postmerge binding approval keeps the SAME audit provenance as the
        # implementation review: mode, proof reference, binding and ack are
        # published and revalidated by the strict audit gate (the staged
        # trio is validated with the discovered staging bundle below), so a
        # binding trio can never look legacy while an owned proof exists.
        common = {'review-mode': OWNED_AUDIT_MODE,
                  'owned-proof': authority['reference'],
                  'audit-binding': authority['audit-binding']}
        if isinstance(authority.get('red'), dict):
            common['owned-red'] = authority['red']['reference']
    evidence = dict(evidence, status='ready-for-merge', **common,
                    **{'content-digest': EMPTY_SHA})
    review_datum = dict(common, **{'reviewer': output['reviewer'], 'verdict': 'APPROVED',
                                   'timestamp': _iso_now(), 'reviewed-sha': staged['merge-sha'],
                                   'content-digest': EMPTY_SHA, 'scope': output['scope'],
                                   'report': output['report']})
    if authority is not None:
        review_datum['audit-ack'] = output['audit-ack']
    validation = dict(common, **{'status': 'current', 'planning-sync': 'current',
                                 'milestone': evidence['milestone'], 'wave': evidence['wave'],
                                 'issue': evidence['issue'], 'branch': staged['branch'],
                                 'implementation-sha': staged['merge-sha'],
                                 'content-digest': EMPTY_SHA, 'red-first': gates['red-first'],
                                 'focused-tests': gates['focused-tests'],
                                 'format-compile': gates['format-compile'], 'lint': gates['lint'],
                                 'fast': gates['fast'], 'review-artifact': review_rel,
                                 'remaining-items': gates['remaining-items']})
    for path, datum in ((evidence_path, evidence), (review_path, review_datum),
                        (validation_path, validation)):
        path.unlink()
        write_rktd(datum, path)
    if authority is not None:
        _owned_publication_files(staging, authority)
    result = binding_review(repo, plan, wave, staging, campaign_root, expected_branch)
    require(result['status'] == 'reviewed',
            'the finalized staged trio failed the strict binding gate: %s'
            % result.get('status'))
    return dict(result, stage='binding-review', head=expected_head,
                branch=expected_branch, postmerge=True)


def auto_review(repo, plan, wave, phase, stage, expected_head, expected_branch,
                campaign_root, generation, staging_root, reviewer_output=None,
                *, manual_compatibility=False):
    """The auto-review port (see the block comment above). `prepare` decides
    dispatchability through the unchanged validation paths; `finalize` binds
    digests, publishes evidence-only with a pinned non-force push and lets
    only the strict gates produce a reviewed verdict."""
    require(isinstance(plan, str) and re.fullmatch('[0-9a-f]{64}', plan),
            'invalid campaign id')
    require(type(wave) is int and wave >= 0, 'invalid wave index')
    require(phase in ('prepare', 'finalize'), 'invalid auto-review phase')
    require(stage in AUTO_REVIEW_STAGES, 'invalid auto-review stage')
    require(isinstance(expected_head, str) and full_sha(expected_head),
            'auto-review requires the durable receipt head')
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'auto-review requires the durable receipt branch')
    require(campaign_root is not None, 'auto-review requires --campaign-root')
    require(type(generation) is int and generation >= 0, 'invalid binding generation')
    require(staging_root is not None, 'auto-review requires --staging-root')
    require(generation == journal_generation(repo, plan, wave, campaign_root),
            'the review binding generation does not match the authoritative journal')
    try:
        if phase == 'prepare':
            return auto_review_prepare(repo, plan, wave, stage, expected_head,
                                       expected_branch, campaign_root, generation,
                                       staging_root, manual_compatibility)
        require(reviewer_output is not None, 'finalize requires --reviewer-output')
        if stage == 'binding-review':
            return _finalize_binding(repo, plan, wave, expected_head, expected_branch,
                                     campaign_root, generation, staging_root,
                                     reviewer_output, manual_compatibility)
        return _finalize_implementation(repo, plan, wave, expected_head, expected_branch,
                                        campaign_root, generation, staging_root,
                                        reviewer_output, manual_compatibility)
    except _ReviewInfra as error:
        return {'status': 'review-infra', 'reason': str(error)}


def governance(repo, plan, wave, expected_branch):
    """Validate the merged binding publication and its protected-main governance run."""
    require(isinstance(expected_branch, str) and expected_branch.strip(),
            'governance requires --expected-branch')
    # The publication under validation may belong to ANY deterministic
    # generation of the binding branch name (base or -r<n> republication):
    # a repair tail does not invalidate an already-published binding for an
    # unchanged implementation (the rebind refusal keeps that binding
    # authoritative), so governance must accept the generation actually
    # published while every downstream fact below is still validated in
    # full (exactly one merged PR, fresh-main single-parent publication,
    # trusted protected checks).
    base = binding_branch(plan, wave)
    require(expected_branch == base or
            re.fullmatch(re.escape(base) + r'-r[1-9][0-9]*', expected_branch),
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
                                           'governance','preflight','publish-head','auto-review'])
    parser.add_argument('--repo', type=Path, required=True)
    parser.add_argument('--plan')
    parser.add_argument('--wave', type=int)
    parser.add_argument('--pr', type=int)
    parser.add_argument('--evidence')
    parser.add_argument('--expected-branch',
                        help='PR head branch / sync branch; never switches branches')
    parser.add_argument('--expected-head',
                        help='exact verified implementation head (from the durable receipt)')
    parser.add_argument('--expected-origin',
                        help='exact configured origin URL captured in the approved receipt')
    parser.add_argument('--expected-tree',
                        help='exact commit tree captured in the approved receipt')
    parser.add_argument('--output', type=Path)
    parser.add_argument('--campaign-root', type=Path,
                        help='campaign working tree holding .planning/campaigns/<plan-id>/plan-snapshot')
    parser.add_argument('--generation', type=int,
                        help='authoritative binding generation expected by the coordinator')
    parser.add_argument('--phase', choices=['prepare','finalize'],
                        help='auto-review phase: prepare the gate-red template and typed '
                             'request, or finalize a fresh reviewer result through the '
                             'unchanged strict gates')
    parser.add_argument('--stage', choices=['implementation-review','binding-review'],
                        help='the delivery stage the auto-review dispatch serves')
    parser.add_argument('--staging-root', type=Path,
                        help='deterministic campaign-local review staging directory')
    parser.add_argument('--manual-review-compatibility', action='store_true',
                        help='explicit legacy/manual fixtures only; never production audit fallback')
    parser.add_argument('--reviewer-output', type=Path,
                        help='the fresh reviewer session typed result (coordinator-written)')
    args = parser.parse_args()
    try:
        if args.action == 'sync':
            result = sync(args.repo, args.expected_branch)
        elif args.action == 'status':
            require(args.campaign_root, 'status requires --campaign-root')
            result = status(args.repo, args.plan, args.wave, args.campaign_root)
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
                            args.expected_branch, args.campaign_root, args.generation)
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
        elif args.action == 'publish-head':
            require(args.expected_head and args.expected_branch and args.expected_origin and args.expected_tree,
                    'publish-head requires --expected-head, --expected-branch, --expected-origin and --expected-tree')
            result = publish_head(args.repo, args.expected_branch, args.expected_head,
                                  args.expected_origin, args.expected_tree, args.plan, args.wave)
        elif args.action == 'auto-review':
            require(args.phase and args.stage and args.expected_head and args.expected_branch and
                    args.campaign_root is not None and args.generation is not None and
                    args.staging_root is not None,
                    'auto-review requires --phase, --stage, --expected-head, --expected-branch, '
                    '--campaign-root, --generation and --staging-root')
            result = auto_review(args.repo, args.plan, args.wave, args.phase, args.stage,
                                 args.expected_head, args.expected_branch,
                                 args.campaign_root, args.generation, args.staging_root,
                                 args.reviewer_output,
                                 manual_compatibility=args.manual_review_compatibility)
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
                             args.campaign_root, args.output, args.expected_branch,
                             args.expected_head)
        print(json.dumps(result))
        return 0
    except (Pending, ValueError, KeyError, TypeError, OSError, AttributeError) as error:
        print(json.dumps({'status':'delivery-pending', 'reason':str(error)}))
        return 2

if __name__ == '__main__':
    sys.exit(main())
