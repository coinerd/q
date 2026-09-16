#!/usr/bin/env python3
"""Coordinator-owned delivery preparation/readback. Never merges or approves PRs.

status: exact protected-main proof and current-checkout readback (JSON).
prepare: produce a deliberately gate-red schema-2 binding trio in a new directory.
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
ACTIONS_APP_ID = 15368
RUNTIME_CALLER_BUDGET = 240.0  # delivery-handoff run-subprocess timeout (seconds)

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
        text = stderr.decode('utf-8', errors='replace')
        if _AUTH_ERROR.search(text):
            # Explicit reason, never the command output: stderr could echo
            # credential-bearing URLs and credentials are never exposed here.
            raise Pending('GitHub authentication failed (check: gh auth status); '
                          'credentials are never logged or exposed')
        # git/gh/racket errors may contain credential-bearing URLs or remote
        # text. Never serialize raw subprocess output into durable handoffs.
        raise Pending('delivery command failed (%s, exit %s); inspect authentication, '
                      'repository refs and required artifacts locally'
                      % (Path(args[0]).name, result.returncode))
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
    """Run the existing, unchanged strict gate on committed blobs and real diff."""
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
        return evidence, [relative, review, validation]

def validate_merge(pr, commit, slug, merge, head):
    require(isinstance(pr, dict) and isinstance(commit, dict), 'malformed PR/commit response')
    require(pr.get('merged') is True, 'implementation PR is not merged')
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

def independent_approval(reviews, author, head):
    """One genuine independent human approval at the EXACT head: a review by a
    non-bot reviewer (not the author) on that commit with no later
    CHANGES_REQUESTED sweep. Reviewer unavailability is a typed awaiting-review
    condition, never implicit approval."""
    require(isinstance(reviews, list), 'malformed pull-request reviews response')
    on_head = [r for r in reviews if isinstance(r, dict) and
               r.get('commit_id') == head and
               dig(r, 'user', 'type') == 'User' and
               dig(r, 'user', 'login') != author and
               r.get('state') in ('APPROVED', 'CHANGES_REQUESTED')]
    if not on_head:
        return None
    latest = max(on_head, key=lambda r: r.get('submitted_at') or '')
    if latest.get('state') != 'APPROVED':
        return None
    return latest

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

def merge(repo, plan, wave, number, expected_head, expected_branch, source):
    """Deterministic protected squash merge of the implementation PR.

    Every gate runs BEFORE any mutation; `expected_head` comes from the durable
    verified receipt, never from model output. Order:
      1. same-repo main-base identity
      2. exact expected head (both the PR claim and the actually-fetched refs/pull/N/head)
      3. already-merged at the expected head -> idempotent, no second merge
      4. fresh main (PR based on current origin/main)
      5. genuine independent human approval at the exact head (else awaiting-review)
      6. every required check green at that exact head (policy snapshot + protection)
      7. unchanged strict trio/digest preflight at PR base...head
      8. squash-only PUT to the PR merge endpoint (never admin, never main push)
      9. post-merge single-parent squash provenance via validate_merge
    Returns {'status':'merged'|'already-merged'|'awaiting-review', ...}; model
    or journal completion alone never sets delivery."""
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
    if pr.get('merged') is True:
        merge_sha = dig(pr, 'merge_commit_sha')
        validate_merge(pr, api(slug, f'commits/{merge_sha}'), slug, merge_sha, head)
        return {'status': 'already-merged', 'merge-sha': merge_sha, 'plan-id': plan,
                'wave': wave, 'head': head, 'pr': number}
    require(pr.get('state') == 'open', 'implementation PR is neither open nor already merged')
    require(dig(pr, 'base', 'sha') == main,
            'implementation PR is not based on fresh origin/main; reanchor and re-verify')
    fetched = fetch_head(repo, number)
    require(fetched == head, 'fetched implementation head does not match expected head')
    if independent_approval(api(slug, f'pulls/{number}/reviews'),
                            dig(pr, 'user', 'login') or '', head) is None:
        return {'status': 'awaiting-review', 'plan-id': plan, 'wave': wave, 'head': head,
                'pr': number,
                'reason': 'no genuine independent human approval at the exact expected head'}
    names = policy_names(repo, main)
    protection(slug, names)
    for name in names:
        trusted_check(slug, head, name, 'pull_request', expected_branch)
    validate_trio(repo, head, source, dig(pr, 'base', 'sha'))
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

def resolve_existing_pr(slug, branch):
    """Resolve-before-create: at most one open PR may already target the
    exact head branch. Returns it, or None when none exists; multiple open
    PRs for one branch fail closed (never silently pick one to reuse)."""
    require(isinstance(branch, str) and branch.strip(), 'invalid branch identity')
    opened = api(slug, f'pulls?state=open&head={slug}:{branch}')
    require(isinstance(opened, list), 'malformed open-PR response')
    require(len(opened) <= 1,
            'multiple open pull requests already target branch ' + branch)
    return opened[0] if opened else None

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
    out = git(repo, 'diff-tree', '--no-commit-id', '--name-only', '-r', publication,
              '--', 'docs/reports/gsd-wave-evidence/')
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
        merged = (isinstance(number, int) and
                  api(slug, f'pulls/{number}').get('merged') is True)
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
    evidence, paths = validate_trio(repo, publication, relative, parent)
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

def prepare(repo, plan, wave, number, relative, campaign_root, output):
    binding = binding_path(plan, wave)
    slug = repository(repo)
    refresh(repo)
    main = git(repo, 'rev-parse', 'refs/remotes/origin/main').strip()
    require(full_sha(main), 'malformed origin/main head')
    pr = api(slug, f'pulls/{number}')
    require(isinstance(pr, dict), 'malformed PR response')
    merge, head = dig(pr, 'merge_commit_sha'), dig(pr, 'head', 'sha')
    require(pr.get('merged') is True, 'implementation PR must be independently reviewed and merged first')
    validate_merge(pr, api(slug, f'commits/{merge}'), slug, merge, head)
    fetched = fetch_head(repo, number)
    require(fetched == head, 'fetched implementation head does not match PR head SHA')
    require(tree_of(repo, merge) == tree_of(repo, fetched),
            'implementation merge tree differs from fetched PR head tree')
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
    require(not git(repo, 'status', '--porcelain').strip(), 'dirty checkout; refusing synchronization')
    refresh(repo)
    git(repo, 'merge', '--ff-only', 'origin/main')
    return {'status':'synchronized', 'head':git(repo, 'rev-parse', 'HEAD').strip(),
            'branch':current}

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=['status','prepare','sync','merge'])
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
        else:
            require(args.pr and args.evidence and args.output and args.campaign_root,
                    'prepare requires --pr, --evidence, --output and --campaign-root')
            result = prepare(args.repo, args.plan, args.wave, args.pr, args.evidence,
                             args.campaign_root, args.output)
        print(json.dumps(result))
        return 0
    except (Pending, ValueError, KeyError, TypeError, OSError, AttributeError) as error:
        print(json.dumps({'status':'delivery-pending', 'reason':str(error)}))
        return 2

if __name__ == '__main__':
    sys.exit(main())
