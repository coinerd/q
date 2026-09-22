"""Fail-closed delivery controller regressions; no network.

Covers the remediation contract: frozen-snapshot source provenance, rebind
refusal, exact-PR-head fetch, implementation-head required checks, publication
green-skip rule-out, protection name snapshot, budget bounding, per-process
API caching, auth-error surfacing, malformed-nullable safety, and
sync dirty/detached/expected-branch refusal — using real git/temp fixtures
and a fake GitHub API.
"""
import contextlib
import hashlib
import importlib.util
import io
import json
import pathlib
import subprocess
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = pathlib.Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('delivery', ROOT / 'scripts/gsd-delivery.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

POLICY_NAMES = ['lint', 'test (0)', 'workflows (0)']
EMPTY_SHA = hashlib.sha256(b'').hexdigest()
SLUG = 'owner/repo'
BRANCH = 'campaign/v9.9.9-w1'
WAVE_BRANCH = 'campaign/v9.9.9-w1'


def sh(*args, cwd=None):
    result = subprocess.run([str(a) for a in args], cwd=str(cwd) if cwd is not None else None,
                            capture_output=True, text=True)
    assert result.returncode == 0, (args, result.stderr)
    return result.stdout


def init_repo(path):
    path = Path(path)
    sh('git', 'init', '-b', 'main', path)
    sh('git', 'config', 'user.email', 'fixture@example.com', cwd=path)
    sh('git', 'config', 'user.name', 'fixture', cwd=path)
    return path


def write_file(path, text):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)
    return path


def hexpairs(pairs):
    return '#hasheq(' + ' '.join(f'({k} . {v})' for k, v in pairs) + ')'


def trio(label, wave, *, impl_sha, digest, milestone=895, issue=9686,
         branch=WAVE_BRANCH, plan_id=None, merge=None, head=None, pr=None,
         review_verdict='APPROVED', evidence_branch=None, wave_branch=None,
         merge_auth=True, reviewed_sha=None, reviewer='wave-reviewer'):
    review = f'docs/reports/gsd-wave-reviews/{label}.rktd'
    validation = f'docs/reports/gsd-wave-validation/{label}.rktd'
    wave_label = f'W{wave}'
    evidence_pairs = [
        ('schema-version', 2), ('milestone', milestone), ('wave', f'"{wave_label}"'),
        ('issue', issue), ('status', '"ready-for-merge"'),
        ('implementation-sha', f'"{impl_sha}"'), ('content-digest', f'"{digest}"'),
        ('required-checks', '("' + '" "'.join(POLICY_NAMES) + '")'),
        ('required-pr-checks', '("' + '" "'.join(POLICY_NAMES) + '")'),
        ('review-artifact', f'"{review}"'), ('validation-artifact', f'"{validation}"'),
    ]
    if merge_auth is True:
        # Amended approval contract (F11): the durable operator authorization
        # travels in the committed evidence record and names the exact verified
        # implementation head (the receipt head) it authorizes for merge.
        evidence_pairs.append(('merge-authorization', hexpairs([
            ('operator', '"coinerd"'), ('wave', f'"{wave_label}"'),
            ('head', f'"{impl_sha}"'),
            ('action', '"squash-merge the reviewed wave implementation PR"'),
            ('source', '"operator directive in the campaign delivery session per the '
                       'amended approval contract (PLAN-v1.00.31, Approval contract)"')])))
    elif isinstance(merge_auth, dict):
        evidence_pairs.append(
            ('merge-authorization',
             hexpairs([(k, v if v.startswith('"') else f'"{v}"')
                       for k, v in merge_auth.items()])))
    if plan_id is not None:
        evidence_pairs.append(('plan-id', f'"{plan_id}"'))
    if evidence_branch is not None:
        evidence_pairs.append(('branch', f'"{evidence_branch}"'))
    if merge is not None:
        evidence_pairs += [('merge-sha', f'"{merge}"'), ('merge-method', '"squash"')]
    if pr is not None:
        evidence_pairs += [('delivery-pr', pr), ('merged-at', '"2026-09-14T16:23:06Z"')]
    if head is not None:
        evidence_pairs += [('delivery-head-sha', f'"{head}"'),
                           ('wave-branch', f'"{wave_branch or branch}"')]
    review_pairs = [
        ('reviewer', f'"{reviewer}"'), ('verdict', f'"{review_verdict}"'),
        ('timestamp', '"2026-09-14T16:00:00Z"'),
        ('reviewed-sha', f'"{reviewed_sha if reviewed_sha is not None else impl_sha}"'),
        ('content-digest', f'"{digest}"'),
        ('scope', '"independent read-only review of the wave implementation, '
                  'gate evidence and provenance chain"'),
        ('report', '"reviewed the commit chain, ran the focused and fast gates, and '
                   'found no blocking defect at the reviewed head"'),
    ]
    passed = '#hasheq((result . "passed") (command . "x"))'
    validation_pairs = [
        ('status', '"current"'), ('milestone', milestone), ('wave', f'"{wave_label}"'),
        ('issue', issue), ('branch', f'"{branch}"'),
        ('implementation-sha', f'"{impl_sha}"'), ('content-digest', f'"{digest}"'),
        ('red-first', '#hasheq((command . "raco test tests/test-wave-fixture.rkt") '
                      '(failure . "reproduction observed: the malformed digest passed the gate"))'),
        ('focused-tests', passed), ('format-compile', passed), ('lint', passed), ('fast', passed),
        ('review-artifact', f'"{review}"'), ('remaining-items', '()'),
        ('planning-sync', '"current"'),
    ]
    files = {
        f'docs/reports/gsd-wave-evidence/{label}.rktd': hexpairs(evidence_pairs),
        f'docs/reports/gsd-wave-reviews/{label}.rktd': hexpairs(review_pairs),
        f'docs/reports/gsd-wave-validation/{label}.rktd': hexpairs(validation_pairs),
    }
    return files


def check_run(name, sha, run=777, job=1, conclusion='success', status='completed',
              app=15368, head=None, slug=SLUG):
    return {'name': name, 'status': status, 'conclusion': conclusion,
            'head_sha': head or sha,
            'app': {'id': app},
            'details_url': f'https://github.com/{slug}/actions/runs/{run}/job/{job}'}


def run_details(sha, event, branch='main', status='completed', conclusion='success'):
    return {'head_sha': sha, 'event': event, 'status': status, 'conclusion': conclusion,
            'repository': {'full_name': SLUG}, 'path': '.github/workflows/ci.yml',
            'head_branch': branch}


DECLARED_DIRS = {'evidence': 'gsd-wave-evidence',
                 'review': 'gsd-wave-reviews',
                 'validation': 'gsd-wave-validation'}


def protection(contexts):
    return {'enforce_admins': {'enabled': True},
            'required_status_checks':
                {'strict': True,
                 'contexts': None if contexts is None else list(contexts)},
            'allow_force_pushes': {'enabled': False},
            'allow_deletions': {'enabled': False}}


def binding_staging(base, plan, wave):
    return Path(base) / '.planning' / 'campaigns' / plan / f'binding-w{wave}'


def finalize_binding(base, w, output):
    branch = m.binding_branch(w['plan'], w['wave'])
    files = trio(f"{w['plan']}-w{w['wave']}", w['wave'],
                 impl_sha=w['merge'], digest=m.EMPTY_SHA,
                 plan_id=w['plan'], merge=w['merge'], head=w['head'],
                 pr=w['pr'], branch=branch, evidence_branch=branch,
                 wave_branch=WAVE_BRANCH)
    for relative, text in files.items():
        target = output / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text)


class FakeAPI:
    def __init__(self, routes):
        self.routes = routes
        self.calls = []

    def __call__(self, slug, route, paginate=False, _refresh=False):
        self.calls.append((slug, route, paginate, _refresh))
        key = (slug, route, paginate)
        assert key in self.routes, 'unexpected api route: %r' % (key,)
        value = self.routes[key]
        return value(slug, route, paginate, _refresh) if callable(value) else value


def build_campaign(base, plan, wave, *, declared=True, q_declared=False,
                   issue=9686, milestone=895, valid=True):
    root = Path(base) / 'campaign'
    planning = root / '.planning'
    plan_text = '# Fixture plan\n\nSingle wave: waves/W%d-fixture.md owns the delivery.\n' % wave
    file_lines = ''
    if declared:
        prefix = 'q/' if q_declared else ''
        label = f'v9.9.9-w{wave}'
        file_lines = ('\n## Files\n\n'
                      f'- File: `{prefix}docs/reports/gsd-wave-evidence/{label}.rktd`\n'
                      f'- File: `{prefix}docs/reports/gsd-wave-reviews/{label}.rktd`\n'
                      f'- File: `{prefix}docs/reports/gsd-wave-validation/{label}.rktd`\n')
    wave_text = ('# W%d: Fixture wave\n\nBranch: `%s`; GitHub issue '
                 '[#%d](https://github.com/owner/repo/issues/%d), milestone #%d.%s'
                 % (wave, BRANCH, issue, issue, milestone, file_lines))
    write_file(planning / 'PLAN.md', plan_text)
    write_file(planning / f'waves/W{wave}-fixture.md', wave_text)
    snapshot = planning / 'campaigns' / plan / 'plan-snapshot'
    entries = []
    for rel, text in (('PLAN.md', plan_text), (f'waves/W{wave}-fixture.md', wave_text)):
        data = text.encode()
        # plan-snapshot hashes the canonical form: string-split/join trims
        # leading/trailing empty lines, and these fixture texts carry no
        # status rows or generated prefixes that would normalize further.
        canonical = text.strip('\n').encode()
        digest = hashlib.sha256(canonical if valid else (canonical + b'tampered')).hexdigest()
        entries.append(f'#hasheq((path . "{rel}") (size . {len(data)}) (sha256 . "{digest}"))')
    write_file(snapshot / 'snapshot-manifest.rktd',
               '#hasheq((schema-version . 1) (campaign . "%s") (created-at . 1789000000.0) '
               '(plan-id . "%s") (files . (%s)))' % (plan, plan, ' '.join(entries)))
    (snapshot / 'waves').mkdir(parents=True, exist_ok=True)
    (snapshot / 'PLAN.md').write_bytes(plan_text.encode())
    (snapshot / f'waves/W{wave}-fixture.md').write_bytes(wave_text.encode())
    return root


def build_world(base, *, publish=True, plan='a' * 64, wave=1, pr=42, q_named=False,
                source='version', second_impl=False, pub_impl_sha=None,
                green_skip=False, source_plan_id=None, source_issue=None,
                mixed_record=False):
    """Real git fixture: c0 -> impl head h -> squash M on main [-> M2] [-> P].

    Pushes main plus refs/pull/<pr>/head to a local bare origin, then clones a
    subject checkout whose origin URL is a github.com URL rewritten via
    url.<local>.insteadOf so every fetch stays offline.
    """
    base = Path(base)
    origin_path = base / 'origin.git'
    sh('git', 'init', '--bare', '-b', 'main', origin_path)
    work = init_repo(base / 'work')
    write_file(work / 'scripts/required-pr-checks.policy',
               '("' + '" "'.join(POLICY_NAMES) + '")')
    write_file(work / 'src/file.txt', 'one\n')
    sh('git', 'add', '-A', cwd=work)
    sh('git', 'commit', '-m', 'c0', cwd=work)
    c0 = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    sh('git', 'checkout', '-b', 'impl', cwd=work)
    write_file(work / 'src/file.txt', 'two\n')
    sh('git', 'add', '-A', cwd=work)
    sh('git', 'commit', '-m', 'h1', cwd=work)
    h1 = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    label = f'v9.9.9-w{wave}'
    binding_label = f'{plan}-w{wave}'
    source_label = label if source == 'version' else binding_label
    digest = m.digest(work, c0, h1)
    kwargs = {}
    if source_plan_id is not None:
        kwargs['plan_id'] = source_plan_id
    if source_issue is not None:
        kwargs['issue'] = source_issue
    for rel, text in trio(source_label, wave, impl_sha=h1, digest=digest, **kwargs).items():
        write_file(work / rel, text)
    if mixed_record:
        # Register F4 incident shape: the record authored together with a
        # non-evidence path in the same commit.
        write_file(work / 'README.md', 'drifted alongside the record\n')
    sh('git', 'add', '-A', cwd=work)
    sh('git', 'commit', '-m', 'h2 source trio', cwd=work)
    head = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    sh('git', 'checkout', 'main', cwd=work)
    sh('git', 'merge', '--squash', 'impl', cwd=work)
    sh('git', 'commit', '-m', 'M squash', cwd=work)
    merge = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    merge2 = head2 = None
    if second_impl:
        sh('git', 'checkout', '-b', 'impl2', cwd=work)
        write_file(work / 'src/file.txt', 'three\n')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'h2b', cwd=work)
        head2 = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        sh('git', 'checkout', 'main', cwd=work)
        sh('git', 'merge', '--squash', 'impl2', cwd=work)
        sh('git', 'commit', '-m', 'M2 squash', cwd=work)
        merge2 = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    publication = None
    if publish:
        binding_branch = m.binding_branch(plan, wave)
        impl_for_binding = pub_impl_sha or merge
        pub_files = trio(label, wave, impl_sha=impl_for_binding, digest=EMPTY_SHA,
                         plan_id=plan, merge=merge, head=head, pr=pr,
                         evidence_branch=binding_branch, wave_branch=WAVE_BRANCH)
        # Binding PR changes: the hash-named evidence plus the version-named
        # review/validation artifacts rewritten for the binding content.
        write_file(work / f'docs/reports/gsd-wave-evidence/{binding_label}.rktd',
                   pub_files[f'docs/reports/gsd-wave-evidence/{label}.rktd'])
        for rel, text in pub_files.items():
            if '/gsd-wave-evidence/' not in rel:
                write_file(work / rel, text)
        if green_skip:
            write_file(work / f'docs/reports/gsd-wave-evidence/other-w{wave}.rktd',
                       '#hasheq((schema-version . 2))')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'P publication', cwd=work)
        publication = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    sh('git', 'push', '-q', origin_path, 'main:main', cwd=work)
    sh('git', 'push', '-q', origin_path, f'{head}:refs/pull/{pr}/head', cwd=work)
    if head2 is not None:
        sh('git', 'push', '-q', origin_path, f'{head2}:refs/pull/{pr + 1}/head', cwd=work)
    subject = base / ('q' if q_named else 'repo')
    sh('git', 'clone', '-q', '--branch', 'main', origin_path, subject)
    sh('git', 'remote', 'set-url', 'origin', f'https://github.com/{SLUG}.git', cwd=subject)
    sh('git', 'config', f'url.{origin_path}.insteadOf', f'https://github.com/{SLUG}.git',
       cwd=subject)
    sh('git', 'config', 'user.email', 'fixture@example.com', cwd=subject)
    sh('git', 'config', 'user.name', 'fixture', cwd=subject)
    return {'origin': origin_path, 'work': work, 'subject': subject,
            'c0': c0, 'head': head, 'merge': merge, 'merge2': merge2, 'head2': head2,
            'publication': publication, 'plan': plan, 'wave': wave, 'pr': pr,
            'label': label, 'binding': f'docs/reports/gsd-wave-evidence/{plan}-w{wave}.rktd',
            'source': f'docs/reports/gsd-wave-evidence/{source_label}.rktd'}


def pr_payload(world, merge=None, head=None, number=None, base=None):
    return {'number': number or world['pr'], 'merged': True,
            'merge_commit_sha': merge or world['merge'],
            'merged_at': '2026-09-14T16:23:06Z',
            'head': {'sha': head or world['head'], 'ref': WAVE_BRANCH,
                     'repo': {'full_name': SLUG}},
            'base': {'ref': 'main', 'sha': base or world['c0'],
                     'repo': {'full_name': SLUG}}}


def commit_payload(sha, parent):
    return {'sha': sha, 'parents': [{'sha': parent}],
            'commit': {'tree': {'sha': 't' * 40}}}


def status_routes(world, *, names=None, governance=True, contexts=None,
                  pr=None, pub_prs=None):
    names = POLICY_NAMES if names is None else names
    w = world
    routes = {
        (SLUG, f"pulls/{w['pr']}", False): pr or pr_payload(w),
        (SLUG, f"commits/{w['merge']}", False): commit_payload(w['merge'], w['c0']),
        (SLUG, f"commits/{w['publication']}/pulls", False):
            pub_prs if pub_prs is not None else
            [{'merged': True, 'merge_commit_sha': w['publication'],
              'head': {'repo': {'full_name': SLUG}},
              'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}],
        (SLUG, 'branches/main/protection', False):
            protection(contexts if contexts is not None else POLICY_NAMES),
        (SLUG, f"commits/{w['head']}/check-runs?per_page=100&page=1", False):
            {'check_runs': [check_run(name, w['head'], job=i)
                            for i, name in enumerate(names)],
             'total_count': len(names)},
        (SLUG, 'actions/runs/777', False):
            run_details(w['head'], 'pull_request', branch=WAVE_BRANCH),
    }
    if governance:
        routes[(SLUG, f"commits/{w['publication']}/check-runs?per_page=100&page=1", False)] = \
            {'check_runs': [check_run('gsd-governance', w['publication'], run=888)],
             'total_count': 1}
        routes[(SLUG, 'actions/runs/888', False)] = \
            run_details(w['publication'], 'push', branch='main')
    else:
        routes[(SLUG, f"commits/{w['publication']}/check-runs?per_page=100&page=1", False)] = \
            {'check_runs': [], 'total_count': 0}
    return routes


class DeliveryTests(unittest.TestCase):
    def setUp(self):
        m._API_CACHE.clear()
        m._CHECKS_CACHE.clear()
        m.PROCESS_BUDGET_SEC = 10 ** 6
        m._start = time.monotonic()
        self._tmp = tempfile.TemporaryDirectory(prefix='q-delivery-tests-')
        self.addCleanup(self._tmp.cleanup)
        self.base = Path(self._tmp.name)

    def world(self, **kwargs):
        return build_world(self.base / f'world-{len(list(self.base.iterdir()))}', **kwargs)

    @contextlib.contextmanager
    def fake_api(self, routes):
        fake = FakeAPI(routes)
        with patch.object(m, 'api', fake):
            yield fake

    # ------------------------------------------------------------------
    # Retained unit regressions
    # ------------------------------------------------------------------

    def test_identity_rejects_paths_and_other_waves(self):
        with self.assertRaises(m.Pending):
            m.binding_path('../other', 1)
        with self.assertRaises(m.Pending):
            m.binding_path('a' * 64, -1)
        self.assertEqual(m.binding_path('a' * 64, 1),
                         'docs/reports/gsd-wave-evidence/' + 'a' * 64 + '-w1.rktd')

    def test_single_parent_and_exact_pr_are_required(self):
        pr = dict(merged=True, merge_commit_sha='b' * 40,
                  head={'sha': 'c' * 40, 'repo': {'full_name': 'owner/repo'}},
                  base={'ref': 'main', 'repo': {'full_name': 'owner/repo'}})
        commit = {'sha': 'b' * 40, 'parents': [{'sha': 'd' * 40}]}
        m.validate_merge(pr, commit, 'owner/repo', 'b' * 40, 'c' * 40)
        for change in [dict(merged=False), dict(merge_commit_sha='e' * 40),
                       dict(base={'ref': 'elsewhere', 'repo': {'full_name': 'owner/repo'}})]:
            with self.assertRaises(m.Pending):
                m.validate_merge(dict(pr, **change), commit, 'owner/repo', 'b' * 40, 'c' * 40)
        with self.assertRaises(m.Pending):
            m.validate_merge(pr, dict(commit, parents=[{}, {}]), 'owner/repo', 'b' * 40, 'c' * 40)

    def test_artifact_paths_are_confined(self):
        for path in ['../review.rktd', '/tmp/review.rktd', 'docs/../../bad', 'docs/x.py']:
            with self.assertRaises(m.Pending):
                m.artifact_path(path, 'gsd-wave-reviews')
        self.assertEqual(m.artifact_path('docs/reports/gsd-wave-reviews/v1-w1.rktd',
                                         'gsd-wave-reviews'),
                         'docs/reports/gsd-wave-reviews/v1-w1.rktd')

    def test_command_errors_timeout_missing_binary_fail_closed(self):
        with self.assertRaises(m.Pending):
            m.command(['this-command-does-not-exist-839581'])
        with self.assertRaises(m.Pending):
            m.command(['python3', '-c', 'raise SystemExit(7)'])
        with self.assertRaises(m.Pending):
            m.command(['python3', '-c', 'import time; time.sleep(3)'], timeout=.01)
        self.assertEqual(m.command(['python3', '-c', 'print("ok")']).strip(), 'ok')

    def test_prepare_never_reuses_approval(self):
        source = {'wave': 'W1', 'milestone': 895, 'issue': 9687}
        request = m.draft_request(source, 'a' * 64, 1,
                                  {'merge_commit_sha': 'b' * 40, 'number': 42,
                                   'merged_at': '2026-01-01T00:00:00Z',
                                   'head': {'sha': 'c' * 40, 'ref': 'wave'}},
                                  'draft', POLICY_NAMES)
        self.assertEqual(request['status'], 'pending-review')
        self.assertEqual(request['merge-sha'], 'b' * 40)
        self.assertEqual(request['plan-id'], 'a' * 64)
        self.assertEqual(request['required-pr-checks'], POLICY_NAMES)
        self.assertNotIn('APPROVED', str(request))
        with self.assertRaises(m.Pending):
            m.draft_request(dict(source, wave='W0'), 'a' * 64, 1, {}, 'draft', POLICY_NAMES)
        with self.assertRaises(m.Pending):
            m.draft_request(source, 'a' * 64, 1,
                            {'merge_commit_sha': 'b' * 40, 'number': 1,
                             'merged_at': 'x', 'head': {'sha': 'c' * 40, 'ref': 'w'}},
                            'draft', [])

    # ------------------------------------------------------------------
    # Budget bounding (total under the 240 s runtime caller)
    # ------------------------------------------------------------------

    def test_default_budget_stays_under_runtime_caller(self):
        with patch.dict('os.environ', {}, clear=False):
            import os
            saved = os.environ.pop('GSD_DELIVERY_BUDGET_SEC', None)
            try:
                default = m._budget_setting()
            finally:
                if saved is not None:
                    os.environ['GSD_DELIVERY_BUDGET_SEC'] = saved
        self.assertLessEqual(default + 5, m.RUNTIME_CALLER_BUDGET)
        with patch.dict('os.environ', {'GSD_DELIVERY_BUDGET_SEC': '1000'}):
            self.assertEqual(m._budget_setting(), 200.0)
        with patch.dict('os.environ', {'GSD_DELIVERY_BUDGET_SEC': '5'}):
            self.assertEqual(m._budget_setting(), 200.0)
        with patch.dict('os.environ', {'GSD_DELIVERY_BUDGET_SEC': 'garbage'}):
            self.assertEqual(m._budget_setting(), 200.0)
        with patch.dict('os.environ', {'GSD_DELIVERY_BUDGET_SEC': '120'}):
            self.assertEqual(m._budget_setting(), 120.0)

    def test_budget_exhaustion_fails_closed_before_any_call(self):
        m.PROCESS_BUDGET_SEC = 0.0
        with self.assertRaises(m.Pending) as caught:
            m.command(['true'])
        self.assertIn('budget', str(caught.exception))

    def test_each_call_timeout_is_clamped_to_remaining_budget(self):
        captured = {}

        class FakeResult:
            returncode = 0
            stdout = b'ok'
            stderr = b''

        def fake_run(args, cwd=None, capture_output=True, timeout=None):
            captured['timeout'] = timeout
            return FakeResult()

        m.PROCESS_BUDGET_SEC = 30.0
        m._start = time.monotonic()
        with patch.object(m.subprocess, 'run', fake_run):
            m.command(['x'], timeout=90)
        self.assertGreater(captured['timeout'], 0)
        self.assertLessEqual(captured['timeout'], m.remaining_budget() + 1.0)
        self.assertLessEqual(captured['timeout'], 30.0)

    # ------------------------------------------------------------------
    # Explicit auth failure reason, never credential exposure
    # ------------------------------------------------------------------

    def test_auth_failure_reason_is_explicit_without_echoing_credentials(self):
        class FakeResult:
            returncode = 1
            stdout = b''
            stderr = b'gh: HTTP 401: Bad credentials - token abc123secret'

        with patch.object(m.subprocess, 'run', lambda *a, **k: FakeResult()):
            with self.assertRaises(m.Pending) as caught:
                m.command(['gh', 'api', 'repos/x'])
        self.assertIn('authentication', str(caught.exception))
        self.assertNotIn('abc123secret', str(caught.exception))
        self.assertNotIn('Bad credentials', str(caught.exception))

    def test_auth_error_detected_across_gh_dialects(self):
        for stderr in (b'gh: To get started with GitHub CLI, please run: gh auth login',
                       b'gh: GITHUB_TOKEN is not set',
                       b'gh: authentication required'):
            def fake_run(*args, _stderr=stderr, **kwargs):
                class FakeResult:
                    returncode = 1
                    stdout = b''
                    stderr = _stderr
                return FakeResult()
            with patch.object(m.subprocess, 'run', fake_run), \
                    self.assertRaises(m.Pending) as caught:
                m.command(['gh', 'api', 'x'])
            self.assertIn('authentication', str(caught.exception))

    # ------------------------------------------------------------------
    # Per-process API caching (13 checks must not mean 13 request sets)
    # ------------------------------------------------------------------

    def test_api_caches_route_results_per_process(self):
        calls = []

        class FakeResult:
            returncode = 0
            stdout = b'{"k": 1}'
            stderr = b''

        def fake_command(args, cwd=None, timeout=45, raw=False):
            calls.append(list(args))
            return b'{"k": 1}' if raw else '{"k": 1}'

        with patch.object(m, 'command', fake_command):
            first = m.api(SLUG, 'pulls/1')
            second = m.api(SLUG, 'pulls/1')
        self.assertIs(first, second)
        self.assertEqual(len(calls), 1)

    def test_checks_pages_and_run_details_are_cached_per_process(self):
        sha = 'e' * 40
        calls = []

        def fake_command(args, cwd=None, timeout=45, raw=False):
            route = ' '.join(args)
            calls.append(route)
            if 'check-runs' in route:
                return json.dumps({'check_runs': [check_run('lint', sha),
                                                  check_run('test (0)', sha, job=2),
                                                  check_run('workflows (0)', sha, job=3)],
                                   'total_count': 3})
            assert 'actions/runs/777' in route
            return json.dumps(run_details(sha, 'pull_request', branch='b'))

        with patch.object(m, 'command', fake_command):
            for _ in range(2):
                for name in ('lint', 'test (0)', 'workflows (0)'):
                    m.trusted_check(SLUG, sha, name, 'pull_request', 'b')
        # One check-page fetch and one run-details fetch for three names
        # sharing a run, repeated verification adds no requests.
        self.assertEqual(sum('check-runs' in c for c in calls), 1)
        self.assertEqual(sum('actions/runs/777' in c for c in calls), 1)

    def test_incomplete_or_malformed_check_pages_fail_closed(self):
        for pages in ([], [{}], [{'check_runs': [42]}], {'not': 'a list'},
                      {'check_runs': [], 'total_count': 2}):
            fake = FakeAPI({(SLUG, 'commits/x/check-runs?per_page=100&page=1', False): pages})
            with patch.object(m, 'api', fake), self.assertRaises(m.Pending):
                m.checks(SLUG, 'x')

    # ------------------------------------------------------------------
    # Malformed nullable API fields must not crash
    # ------------------------------------------------------------------

    def test_dig_traverses_nullable_fields_safely(self):
        self.assertIsNone(m.dig({'a': None}, 'a', 'b'))
        self.assertIsNone(m.dig([1, 2], 'a'))
        self.assertEqual(m.dig({'a': {'b': 2}}, 'a', 'b'), 2)
        self.assertIsNone(m.dig(None, 'a', 'b'))

    def test_validate_merge_survives_malformed_nullables(self):
        good_commit = {'sha': 'b' * 40, 'parents': [{'sha': 'd' * 40}]}
        for pr in (dict(merged=True, merge_commit_sha='b' * 40, head=None, base=None),
                   dict(merged=True, merge_commit_sha='b' * 40,
                        head={'sha': 'c' * 40, 'repo': None},
                        base={'ref': 'main', 'repo': {'full_name': SLUG}}),
                   None):
            with self.assertRaises(m.Pending):
                m.validate_merge(pr, good_commit, SLUG, 'b' * 40, 'c' * 40)
        with self.assertRaises(m.Pending):
            m.validate_merge({'merged': True, 'merge_commit_sha': 'b' * 40,
                              'head': {'sha': 'c' * 40, 'repo': {'full_name': SLUG}},
                              'base': {'ref': 'main', 'repo': {'full_name': SLUG}}},
                             {'sha': 'b' * 40, 'parents': None}, SLUG, 'b' * 40, 'c' * 40)

    def test_trusted_check_rejects_untrusted_identity(self):
        sha = 'e' * 40
        cases = [
            ('untrusted app', dict(app=999), {}),
            ('failed check', dict(conclusion='failure'), {}),
            ('pending check', dict(status='in_progress'), {}),
            ('wrong head', dict(head='f' * 40), {}),
            ('run event mismatch', {}, dict(event='push')),
            ('run branch mismatch', {}, dict(branch='other')),
            ('run not concluded', {}, dict(conclusion='failure')),
            ('run status stale', {}, dict(status='in_progress')),
        ]
        for label, check_kwargs, run_kwargs in cases:
            head_override = check_kwargs.pop('head', None)
            routes = {
                (SLUG, f'commits/{sha}/check-runs?per_page=100&page=1', False):
                    {'check_runs': [check_run('lint', sha, head=head_override,
                                              **check_kwargs)],
                     'total_count': 1},
                (SLUG, 'actions/runs/777', False):
                    run_details(sha, run_kwargs.get('event', 'pull_request'),
                                branch=run_kwargs.get('branch', 'b'),
                                status=run_kwargs.get('status', 'completed'),
                                conclusion=run_kwargs.get('conclusion', 'success')),
            }
            m._API_CACHE.clear()
            m._CHECKS_CACHE.clear()
            with patch.object(m, 'api', FakeAPI(routes)), \
                    self.assertRaises(m.Pending, msg=label):
                m.trusted_check(SLUG, sha, 'lint', 'pull_request', 'b')
        m._API_CACHE.clear()
        m._CHECKS_CACHE.clear()
        routes = {(SLUG, f'commits/{sha}/check-runs?per_page=100&page=1', False):
                  {'check_runs': [], 'total_count': 0}}
        with patch.object(m, 'api', FakeAPI(routes)), self.assertRaises(m.Pending):
            m.trusted_check(SLUG, sha, 'lint', 'pull_request', 'b')

    def test_protection_requires_configured_expected_name_snapshot(self):
        def routes(contexts):
            return {(SLUG, 'branches/main/protection', False): protection(contexts)}
        with patch.object(m, 'api', FakeAPI(routes([]))), self.assertRaises(m.Pending) as caught:
            m.protection(SLUG, POLICY_NAMES)
        self.assertIn('configured native checks', str(caught.exception))
        with patch.object(m, 'api', FakeAPI(routes(None))), self.assertRaises(m.Pending):
            m.protection(SLUG, POLICY_NAMES)
        missing = [n for n in POLICY_NAMES if n != 'lint']
        with patch.object(m, 'api', FakeAPI(routes(missing))), self.assertRaises(m.Pending):
            m.protection(SLUG, POLICY_NAMES)
        # Additive newer names beyond the snapshot stay acceptable.
        additive = POLICY_NAMES + ['newer-check']
        with patch.object(m, 'api', FakeAPI(routes(additive))):
            m.protection(SLUG, POLICY_NAMES)
        weakened = protection(additive)
        weakened['required_status_checks']['strict'] = False
        with patch.object(m, 'api',
                          FakeAPI({(SLUG, 'branches/main/protection', False): weakened})), \
                self.assertRaises(m.Pending):
            m.protection(SLUG, POLICY_NAMES)

    # ------------------------------------------------------------------
    # Frozen snapshot provenance (trusted plan-snapshot module)
    # ------------------------------------------------------------------

    def test_declared_outputs_normalize_q_prefix_only_for_q_repo(self):
        facts = {'declared': {'evidence': ['q/docs/reports/gsd-wave-evidence/v1-w1.rktd'],
                              'review': ['q/docs/reports/gsd-wave-reviews/v1-w1.rktd'],
                              'validation': ['q/docs/reports/gsd-wave-validation/v1-w1.rktd']}}
        q_dir = self.base / 'q'
        q_dir.mkdir()
        declared = m.declared_outputs(facts, q_dir)
        self.assertEqual(declared['evidence'], 'docs/reports/gsd-wave-evidence/v1-w1.rktd')
        with self.assertRaises(m.Pending):
            m.declared_outputs(facts, self.base / 'repo')

    def test_declared_outputs_refuse_ambiguous_or_partial_declarations(self):
        for declared in ({'evidence': ['a.rktd', 'b.rktd'],
                          'review': ['r.rktd'], 'validation': ['v.rktd']},
                         {'evidence': ['docs/reports/gsd-wave-evidence/a.rktd']},
                         {'evidence': 'not-a-list', 'review': [], 'validation': []}):
            facts = {'declared': declared}
            with self.assertRaises(m.Pending):
                m.declared_outputs(facts, self.base)
        self.assertIsNone(m.declared_outputs({'declared': {}}, self.base))
        self.assertIsNone(m.declared_outputs({}, self.base))

    def test_snapshot_facts_fail_closed(self):
        plan = 'a' * 64
        for kwargs in (dict(valid=False),):
            root = build_campaign(self.base / f'camp-{kwargs}', plan, 1, **kwargs)
            with self.assertRaises(m.Pending):
                m.snapshot_facts(root, plan, 1)
        with self.assertRaises(m.Pending):
            m.snapshot_facts(None, plan, 1)
        with self.assertRaises(m.Pending):
            m.snapshot_facts(self.base / 'missing', plan, 1)
        root = build_campaign(self.base / 'camp-ok', plan, 1)
        facts = m.snapshot_facts(root, plan, 1)
        self.assertEqual(facts['status'], 'ok')
        self.assertEqual(facts['issue'], 9686)
        self.assertEqual(facts['milestone'], 895)
        self.assertEqual(facts['declared']['evidence'],
                         ['docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd'])

    # ------------------------------------------------------------------
    # status end-to-end (real git fixture + fake API)
    # ------------------------------------------------------------------

    def test_status_end_to_end_delivered(self):
        w = self.world()
        with self.fake_api(status_routes(w)):
            result = m.status(w['subject'], w['plan'], w['wave'])
        self.assertEqual(result['status'], 'delivered')
        self.assertEqual(result['merge-sha'], w['merge'])
        self.assertEqual(result['publication-sha'], w['publication'])
        self.assertEqual(result['plan-id'], w['plan'])

    def test_status_requires_implementation_sha_equal_to_merge_sha(self):
        w = self.world(pub_impl_sha='f' * 40)
        with self.fake_api(status_routes(w)), self.assertRaises(m.Pending) as caught:
            m.status(w['subject'], w['plan'], w['wave'])
        self.assertIn('implementation-sha', str(caught.exception))

    def test_status_refuses_publication_changing_more_than_the_current_binding(self):
        w = self.world(green_skip=True)
        with self.fake_api(status_routes(w)), self.assertRaises(m.Pending) as caught:
            m.status(w['subject'], w['plan'], w['wave'])
        self.assertIn('exactly the current binding', str(caught.exception))

    def test_status_requires_every_expected_check_green_at_exact_head(self):
        w = self.world()
        routes = status_routes(w, names=[n for n in POLICY_NAMES if n != 'workflows (0)'])
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.status(w['subject'], w['plan'], w['wave'])
        self.assertIn('missing check', str(caught.exception))

    def test_status_requires_actual_governance_run_at_publication(self):
        w = self.world()
        with self.fake_api(status_routes(w, governance=False)), \
                self.assertRaises(m.Pending) as caught:
            m.status(w['subject'], w['plan'], w['wave'])
        self.assertIn('missing check', str(caught.exception))

    def test_status_requires_publication_through_merged_same_repo_pr(self):
        w = self.world()
        for pub_prs in ([], [{'merged': False, 'merge_commit_sha': w['publication'],
                              'head': {'repo': {'full_name': SLUG}},
                              'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}]):
            routes = status_routes(w, pub_prs=pub_prs)
            with self.fake_api(routes), self.assertRaises(m.Pending):
                m.status(w['subject'], w['plan'], w['wave'])

    def test_status_requires_configured_protection_names(self):
        w = self.world()
        with self.fake_api(status_routes(w, contexts=[])), self.assertRaises(m.Pending):
            m.status(w['subject'], w['plan'], w['wave'])

    def test_status_refuses_head_sha_not_actually_fetched_from_pr_ref(self):
        w = self.world()
        # The PR payload and the binding agree on the head SHA, but the actual
        # refs/pull/<n>/head ref on the remote points elsewhere: the fetch
        # assertion must catch it instead of trusting metadata.
        sh('git', 'push', '-q', '--force', w['origin'],
           f"{w['c0']}:refs/pull/{w['pr']}/head", cwd=w['work'])
        with self.fake_api(status_routes(w)), self.assertRaises(m.Pending) as caught:
            m.status(w['subject'], w['plan'], w['wave'])
        self.assertIn('fetched implementation head', str(caught.exception))

    def test_status_refuses_foreign_publication_pr_or_fork(self):
        w = self.world()
        forked = [{'merged': True, 'merge_commit_sha': w['publication'],
                   'head': {'repo': {'full_name': 'someone/fork'}},
                   'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}]
        with self.fake_api(status_routes(w, pub_prs=forked)), self.assertRaises(m.Pending):
            m.status(w['subject'], w['plan'], w['wave'])

    def test_publication_pr_accepts_merged_at_association_quirk(self):
        # GitHub's commits/{sha}/pulls endpoint reports `merged: null` even
        # for a genuinely merged squash PR; state == 'closed' AND a non-null
        # merged_at are the merged signal (verified against a real binding).
        pub = 'a' * 40
        pr = {'number': 9699, 'merged': None, 'state': 'closed',
              'merged_at': '2026-09-14T17:34:25Z', 'merge_commit_sha': pub,
              'head': {'repo': {'full_name': SLUG}},
              'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}
        with self.fake_api({(SLUG, f'commits/{pub}/pulls', False): [pr]}):
            self.assertEqual(m.publication_pr(SLUG, pub)['number'], 9699)

    def test_publication_pr_authoritative_fallback_and_fail_closed(self):
        pub = 'b' * 40
        pr = {'number': 7777, 'merged': None, 'state': 'closed', 'merged_at': None,
              'merge_commit_sha': pub,
              'head': {'repo': {'full_name': SLUG}},
              'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}
        # association payload inconclusive -> authoritative endpoint confirms
        with self.fake_api({(SLUG, f'commits/{pub}/pulls', False): [pr],
                            (SLUG, 'pulls/7777', False): {'merged': True}}):
            self.assertEqual(m.publication_pr(SLUG, pub)['number'], 7777)
        # authoritative endpoint confirms the closed-plus-merged_at shape
        with self.fake_api({(SLUG, f'commits/{pub}/pulls', False): [pr],
                            (SLUG, 'pulls/7777', False): {
                                'state': 'closed', 'merged': False,
                                'merged_at': '2026-09-15T10:00:00Z',
                                'merge_commit_sha': pub}}):
            self.assertEqual(m.publication_pr(SLUG, pub)['number'], 7777)
        # authoritative endpoint says not merged -> fails closed
        with self.assertRaises(m.Pending):
            with self.fake_api({(SLUG, f'commits/{pub}/pulls', False): [pr],
                                (SLUG, 'pulls/7777', False): {'merged': False}}):
                m.publication_pr(SLUG, pub)

    def test_status_pending_when_binding_absent(self):
        w = self.world()
        with self.assertRaises(m.Pending):
            m.status(w['subject'], 'b' * 64, w['wave'])

    def test_status_refuses_uncommitted_local_edits_to_published_trio(self):
        w = self.world()
        subject = w['subject']
        target = subject / 'docs/reports/gsd-wave-reviews' / f"{w['label']}.rktd"
        target.write_text(target.read_text() + '\n;; drift\n')
        with self.fake_api(status_routes(w)), self.assertRaises(m.Pending) as caught:
            m.status(subject, w['plan'], w['wave'])
        self.assertIn('differs from committed proof', str(caught.exception))

    # ------------------------------------------------------------------
    # prepare (frozen provenance, rebind refusal, draft scaffold)
    # ------------------------------------------------------------------

    def prepare_routes(self, w, *, merge=None, head=None, number=None):
        merge = merge or w['merge']
        head = head or w['head']
        number = number or w['pr']
        parent = w['c0'] if merge == w['merge'] else w['merge']
        return {
            (SLUG, f'pulls/{number}', False):
                pr_payload(w, merge=merge, head=head, number=number, base=parent),
            (SLUG, f'commits/{merge}', False): commit_payload(merge, parent),
            (SLUG, 'branches/main/protection', False): protection(POLICY_NAMES),
        }

    def test_prepare_draft_is_gate_red_but_structurally_complete(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        output = self.base / 'draft-out'
        with self.fake_api(self.prepare_routes(w)):
            result = m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                               f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                               campaign, output)
        self.assertEqual(result['status'], 'pending-review')
        binding = output / 'docs/reports/gsd-wave-evidence' / f"{w['plan']}-w{w['wave']}.rktd"
        self.assertTrue(binding.is_file())
        draft = m.read_datum(binding)
        self.assertEqual(draft['required-checks'], POLICY_NAMES)
        self.assertEqual(draft['status'], 'pending-review')
        self.assertEqual(draft['implementation-sha'], w['merge'])
        self.assertEqual(draft['content-digest'], 'PENDING')
        self.assertNotIn('APPROVED', json.dumps(draft))
        review = m.read_datum(output / draft['review-artifact'])
        self.assertEqual(review['verdict'], 'PENDING')
        validation = m.read_datum(output / draft['validation-artifact'])
        self.assertEqual(validation['red-first']['command'], 'PENDING')
        self.assertEqual(validation['fast']['result'], 'PENDING')
        # The unchanged strict gate must reject the draft for the honest
        # placeholder reasons (never a fabricated pass).
        policy = write_file(self.base / 'draft-policy',
                            '("' + '" "'.join(POLICY_NAMES) + '")')
        gate = subprocess.run(
            ['racket', str(ROOT / 'scripts/gsd-wave-gate.rkt'), str(binding),
             '--content-digest', EMPTY_SHA, '--root', str(output), '--policy', str(policy)],
            capture_output=True, text=True)
        self.assertNotEqual(gate.returncode, 0)
        self.assertIn('content-digest', gate.stdout)
        self.assertIn('status must be ready-for-merge', gate.stdout)
        self.assertIn('APPROVED', gate.stdout)

    def test_prepare_self_resolves_merged_pr_without_number(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        routes = self.prepare_routes(w)
        routes[(SLUG, f'pulls?state=all&head=owner:{WAVE_BRANCH}', False)] = [
            dict(pr_payload(w), state='closed')]
        with self.fake_api(routes):
            result = m.prepare(w['subject'], w['plan'], w['wave'], None,
                               f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                               campaign, output, expected_branch=WAVE_BRANCH)
        self.assertEqual(result['status'], 'pending-review')
        binding = output / 'docs/reports/gsd-wave-evidence' / f"{w['plan']}-w{w['wave']}.rktd"
        self.assertTrue(binding.is_file())
        self.assertEqual(m.read_datum(binding)['merge-sha'], w['merge'])

    def test_prepare_self_resolve_requires_receipt_branch(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        with self.assertRaisesRegex(m.Pending, 'expected-branch'):
            m.prepare(w['subject'], w['plan'], w['wave'], None,
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, self.base / 'prepared')

    def test_binding_review_keeps_pending_draft_unapproved(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        result = m.binding_review(w['subject'], w['plan'], w['wave'], output,
                                  campaign_root=self.base / 'campaign',
                                  expected_branch=WAVE_BRANCH)
        self.assertEqual(result['status'], 'pending-review')
        self.assertEqual(result['branch'], m.binding_branch(w['plan'], w['wave']))

    def test_binding_review_accepts_finalized_staged_trio(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        binding_branch = m.binding_branch(w['plan'], w['wave'])
        files = trio(f"{w['plan']}-w{w['wave']}", w['wave'],
                     impl_sha=w['merge'], digest=m.EMPTY_SHA,
                     plan_id=w['plan'], merge=w['merge'], head=w['head'],
                     pr=w['pr'], branch=binding_branch,
                     evidence_branch=binding_branch, wave_branch=WAVE_BRANCH)
        for relative, text in files.items():
            target = output / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(text)
        result = m.binding_review(w['subject'], w['plan'], w['wave'], output,
                                  campaign_root=self.base / 'campaign',
                                  expected_branch=WAVE_BRANCH)
        self.assertEqual(result['status'], 'reviewed')
        self.assertEqual(result['branch'], binding_branch)

    def test_binding_review_refuses_sentinel_reviewer_identity(self):
        """Register F12: a finalized review must carry a genuine reviewer."""
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        binding_branch = m.binding_branch(w['plan'], w['wave'])
        files = trio(f"{w['plan']}-w{w['wave']}", w['wave'],
                     impl_sha=w['merge'], digest=m.EMPTY_SHA,
                     plan_id=w['plan'], merge=w['merge'], head=w['head'],
                     pr=w['pr'], branch=binding_branch,
                     evidence_branch=binding_branch, wave_branch=WAVE_BRANCH)
        for relative, text in files.items():
            target = output / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(text)
        review_rel = f"docs/reports/gsd-wave-reviews/{w['plan']}-w{w['wave']}.rktd"
        review_path = output / review_rel
        review_path.write_text(files[review_rel].replace('"wave-reviewer"', '"PENDING"'))
        with self.assertRaises(m.Pending) as caught:
            m.binding_review(w['subject'], w['plan'], w['wave'], output,
                             campaign_root=self.base / 'campaign',
                             expected_branch=WAVE_BRANCH)
        self.assertIn('genuine reviewer identity', str(caught.exception))

    def test_binding_review_reviewer_identity_sentinels(self):
        """Review R6: compound sentinels are refused, genuine names are not.

        The whole-string pattern covers pure sentinels ("PENDING"); the
        boundary-anchored compound pattern covers identities like
        "PENDING-INDEPENDENT-REVIEW" without rejecting legitimate names
        that merely contain sentinel-like letter sequences ("Natalie")."""
        def staged_world_with_reviewer(reviewer):
            w = self.world(publish=False)
            campaign_dir = self.base / ('campaign-%d' % len(list(self.base.iterdir())))
            campaign = build_campaign(campaign_dir, w['plan'], w['wave'])
            output = binding_staging(campaign_dir, w['plan'], w['wave'])
            with self.fake_api(self.prepare_routes(w)):
                m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                          f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                          campaign, output)
            binding_branch = m.binding_branch(w['plan'], w['wave'])
            files = trio(f"{w['plan']}-w{w['wave']}", w['wave'],
                         impl_sha=w['merge'], digest=m.EMPTY_SHA,
                         plan_id=w['plan'], merge=w['merge'], head=w['head'],
                         pr=w['pr'], branch=binding_branch,
                         evidence_branch=binding_branch, wave_branch=WAVE_BRANCH,
                         reviewer=reviewer)
            for relative, text in files.items():
                target = output / relative
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text(text)
            return w, campaign_dir, output

        for reviewer in ('PENDING-INDEPENDENT-REVIEW', 'PLACEHOLDER-REVIEWER'):
            w, campaign_dir, output = staged_world_with_reviewer(reviewer)
            with self.fake_api(self.prepare_routes(w)), \
                    self.assertRaises(m.Pending) as caught:
                m.binding_review(w['subject'], w['plan'], w['wave'], output,
                                 campaign_root=campaign_dir,
                                 expected_branch=WAVE_BRANCH)
            self.assertIn('genuine reviewer identity', str(caught.exception),
                          msg=reviewer)

        w, campaign_dir, output = staged_world_with_reviewer('Natalie A. Reviewer')
        result = m.binding_review(w['subject'], w['plan'], w['wave'], output,
                                  campaign_root=campaign_dir,
                                  expected_branch=WAVE_BRANCH)
        self.assertEqual(result['status'], 'reviewed')

    def test_prepare_refuses_mixed_record_commit(self):
        """Register F4: the evidence record must be authored evidence-only."""
        w = self.world(publish=False, mixed_record=True)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)), \
                self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, self.base / 'draft-mixed')
        message = str(caught.exception)
        self.assertIn('impure-record-commit', message)
        self.assertIn('README.md', message)

    def test_binding_review_reports_missing_or_malformed_staging(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        missing = m.binding_review(w['subject'], w['plan'], w['wave'], output,
                                   campaign_root=self.base / 'campaign',
                                   expected_branch=WAVE_BRANCH)
        self.assertEqual(missing['status'], 'awaiting-review')
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        (output / 'docs/reports/gsd-wave-evidence' / f"{w['plan']}-w{w['wave']}.rktd").write_text('#hasheq((schema-version . 99))')
        with self.assertRaisesRegex(m.Pending, 'schema'):
            m.binding_review(w['subject'], w['plan'], w['wave'], output,
                             campaign_root=self.base / 'campaign',
                             expected_branch=WAVE_BRANCH)

    def test_binding_review_rejects_output_escape_and_rebinding(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        with self.assertRaisesRegex(m.Pending, 'output'):
            m.binding_review(w['subject'], w['plan'], w['wave'], self.base / 'escape',
                             campaign_root=self.base / 'campaign',
                             expected_branch=WAVE_BRANCH)

        write_file(w['subject'] / w['binding'],
                   m.read_datum(output / 'docs/reports/gsd-wave-evidence' / f"{w['plan']}-w{w['wave']}.rktd")['implementation-sha'])
        sh('git', 'add', '-A', cwd=w['subject'])
        sh('git', 'commit', '-m', 'different binding', cwd=w['subject'])
        sh('git', 'update-ref', 'refs/remotes/origin/main', 'HEAD', cwd=w['subject'])
        with patch.object(m, 'refresh', lambda repo: None), self.assertRaisesRegex(m.Pending, 'rebind'):
            m.binding_review(w['subject'], w['plan'], w['wave'], output,
                             campaign_root=self.base / 'campaign',
                             expected_branch=WAVE_BRANCH)

    def test_binding_publish_reuses_open_pr(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        finalize_binding(self.base / 'campaign', w, output)
        branch = m.binding_branch(w['plan'], w['wave'])
        with tempfile.TemporaryDirectory() as temp:
            worktree = Path(temp) / 'publish'
            sh('git', 'worktree', 'add', '--detach', str(worktree),
               'refs/remotes/origin/main', cwd=w['subject'])
            try:
                for kind in ('evidence', 'reviews', 'validation'):
                    source = output / 'docs/reports' / ('gsd-wave-' + kind) / f"{w['plan']}-w{w['wave']}.rktd"
                    target = worktree / 'docs/reports' / ('gsd-wave-' + kind) / source.name
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(source.read_bytes())
                sh('git', 'add', '-A', cwd=worktree)
                sh('git', 'commit', '--no-gpg-sign', '-m', f'W{w["wave"]} binding draft: {w["plan"]}-w{w["wave"]}',
                   cwd=worktree)
                commit = sh('git', 'rev-parse', 'HEAD', cwd=worktree).strip()
                sh('git', 'push', '-q', 'origin', f'{commit}:refs/heads/{branch}', cwd=w['subject'])
                sh('git', 'fetch', '-q', 'origin', f'refs/heads/{branch}:refs/remotes/origin/{branch}',
                   cwd=w['subject'])
            finally:
                sh('git', 'worktree', 'remove', '--force', str(worktree), cwd=w['subject'])
        pr_number = 77
        pr_data = {'number': pr_number, 'merged': False, 'state': 'open',
                   'head': {'sha': commit, 'ref': branch, 'repo': {'full_name': SLUG}},
                   'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}
        routes = {(SLUG, f'pulls?state=open&head=owner:{branch}', False): [pr_data],
                  (SLUG, f'pulls?state=all&head=owner:{branch}', False): [],
                  (SLUG, f'pulls/{pr_number}', False): pr_data}
        with self.fake_api(routes):
            result = m.binding_publish(w['subject'], w['plan'], w['wave'], output,
                                       campaign_root=self.base / 'campaign')
        self.assertEqual(result['status'], 'exists')
        self.assertEqual(result['pr'], pr_number)
        self.assertTrue(sh('git', 'show-ref', '--verify',
                           f'refs/remotes/origin/{branch}', cwd=w['subject']).strip())

    def test_binding_publish_creates_fresh_main_pr(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        finalize_binding(self.base / 'campaign', w, output)
        branch = m.binding_branch(w['plan'], w['wave'])
        pr_number = 79
        routes = {(SLUG, f'pulls?state=open&head=owner:{branch}', False): [],
                  (SLUG, f'pulls?state=all&head=owner:{branch}', False): []}
        def created_pr(slug, route, paginate=False, _refresh=False):
            return {'number': pr_number, 'merged': False, 'state': 'open',
                    'head': {'sha': sh('git', 'rev-parse',
                                        f'refs/remotes/origin/{branch}',
                                        cwd=w['subject']).strip(),
                             'ref': branch, 'repo': {'full_name': SLUG}},
                    'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}
        routes[(SLUG, f'pulls/{pr_number}', False)] = created_pr
        with self.fake_api(routes), patch.object(m, 'gh_post',
                                                  return_value={'number': pr_number}):
            result = m.binding_publish(w['subject'], w['plan'], w['wave'], output,
                                       campaign_root=self.base / 'campaign')
        self.assertEqual(result['status'], 'opened')
        self.assertEqual(result['pr'], pr_number)
        self.assertTrue(sh('git', 'show-ref', '--verify',
                           f'refs/remotes/origin/{branch}', cwd=w['subject']).strip())

    def test_binding_publish_rejects_divergent_existing_branch(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'campaign', w['plan'], w['wave'])
        output = binding_staging(self.base / 'campaign', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                      f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                      campaign, output)
        finalize_binding(self.base / 'campaign', w, output)
        branch = m.binding_branch(w['plan'], w['wave'])
        with tempfile.TemporaryDirectory() as temp:
            worktree = Path(temp) / 'publish'
            sh('git', 'worktree', 'add', '--detach', str(worktree),
               'refs/remotes/origin/main', cwd=w['subject'])
            try:
                for kind in ('evidence', 'reviews', 'validation'):
                    source = output / 'docs/reports' / ('gsd-wave-' + kind) / f"{w['plan']}-w{w['wave']}.rktd"
                    target = worktree / 'docs/reports' / ('gsd-wave-' + kind) / source.name
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(source.read_bytes())
                # Diverge only one binding blob; ref identity alone must not
                # be enough to reuse a pre-existing publication branch.
                evidence = output / 'docs/reports/gsd-wave-evidence' / f"{w['plan']}-w{w['wave']}.rktd"
                divergent = evidence.read_text().replace('"ready-for-merge"', '"tampered"')
                (worktree / 'docs/reports/gsd-wave-evidence' / evidence.name).write_text(divergent)
                sh('git', 'add', '-A', cwd=worktree)
                sh('git', 'commit', '--no-gpg-sign', '-m', f'W{w["wave"]} divergent binding',
                   cwd=worktree)
                commit = sh('git', 'rev-parse', 'HEAD', cwd=worktree).strip()
                sh('git', 'push', '-q', 'origin', f'{commit}:refs/heads/{branch}', cwd=w['subject'])
                sh('git', 'fetch', '-q', 'origin', f'refs/heads/{branch}:refs/remotes/origin/{branch}',
                   cwd=w['subject'])
            finally:
                sh('git', 'worktree', 'remove', '--force', str(worktree), cwd=w['subject'])
        pr_number = 81
        pr_data = {'number': pr_number, 'merged': False, 'state': 'open',
                   'head': {'sha': commit, 'ref': branch, 'repo': {'full_name': SLUG}},
                   'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}
        routes = {(SLUG, f'pulls?state=open&head=owner:{branch}', False): [pr_data],
                  (SLUG, f'pulls?state=all&head=owner:{branch}', False): [],
                  (SLUG, f'pulls/{pr_number}', False): pr_data}
        with self.fake_api(routes), self.assertRaisesRegex(m.Pending, 'staged trio'):
            m.binding_publish(w['subject'], w['plan'], w['wave'], output,
                              campaign_root=self.base / 'campaign')

    # ------------------------------------------------------------------
    # Binding resolve/CI/merge actions: deterministic branch identity,
    # no implementation-receipt ancestry, protected merge delegation.
    # ------------------------------------------------------------------

    def binding_pr_world(self, *, merged=False, bpr=910, trio_options=None):
        """A finalized binding publication branch pushed to origin: main stays
        at the implementation squash M and the binding branch adds only the
        three plan-named evidence artifacts, so its changed-content digest is
        EMPTY_SHA. The branch carries an open (or merged) same-repo PR."""
        w = self.world(publish=False)
        branch = m.binding_branch(w['plan'], w['wave'])
        work = w['work']
        sh('git', 'checkout', '-q', '-b', branch, 'main', cwd=work)
        for rel, text in trio(f"{w['plan']}-w{w['wave']}", w['wave'],
                              impl_sha=w['merge'], digest=m.EMPTY_SHA,
                              plan_id=w['plan'], merge=w['merge'], head=w['head'],
                              pr=w['pr'], branch=branch, evidence_branch=branch,
                              wave_branch=WAVE_BRANCH,
                              **(trio_options or {})).items():
            write_file(work / rel, text)
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'binding publication', cwd=work)
        bhead = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        sh('git', 'checkout', '-q', 'main', cwd=work)
        sh('git', 'push', '-q', w['origin'], f'{bhead}:refs/heads/{branch}', cwd=work)
        sh('git', 'push', '-q', w['origin'], f'{bhead}:refs/pull/{bpr}/head', cwd=work)
        payload = {'number': bpr, 'merged': merged, 'merge_commit_sha': w['merge'],
                   'merged_at': '2026-09-14T16:23:06Z' if merged else None,
                   'state': 'closed' if merged else 'open',
                   'user': {'login': 'wave-author', 'type': 'User'},
                   'head': {'sha': bhead, 'ref': branch,
                            'repo': {'full_name': SLUG}},
                   'base': {'ref': 'main', 'sha': w['merge'],
                            'repo': {'full_name': SLUG}}}
        return dict(w, bbranch=branch, bhead=bhead, bpr=bpr, bpayload=payload)

    def binding_resolve_routes(self, w, *, merged=False):
        return {
            (SLUG, f'pulls?state=open&head=owner:{w["bbranch"]}', False):
                [] if merged else [w['bpayload']],
            (SLUG, f'pulls?state=all&head=owner:{w["bbranch"]}', False):
                [w['bpayload']] if merged else [],
            (SLUG, f"pulls/{w['bpr']}", False): w['bpayload'],
        }

    def test_binding_resolve_pr_resolves_open_pr_with_exact_head_and_metadata(self):
        w = self.binding_pr_world()
        with self.fake_api(self.binding_resolve_routes(w)):
            result = m.binding_resolve_pr(w['subject'], w['bbranch'],
                                          plan=w['plan'], wave=w['wave'])
        self.assertEqual(result, {'status': 'resolved', 'pr': w['bpr'],
                                  'branch': w['bbranch'], 'head': w['bhead'],
                                  'plan-id': w['plan'], 'wave': w['wave']})

    def test_binding_resolve_pr_reports_already_merged_and_none(self):
        w = self.binding_pr_world()
        branch = w['bbranch']
        # No PR at all: the fetched deterministic branch alone yields the
        # typed none result (publication may not have been opened yet).
        with self.fake_api({
                (SLUG, f'pulls?state=open&head=owner:{branch}', False): [],
                (SLUG, f'pulls?state=all&head=owner:{branch}', False): []}):
            self.assertEqual(m.binding_resolve_pr(w['subject'], branch),
                             {'status': 'none', 'branch': branch})
        # Idempotent resume: exactly one merged PR whose head is the exact
        # fetched branch tip.
        merged = self.binding_pr_world(merged=True)
        with self.fake_api(self.binding_resolve_routes(merged, merged=True)):
            result = m.binding_resolve_pr(merged['subject'], branch,
                                          plan=merged['plan'], wave=merged['wave'])
        self.assertEqual(result['status'], 'already-merged')
        self.assertEqual(result['pr'], merged['bpr'])
        self.assertEqual(result['head'], merged['bhead'])
        self.assertEqual(result['branch'], branch)
        self.assertEqual(result['plan-id'], merged['plan'])
        self.assertEqual(result['wave'], merged['wave'])

    def test_binding_resolve_pr_rejects_head_or_branch_mismatch(self):
        w = self.binding_pr_world()
        wrong_head = dict(w['bpayload'],
                          head={'sha': '9' * 40, 'ref': w['bbranch'],
                                'repo': {'full_name': SLUG}})
        with self.fake_api({**self.binding_resolve_routes(w),
                            (SLUG, f"pulls/{w['bpr']}", False): wrong_head}), \
                self.assertRaises(m.Pending) as caught:
            m.binding_resolve_pr(w['subject'], w['bbranch'])
        self.assertIn('head does not match', str(caught.exception))
        wrong_branch = dict(w['bpayload'],
                            head={'sha': w['bhead'], 'ref': 'campaign/wrong',
                                  'repo': {'full_name': SLUG}})
        with self.fake_api({**self.binding_resolve_routes(w),
                            (SLUG, f"pulls/{w['bpr']}", False): wrong_branch}), \
                self.assertRaises(m.Pending):
            m.binding_resolve_pr(w['subject'], w['bbranch'])

    def test_binding_resolve_pr_resumes_merged_pr_after_branch_deletion(self):
        # Post-merge hygiene commonly deletes the head branch. Idempotent
        # resume must still find the merged PR via refs/pull/N/head instead
        # of failing on the deleted branch fetch (review finding F2).
        w = self.binding_pr_world(merged=True)
        sh('git', 'push', '-q', w['origin'], f':refs/heads/{w["bbranch"]}', cwd=w['work'])
        with self.fake_api(self.binding_resolve_routes(w, merged=True)):
            result = m.binding_resolve_pr(w['subject'], w['bbranch'],
                                          plan=w['plan'], wave=w['wave'])
        self.assertEqual(result['status'], 'already-merged')
        self.assertEqual(result['pr'], w['bpr'])
        self.assertEqual(result['head'], w['bhead'])
        self.assertEqual(result['plan-id'], w['plan'])

    def test_binding_resolve_pr_refuses_divergent_recreated_branch_after_merge(self):
        # A recreated branch must never inherit a stale merged PR identity.
        # The branch-tip equality is enforced OUTSIDE the deletion tolerance:
        # a divergent recreated tip fails closed (review finding R2).
        w = self.binding_pr_world(merged=True)
        branch = w['bbranch']
        sh('git', 'push', '-q', w['origin'], f':refs/heads/{branch}', cwd=w['work'])
        sh('git', 'checkout', '-q', '-B', branch, 'main', cwd=w['work'])
        write_file(w['work'] / 'src/file.txt', 'recreated\n')
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-m', 'recreated divergent', cwd=w['work'])
        divergent = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'checkout', '-q', 'main', cwd=w['work'])
        sh('git', 'push', '-q', w['origin'], f'{divergent}:refs/heads/{branch}', cwd=w['work'])
        with self.fake_api(self.binding_resolve_routes(w, merged=True)), \
                self.assertRaises(m.Pending) as caught:
            m.binding_resolve_pr(w['subject'], branch, plan=w['plan'], wave=w['wave'])
        self.assertIn('does not match fetched branch tip', str(caught.exception))

    def test_binding_resolve_pr_reports_none_before_first_publication(self):
        # No PR and no branch yet: the typed none result must be reachable
        # without an opaque git fetch failure (review finding F2).
        w = self.binding_pr_world()
        sh('git', 'push', '-q', w['origin'], f':refs/heads/{w["bbranch"]}', cwd=w['work'])
        with self.fake_api({
                (SLUG, f'pulls?state=open&head=owner:{w["bbranch"]}', False): [],
                (SLUG, f'pulls?state=all&head=owner:{w["bbranch"]}', False): []}):
            self.assertEqual(m.binding_resolve_pr(w['subject'], w['bbranch']),
                             {'status': 'none', 'branch': w['bbranch']})

    def binding_ci_routes(self, w, *, names=None, conclusion='success'):
        names = POLICY_NAMES if names is None else names
        return {
            (SLUG, f"pulls/{w['bpr']}", False): w['bpayload'],
            (SLUG, 'branches/main/protection', False): protection(POLICY_NAMES),
            (SLUG, f"commits/{w['bhead']}/check-runs?per_page=100&page=1", False):
                {'check_runs': [check_run(name, w['bhead'], job=i,
                                          conclusion=conclusion)
                                for i, name in enumerate(names)],
                 'total_count': len(names)},
            (SLUG, 'actions/runs/777', False):
                run_details(w['bhead'], 'pull_request', branch=w['bbranch']),
        }

    def test_binding_ci_reports_green_at_exact_head(self):
        w = self.binding_pr_world()
        m._CHECKS_CACHE.clear()
        with self.fake_api(self.binding_ci_routes(w)):
            result = m.binding_ci(w['subject'], w['bpr'], w['bbranch'], w['bhead'])
        self.assertEqual(result, {'status': 'green', 'pr': w['bpr'],
                                  'branch': w['bbranch'], 'head': w['bhead']})

    def test_binding_ci_requires_exact_head_and_green_checks(self):
        w = self.binding_pr_world()
        with self.fake_api(self.binding_ci_routes(w)), \
                self.assertRaises(m.Pending) as caught:
            m.binding_ci(w['subject'], w['bpr'], w['bbranch'], '9' * 40)
        self.assertIn('expected head', str(caught.exception))
        m._CHECKS_CACHE.clear()
        with self.fake_api(self.binding_ci_routes(w, conclusion='failure')), \
                self.assertRaises(m.Pending) as caught:
            m.binding_ci(w['subject'], w['bpr'], w['bbranch'], w['bhead'])
        self.assertIn(POLICY_NAMES[0], str(caught.exception))
        self.assertIn('failed', str(caught.exception))

    def test_binding_ci_refuses_wrong_branch_or_foreign_repository(self):
        w = self.binding_pr_world()
        wrong_branch = dict(w['bpayload'],
                            head={'sha': w['bhead'], 'ref': 'campaign/wrong',
                                  'repo': {'full_name': SLUG}})
        with self.fake_api({**self.binding_ci_routes(w),
                            (SLUG, f"pulls/{w['bpr']}", False): wrong_branch}), \
                self.assertRaises(m.Pending):
            m.binding_ci(w['subject'], w['bpr'], w['bbranch'], w['bhead'])
        foreign = dict(w['bpayload'],
                       head={'sha': w['bhead'], 'ref': w['bbranch'],
                             'repo': {'full_name': 'someone/fork'}})
        with self.fake_api({**self.binding_ci_routes(w),
                            (SLUG, f"pulls/{w['bpr']}", False): foreign}), \
                self.assertRaises(m.Pending):
            m.binding_ci(w['subject'], w['bpr'], w['bbranch'], w['bhead'])

    def binding_merge_routes(self, w, *, reviews=None):
        return self.binding_ci_routes(w)

    def test_binding_merge_delegates_protected_squash_merge(self):
        w = self.binding_pr_world()
        calls = []

        class FakeResult:
            returncode = 0
            stdout = b'{"merged": true, "sha": "' + w['bhead'].encode() + b'"}'
            stderr = b''

        def fake_command(args, cwd=None, timeout=45, raw=False):
            calls.append(list(args))
            if '/merge' in ' '.join(args):
                return FakeResult.stdout if raw else FakeResult.stdout.decode()
            return real_command(args, cwd=cwd, timeout=timeout, raw=raw)

        real_command = m.command
        routes = self.binding_merge_routes(w)
        merged_pr = dict(w['bpayload'], merged=True, state='closed',
                         merge_commit_sha=w['bhead'])
        calls_pr = {'n': 0}
        def pull_route(*_args):
            calls_pr['n'] += 1
            return w['bpayload'] if calls_pr['n'] == 1 else merged_pr
        routes[(SLUG, f"pulls/{w['bpr']}", False)] = pull_route
        routes[(SLUG, f"commits/{w['bhead']}", False)] = \
            commit_payload(w['bhead'], w['merge'])
        with self.fake_api(routes), patch.object(m, 'command', fake_command):
            result = m.binding_merge(w['subject'], w['plan'], w['wave'], w['bpr'],
                                     w['bhead'], w['bbranch'], w['binding'])
        self.assertEqual(result['status'], 'merged')
        self.assertEqual(result['merge-sha'], w['bhead'])
        self.assertEqual(result['plan-id'], w['plan'])
        self.assertEqual(result['wave'], w['wave'])
        mutation = [a for a in calls if '/merge' in ' '.join(a)]
        self.assertEqual(len(mutation), 1)
        self.assertIn('merge_method=squash', ' '.join(mutation[0]))

    def test_binding_merge_refuses_unauthorized_evidence_and_head_drift(self):
        """F11: binding merges obey the same amended approval contract — an
        unstaged authorization is a typed refusal, and head drift is refused."""
        w = self.binding_pr_world(trio_options={'merge_auth': False})
        with self.fake_api(self.binding_merge_routes(w)), \
                self.assertRaises(m.Pending) as caught:
            m.binding_merge(w['subject'], w['plan'], w['wave'], w['bpr'],
                            w['bhead'], w['bbranch'], w['binding'])
        self.assertIn('no-operator-authorization', str(caught.exception))

        w2 = self.binding_pr_world()
        wrong_head = dict(w2['bpayload'],
                          head={'sha': '9' * 40, 'ref': w2['bbranch'],
                                'repo': {'full_name': SLUG}})
        with self.fake_api({**self.binding_merge_routes(w2),
                            (SLUG, f"pulls/{w2['bpr']}", False): wrong_head}), \
                self.assertRaises(m.Pending) as caught:
            m.binding_merge(w2['subject'], w2['plan'], w2['wave'], w2['bpr'],
                            w2['bhead'], w2['bbranch'], w2['binding'])
        self.assertIn('expected head', str(caught.exception))

    def test_binding_merge_is_idempotent_when_already_merged(self):
        w = self.binding_pr_world(merged=True)
        calls = []

        def fake_command(args, cwd=None, timeout=45, raw=False):
            calls.append(list(args))
            return real_command(args, cwd=cwd, timeout=timeout, raw=raw)

        real_command = m.command
        routes = self.binding_merge_routes(w)
        routes[(SLUG, f"commits/{w['merge']}", False)] = \
            commit_payload(w['merge'], w['c0'])
        with self.fake_api(routes), patch.object(m, 'command', fake_command):
            result = m.binding_merge(w['subject'], w['plan'], w['wave'], w['bpr'],
                                     w['bhead'], w['bbranch'], w['binding'])
        self.assertEqual(result['status'], 'already-merged')
        self.assertEqual(result['merge-sha'], w['merge'])
        self.assertFalse(any('/merge' in ' '.join(a) for a in calls))

    def test_cli_binding_actions_require_args_and_dispatch(self):
        w = self.binding_pr_world()
        for action, expected in (
                ('binding-ci', 'binding-ci requires'),
                ('binding-merge', 'binding-merge requires'),
                ('binding-resolve-pr', 'binding-resolve-pr requires')):
            argv = ['gsd-delivery.py', action, '--repo', str(w['subject'])]
            stdout = io.StringIO()
            with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
                code = m.main()
            self.assertEqual(code, 2, msg=action)
            self.assertIn(expected, json.loads(stdout.getvalue())['reason'], msg=action)

        argv = ['gsd-delivery.py', 'binding-resolve-pr', '--repo', str(w['subject']),
                '--plan', w['plan'], '--wave', str(w['wave']),
                '--expected-branch', w['bbranch']]
        stdout = io.StringIO()
        with self.fake_api(self.binding_resolve_routes(w)), \
                patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
            code = m.main()
        self.assertEqual(code, 0)
        data = json.loads(stdout.getvalue())
        self.assertEqual(data['status'], 'resolved')
        self.assertEqual(data['head'], w['bhead'])
        self.assertEqual(data['plan-id'], w['plan'])
        self.assertEqual(data['wave'], w['wave'])

    def test_governance_checks_binding_publication_on_main(self):
        w = self.world(publish=True)
        branch = m.binding_branch(w['plan'], w['wave'])
        publication_pr = {'number': 78, 'state': 'closed', 'merged': True,
                          'merge_commit_sha': w['publication'],
                          'merged_at': '2026-09-14T16:30:00Z',
                          'head': {'sha': w['publication'], 'ref': branch,
                                   'repo': {'full_name': SLUG}},
                          'base': {'ref': 'main', 'repo': {'full_name': SLUG}}}
        routes = status_routes(w, governance=True)
        routes[(SLUG, f'pulls?state=all&head=owner:{branch}', False)] = [publication_pr]
        routes[(SLUG, f'commits/{w["publication"]}', False)] = \
            commit_payload(w['publication'], w['merge'])
        with self.fake_api(routes):
            result = m.governance(w['subject'], w['plan'], w['wave'], branch)
        self.assertEqual(result['status'], 'governed')

    def test_prepare_accepts_merged_at_authoritative_shape(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        output = self.base / 'draft-out'
        payload = dict(self.prepare_routes(w)[(SLUG, f'pulls/{w["pr"]}', False)],
                       state='closed', merged=False,
                       merged_at='2026-09-15T10:00:00Z')
        with self.fake_api({**self.prepare_routes(w),
                            (SLUG, f'pulls/{w["pr"]}', False): payload}):
            result = m.prepare(w['subject'], w['plan'], w['wave'], w['pr'],
                               f"docs/reports/gsd-wave-evidence/{w['label']}.rktd",
                               campaign, output)
        self.assertEqual(result['status'], 'pending-review')

    def test_prepare_accepts_exact_hash_named_source_without_declarations(self):
        w = self.world(publish=False, source='hash')
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'], declared=False)
        output = self.base / 'draft-hash'
        with self.fake_api(self.prepare_routes(w)):
            result = m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['binding'],
                               campaign, output)
        self.assertEqual(result['status'], 'pending-review')
        self.assertEqual(result['source'], w['binding'])

    def test_prepare_refuses_source_that_is_not_the_frozen_declared_output(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        output = self.base / 'draft-bad'
        other = f'docs/reports/gsd-wave-evidence/v1.00.24-w{w["wave"]}.rktd'
        with self.fake_api(self.prepare_routes(w)), self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], other, campaign, output)
        self.assertIn('frozen wave output', str(caught.exception))

    def test_prepare_refuses_hash_named_guess_when_outputs_are_declared(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)), self.assertRaises(m.Pending):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['binding'],
                      campaign, self.base / 'draft-nope')

    def test_prepare_refuses_explicit_foreign_plan_id_in_source(self):
        w = self.world(publish=False, source_plan_id='b' * 64)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w)), self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                      campaign, self.base / 'draft-foreign')
        self.assertIn('another campaign', str(caught.exception))

    def test_prepare_refuses_source_issue_diverging_from_frozen_metadata(self):
        w = self.world(publish=False, source_issue=1111)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'], issue=9686)
        with self.fake_api(self.prepare_routes(w)), self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                      campaign, self.base / 'draft-issue')
        self.assertIn('issue', str(caught.exception))

    def test_prepare_requires_validated_snapshot_before_provenance(self):
        w = self.world(publish=False)
        # Campaign root exists but the snapshot is tampered: trusted module
        # hash validation must refuse before any source is accepted.
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'], valid=False)
        with self.fake_api(self.prepare_routes(w)), self.assertRaises(m.Pending):
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                      campaign, self.base / 'draft-tamper')

    def test_prepare_q_declared_source_normalizes_only_for_q_repository(self):
        w = self.world(publish=False, q_named=True)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'], q_declared=True)
        output = self.base / 'draft-q'
        with self.fake_api(self.prepare_routes(w)):
            result = m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                               campaign, output)
        self.assertEqual(result['status'], 'pending-review')
        # Same declared snapshot against a non-q repository must refuse.
        w2 = self.world(publish=False)
        campaign2 = build_campaign(self.base / 'camp2', w2['plan'], w2['wave'], q_declared=True)
        with self.fake_api(self.prepare_routes(w2)), self.assertRaises(m.Pending) as caught:
            m.prepare(w2['subject'], w2['plan'], w2['wave'], w2['pr'], w2['source'],
                      campaign2, self.base / 'draft-q2')
        self.assertIn('q/', str(caught.exception))

    def test_prepare_refuses_rebinding_published_wave_to_different_implementation(self):
        w = self.world(publish=True, second_impl=True)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        with self.fake_api(self.prepare_routes(w, merge=w['merge2'], head=w['head2'],
                                               number=w['pr'] + 1)), \
                self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'] + 1, w['source'],
                      campaign, self.base / 'draft-rebind')
        self.assertIn('refusing rebind', str(caught.exception))

    def test_prepare_allows_redraft_of_identical_verified_implementation(self):
        w = self.world(publish=True)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        output = self.base / 'draft-same'
        with self.fake_api(self.prepare_routes(w)):
            result = m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                               campaign, output)
        self.assertEqual(result['status'], 'pending-review')

    def test_prepare_never_overwrites_existing_output(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        output = self.base / 'draft-exists'
        output.mkdir()
        with self.fake_api(self.prepare_routes(w)), self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                      campaign, output)
        self.assertIn('refusing to overwrite', str(caught.exception))

    def test_prepare_requires_merged_independently_reviewed_pr(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        routes = self.prepare_routes(w)
        unmerged = dict(routes[(SLUG, f"pulls/{w['pr']}", False)])
        unmerged['merged'] = False
        routes[(SLUG, f"pulls/{w['pr']}", False)] = unmerged
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                      campaign, self.base / 'draft-unmerged')
        self.assertIn('merged', str(caught.exception))

    def test_prepare_asserts_fetched_pr_head_matches(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        routes = self.prepare_routes(w, head='9' * 40)
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                      campaign, self.base / 'draft-head')
        self.assertIn('fetched implementation head', str(caught.exception))

    def test_prepare_refuses_merge_tree_differing_from_pr_head_tree(self):
        w = self.world(publish=False)
        campaign = build_campaign(self.base / 'camp', w['plan'], w['wave'])
        work = w['work']
        # A real, fetchable PR head whose tree diverges from the merge tree
        # must be refused (not a clean squash).
        sh('git', 'checkout', '-q', 'impl', cwd=work)
        write_file(work / 'src/file.txt', 'divergent\n')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'divergent', cwd=work)
        divergent = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        sh('git', 'push', '-q', w['origin'], f'{divergent}:refs/pull/{w["pr"]}/head', cwd=work)
        routes = self.prepare_routes(w, head=divergent)
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.prepare(w['subject'], w['plan'], w['wave'], w['pr'], w['source'],
                      campaign, self.base / 'draft-tree')
        self.assertIn('tree', str(caught.exception))

    # ------------------------------------------------------------------
    # sync (clean, expected-branch, fast-forward only)
    # ------------------------------------------------------------------

    def advance_origin(self, w):
        work = w['work']
        sh('git', 'checkout', '-q', 'main', cwd=work)
        write_file(work / 'src/file.txt', 'advanced\n')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'advance', cwd=work)
        sh('git', 'push', '-q', w['origin'], 'main:main', cwd=work)
        return sh('git', 'rev-parse', 'HEAD', cwd=work).strip()

    def test_sync_fast_forwards_expected_branch(self):
        w = self.world()
        advanced = self.advance_origin(w)
        result = m.sync(w['subject'], 'main')
        self.assertEqual(result['status'], 'synchronized')
        self.assertEqual(result['head'], advanced)

    def test_sync_refuses_dirty_checkout(self):
        w = self.world()
        self.advance_origin(w)
        write_file(w['subject'] / 'src/file.txt', 'local-edit\n')
        with self.assertRaises(m.Pending) as caught:
            m.sync(w['subject'], 'main')
        self.assertIn('dirty', str(caught.exception))

    def test_sync_tolerates_executor_scratch_probes(self):
        # sandbox-write-scratch-parity W1: untracked files under
        # .planning/scratch/ are disposable executor probes — they must not
        # block delivery synchronization. Real edits still refuse.
        w = self.world()
        self.advance_origin(w)
        scratch = w['subject'] / '.planning' / 'scratch' / 'tok1234567890ab'
        scratch.mkdir(parents=True)
        write_file(scratch / 'probe.rkt', '#lang racket/base\n')
        result = m.sync(w['subject'], 'main')
        self.assertEqual(result['status'], 'synchronized')

    def test_sync_refuses_committed_scratch_edit(self):
        # A COMMITTED scratch file's local modification is real working-tree
        # dirt: only untracked probe residue is exempt.
        w = self.world()
        self.advance_origin(w)
        scratch = w['subject'] / '.planning' / 'scratch' / 'tok1234567890ab'
        scratch.mkdir(parents=True)
        write_file(scratch / 'committed.rkt', 'one\n')
        sh('git', 'add', '-A', cwd=w['subject'])
        sh('git', 'commit', '-q', '-m', 'scratch', cwd=w['subject'])
        write_file(scratch / 'committed.rkt', 'two\n')
        with self.assertRaises(m.Pending) as caught:
            m.sync(w['subject'], 'main')
        self.assertIn('dirty', str(caught.exception))

    def test_sync_refuses_dirty_checkout_outside_scratch(self):
        # A tracked-file edit plus a scratch probe still refuses: the scratch
        # tolerance never widens into a general dirty-checkout allowance.
        w = self.world()
        self.advance_origin(w)
        scratch = w['subject'] / '.planning' / 'scratch' / 'tok1234567890ab'
        scratch.mkdir(parents=True)
        write_file(scratch / 'probe.rkt', '#lang racket/base\n')
        write_file(w['subject'] / 'src/file.txt', 'local-edit\n')
        with self.assertRaises(m.Pending) as caught:
            m.sync(w['subject'], 'main')
        self.assertIn('dirty', str(caught.exception))

    def test_sync_refuses_detached_head(self):
        w = self.world()
        sh('git', 'checkout', '-q', '--detach', 'HEAD', cwd=w['subject'])
        with self.assertRaises(m.Pending) as caught:
            m.sync(w['subject'], 'main')
        self.assertIn('detached', str(caught.exception))

    def test_sync_requires_explicit_expected_branch_match(self):
        w = self.world()
        with self.assertRaises(m.Pending) as caught:
            m.sync(w['subject'], 'some-other-branch')
        self.assertIn('expected', str(caught.exception))
        subject = w['subject']
        sh('git', 'checkout', '-q', '-b', 'unrelated-work', cwd=subject)
        with self.assertRaises(m.Pending) as caught:
            m.sync(subject, 'main')
        self.assertIn('unrelated-work', str(caught.exception))
        with self.assertRaises(m.Pending):
            m.sync(subject, None)
        with self.assertRaises(m.Pending):
            m.sync(subject, '')

    def test_sync_refuses_non_fast_forward(self):
        w = self.world()
        work = w['work']
        sh('git', 'checkout', '-q', '-b', 'diverge', w['c0'], cwd=work)
        write_file(work / 'src/file.txt', 'rewritten\n')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'diverge', cwd=work)
        divergent = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        sh('git', 'push', '-q', '--force', w['origin'], f'{divergent}:refs/heads/main', cwd=work)
        with self.assertRaises(m.Pending):
            m.sync(w['subject'], 'main')

    # ------------------------------------------------------------------
    # merge (deterministic protected squash merge of the implementation PR)
    # ------------------------------------------------------------------

    def merge_routes(self, w, *, names=None, head=None, pr=None, base=None,
                     author='wave-author', merged=None, merge_sha=None):
        names = POLICY_NAMES if names is None else names
        number = pr or w['pr']
        head = head or w['head']
        base = base or w['c0']
        payload = {'number': number, 'merged': bool(merged) if merged is not None else False,
                   'merge_commit_sha': merge_sha,
                   'state': 'closed' if merged else 'open',
                   'user': {'login': author, 'type': 'User'},
                   'head': {'sha': head, 'ref': WAVE_BRANCH,
                            'repo': {'full_name': SLUG}},
                   'base': {'ref': 'main', 'sha': base,
                            'repo': {'full_name': SLUG}}}
        routes = {
            (SLUG, f'pulls/{number}', False): payload,
            (SLUG, 'branches/main/protection', False): protection(POLICY_NAMES),
            (SLUG, f'commits/{head}/check-runs?per_page=100&page=1', False):
                {'check_runs': [check_run(name, head, job=i)
                                for i, name in enumerate(names)],
                 'total_count': len(names)},
            (SLUG, 'actions/runs/777', False):
                run_details(head, 'pull_request', branch=WAVE_BRANCH),
        }
        return routes

    def open_impl_world(self, *, pr=42, merged=False, plan=None, source='version',
                        trio_options=None):
        """An OPEN implementation PR: main stays at c0 (no squash yet) while the
        impl branch carries the source trio and is pushed as refs/pull/<pr>/head.
        When merged=True, main additionally contains the squash M so the
        already-merged idempotent path can be exercised against a real origin.
        """
        plan = plan or 'a' * 64
        base = self.base / f'world-open-{len(list(self.base.iterdir()))}'
        origin_path = base / 'origin.git'
        sh('git', 'init', '--bare', '-b', 'main', origin_path)
        work = init_repo(base / 'work')
        write_file(work / 'scripts/required-pr-checks.policy',
                   '("' + '" "'.join(POLICY_NAMES) + '")')
        write_file(work / 'src/file.txt', 'one\n')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'c0', cwd=work)
        c0 = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        sh('git', 'checkout', '-b', 'impl', cwd=work)
        write_file(work / 'src/file.txt', 'two\n')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'h1', cwd=work)
        h1 = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        label = 'v9.9.9-w1'
        source_label = 'v9.9.9-w1' if source == 'version' else f'{plan}-w1'
        digest = m.digest(work, c0, h1)
        for rel, text in trio(source_label, 1, impl_sha=h1,
                              digest=digest, **(trio_options or {})).items():
            write_file(work / rel, text)
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'h2 source trio', cwd=work)
        head = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        sh('git', 'checkout', 'main', cwd=work)
        if merged:
            sh('git', 'merge', '--squash', 'impl', cwd=work)
            sh('git', 'commit', '-m', 'M squash', cwd=work)
        merge = None
        if merged:
            merge = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
        sh('git', 'push', '-q', origin_path, 'main:main', cwd=work)
        sh('git', 'push', '-q', origin_path, f'{head}:refs/pull/{pr}/head', cwd=work)
        sh('git', 'push', '-q', origin_path, f'{head}:refs/heads/{WAVE_BRANCH}', cwd=work)
        subject = base / 'repo'
        sh('git', 'clone', '-q', '--branch', 'main', origin_path, subject)
        sh('git', 'remote', 'set-url', 'origin', f'https://github.com/{SLUG}.git', cwd=subject)
        sh('git', 'config', f'url.{origin_path}.insteadOf', f'https://github.com/{SLUG}.git',
           cwd=subject)
        sh('git', 'config', 'user.email', 'fixture@example.com', cwd=subject)
        sh('git', 'config', 'user.name', 'fixture', cwd=subject)
        return {'origin': origin_path, 'work': work, 'subject': subject,
                'c0': c0, 'head': head, 'h1': h1, 'merge': merge, 'plan': plan,
                'wave': 1, 'pr': pr, 'label': label,
                'source': f'docs/reports/gsd-wave-evidence/{source_label}.rktd'}

    def test_merge_requires_exact_expected_head(self):
        w = self.open_impl_world()
        routes = self.merge_routes(w, head='9' * 40)
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])
        self.assertIn('expected head', str(caught.exception))

    def test_merge_refuses_foreign_repo_or_non_main_base(self):
        w = self.open_impl_world()
        forked = dict(self.merge_routes(w)[(SLUG, f"pulls/{w['pr']}", False)],
                      head={'sha': w['head'], 'ref': WAVE_BRANCH,
                            'repo': {'full_name': 'someone/fork'}})
        with self.fake_api({**self.merge_routes(w),
                            (SLUG, f"pulls/{w['pr']}", False): forked}), \
                self.assertRaises(m.Pending):
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])
        wrong_base = dict(self.merge_routes(w)[(SLUG, f"pulls/{w['pr']}", False)],
                          base={'ref': 'develop', 'sha': w['c0'],
                                'repo': {'full_name': SLUG}})
        with self.fake_api({**self.merge_routes(w),
                            (SLUG, f"pulls/{w['pr']}", False): wrong_base}), \
                self.assertRaises(m.Pending):
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])

    def test_merge_requires_fresh_main(self):
        w = self.open_impl_world()
        # main advanced after the PR was cut: the PR payload still claims the
        # old c0 base, so the merge must fail closed on freshness.
        self.advance_origin_worktree(w)
        routes = self.merge_routes(w)
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])
        self.assertIn('fresh origin/main', str(caught.exception))

    def advance_origin_worktree(self, w):
        work = w['work']
        sh('git', 'checkout', '-q', 'main', cwd=work)
        write_file(work / 'src/file.txt', 'advanced\n')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'advance', cwd=work)
        sh('git', 'push', '-q', w['origin'], 'main:main', cwd=work)

    def test_merge_asserts_fetched_head_equals_expected(self):
        w = self.open_impl_world()
        sh('git', 'push', '-q', '--force', w['origin'],
           f"{w['c0']}:refs/pull/{w['pr']}/head", cwd=w['work'])
        with self.fake_api(self.merge_routes(w)), self.assertRaises(m.Pending) as caught:
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])
        self.assertIn('fetched', str(caught.exception))

    def test_merge_requires_recorded_operator_authorization(self):
        """F11 direction (a): no durable merge-authorization, no merge."""
        w = self.open_impl_world(trio_options={'merge_auth': False})
        with self.fake_api(self.merge_routes(w)), \
                self.assertRaises(m.Pending) as caught:
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])
        self.assertIn('no-operator-authorization', str(caught.exception))

    def test_merge_refuses_unbound_or_uncited_authorization(self):
        """F11 direction (a): an authorization that names another head, another
        wave, or cites no source is not an authorization for this merge."""
        cases = [
            {'merge_auth': {'operator': 'coinerd', 'wave': 'W1',
                            'head': 'f' * 40,
                            'action': 'squash-merge the reviewed wave implementation PR',
                            'source': 'operator directive'}},
            {'merge_auth': {'operator': 'coinerd', 'wave': 'W2',
                            'head': 'placeholder',
                            'action': 'squash-merge the reviewed wave implementation PR',
                            'source': 'operator directive'}},
            {'merge_auth': {'operator': 'coinerd', 'wave': 'W1',
                            'action': 'squash-merge the reviewed wave implementation PR',
                            'source': 'operator directive'}},
            {'merge_auth': {'operator': 'coinerd', 'wave': 'W1',
                            'head': 'placeholder',
                            'action': 'squash-merge the reviewed wave implementation PR'}},
            {'merge_auth': {'operator': 'coinerd', 'wave': 'W1', 'head': 'placeholder',
                            'action': 'squash-merge the reviewed wave implementation PR',
                            'source': '   '}},
        ]
        for options in cases:
            w = self.open_impl_world(trio_options=options)
            with self.fake_api(self.merge_routes(w)), \
                    self.assertRaises(m.Pending) as caught:
                m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                        WAVE_BRANCH, w['source'])
            self.assertIn('no-operator-authorization', str(caught.exception),
                          msg=repr(options))

    def test_merge_requires_approved_nonauthor_review_at_exact_head(self):
        """F11 direction (b): the review artifact must be APPROVED, from a
        non-author reviewer, and bound to the exact head."""
        head = 'placeholder'
        cases = [
            # Non-APPROVED verdicts and a reviewed-sha bound to another head
            # are already refused by the unchanged strict gate inside the trio
            # preflight (generic typed failure there); the author-identity
            # direction is refused by the merge gate with the amended-contract
            # token as defense in depth.
            ({'review_verdict': 'NEEDS_WORK'}, None),
            ({'reviewer': 'wave-author'}, 'no-review-artifact'),
            ({'reviewed_sha': 'a' * 40}, None),
        ]
        for options, token in cases:
            w = self.open_impl_world(trio_options=options)
            with self.fake_api(self.merge_routes(w)), \
                    self.assertRaises(m.Pending) as caught:
                m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                        WAVE_BRANCH, w['source'])
            if token is not None:
                self.assertIn(token, str(caught.exception), msg=repr(options))

    def test_merge_permits_authorized_approved_nonauthor_review(self):
        """F11 direction (c): authorization plus an APPROVED non-author review
        artifact at the exact head is exactly the satisfiable merge gate."""
        w = self.open_impl_world()
        routes = self.merge_routes(w)
        open_pr = routes[(SLUG, f"pulls/{w['pr']}", False)]
        merged_pr = dict(open_pr, merged=True, state='closed', merge_commit_sha=w['h1'])
        calls_pr = {'n': 0}
        def pull_route(*_args):
            calls_pr['n'] += 1
            return open_pr if calls_pr['n'] == 1 else merged_pr
        routes[(SLUG, f"pulls/{w['pr']}", False)] = pull_route
        routes[(SLUG, f'commits/{w["h1"]}', False)] = commit_payload(w['h1'], w['c0'])

        class FakeResult:
            returncode = 0
            stdout = b'{"merged": true, "sha": "' + w['h1'].encode() + b'"}'
            stderr = b''

        def fake_command(args, cwd=None, timeout=45, raw=False):
            if '/merge' in ' '.join(args):
                return FakeResult.stdout if raw else FakeResult.stdout.decode()
            return real_command(args, cwd=cwd, timeout=timeout, raw=raw)

        real_command = m.command
        with self.fake_api(routes), patch.object(m, 'command', fake_command):
            result = m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                             WAVE_BRANCH, w['source'])
        self.assertEqual(result['status'], 'merged')
        self.assertEqual(result['merge-sha'], w['h1'])

    def test_merge_requires_every_required_check_at_exact_head(self):
        w = self.open_impl_world()
        routes = self.merge_routes(
            w, names=[n for n in POLICY_NAMES if n != 'workflows (0)'])
        with self.fake_api(routes), self.assertRaises(m.Pending):
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])

    def test_merge_preflights_source_trio_before_any_mutation(self):
        w = self.open_impl_world()
        routes = self.merge_routes(w)
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'] + '-missing')
        self.assertIn('artifact', str(caught.exception).lower())

    def test_merge_squash_only_via_pr_endpoint_never_admin_or_main_push(self):
        w = self.open_impl_world()
        calls = []

        class FakeResult:
            returncode = 0
            stdout = b'{"merged": true, "sha": "' + w['h1'].encode() + b'"}'
            stderr = b''

        def fake_command(args, cwd=None, timeout=45, raw=False):
            calls.append(list(args))
            if '/merge' in ' '.join(args):
                return FakeResult.stdout if raw else FakeResult.stdout.decode()
            return real_command(args, cwd=cwd, timeout=timeout, raw=raw)

        real_command = m.command
        routes = self.merge_routes(w)
        # The post-merge refetch must not be served from the process cache:
        # stage-1 reads never satisfy post-mutation proof. The PR route is
        # stateful — open before the PUT, merged after the (refreshed) refetch.
        open_pr = routes[(SLUG, f'pulls/{w["pr"]}', False)]
        merged_pr = {
            'number': w['pr'], 'merged': True, 'merge_commit_sha': w['h1'],
            'state': 'closed', 'user': {'login': 'wave-author', 'type': 'User'},
            'head': {'sha': w['head'], 'ref': WAVE_BRANCH,
                     'repo': {'full_name': SLUG}},
            'base': {'ref': 'main', 'sha': w['c0'], 'repo': {'full_name': SLUG}}}
        calls_pr = {'n': 0}
        def pull_route(*_args):
            calls_pr['n'] += 1
            return open_pr if calls_pr['n'] == 1 else merged_pr
        routes[(SLUG, f'pulls/{w["pr"]}', False)] = pull_route
        routes[(SLUG, f'commits/{w["h1"]}', False)] = commit_payload(w['h1'], w['c0'])
        with self.fake_api(routes), patch.object(m, 'command', fake_command):
            result = m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                             WAVE_BRANCH, w['source'])
        self.assertEqual(result['status'], 'merged')
        self.assertEqual(result['merge-sha'], w['h1'])
        self.assertEqual(calls_pr['n'], 2)
        mutation = [a for a in calls if '/merge' in ' '.join(a)]
        self.assertEqual(len(mutation), 1)
        joined = ' '.join(mutation[0])
        self.assertIn('-X', joined)
        self.assertIn('PUT', joined)
        self.assertIn('merge_method=squash', joined)
        self.assertNotIn('admin', joined.lower())
        for call in calls:
            joined_call = ' '.join(call)
            self.assertFalse('push' in joined_call and 'refs/heads/main' in joined_call)

    def test_merge_already_merged_is_idempotent_no_second_merge(self):
        w = self.open_impl_world(merged=True)
        calls = []

        class FakeResult:
            returncode = 0
            stdout = b'{"merged": true, "sha": "' + w['merge'].encode() + b'"}'
            stderr = b''

        def fake_command(args, cwd=None, timeout=45, raw=False):
            calls.append(list(args))
            if '/merge' in ' '.join(args):
                return FakeResult.stdout if raw else FakeResult.stdout.decode()
            return real_command(args, cwd=cwd, timeout=timeout, raw=raw)

        real_command = m.command
        routes = self.merge_routes(w, merged=True, merge_sha=w['merge'])
        routes[(SLUG, f"commits/{w['merge']}", False)] = \
            commit_payload(w['merge'], w['c0'])
        with self.fake_api(routes), patch.object(m, 'command', fake_command):
            result = m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                             WAVE_BRANCH, w['source'])
        self.assertEqual(result['status'], 'already-merged')
        self.assertEqual(result['merge-sha'], w['merge'])
        self.assertFalse([a for a in calls if '/merge' in ' '.join(a)])

    def test_merge_already_merged_still_requires_the_approval_contract(self):
        """Review R3: the idempotent path re-runs the same pre-merge proofs —
        a merge that happened outside this gate is refused, not accepted."""
        w = self.open_impl_world(merged=True, trio_options={'merge_auth': False})
        routes = self.merge_routes(w, merged=True, merge_sha=w['merge'])
        routes[(SLUG, f"commits/{w['merge']}", False)] = \
            commit_payload(w['merge'], w['c0'])
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])
        self.assertIn('no-operator-authorization', str(caught.exception))

    def test_merge_already_merged_still_requires_protection(self):
        """Review R4: the idempotent path also proves the branch-protection
        snapshot; weakened or absent protection refuses the resume."""
        w = self.open_impl_world(merged=True)
        routes = self.merge_routes(w, merged=True, merge_sha=w['merge'])
        routes[(SLUG, f"commits/{w['merge']}", False)] = \
            commit_payload(w['merge'], w['c0'])
        routes[(SLUG, 'branches/main/protection', False)] = protection([])
        with self.fake_api(routes), self.assertRaises(m.Pending) as caught:
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])
        self.assertIn('protection', str(caught.exception))

    def test_merge_already_merged_accepts_merged_at_authoritative_shape(self):
        w = self.open_impl_world(merged=True)
        payload = dict(self.open_pr_payload(w), state='closed', merged=False,
                       merged_at='2026-09-15T10:00:00Z',
                       merge_commit_sha=w['merge'],
                       user={'login': 'wave-author', 'type': 'User'})
        routes = self.merge_routes(w, merged=True, merge_sha=w['merge'])
        routes[(SLUG, f"commits/{w['merge']}", False)] = \
            commit_payload(w['merge'], w['c0'])
        routes[(SLUG, f'pulls/{w["pr"]}', False)] = payload
        with self.fake_api(routes):
            result = m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                             WAVE_BRANCH, w['source'])
        self.assertEqual(result['status'], 'already-merged')
        self.assertEqual(result['merge-sha'], w['merge'])

    def test_merge_refuses_already_merged_at_a_different_head(self):
        w = self.open_impl_world(merged=True)
        routes = self.merge_routes(w, merged=True, merge_sha=w['merge'],
                                   head='d' * 40)
        routes[(SLUG, f"commits/{w['merge']}", False)] = \
            commit_payload(w['merge'], w['c0'])
        with self.fake_api(routes), self.assertRaises(m.Pending):
            m.merge(w['subject'], w['plan'], w['wave'], w['pr'], w['head'],
                    WAVE_BRANCH, w['source'])

    def test_merge_refuses_multiple_open_prs_for_one_branch(self):
        w = self.open_impl_world()
        head = f'owner:{WAVE_BRANCH}'
        open_list = [{'number': w['pr']}, {'number': w['pr'] + 1}]
        with self.fake_api({(SLUG, f'pulls?state=open&head={head}', False): open_list}), \
                self.assertRaises(m.Pending) as caught:
            m.resolve_existing_pr(SLUG, WAVE_BRANCH)
        self.assertIn('multiple', str(caught.exception))
        one = [{'number': w['pr']}]
        with self.fake_api({(SLUG, f'pulls?state=open&head={head}', False): one}):
            self.assertEqual(m.resolve_existing_pr(SLUG, WAVE_BRANCH)['number'],
                             w['pr'])
        with self.fake_api({(SLUG, f'pulls?state=open&head={head}', False): []}):
            self.assertIsNone(m.resolve_existing_pr(SLUG, WAVE_BRANCH))

    # ------------------------------------------------------------------
    # implementation-review: durable review validation at the receipt head
    # ------------------------------------------------------------------

    def review_world(self):
        """An impl branch carrying the BINDING-named trio (the exact source
        merge() enforces) pushed as refs/pull/<pr>/head; no PR object is
        consulted — review is a pure git-object readback. The work repo is
        left on the impl branch so tests can append followup commits."""
        w = self.open_impl_world(source='binding')
        sh('git', 'checkout', '-q', 'impl', cwd=w['work'])
        return w

    def test_review_accepts_valid_trio_at_receipt_head(self):
        w = self.review_world()
        result = m.review(w['subject'], w['plan'], w['wave'], w['head'], WAVE_BRANCH)
        self.assertEqual(result['status'], 'reviewed')
        self.assertEqual(result['head'], w['head'])
        # trio() binds the review to the implementation commit h1; the trio
        # commit itself only adds excluded-dir files, so the binding holds.
        self.assertEqual(result['reviewed-sha'], w['h1'])
        self.assertEqual(result['review-artifact'],
                         'docs/reports/gsd-wave-reviews/%s-w1.rktd' % w['plan'])

    def test_review_awaits_when_review_artifact_is_absent(self):
        w = self.review_world()
        review_rel = 'docs/reports/gsd-wave-reviews/%s-w1.rktd' % w['plan']
        sh('git', 'rm', '-q', review_rel, cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'drop review', cwd=w['work'])
        head2 = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{head2}:refs/heads/{WAVE_BRANCH}',
           f'{head2}:refs/pull/{w["pr"]}/head', cwd=w['work'])
        result = m.review(w['subject'], w['plan'], w['wave'], head2, WAVE_BRANCH)
        self.assertEqual(result['status'], 'awaiting-review')
        self.assertEqual(result['review-artifact'], review_rel)
        self.assertNotIn('reviewed-sha', result)

    def test_review_refuses_missing_binding_trio(self):
        w = self.review_world()
        sh('git', 'rm', '-q', w['source'], cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'drop evidence', cwd=w['work'])
        head2 = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{head2}:refs/heads/{WAVE_BRANCH}',
           f'{head2}:refs/pull/{w["pr"]}/head', cwd=w['work'])
        with self.assertRaises(m.Pending) as caught:
            m.review(w['subject'], w['plan'], w['wave'], head2, WAVE_BRANCH)
        self.assertIn('is absent on branch', str(caught.exception))

    def test_review_refuses_forged_or_unbound_review(self):
        w = self.review_world()
        digest = m.digest(w['work'], w['c0'], w['head'])
        label = f"{w['plan']}-w1"
        cases = [
            trio(label, w['wave'], impl_sha=w['head'], digest=digest,
                 review_verdict='REJECTED'),
            trio(label, w['wave'], impl_sha='9' * 40, digest=digest),
            trio(label, w['wave'], impl_sha=w['head'], digest=EMPTY_SHA),
        ]
        for files in cases:
            for rel, text in files.items():
                write_file(w['work'] / rel, text)
            sh('git', 'add', '-A', cwd=w['work'])
            sh('git', 'commit', '-q', '-m', 'forged trio', cwd=w['work'])
            head2 = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
            sh('git', 'push', '-q', w['origin'], f'{head2}:refs/heads/{WAVE_BRANCH}',
               f'{head2}:refs/pull/{w["pr"]}/head', cwd=w['work'])
            with self.assertRaises(m.Pending):
                m.review(w['subject'], w['plan'], w['wave'], head2, WAVE_BRANCH)

    def test_review_accepts_excluded_dirs_only_post_review_commits(self):
        w = self.review_world()
        extra = 'docs/reports/gsd-wave-reviews/other-note.rktd'
        write_file(w['work'] / extra, '#hasheq((note . "post-review evidence edit"))')
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'evidence-only followup', cwd=w['work'])
        head2 = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{head2}:refs/heads/{WAVE_BRANCH}',
           f'{head2}:refs/pull/{w["pr"]}/head', cwd=w['work'])
        result = m.review(w['subject'], w['plan'], w['wave'], head2, WAVE_BRANCH)
        self.assertEqual(result['status'], 'reviewed')
        self.assertEqual(result['reviewed-sha'], w['h1'])

    def test_review_refuses_review_bound_behind_later_source_commits(self):
        w = self.review_world()
        # Source advances AFTER the review, then the trio is re-declared with
        # the OLD implementation SHA and a digest recomputed at the new head:
        # the strict gate passes (digest matches, review binds the evidence's
        # own implementation-sha) — the receipt-binding clause is the layer
        # that refuses a review which never covered the later source.
        write_file(w['work'] / 'src/file.txt', 'post-review change\n')
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'source drift after review', cwd=w['work'])
        drifted = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        label = f"{w['plan']}-w1"
        digest = m.digest(w['work'], w['c0'], drifted)
        for rel, text in trio(label, w['wave'], impl_sha=w['h1'], digest=digest).items():
            write_file(w['work'] / rel, text)
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'redeclare stale trio', cwd=w['work'])
        head2 = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{head2}:refs/heads/{WAVE_BRANCH}',
           f'{head2}:refs/pull/{w["pr"]}/head', cwd=w['work'])
        with self.assertRaises(m.Pending) as caught:
            m.review(w['subject'], w['plan'], w['wave'], head2, WAVE_BRANCH)
        self.assertIn('non-evidence', str(caught.exception))

    def test_review_requires_fresh_origin_main(self):
        w = self.review_world()
        sh('git', 'branch', '-m', 'main', 'main-renamed', cwd=w['origin'])
        with self.assertRaises(m.Pending):
            m.review(w['subject'], w['plan'], w['wave'], w['head'], WAVE_BRANCH)

    def test_review_validates_at_branch_tip_when_receipt_head_is_ancestor(self):
        """Production resume shape: the receipt froze the verified head; the
        trio + review are produced after Verify. Evidence-only tip commits are
        tolerated; the receipt identity stays authoritative."""
        w = self.review_world()
        extra = 'docs/reports/gsd-wave-reviews/other-note.rktd'
        write_file(w['work'] / extra, '#hasheq((note . "post-verify evidence"))')
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'evidence-only tip commit', cwd=w['work'])
        tip = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{tip}:refs/heads/{WAVE_BRANCH}',
           f'{tip}:refs/pull/{w["pr"]}/head', cwd=w['work'])
        result = m.review(w['subject'], w['plan'], w['wave'], w['head'], WAVE_BRANCH)
        self.assertEqual(result['status'], 'reviewed')
        self.assertEqual(result['head'], w['head'])

    def test_review_refuses_source_drift_after_the_receipt_head(self):
        w = self.review_world()
        write_file(w['work'] / 'src/file.txt', 'post-receipt change\n')
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'source drift after Verify', cwd=w['work'])
        tip = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{tip}:refs/heads/{WAVE_BRANCH}',
           f'{tip}:refs/pull/{w["pr"]}/head', cwd=w['work'])
        with self.assertRaises(m.Pending) as caught:
            m.review(w['subject'], w['plan'], w['wave'], w['head'], WAVE_BRANCH)
        self.assertIn('non-evidence paths', str(caught.exception))

    def test_review_refuses_rewritten_branch_without_the_receipt_head(self):
        w = self.review_world()
        sh('git', 'checkout', '-q', '--orphan', 'rewritten', cwd=w['work'])
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-q', '-m', 'unrelated rewritten history', cwd=w['work'])
        tip = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', '--force', w['origin'],
           f'{tip}:refs/heads/{WAVE_BRANCH}', cwd=w['work'])
        with self.assertRaises(m.Pending):
            m.review(w['subject'], w['plan'], w['wave'], w['head'], WAVE_BRANCH)

    def test_cli_review_requires_head_and_branch(self):
        w = self.review_world()
        proc = subprocess.run(
            ['python3', str(ROOT / 'scripts/gsd-delivery.py'), 'review',
             '--repo', str(w['subject']), '--plan', w['plan'], '--wave', '1'],
            capture_output=True, text=True, timeout=60)
        self.assertEqual(proc.returncode, 2)
        payload = json.loads(proc.stdout)
        self.assertEqual(payload['status'], 'delivery-pending')
        self.assertIn('expected-head', payload['reason'])

    # ------------------------------------------------------------------
    # implementation PR creation and CI readback (W2)
    # ------------------------------------------------------------------

    @staticmethod
    def open_pr_payload(w, *, head=None, number=None):
        payload = dict(pr_payload(w, merge=None, head=head, number=number))
        payload.update({'merged': False, 'merge_commit_sha': None,
                        'merged_at': None, 'state': 'open'})
        return payload

    @staticmethod
    def ci_routes(w, *, names=None, protection_names=None,
                  conclusion='success', missing_names=None):
        names = POLICY_NAMES if names is None else names
        protection_names = POLICY_NAMES if protection_names is None else protection_names
        missing_names = set() if missing_names is None else set(missing_names)
        head = w['head']
        routes = {
            (SLUG, f'pulls/{w["pr"]}', False): DeliveryTests.open_pr_payload(w),
            (SLUG, 'branches/main/protection', False): protection(protection_names),
            (SLUG, f'commits/{head}/check-runs?per_page=100&page=1', False):
                {'check_runs': [check_run(name, head, job=i, conclusion=conclusion)
                                for i, name in enumerate(names)
                                if name not in missing_names],
                 'total_count': len([name for name in names if name not in missing_names])},
        }
        if not missing_names:
            routes[(SLUG, 'actions/runs/777', False)] = \
                run_details(head, 'pull_request', branch=WAVE_BRANCH)
        return routes

    def test_open_pr_resolves_existing_before_create_and_rereads_identity(self):
        w = self.open_impl_world()
        existing = self.open_pr_payload(w)
        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [existing],
            (SLUG, f'pulls/{w["pr"]}', False): existing,
        }
        with self.fake_api(routes) as fake:
            result = m.open_pr(w['subject'], w['plan'], w['wave'], WAVE_BRANCH)
        self.assertEqual(result['status'], 'opened')
        self.assertEqual(result['pr'], w['pr'])
        self.assertEqual(result['head'], w['head'])
        self.assertEqual(fake.calls[-1],
                         (SLUG, f'pulls/{w["pr"]}', False, True))

    def test_open_pr_posts_deterministic_payload_from_fetched_branch_tip(self):
        w = self.open_impl_world()
        created = self.open_pr_payload(w)
        calls = []

        def post(slug, route, fields):
            calls.append((slug, route, fields))
            return created

        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [],
            (SLUG, f'pulls/{w["pr"]}', False): created,
        }
        with self.fake_api(routes), patch.object(m, 'gh_post', side_effect=post):
            result = m.open_pr(w['subject'], w['plan'], w['wave'], WAVE_BRANCH)
        title = f'W{w["wave"]} delivery: {WAVE_BRANCH}'
        body = ('Plan: %s\nWave: W%d\nHead: %s\nBranch: %s\n'
                'Binding evidence: %s\n'
                'Review: genuine independent review required at the exact implementation head.\n') % (
                    w['plan'], w['wave'], w['head'], WAVE_BRANCH,
                    m.binding_path(w['plan'], w['wave']))
        self.assertEqual(calls, [(SLUG, 'pulls',
                                  {'title': title, 'body': body,
                                   'head': WAVE_BRANCH, 'base': 'main'})])
        self.assertEqual(result, {'status': 'opened', 'pr': w['pr'],
                                  'branch': WAVE_BRANCH, 'head': w['head']})

    def test_open_pr_refuses_duplicate_open_prs_without_post(self):
        w = self.open_impl_world()
        first = self.open_pr_payload(w)
        second = self.open_pr_payload(w, number=w['pr'] + 1)
        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [first, second],
        }
        def fail_post(*args):
            raise AssertionError('duplicate open PRs must not be posted')
        with self.fake_api(routes), patch.object(m, 'gh_post', side_effect=fail_post):
            with self.assertRaises(m.Pending):
                m.open_pr(w['subject'], w['plan'], w['wave'], WAVE_BRANCH)

    def test_open_pr_uses_fetched_branch_tip_not_working_tree(self):
        w = self.open_impl_world()
        write_file(w['subject'] / 'src/local.txt', 'local-only\n')
        sh('git', 'add', 'src/local.txt', cwd=w['subject'])
        sh('git', 'commit', '-q', '-m', 'local-only', cwd=w['subject'])
        local_tip = sh('git', 'rev-parse', 'HEAD', cwd=w['subject']).strip()
        created = self.open_pr_payload(w)
        calls = []
        def post(slug, route, fields):
            calls.append(fields)
            return created
        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [],
            (SLUG, f'pulls/{w["pr"]}', False): created,
        }
        with self.fake_api(routes), patch.object(m, 'gh_post', side_effect=post):
            result = m.open_pr(w['subject'], w['plan'], w['wave'], WAVE_BRANCH)
        self.assertEqual(result['head'], w['head'])
        self.assertNotEqual(result['head'], local_tip)
        self.assertEqual(calls[0]['head'], WAVE_BRANCH)

    def test_open_pr_requires_expected_branch(self):
        w = self.open_impl_world()
        with self.assertRaises(m.Pending) as caught:
            m.open_pr(w['subject'], w['plan'], w['wave'], None)
        self.assertIn('expected-branch', str(caught.exception))

    def test_open_pr_refuses_source_drift_after_receipt_head(self):
        w = self.open_impl_world()
        sh('git', 'fetch', '-q', 'origin',
           'refs/heads/' + WAVE_BRANCH + ':refs/remotes/origin/' + WAVE_BRANCH,
           cwd=w['subject'])
        sh('git', 'checkout', '-q', '-b', 'drift',
           'refs/remotes/origin/' + WAVE_BRANCH, cwd=w['subject'])
        write_file(w['subject'] / 'src/drift.txt', 'unreviewed\n')
        sh('git', 'add', 'src/drift.txt', cwd=w['subject'])
        sh('git', 'commit', '-q', '-m', 'unreviewed source drift', cwd=w['subject'])
        sh('git', 'push', '-q', 'origin', 'HEAD:refs/heads/' + WAVE_BRANCH,
           cwd=w['subject'])
        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [],
        }
        def fail_post(*args):
            raise AssertionError('drifted branch must not be posted')
        with self.fake_api(routes), patch.object(m, 'gh_post', side_effect=fail_post):
            with self.assertRaises(m.Pending) as caught:
                m.open_pr(w['subject'], w['plan'], w['wave'], WAVE_BRANCH, w['head'])
        self.assertIn('non-evidence paths', str(caught.exception))

    def test_pr_ci_reports_green_only_for_all_trusted_required_checks(self):
        w = self.open_impl_world()
        m._CHECKS_CACHE.clear()
        with self.fake_api(self.ci_routes(w)):
            result = m.pr_ci(w['subject'], w['pr'], WAVE_BRANCH)
        self.assertEqual(result, {'status': 'green', 'pr': w['pr'],
                                  'branch': WAVE_BRANCH, 'head': w['head']})

    def test_pr_ci_names_pending_failed_and_missing_required_checks(self):
        w = self.open_impl_world()
        for conclusion, expected in (('pending', 'pending'), ('failure', 'failed')):
            with self.subTest(conclusion=conclusion):
                routes = self.ci_routes(w, conclusion=conclusion)
                m._CHECKS_CACHE.clear()
                with self.fake_api(routes):
                    with self.assertRaises(m.Pending) as caught:
                        m.pr_ci(w['subject'], w['pr'], WAVE_BRANCH)
                    self.assertIn(POLICY_NAMES[0], str(caught.exception))
                    self.assertIn(expected, str(caught.exception))
        with self.subTest(conclusion='missing'):
            m._CHECKS_CACHE.clear()
            with self.fake_api(self.ci_routes(w, missing_names=[POLICY_NAMES[0]])):
                with self.assertRaises(m.Pending) as caught:
                    m.pr_ci(w['subject'], w['pr'], WAVE_BRANCH)
            self.assertIn('missing check: ' + POLICY_NAMES[0], str(caught.exception))

    def test_pr_ci_refuses_wrong_branch_or_foreign_repository(self):
        w = self.open_impl_world()
        wrong_branch = dict(self.open_pr_payload(w),
                            head={'sha': w['head'], 'ref': 'campaign/wrong',
                                  'repo': {'full_name': SLUG}})
        with self.fake_api({**self.ci_routes(w),
                            (SLUG, f'pulls/{w["pr"]}', False): wrong_branch}):
            with self.assertRaises(m.Pending):
                m.pr_ci(w['subject'], w['pr'], WAVE_BRANCH)
        foreign = dict(self.open_pr_payload(w),
                       head={'sha': w['head'], 'ref': WAVE_BRANCH,
                             'repo': {'full_name': 'someone/fork'}})
        with self.fake_api({**self.ci_routes(w),
                            (SLUG, f'pulls/{w["pr"]}', False): foreign}):
            with self.assertRaises(m.Pending):
                m.pr_ci(w['subject'], w['pr'], WAVE_BRANCH)

    def test_cli_open_pr_requires_branch_and_dispatches_create_action(self):
        w = self.open_impl_world()
        argv = ['gsd-delivery.py', 'open-pr', '--repo', str(w['subject']),
                '--plan', w['plan'], '--wave', '1']
        stdout = io.StringIO()
        with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
            code = m.main()
        self.assertEqual(code, 2)
        self.assertIn('expected-branch', json.loads(stdout.getvalue())['reason'])

        created = self.open_pr_payload(w)
        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [],
            (SLUG, f'pulls/{w["pr"]}', False): created,
        }
        def post(slug, route, fields):
            return created
        argv = ['gsd-delivery.py', 'open-pr', '--repo', str(w['subject']),
                '--plan', w['plan'], '--wave', '1',
                '--expected-branch', WAVE_BRANCH]
        stdout = io.StringIO()
        with self.fake_api(routes), patch.object(m, 'gh_post', side_effect=post), \
                patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
            code = m.main()
        self.assertEqual(code, 0)
        self.assertEqual(json.loads(stdout.getvalue())['status'], 'opened')

    # ------------------------------------------------------------------
    # CLI shape
    # ------------------------------------------------------------------

    def test_cli_pending_json_shape(self):
        w = self.world()
        argv = ['gsd-delivery.py', 'status', '--repo', str(w['subject']),
                '--plan', 'c' * 64, '--wave', '1']
        stdout = io.StringIO()
        with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
            code = m.main()
        self.assertEqual(code, 2)
        data = json.loads(stdout.getvalue())
        self.assertEqual(data['status'], 'delivery-pending')
        self.assertIn('reason', data)
        self.assertNotIn('token', json.dumps(data).lower())

    def test_cli_prepare_requires_campaign_root_and_sync_requires_branch(self):
        w = self.world()
        argv = ['gsd-delivery.py', 'prepare', '--repo', str(w['subject']),
                '--plan', 'a' * 64, '--wave', '1', '--pr', '1',
                '--evidence', 'docs/reports/gsd-wave-evidence/x-w1.rktd',
                '--output', 'out']
        stdout = io.StringIO()
        with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
            code = m.main()
        self.assertEqual(code, 2)
        self.assertIn('--campaign-root', json.loads(stdout.getvalue())['reason'])
        argv = ['gsd-delivery.py', 'sync', '--repo', str(w['subject'])]
        stdout = io.StringIO()
        with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
            code = m.main()
        self.assertEqual(code, 2)
        self.assertIn('expected-branch', json.loads(stdout.getvalue())['reason'])

    def test_cli_resolve_pr_resolves_open_pr_by_branch_and_refuses_nonepr(self):
        w = self.open_impl_world()
        head = f'owner:{WAVE_BRANCH}'
        with self.fake_api({(SLUG, f'pulls?state=open&head={head}', False):
                            [{'number': w['pr']}]}):
            argv = ['gsd-delivery.py', 'resolve-pr', '--repo', str(w['subject']),
                    '--plan', w['plan'], '--wave', str(w['wave']),
                    '--expected-branch', WAVE_BRANCH]
            stdout = io.StringIO()
            with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
                code = m.main()
            self.assertEqual(code, 0)
            data = json.loads(stdout.getvalue())
            self.assertEqual(data['status'], 'resolved')
            self.assertEqual(data['pr'], w['pr'])
            self.assertIn('head', data)
        with self.fake_api({(SLUG, f'pulls?state=open&head={head}', False): []}):
            argv = ['gsd-delivery.py', 'resolve-pr', '--repo', str(w['subject']),
                    '--plan', w['plan'], '--wave', str(w['wave']),
                    '--expected-branch', WAVE_BRANCH]
            stdout = io.StringIO()
            with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
                code = m.main()
            self.assertEqual(code, 0)
            self.assertEqual(json.loads(stdout.getvalue())['status'], 'none')
        # Missing branch identity fails closed (never picks a PR silently).
        argv = ['gsd-delivery.py', 'resolve-pr', '--repo', str(w['subject']),
                '--plan', w['plan'], '--wave', str(w['wave'])]
        stdout = io.StringIO()
        with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
            code = m.main()
        self.assertEqual(code, 2)
        self.assertIn('expected-branch', json.loads(stdout.getvalue())['reason'])

    def test_resolve_pr_validates_reviewed_tip_and_returns_exact_pr_head(self):
        w = self.open_impl_world()
        payload = {'number': w['pr'], 'state': 'open',
                   'head': {'sha': w['head'], 'ref': WAVE_BRANCH,
                            'repo': {'full_name': SLUG}},
                   'base': {'ref': 'main', 'sha': w['c0'],
                            'repo': {'full_name': SLUG}}}
        # The receipt head is h1; the source trio at the PR tip is allowed
        # evidence-only drift. Resolve must return that actual tip so merge can
        # enforce its exact-head contract without weakening merge().
        with self.fake_api({(SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False):
                            [payload]}):
            argv = ['gsd-delivery.py', 'resolve-pr', '--repo', str(w['subject']),
                    '--plan', w['plan'], '--wave', str(w['wave']),
                    '--expected-branch', WAVE_BRANCH, '--expected-head', w['h1']]
            stdout = io.StringIO()
            with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
                code = m.main()
            self.assertEqual(code, 0)
            data = json.loads(stdout.getvalue())
            self.assertEqual(data, {'status': 'resolved', 'pr': w['pr'],
                                    'plan-id': w['plan'], 'wave': w['wave'],
                                    'branch': WAVE_BRANCH, 'head': w['head']})

    def test_resolve_pr_accepts_merged_pr_for_idempotent_resume(self):
        w = self.open_impl_world()
        payload = dict(self.open_pr_payload(w), state='closed', merged=True)
        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [],
            (SLUG, f'pulls?state=all&head=owner:{WAVE_BRANCH}', False): [payload],
        }
        with self.fake_api(routes):
            argv = ['gsd-delivery.py', 'resolve-pr', '--repo', str(w['subject']),
                    '--plan', w['plan'], '--wave', str(w['wave']),
                    '--expected-branch', WAVE_BRANCH, '--expected-head', w['h1']]
            stdout = io.StringIO()
            with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
                code = m.main()
            self.assertEqual(code, 0)
            self.assertEqual(json.loads(stdout.getvalue())['head'], w['head'])

    def test_open_pr_accepts_evidence_only_tip_after_receipt_head(self):
        w = self.open_impl_world()
        created = self.open_pr_payload(w)
        routes = {
            (SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False): [],
            (SLUG, f'pulls/{w["pr"]}', False): created,
        }
        with self.fake_api(routes), patch.object(m, 'gh_post', return_value=created):
            result = m.open_pr(w['subject'], w['plan'], w['wave'], WAVE_BRANCH,
                               expected_head=w['h1'])
        self.assertEqual(result['head'], w['head'])

    def test_pr_ci_accepts_evidence_only_tip_after_receipt_head(self):
        w = self.open_impl_world()
        m._CHECKS_CACHE.clear()
        with self.fake_api(self.ci_routes(w)):
            result = m.pr_ci(w['subject'], w['pr'], WAVE_BRANCH,
                             expected_head=w['h1'])
        self.assertEqual(result['head'], w['head'])
        self.assertEqual(result['status'], 'green')

    def test_scratch_exempt_porcelain_does_not_split_untracked_arrow_names(self):
        w = self.open_impl_world()
        scratch = w['subject'] / '.planning' / 'scratch' / 'probe'
        scratch.parent.mkdir(parents=True, exist_ok=True)
        scratch.write_text('probe\n')
        foreign_name = w['subject'] / 'src' / 'x -> .planning/scratch/y'
        foreign_name.parent.mkdir(parents=True, exist_ok=True)
        foreign_name.write_text('foreign\n')
        kept = m.scratch_exempt_porcelain(w['subject'])
        self.assertNotIn('.planning/scratch/probe', kept)
        self.assertIn('src/x -> .planning/scratch/y', kept)

    def test_resolve_pr_rejects_rename_from_source_to_evidence(self):
        w = self.open_impl_world()
        sh('git', 'fetch', '-q', 'origin',
           'refs/heads/' + WAVE_BRANCH + ':refs/remotes/origin/' + WAVE_BRANCH,
           cwd=w['subject'])
        sh('git', 'checkout', '-q', '-b', 'rename-drift',
           'refs/remotes/origin/' + WAVE_BRANCH, cwd=w['subject'])
        sh('git', 'mv', 'src/file.txt',
           'docs/reports/gsd-wave-evidence/renamed.txt', cwd=w['subject'])
        sh('git', 'commit', '-q', '-m', 'rename source into evidence', cwd=w['subject'])
        drifted_head = sh('git', 'rev-parse', 'HEAD', cwd=w['subject']).strip()
        sh('git', 'push', '-q', 'origin',
           'HEAD:refs/heads/' + WAVE_BRANCH,
           'HEAD:refs/pull/%d/head' % w['pr'], cwd=w['subject'])
        payload = {'number': w['pr'], 'state': 'open',
                   'head': {'sha': drifted_head, 'ref': WAVE_BRANCH,
                            'repo': {'full_name': SLUG}},
                   'base': {'ref': 'main', 'sha': w['c0'],
                            'repo': {'full_name': SLUG}}}
        with self.fake_api({(SLUG, f'pulls?state=open&head=owner:{WAVE_BRANCH}', False):
                            [payload]}):
            argv = ['gsd-delivery.py', 'resolve-pr', '--repo', str(w['subject']),
                    '--plan', w['plan'], '--wave', str(w['wave']),
                    '--expected-branch', WAVE_BRANCH, '--expected-head', w['h1']]
            stdout = io.StringIO()
            with patch.object(m.sys, 'argv', argv), contextlib.redirect_stdout(stdout):
                code = m.main()
            self.assertEqual(code, 2)
            self.assertIn('non-evidence paths', json.loads(stdout.getvalue())['reason'])


if __name__ == '__main__':
    unittest.main()
