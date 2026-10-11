"""Automatic independent review dispatch regressions (BUG_PLAN slice B).

The trusted controller gains an auto-review port: when the implementation
review trio is genuinely MISSING at the receipt identity, or a staged binding
trio is still pending-review (post-merge finalization), the controller can
prepare a genuinely gate-red template and a typed review request, and later
finalize a FRESH non-author reviewer's typed output through the unchanged
strict gates:

  * prepare never fabricates progress: a valid trio passes through as
    reviewed; real refusals (source drift, stale generations, malformed
    artifacts) stay typed Pending stops and never become dispatchable;
  * finalize trusts ROLE, not prose: the controller binds digests, stamps
    verdict/SHAs itself, commits evidence-only on the receipt branch through
    a pinned non-force push with readback, and only the unchanged strict
    gate can turn an approval into a reviewed state;
  * explicit head-bound REQUEST_CHANGES findings persist through a trusted
    artifact commit and surface as review-rejected with the exact
    receipt head/branch/attempt/fence — the APPROVED gate itself is never
    weakened to accept a rejection verdict;
  * malformed/sentinel reviewer output, transport failures and remote
    divergence are review-infra, never a rejection and never an approval;
  * post-merge binding rejections are typed separately (postmerge) and
    repair the staged evidence, never the merged implementation;
  * staging stays generation-scoped; old generations are never rewritten.
"""
import hashlib
import importlib.util
import json
import pathlib
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

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
    sh('git', 'config', 'gc.auto', '0', cwd=path)
    return path


def write_file(path, text):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)
    return path


def journal_fixture(plan, wave, head, history=0, branch=WAVE_BRANCH,
                    attempt='attempt-5', fence=2):
    receipt = {'repo': '/repo', 'origin': 'https://github.com/owner/repo.git',
               'head': head, 'tree': 'd' * 40, 'verified-at': 1, 'evidence': 'Verify passed',
               'branch': branch, 'attempt-id': attempt, 'attempt-fence': fence}
    return {'schema-version': 1, 'plan-id': plan, 'wave': wave, 'stage': 'context-ready',
            'receipt': receipt,
            'receipt-history': [dict(receipt, head=('%040x' % (i + 1))) for i in range(history)]}


def build_campaign(base, plan, wave, *, declared=True, issue=9686, milestone=895):
    root = Path(base) / 'campaign'
    planning = root / '.planning'
    plan_text = '# Fixture plan\n\nSingle wave: waves/W%d-fixture.md owns the delivery.\n' % wave
    file_lines = ''
    if declared:
        label = f'v9.9.9-w{wave}'
        file_lines = ('\n## Files\n\n'
                      f'- File: `docs/reports/gsd-wave-evidence/{label}.rktd`\n'
                      f'- File: `docs/reports/gsd-wave-reviews/{label}.rktd`\n'
                      f'- File: `docs/reports/gsd-wave-validation/{label}.rktd`\n')
    wave_text = ('# W%d: Fixture wave\n\nBranch: `%s`; GitHub issue '
                 '[#%d](https://github.com/owner/repo/issues/%d), milestone #%d.%s'
                 % (wave, BRANCH, issue, issue, milestone, file_lines))
    write_file(planning / 'PLAN.md', plan_text)
    write_file(planning / f'waves/W{wave}-fixture.md', wave_text)
    snapshot = planning / 'campaigns' / plan / 'plan-snapshot'
    entries = []
    for rel, text in (('PLAN.md', plan_text), (f'waves/W{wave}-fixture.md', wave_text)):
        data = text.encode()
        canonical = text.strip('\n').encode()
        digest = hashlib.sha256(canonical).hexdigest()
        entries.append(f'#hasheq((path . "{rel}") (size . {len(data)}) (sha256 . "{digest}"))')
    write_file(snapshot / 'snapshot-manifest.rktd',
               '#hasheq((schema-version . 1) (campaign . "%s") (plan-id . "%s") (created-at . 1789000000.0) '
               '(files . (%s)))' % (plan, plan, ' '.join(entries)))
    (snapshot / 'waves').mkdir(parents=True, exist_ok=True)
    (snapshot / 'PLAN.md').write_bytes(plan_text.encode())
    (snapshot / f'waves/W{wave}-fixture.md').write_bytes(wave_text.encode())
    return root


def build_world(base, *, plan='a' * 64, wave=1, pr=42, with_trio=False):
    """Real git fixture: main c0; impl branch h1 (src change) [+ source trio].

    The repro shape keeps the review trio ABSENT at the receipt head unless
    with_trio is set. The impl branch is pushed as the wave branch and the
    campaign journal binds the receipt to h1.
    """
    base = Path(base)
    origin_path = base / 'origin.git'
    sh('git', 'init', '--bare', '-b', 'main', origin_path)
    sh('git', 'config', 'gc.auto', '0', cwd=origin_path)
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
    head = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    if with_trio:
        label = f'v9.9.9-w{wave}'
        digest = m.digest(work, c0, head)
        review = f'docs/reports/gsd-wave-reviews/{label}.rktd'
        validation = f'docs/reports/gsd-wave-validation/{label}.rktd'
        write_file(work / f'docs/reports/gsd-wave-evidence/{label}.rktd',
                   '#hasheq((schema-version . 2) (milestone . 895) (wave . "W1") '
                   '(issue . 9686) (status . "ready-for-merge") '
                   f'(implementation-sha . "{head}") (content-digest . "{digest}") '
                   '(required-checks . ("' + '" "'.join(POLICY_NAMES) + '")) '
                   f'(review-artifact . "{review}") (validation-artifact . "{validation}"))')
        write_file(work / review,
                   f'#hasheq((reviewer . "independent-reviewer") (verdict . "APPROVED") '
                   '(timestamp . "2026-09-14T16:00:00Z") '
                   f'(reviewed-sha . "{head}") (content-digest . "{digest}") '
                   '(scope . "independent read-only review of the wave implementation and gate '
                   'evidence at the reviewed head") '
                   '(report . "reviewed the commit chain, ran the focused and fast gates, and '
                   'found no blocking defect at the reviewed head"))')
        write_file(work / validation,
                   '#hasheq((status . "current") (milestone . 895) (wave . "W1") '
                   '(issue . 9686) (branch . "campaign/v9.9.9-w1") '
                   f'(implementation-sha . "{head}") (content-digest . "{digest}") '
                   '(red-first . #hasheq((command . "raco test tests/t.rkt") '
                   '(failure . "reproduction observed: the malformed digest passed the gate"))) '
                   '(focused-tests . #hasheq((result . "passed") (command . "x"))) '
                   '(format-compile . #hasheq((result . "passed") (command . "x"))) '
                   '(lint . #hasheq((result . "passed") (command . "x"))) '
                   '(fast . #hasheq((result . "passed") (command . "x"))) '
                   f'(review-artifact . "{review}") (remaining-items . ()) '
                   '(planning-sync . "current"))')
        sh('git', 'add', '-A', cwd=work)
        sh('git', 'commit', '-m', 'h2 trio', cwd=work)
        head = sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    sh('git', 'push', '-q', origin_path, 'main:main', cwd=work)
    sh('git', 'push', '-q', origin_path, f'impl:refs/heads/{WAVE_BRANCH}', cwd=work)
    sh('git', 'push', '-q', origin_path, f'{head}:refs/pull/{pr}/head', cwd=work)
    subject = base / 'repo'
    sh('git', 'clone', '-q', '--branch', 'main', origin_path, subject)
    sh('git', 'remote', 'set-url', 'origin', f'https://github.com/{SLUG}.git', cwd=subject)
    sh('git', 'config', f'url.{origin_path}.insteadOf', f'https://github.com/{SLUG}.git',
       cwd=subject)
    sh('git', 'config', 'user.email', 'fixture@example.com', cwd=subject)
    sh('git', 'config', 'user.name', 'fixture', cwd=subject)
    return {'origin': origin_path, 'work': work, 'subject': subject,
            'c0': c0, 'head': head, 'plan': plan, 'wave': wave, 'pr': pr}


def reviewer_output(kind='approved', findings=None, reviewer='fresh-reviewer-session'):
    out = {'kind': kind, 'reviewer': reviewer,
           'scope': 'independent review of the implementation, gates and provenance at the '
                    'pinned receipt head with fresh session context',
           'report': 'ran the focused and fast gates at the pinned head and verified the '
                     'provenance chain; no blocking defect found'}
    if kind == 'request-changes':
        out['findings'] = findings if findings is not None else ['focused tests fail at the head']
    if kind == 'approved':
        out['validation'] = {
            'red-first': {'command': 'raco test tests/test-wave-fixture.rkt',
                          'failure': 'reproduction observed: the malformed digest passed the '
                                     'gate before the fix'},
            'focused-tests': {'result': 'passed', 'command': 'raco test tests/test-wave.rkt'},
            'format-compile': {'result': 'passed', 'command': 'raco fmt --check x.rkt'},
            'lint': {'result': 'passed', 'command': 'raco make'},
            'fast': {'result': 'passed', 'command': 'raco test tests/fast'},
            'remaining-items': [],
            'planning-sync': 'current'}
    if kind == 'review-failed':
        out['reason'] = 'reviewer session timed out'
    return out


class AutoReviewTests(unittest.TestCase):
    def setUp(self):
        m._API_CACHE.clear()
        m._CHECKS_CACHE.clear()
        m.PROCESS_BUDGET_SEC = 10 ** 6
        m._start = time.monotonic()
        self._tmp = tempfile.TemporaryDirectory(prefix='q-auto-review-tests-')
        self.addCleanup(self._tmp.cleanup)
        self.base = Path(self._tmp.name)
        self.worlds = 0

    def world(self, **kwargs):
        self.worlds += 1
        return build_world(self.base / f'world-{self.worlds}', **kwargs)

    def campaign(self, w, *, history=0, declared=True, head=None, stage='context-ready'):
        root = build_campaign(self.base / f'campaign-{self.worlds}', w['plan'], w['wave'],
                              declared=declared)
        journal = journal_fixture(w['plan'], w['wave'], head or w['head'], history)
        journal['stage'] = stage
        write_file(root / '.planning/campaigns' / w['plan'] / f"coordinator-w{w['wave']}.json",
                   json.dumps(journal))
        return root

    def impl_staging(self, campaign, plan, wave, generation):
        return Path(campaign) / '.planning' / 'campaigns' / plan / f'review-w{wave}-g{generation}'

    def prepare(self, w, campaign, generation, stage='implementation-review'):
        return m.auto_review(w['subject'], w['plan'], w['wave'], 'prepare', stage,
                             w['head'], WAVE_BRANCH, campaign, generation,
                             self.impl_staging(campaign, w['plan'], w['wave'], generation))

    def write_reviewer(self, staging, output):
        path = Path(staging) / 'reviewer-output.json'
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(output))
        return path

    def finalize(self, w, campaign, generation, reviewer, stage='implementation-review'):
        return m.auto_review(w['subject'], w['plan'], w['wave'], 'finalize', stage,
                             w['head'], WAVE_BRANCH, campaign, generation,
                             self.impl_staging(campaign, w['plan'], w['wave'], generation),
                             reviewer_output=reviewer)

    def remote_tip(self, w):
        sh('git', 'fetch', '-q', w['origin'], f'refs/heads/{WAVE_BRANCH}', cwd=w['subject'])
        return sh('git', 'rev-parse', 'FETCH_HEAD', cwd=w['subject']).strip()

    # ------------------------------------------------------------------
    # prepare: honest gate-red template, typed passthrough, real refusals
    # ------------------------------------------------------------------

    def test_prepare_missing_trio_is_dispatchable_with_gate_red_template(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        result = self.prepare(w, campaign, 0)
        self.assertEqual(result['status'], 'awaiting-review')
        self.assertEqual(result['stage'], 'implementation-review')
        request = result['request']
        self.assertEqual(request['plan-id'], w['plan'])
        self.assertEqual(request['wave'], w['wave'])
        self.assertEqual(request['generation'], 0)
        self.assertEqual(request['head'], w['head'])
        self.assertEqual(request['branch'], WAVE_BRANCH)
        self.assertEqual(request['attempt-id'], 'attempt-5')
        self.assertEqual(request['attempt-fence'], 2)
        self.assertEqual(request['evidence'],
                         'docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd')
        self.assertEqual(request['review'], 'docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd')
        self.assertEqual(request['validation'],
                         'docs/reports/gsd-wave-validation/v9.9.9-w1.rktd')
        self.assertEqual(request['staging'], str(staging))
        self.assertEqual(request['reviewer-output'], str(staging / 'reviewer-output.json'))
        self.assertTrue(request['required-checks'] == POLICY_NAMES)
        self.assertIn('gate-red', request['prompt'])
        self.assertIn(w['head'], request['prompt'])
        self.assertTrue(request['fresh-session-required'])
        self.assertTrue(request['non-author-review-required'])
        # The template trio is genuinely gate-red: honest placeholders, no
        # fabricated APPROVED verdict, no ready-for-merge, no acceptance.
        evidence = (staging / 'docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd').read_text()
        review = (staging / 'docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd').read_text()
        validation = (staging / 'docs/reports/gsd-wave-validation/v9.9.9-w1.rktd').read_text()
        for text in (evidence, review, validation):
            self.assertNotIn('APPROVED', text)
            self.assertNotIn('ready-for-merge', text)
            self.assertIn('PENDING', text)
        self.assertIn('pending-review', evidence)
        self.assertIn('"fresh-independent-review-required"', evidence)

    def test_prepare_is_idempotent_across_restarts(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        first = self.prepare(w, campaign, 0)
        template = (staging / 'docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd').read_bytes()
        second = self.prepare(w, campaign, 0)
        self.assertEqual(first['status'], second['status'])
        self.assertEqual(first['request'], second['request'])
        self.assertEqual(template,
                         (staging / 'docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd').read_bytes())

    def test_prepare_passes_through_an_already_valid_trio(self):
        w = self.world(with_trio=True)
        campaign = self.campaign(w)
        result = self.prepare(w, campaign, 0)
        self.assertEqual(result['status'], 'reviewed')
        self.assertEqual(result['head'], w['head'])
        self.assertFalse(self.impl_staging(campaign, w['plan'], w['wave'], 0).exists())

    def test_prepare_refuses_source_drift_without_dispatching(self):
        w = self.world()
        campaign = self.campaign(w)
        write_file(w['work'] / 'src/other.txt', 'drift\n')
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-m', 'drift', cwd=w['work'])
        tip = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{tip}:refs/heads/{WAVE_BRANCH}', cwd=w['work'])
        with self.assertRaises(m.Pending) as caught:
            self.prepare(w, campaign, 0)
        self.assertIn('non-evidence paths', str(caught.exception))
        self.assertFalse(self.impl_staging(campaign, w['plan'], w['wave'], 0).exists())

    def test_prepare_generation_uses_repair_paths_and_preserves_old_generations(self):
        w = self.world()
        campaign = self.campaign(w, history=1)
        result = self.prepare(w, campaign, 1)
        self.assertEqual(result['status'], 'awaiting-review')
        request = result['request']
        self.assertEqual(request['generation'], 1)
        self.assertEqual(request['evidence'],
                         'docs/reports/gsd-wave-evidence/v9.9.9-w1-r1.rktd')
        self.assertEqual(request['review'], 'docs/reports/gsd-wave-reviews/v9.9.9-w1-r1.rktd')
        self.assertEqual(request['staging'],
                         str(self.impl_staging(campaign, w['plan'], w['wave'], 1)))
        # a prior generation's staging family is a different directory and is
        # never written by this dispatch
        self.assertNotEqual(request['staging'],
                            str(self.impl_staging(campaign, w['plan'], w['wave'], 0)))
        self.assertFalse(self.impl_staging(campaign, w['plan'], w['wave'], 0).exists())

    # ------------------------------------------------------------------
    # finalize: approval only through the unchanged strict gate
    # ------------------------------------------------------------------

    def test_finalize_approved_commits_evidence_only_trio_and_passes_strict_gate(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        reviewer = self.write_reviewer(staging, reviewer_output('approved'))
        result = self.finalize(w, campaign, 0, reviewer)
        self.assertEqual(result['status'], 'reviewed')
        self.assertEqual(result['head'], w['head'])
        self.assertEqual(result['reviewed-sha'], w['head'])
        self.assertEqual(result['review-artifact'], 'docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd')
        # the receipt branch advanced by exactly one evidence-only commit
        tip = self.remote_tip(w)
        self.assertNotEqual(tip, w['head'])
        self.assertEqual(tip, result['tip'])
        changed = sh('git', 'diff', '--name-only', f"{w['head']}..{tip}", cwd=w['subject']).split()
        self.assertEqual(sorted(changed),
                         sorted(['docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd',
                                 'docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd',
                                 'docs/reports/gsd-wave-validation/v9.9.9-w1.rktd']))
        # the UNCHANGED review path re-validates the committed trio at the
        # same receipt head: only the strict gate produced this verdict
        self.assertEqual(m.review(w['subject'], w['plan'], w['wave'], w['head'], WAVE_BRANCH,
                                  campaign_root=campaign)['status'], 'reviewed')
        committed = sh('git', 'show', f'{tip}:docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd',
                       cwd=w['subject'])
        self.assertIn('ready-for-merge', committed)
        self.assertIn(w['head'], committed)
        committed_review = sh('git', 'show',
                              f'{tip}:docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd',
                              cwd=w['subject'])
        self.assertIn('APPROVED', committed_review)
        self.assertIn('fresh-reviewer-session', committed_review)

    def test_strict_gate_failure_does_not_publish_candidate(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        output = reviewer_output('approved')
        output['validation']['remaining-items'] = [{'classification': 'blocking'}]
        reviewer = self.write_reviewer(staging, output)
        result = self.finalize(w, campaign, 0, reviewer)
        self.assertEqual(result['status'], 'review-infra')
        self.assertIn('strict', result['reason'])
        self.assertEqual(self.remote_tip(w), w['head'])

    def test_published_rejection_replays_without_journal_marker_or_staging(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        findings = ['  exact finding with whitespace  ', 'a=b; findings: exact\nsecond line']
        reviewer = self.write_reviewer(staging, reviewer_output('request-changes', findings))
        published = self.finalize(w, campaign, 0, reviewer)
        import shutil
        shutil.rmtree(staging)
        journal_path = campaign / '.planning/campaigns' / w['plan'] / 'coordinator-w1.json'
        before = journal_path.read_bytes()
        replay = self.prepare(w, campaign, 0)
        self.assertEqual(replay, published)
        self.assertEqual(replay['findings'], findings)
        self.assertEqual(journal_path.read_bytes(), before)
        self.assertFalse(staging.exists())
        self.assertEqual(self.remote_tip(w), published['tip'])

    def test_same_head_receipt_and_generation_drift_refuse_finalize(self):
        for mode in ('attempt-id', 'attempt-fence', 'generation'):
            with self.subTest(mode=mode):
                w = self.world()
                campaign = self.campaign(w)
                staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
                self.prepare(w, campaign, 0)
                path = campaign / '.planning/campaigns' / w['plan'] / 'coordinator-w1.json'
                journal = json.loads(path.read_text())
                if mode == 'generation':
                    journal['receipt-history'].append(dict(journal['receipt']))
                else:
                    journal['receipt'][mode] = 'new-attempt' if mode == 'attempt-id' else 3
                path.write_text(json.dumps(journal))
                output = self.write_reviewer(staging, reviewer_output('request-changes'))
                with self.assertRaises(m.Pending):
                    self.finalize(w, campaign, 0, output)
                self.assertEqual(self.remote_tip(w), w['head'])

    def test_next_generation_does_not_replay_or_rewrite_same_head_prior_rejection(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        output = self.write_reviewer(staging, reviewer_output('request-changes'))
        published = self.finalize(w, campaign, 0, output)
        rel = 'docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd'
        old_blob = m.blob_or_none(w['subject'], published['tip'], rel)
        old_staging = {p.relative_to(staging): p.read_bytes()
                       for p in staging.rglob('*') if p.is_file()}
        path = campaign / '.planning/campaigns' / w['plan'] / 'coordinator-w1.json'
        journal = json.loads(path.read_text())
        journal['receipt-history'].append(dict(journal['receipt']))
        journal['receipt']['attempt-id'] = 'new-attempt'
        journal['receipt']['attempt-fence'] = 3
        path.write_text(json.dumps(journal))
        request = self.prepare(w, campaign, 1)
        self.assertEqual(request['status'], 'awaiting-review')
        self.assertEqual(request['request']['generation'], 1)
        self.assertEqual(request['request']['attempt-id'], 'new-attempt')
        self.assertEqual(m.blob_or_none(w['subject'], self.remote_tip(w), rel), old_blob)
        self.assertEqual({p.relative_to(staging): p.read_bytes()
                          for p in staging.rglob('*') if p.is_file()}, old_staging)
        with self.assertRaises(m.Pending):
            self.prepare(w, campaign, 0)

    def test_recovery_refuses_reverted_source_commit_in_review_ancestry(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        output = self.write_reviewer(staging, reviewer_output('request-changes'))
        published = self.finalize(w, campaign, 0, output)
        sh('git', 'fetch', '-q', w['origin'], WAVE_BRANCH, cwd=w['work'])
        sh('git', 'checkout', '--detach', 'FETCH_HEAD', cwd=w['work'])
        write_file(w['work'] / 'src/file.txt', 'unverified source\n')
        sh('git', 'add', 'src/file.txt', cwd=w['work'])
        sh('git', 'commit', '-qm', 'source drift', cwd=w['work'])
        sh('git', 'revert', '--no-edit', 'HEAD', cwd=w['work'])
        sh('git', 'push', '-q', w['origin'], f'HEAD:refs/heads/{WAVE_BRANCH}', cwd=w['work'])
        with self.assertRaises(m.Pending) as caught:
            self.prepare(w, campaign, 0)
        self.assertIn('ancestry', str(caught.exception))

    def test_rejection_recovery_refuses_wrong_receipt_and_malformed_rejection(self):
        for field, value in [('attempt-id', 'other'), ('attempt-fence', 3),
                             ('generation', 1), ('head', 'e' * 40),
                             ('branch', 'campaign/other'), ('findings', []),
                             ('reviewer', 'PENDING'), ('verdict', 'REJECTED')]:
            with self.subTest(field=field):
                w = self.world()
                campaign = self.campaign(w)
                staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
                self.prepare(w, campaign, 0)
                output = self.write_reviewer(staging, reviewer_output('request-changes'))
                published = self.finalize(w, campaign, 0, output)
                rel = 'docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd'
                datum = m.read_datum(m.materialize(w['subject'], published['tip'], rel,
                                                  self.base / 'materialized'))
                datum[field] = value
                candidate = m._commit_candidate(w['subject'], published['tip'],
                                                {rel: datum}, 'malformed rejection fixture')
                m._publish_candidate(w['subject'], WAVE_BRANCH, published['tip'], candidate)
                before = self.remote_tip(w)
                with self.assertRaises(m.Pending):
                    self.prepare(w, campaign, 0)
                self.assertEqual(self.remote_tip(w), before)

    def test_finalize_approved_binds_controller_digest_not_reviewer_prose(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        output = reviewer_output('approved')
        output['content-digest'] = 'f' * 64  # reviewer-prose digest must be ignored
        reviewer = self.write_reviewer(staging, output)
        result = self.finalize(w, campaign, 0, reviewer)
        self.assertEqual(result['status'], 'reviewed')
        tip = self.remote_tip(w)
        committed = sh('git', 'show', f'{tip}:docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd',
                       cwd=w['subject'])
        expected = m.digest(w['subject'], 'refs/remotes/origin/main', tip)
        self.assertIn(expected, committed)
        self.assertNotIn('f' * 64, committed)

    def test_finalize_request_changes_persists_findings_and_never_weakens_the_gate(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        findings = ['focused tests fail at the pinned head',
                    'the digest law is violated by the touched module']
        reviewer = self.write_reviewer(staging, reviewer_output('request-changes', findings))
        result = self.finalize(w, campaign, 0, reviewer)
        self.assertEqual(result['status'], 'review-rejected')
        self.assertEqual(result['stage'], 'implementation-review')
        self.assertEqual(result['head'], w['head'])
        self.assertEqual(result['branch'], WAVE_BRANCH)
        self.assertEqual(result['attempt-id'], 'attempt-5')
        self.assertEqual(result['attempt-fence'], 2)
        self.assertFalse(result['postmerge'])
        self.assertEqual(result['findings'], findings)
        # the findings are durable: committed at the review artifact path
        tip = self.remote_tip(w)
        rejected = sh('git', 'show', f'{tip}:docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd',
                      cwd=w['subject'])
        self.assertIn('REQUEST_CHANGES', rejected)
        self.assertIn(w['head'], rejected)
        self.assertIn(findings[0], rejected)
        # the strict APPROVED gate refuses the rejected verdict: the gate
        # itself is never weakened to carry a rejection through
        with self.assertRaises(m.Pending):
            m.review(w['subject'], w['plan'], w['wave'], w['head'], WAVE_BRANCH,
                     campaign_root=campaign)

    def test_finalize_malformed_reviewer_output_is_review_infra(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        bad_outputs = [
            reviewer_output('bogus'),
            reviewer_output('request-changes', []),
            reviewer_output('request-changes', ['']),
            reviewer_output('approved', reviewer='PENDING'),
            reviewer_output('approved', reviewer='pending-independent-review'),
            dict(reviewer_output('approved'), scope='too short'),
            dict(reviewer_output('approved'), validation={'red-first': 'not a hash'}),
        ]
        for output in bad_outputs:
            reviewer = self.write_reviewer(staging, output)
            result = self.finalize(w, campaign, 0, reviewer)
            self.assertEqual(result['status'], 'review-infra', output)
            self.assertTrue(result['reason'].strip(), output)
        # nothing was committed for any malformed output
        self.assertEqual(self.remote_tip(w), w['head'])

    def test_finalize_review_failed_session_is_review_infra_without_finalize(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        reviewer = self.write_reviewer(staging, reviewer_output('review-failed'))
        result = self.finalize(w, campaign, 0, reviewer)
        self.assertEqual(result['status'], 'review-infra')
        self.assertIn('reviewer session failed', result['reason'])
        self.assertEqual(self.remote_tip(w), w['head'])

    def test_finalize_refuses_missing_dispatch_request(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        reviewer = self.write_reviewer(staging, reviewer_output('approved'))
        with self.assertRaises(m.Pending) as caught:
            self.finalize(w, campaign, 0, reviewer)
        self.assertIn('no review dispatch request', str(caught.exception))

    def test_finalize_refuses_tampered_dispatch_request_identity(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        request_path = staging / 'request.json'
        request = json.loads(request_path.read_text())
        request['head'] = 'e' * 40
        request_path.write_text(json.dumps(request))
        reviewer = self.write_reviewer(staging, reviewer_output('approved'))
        with self.assertRaises(m.Pending) as caught:
            self.finalize(w, campaign, 0, reviewer)
        self.assertIn('does not match the durable receipt', str(caught.exception))

    def test_finalize_remote_divergence_is_review_infra_and_never_overwrites(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        # a concurrent publisher advances the remote branch while the fresh
        # reviewer ran: the pinned non-force push must refuse
        write_file(w['work'] / 'docs/reports/gsd-wave-evidence/foreign.rktd', 'foreign\n')
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-m', 'concurrent', cwd=w['work'])
        foreign = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], f'{foreign}:refs/heads/{WAVE_BRANCH}', cwd=w['work'])
        reviewer = self.write_reviewer(staging, reviewer_output('approved'))
        result = self.finalize(w, campaign, 0, reviewer)
        self.assertEqual(result['status'], 'review-infra')
        self.assertEqual(self.remote_tip(w), foreign)

    def test_finalize_refuses_receipt_drift_before_committing(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = self.impl_staging(campaign, w['plan'], w['wave'], 0)
        self.prepare(w, campaign, 0)
        # the journal receipt moved to a different head mid-review
        journal_path = (campaign / '.planning/campaigns' / w['plan'] /
                        f"coordinator-w{w['wave']}.json")
        journal = json.loads(journal_path.read_text())
        journal['receipt']['head'] = 'e' * 40
        journal_path.write_text(json.dumps(journal))
        reviewer = self.write_reviewer(staging, reviewer_output('approved'))
        with self.assertRaises(m.Pending) as caught:
            self.finalize(w, campaign, 0, reviewer)
        self.assertIn('does not match the durable receipt', str(caught.exception))
        self.assertEqual(self.remote_tip(w), w['head'])

    # ------------------------------------------------------------------
    # post-merge binding review: staged pending trio finalization
    # ------------------------------------------------------------------

    def binding_world(self, *, generation=0):
        """Impl head h carries the source trio; merged squash M on main; a
        staged binding draft trio (pending-review placeholders) in the
        campaign-local binding staging directory."""
        w = self.world()
        plan, wave, pr = w['plan'], w['wave'], w['pr']
        digest = m.digest(w['work'], w['c0'], w['head'])
        label = f'{plan}-w{wave}'
        for rel, text in (
            (f'docs/reports/gsd-wave-evidence/{label}.rktd',
             '#hasheq((schema-version . 2) (plan-id . "%s") (milestone . 895) (wave . "W%d") '
             '(issue . 9686) (status . "ready-for-merge") '
             '(implementation-sha . "%s") (content-digest . "%s") '
             '(required-checks . ("' % (plan, wave, w['head'], digest) + '" "'.join(POLICY_NAMES)
             + '")) (review-artifact . "docs/reports/gsd-wave-reviews/%s.rktd") '
             '(validation-artifact . "docs/reports/gsd-wave-validation/%s.rktd"))' % (label, label)),
            (f'docs/reports/gsd-wave-reviews/{label}.rktd',
             '#hasheq((reviewer . "source-reviewer") (verdict . "APPROVED") '
             '(timestamp . "2026-09-14T16:00:00Z") (reviewed-sha . "%s") '
             '(content-digest . "%s") '
             '(scope . "independent read-only review of the wave implementation and gate '
             'evidence at the reviewed head") '
             '(report . "reviewed the commit chain, ran the focused and fast gates, and found '
             'no blocking defect at the reviewed head"))' % (w['head'], digest)),
            (f'docs/reports/gsd-wave-validation/{label}.rktd',
             '#hasheq((status . "current") (milestone . 895) (wave . "W%d") (issue . 9686) '
             '(branch . "%s") (implementation-sha . "%s") (content-digest . "%s") '
             '(red-first . #hasheq((command . "raco test tests/t.rkt") '
             '(failure . "reproduction observed: the malformed digest passed the gate"))) '
             '(focused-tests . #hasheq((result . "passed") (command . "x"))) '
             '(format-compile . #hasheq((result . "passed") (command . "x"))) '
             '(lint . #hasheq((result . "passed") (command . "x"))) '
             '(fast . #hasheq((result . "passed") (command . "x"))) '
             '(review-artifact . "docs/reports/gsd-wave-reviews/%s.rktd") '
             '(remaining-items . ()) (planning-sync . "current"))'
             % (wave, WAVE_BRANCH, w['head'], digest, label)),
        ):
            write_file(w['work'] / rel, text)
        sh('git', 'add', '-A', cwd=w['work'])
        sh('git', 'commit', '-m', 'h2 source trio', cwd=w['work'])
        head = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'checkout', '-q', 'main', cwd=w['work'])
        sh('git', 'merge', '--squash', 'impl', cwd=w['work'])
        sh('git', 'commit', '-m', 'M squash', cwd=w['work'])
        merge = sh('git', 'rev-parse', 'HEAD', cwd=w['work']).strip()
        sh('git', 'push', '-q', w['origin'], 'main:main', cwd=w['work'])
        sh('git', 'push', '-q', '--force', w['origin'],
           f'{head}:refs/heads/{WAVE_BRANCH}', f'{head}:refs/pull/{pr}/head', cwd=w['work'])
        w.update({'head': head, 'merge': merge})
        campaign = self.campaign(w, history=generation, head=head)
        staging = m.binding_staging_path(campaign, plan, wave, generation)
        staging.mkdir(parents=True, exist_ok=True)
        pr_payload = {'number': pr, 'merged': True, 'merge_commit_sha': merge,
                      'merged_at': '2026-09-14T16:23:06Z',
                      'head': {'sha': head, 'ref': WAVE_BRANCH,
                               'repo': {'full_name': SLUG}},
                      'base': {'ref': 'main', 'sha': w['c0'],
                               'repo': {'full_name': SLUG}}}
        request = m.draft_request({'wave': f'W{wave}', 'milestone': 895, 'issue': 9686},
                                  plan, wave, pr_payload,
                                  m.binding_branch(plan, wave, generation), POLICY_NAMES,
                                  generation)
        payload = staging / 'request.json'
        payload.write_text(json.dumps(request))
        m.command(['racket', str(m.HERE / 'gsd-binding-data.rkt'), 'prepare',
                   str(payload), str(staging)])
        payload.unlink()
        return w, campaign, staging

    def test_binding_prepare_pending_draft_is_dispatchable(self):
        w, campaign, staging = self.binding_world()
        result = m.auto_review(w['subject'], w['plan'], w['wave'], 'prepare', 'binding-review',
                               w['head'], WAVE_BRANCH, campaign, 0, staging)
        self.assertEqual(result['status'], 'awaiting-review')
        request = result['request']
        self.assertEqual(request['kind'], 'binding-review')
        self.assertEqual(request['head'], w['head'])
        self.assertEqual(request['branch'], WAVE_BRANCH)
        self.assertEqual(request['merge-sha'], w['merge'])
        self.assertEqual(request['attempt-id'], 'attempt-5')
        self.assertEqual(request['content-digest'], EMPTY_SHA)
        self.assertEqual(request['staging'], str(staging))
        self.assertIn('pending-review', request['prompt'])
        self.assertTrue(request['fresh-session-required'])
        self.assertTrue(request['non-author-review-required'])

    def test_binding_finalize_approved_finalizes_staged_trio_through_strict_gate(self):
        w, campaign, staging = self.binding_world()
        m.auto_review(w['subject'], w['plan'], w['wave'], 'prepare', 'binding-review',
                      w['head'], WAVE_BRANCH, campaign, 0, staging)
        reviewer = self.write_reviewer(staging, reviewer_output('approved'))
        result = m.auto_review(w['subject'], w['plan'], w['wave'], 'finalize', 'binding-review',
                               w['head'], WAVE_BRANCH, campaign, 0, staging,
                               reviewer_output=reviewer)
        self.assertEqual(result['status'], 'reviewed')
        self.assertEqual(result['merge-sha'], w['merge'])
        evidence = m.read_datum(staging / 'docs/reports/gsd-wave-evidence' /
                                f"{w['plan']}-w{w['wave']}.rktd")
        self.assertEqual(evidence['status'], 'ready-for-merge')
        self.assertEqual(evidence['content-digest'], EMPTY_SHA)
        review = m.read_datum(staging / 'docs/reports/gsd-wave-reviews' /
                              f"{w['plan']}-w{w['wave']}.rktd")
        self.assertEqual(review['verdict'], 'APPROVED')
        self.assertEqual(review['reviewed-sha'], w['merge'])
        # the unchanged binding-review path accepts the finalized trio
        self.assertEqual(m.binding_review(w['subject'], w['plan'], w['wave'], staging,
                                          campaign, WAVE_BRANCH)['status'], 'reviewed')

    def test_binding_finalize_request_changes_is_postmerge_artifact_repair(self):
        w, campaign, staging = self.binding_world()
        m.auto_review(w['subject'], w['plan'], w['wave'], 'prepare', 'binding-review',
                      w['head'], WAVE_BRANCH, campaign, 0, staging)
        findings = ['staged binding review never covered the published implementation head']
        reviewer = self.write_reviewer(staging, reviewer_output('request-changes', findings))
        result = m.auto_review(w['subject'], w['plan'], w['wave'], 'finalize', 'binding-review',
                               w['head'], WAVE_BRANCH, campaign, 0, staging,
                               reviewer_output=reviewer)
        self.assertEqual(result['status'], 'review-rejected')
        self.assertEqual(result['stage'], 'binding-review')
        self.assertTrue(result['postmerge'])
        self.assertEqual(result['head'], w['head'])
        self.assertEqual(result['branch'], WAVE_BRANCH)
        self.assertEqual(result['attempt-id'], 'attempt-5')
        self.assertEqual(result['attempt-fence'], 2)
        self.assertEqual(result['findings'], findings)
        # findings persist as a trusted controller-authored staging artifact;
        # the staged draft trio itself is untouched (still pending-review)
        rejected = m.read_datum(staging / 'review-rejected.rktd')
        self.assertEqual(rejected['verdict'], 'REQUEST_CHANGES')
        self.assertEqual(rejected['reviewed-sha'], w['merge'])
        self.assertEqual(rejected['findings'], findings)
        self.assertEqual(m.binding_review(w['subject'], w['plan'], w['wave'], staging,
                                          campaign, WAVE_BRANCH)['status'], 'pending-review')

    def test_binding_malformed_reviewer_output_is_review_infra(self):
        w, campaign, staging = self.binding_world()
        m.auto_review(w['subject'], w['plan'], w['wave'], 'prepare', 'binding-review',
                      w['head'], WAVE_BRANCH, campaign, 0, staging)
        reviewer = self.write_reviewer(staging, reviewer_output('approved', reviewer='TBD'))
        result = m.auto_review(w['subject'], w['plan'], w['wave'], 'finalize', 'binding-review',
                               w['head'], WAVE_BRANCH, campaign, 0, staging,
                               reviewer_output=reviewer)
        self.assertEqual(result['status'], 'review-infra')
        self.assertEqual(m.binding_review(w['subject'], w['plan'], w['wave'], staging,
                                          campaign, WAVE_BRANCH)['status'], 'pending-review')

    def test_binding_prepare_refuses_missing_staging(self):
        w = self.world()
        campaign = self.campaign(w)
        staging = m.binding_staging_path(campaign, w['plan'], w['wave'], 0)
        with self.assertRaises(m.Pending) as caught:
            m.auto_review(w['subject'], w['plan'], w['wave'], 'prepare', 'binding-review',
                          w['head'], WAVE_BRANCH, campaign, 0, staging)
        self.assertIn('binding staging', str(caught.exception))


if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == '--fixture':
        BRANCH = WAVE_BRANCH = 'campaign/production-review'
        base, plan, wave, generation = Path(sys.argv[2]), sys.argv[3], int(sys.argv[4]), int(sys.argv[5])
        w = build_world(base, plan=plan, wave=wave)
        sh('git', 'checkout', '-q', '-b', WAVE_BRANCH, w['head'], cwd=w['subject'])
        root = build_campaign(base, plan, wave)
        journal = journal_fixture(plan, wave, w['head'], generation, branch=WAVE_BRANCH)
        journal['receipt']['repo'] = str(w['subject'])
        journal['receipt']['tree'] = sh('git', 'rev-parse', w['head'] + '^{tree}', cwd=w['subject']).strip()
        write_file(root / '.planning/campaigns' / plan / f'coordinator-w{wave}.json', json.dumps(journal))
        print(json.dumps({'root': str(root), 'repo': str(w['subject']), 'receipt': journal['receipt']}))
    else:
        unittest.main()
