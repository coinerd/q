"""Real-Git owned audit consumer fixtures, not live W1/model acceptance.

Positive path: an actual compiled implementation with wrong behavior plus a
failing focused test produces an authentic registered behavioral RED at the
committed pre-fix head; the SAME function is then fixed, the real frozen
Verify chain (raco fmt -i, raco make, honest fixture run-tests/lint-all
runners, --suite fast) is executed by the owned registry, and the approved
audit — exact audit-ack plus meaningful red-ack of the retained raw failure
output — finalizes through the unchanged strict gate and publishes with ZERO
duplicate Verify launches. F11 (operator merge authorization) remains a
separate gate and is never manufactured. Missing RED fails closed as typed
review-infra and is never consumed as a rejection or approval.
"""
import copy
import importlib.util
import json
import shutil
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

def module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result

m = module('owned_delivery', ROOT / 'scripts/gsd-delivery.py')
f = module('manual_fixture', ROOT / 'tests/test-gsd-delivery-auto-review.py')
HELPER = ROOT / 'tests/helpers/owned-audit-fixture.rkt'

# The frozen Verify chain spells REAL repository commands only (the same
# grammar as the frozen W1 declaration): in-place canonical format, compile,
# the focused positional test stage, the full fast suite through the real
# --suite CLI flag, and the whole-repository lint entrypoint. There is no
# shimmed or fictional CLI anywhere in this fixture.
CHAIN = ('cd <project-base>/q && raco fmt -i tests/w1-fixture.rkt src/gsd-fixture-widget.rkt'
         ' && raco make tests/w1-fixture.rkt'
         ' && racket scripts/run-tests.rkt tests/w1-fixture.rkt'
         ' && racket scripts/run-tests.rkt --suite fast'
         ' && racket scripts/lint-all.rkt')

RUNNER = '''#lang racket/base
;; Fixture gate runner for the owned-audit integration fixture ONLY.
;; Runs the named focused test files (or, for --suite fast, every tests/*.rkt
;; file in this fixture repository) through the REAL `raco test` runner,
;; fail-fast, propagating the real exit code. This is NOT the repository's
;; production test runner.
(require racket/list racket/system)
(define args (vector->list (current-command-line-arguments)))
(define files
  (cond
    [(null? args) (error 'run-tests "fixture runner requires test files or --suite fast")]
    [(equal? (car args) "--suite")
     (unless (equal? (cdr args) '("fast"))
       (error 'run-tests "fixture runner supports only --suite fast"))
     (sort (for/list ([p (directory-list "tests")] #:when (regexp-match? #rx"[.]rkt$" p))
             (build-path "tests" p))
           path<?)]
    [else (map string->path args)]))
(for ([path (in-list files)])
  (define code (system*/exit-code (find-executable-path "raco") "test" (path->string path)))
  (unless (zero? code) (exit code)))
'''

LINT = '''#lang racket/base
;; Fixture whole-repository lint for the owned-audit integration fixture
;; ONLY: real checks (no tabs, no trailing whitespace, no line over 100
;; columns) over this fixture's Racket sources. NOT the production linter.
(require racket/file racket/list racket/string)
(define files
  (sort (append (for/list ([p (directory-list "tests")] #:when (regexp-match? #rx"[.]rkt$" p))
                  (build-path "tests" p))
                (for/list ([p (directory-list "src")] #:when (regexp-match? #rx"[.]rkt$" p))
                  (build-path "src" p)))
        path<?))
(define (file-violations path)
  (for/list ([line (in-list (file->lines path))]
             [n (in-naturals 1)]
             #:when (or (string-contains? line "\\t")
                        (regexp-match? #rx"[[:space:]]$" line)
                        (> (string-length line) 100)))
    (format "~a:~a" path n)))
(define bad
  (for*/fold ([acc '()]) ([path (in-list files)])
    (append (file-violations path) acc)))
(unless (null? bad)
  (eprintf "fixture lint failures:\\n~a\\n" (string-join bad "\\n"))
  (exit 1))
'''

WIDGET_BUGGY = '''#lang racket/base
;; Fixture widget: deliberately wrong scoring rule (pre-fix RED state).
(provide widget-score)
(define (widget-score n)
  (* n 2))
'''

WIDGET_FIXED = '''#lang racket/base
;; Fixture widget: correct scoring rule (post-fix green state).
(provide widget-score)
(define (widget-score n)
  (* n n))
'''

TEST = '''#lang racket/base
(require rackunit "../src/gsd-fixture-widget.rkt")
(module+ test
  (check-equal? (widget-score 3) 9 "widget-score must square the input")
  (check-equal? (widget-score 0) 0 "widget-score must square zero"))
'''


def build_owned_world(base, plan, *, with_red=True):
    """Real Git fixture: buggy committed implementation + failing focused
    test at head1 (RED), then the same function fixed at head2 (green)."""
    w = f.build_world(base / 'world')
    work = w['work']
    f.write_file(work / '.gitignore', 'compiled/\n')
    f.write_file(work / 'src/gsd-fixture-widget.rkt', WIDGET_BUGGY)
    f.write_file(work / 'tests/w1-fixture.rkt', TEST)
    f.write_file(work / 'scripts/run-tests.rkt', RUNNER)
    f.write_file(work / 'scripts/lint-all.rkt', LINT)
    # Canonicalize formatting BEFORE committing: the frozen `raco fmt -i`
    # stage then runs for real as a genuine no-op (clean before/after).
    subprocess.run(['raco', 'fmt', '-i', 'tests/w1-fixture.rkt',
                    'src/gsd-fixture-widget.rkt', 'scripts/run-tests.rkt',
                    'scripts/lint-all.rkt'], cwd=str(work), check=True)
    f.sh('git', 'add', '--', '.', cwd=work)
    f.sh('git', 'commit', '-m', 'fixture: buggy widget + failing focused test', cwd=work)
    head1 = f.sh('git', 'rev-parse', 'HEAD', cwd=work).strip()
    f.sh('git', 'push', '-q', w['origin'], f'impl:refs/heads/{f.WAVE_BRANCH}', cwd=work)
    campaign = f.build_campaign(
        base, plan, 1,
        heading='# Plan: Fixture\n\n## Waves\n\n- [Inbox] W1: Fixture → waves/W1-fixture.md',
        wave_override=('# W1: fixture\n\nBranch: `%s`; GitHub issue '
                       '[#9686](https://github.com/owner/repo/issues/9686), '
                       'milestone #895.\n\n## Verify\n%s\n' % (f.WAVE_BRANCH, CHAIN)))
    shutil.rmtree(campaign / '.planning/campaigns' / plan / 'plan-snapshot')
    f.sh('git', 'fetch', '-q', 'origin', f'refs/heads/{f.WAVE_BRANCH}', cwd=w['subject'])
    f.sh('git', 'worktree', 'add', '-b', f.WAVE_BRANCH, campaign / 'q', head1, cwd=w['subject'])
    if with_red:
        run = subprocess.run(['racket', str(HELPER), 'red', str(campaign), plan],
                             capture_output=True, text=True, timeout=300)
        if run.returncode:
            raise AssertionError(run.stdout + run.stderr)
    q = campaign / 'q'
    f.write_file(q / 'src/gsd-fixture-widget.rkt', WIDGET_FIXED)
    subprocess.run(['raco', 'fmt', '-i', 'src/gsd-fixture-widget.rkt'], cwd=str(q), check=True)
    f.sh('git', 'add', '--', 'src', cwd=q)
    f.sh('git', 'commit', '-m', 'fixture: correct widget scoring (fix)', cwd=q)
    head2 = f.sh('git', 'rev-parse', 'HEAD', cwd=q).strip()
    f.sh('git', 'push', '-q', w['origin'], f'{f.WAVE_BRANCH}:refs/heads/{f.WAVE_BRANCH}', cwd=q)
    run = subprocess.run(['racket', str(HELPER), 'produce', str(campaign), plan],
                         capture_output=True, text=True, timeout=600)
    if run.returncode:
        raise AssertionError(run.stdout + run.stderr)
    staging = campaign / '.planning/campaigns' / plan / 'review-w1-g0'
    return {'world': w, 'campaign': campaign, 'head1': head1, 'head2': head2,
            'plan': plan, 'with_red': with_red, 'staging': staging}


class OwnedAuditTests(unittest.TestCase):
    def test_existing_prompt_audits_instead_of_replaying_owned_gates(self):
        paths = dict(evidence='e', review='r', validation='v')
        for stage in m.AUTO_REVIEW_STAGES:
            prompt = m._review_prompt(stage, 'a' * 64, 1, 'b' * 40, 'campaign/w1',
                                      paths, '/staging', 'c' * 64, ['lint'])
            self.assertIn('AUDIT', prompt)
            self.assertIn('Do not rerun', prompt)
            self.assertIn('600 seconds', prompt)
            self.assertNotIn('Run the genuine gates', prompt)

    def test_no_model_gate_fact_fallback_or_unbound_ack(self):
        request = dict(zip(m.AUDIT_KEYS, ['x'] * len(m.AUDIT_KEYS)),
                       **{'review-mode': m.OWNED_AUDIT_MODE})
        out = f.reviewer_output()
        with self.assertRaises(m._ReviewInfra):
            m.validate_reviewer_output(out, request)
        out.pop('validation')
        out['audit-ack'] = {'mode': m.OWNED_AUDIT_MODE, 'verification-origin': 'audited-owned',
                            'source-read': True, 'raw-output-read': True,
                            'binding': m._audit_binding(request)}
        self.assertEqual(m.validate_reviewer_output(out, request)['kind'], 'approved')
        for key in m.AUDIT_KEYS:
            bad = copy.deepcopy(out)
            bad['audit-ack']['binding'][key] = 'stale'
            with self.assertRaises(m._ReviewInfra, msg=key):
                m.validate_reviewer_output(bad, request)
        for key in ('source-read', 'raw-output-read', 'verification-origin', 'mode'):
            bad = copy.deepcopy(out)
            bad['audit-ack'][key] = False
            with self.assertRaises(m._ReviewInfra, msg=key):
                m.validate_reviewer_output(bad, request)

    def test_red_ack_is_meaningful_only_for_a_linked_red(self):
        request = dict(zip(m.AUDIT_KEYS, ['x'] * len(m.AUDIT_KEYS)),
                       **{'review-mode': m.OWNED_AUDIT_MODE,
                          'owned-authority': {'red': {'reference': {'sha256': 'r' * 64}}}})
        out = f.reviewer_output()
        out.pop('validation')
        ack = {'mode': m.OWNED_AUDIT_MODE, 'verification-origin': 'audited-owned',
               'source-read': True, 'raw-output-read': True,
               'binding': m._audit_binding(request)}
        out['audit-ack'] = dict(ack, **{'red-ack': {'proof-sha256': 'r' * 64,
                                                    'raw-output-read': True}})
        self.assertEqual(m.validate_reviewer_output(out, request)['kind'], 'approved')
        for bad_ack in ({'proof-sha256': 'wrong', 'raw-output-read': True},
                        {'proof-sha256': 'r' * 64, 'raw-output-read': False},
                        {'proof-sha256': 'r' * 64}):
            out['audit-ack'] = dict(ack, **{'red-ack': bad_ack})
            with self.assertRaises(m._ReviewInfra):
                m.validate_reviewer_output(out, request)
        out['audit-ack'] = dict(ack, **{'red-ack': {'proof-sha256': 'r' * 64,
                                                    'raw-output-read': True}})
        no_red = {k: v for k, v in request.items() if k != 'owned-authority'}
        with self.assertRaises(m._ReviewInfra):
            m.validate_reviewer_output(out, no_red)


class _OwnedWorldTests(unittest.TestCase):
    """Shared real-Git fixture plumbing. Each concrete class builds exactly
    the independent worlds its budget needs (a published verdict ends a
    world's generation under the explicit reject-retry law, so approval,
    rejection and the no-red world can never share one fixture)."""
    SPECS = ()

    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory(prefix='q-owned-audit-')
        cls.base = Path(cls.tmp.name)
        cls.fixtures = {}
        for name, plan, with_red in cls.SPECS:
            cls.fixtures[name] = build_owned_world(cls.base / name, plan, with_red=with_red)
        cls.original_authority = m._owned_authority

        def authority(root, plan, wave, staging):
            try:
                return json.loads(f.sh('racket', HELPER, 'authority', root, plan, staging))
            except (AssertionError, ValueError) as error:
                raise m._ReviewInfra('invalid owned fixture proof: ' + str(error)) from error
        m._owned_authority = authority
        m.PROCESS_BUDGET_SEC = 10 ** 6
        m._start = time.monotonic()
        for fixture in cls.fixtures.values():
            prepared = cls.prepare(fixture)
            if prepared['status'] != 'awaiting-review':
                raise AssertionError(prepared)
            fixture['request'] = prepared['request']

    @classmethod
    def tearDownClass(cls):
        m._owned_authority = cls.original_authority
        cls.tmp.cleanup()

    @classmethod
    def prepare(cls, fixture):
        w = fixture['world']
        return m.auto_review(w['subject'], fixture['plan'], 1, 'prepare',
                             'implementation-review', fixture['head2'], f.WAVE_BRANCH,
                             fixture['campaign'], 0, fixture['staging'])

    def finalize(self, fixture, output):
        path = fixture['staging'] / 'reviewer-output.json'
        path.write_text(json.dumps(output))
        return m.auto_review(fixture['world']['subject'], fixture['plan'], 1, 'finalize',
                             'implementation-review', fixture['head2'], f.WAVE_BRANCH,
                             fixture['campaign'], 0, fixture['staging'], path)

    def audit_output(self, fixture, kind='approved'):
        authority = fixture['request']['owned-authority']
        root = fixture['staging'] / authority['bundle']
        manifest = json.loads((root / authority['reference']['path']).read_text())
        (root / manifest['log']['path']).read_text()
        red = authority['red']
        red_log = (root / red['log-reference']['path']).read_text()
        self.assertIn('test failures', red_log)
        out = f.reviewer_output(kind=kind)
        out.pop('validation', None)
        out['report'] = ('Audited the pinned implementation source, the authenticated owned '
                         'manifest and the actual retained raw outputs, including the linked '
                         'RED proof and its real failure output. I did not rerun any owned '
                         'verification gate, and I executed no gate myself.')
        out['audit-ack'] = {'mode': m.OWNED_AUDIT_MODE, 'verification-origin': 'audited-owned',
                            'source-read': True, 'raw-output-read': True,
                            'binding': m._audit_binding(fixture['request']),
                            'red-ack': {'proof-sha256': red['reference']['sha256'],
                                        'raw-output-read': True}}
        return out

    def gate(self, fixture, trio, authority, root):
        payload = root / 'authority.json'
        payload.write_text(json.dumps(authority))
        # The offline validator opens only the portable bundle: copy the
        # addressed manifest/raw-log files from the staging export.
        for ref in (authority['reference'], authority['log-reference'],
                    authority['red']['reference'], authority['red']['log-reference']):
            target = root / authority['bundle'] / ref['path']
            if target.exists():
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(fixture['staging'] / authority['bundle'] / ref['path'], target)
        return subprocess.run(
            ['racket', str(ROOT / 'scripts/gsd-wave-gate.rkt'), str(root / trio['evidence']),
             '--content-digest', trio['digest'], '--root', str(root),
             '--policy', str(root / 'scripts/required-pr-checks.policy'),
             '--owned-proof-authority', str(payload)],
            capture_output=True, text=True, timeout=120)

    def strict_trio(self, fixture, root):
        """Controller-shaped finalized trio (pre-publication) for the offline
        strict gate: every gate fact from the trusted authority, never from
        model prose."""
        request = fixture['request']
        authority = request['owned-authority']
        red = authority['red']
        common = {'review-mode': m.OWNED_AUDIT_MODE, 'owned-proof': authority['reference'],
                  'audit-binding': m._audit_binding(request),
                  'owned-red': red['reference']}
        paths = {'review': 'docs/reports/gsd-wave-reviews/strict.rktd',
                 'validation': 'docs/reports/gsd-wave-validation/strict.rktd',
                 'evidence': 'docs/reports/gsd-wave-evidence/strict.rktd'}
        digest_value = request['content-digest']
        evidence = dict(common, **{'schema-version': 2, 'plan-id': fixture['plan'], 'wave': 'W1',
                                   'milestone': 895, 'issue': 9686, 'status': 'ready-for-merge',
                                   'branch': f.WAVE_BRANCH,
                                   'implementation-sha': fixture['head2'],
                                   'content-digest': digest_value,
                                   'required-checks': f.POLICY_NAMES,
                                   'review-artifact': paths['review'],
                                   'validation-artifact': paths['validation']})
        output = self.audit_output(fixture)
        review = dict(common, **{'reviewer': output['reviewer'], 'verdict': 'APPROVED',
                                 'timestamp': '2026-09-14T16:00:00Z',
                                 'reviewed-sha': fixture['head2'],
                                 'content-digest': digest_value, 'scope': output['scope'],
                                 'report': output['report'], 'audit-ack': output['audit-ack']})
        gates = m._owned_approval_gates(None, authority)
        validation = dict(common, **{'status': 'current', 'planning-sync': 'current',
                                     'milestone': 895, 'wave': 'W1', 'issue': 9686,
                                     'branch': f.WAVE_BRANCH,
                                     'implementation-sha': fixture['head2'],
                                     'content-digest': digest_value,
                                     'red-first': gates['red-first'],
                                     'focused-tests': gates['focused-tests'],
                                     'format-compile': gates['format-compile'],
                                     'lint': gates['lint'], 'fast': gates['fast'],
                                     'review-artifact': paths['review'], 'remaining-items': []})
        for key, data in (('evidence', evidence), ('review', review), ('validation', validation)):
            m.write_rktd(data, root / paths[key])
        f.write_file(root / 'scripts/required-pr-checks.policy',
                     '("lint" "test (0)" "workflows (0)")')
        return dict(paths, digest=digest_value)

    # ------------------------------------------------------ no-RED world

class NoRedWorldTests(_OwnedWorldTests):
    SPECS = (('nored', 'b' * 64, False),)

    def test_missing_red_fails_closed_and_is_never_consumed(self):
        fixture = self.fixtures['nored']
        request = fixture['request']
        self.assertFalse(request['owned-authority']['red'])
        self.assertIn('No behavioral RED proof is linked', request['prompt'])
        out = f.reviewer_output()
        out.pop('validation')
        out['audit-ack'] = {'mode': m.OWNED_AUDIT_MODE, 'verification-origin': 'audited-owned',
                            'source-read': True, 'raw-output-read': True,
                            'binding': m._audit_binding(request)}
        result = self.finalize(fixture, out)
        self.assertEqual(result['status'], 'review-infra')
        self.assertIn('missing-meaningful-red-proof', result['reason'])
        self.assertEqual(f.sh('git', 'ls-remote', fixture['world']['origin'],
                              f'refs/heads/{f.WAVE_BRANCH}').split()[0], fixture['head2'])
        retry = self.prepare(fixture)
        self.assertEqual(retry['status'], 'awaiting-review', retry)
        out['audit-ack']['red-ack'] = {'proof-sha256': 'f' * 64, 'raw-output-read': True}
        self.assertEqual(self.finalize(fixture, out)['status'], 'review-infra')


# ------------------------------------------------------ linked-RED negative matrix

class RedInfraStopTests(_OwnedWorldTests):
    SPECS = (('infra', 'a' * 64, True),)

    def test_stale_or_missing_red_ack_and_model_gate_prose_are_infra(self):
        fixture = self.fixtures['infra']
        out = self.audit_output(fixture)
        out['audit-ack'].pop('red-ack')
        self.assertEqual(self.finalize(fixture, out)['status'], 'review-infra')
        out = self.audit_output(fixture)
        out['audit-ack']['red-ack']['proof-sha256'] = '0' * 64
        self.assertEqual(self.finalize(fixture, out)['status'], 'review-infra')
        out = self.audit_output(fixture)
        out['validation'] = f.reviewer_output()['validation']
        self.assertEqual(self.finalize(fixture, out)['status'], 'review-infra')
        self.assertEqual(f.sh('git', 'ls-remote', fixture['world']['origin'],
                              f'refs/heads/{f.WAVE_BRANCH}').split()[0], fixture['head2'])

    def test_stale_ack_binding_generation_is_also_infra(self):
        fixture = self.fixtures['infra']
        out = self.audit_output(fixture)
        out['audit-ack']['binding']['generation'] = 1
        self.assertEqual(self.finalize(fixture, out)['status'], 'review-infra')

    def test_request_proof_and_tree_drift_are_typed_stops(self):
        fixture = self.fixtures['infra']
        path = fixture['staging'] / 'request.json'
        original = path.read_bytes()
        for key in ('proof-sha256', 'tree', 'attempt-id', 'attempt-fence'):
            bad = copy.deepcopy(fixture['request'])
            bad[key] = 999 if key == 'attempt-fence' else 'wrong'
            path.write_text(json.dumps(bad))
            try:
                result = self.finalize(fixture, self.audit_output(fixture))
                self.assertEqual(result['status'], 'review-infra')
            except m.Pending as error:
                self.assertIn('receipt', str(error))
            finally:
                path.write_bytes(original)

    def test_publication_guard_rechecks_raw_proof_after_fetch_before_push(self):
        fixture = self.fixtures['infra']
        authority = fixture['request']['owned-authority']
        raw = (fixture['campaign'] / 'proof-store' / fixture['plan'] /
               authority['log-reference']['path'])
        original = raw.read_bytes()

        def drift_guard():
            raw.chmod(0o600)
            raw.write_bytes(original + b'drift-before-push')
            m._reauthenticate_owned_request(fixture['request'], fixture['campaign'],
                                            fixture['plan'], 1, fixture['staging'], False)
        try:
            with self.assertRaises(m._ReviewInfra):
                m._publish_candidate(fixture['world']['subject'], f.WAVE_BRANCH,
                                     fixture['head2'], fixture['head2'], drift_guard)
        finally:
            raw.write_bytes(original)
            raw.chmod(0o400)
        self.assertEqual(f.sh('git', 'ls-remote', fixture['world']['origin'],
                              f'refs/heads/{f.WAVE_BRANCH}').split()[0], fixture['head2'])


class RedStrictGateTests(_OwnedWorldTests):
    SPECS = (('strict', 'd' * 64, True),)

    def test_tampered_red_bundle_bytes_fail_offline_strict_validation(self):
        fixture = self.fixtures['strict']
        authority = fixture['request']['owned-authority']
        with tempfile.TemporaryDirectory(prefix='q-strict-') as temp:
            root = Path(temp)
            trio = self.strict_trio(fixture, root)
            # Untampered controller-shaped trio passes the strict gate.
            gate = self.gate(fixture, trio, authority, root)
            self.assertEqual(gate.returncode, 0, gate.stdout + gate.stderr)
            # Model-authored red facts (prose instead of retained bytes) fail.
            tampered = json.loads(json.dumps(authority))
            tampered['red']['failure-summary'] = \
                'reproduction observed: the malformed digest passed the gate before the fix'
            gate = self.gate(fixture, trio, tampered, root)
            self.assertNotEqual(gate.returncode, 0)
            self.assertIn('authority-red-facts-do-not-match-retained-red', gate.stdout)
            # Raw retained red bytes are revalidated byte-for-byte offline.
            red_path = root / authority['bundle'] / authority['red']['reference']['path']
            red_path.chmod(0o600)
            red_path.write_bytes(red_path.read_bytes() + b'tamper')
            gate = self.gate(fixture, trio, authority, root)
            self.assertNotEqual(gate.returncode, 0)

    def test_z_genuine_rejection_publishes_with_red_and_recovers_exactly(self):
        fixture = self.fixtures['strict']
        out = self.audit_output(fixture, kind='request-changes')
        out['findings'] = ['The fixture widget fix is behaviorally correct but the operator '
                           'merge authorization (F11) remains outstanding.']
        result = self.finalize(fixture, out)
        self.assertEqual(result['status'], 'review-rejected', result)
        self.assertEqual(result['findings'], out['findings'])
        authority = fixture['request']['owned-authority']
        for ref in (authority['reference'], authority['log-reference'],
                    authority['red']['reference'], authority['red']['log-reference']):
            relative = authority['bundle'] + '/' + ref['path']
            actual = subprocess.run(
                ['git', '-C', str(fixture['world']['subject']), 'show',
                 result['tip'] + ':' + relative], capture_output=True, timeout=60)
            self.assertEqual(actual.returncode, 0)
            self.assertEqual(actual.stdout, (fixture['staging'] / relative).read_bytes())
        self.assertEqual(self.prepare(fixture)['status'], 'review-rejected')


# ------------------------------------------------------ positive path

class LinkedRedApprovalTests(_OwnedWorldTests):
    SPECS = (('positive', 'c' * 64, True),)

    def test_approved_audit_publishes_through_strict_gate_with_red(self):
        fixture = self.fixtures['positive']
        request = fixture['request']
        authority = request['owned-authority']
        self.assertTrue(authority['red'])
        self.assertEqual(authority['red']['relation'], 'preimplementation-ancestor')
        self.assertEqual(authority['red']['red-head'], fixture['head1'])
        self.assertNotEqual(authority['red']['red-tree'],
                            json.loads((fixture['staging'] / authority['bundle'] /
                                        authority['reference']['path']).read_text())
                            ['binding']['tree'])
        self.assertIn('test failures', authority['red']['failure-summary'])
        result = self.finalize(fixture, self.audit_output(fixture))
        self.assertEqual(result['status'], 'reviewed', result)
        tip = result['tip']
        # The strict offline gate passes on the PUBLISHED committed trio.
        with tempfile.TemporaryDirectory(prefix='q-published-') as temp:
            root = Path(temp)
            for rel in (request['evidence'], request['review'], request['validation'],
                        'scripts/required-pr-checks.policy'):
                target = root / rel
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(subprocess.run(
                    ['git', '-C', str(fixture['world']['subject']), 'show', f'{tip}:{rel}'],
                    capture_output=True, check=True).stdout)
            for ref in (authority['reference'], authority['log-reference'],
                        authority['red']['reference'], authority['red']['log-reference']):
                rel = authority['bundle'] + '/' + ref['path']
                target = root / rel
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(subprocess.run(
                    ['git', '-C', str(fixture['world']['subject']), 'show', f'{tip}:{rel}'],
                    capture_output=True, check=True).stdout)
            payload = root / 'authority.json'
            payload.write_text(json.dumps(authority))
            gate = subprocess.run(
                ['racket', str(ROOT / 'scripts/gsd-wave-gate.rkt'),
                 str(root / request['evidence']),
                 '--content-digest', request['content-digest'], '--root', str(root),
                 '--policy', str(root / 'scripts/required-pr-checks.policy'),
                 '--owned-proof-authority', str(payload)],
                capture_output=True, text=True, timeout=120)
            self.assertEqual(gate.returncode, 0, gate.stdout + gate.stderr)
        # F11 stays a separate operator gate: approval never manufactures it.
        review_blob = subprocess.run(
            ['git', '-C', str(fixture['world']['subject']), 'show',
             f'{tip}:{request["review"]}'], capture_output=True, check=True).stdout
        self.assertNotIn(b'merge-authorization', review_blob)
        self.assertNotIn('merge-authorization', result)
        # Zero duplicate Verify launches: exactly one green manifest in the
        # owner store, and the audit cycle never added a second execution.
        store = fixture['campaign'] / 'proof-store' / fixture['plan']
        kinds = [json.loads(p.read_text()).get('kind') for p in store.glob('*.json')]
        self.assertEqual(kinds.count('declared-verify-green'), 1)
        self.assertEqual(kinds.count('declared-red-failure'), 1)
        fresh = m._owned_authority(fixture['campaign'], fixture['plan'], 1, fixture['staging'])
        self.assertEqual(fresh['reference'], authority['reference'])
        self.assertEqual(fresh['red']['reference'], authority['red']['reference'])


if __name__ == '__main__':
    unittest.main()
