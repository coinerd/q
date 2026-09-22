"""Amended approval-contract regressions (register F11, v1.00.31 W4).

The merge gate requires a recorded operator authorization carried in the
committed evidence record's `merge-authorization` object plus an APPROVED
independent non-author review artifact bound to the verified implementation
head. Both refusal directions are enforced here offline; the full merge-ladder
wiring is covered by tests/test-gsd-delivery-controller.py.
"""
import hashlib
import importlib.util
import pathlib
import unittest
from unittest.mock import patch

ROOT = pathlib.Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('delivery', ROOT / 'scripts/gsd-delivery.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

RECEIPT = 'b' * 40
OTHER = 'c' * 40
DIGEST = hashlib.sha256(b'').hexdigest()
AUTH_KEY = 'merge-authorization'


def evidence(**overrides):
    datum = {
        'schema-version': 2,
        'implementation-sha': RECEIPT,
        'content-digest': DIGEST,
        AUTH_KEY: {
            'operator': 'coinerd',
            'wave': 'W4',
            'head': RECEIPT,
            'action': 'squash-merge the reviewed wave implementation PR',
            'source': 'operator directive in the campaign delivery session per '
                      'the amended approval contract',
        },
    }
    datum.update(overrides)
    return datum


def review(**overrides):
    datum = {
        'verdict': 'APPROVED',
        'reviewer': 'independent read-only reviewer subagent',
        'reviewed-sha': RECEIPT,
    }
    datum.update(overrides)
    return datum


class MergeAuthorizationTests(unittest.TestCase):
    def setUp(self):
        # Content equality between the authorized receipt and the merge tip is
        # proven with the excluded-paths digest; the real git-backed digest is
        # exercised end-to-end in the controller suite. Here both sides agree.
        self.digest_patch = patch.object(
            m, 'digest', lambda repo, base, ref: DIGEST, create=True)
        self.digest_patch.start()
        self.addCleanup(self.digest_patch.stop)

    def test_accepts_complete_authorization(self):
        auth = m.merge_authorization(evidence(), 'W4', RECEIPT, 'd' * 40, None)
        self.assertEqual(auth['operator'], 'coinerd')

    def test_missing_object_refused(self):
        datum = evidence(**{AUTH_KEY: None})
        with self.assertRaises(m.Pending) as caught:
            m.merge_authorization(datum, 'W4', RECEIPT, 'd' * 40, None)
        self.assertIn('no-operator-authorization', str(caught.exception))

    def test_each_required_field_refused(self):
        base = evidence()[AUTH_KEY]
        for field in ('operator', 'wave', 'head', 'action', 'source'):
            auth = dict(base)
            auth.pop(field)
            with self.assertRaises(m.Pending) as caught:
                m.merge_authorization(evidence(**{AUTH_KEY: auth}), 'W4',
                                      RECEIPT, 'd' * 40, None)
            self.assertIn('no-operator-authorization', str(caught.exception),
                          msg=field)

    def test_blank_source_refused(self):
        auth = dict(evidence()[AUTH_KEY], source='   ')
        with self.assertRaises(m.Pending) as caught:
            m.merge_authorization(evidence(**{AUTH_KEY: auth}), 'W4',
                                  RECEIPT, 'd' * 40, None)
        self.assertIn('no-operator-authorization', str(caught.exception))

    def test_foreign_wave_refused(self):
        auth = dict(evidence()[AUTH_KEY], wave='W5')
        with self.assertRaises(m.Pending) as caught:
            m.merge_authorization(evidence(**{AUTH_KEY: auth}), 'W4',
                                  RECEIPT, 'd' * 40, None)
        self.assertIn('no-operator-authorization', str(caught.exception))

    def test_head_bound_to_verified_implementation_head(self):
        auth = dict(evidence()[AUTH_KEY], head=OTHER)
        with self.assertRaises(m.Pending) as caught:
            m.merge_authorization(evidence(**{AUTH_KEY: auth}), 'W4',
                                  RECEIPT, 'd' * 40, None)
        self.assertIn('no-operator-authorization', str(caught.exception))

    def test_content_drift_between_receipt_and_tip_refused(self):
        with patch.object(m, 'digest',
                          lambda repo, base, ref: OTHER if ref == RECEIPT else DIGEST,
                          create=True):
            with self.assertRaises(m.Pending) as caught:
                m.merge_authorization(evidence(), 'W4', RECEIPT, 'd' * 40, None)
        self.assertIn('no-operator-authorization', str(caught.exception))

    def test_divergent_authorization_shape_refused(self):
        """Task 6: only the evidence-record object shape is accepted — a
        differently named carrier is no authorization at all."""
        datum = evidence()
        datum['operator-authorization'] = datum.pop(AUTH_KEY)
        with self.assertRaises(m.Pending) as caught:
            m.merge_authorization(datum, 'W4', RECEIPT, 'd' * 40, None)
        self.assertIn('no-operator-authorization', str(caught.exception))


class ReviewedHeadApprovalTests(unittest.TestCase):
    def test_accepts_approved_nonauthor_review(self):
        got = m.reviewed_head_approval(review(), evidence(), 'coinerd')
        self.assertEqual(got['verdict'], 'APPROVED')

    def test_missing_review_refused(self):
        with self.assertRaises(m.Pending) as caught:
            m.reviewed_head_approval(None, evidence(), 'coinerd')
        self.assertIn('no-review-artifact', str(caught.exception))

    def test_non_approved_verdict_refused(self):
        with self.assertRaises(m.Pending) as caught:
            m.reviewed_head_approval(review(verdict='NEEDS_WORK'), evidence(),
                                     'coinerd')
        self.assertIn('no-review-artifact', str(caught.exception))

    def test_author_as_reviewer_refused(self):
        with self.assertRaises(m.Pending) as caught:
            m.reviewed_head_approval(review(reviewer='coinerd'), evidence(),
                                     'coinerd')
        self.assertIn('no-review-artifact', str(caught.exception))

    def test_author_identity_is_normalized(self):
        # Case/whitespace variants of the author login do not pass as
        # independent reviewers (review R1 finding).
        with self.assertRaises(m.Pending) as caught:
            m.reviewed_head_approval(review(reviewer='  Coinerd '), evidence(),
                                     'coinerd')
        self.assertIn('no-review-artifact', str(caught.exception))

    def test_missing_author_identity_refused(self):
        # A missing PR author login must fail closed, never widen the gate.
        with self.assertRaises(m.Pending) as caught:
            m.reviewed_head_approval(review(), evidence(), '')
        self.assertIn('no-review-artifact', str(caught.exception))

    def test_review_bound_to_other_head_refused(self):
        with self.assertRaises(m.Pending) as caught:
            m.reviewed_head_approval(review(**{'reviewed-sha': OTHER}),
                                     evidence(), 'coinerd')
        self.assertIn('head-binding-mismatch', str(caught.exception))


if __name__ == '__main__':
    unittest.main(verbosity=1)
