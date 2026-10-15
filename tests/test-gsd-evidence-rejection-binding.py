"""Bounded recovery regression: no owned gate executions or external processes."""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('rejection_delivery', Path(__file__).resolve().parents[1] / 'scripts/gsd-delivery.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)


class Recovery(unittest.TestCase):
    def setUp(self):
        self.receipt = dict(head='a' * 40, tree='b' * 40, branch='work',
                            **{'attempt-id': 'attempt', 'attempt-fence': 1,
                               'owned-proof': {'sha256': 'green'}})
        self.authority = {'receipt': copy.deepcopy(self.receipt), 'generation': 2,
                          'reference': self.receipt['owned-proof'],
                          'evidence-revision': 1, 'evidence-registration-sha256': 'registered',
                          'red': {'reference': {'sha256': 'red'}}}
        request = dict(self.receipt, **{'plan-id': 'plan', 'wave': 1, 'generation': 2,
                       'content-digest': 'digest', 'proof-sha256': 'green',
                       'evidence-revision': 1, 'evidence-registration-sha256': 'registered'})
        binding = m._audit_binding(request)
        self.datum = dict(request, stage='implementation-review', wave='W1',
                          verdict='REQUEST_CHANGES', **{'reviewed-sha': self.receipt['head'],
                          'timestamp': '2026-01-01T00:00:00Z', 'review-mode': m.OWNED_AUDIT_MODE,
                          'owned-proof': self.receipt['owned-proof'], 'owned-red': {'sha256': 'red'},
                          'audit-binding': binding, 'audit-ack': {'mode': m.OWNED_AUDIT_MODE,
                          'verification-origin': 'audited-owned', 'source-read': True,
                          'raw-output-read': True, 'binding': binding,
                          'red-ack': {'proof-sha256': 'red', 'raw-output-read': True}},
                          'reviewer': 'Independent reviewer', 'scope': 'Inspected implementation source. ' * 20,
                          'report': 'Observed a concrete correctness defect. ' * 20,
                          'findings': ['Fix incorrect behavior.']})

    def recover(self, authority=True):
        with patch.object(m, 'blob_or_none', side_effect=[b'published', b'old']), \
             patch.object(m, 'read_datum', return_value=self.datum), \
             patch.object(m, 'digest', return_value='digest'):
            args = ('repo', 'c' * 40, 'review', 'plan', 1, 2, self.receipt)
            return m._recover_rejected_artifact(*args, **({'authority': self.authority} if authority else {}))

    def test_registered_rejection_recovers(self):
        self.assertEqual(self.recover()['status'], 'review-rejected')

    def test_missing_authority_refused(self):
        with self.assertRaises(m.Pending):
            self.recover(False)

    def test_drift_refused(self):
        for key, value in [('evidence-revision', 2), ('evidence-registration-sha256', 'other'),
                           ('reference', {'sha256': 'other'}), ('generation', 3),
                           ('red', {'reference': {'sha256': 'other'}})]:
            with self.subTest(key=key):
                original = copy.deepcopy(self.authority)
                self.authority[key] = value
                with self.assertRaises(m.Pending):
                    self.recover()
                self.authority = original

    def test_receipt_drift_refused(self):
        for key in self.receipt:
            with self.subTest(key=key):
                original = copy.deepcopy(self.authority['receipt'])
                self.authority['receipt'][key] = 'drift'
                with self.assertRaises(m.Pending):
                    self.recover()
                self.authority['receipt'] = original

    def test_ack_guards_refused(self):
        for key in ('source-read', 'raw-output-read'):
            with self.subTest(key=key):
                self.datum['audit-ack'][key] = False
                with self.assertRaises(m.Pending):
                    self.recover()
                self.datum['audit-ack'][key] = True
        self.datum['audit-ack']['binding']['extra'] = 'untrusted'
        with self.assertRaises(m.Pending):
            self.recover()

    def test_revision_zero(self):
        self.authority['evidence-revision'] = 0
        self.authority.pop('evidence-registration-sha256')
        for binding in [self.datum['audit-binding'], self.datum['audit-ack']['binding']]:
            binding.pop('evidence-revision', None)
            binding.pop('evidence-registration-sha256', None)
        self.assertEqual(self.recover()['status'], 'review-rejected')

    def test_manual_compatibility(self):
        self.datum.pop('review-mode')
        self.assertEqual(self.recover(False)['status'], 'review-rejected')


if __name__ == '__main__':
    unittest.main()
