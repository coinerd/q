"""Pure consumer-schema tests; these dictionaries confer no operator authority."""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    'delivery', Path(__file__).resolve().parents[1] / 'scripts/gsd-delivery.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)


class PromotionBinding(unittest.TestCase):
    def setUp(self):
        self.sha = 'a' * 64
        self.authority = {'reference': {'supplemental-contract': {}},
                          'supplemental-promotion-sha256': self.sha}
        self.request = {'owned-authority': self.authority,
                        'supplemental-promotion-sha256': self.sha}

    def test_explicit_pin_and_ack_key_identity(self):
        binding = m._audit_binding(self.request)
        self.assertEqual(binding['supplemental-promotion-sha256'], self.sha)
        old = dict(binding)
        old.pop('supplemental-promotion-sha256')
        self.assertFalse(m._exact_audit_binding(old, binding))

    def test_missing_malformed_or_wrong_pin_refuses(self):
        for value in (None, 'bad', 'b' * 64):
            request = dict(self.request, **{'supplemental-promotion-sha256': value})
            with self.assertRaises(m._ReviewInfra):
                m._audit_binding(request)

    def test_ordinary_old_binding_unchanged(self):
        self.assertEqual(m._audit_binding({}), {key: None for key in m.AUDIT_KEYS})

    def test_prepare_copies_pin_from_trusted_consumer_seam(self):
        receipt = {'head': 'head', 'tree': 'tree', 'branch': 'branch',
                   'attempt-id': 'attempt', 'attempt-fence': 2}
        authority = copy.deepcopy(self.authority)
        authority.update({'receipt': receipt, 'generation': 1, 'bundle': 'bundle'})
        authority['reference']['sha256'] = 'c' * 64
        request = dict(receipt, generation=1, prompt='review')
        with patch.object(m, '_owned_authority', return_value=authority):
            m._bind_owned_request(request, None, 'plan', 1, Path('/unused'))
        self.assertEqual(request['supplemental-promotion-sha256'], self.sha)
        self.assertEqual(authority['audit-binding'], m._audit_binding(request))
        fresh = copy.deepcopy(authority)
        fresh['supplemental-promotion-sha256'] = 'd' * 64
        with patch.object(m, '_owned_authority', return_value=fresh):
            with self.assertRaises(m._ReviewInfra):
                m._reauthenticate_owned_request(request, None, 'plan', 1, Path('/unused'), False)


if __name__ == '__main__':
    unittest.main()
