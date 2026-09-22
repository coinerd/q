"""Delivery-tool GitHub API contract fixtures (register F6, v1.00.31 W4).

Offline, deterministic: the route strings the tool issues must equal the
routes recorded against the live GitHub API in
artifacts/wave-delivery-integrity/v1.00.31-w4/api-contract.json, the pulls
head filter must use the real `{owner}:{branch}` form (never
`{owner}/{repo}:{branch}`, which matches nothing and silently disables PR
resolution), merged-PR recovery must work without the head branch, and
multiple candidates must fail closed.
"""
import hashlib
import importlib.util
import json
import pathlib
import unittest
from unittest.mock import patch

ROOT = pathlib.Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('delivery', ROOT / 'scripts/gsd-delivery.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

CONTRACT_PATH = (ROOT / 'artifacts/wave-delivery-integrity/v1.00.31-w4/'
                 'api-contract.json')
SLUG = 'coinerd/q'
BRANCH = 'binding/fb67e0429ed1-w3'


def recording_api(responses):
    """A fake api() that records every issued route and serves `responses`."""
    calls = []

    def fake(slug, route, paginate=False, _refresh=False):
        calls.append((slug, route, paginate, _refresh))
        return responses.get(route, [])

    return fake, calls


class BranchOwnerTests(unittest.TestCase):
    def test_owner_of_slug(self):
        self.assertEqual(m.branch_owner('coinerd/q'), 'coinerd')
        self.assertEqual(m.branch_owner('owner/repo'), 'owner')

    def test_malformed_slugs_refuse(self):
        for slug in ('coinerd', 'coinerd/q/x', '', None, '/q', 'coinerd/'):
            with self.assertRaises(m.Pending):
                m.branch_owner(slug)


class PullsHeadFilterTests(unittest.TestCase):
    """The live-contract route shape, asserted offline against the recording."""

    def test_resolvers_issue_owner_branch_filter_only(self):
        fake, calls = recording_api({})
        with patch.object(m, 'api', fake):
            m.resolve_existing_pr(SLUG, BRANCH)
            m.resolve_merged_pr(SLUG, BRANCH)
        issued = [route for (_slug, route, _p, _r) in calls]
        self.assertEqual(issued, [
            'pulls?state=open&head=coinerd:' + BRANCH,
            'pulls?state=all&head=coinerd:' + BRANCH,
        ])
        for route in issued:
            # The pre-fix (F6) form matched nothing on the live API.
            self.assertNotIn(SLUG + ':', route)

    def test_routes_equal_the_recorded_live_contract(self):
        contract = json.loads(CONTRACT_PATH.read_text())
        recorded = {probe['route'] for probe in contract['probes'].values()
                    if probe.get('route')}
        self.assertTrue(recorded, 'recorded live-contract probes are missing')
        fake, calls = recording_api({})
        with patch.object(m, 'api', fake):
            m.resolve_existing_pr(SLUG, BRANCH)
            m.resolve_merged_pr(SLUG, BRANCH)
        for (_slug, route, _p, _r) in calls:
            self.assertIn(route, recorded,
                          'tool route %r is not covered by the recorded live '
                          'contract' % (route,))

    def test_open_pr_resolution_returns_exactly_one(self):
        payload = [{'number': 9741, 'state': 'open',
                    'head': {'sha': 'a' * 40, 'ref': BRANCH}}]
        fake, _calls = recording_api({'pulls?state=open&head=coinerd:' + BRANCH: payload})
        with patch.object(m, 'api', fake):
            pr = m.resolve_existing_pr(SLUG, BRANCH)
        self.assertEqual(pr['number'], 9741)


class MergedRecoveryTests(unittest.TestCase):
    def test_merged_pr_found_even_when_head_branch_is_deleted(self):
        """Idempotent resume: a lost merge response must be recoverable from
        the all-state query alone; the head branch ref is irrelevant."""
        payload = [{'number': 9743, 'state': 'closed',
                    'merged_at': '2026-09-22T15:12:44Z',
                    'merge_commit_sha': 'b' * 40,
                    'head': {'sha': 'c' * 40, 'ref': BRANCH}}]
        fake, _calls = recording_api({'pulls?state=all&head=coinerd:' + BRANCH: payload})
        with patch.object(m, 'api', fake):
            pr = m.resolve_merged_pr(SLUG, BRANCH)
        self.assertEqual(pr['number'], 9743)
        self.assertTrue(m.pr_is_merged(pr))

    def test_unmerged_closed_pr_is_not_merged_resume(self):
        payload = [{'number': 9743, 'state': 'closed', 'merged_at': None,
                    'head': {'sha': 'c' * 40, 'ref': BRANCH}}]
        fake, _calls = recording_api({'pulls?state=all&head=coinerd:' + BRANCH: payload})
        with patch.object(m, 'api', fake):
            self.assertIsNone(m.resolve_merged_pr(SLUG, BRANCH))

    def test_multiple_merged_candidates_fail_closed(self):
        def payload(n):
            return {'number': n, 'state': 'closed',
                    'merged_at': '2026-09-22T15:12:44Z',
                    'head': {'sha': 'c' * 40, 'ref': BRANCH}}
        fake, _calls = recording_api(
            {'pulls?state=all&head=coinerd:' + BRANCH: [payload(1), payload(2)]})
        with patch.object(m, 'api', fake), self.assertRaises(m.Pending) as caught:
            m.resolve_merged_pr(SLUG, BRANCH)
        self.assertIn('multiple merged pull requests', str(caught.exception))

    def test_malformed_responses_fail_closed(self):
        for bad in (None, {}, [1, 2], '[]'):
            fake, _calls = recording_api(
                {'pulls?state=all&head=coinerd:' + BRANCH: bad})
            with patch.object(m, 'api', fake):
                if bad is None or not isinstance(bad, list):
                    with self.assertRaises(m.Pending):
                        m.resolve_merged_pr(SLUG, BRANCH)


class CheckRouteTests(unittest.TestCase):
    def test_check_runs_route_uses_commit_scoped_path(self):
        """The route recorded live for check sampling is commit-scoped and
        paginated; the tool must keep issuing exactly that shape."""
        contract = json.loads(CONTRACT_PATH.read_text())
        routes = [p['route'] for p in contract['probes'].values() if p.get('route')]
        check_routes = [r for r in routes if r.startswith('commits/')]
        self.assertTrue(check_routes)
        for route in check_routes:
            self.assertIn('/check-runs?', route)

    def test_checks_issues_the_recorded_paginated_route(self):
        """Fixture fidelity for check sampling: m.checks() must issue exactly
        the tool-shaped paginated route recorded against the live API."""
        contract = json.loads(CONTRACT_PATH.read_text())
        recorded = contract['probes']['check-runs-paginated']['route']
        sha = contract['publication']
        calls = []

        def fake(slug, route, paginate=False, _refresh=False):
            calls.append((slug, route))
            return {'check_runs': [{'name': 'gsd-governance', 'status': 'completed',
                                    'conclusion': 'success'}],
                    'total_count': 1}

        m._CHECKS_CACHE.clear()
        try:
            with patch.object(m, 'api', fake):
                runs = m.checks(SLUG, sha)
        finally:
            m._CHECKS_CACHE.clear()
        self.assertEqual(calls, [(SLUG, recorded)])
        self.assertEqual(len(runs), 1)


if __name__ == '__main__':
    unittest.main(verbosity=1)
