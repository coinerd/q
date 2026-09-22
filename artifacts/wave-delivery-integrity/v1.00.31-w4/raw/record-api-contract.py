#!/usr/bin/env python3
"""Coordinator-owned live-contract recorder (v1.00.31 W4, register F6).

Exercises the delivery tool's PR resolution and governance against the real
GitHub API and records the observed routes/responses with SHA-256 checksums.
Run manually by the coordinator; never part of the unit-test suite.
"""
import hashlib
import json
import os
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]          # the q checkout that carries this artifact tree
TOOL = REPO / 'scripts/gsd-delivery.py'
PAT = Path(os.path.expanduser('~/GH_PAT')).read_text().strip()
SLUG = 'coinerd/q'
W3_BRANCH = 'campaign/v1.00.31-w3'
W3_BINDING = 'binding/fb67e0429ed1-w3'
PLAN = 'fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75'


def api(route):
    out = subprocess.run(['curl', '-sS', '-H', f'Authorization: Bearer {PAT}',
                          f'https://api.github.com/repos/{SLUG}/{route}'],
                         capture_output=True, text=True).stdout
    return json.loads(out)


def digest(obj):
    return hashlib.sha256(json.dumps(obj, sort_keys=True).encode()).hexdigest()


def tool(action, args):
    cmd = ['python3', str(TOOL), action, '--repo', str(REPO)] + args
    run = subprocess.run(cmd, capture_output=True, text=True, timeout=600)
    try:
        payload = json.loads(run.stdout)
    except json.JSONDecodeError:
        payload = {'stdout': run.stdout, 'stderr': run.stderr}
    return {'argv': cmd[2:], 'exit-code': run.returncode, 'result': payload}


def pr_summary(prs):
    return [{'number': p['number'], 'state': p['state'],
             'merged_at': p.get('merged_at'),
             'head-ref': p['head']['ref']} for p in prs]


def main():
    probes = {}
    # Route-shape probes: the real owner:branch form for open and merged queries.
    for name, route in [
        ('resolve-open-w3-impl', f'pulls?state=open&head=coinerd:{W3_BRANCH}'),
        ('resolve-merged-w3-impl', f'pulls?state=all&head=coinerd:{W3_BRANCH}'),
        ('resolve-open-w3-binding', f'pulls?state=open&head=coinerd:{W3_BINDING}'),
        ('resolve-merged-w3-binding', f'pulls?state=all&head=coinerd:{W3_BINDING}'),
        ('wrong-form-control-impl', f'pulls?state=all&head={SLUG}:{W3_BRANCH}'),
        ('wrong-form-control-binding', f'pulls?state=all&head={SLUG}:{W3_BINDING}'),
    ]:
        observed = api(route)
        probes[name] = {'route': route,
                        'observed-sha256': digest(observed),
                        'observed-summary': pr_summary(observed) if isinstance(observed, list) else observed}
    # A commit-scoped check-runs probe (trusted_check route shape).
    pub = subprocess.run(['git', '-C', str(REPO), 'rev-parse',
                          'refs/remotes/origin/main'], capture_output=True,
                         text=True).stdout.strip()
    route = f'commits/{pub}/check-runs?check_name=gsd-governance'
    observed = api(route)
    runs = observed.get('check_runs', [])
    probes['check-runs-governance'] = {
        'route': route, 'observed-sha256': digest(observed),
        'observed-summary': [{'name': r['name'], 'status': r['status'],
                              'conclusion': r['conclusion']} for r in runs]}
    # Tool-level ladder probes against real repositories/PRs.
    probes['tool-resolve-pr'] = tool('resolve-pr', [
        '--plan', PLAN, '--wave', '3', '--expected-branch', W3_BRANCH])
    probes['tool-binding-resolve-pr'] = tool('binding-resolve-pr', [
        '--plan', PLAN, '--wave', '3', '--expected-branch', W3_BINDING])
    probes['tool-governance'] = tool('governance', [
        '--plan', PLAN, '--wave', '3', '--expected-branch', W3_BINDING])
    doc = {'schema': 'gsd-delivery-api-contract/1',
           'plan': PLAN, 'wave': 'W4', 'milestone': 896, 'repo': SLUG,
           'recorded-at': datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
           'publication': pub, 'probes': probes}
    out = HERE.parent / 'api-contract.json'
    out.write_text(json.dumps(doc, indent=1, sort_keys=True) + '\n')
    print('wrote', out)
    return 0


if __name__ == '__main__':
    sys.exit(main())
