"""Real declared owned Verify setup for the production coordinator fixture."""
import importlib.util
import json
from pathlib import Path
import subprocess
import shutil
import sys

TESTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(TESTS))
spec = importlib.util.spec_from_file_location('owned_fixture', TESTS / 'test-gsd-owned-audit.py')
a = importlib.util.module_from_spec(spec)
spec.loader.exec_module(a)
f = a.f
base, plan, wave, generation = Path(sys.argv[1]), sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
f.BRANCH = f.WAVE_BRANCH = 'campaign/production-review'
w = f.build_world(base, plan=plan, wave=wave)
repo = w['subject']
f.sh('git', 'checkout', '-q', '-b', f.WAVE_BRANCH, w['head'], cwd=repo)
for rel, text in [('src/gsd-fixture-widget.rkt', a.WIDGET_FIXED),
                  ('tests/w1-fixture.rkt', a.TEST), ('scripts/run-tests.rkt', a.RUNNER),
                  ('scripts/lint-all.rkt', a.LINT), ('.gitignore', 'compiled/\n')]:
    f.write_file(repo / rel, text)
subprocess.run(['raco', 'fmt', '-i', 'src/gsd-fixture-widget.rkt', 'tests/w1-fixture.rkt',
                'scripts/run-tests.rkt', 'scripts/lint-all.rkt'], cwd=repo, check=True,
               stdout=subprocess.PIPE)
f.sh('git', 'add', '.', cwd=repo)
f.sh('git', 'commit', '-qm', 'fixture: real acceptance gates', cwd=repo)
f.sh('git', 'push', '-q', 'origin', f.WAVE_BRANCH, cwd=repo)
root = f.build_campaign(base, plan, wave,
                       heading=f'# Fixture plan\n\n## Waves\n\n- [Inbox] W{wave}: fixture → waves/W{wave}-fixture.md',
                       wave_override=(f'# W{wave}: fixture\n\nBranch: `{f.WAVE_BRANCH}`; GitHub issue '
                                      '[#9686](https://github.com/owner/repo/issues/9686), milestone #895.\n\n'
                                      f'## Verify\n{a.CHAIN}\n'))
# Recreate via the real snapshot API, including UTF-8 byte counts and hashes.
shutil.rmtree(root / '.planning/campaigns' / plan / 'plan-snapshot')
(root / 'q').symlink_to(repo, target_is_directory=True)
run = subprocess.run(['racket', str(TESTS / 'helpers/auto-review-owned-fixture.rkt'),
                      'verify', str(root), plan, str(wave), str(repo)],
                     capture_output=True, text=True, timeout=180)
if run.returncode:
    raise RuntimeError(run.stdout + run.stderr)
path = root / '.planning/campaigns' / plan / f'coordinator-w{wave}.json'
journal = json.loads(path.read_text())
# Preserve the pre-existing fixture's simulated prior-generation setup; the
# current receipt's execution proof is genuinely produced above, never mocked.
prior = f.journal_fixture(plan, wave, journal['receipt']['head'], generation, branch=f.WAVE_BRANCH)
if 'receipt-history' in prior:
    journal['receipt-history'] = prior['receipt-history']
path.write_text(json.dumps(journal), encoding='utf-8')
print(json.dumps({'root': str(root), 'repo': str(repo), 'receipt': journal['receipt']}))
