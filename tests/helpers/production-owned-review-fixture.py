"""Real declared owned Verify setup for the production coordinator fixture."""
import importlib.util
import json
import re
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
# Discovery authenticates the installed runner's complete dependency closure,
# not a fixture-local CLI that merely happens to run green checks.
installed = TESTS.parent
seen = set()
def install_runner(relative):
    source = (installed / relative).resolve()
    if source in seen:
        return
    seen.add(source)
    source.relative_to(installed)
    text = source.read_text()
    f.write_file(repo / source.relative_to(installed), text)
    for dependency in re.findall(r'"([^"\n]+\.rkt)"', text):
        candidate = source.parent / dependency
        if candidate.is_file():
            install_runner(candidate.resolve().relative_to(installed))

install_runner(Path('scripts/run-tests.rkt'))
regression = '''#lang racket/base
;; @speed fast
;; @suite fixture
(require rackunit "../src/gsd-fixture-widget.rkt")
(provide evidence-suite)
(define evidence-suite
  (test-suite "widget"
    (test-case "squares input" (check-equal? (widget-score 3) 9))
    (test-case "squares zero" (check-equal? (widget-score 0) 0))))
(module+ test
  (require rackunit/text-ui)
  (exit (run-tests evidence-suite)))
'''
f.write_file(repo / 'src/gsd-fixture-widget.rkt', a.WIDGET_BUGGY)
f.sh('git', 'add', '.', cwd=repo)
f.sh('git', 'commit', '-qm', 'fixture: baseline widget behavior', cwd=repo)
f.sh('git', 'branch', '-f', 'main', 'HEAD', cwd=repo)
f.sh('git', 'push', '-q', 'origin', 'main', cwd=repo)
for rel, text in [('src/gsd-fixture-widget.rkt', a.WIDGET_FIXED),
                  ('tests/w1-fixture.rkt', regression),
                  ('scripts/lint-all.rkt', a.LINT), ('.gitignore', 'compiled/\n')]:
    f.write_file(repo / rel, text)
subprocess.run(['raco', 'fmt', '-i', 'src/gsd-fixture-widget.rkt', 'tests/w1-fixture.rkt',
                'scripts/lint-all.rkt'], cwd=repo, check=True,
               stdout=subprocess.PIPE)
f.sh('git', 'add', '.', cwd=repo)
f.sh('git', 'commit', '-qm', 'fixture: real acceptance gates', cwd=repo)
f.sh('git', 'push', '-q', 'origin', f.WAVE_BRANCH, cwd=repo)
root = f.build_campaign(base, plan, wave,
                       heading=f'# Fixture plan\n\n## Waves\n\n- [Inbox] W{wave}: fixture → waves/W{wave}-fixture.md',
                       wave_override=(f'# W{wave}: fixture\n\nBranch: `{f.WAVE_BRANCH}`; GitHub issue '
                                      '[#9686](https://github.com/owner/repo/issues/9686), milestone #895.\n\n'
                                      f'## Verify\n{a.CHAIN.replace("raco fmt -i tests/w1-fixture.rkt src/gsd-fixture-widget.rkt", "raco fmt -i src/gsd-fixture-widget.rkt").replace("raco make tests/w1-fixture.rkt", "raco make src/gsd-fixture-widget.rkt")}\n'))
# Recreate via the real snapshot API, including UTF-8 byte counts and hashes.
shutil.rmtree(root / '.planning/campaigns' / plan / 'plan-snapshot')
(root / 'q').symlink_to(repo, target_is_directory=True)
run = subprocess.run(['racket', str(TESTS / 'helpers/auto-review-owned-fixture.rkt'),
                      'verify', str(root), plan, str(wave), str(repo)],
                     capture_output=True, text=True, timeout=180)
if run.returncode:
    raise RuntimeError(run.stdout + run.stderr)
path = root / '.planning/campaigns' / plan / f'coordinator-w{wave}.json'
try:
    journal = json.loads(path.read_text())
    if not isinstance(journal.get('receipt'), dict):
        raise ValueError('owned Verify returned without a receipt')
except (OSError, ValueError) as exc:
    # Approval alone is not receipt evidence. Fail at the producer boundary,
    # preserving its captured output instead of a context-free FileNotFoundError
    # (and never inventing a journal or retrying until the fixture looks green).
    git = subprocess.run(['git', 'status', '--short', '--branch'], cwd=repo,
                         capture_output=True, text=True, timeout=30)
    raise RuntimeError(
        f'owned Verify did not persist a valid receipt at {path}: {exc}\n'
        f'producer exit={run.returncode}\nstdout:\n{run.stdout}\n'
        f'stderr:\n{run.stderr}\ngit status:\n{git.stdout}{git.stderr}'
    ) from exc
# Preserve the pre-existing fixture's simulated prior-generation setup; the
# current receipt's execution proof is genuinely produced above, never mocked.
prior = f.journal_fixture(plan, wave, journal['receipt']['head'], generation, branch=f.WAVE_BRANCH)
if 'receipt-history' in prior:
    journal['receipt-history'] = prior['receipt-history']
path.write_text(json.dumps(journal), encoding='utf-8')
# Keep the underlying readiness diagnosis visible, rather than collapsing it
# into the coordinator's generic evidence-repair-required outcome.
readiness_module = TESTS.parent / 'extensions/gsd/evidence-readiness.rkt'
readiness_run = subprocess.run(
    ['racket', '-e',
     '(begin (require json (file ' + json.dumps(str(readiness_module)) + ')) '
     '(write-json (evidence-readiness ' + json.dumps(str(root)) + ' '
     + json.dumps(plan) + ' ' + str(wave) + ')))'],
    capture_output=True, text=True, check=True, timeout=30)
readiness = json.loads(readiness_run.stdout)
print(json.dumps({'root': str(root), 'repo': str(repo), 'receipt': journal['receipt'],
                  'readiness': readiness}))
