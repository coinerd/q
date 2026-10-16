#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @isolation process-tree
;; @mutates repo
;; @timeout 960
;; @covers scripts/gsd-delivery.py scripts/gsd-binding-data.rkt scripts/gsd-gates/owned-audit.rkt
(require rackunit
         racket/runtime-path
         "../sandbox/subprocess.rkt")
(define-runtime-path python-tests "test-gsd-owned-audit.py")
(module+ test
  (test-case "blocked readiness cannot downgrade authentic legacy authority"
    (define result
      (run-subprocess
       "python3"
       #:args
       (list
        "-c"
        (string-append
         "import importlib.util, pathlib, tempfile, subprocess, json\n"
         "p = pathlib.Path("
         (format "~s" (path->string python-tests))
         ")\n"
         "s = importlib.util.spec_from_file_location('owned_fixture', p)\n"
         "m = importlib.util.module_from_spec(s); s.loader.exec_module(m)\n"
         "with tempfile.TemporaryDirectory() as d:\n"
         " w = m.build_owned_world(pathlib.Path(d), 'e' * 64)\n"
         " def authority(dest):\n"
         "  return subprocess.run(['racket', str(m.HELPER), 'authority', "
         "str(w['campaign']), w['plan'], str(dest)], capture_output=True, text=True)\n"
         " good = authority(pathlib.Path(d) / 'valid')\n"
         " assert good.returncode == 0, good.stderr\n"
         " a = json.loads(good.stdout)\n"
         " assert a['evidence-revision'] == 0 and a['red']['red-head'] == w['head1'], a\n"
         " ledger = w['campaign'] / 'evidence-repairs' / w['plan'] / 'W1' / 'ledger.json'\n"
         " ledger.parent.mkdir(parents=True, exist_ok=True); ledger.write_text('{broken')\n"
         " dest = pathlib.Path(d) / 'blocked'\n"
         " bad = authority(dest)\n"
         " assert bad.returncode != 0, 'blocked readiness returned legacy authority: ' + bad.stdout\n"
         " assert 'invalid-current-evidence' in bad.stderr and 'blocked' in bad.stderr, bad.stderr\n"
         " assert not dest.exists(), 'refusal exported proof side effects'\n"))
       #:timeout 900
       #:process-group? #t))
    (check-false (subprocess-result-timed-out? result))
    (check-equal? (subprocess-result-exit-code result) 0 (subprocess-result-stderr result)))
  (test-case "linked RED: production binding audit publishes provenance and validates"
    (define result
      (run-subprocess "python3"
                      #:args (list (path->string python-tests) "OwnedBindingTests")
                      #:timeout 900
                      #:process-group? #t))
    (check-false (subprocess-result-timed-out? result))
    (check-equal? (subprocess-result-exit-code result) 0 (subprocess-result-stderr result))))
