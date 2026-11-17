;; scripts/testing/prepare-exclusions.rktd — reviewed eager-preparation
;; exclusions (testing-framework final plan §4.3, rev 3 decision).
;;
;; Each entry is a (repo-relative-path reason) pair. Listed paths must never
;; enter preparation's `raco make` target list: they are copy-input fixtures
;; that fail compilation in place BY DESIGN (their module graph only resolves
;; after a harness copies the tree elsewhere). The guard test
;; tests/test-run-tests-preparation.rkt asserts every listed path exists and
;; is a negative-compilation fixture (compile-check, never parse-check), and
;; that the list is non-empty.
;;
;; Review note (Wave C scouting, 2026-11-17, cold clone of this branch):
;; every discoverable test file plus its static closure was probed with
;; `raco make`. The four files below were the ONLY failures. The handoff's
;; other candidates (tests/metadata-discovery/fixture/**, tests/mock-worker.rkt,
;; tests/helpers/metadata-cache-repro.rkt, tests/reproducers/*.rkt) all compile
;; cleanly and were verified outside every closure — deliberately NOT listed.
;;
;; A fixture that starts failing preparation WITHOUT being listed here is a
;; hard error (the run fails closed); it is never a silent source-mode fallback.
(("tests/fixtures/supplemental-evidence/baseline-query.rkt"
  "copy-input fixture: sibling helpers/tracker-evidence.rkt resolves only after the harness copies the tree")
 ("tests/fixtures/supplemental-evidence/candidate-query.rkt"
  "copy-input fixture: sibling helpers/tracker-evidence.rkt resolves only after the harness copies the tree")
 ("tests/fixtures/supplemental-evidence/probe.rkt"
  "copy-input fixture: sibling helpers/tracker-evidence.rkt resolves only after the harness copies the tree")
 ("tests/fixtures/supplemental-evidence/tracker-evidence.rkt"
  "copy-input fixture: compiled only inside the copied tree, never in place"))
