#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-d5-executor-cwd-contract.rkt — S2b/D5 regression tests
;;
;; Campaign 81f9be4b: attempt-3 burned tool budget on "Wrong working dir"
;; (repo-root vs q/ confusion); attempt-5 read q/tui/key-dispatch.rkt when
;; the plan says q/tui/keybindings/key-dispatch.rkt. The executor session
;; must receive an explicit working-directory contract and per-target
;; existence validation so it can distinguish a genuinely missing file
;; from a path-resolution mistake.

(require rackunit
         racket/file
         racket/string
         "../extensions/gsd/command-handlers.rkt"
         (only-in "../extensions/gsd/plan-types.rkt" gsd-plan gsd-wave))

;; ============================================================
;; Fixtures
;; ============================================================

(define (make-fixture-dir)
  (define dir (make-temporary-file "d5-cwd-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (make-directory* (build-path dir "q" "tui" "keybindings"))
  (make-directory* (build-path dir "q" "ui-core"))
  ;; A real target that exists relative to the project root.
  (call-with-output-file (build-path dir "q" "tui" "keybindings" "key-dispatch.rkt")
                         (lambda (out) (display ";; key-dispatch\n" out)))
  ;; A declared target that does NOT exist (to exercise MISSING).
  dir)

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

(define (make-test-plan exists-target missing-target)
  (gsd-plan (list (gsd-wave 0
                            "W0: test wave"
                            'pending
                            "root cause"
                            (list exists-target missing-target)
                            '()
                            "raco test"
                            '("done")))
            #f
            '()
            '()))

(define (build-prompt dir plan)
  ;; base-dir = the fixture root; a wave doc is needed for the entry lookup,
  ;; so we write PLAN.md + the wave doc first.
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display "# Plan: D5 Test\n\n## Wave 0: W0 test\n- [Inbox] W0: W0 test → waves/W0-wave.md\n"
              out)))
  (call-with-output-file
   (build-path dir ".planning" "waves" "W0-wave.md")
   (lambda (out)
     (display
      "# Wave 0\nStatus: PENDING\n\n# W0 test\n\n- File: q/tui/keybindings/key-dispatch.rkt\n- File: q/missing-file.rkt\n"
      out)))
  (build-single-wave-prompt dir plan 0))

;; ============================================================
;; Tests
;; ============================================================

(test-case "S2b: wave prompt pins the working-directory contract"
  (define dir (make-fixture-dir))
  (dynamic-wind void
                (lambda ()
                  (define plan
                    (make-test-plan "q/tui/keybindings/key-dispatch.rkt" "q/missing-file.rkt"))
                  (define prompt (build-prompt dir plan))
                  (check-true (string-contains? prompt "## Working Directory Contract"))
                  (check-true (string-contains? prompt "Project root (base-dir)"))
                  (check-true (string-contains? prompt "Process working directory"))
                  (check-true (string-contains? prompt "Source subdir is 'q'")))
                (lambda () (cleanup-tmp dir))))

(test-case "S2b: existing File target is validated [exists]"
  (define dir (make-fixture-dir))
  (dynamic-wind
   void
   (lambda ()
     (define plan (make-test-plan "q/tui/keybindings/key-dispatch.rkt" "q/missing-file.rkt"))
     (define prompt (build-prompt dir plan))
     (check-true (string-contains? prompt "q/tui/keybindings/key-dispatch.rkt [exists]"))
     (check-false (string-contains? prompt "q/tui/keybindings/key-dispatch.rkt [MISSING]")))
   (lambda () (cleanup-tmp dir))))

(test-case "S2b: missing File target is flagged [MISSING]"
  (define dir (make-fixture-dir))
  (dynamic-wind
   void
   (lambda ()
     (define plan (make-test-plan "q/tui/keybindings/key-dispatch.rkt" "q/missing-file.rkt"))
     (define prompt (build-prompt dir plan))
     (check-true (string-contains? prompt "q/missing-file.rkt [MISSING]")
                 "nonexistent target must be flagged so the executor does not burn budget"))
   (lambda () (cleanup-tmp dir))))

(test-case "S2b: full target list is enumerated under the contract"
  (define dir (make-fixture-dir))
  (dynamic-wind
   void
   (lambda ()
     (define plan (make-test-plan "q/tui/keybindings/key-dispatch.rkt" "q/missing-file.rkt"))
     (define prompt (build-prompt dir plan))
     ;; The contract section sits between the working-directory header and
     ;; the runtime-enforced execution block.
     (define after-contract (car (regexp-split #rx"## Working Directory Contract" prompt)))
     (define contract-section
       (car (regexp-split #rx"# Runtime-Enforced Single-Wave Execution"
                          (substring prompt (string-length after-contract)))))
     (check-true (string-contains? contract-section "Declared file targets"))
     (check-true (string-contains? contract-section "* q/tui/keybindings/key-dispatch.rkt"))
     (check-true (string-contains? contract-section "* q/missing-file.rkt")))
   (lambda () (cleanup-tmp dir))))
