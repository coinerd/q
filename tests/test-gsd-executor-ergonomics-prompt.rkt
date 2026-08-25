#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-executor-ergonomics-prompt.rkt — W4 (BUG-0027, BUG-0026)
;;
;; BUG-0027: the single-wave executor prompt must pin the git root (resolved
;; at runtime with the delivery verifier's git-root-for logic) and — only
;; when base-dir is NOT the git root — the correction line telling executors
;; to run git against the root, instead of burning calls on
;; "fatal: Kein Git-Repository" before rediscovering q/.
;; BUG-0026: the prompt must state the sanctioned scratch-file pattern
;; (edit-tool files under tmp/, then delete) so executors stop improvising
;; heredoc scratch writes that the destructive-write guard blocks.
;;
;; Snapshot assertions pin that NO OTHER template text changed: stripping the
;; added lines must yield the exact pre-W4 template bytes.

(require rackunit
         racket/file
         racket/string
         "../extensions/gsd/command-handlers.rkt"
         (only-in "../extensions/gsd/prompts.rkt" planning-implement-prompt)
         (only-in "../extensions/gsd/plan-context-builder.rkt" find-git-root-dir)
         (only-in "../extensions/gsd/plan-types.rkt" gsd-plan gsd-wave))

;; ============================================================
;; Fixtures
;; ============================================================

(define scratch-guidance-line
  "8. Scratch files (BUG-0026): for throwaway experiments use the edit tool to create files under `tmp/`, run them, then delete them.\n")

(define scratch-guidance-sentence
  "for throwaway experiments use the edit tool to create files under `tmp/`, run them, then delete them")

(define git-root-correction-line
  "- run all git commands against the git root (`cd q` or `git -C q`)\n")

(define (make-fixture-dir #:git-at-base? [git-at-base? #f] #:git-at-q? [git-at-q? #f])
  (define dir (make-temporary-file "erg-prompt-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (make-directory* (build-path dir "q" "tui" "keybindings"))
  (call-with-output-file (build-path dir "q" "tui" "keybindings" "key-dispatch.rkt")
                         (lambda (out) (display ";; key-dispatch\n" out)))
  (when git-at-base?
    (make-directory* (build-path dir ".git")))
  (when git-at-q?
    (make-directory* (build-path dir "q" ".git")))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display
      "# Plan: Ergonomics Test\n\n## Wave 0: W0 test\n- [Inbox] W0: W0 test → waves/W0-wave.md\n"
      out)))
  (call-with-output-file
   (build-path dir ".planning" "waves" "W0-wave.md")
   (lambda (out)
     (display
      "# Wave 0\nStatus: PENDING\n\n# W0 test\n\n- File: q/tui/keybindings/key-dispatch.rkt\n- File: q/missing-file.rkt\n"
      out)))
  dir)

(define (make-test-plan)
  (gsd-plan (list (gsd-wave 0
                            "W0: test wave"
                            'pending
                            "root cause"
                            (list "q/tui/keybindings/key-dispatch.rkt" "q/missing-file.rkt")
                            '()
                            "raco test"
                            '("done")))
            #f
            '()
            '()))

(define (build-prompt dir)
  (build-single-wave-prompt dir (make-test-plan) 0))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

;; ============================================================
;; Section extraction + normalization (for snapshot assertions)
;; ============================================================

(define (contract-section prompt)
  (define after-header (cadr (regexp-split #rx"## Working Directory Contract" prompt)))
  (car (regexp-split #rx"# Runtime-Enforced Single-Wave Execution" after-header)))

;; Mask environment-dependent values so the snapshot is byte-stable.
(define (normalize-section section)
  (string-replace
   (string-replace (string-replace section #rx"- Git root: [^\n]*" "- Git root: <GITROOT>")
                   #rx"- Project root \\(base-dir\\): [^\n]*"
                   "- Project root (base-dir): <BASE>")
   #rx"- Process working directory: [^\n]*"
   "- Process working directory: <CWD>"))

;; The exact pre-W4 contract section (normalized): the D5/S2b template with
;; NO git-root lines. If W4's splice changes anything but the two added lines,
;; the strip checks below fail.
(define pre-w4-contract-section
  (string-append
   "\n"
   "- Project root (base-dir): <BASE>\n"
   "- Process working directory: <CWD>\n"
   "- Source subdir is 'q' under the project root. Resolve 'File:' paths relative to the project root unless they are absolute.\n"
   "- Declared file targets (existence checked against project root):\n"
   "  * q/tui/keybindings/key-dispatch.rkt [exists]\n"
   "  * q/missing-file.rkt [MISSING]\n"
   "\n"))

;; The exact pre-W4 planning-implement-prompt (rules 1-7 + tail, no rule 8).
(define pre-w4-planning-implement-prompt
  (string-append
   "[gsd-planning] EXECUTE the plan below. IMPLEMENT NOW — do NOT explore.\n"
   "\n"
   "CRITICAL RULES:\n"
   "1. Do NOT re-read the plan. It is provided below in full.\n"
   "2. Do NOT write a new plan. Execute the existing one.\n"
   "3. Do NOT use planning-write during implementation.\n"
   "   planning-read is allowed to check STATE or VALIDATION.\n"
   "4. Read each target file BEFORE editing it. You need the current content\n"
   "   to apply edits correctly. Read is necessary and expected.\n"
   "5. After reading, apply the edits specified in the wave doc actions.\n"
   "6. After completing the assigned wave, run its verify command.\n"
   "7. Do NOT call /wave-done; the runtime coordinator owns status transitions only.\n"
   "   After you return, the coordinator verifies real delivery evidence (expected branch,\n"
   "   changed wave files, passing verify command). Only that evidence marks the wave DONE.\n"
   "\n"
   "The plan follows. Start implementing immediately.\n"))

;; ============================================================
;; BUG-0027: git-root contract lines
;; ============================================================

(test-case "BUG-0027: git root pinned + correction line present when base-dir is not the git root"
  (define dir (make-fixture-dir #:git-at-q? #t))
  (dynamic-wind void
                (lambda ()
                  (define prompt (build-prompt dir))
                  (check-true (string-contains? prompt "- Git root: "))
                  (check-true (string-contains? prompt (path->string (build-path dir "q")))
                              "the git root path (base/q) must be pinned")
                  (check-true (string-contains? prompt "run all git commands against the git root"))
                  (check-true (string-contains? prompt git-root-correction-line)))
                (lambda () (cleanup-tmp dir))))

(test-case "BUG-0027: correction line ABSENT when base-dir IS the git root (iff)"
  (define dir (make-fixture-dir #:git-at-base? #t))
  (dynamic-wind void
                (lambda ()
                  (define prompt (build-prompt dir))
                  (check-true (string-contains? prompt "- Git root: ")
                              "git root (== base) is still pinned")
                  (check-false (string-contains? prompt "run all git commands against the git root")
                               "no correction needed when base == git root"))
                (lambda () (cleanup-tmp dir))))

(test-case "BUG-0027: no git root found — none-line, no correction (env-tolerant)"
  (define dir (make-fixture-dir))
  (dynamic-wind
   void
   (lambda ()
     (define prompt (build-prompt dir))
     (define root (find-git-root-dir dir))
     (cond
       [(not root)
        (check-true (string-contains? prompt "- Git root: none found"))
        (check-false (string-contains? prompt "run all git commands against the git root"))]
       [else
        ;; Pathological environment (tmp dir inside a repo): the
        ;; iff contract must still hold.
        (check-equal? (string-contains? prompt "run all git commands against the git root")
                      (not (equal? (simplify-path (path->complete-path root))
                                   (simplify-path (path->complete-path dir)))))]))
   (lambda () (cleanup-tmp dir))))

;; ============================================================
;; BUG-0026: scratch-file guidance
;; ============================================================

(test-case "BUG-0026: single-wave prompt carries the sanctioned scratch-file guidance"
  (define dir (make-fixture-dir #:git-at-q? #t))
  (dynamic-wind void
                (lambda ()
                  (define prompt (build-prompt dir))
                  (check-true (string-contains? prompt scratch-guidance-sentence))
                  (check-true (string-prefix? prompt planning-implement-prompt)
                              "prompt must start with the (extended) template verbatim"))
                (lambda () (cleanup-tmp dir))))

;; ============================================================
;; Snapshot assertions: no other template text changed
;; ============================================================

(test-case "snapshot: contract section = pre-W4 text + exactly the two git-root lines (base ≠ root)"
  (define dir (make-fixture-dir #:git-at-q? #t))
  (dynamic-wind
   void
   (lambda ()
     (define section (normalize-section (contract-section (build-prompt dir))))
     (check-equal?
      section
      (string-append
       "\n"
       "- Project root (base-dir): <BASE>\n"
       "- Process working directory: <CWD>\n"
       "- Git root: <GITROOT>\n"
       "- run all git commands against the git root (`cd q` or `git -C q`)\n"
       "- Source subdir is 'q' under the project root. Resolve 'File:' paths relative to the project root unless they are absolute.\n"
       "- Declared file targets (existence checked against project root):\n"
       "  * q/tui/keybindings/key-dispatch.rkt [exists]\n"
       "  * q/missing-file.rkt [MISSING]\n"
       "\n"))
     ;; Stripping the two added lines must yield the exact
     ;; pre-W4 section — proves nothing else changed.
     (check-equal?
      (string-replace (string-replace section "- Git root: <GITROOT>\n" "")
                      "- run all git commands against the git root (`cd q` or `git -C q`)\n"
                      "")
      pre-w4-contract-section))
   (lambda () (cleanup-tmp dir))))

(test-case "snapshot: contract section = pre-W4 text + git-root line only (base == root)"
  (define dir (make-fixture-dir #:git-at-base? #t))
  (dynamic-wind void
                (lambda ()
                  (define section (normalize-section (contract-section (build-prompt dir))))
                  (check-equal? (string-replace section "- Git root: <GITROOT>\n" "")
                                pre-w4-contract-section)
                  (check-false (string-contains? section "run all git commands")))
                (lambda () (cleanup-tmp dir))))

(test-case "snapshot: planning-implement-prompt = pre-W4 template + exactly the scratch line"
  (check-equal? (string-replace planning-implement-prompt scratch-guidance-line "")
                pre-w4-planning-implement-prompt)
  (check-equal? (length (regexp-match* (regexp-quote scratch-guidance-line)
                                       planning-implement-prompt))
                1
                "scratch guidance must be exactly one line"))
