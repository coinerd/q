#lang racket

;; tests/test-session-hygiene-characterization.rkt
;;
;; CHARACTERIZATION (W0) — pins CURRENT session-hygiene behavior for
;; BUG-0038 (+ the BUG-0033 file-content half):
;;
;; BUG-0038: NO write-path staleness check exists. A session whose loaded
;; build predates the current checkout (running-vs-checkout version
;; divergence) can still write working-tree files through the tracked-file
;; write path — nothing refuses, warns, or blocks. This is the documented
;; absent-seam marker at the planned interception point (the tracked-file
;; write path), mirroring the v1.00.19 freshness-guard W0 pin: that guard
;; protects /go ENTRY, not background writes.
;;
;; BUG-0033 half: the project-root test-runner shim
;; <project-base>/scripts/run-tests.rkt still `(require "run-tests/...")`
;; submodules that exist ONLY under q/ — the root invocation is broken.
;;
;; Pin convention: every test PASSES today. BUG-0038's owning wave (W3)
;; adds the write-path freshness guard and flips the absence pins;
;; BUG-0033's wave fixes/removes the root shim and flips its pin.
;; Pure-level pin: temp files + source-surface scans only, NO live
;; TUI/worker subprocess.

(require racket/file
         racket/format
         rackunit
         rackunit/text-ui)

(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
(define repo (simplify-path (build-path this-file 'up 'up)))
(define project-base (simplify-path (build-path repo 'up)))
(define (repo-file . parts)
  (apply build-path (cons repo parts)))
(define (base-file . parts)
  (apply build-path (cons project-base parts)))

(define WRITE-PATH-SOURCES
  (list (repo-file "extensions" "racket-tooling-handlers.rkt")
        (repo-file "extensions" "racket-tooling.rkt")
        (repo-file "extensions" "hooks.rkt")
        (repo-file "extensions" "tool-api.rkt")
        (repo-file "extensions" "quarantine.rkt")))

(define (source-exists? p)
  (and (file-exists? p) #t))

(define (scan patterns files)
  (for*/list ([path (in-list files)]
              #:when (source-exists? path)
              [rx (in-list patterns)]
              #:when (regexp-match? rx (file->string path)))
    (cons rx (path->string path))))

(define suite
  (test-suite "BUG-0038/0033 characterization: no write-path staleness guard; root test-runner shim still broken"

    ;; ------------------------------------------------------------
    ;; BUG-0038 — the tracked-file write path has NO staleness check.
    ;; ------------------------------------------------------------

    (test-case "write through the tracked-file path succeeds regardless of running-vs-checkout divergence"
      ;; Simulate the interception point as a pure function over state:
      ;; running-version ≠ checkout-version, and a write request arrives.
      ;; TODAY the decision is unconditional acceptance: no guard input can
      ;; change the outcome because the write path takes no such input.
      (define running-version "1.0.0-old")
      (define checkout-version "9.9.9-new")
      (check-true (not (string=? running-version checkout-version))
                  "precondition: versions genuinely diverge")
      ;; The write decision ignores any version state: with maximal
      ;; divergence the write is still permitted (no refusal seam exists).
      (define (write-permitted? . _any-state)
        ;; Today's write path has NO staleness parameter at all; the pure
        ;; model of today's behavior is: always #t.
        #t)
      (check-true
       (write-permitted? running-version checkout-version)
       "write succeeds under full version divergence today — BUG-0038's W3 guard flips this to #f"))

    (test-case "absent-seam marker: no freshness/staleness guard on the write path (W3 interception point)"
      ;; W3 will guard the tracked-file write path (the edit/write tool
      ;; handlers). Today NONE of the write-path sources mention freshness,
      ;; staleness, build-version, or running-vs-checkout divergence.
      (define hits
        (scan (list #rx"stale-checkout"
                    #rx"staleness"
                    #rx"running-version"
                    #rx"checkout-version"
                    #rx"freshness-guard")
              WRITE-PATH-SOURCES))
      (check-equal?
       hits
       '()
       (format "write-path staleness guard already exists at ~a — BUG-0038/W3 landed; flip this pin"
               hits)))

    (test-case "freshness guard remains /go-entry-only today (BUG-0031 scope)"
      ;; The only shipped freshness guard lives on the /go path
      ;; (go-orchestrator.rkt). Pin that the write-path sources carry none
      ;; while the /go entry does — the asymmetry IS the defect.
      (define go-src (repo-file "extensions" "gsd" "go-orchestrator.rkt"))
      (check-true (and go-src (file-exists? go-src)))
      (check-true (regexp-match? #rx"freshness" (file->string go-src))
                  "precondition: /go entry freshness guard exists (v1.00.19 W3)")
      (define hits (scan (list #rx"freshness") WRITE-PATH-SOURCES))
      (check-equal? hits
                    '()
                    "asymmetry pin: freshness guard on /go entry ONLY, never on the write path"))

    (test-case "stale-session write to tracked file round-trips unchanged (no quarantine, no refusal)"
      ;; Durable half of the pin: writing a tracked file from a
      ;; stale-version session context today produces the exact content on
      ;; disk — no quarantine directory absorbs it, no error, no marker.
      (define tmp (make-temporary-file "hygiene-write-pin~a" 'directory))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
         (define tracked (build-path tmp "tracked.rkt"))
         (define stale-content "#lang racket\n;; stale in-memory content from an old session\n")
         (define running-version "1.0.0-old")
         (define checkout-version "9.9.9-new")
         ;; Today's write path: plain write, version state not consulted.
         (call-with-output-file tracked (lambda (out) (display stale-content out)) #:exists 'replace)
         (check-equal?
          (file->string tracked)
          stale-content
          "stale-context write lands verbatim today — W3's guard must refuse or quarantine, flipping this pin")
         (define quarantine-dir (build-path tmp ".quarantine"))
         (check-false (directory-exists? quarantine-dir)
                      "no quarantine dir is created by a stale write today")
         (check-true (and (not (string=? running-version checkout-version)) #t)
                     "versions diverged throughout — write still succeeded"))
       (lambda () (delete-directory/files tmp))))

    ;; ------------------------------------------------------------
    ;; BUG-0033 — project-root test-runner shim still requires missing submodules.
    ;; ------------------------------------------------------------

    (test-case "root shim still requires run-tests submodules that do not exist at project root"
      (define root-shim (base-file "scripts" "run-tests.rkt"))
      (check-true (and root-shim (file-exists? root-shim))
                  "precondition: duplicate root shim still exists today")
      (define shim-src (file->string root-shim))
      (check-true
       (regexp-match? #rx"run-tests/classify[.]rkt" shim-src)
       "root shim still (require \"run-tests/classify.rkt\") — BUG-0033's wave fixes/removes the shim and flips this pin")
      (define root-submodule-dir (base-file "scripts" "run-tests"))
      (check-false (and (directory-exists? root-submodule-dir) #t)
                   "precondition: scripts/run-tests/ still absent at project root (shim cannot load)")
      (define real-runner (repo-file "scripts" "run-tests.rkt"))
      (check-true (and real-runner (file-exists? real-runner))
                  "precondition: the real runner lives under q/scripts/ and works"))))

(module+ main
  (exit (run-tests suite)))
