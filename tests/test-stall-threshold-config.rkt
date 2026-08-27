#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-stall-threshold-config.rkt
;; BUG-0044 characterization pin (v1.00.21 W0; FLIPPED by W1).
;;
;; TODAY the stall watchdog's thresholds are compile-time constants.
;; The single construction site — wrap-run-one-with-stall-watchdog,
;; defined and called only in go-orchestrator.rkt — wires the
;; STALL-*-DEFAULT constants unconditionally. run-campaign-wave
;; exposes only #:stall-soft-limit / #:stall-hard-limit keyword
;; overrides (themselves defaulting to the constants); there are no
;; window/backstop keywords, no gsd.stall.* settings keys, no
;; settings-query accessor, and no consumption path. Every assertion
;; below PASSES against today's red behavior; W1 flips them once
;; settings keys override the defaults.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/path
         racket/runtime-path
         "../extensions/gsd/wave-executor.rkt")

(define-runtime-path go-orchestrator-src "../extensions/gsd/go-orchestrator.rkt")
(define-runtime-path wave-executor-src "../extensions/gsd/wave-executor.rkt")
(define-runtime-path settings-query-src "../runtime/settings-query.rkt")

;; ── The pinned constants ──────────────────────────────────────

(define stall-config-suite
  (test-suite "BUG-0044 characterization: stall thresholds are hard-coded (W0 pin; W1 flips)"
    (test-case "STALL-*-DEFAULT constants are 8/15/30/300"
      (check-equal? STALL-SOFT-LIMIT-DEFAULT 8)
      (check-equal? STALL-HARD-LIMIT-DEFAULT 15)
      (check-equal? STALL-REPETITION-WINDOW-DEFAULT 30)
      (check-equal? STALL-BACKSTOP-LIMIT-DEFAULT 300))

    ;; Behavioral probe: the watchdog the orchestrator actually
    ;; constructs (no arguments at the call site beyond soft/hard, which
    ;; themselves default to the constants) produces 8/15/30/300 behavior
    ;; regardless of any settings file — no settings value can reach it.
    (test-case "default-constructed watchdog is 8/15/30/300 — settings cannot influence it"
      (define wd (make-stall-watchdog))
      (check-equal? (stall-watchdog-soft-limit wd) 8)
      (check-equal? (stall-watchdog-hard-limit wd) 15)
      (check-equal? (stall-watchdog-window wd) 30)
      (check-equal? (stall-watchdog-backstop wd) 300))

    (test-case "the only override surface is soft/hard; window/backstop are constants"
      (define wd (make-stall-watchdog #:soft-limit 3 #:hard-limit 4))
      (check-equal? (stall-watchdog-soft-limit wd) 3)
      (check-equal? (stall-watchdog-hard-limit wd) 4)
      (check-equal? (stall-watchdog-window wd) 30)
      (check-equal? (stall-watchdog-backstop wd) 300))

    ;; ── Source-surface pins on the construction site ──────────────

    (test-case "wrap-run-one-with-stall-watchdog is defined in go-orchestrator.rkt only"
      (define orch (file->string go-orchestrator-src))
      (define wx (file->string wave-executor-src))
      (check-true (string-contains? orch "(define (wrap-run-one-with-stall-watchdog"))
      ;; the constants and make-stall-watchdog live in wave-executor.rkt …
      (check-true (string-contains? wx "(define STALL-SOFT-LIMIT-DEFAULT 8)"))
      (check-true (string-contains? wx "(define (make-stall-watchdog"))
      ;; … but wave-executor does NOT define the wrapper (single site)
      (check-false (string-contains? wx "wrap-run-one-with-stall-watchdog")))

    (test-case "run-campaign-wave exposes only soft/hard keyword overrides (no window/backstop keywords)"
      (define orch (file->string go-orchestrator-src))
      (check-true (string-contains? orch
                                    "#:stall-soft-limit [stall-soft-limit STALL-SOFT-LIMIT-DEFAULT]"))
      (check-true (string-contains? orch
                                    "#:stall-hard-limit [stall-hard-limit STALL-HARD-LIMIT-DEFAULT]"))
      (check-false (string-contains? orch "stall-window"))
      (check-false (string-contains? orch "stall-backstop"))
      (check-false (string-contains? orch "STALL-REPETITION-WINDOW-DEFAULT"))
      (check-false (string-contains? orch "STALL-BACKSTOP-LIMIT-DEFAULT")))

    (test-case "the watchdog construction passes only soft/hard limits"
      (define orch (file->string go-orchestrator-src))
      (check-true (string-contains? orch "(make-stall-watchdog #:soft-limit stall-soft-limit"))
      (check-true (string-contains? orch "#:hard-limit stall-hard-limit)")))

    (test-case "go-orchestrator reads no settings anywhere (no settings seam at the watchdog site)"
      (define orch (file->string go-orchestrator-src))
      (check-false (string-contains? orch "setting-ref"))
      (check-false (string-contains? orch "gsd.stall"))
      (check-false (string-contains? orch "load-q-settings")))

    ;; ── Absent-seam markers (v1.00.19 freshness-pin precedent) ────

    (test-case "settings-query provides no stall accessor (seam absent; contrast: worktree-isolation has one)"
      (define sq (file->string settings-query-src))
      (check-false (string-contains? sq "stall")
                   "TODAY no gsd.stall.* accessor exists — the seam gsd.stall.* would need is absent")
      ;; the equivalent seam already exists for other gsd.* flags, which is
      ;; what makes this a missing-accessor defect rather than a missing
      ;; settings capability:
      (check-true (string-contains? sq "gsd-worktree-isolation-enabled?")))

    (test-case "no gsd.stall key is read anywhere under extensions/gsd"
      (define gsd-dir (simplify-path (build-path (path-only go-orchestrator-src))))
      (check-false
       (for/or ([f (in-list (find-files (lambda (p)
                                          (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                               (not (string-contains? (path->string p) "compiled"))))
                                        gsd-dir))])
         (string-contains? (file->string f) "gsd.stall"))))))

(module+ main
  (exit (run-tests stall-config-suite)))
