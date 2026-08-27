#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-stall-threshold-config.rkt
;; BUG-0044 settings-key pin (v1.00.21 W1; FLIPPED from W0).
;;
;; The stall watchdog thresholds are SETTINGS-DRIVEN:
;;   gsd.stall.soft-limit / gsd.stall.hard-limit / gsd.stall.window /
;;   gsd.stall.backstop
;; read at watchdog construction through the settings-query seam
;; (go-orchestrator.rkt composition root). Keys absent → the
;; 8/15/30/300 defaults; invalid values → defaults + warning, never a
;; mid-campaign crash; keyword overrides on run-campaign-wave win over
;; settings; #f disables a limit. W1 asserts the flips: a settings
;; file with soft-limit 3 changes trip behavior WITHOUT source edits,
;; absent keys keep the defaults, and the startup log line exposes the
;; effective thresholds.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/path
         racket/runtime-path
         racket/format
         "../extensions/gsd/wave-executor.rkt"
         "../runtime/settings-query.rkt"
         (only-in "../runtime/settings.rkt" load-settings))

(define-runtime-path go-orchestrator-src "../extensions/gsd/go-orchestrator.rkt")
(define-runtime-path wave-executor-src "../extensions/gsd/wave-executor.rkt")
(define-runtime-path settings-query-src "../runtime/settings-query.rkt")

;; ── Synthetic settings file helper (BUG-0044 action 4) ─────────
;; Writes a config.json shaped like the real <proj>/.q/config.json and
;; loads it via the same load-settings seam the orchestrator uses, so
;; the pin exercises the on-disk path (no source edits required).

(define (write-settings-file json-content)
  (define dir (make-temporary-file "stall-cfg-~a" 'directory))
  (define cfg-dir (build-path dir ".q"))
  (make-directory* cfg-dir)
  (define cfg-path (build-path cfg-dir "config.json"))
  (call-with-output-file cfg-path (lambda (out) (display json-content out)) #:exists 'truncate)
  cfg-path)

;; ── The pinned defaults ────────────────────────────────────────

(define stall-config-suite
  (test-suite "BUG-0044: stall thresholds are settings-driven (W1 flip)"
    (test-case "STALL-*-DEFAULT constants are 8/15/30/300"
      (check-equal? STALL-SOFT-LIMIT-DEFAULT 8)
      (check-equal? STALL-HARD-LIMIT-DEFAULT 15)
      (check-equal? STALL-REPETITION-WINDOW-DEFAULT 30)
      (check-equal? STALL-BACKSTOP-LIMIT-DEFAULT 300))

    (test-case "default-constructed watchdog is 8/15/30/300"
      (define wd (make-stall-watchdog))
      (check-equal? (stall-watchdog-soft-limit wd) 8)
      (check-equal? (stall-watchdog-hard-limit wd) 15)
      (check-equal? (stall-watchdog-window wd) 30)
      (check-equal? (stall-watchdog-backstop wd) 300))

    (test-case "all four keyword overrides reach the watchdog"
      (define wd (make-stall-watchdog #:soft-limit 3 #:hard-limit 4 #:window 5 #:backstop 6))
      (check-equal? (stall-watchdog-soft-limit wd) 3)
      (check-equal? (stall-watchdog-hard-limit wd) 4)
      (check-equal? (stall-watchdog-window wd) 5)
      (check-equal? (stall-watchdog-backstop wd) 6))

    ;; ── Settings seam behavior (action 1 + action 4) ─────────────

    (test-case "absent keys → 8/15/30/300 defaults (no settings object)"
      (check-equal? (gsd-stall-soft-limit #f) 8)
      (check-equal? (gsd-stall-hard-limit #f) 15)
      (check-equal? (gsd-stall-window #f) 30)
      (check-equal? (gsd-stall-backstop #f) 300))

    (test-case "synthetic settings file with soft-limit 3 changes trip behavior without source edits"
      (define cfg (write-settings-file "{\"gsd\":{\"stall\":{\"soft-limit\":3}}}"))
      (define settings (load-settings #:config-path cfg))
      (check-equal? (gsd-stall-soft-limit settings) 3)
      ;; untouched keys still default
      (check-equal? (gsd-stall-hard-limit settings) 15)
      (check-equal? (gsd-stall-window settings) 30)
      (check-equal? (gsd-stall-backstop settings) 300)
      ;; the value actually changes a constructed watchdog's behavior
      (define wd (make-stall-watchdog #:soft-limit (gsd-stall-soft-limit settings)))
      (check-equal? (stall-watchdog-soft-limit wd) 3))

    (test-case "all four keys from a settings file override the defaults"
      (define cfg
        (write-settings-file
         "{\"gsd\":{\"stall\":{\"soft-limit\":2,\"hard-limit\":4,\"window\":6,\"backstop\":8}}}"))
      (define settings (load-settings #:config-path cfg))
      (check-equal? (gsd-stall-soft-limit settings) 2)
      (check-equal? (gsd-stall-hard-limit settings) 4)
      (check-equal? (gsd-stall-window settings) 6)
      (check-equal? (gsd-stall-backstop settings) 8))

    (test-case "invalid (non-positive/non-integer) values fall back to defaults — never crash"
      (define cfg
        (write-settings-file
         "{\"gsd\":{\"stall\":{\"soft-limit\":\"banana\",\"hard-limit\":-3,\"window\":1.5,\"backstop\":\"\"}}}"))
      (define settings (load-settings #:config-path cfg))
      (check-equal? (gsd-stall-soft-limit settings) 8)
      (check-equal? (gsd-stall-hard-limit settings) 15)
      (check-equal? (gsd-stall-window settings) 30)
      (check-equal? (gsd-stall-backstop settings) 300))

    ;; ── Source-surface pins on the composition root ──────────────

    (test-case "defaults live in settings-query.rkt and are re-exported by wave-executor"
      (define sq (file->string settings-query-src))
      (define wx (file->string wave-executor-src))
      (check-true (string-contains? sq "(define STALL-SOFT-LIMIT-DEFAULT 8)"))
      (check-true (string-contains? sq "(define STALL-HARD-LIMIT-DEFAULT 15)"))
      (check-true (string-contains? sq "(define STALL-REPETITION-WINDOW-DEFAULT 30)"))
      (check-true (string-contains? sq "(define STALL-BACKSTOP-LIMIT-DEFAULT 300)"))
      (check-true (string-contains? wx "STALL-SOFT-LIMIT-DEFAULT"))
      (check-true (string-contains? wx "(define (make-stall-watchdog"))
      ;; wrapper still defined only in go-orchestrator (single site)
      (check-false (string-contains? wx "wrap-run-one-with-stall-watchdog")))

    (test-case "run-campaign-wave exposes all four keyword overrides defaulting to 'unset"
      (define orch (file->string go-orchestrator-src))
      (check-true (string-contains? orch "#:stall-soft-limit [stall-soft-limit 'unset]"))
      (check-true (string-contains? orch "#:stall-hard-limit [stall-hard-limit 'unset]"))
      (check-true (string-contains? orch "#:stall-window [stall-window 'unset]"))
      (check-true (string-contains? orch "#:stall-backstop [stall-backstop 'unset]")))

    (test-case "watchdog construction forwards all four effective values"
      (define orch (file->string go-orchestrator-src))
      (check-true (string-contains? orch
                                    "(make-stall-watchdog #:soft-limit effective-stall-soft-limit"))
      (check-true (string-contains? orch "#:hard-limit effective-stall-hard-limit"))
      (check-true (string-contains? orch "#:window effective-stall-window"))
      (check-true (string-contains? orch "#:backstop effective-stall-backstop")))

    (test-case "settings (via the settings-query seam) are the source when keys are present"
      (define orch (file->string go-orchestrator-src))
      (check-true (string-contains? orch "gsd-stall-soft-limit"))
      (check-true (string-contains? orch "gsd-stall-hard-limit"))
      (check-true (string-contains? orch "gsd-stall-window"))
      (check-true (string-contains? orch "gsd-stall-backstop"))
      ;; keyword overrides win over settings ('unset → settings → default)
      (check-true (string-contains? orch "if (eq? stall-soft-limit 'unset)")))

    (test-case "startup log line exposes the EFFECTIVE thresholds once per wave (action 3)"
      (define orch (file->string go-orchestrator-src))
      (check-true (string-contains? orch "effective stall thresholds"))
      (check-true (string-contains? orch "soft=~a hard=~a window=~a backstop=~a"))
      (check-true (string-contains? orch "effective-stall-soft-limit"))
      (check-true (string-contains? orch "effective-stall-window"))
      (check-true (string-contains? orch "effective-stall-backstop")))))

(module+ main
  (exit (run-tests stall-config-suite)))
