#lang racket/base

;; q/tests/test-gsd-stall-policy.rkt — characterization tests for the
;; stall-policy seam (BUG-0042, v1.00.22 W7).
;;
;; W7 extracts stall messages + classification predicates from
;; go-orchestrator.rkt VERBATIM. These tests pin the CURRENT outputs
;; (message formats incl. the diversity-gate wording, stall-cause
;; classification, no-change rejection parsing, notification-kind
;; mapping) so the extraction is provably behavior-preserving: they ran
;; against the pre-extraction inline definitions and must keep passing
;; against the module. The go-orchestrator re-export shim is pinned by
;; checking the re-provided bindings are IDENTICAL to the module's.

(require racket/string
         racket/format
         rackunit
         rackunit/text-ui
         (only-in "../runtime/settings-core.rkt" q-settings)
         "../extensions/gsd/stall-policy.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         (prefix-in s: "../extensions/gsd/stall-policy.rkt")
         (prefix-in g: "../extensions/gsd/go-orchestrator.rkt"))

(define hard-basic
  "mutation-stall watchdog: attempt terminated after 12 mutation-free calls (limit 30). \
Target files: a.rkt, b.rkt. Recent tools: read, grep. The attempt will be \
re-attempted automatically with its prior context preserved — resume \
implementation from recorded state.")

(define stall-policy-suite
  (test-suite "gsd-stall-policy"

    ;; ---- 8. go-orchestrator compat shim: identical re-provided bindings --

    (test-case "go-orchestrator re-exports the stall-policy bindings themselves"
      (for ([probe (list (cons s:stall-cause-message? g:stall-cause-message?)
                         (cons s:stall-hard-failure-message g:stall-hard-failure-message)
                         (cons s:stall-steering-message g:stall-steering-message)
                         (cons s:no-change-rejection? g:no-change-rejection?)
                         (cons s:no-change-target-files g:no-change-target-files)
                         (cons s:wave-failure-notification-kind g:wave-failure-notification-kind)
                         (cons s:make-gsd-stall-exn g:make-gsd-stall-exn)
                         (cons s:gsd-stall-exn? g:gsd-stall-exn?)
                         (cons s:wave-doc-target-files g:wave-doc-target-files)
                         (cons s:wrap-run-one-with-stall-watchdog g:wrap-run-one-with-stall-watchdog)
                         (cons s:resolve-effective-stall-thresholds
                               g:resolve-effective-stall-thresholds))])
        (check-true (eq? (car probe) (cdr probe)) (format "shim diverged for ~s" (car probe)))))

    ;; ---- 1. stall-cause classification (BUG-0037 W1 / W6 BUG-0040) --------

    (test-case "stall-cause-prefix is the canonical watchdog prefix"
      (check-equal? stall-cause-prefix "mutation-stall watchdog:"))

    (test-case "stall-cause-message? matches only the canonical prefix"
      (check-true (stall-cause-message?
                   "mutation-stall watchdog: attempt terminated after 12 mutation-free calls"))
      (check-true (stall-cause-message? stall-cause-prefix))
      (check-false (stall-cause-message? "mutation-stall watchdog")) ; missing colon
      (check-false (stall-cause-message? "runtime watchdog: killed"))
      (check-false (stall-cause-message? ""))
      (check-false (stall-cause-message? 42))
      (check-false (stall-cause-message? #f)))

    ;; ---- 2. hard-failure message format (retryable infra vocabulary) ------

    (test-case "stall-hard-failure-message: full format, targets and recent tools joined"
      (check-equal? (stall-hard-failure-message 12 30 '("a.rkt" "b.rkt") #f '("read" "grep"))
                    hard-basic))

    (test-case "stall-hard-failure-message: defaults (no stall tool, no recent tools)"
      (check-equal?
       (stall-hard-failure-message 3 3 '("x.rkt"))
       "mutation-stall watchdog: attempt terminated after 3 mutation-free calls (limit 3). \
Target files: x.rkt. Recent tools: (none recorded). The attempt will be \
re-attempted automatically with its prior context preserved — resume \
implementation from recorded state."))

    (test-case "stall-hard-failure-message: repeating tool clause"
      (check-equal?
       (stall-hard-failure-message 9 15 '() 'read)
       "mutation-stall watchdog: attempt terminated after 9 mutation-free calls \
(limit 15) — repeating 'read'. Target files: (none recorded). Recent tools: \
(none recorded). The attempt will be re-attempted automatically with its \
prior context preserved — resume implementation from recorded state."))

    (test-case "hard-failure output is itself classified as a stall cause (round trip)"
      (check-true (stall-cause-message? (stall-hard-failure-message 5 8 '("f.rkt")))))

    ;; ---- 3. soft-limit steering message (reuses W2 re-anchor) ------------

    (test-case "stall-steering-message: soft-limit block, counts, targets, order"
      (define msg
        (stall-steering-message 5 "W3" "camp-42" "W3: extract modules" '("q/a.rkt" "q/b.rkt")))
      (check-true (string-contains? msg "[MUTATION-STALL WATCHDOG — SOFT LIMIT REACHED]"))
      (check-true (string-contains?
                   msg
                   "You have made 5 calls without any edit. Wave targets: q/a.rkt, q/b.rkt. "))
      (check-true (string-contains? msg "Begin the first edit now."))
      ;; re-anchor constructor must travel with the steering (W2 seam)
      (check-true
       (string-contains? msg "(no edit has been made yet — this session has only read/explored)"))
      (check-true (string-contains? msg "W3: extract modules")))

    (test-case "stall-steering-message: no recorded targets degrade, not crash"
      (define msg (stall-steering-message 2 "W0" "camp-1" "W0: baseline" '()))
      (check-true (string-contains? msg "Wave targets: (none recorded).")))

    ;; ---- 4. no-change rejection (v1.00.17 W3 — #9515) ---------------------

    (test-case "no-change-rejection? matches the verifier's zero-change verdict"
      (check-true (no-change-rejection? "no wave target files changed: q/a.rkt, q/b.rkt"))
      (check-false (no-change-rejection? "verifier rejected"))
      (check-false (no-change-rejection? ""))
      (check-false (no-change-rejection? 'sym)))

    (test-case "no-change-target-files recovers the comma-space-joined list"
      (check-equal? (no-change-target-files "no wave target files changed: q/a.rkt, q/b.rkt")
                    '("q/a.rkt" "q/b.rkt"))
      (check-equal? (no-change-target-files "no wave target files changed: q/only.rkt")
                    '("q/only.rkt")))

    ;; ---- 5. notification-kind mapping (W6 BUG-0040) -----------------------

    (test-case "wave-failure-notification-kind separates stall-terminal from wave-failed"
      (check-eq? (wave-failure-notification-kind (stall-hard-failure-message 4 8 '("f.rkt")))
                 'stall-terminal)
      (check-eq? (wave-failure-notification-kind "runner error") 'wave-failed)
      (check-eq? (wave-failure-notification-kind "") 'wave-failed))

    ;; ---- 6. gsd-stall-exn protocol -----------------------------------------

    (test-case "gsd-stall-exn is a transparent exn:fail subtype with message intact"
      (define e (make-gsd-stall-exn hard-basic))
      (check-true (gsd-stall-exn? e))
      (check-true (exn:fail? e))
      (check-equal? (exn-message e) hard-basic)
      (check-false (gsd-stall-exn? (exn:fail "plain" (current-continuation-marks)))))

    ;; ---- 7. wave-doc File: line parsing ------------------------------------

    (test-case "wave-file-line-rx captures the first path token for - and * bullets"
      (check-equal? (cadr (regexp-match wave-file-line-rx "- File: q/a.rkt [exists]")) "q/a.rkt")
      (check-equal? (cadr (regexp-match wave-file-line-rx "* File: q/b.rkt")) "q/b.rkt")
      (check-equal? (cadr (regexp-match wave-file-line-rx "-   File:q/c.rkt")) "q/c.rkt")
      (check-false (regexp-match? wave-file-line-rx "- NotFile: q/a.rkt"))
      (check-false (regexp-match? wave-file-line-rx "File: q/a.rkt")))

    (test-case "wave-doc-target-files degrades when the doc cannot be read"
      (check-equal? (or (wave-doc-target-files "/nonexistent/base-dir-xyz" 3) '()) '()))

    ;; ---- 9. effective-threshold composition (W1 BUG-0044 → W7 seam) -------

    (test-case "resolve-effective-stall-thresholds: no settings, no overrides → 8/15/30/300"
      (define-values (soft hard window backstop) (resolve-effective-stall-thresholds #f))
      (check-eq? soft 8)
      (check-eq? hard 15)
      (check-eq? window 30)
      (check-eq? backstop 300))

    (test-case "resolve-effective-stall-thresholds: keyword overrides win over settings"
      (define settings
        (q-settings (hash) (hash) (hash 'gsd (hash 'stall (hash 'soft-limit 12 'hard-limit 40)))))
      (define-values (soft hard window backstop)
        (resolve-effective-stall-thresholds settings #:soft 5 #:hard 50 #:window 7 #:backstop 77))
      (check-eq? soft 5)
      (check-eq? hard 50)
      (check-eq? window 7)
      (check-eq? backstop 77))

    (test-case "resolve-effective-stall-thresholds: settings keys fill unset slots"
      (define settings
        (q-settings (hash) (hash) (hash 'gsd (hash 'stall (hash 'soft-limit 12 'window 31)))))
      (define-values (soft hard window backstop) (resolve-effective-stall-thresholds settings))
      (check-eq? soft 12)
      (check-eq? hard 15) ; key absent → default
      (check-eq? window 31)
      (check-eq? backstop 300))

    (test-case "resolve-effective-stall-thresholds: disabled (#f) key falls back to default"
      ;; gsd.stall.* = #f means "disabled"; the accessors return #f and the
      ;; composition treats it as absent → canonical default (pre-W7 behavior).
      (define settings (q-settings (hash) (hash) (hash 'gsd (hash 'stall (hash 'soft-limit #f)))))
      (define-values (soft hard window backstop) (resolve-effective-stall-thresholds settings))
      (check-eq? soft 8)
      (check-eq? hard 15))))

(module+ main
  (exit (run-tests stall-policy-suite)))

(module+ test
  (void (run-tests stall-policy-suite)))
