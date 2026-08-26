#lang racket

;; tests/test-reset-reconciliation-characterization.rkt
;;
;; CHARACTERIZATION (W0) — pins the CURRENT /reset behavior for BUG-0036:
;;
;;   `/reset` (cmd-reset → gsm-reset!) resets ONLY in-memory state
;;   (mode → idle, wave-executor dropped). Durable artifacts of the
;;   abandoned campaign — .planning/campaigns/<hash>.rktd (waves stuck in
;;   non-terminal statuses) and its .lock — are left completely untouched:
;;   no reconciliation to a terminal state, no listing, no cleanup offer.
;;   Nothing ever mentions them again.
;;
;; Pin convention: every test here PASSES against today's behavior and is
;; FLIPPED into a fix-regression test by BUG-0036's owning wave (reset
;; detects the durable record, marks non-terminal waves interrupted,
;; emits a summary line; /gsd lists orphans).
;; Pure-level pin: temp campaign record + the real cmd-reset entry — NO
;; live TUI/worker subprocess.

(require racket/file
         racket/format
         racket/list
         rackunit
         rackunit/text-ui
         "../extensions/gsd/core.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt")

(define (make-temp-planning-with-campaign)
  ;; Minimal valid manifest; plan-id MUST equal the manifest hash (the
  ;; repository fails closed otherwise), so it is derived, never hardcoded.
  (define manifest (make-campaign-manifest 1 "reset-pin-campaign" '() '() "pin-constraints"))
  (define wave (make-campaign-wave* 0 "W0" 'in-progress 2 #f))
  (define rec
    (make-campaign-record (campaign-manifest-hash manifest) manifest (list wave) #f #f 'pin 1 2))
  (define tmp (make-temporary-file "reset-reconciliation-pin~a" 'directory))
  (persist-campaign! tmp rec)
  (define rktd
    (car (filter (lambda (p) (regexp-match? #rx"[.]rktd$" p))
                 (find-files (lambda (p) (file-exists? p)) tmp))))
  (define lock (path-replace-extension rktd ".lock"))
  (call-with-output-file lock (lambda (out) (display "pin-lock\n" out)) #:exists 'truncate)
  (values tmp rec rktd lock))

(define suite
  (test-suite "BUG-0036 characterization: /reset leaves durable campaign record + lock untouched, unlisted"

    (test-case "cmd-reset returns plain idle message mentioning no campaign artifacts"
      (define result (cmd-reset))
      (define msg (format "~a" result))
      (check-true (regexp-match? #rx"reset to idle" msg)
                  "reset reports the in-memory idle transition")
      ;; THE PIN (listing half): no orphan/campaign/lock mention ever.
      (check-false
       (regexp-match? #rx"campaign" (string-downcase msg))
       "reset output mentions NO campaign artifact today; BUG-0036's wave adds a summary line and flips this pin")
      (check-false (regexp-match? #rx"lock" (string-downcase msg))
                   "reset output mentions NO lock today"))

    (test-case "reset leaves non-terminal campaign record and .lock untouched on disk"
      (define-values (tmp rec rktd lock) (make-temp-planning-with-campaign))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
         (define before (file->bytes rktd))
         (cmd-reset)
         ;; Record untouched: byte-identical, still loadable, waves still
         ;; non-terminal (never reconciled to interrupted/cancelled).
         (check-equal? (file->bytes rktd)
                       before
                       "campaign record is byte-identical across reset today")
         (define reloaded (load-campaign-record tmp (campaign-plan-id rec)))
         (check-true (and reloaded #t) "record still loadable after reset")
         (define statuses
           (map (lambda (w) (format "~a" (campaign-wave-status w))) (campaign-record-waves reloaded)))
         (check-true
          (andmap (lambda (s) (not (regexp-match? #rx"interrupted|cancelled" s))) statuses)
          "wave statuses remain NON-terminal after reset today — BUG-0036's reconciliation flips this pin")
         ;; Lock remains: reset never removes or reports it.
         (check-true (file-exists? lock) ".lock still exists after reset today"))
       (lambda () (delete-directory/files tmp))))

    (test-case "absent-seam marker: reset path references no campaign reconciliation"
      ;; BUG-0036 acceptance #1 needs cmd-reset/gsm-reset! to reconcile the
      ;; durable record. Today neither reset implementation touches
      ;; campaign durability — pinned by scanning the reset sources for any
      ;; reconciliation concept.
      (define core-src (file->string (build-path here "extensions" "gsd" "core.rkt")))
      (define sm-src (file->string (build-path here "extensions" "gsd" "state-machine.rkt")))
      (for ([src (in-list (list (cons "core.rkt" core-src) (cons "state-machine.rkt" sm-src)))])
        (define body
          (or (regexp-match #px"(?s:\\(define \\(cmd-reset\\).*?\\(define \\(cmd-done)" (cdr src))
              (regexp-match #px"(?s:\\(define \\(gsm-reset!\\).*?\\(define \\(reset-gsm!)"
                            (cdr src))))
        (check-true (and body #t) (format "reset definition found in ~a" (car src)))
        (when body
          (define def (car body))
          (check-false
           (regexp-match? #rx"campaign|interrupted|orphan|lock" def)
           (format "~a reset body references campaign reconciliation — BUG-0036 landed; flip this pin"
                   (car src))))))))

;; ------------------------------------------------------------
;; Path helper (this test works from any invocation cwd)
;; ------------------------------------------------------------
(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
(define here (simplify-path (build-path this-file 'up 'up)))

(module+ main
  (exit (run-tests suite)))
