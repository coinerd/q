#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-tui-diagnostic-surface.rkt — BUG-0098 (R98): honest diagnostic
;; clipping in the TUI status line.
;;
;; Everything here drives the REAL installed wiring: a fixture TUI context
;; (make-tui-ctx), the REAL stderr guard (install-stderr-guard! — which swaps
;; current-logger exactly as production does), and the REAL installed status
;; callback from tui-init (make-stderr-status-callback). Bare `log-error` then
;; flows the production route: guard logger -> receiver thread -> spill file
;; -> callback -> status message on the fixture context.
;;
;; Contract under test:
;;  - compact 160-char preview KEPT, but an explicit truncation marker plus a
;;    discoverable pointer to the guard's REAL spill path is appended, so the
;;    recovery tail beyond char 160 is inspectable via that path;
;;  - preview is sanitized (C0/C1 neutralized except newline), multiline is
;;    shown with visible line markers (the status bar is single-line), and
;;    cutting never splits a grapheme cluster nor exceeds the cell budget;
;;  - spill-write failure and callback failure are DISCLOSED: the status
;;    distinguishes capture failure from a successful spill and never claims
;;    a full log that was not captured;
;;  - teardown with queued logs does not silently lose them; spill stays
;;    owner-only; memory stays bounded (status is one bounded string).

(require rackunit
         rackunit/text-ui
         (only-in "../tui/tui-init.rkt" make-stderr-status-callback)
         (only-in "../tui/stderr-guard.rkt"
                  install-stderr-guard!
                  restore-stderr-guard!
                  stderr-guard-active?
                  stderr-guard-spill-path
                  stderr-guard-spill-port)
         (only-in "../tui/context.rkt" make-tui-ctx tui-ctx-ui-state-box)
         (only-in "../tui/state-types.rkt" ui-state-status-message)
         (only-in "../tui/char-width.rkt" string-visible-width))

;; ── helpers ────────────────────────────────────────────────────────────────

(define (current-status ctx)
  (ui-state-status-message (unbox (tui-ctx-ui-state-box ctx))))

;; The callback route is asynchronous (guard receiver thread); poll bounded.
(define (wait-for-status! ctx [timeout-ms 5000])
  (let loop ([deadline (+ (current-inexact-milliseconds) timeout-ms)])
    (or (current-status ctx)
        (if (< (current-inexact-milliseconds) deadline)
            (begin (sleep 0.02) (loop deadline))
            #f))))

;; When a case emits a SECOND record, wait until the status actually CHANGES
;; (the surface overwrites the box; the first record's status would otherwise
;; be observed racily).
(define (wait-for-new-status! ctx previous [timeout-ms 5000])
  (let loop ([deadline (+ (current-inexact-milliseconds) timeout-ms)])
    (define s (current-status ctx))
    (if (and s previous (not (string=? s previous)))
        s
        (if (< (current-inexact-milliseconds) deadline)
            (begin (sleep 0.02) (loop deadline))
            #f))))

(define (spill-string path)
  (if (file-exists? path) (port->string (open-input-file path)) ""))

(define (spill-contains? path needle [timeout-ms 5000])
  (let loop ([deadline (+ (current-inexact-milliseconds) timeout-ms)])
    (or (string-contains? (spill-string path) needle)
        (if (< (current-inexact-milliseconds) deadline)
            (begin (sleep 0.02) (loop deadline))
            #f))))

;; Real installed wiring: fresh fixture ctx + real guard + REAL callback.
(define (install-fixture!)
  (define ctx (make-tui-ctx))
  (define spill (install-stderr-guard! #:on-error-record (make-stderr-status-callback ctx)))
  (values ctx spill))

;; Every case installs its own guard instance; restore even on failure.
(define (with-guard! body)
  (dynamic-wind void body (lambda () (restore-stderr-guard!))))

;; status -> two values: preview-string, truncated?
;; The surface appends "... [truncated; ...]" directly after the preview.
;; Note: records arrive via the named 'q-tui logger, so the delivered message
;; carries the existing "q-tui: " prefix (same as today's status line).
(define truncated-prefix-regex
  #rx"^error: (.*?)\\.\\.\\. \\[truncated;")

(define (extract-preview status)
  (cond
    [(regexp-match truncated-prefix-regex status)
     => (lambda (m) (values (second m) #t))]
    [else (values (substring status (string-length "error: ")) #f)]))

;; Independent test oracle: does a cut of `flat` at `cut` split a cluster?
;; Conservative extend set: combining marks, ZWJ, emoji skin-tone modifiers.
(define (extends-cluster? c)
  (define cp (char->integer c))
  (or (memq (char-general-category c) '(mn mc me))
      (= cp #x200D)
      (and (>= cp #x1F3FB) (<= cp #x1F3FF))))

(define (grapheme-clean-cut? flat cut)
  (and (or (= cut (string-length flat))
           (not (extends-cluster? (string-ref flat cut))))
       (or (zero? cut)
           (not (extends-cluster? (string-ref flat (sub1 cut)))))))

(define (string-raw-control-free? s)
  (for/and ([c (in-string s)])
    (define cp (char->integer c))
    (not (or (< cp #x20) (= cp #x7F) (<= #x80 cp #x9F)))))

(define family-emoji
  (string (integer->char #x1F468) (integer->char #x200D)
          (integer->char #x1F469) (integer->char #x200D)
          (integer->char #x1F467)))

;; The guard logger is named 'q-tui, so the receiver delivers the record with
;; this existing prefix (same string today's status line shows).
(define delivered-prefix "q-tui: ")

;; ── suite ──────────────────────────────────────────────────────────────────

(define diagnostic-surface-tests
  (test-suite "TUI diagnostic surface (BUG-0098/R98, real installed wiring)"

    (test-case "short record: full preview kept, no truncation claim"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (log-error "r98-short-record")
         (define status (wait-for-status! ctx))
         (check-true (string? status) "status must be set via the real callback route")
         (check-equal? status "error: q-tui: r98-short-record")
         (check-false (string-contains? status "truncated")
                      "nothing was cut: no truncation marker")
         (check-false (string-contains? status "full log")
                      "nothing was cut: no full-log claim needed"))))

    (test-case "exact-limit: fills the 160-char preview without truncation claim"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         ;; delivered = "q-tui: " (7) + 153 = exactly the 160-char budget
         (log-error (make-string 153 #\a))
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (define-values (preview truncated?) (extract-preview status))
         (check-equal? (string-length preview) 160 "160-char compact preview kept")
         (check-false truncated? "exact limit must not be flagged as truncated"))))

    (test-case "over-limit: explicit marker + real spill path; tail retrievable"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (log-error (string-append (make-string 300 #\a) "R98-TAIL-9876543210"))
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (define m (regexp-match #rx"^error: (.*?)\\.\\.\\. \\[truncated; full log: (.*)\\]$" status))
         (check-true (list? m) "status must carry truncation marker + full-log pointer")
         (when m
           (define preview (second m))
           (define pointed-path (third m))
           (check-false (string-contains? status "R98-TAIL-9876543210")
                        "recovery tail must NOT be crammed into the status line")
           (check-true (string-contains? (path->string spill) pointed-path)
                       "pointer must be the guard's real spill path")
           (check-true (<= (string-length preview) 160) "preview bounded at 160 chars")
           (check-true (<= (string-visible-width preview) 160) "preview bounded at 160 cells")
           (check-true (<= (string-length status) 300) "status stays bounded")
           ;; Recovery text beyond char 160 is actually inspectable via the path:
           (check-true (spill-contains? spill "R98-TAIL-9876543210")
                       "recovery tail retrievable from the spill file")))))

    (test-case "multiline: visible line markers, no raw newline in status, real newlines in spill"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (log-error "line1\nline2\nline3")
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (check-false (string-contains? status "\n")
                      "status bar is single-line: raw newline would corrupt the frame")
         (check-true (string-contains? status "\\n")
                     "line structure shown via visible \\\\n markers")
         (check-equal? (length (regexp-match* #rx"\\\\n" status))
                       2
                       "both line breaks accounted for")
         (check-true (spill-contains? spill "line1\nline2\nline3")
                     "spill record keeps the real newlines"))))

    (test-case "combining accents: intact when short, grapheme-safe cut when long"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         ;; Short: the accent stays attached.
         (log-error (string-append "cafe" (string (integer->char #x0301))))
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (check-true (string-contains? status
                                       (string-append "cafe" (string (integer->char #x0301))))
                     "combining accent must survive to the surface")
         ;; Long: the cut must not split the e+U+0301 cluster.
         ;; delivered = "q-tui: " (7) + 152 a's + "e" + U+0301 + tail, so the
         ;; 160-char boundary falls exactly between base and combining mark.
         (define long-msg
           (string-append (make-string 152 #\a)
                          "e" (string (integer->char #x0301))
                          "R98-TAIL-ACCENT"))
         (define delivered (string-append delivered-prefix long-msg))
         (log-error long-msg)
         (define status2 (wait-for-new-status! ctx status))
         (check-true (string? status2))
         (define-values (preview truncated?) (extract-preview status2))
         (check-true truncated? "over-budget record is marked truncated")
         (check-true (grapheme-clean-cut? delivered (string-length preview))
                     "cut must not split a grapheme cluster"))))

    (test-case "CJK: cell-width budget respected (2 cells per char)"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (log-error (make-string 200 (integer->char #x4E00)))
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (define-values (preview truncated?) (extract-preview status))
         (check-true truncated? "200 CJK chars exceed the cell budget -> marked")
         (check-true (<= (string-visible-width preview) 160)
                     "preview must stay within the 160-cell budget")
         (check-true (<= (string-length preview) 160) "and within 160 chars")
         (check-true (spill-contains? spill (make-string 200 (integer->char #x4E00)))
                     "full CJK record retrievable from the spill"))))

    (test-case "ZWJ emoji family: never cut inside the sequence"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (define msg
           (string-append (make-string 152 #\a) family-emoji "R98-TAIL-ZWJ"))
         (define delivered (string-append delivered-prefix msg))
         (log-error msg)
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (define-values (preview truncated?) (extract-preview status))
         (check-true truncated?)
         (check-true (grapheme-clean-cut? delivered (string-length preview))
                     "cut must not split the ZWJ sequence")
         (check-true
          (or (not (string-contains? preview (string (integer->char #x1F468))))
              (string-contains? preview family-emoji))
          "no lone family member may be shown as a partial sequence"))))

    (test-case "ANSI controls: neutralized in status, raw fidelity in spill"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (log-error (string-append "bad" "\u1b[31m" "RED" "\u1b[0m" "\rX\u7fY"))
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (check-true (string-raw-control-free? status)
                     "no C0/C1/DEL may reach the terminal via the status line")
         (check-true (string-contains? status "^[") "ESC shown as caret notation")
         (check-true (string-contains? status "^M") "CR shown as caret notation")
         (check-true (string-contains? status "^?") "DEL shown as caret notation")
         (check-true (spill-contains? spill "\u1b[31m")
                     "spill keeps the raw record for forensics"))))

    (test-case "spill-write failure: capture loss disclosed, no false full-log claim"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         ;; Inject the failure: closing the guard's private spill port makes
         ;; the next spill write raise inside the guard.
         (close-output-port (stderr-guard-spill-port))
         (log-error "r98-spill-failed-short")
         (define status (wait-for-status! ctx))
         (check-true (string? status))
         (check-true (string-contains? status "UNAVAILABLE")
                     "status must disclose the missing capture")
         (check-false (string-contains? status "full log:")
                      "must never claim a full log that was not captured")
         (check-false (string-contains? status (path->string spill))
                      "must not point at a spill that does not hold the record")
         ;; Truncated + failed: same honest disclosure.
         (log-error (string-append (make-string 300 #\b) "R98-TAIL-FAILED"))
         (define status2 (wait-for-new-status! ctx status))
         (check-true (string? status2))
         (check-true (string-contains? status2 "truncated; full log UNAVAILABLE")
                     "truncation marker must not promise a full log"))))

    (test-case "callback failure: bounded disclosure in spill, guard thread survives"
      (with-guard!
       (lambda ()
         (define spill
           (install-stderr-guard!
            #:on-error-record (lambda (msg captured) (error "r98 injected callback failure"))))
         (log-error "r98-callback-failure-record")
         (check-true (spill-contains? spill "r98-callback-failure-record")
                     "record is still captured before the callback runs")
         (check-true (spill-contains? spill "diagnostic surface FAILED")
                     "failed surface attempt must be disclosed, not silent")
         ;; The guard thread must survive and process the NEXT record.
         (log-error "r98-after-failure-record")
         (check-true (spill-contains? spill "r98-after-failure-record")
                     "guard must keep draining after a callback failure")
         (check-true (stderr-guard-active?)))))

    (test-case "teardown with queued logs: drained without loss, logger restored"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (define probe (make-log-receiver (current-logger) 'error))
         (log-error "r98-teardown-1")
         (log-error "r98-teardown-2")
         (log-error "r98-teardown-3")
         ;; Confirm delivery to receivers, then restore immediately: the guard
         ;; must still drain its queued copy instead of dropping it.
         (check-true (and (sync/timeout 5 probe) #t) "probe receiver got the records")
         (restore-stderr-guard!)
         (check-false (stderr-guard-active?))
         (check-true (spill-contains? spill "r98-teardown-3")
                     "queued record survives teardown")
         ;; Post-restore records leave the guard alone.
         (define spill-before (spill-string spill))
         (log-error "r98-post-restore-record")
         (sleep 0.15)
         (check-equal? (spill-string spill) spill-before
                       "no guard spill activity after restore"))))

    (test-case "immediate teardown race: restore stays clean and total"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (log-error "r98-race-record")
         (restore-stderr-guard!)
         (check-false (stderr-guard-active?) "restore completes without error"))))

    (test-case "spill file is owner-only"
      (with-guard!
       (lambda ()
         (define-values (ctx spill) (install-fixture!))
         (check-equal? (file-or-directory-permissions spill) '(write read)
                       "spill permissions must be owner-only (0600)"))))))

(module+ main
  (run-tests diagnostic-surface-tests))
