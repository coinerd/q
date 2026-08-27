#lang racket

;; tests/test-status-consistency-characterization.rkt
;;
;; CHARACTERIZATION (W0) → FLIPPED (W2) — pins the dual-source wave-status
;; behavior for BUG-0034 (+ the BUG-0035 inline-format half):
;;
;; BUG-0034: wave status lives TWICE — `- [STATUS] Wn:` rows in
;; .planning/PLAN.md and `Status:` headers in waves/Wn-*.md. W0 pinned
;; that doctored divergence was silently accepted (no read-path check).
;; W2 FLIPPED those pins: check-status-consistency now reports each
;; divergence (wave-idx, row status, doc status, both file paths),
;; /go and /gsd surface them as named warnings (never blocking), and
;; documented precedence (doc header wins for progress statuses; PLAN
;; row wins ONLY for [DEFERRED]) resolves next-wave selection.
;;
;; BUG-0035 half: a status-less relaxed row `- W2: Title` is silently
;; seeded as INBOX, and inline-only plans parse via the inline fallback
;; with NO deprecation indication (BUG-0035) — still pinned, still to
;; be flipped by BUG-0035's owning wave.
;;
;; Pure-level tests: temp .planning tree + parsers only, NO live
;; TUI/worker subprocess.

(require racket/file
         racket/format
         racket/list
         racket/string
         rackunit
         rackunit/text-ui
         "../extensions/gsd/wave-docs.rkt")

(define (make-temp-planning)
  (define tmp (make-temporary-file "status-consistency-pin~a" 'directory))
  (make-directory* (build-path tmp ".planning" "waves"))
  tmp)

(define (write-plan! tmp text)
  (call-with-output-file (build-path tmp ".planning" "PLAN.md")
                         (lambda (out) (display text out))
                         #:exists 'truncate))

(define PLAN-TEXT
  (string-append "# Plan\n\n"
                 "- [DONE] W0: First → waves/W0-first.md\n"
                 "- [Inbox] W1: Second → waves/W1-second.md\n"
                 "- W2: Relaxed Title → waves/W2-relaxed-title.md\n"))

;; Consistent variant: every existing doc agrees with its PLAN row.
(define PLAN-CONSISTENT
  (string-append "# Plan\n\n"
                 "- [Inbox] W0: First → waves/W0-first.md\n"
                 "- [DONE] W1: Second → waves/W1-second.md\n"
                 "- W2: Relaxed Title → waves/W2-relaxed-title.md\n"))

;; Divergent variant for precedence/selection: W0 row DONE vs doc PENDING
;; (doctored revert), W1 row DEFERRED vs doc DONE (the live BUG-0034
;; incident shape), W2 Inbox with PENDING doc (canonically equal).
(define PLAN-PRECEDENCE
  (string-append "# Plan\n\n"
                 "- [DONE] W0: First → waves/W0-first.md\n"
                 "- [DEFERRED] W1: Second → waves/W1-second.md\n"
                 "- [Inbox] W2: Third → waves/W2-third.md\n"))

(define suite
  (test-suite (string-append
               "BUG-0034 W2 flip / BUG-0035 characterization: dual wave-status divergences are loud, "
               "precedence is deterministic; inline/relaxed formats still accepted silently")
    (test-case "BUG-0034 W2 flip: PLAN row DONE vs wave-doc PENDING is REPORTED with both statuses and both file paths"
      (define tmp (make-temp-planning))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
         (write-plan! tmp PLAN-TEXT)
         ;; Wave doc exists but its Status: header DISAGREES with the row.
         (define doc-path
           (write-wave-doc! tmp 0 "first" "# Goal\n\nPin the loud divergence.\n" "PENDING"))
         (check-true (file-exists? doc-path))
         ;; W1 agrees (Inbox row vs Inbox doc header) and the relaxed W2
         ;; row has no doc (BUG-0023 territory, not a consistency concern)
         ;; so exactly ONE divergence must come back: W0.
         (define divs (check-status-consistency tmp))
         (check-equal? (length divs) 1 "exactly one divergence (W0)")
         (define d (car divs))
         (check-true (status-divergence? d))
         (check-equal? (status-divergence-wave-idx d) 0)
         (check-true (string-contains? (string-upcase (format "~a" (status-divergence-plan-status d)))
                                       "DONE")
                     "divergence carries the PLAN.md row status")
         (check-true (string-contains? (string-upcase (format "~a" (status-divergence-doc-status d)))
                                       "PENDING")
                     "divergence carries the wave-doc header status")
         (check-equal? (status-divergence-plan-path d)
                       ".planning/PLAN.md"
                       "divergence names the PLAN.md path")
         (check-equal? (status-divergence-doc-path d)
                       ".planning/waves/W0-first.md"
                       "divergence names the wave-doc path")

         ;; The user-facing warning names BOTH files and the wave index.
         (define warning (format-status-divergence-warning d))
         (check-true (string-contains? warning ".planning/PLAN.md") "warning names the PLAN.md path")
         (check-true (string-contains? warning ".planning/waves/W0-first.md")
                     "warning names the wave-doc path")
         (check-true (string-contains? warning "W0") "warning names the wave")

         ;; Side A/B preconditions still hold (parsers unchanged).
         (define entries (parse-plan-index PLAN-TEXT))
         (check-true (= (length entries) 3) "3 index rows parse (DONE, Inbox, relaxed)")
         (define w0 (findf (lambda (e) (= (wave-index-entry-idx e) 0)) entries))
         (check-true (and w0 #t) "W0 entry exists")
         (check-true (string=? (string-upcase (format "~a" (wave-index-entry-status w0))) "DONE")
                     "PLAN.md row parses as DONE")
         (define doc (read-wave-doc tmp 0 "first"))
         (check-true (hash? doc) "wave doc reads back")
         (check-true (string=? (string-upcase (format "~a" (hash-ref doc 'status))) "PENDING")
                     "wave doc header parses as PENDING"))
       (lambda () (delete-directory/files tmp))))

    (test-case "BUG-0034 W2: consistent plan is SILENT (no divergences)"
      (define tmp (make-temp-planning))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
         (write-plan! tmp PLAN-CONSISTENT)
         (write-wave-doc! tmp 0 "first" "# Goal\n\n0\n" "Inbox")
         (write-wave-doc! tmp 1 "second" "# Goal\n\n1\n" "DONE")
         ;; W2 (relaxed row, Inbox) has NO doc: skipped, not a divergence.
         (check-equal? (check-status-consistency tmp) '() "consistent plan reports nothing"))
       (lambda () (delete-directory/files tmp))))

    (test-case "BUG-0034 W2: canonically-equal spellings are not divergences"
      (define tmp (make-temp-planning))
      (dynamic-wind (lambda () #f)
                    (lambda ()
                      (write-plan! tmp "# Plan\n\n- [Inbox] W0: First → waves/W0-first.md\n")
                      ;; Doc spells the same meaning differently: PENDING == Inbox row.
                      (write-wave-doc! tmp 0 "first" "# Goal\n\n0\n" "PENDING")
                      (check-equal? (check-status-consistency tmp)
                                    '()
                                    "Inbox row vs PENDING doc header is NOT a divergence"))
                    (lambda () (delete-directory/files tmp))))

    (test-case "BUG-0034 W2: documented precedence — doc header wins for progress, PLAN row wins only for [DEFERRED]"
      (check-equal? (resolve-status-precedence "DONE" "PENDING")
                    "PENDING"
                    "progress: doc header wins")
      (check-equal? (resolve-status-precedence "Inbox" "DONE") "DONE" "progress: doc header wins")
      (check-equal? (resolve-status-precedence "Inbox" "DEFERRED")
                    "DEFERRED"
                    "only the PLAN ROW being DEFERRED pins deferred")
      (check-equal? (resolve-status-precedence "DEFERRED" "DONE")
                    "DEFERRED"
                    "DEFERRED row wins over a stale DONE doc header")
      (check-equal? (resolve-status-precedence "In-Progress" "In-Progress")
                    "In-Progress"
                    "agreeing statuses pass through"))

    (test-case "BUG-0034 W2: precedence resolves next-wave selection"
      (define tmp (make-temp-planning))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
         (write-plan! tmp PLAN-PRECEDENCE)
         ;; W0: row [DONE], doc PENDING → doc wins → PENDING → selectable.
         (write-wave-doc! tmp 0 "first" "# Goal\n\n0\n" "PENDING")
         ;; W1: row [DEFERRED], doc DONE → row wins → stays deferred.
         (write-wave-doc! tmp 1 "second" "# Goal\n\n1\n" "DONE")
         ;; W2: row [Inbox], doc PENDING → canonically equal, pending.
         (write-wave-doc! tmp 2 "third" "# Goal\n\n2\n" "PENDING")
         ;; Both doctored waves are reported as divergences (2), and the
         ;; doctored DONE row on W0 no longer buries the wave: selection
         ;; sees the doc's PENDING and picks W0.
         (check-equal? (length (check-status-consistency tmp))
                       2
                       "W0 (done|pending) and W1 (deferred|done) diverge; W2 agrees")
         (define nxt (next-inbox-wave tmp))
         (check-true (and nxt #t) "a next wave is selected")
         (check-equal? (wave-index-entry-idx nxt)
                       0
                       "doc-header PENDING resurrects W0 for selection despite [DONE] row")
         ;; And in a plan whose rows and docs agree, selection is unchanged:
         ;; the first canonically pending wave wins.
         (write-plan! tmp PLAN-CONSISTENT)
         (write-wave-doc! tmp 0 "first" "# Goal\n\n0\n" "PENDING")
         (write-wave-doc! tmp 1 "second" "# Goal\n\n1\n" "DONE")
         (write-wave-doc! tmp 2 "third" "# Goal\n\n2\n" "PENDING")
         (check-equal? (check-status-consistency tmp) '() "agreeing plan is silent")
         (define nxt2 (next-inbox-wave tmp))
         (check-true (and nxt2 #t))
         (check-equal? (wave-index-entry-idx nxt2)
                       0
                       "consistent plan: first pending wave (W0) selected"))
       (lambda () (delete-directory/files tmp))))

    (test-case "BUG-0034 W2 flip: the status-consistency seam now EXISTS on the GSD read path"
      (check-true (divergence-check-symbol-present?)
                  "the read-path comparison exists (W2); W0 pinned its absence and this pin flipped"))

    (test-case "BUG-0034: relaxed status-less row silently seeds INBOX (BUG-0035 half)"
      (define entries (parse-plan-index PLAN-TEXT))
      (define relaxed (findf (lambda (e) (= (wave-index-entry-idx e) 2)) entries))
      (define inbox (findf (lambda (e) (= (wave-index-entry-idx e) 1)) entries))
      (check-true (and relaxed inbox #t))
      ;; Same status as an EXPLICIT [Inbox] row — compared structurally so
      ;; the pin holds regardless of the status representation.
      (check-equal? (wave-index-entry-status relaxed)
                    (wave-index-entry-status inbox)
                    "status-less relaxed row seeds as INBOX with zero warning today"))

    (test-case "BUG-0035 flip: inline-only plan detected by inline counter, deprecation surface now EXISTS"
      (define inline-plan
        (string-append "# Plan\n\n"
                       "## Wave 0: Inline Title\n\nFiles: q/foo.rkt\n\n"
                       "## Wave 1: Inline Second\n\nFiles: q/bar.rkt\n\n"))
      ;; The index parser still sees NO index rows for an inline-only plan
      ;; (the warning is advisory: non-fatal per campaign gate #6).
      (check-equal? (parse-plan-index inline-plan)
                    '()
                    "inline-only plan yields zero index entries (falls to inline grammar)")
      ;; ...while the inline sections ARE there (the fallback would engage).
      (check-true (= (count-inline-wave-sections inline-plan) 2) "2 inline wave sections counted")
      ;; And the deprecation surface EXISTS since W6 (this pin flipped from
      ;; its W0 check-false form: "no deprecation warning exists today").
      (check-true (deprecation-symbol-present?)
                  "BUG-0035 W6: deprecation warnings exist for the inline/relaxed grammars"))))

;; ------------------------------------------------------------
;; Source-surface absence scans (absent-seam markers)
;; ------------------------------------------------------------

(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
(define here (simplify-path (build-path this-file 'up 'up)))
(define (repo-file . parts)
  (apply build-path (cons here parts)))

;; BUG-0034 seam: a read-path comparison between PLAN.md rows and wave-doc
;; headers. ABSENT in W0 (mark-wave-status! only dual-WRITES); PRESENT
;; since W2 (check-status-consistency + /go + /gsd warnings).
(define BUG-0034-SCAN-FILES
  (list (repo-file "extensions" "gsd" "wave-docs.rkt")
        (repo-file "extensions" "gsd" "plan-validator.rkt")
        (repo-file "extensions" "gsd" "go-orchestrator.rkt")
        (repo-file "extensions" "gsd" "command-handlers.rkt")))

(define BUG-0034-SCAN-PATTERNS (list #rx"divergen" #rx"status-mismatch" #rx"status-consistency"))

(define (divergence-check-symbol-present?)
  (for*/or ([path (in-list BUG-0034-SCAN-FILES)]
            #:when (file-exists? path)
            [rx (in-list BUG-0034-SCAN-PATTERNS)])
    (regexp-match? rx (file->string path))))

;; BUG-0035 seam: a deprecation notice for the inline/relaxed grammars.
(define BUG-0035-SCAN-FILES
  (list (repo-file "extensions" "gsd" "wave-docs.rkt")
        (repo-file "extensions" "gsd" "plan-types-parser.rkt")
        (repo-file "extensions" "gsd" "plan-validator.rkt")))

(define BUG-0035-SCAN-PATTERNS (list #rx"deprecat"))

(define (deprecation-symbol-present?)
  (for*/or ([path (in-list BUG-0035-SCAN-FILES)]
            #:when (file-exists? path)
            [rx (in-list BUG-0035-SCAN-PATTERNS)])
    (regexp-match? rx (file->string path))))

(module+ main
  (exit (run-tests suite)))
