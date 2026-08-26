#lang racket

;; tests/test-status-consistency-characterization.rkt
;;
;; CHARACTERIZATION (W0) — pins the CURRENT dual-source wave-status behavior
;; for BUG-0034 (+ the BUG-0035 inline-format half):
;;
;; BUG-0034: wave status lives TWICE — `- [STATUS] Wn:` rows in
;; .planning/PLAN.md and `Status:` headers in waves/Wn-*.md. Both parsers
;; (parse-plan-index / read-wave-doc) happily return CONTRADICTORY statuses
;; and NO read-path check compares them: doctored divergence is silently
;; accepted. Pin: a PLAN row saying DONE with a wave doc saying PENDING
;; parses cleanly on both sides with zero divergence signal.
;;
;; BUG-0035 half: a status-less relaxed row `- W2: Title` is silently
;; seeded as INBOX, and inline-only plans parse via the inline fallback
;; with NO deprecation indication (BUG-0035).
;;
;; Pin convention: every test PASSES today; BUG-0034's owning wave flips
;; the divergence pins (warning + precedence), BUG-0035's wave flips the
;; deprecation pins. Pure-level pin: temp .planning tree + parsers only,
;; NO live TUI/worker subprocess.

(require racket/file
         racket/format
         racket/list
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

(define suite
  (test-suite "BUG-0034/0035 characterization: dual wave-status sources accept divergence silently; inline/relaxed formats accepted without
    deprecation"

    (test-case "BUG-0034: PLAN row DONE vs wave-doc PENDING → no divergence reported anywhere"
      (define tmp (make-temp-planning))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
         (write-plan! tmp PLAN-TEXT)
         ;; Wave doc exists but its Status: header DISAGREES with the row.
         (define doc-path
           (write-wave-doc! (build-path tmp ".planning")
                            0
                            "first"
                            "# Goal\n\nPin the silent divergence.\n"
                            "PENDING"))
         (check-true (file-exists? doc-path))

         ;; Side A: the index parser reports the row status (DONE).
         (define entries (parse-plan-index PLAN-TEXT))
         (check-true (= (length entries) 3) "3 index rows parse (DONE, Inbox, relaxed)")
         (define w0 (findf (lambda (e) (= (wave-index-entry-idx e) 0)) entries))
         (check-true (and w0 #t) "W0 entry exists")
         (check-true (regexp-match? #rx"done"
                                    (string-downcase (format "~a" (wave-index-entry-status w0))))
                     "PLAN.md row parses as DONE")

         ;; Side B: the doc parser reports the header status (PENDING).
         (define doc (read-wave-doc (build-path tmp ".planning") 0 "first"))
         (check-true (hash? doc) "wave doc reads back")
         (check-true (regexp-match? #rx"pending"
                                    (string-downcase (format "~a" (hash-ref doc 'status))))
                     "wave doc header parses as PENDING")

         ;; THE PIN: the two sources contradict each other and NOTHING
         ;; signals it. Neither parser raises, and no read-path comparison
         ;; exists anywhere on the GSD surface (source scan mirrors the
         ;; predecessor freshness-guard absent-seam pin precedent).
         (check-true (not (string=? (string-downcase (format "~a" (wave-index-entry-status w0)))
                                    (string-downcase (format "~a" (hash-ref doc 'status)))))
                     "precondition: sources genuinely diverge")
         (check-false
          (divergence-check-symbol-present?)
          "no status-divergence check exists today; BUG-0034's wave adds one and flips this pin"))
       (lambda () (delete-directory/files tmp))))

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

    (test-case "BUG-0035: inline-only plan detected by inline counter, invisible to index parser, no deprecation"
      (define inline-plan
        (string-append "# Plan\n\n"
                       "## Wave 0: Inline Title\n\nFiles: q/foo.rkt\n\n"
                       "## Wave 1: Inline Second\n\nFiles: q/bar.rkt\n\n"))
      ;; The index parser sees NO index rows for an inline-only plan...
      (check-equal? (parse-plan-index inline-plan)
                    '()
                    "inline-only plan yields zero index entries (falls to inline grammar)")
      ;; ...while the inline sections ARE there (the fallback would engage).
      (check-true (= (count-inline-wave-sections inline-plan) 2) "2 inline wave sections counted")
      ;; And no deprecation surface exists anywhere in the GSD plan modules.
      (check-false
       (deprecation-symbol-present?)
       "no deprecation warning exists for the inline grammar today; BUG-0035's wave adds one and flips this pin"))))

;; ------------------------------------------------------------
;; Source-surface absence scans (absent-seam markers)
;; ------------------------------------------------------------

(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
(define here (simplify-path (build-path this-file 'up 'up)))
(define (repo-file . parts)
  (apply build-path (cons here parts)))

;; BUG-0034 seam: a read-path comparison between PLAN.md rows and wave-doc
;; headers. Today: absent (mark-wave-status! only dual-WRITES).
(define BUG-0034-SCAN-FILES
  (list (repo-file "extensions" "gsd" "wave-docs.rkt")
        (repo-file "extensions" "gsd" "plan-validator.rkt")
        (repo-file "extensions" "gsd" "go-orchestrator.rkt")))

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
