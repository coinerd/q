#lang racket

;;; test-single-wave-doc-status.rkt — fix-regression test for BUG-0050
;;; (W2: single authoritative wave-doc Status line). Originally a W0
;;; characterization pin asserting the checker read ONLY the top
;;; `Status:` header (body contradiction invisible). W2 flipped it:
;;;
;;;   * check-status-consistency reports a 'body-vs-header divergence
;;;     when a doc body carries a second line-anchored `Status:` line.
;;;   * The authoring path (write-wave-doc!) strips body `Status:`
;;;     lines, so every authored doc carries exactly ONE `Status:` line
;;;     (the machine header).
;;;   * mark-wave-status! rewrites route through the same sanitizer, so
;;;     legacy dual-Status docs self-heal on the next transition.
;;;   * lint-wave-doc flags a body `Status:` line as 'duplicate-status
;;;     (advisory, by name).

(require rackunit
         racket/file
         racket/list
         racket/string
         "../extensions/gsd/wave-docs.rkt")

(define base
  (make-temporary-file "bug0050-base~a" 'directory))

(define (cleanup!)
  (with-handlers ([exn:fail? void])
    (delete-directory/files base)))

(define body-status-lines
  (let ([rx #rx"^Status:"])
    (lambda (text)
      (count (lambda (l) (regexp-match? rx l)) (string-split text "\n")))))

(with-handlers ([exn:fail? (lambda (e) (cleanup!) (raise e))])

  ;; PLAN.md index rows: W0 (contradiction pin), W1 (self-heal target).
  (make-directory* (build-path base ".planning" "waves"))
  (call-with-output-file #:exists 'truncate
    (build-path base ".planning" "PLAN.md")
    (lambda (out)
      (display (string-append
                "- [DONE] W0: Single status doc → waves/W0-pin.md\n"
                "- [DONE] W1: Self-heal target → waves/W1-heal.md\n")
               out)))

  ;; Wave doc whose TOP header says DONE but whose BODY carries a
  ;; contradicting `Status: PENDING` line.
  (call-with-output-file #:exists 'truncate
    (build-path base ".planning" "waves" "W0-pin.md")
    (lambda (out)
      (display (string-append
                "# Wave 0\n"
                "Status: DONE\n"
                "\n"
                "## Body\n"
                "\n"
                "Status: PENDING\n"
                "Contradicting status line inside the body.\n")
               out)))

  ;; Legacy dual-Status doc (stale authored-template residue) for the
  ;; mark-wave-status! self-heal case.
  (call-with-output-file #:exists 'truncate
    (build-path base ".planning" "waves" "W1-heal.md")
    (lambda (out)
      (display (string-append
                "# Wave 1\n"
                "Status: DONE\n"
                "\n"
                "## Body\n"
                "\n"
                "Status: PENDING\n"
                "stale authored-template residue\n")
               out)))

  ;; --- FLIPPED PIN: body `Status:` lines are now DETECTED. Both
  ;; legacy docs (W0-pin, W1-heal) diverge with kind 'body-vs-header,
  ;; naming both values.
  (define divs (check-status-consistency base))
  (check-equal? (length divs) 2
                "each dual-Status doc produces exactly one divergence")
  (define w0-div
    (findf (lambda (d) (= (status-divergence-wave-idx d) 0)) divs))
  (check-not-false w0-div "W0 divergence present")
  (check-equal? (status-divergence-kind w0-div) 'body-vs-header
                "a doc body Status: line is a 'body-vs-header divergence")
  (check-equal? (status-divergence-doc-status w0-div) "PENDING"
                "the stale body value is reported")
  (check-equal? (status-divergence-plan-status w0-div) "DONE"
                "the authoritative header value is reported")
  ;; The named warning is user-visible and names BUG-0050.
  (check-true (string-contains? (format-status-divergence-warning w0-div)
                                "BUG-0050")
              "the warning names BUG-0050")

  ;; Direct read-path: the header status still wins for 'status.
  (define doc (read-wave-doc base 0 "pin"))
  (check-equal? (hash-ref doc 'status) "DONE"
                "read-wave-doc extracts only the top Status: header")

  ;; --- Lint: a body `Status:` line is a named 'duplicate-status
  ;; violation.
  (check-not-false (memq 'duplicate-status
                    (map wave-doc-violation-section (lint-wave-doc doc)))
              "lint-wave-doc flags a body Status: line as duplicate-status")

  ;; --- Clean single-Status doc: zero divergences for that doc, lint
  ;; clean on duplicate-status, mid-line prose mentions untouched.
  ;; Overwrite the INDEXED W0-pin.md via the authoring path.
  (write-wave-doc! base 0 "pin"
                   (string-append "## Body\n"
                                  "\n"
                                  "Prose that merely mentions `Status:` mid-line "
                                  "is not a marker line.\n")
                   "DONE")
  (define mid-divs (check-status-consistency base))
  (check-equal? (length mid-divs) 1
                "only the W1 legacy doc diverges once W0 is re-authored clean")
  (check-equal? (status-divergence-wave-idx (car mid-divs)) 1)
  (check-equal? (status-divergence-kind (car mid-divs)) 'body-vs-header)
  (check-false (memq 'duplicate-status
                     (map wave-doc-violation-section
                          (lint-wave-doc (read-wave-doc base 0 "pin"))))
               "clean single-Status doc has no duplicate-status violation")

  ;; --- Authoring path sanitization: writing a body that STILL carries
  ;; a `Status:` line emits a doc with exactly ONE `Status:` line (the
  ;; machine header). Prose mid-line mentions survive untouched.
  (define path (write-wave-doc! base 0 "authored"
                                (string-append "## Body\n"
                                               "\n"
                                               "Status: PENDING\n"
                                               "legacy template residue\n"
                                               "mention of `Status:` in prose\n")
                                "In-Progress"))
  (define authored-text (call-with-input-file path port->string))
  (check-equal? (body-status-lines authored-text) 1
                "write-wave-doc! emits exactly one Status: line")
  (check-true (string-contains? authored-text "mention of `Status:` in prose")
              "mid-line prose mentions of Status: survive sanitization")
  (check-false (string-contains? authored-text "Status: PENDING")
               "the stale body Status: PENDING line is stripped")
  (check-false (memq 'duplicate-status
                     (map wave-doc-violation-section
                          (lint-wave-doc (read-wave-doc base 0 "authored"))))
               "sanitized authored doc lints clean on duplicate-status")

  ;; --- Self-heal: a legacy dual-Status doc is sanitized by the next
  ;; sanctioned status transition (mark-wave-status! rewrites through
  ;; write-wave-doc!).
  (mark-wave-status! base 1 "Done")
  (define healed-text
    (call-with-input-file
        (build-path base ".planning" "waves" "W1-heal.md") port->string))
  (check-equal? (body-status-lines healed-text) 1
                "mark-wave-status! self-heals a legacy dual-Status doc")
  (define healed (read-wave-doc base 1 "heal"))
  (check-equal? (hash-ref healed 'status) "Done"
                "mark-wave-status! header wins after transition")
  (check-false (memq 'duplicate-status
                     (map wave-doc-violation-section (lint-wave-doc healed)))
               "the healed doc lints clean on duplicate-status")

  ;; --- Post-heal: the whole campaign is divergence-free.
  (check-equal? (check-status-consistency base) '()
                "after self-heal every doc carries exactly one Status line")

  (cleanup!))

(displayln "PASS test-single-wave-doc-status (BUG-0050 fix: single authoritative Status line + contradiction detection)")
