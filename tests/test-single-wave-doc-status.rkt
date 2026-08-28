#lang racket

;;; test-single-wave-doc-status.rkt — W0 characterization pin for
;;; BUG-0050: `check-status-consistency` reads ONLY the top `Status:`
;;; header line of a wave doc (wave-header-rx anchors at the first line
;;; after `# Wave N`). A contradicting `Status: PENDING` line in the BODY
;;; of the doc is invisible to the checker — zero warnings are produced
;;; even though the document declares two different statuses.
;;;
;;; Flip owner: W2 (single wave-doc status). When W2 makes the checker
;;; reject any non-header `Status:` line, this pin must be flipped into a
;;; fix-regression test asserting a warning/divergence IS reported.

(require rackunit
         racket/file
         racket/string
         "../extensions/gsd/wave-docs.rkt")

(define base
  (make-temporary-file "bug0050-base~a" 'directory))

(define (cleanup!)
  (with-handlers ([exn:fail? void])
    (delete-directory/files base)))

(with-handlers ([exn:fail? (lambda (e) (cleanup!) (raise e))])

  ;; PLAN.md index row: explicit target → slug "pin".
  (make-directory* (build-path base ".planning" "waves"))
  (call-with-output-file #:exists 'truncate
    (build-path base ".planning" "PLAN.md")
    (lambda (out)
      (display "- [DONE] W0: Single status doc → waves/W0-pin.md\n" out)))

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

  ;; --- Pin: only the TOP `Status:` line is read; the body line is
  ;; invisible → zero warnings despite the internal contradiction.
  (check-equal?
   (check-status-consistency base)
   '()
   "dual Status lines pass check-status-consistency (only the top header line is read)")

  ;; Direct read-path confirmation: read-wave-doc reports the header
  ;; status, ignoring the body line.
  (define doc (read-wave-doc base 0 "pin"))
  (check-equal? (hash-ref doc 'status) "DONE"
                "read-wave-doc extracts only the top Status: header")

  (cleanup!))

(displayln "PASS test-single-wave-doc-status (BUG-0050 pin: body Status: line invisible to checker)")
