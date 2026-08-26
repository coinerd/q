#lang racket/base

;; @speed fast
;; @suite gsd
;; @boundary pure

;; q/tests/test-stall-detection-characterization.rkt
;;
;; W0 CHARACTERIZATION PIN — BUG-0037
;;
;; Pins the CURRENT (defective) semantics of the mutation-stall watchdog:
;; the detector counts ALL tool calls against one flat mutation-free
;; budget and does NOT distinguish "same call repeated" from "distinct
;; files being read". Consequences pinned here:
;;
;;   (a) 60+ DISTINCT read calls (healthy deep exploration of different
;;       files — the exact W5 live incident) → hard-stall kill is
;;       signaled today.
;;   (b) Repeated IDENTICAL calls (a genuinely stuck executor) →
;;       hard-stall kill is signaled (the intended behavior).
;;   (c) The pure fold's snapshot carries NO repetition/distinctness
;;       accounting at all — no field exists that a repetition-based
;;       detector could consult.
;;
;; FLIP CONTRACT: W1 (waves/W1-stall-detection-repetition-retry.md)
;; flips (a) and (c): distinct-path reads must no longer trip the hard
;; limit, and repetition accounting must appear in the snapshot. Only
;; (b) — identical repetition still kills — may survive unchanged.
;;
;; Pure-level pin: synthetic tool-call records only; no live TUI or
;; worker subprocess is spawned.

(require racket/list
         racket/format
         racket/file
         racket/path
         racket/string
         rackunit
         rackunit/text-ui
         "../extensions/gsd/wave-executor.rkt")

;; ---- helpers ----------------------------------------------------------

;; A tool-call record shaped like the post-tool-result hook's payload.
(define (call name [arguments #f])
  (if arguments
      (hasheq 'name name 'arguments arguments)
      (hasheq 'name name)))

;; N DISTINCT read calls — different file paths, i.e. healthy exploration.
;; This is exactly the shape of the W5 live kill: sequential reads of
;; campaign-state.rkt, go-orchestrator.rkt, characterization tests, ...
;; all different targets, zero mutations.
(define (distinct-reads n)
  (build-list n
              (lambda (i)
                (call 'read (hasheq 'path (~a "/repo/q/extensions/gsd/module-" i ".rkt"))))))

;; N IDENTICAL read calls — same file, same arguments, the true-stall shape.
(define (identical-reads n)
  (build-list n
              (lambda (_) (call 'read (hasheq 'path "/repo/q/extensions/gsd/go-orchestrator.rkt")))))

;; Feed every record through a fresh watchdog as its own single-call
;; batch (that is how the live hook sees them) and return the verdicts.
(define (verdict-after records)
  (define wd
    (make-stall-watchdog #:soft-limit STALL-SOFT-LIMIT-DEFAULT #:hard-limit STALL-HARD-LIMIT-DEFAULT))
  (for/list ([r (in-list records)])
    (stall-watchdog-observe! wd (list r))))

(define (final-verdict records)
  (last (verdict-after records)))

;; Extract the source lines of one definition (from the line containing
;; start-marker up to the line containing end-marker) from a file next
;; to this test. File-content pin helper.
(define (definition-source-lines path start-marker end-marker)
  (define lines (file->lines path))
  (define start (index-where lines (lambda (l) (string-contains? l start-marker))))
  (define end (index-where lines (lambda (l) (string-contains? l end-marker))))
  (unless (and start end (> end start))
    (fail (format "marker not found in ~a: '~a'..~a" path start-marker end-marker)))
  (string-join (take (drop lines start) (- end start)) "\n"))

;; ============================================================
;; (a) BUG-0037 pin: 60+ DISTINCT reads trip the hard limit TODAY
;; ============================================================

(define stall-suite
  (test-suite "BUG-0037 W0 pin: trip-on-count semantics (distinct reads are killed too)"

    (test-case "default hard limit is 60 (the flat budget)"
      (check-equal? STALL-HARD-LIMIT-DEFAULT 60))

    (test-case "60 DISTINCT read calls → hard-stall kill signaled (defect)"
      ;; Healthy exploration of 60 different files dies today. The
      ;; detector uses (>= since 60): the 60th call itself trips.
      (define verdicts (verdict-after (distinct-reads 60)))
      (check-eq? (last verdicts)
                 'hard-stall
                 "expected distinct-read exploration to be hard-killed today (BUG-0037)")
      ;; And the kill happens exactly at call 60, not before:
      (check-false (memq 'hard-stall (take verdicts 59))
                   "hard limit must trip at call 60 under flat-count semantics")
      (check-eq? (list-ref verdicts 59) 'hard-stall))

    (test-case "70 distinct greps — still killed; distinctness never exempts"
      (define records
        (build-list 70
                    (lambda (i)
                      (call 'grep (hasheq 'pattern "stall" 'path (~a "/repo/q/dir-" i "/"))))))
      (check-eq? (final-verdict records) 'hard-stall "distinct grep exploration is killed today too"))

    (test-case "mixed distinct exploration (read+grep+find, 65 calls) → killed"
      (define records
        (append
         (distinct-reads 20)
         (build-list 25 (lambda (i) (call 'grep (hasheq 'pattern "x" 'path (~a "/repo/q/m" i)))))
         (build-list 20
                     (lambda (i) (call 'find (hasheq 'name (~a "*.rkt") 'path (~a "/repo/q/f" i)))))))
      (check-eq? (final-verdict records) 'hard-stall))

    ;; ============================================================
    ;; (b) True-stall shape: repeated IDENTICAL calls also trip
    ;; ============================================================

    (test-case "60 IDENTICAL read calls → hard-stall kill (intended behavior)"
      (define verdicts (verdict-after (identical-reads 60)))
      (check-eq? (last verdicts)
                 'hard-stall
                 "identical-call repetition must still be killed after W1"))

    (test-case "60 identical reads and 60 distinct reads are treated IDENTICALLY"
      ;; The core defect in one assertion: today's detector cannot tell
      ;; these two sessions apart. Both verdict vectors are equal.
      (define distinct (verdict-after (distinct-reads 60)))
      (define identical (verdict-after (identical-reads 60)))
      (check-equal?
       distinct
       identical
       "watchdog verdicts must be identical for distinct vs repeated calls (BUG-0037 defect)"))

    ;; ============================================================
    ;; (c) BUG-0037 pin: the snapshot has NO repetition/distinctness accounting
    ;; ============================================================

    (test-case "stall-state carries no distinct-path or repetition fields"
      ;; The pure fold's snapshot keys: only flat counters exist. A
      ;; repetition-based detector needs distinct-tool+args accounting;
      ;; none of these keys may appear today. W1 adds the seam.
      (define st-distinct (stall-state (distinct-reads 40)))
      (define st-identical (stall-state (identical-reads 40)))
      (check-equal? (hash-ref st-distinct 'calls-since-mutation) 40)
      (check-equal? (hash-ref st-identical 'calls-since-mutation) 40)
      ;; The two snapshots are indistinguishable today — that IS the defect.
      (check-equal?
       st-distinct
       st-identical
       "snapshots for distinct vs repeated reads must be equal today (no repetition accounting)")
      (for ([key (in-list '(distinct-calls distinct-paths
                                           repeated-calls
                                           repetition-window
                                           distinct-tool-hashes))])
        (check-false (hash-has-key? st-distinct key)
                     (format "snapshot unexpectedly carries ~a — W1 seam landed early?" key))))

    (test-case "hard-failure message names only the flat count, no distinctness info"
      ;; go-orchestrator's kill message reports "N tool calls without any
      ;; file mutation" — nothing about how many DISTINCT tools were
      ;; called (BUG-0037 acceptance criterion 3 is the flip).
      (define here-dir (simplify-path (path-only (find-system-path 'run-file))))
      (define src-path (build-path here-dir 'up "extensions" "gsd" "go-orchestrator.rkt"))
      (define msg-body
        (definition-source-lines src-path
                                 "(define (stall-hard-failure-message"
                                 "Steering injection hook"))
      (check-true (string-contains? msg-body "tool calls without any file mutation")
                  "flat-count kill message wording moved — pin needs re-anchoring")
      (check-false (string-contains? msg-body "distinct")
                   "kill message unexpectedly mentions distinctness — W1 landed early?")
      (check-false (string-contains? msg-body "repeat")
                   "kill message unexpectedly mentions repetition — W1 landed early?"))))

(module+ main
  (exit (run-tests stall-suite)))
