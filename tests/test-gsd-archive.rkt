#lang racket

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; BOUNDARY: integration

;; tests/test-gsd-archive.rkt — Tests for GSD archive workflow
;; Issues #2155, #2156, #2157, #2158

(require rackunit
         racket/file
         racket/path
         racket/string
         "../extensions/gsd/archive.rkt"
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/wave-docs.rkt"
         "../extensions/gsd/plan-types.rkt"
         (only-in "../extensions/gsd/command-types.rkt"
                  gsd-command-result-success
                  gsd-command-result-message
                  gsd-command-result-data))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-tmp-planning-dir)
  (define tmp (make-temporary-file "gsd-archive-test-~a" 'directory))
  (define planning (build-path tmp ".planning"))
  (make-directory planning)
  tmp)

(define (write-plan! base-dir #:status-lines [status-lines #f])
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (define content
    (string-append
     "# Plan: Test Archive Plan\n\n## Overview\nTest.\n\n## Waves\n"
     (or status-lines
         "- [DONE] W0: setup → waves/W0-setup.md\n- [DONE] W1: implement → waves/W1-implement.md\n")))
  (call-with-output-file plan-path (lambda (out) (display content out)) #:exists 'truncate))

(define (write-plan-incomplete! base-dir)
  (write-plan!
   base-dir
   #:status-lines
   "- [DONE] W0: setup → waves/W0-setup.md\n- [Inbox] W1: implement → waves/W1-implement.md\n"))

(define (write-state! base-dir)
  (define state-path (build-path base-dir ".planning" "STATE.md"))
  (call-with-output-file state-path
                         (lambda (out) (display "# State\nAll done." out))
                         #:exists 'truncate))

(define (write-wave! base-dir idx slug status content)
  (write-wave-doc! base-dir idx slug content status))

(define (cleanup-tmp base-dir)
  (when (directory-exists? base-dir)
    (delete-directory/files base-dir)))

;; ============================================================
;; Tests: all-waves-complete? (#2155)
;; ============================================================

(test-case "all-waves-complete?: #t when all DONE"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (write-plan! tmp)
                  (check-true (all-waves-complete? tmp)))
                (lambda () (cleanup-tmp tmp))))

(test-case "all-waves-complete?: #f when some Inbox"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (write-plan-incomplete! tmp)
                  (check-false (all-waves-complete? tmp)))
                (lambda () (cleanup-tmp tmp))))

(test-case "all-waves-complete?: #f when no PLAN.md"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda () (check-false (all-waves-complete? tmp)))
                (lambda () (cleanup-tmp tmp))))

(test-case "all-waves-complete?: #t with DONE + DEFERRED"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     (write-plan!
      tmp
      #:status-lines
      "- [DONE] W0: setup → waves/W0-setup.md\n- [DEFERRED] W1: optional → waves/W1-optional.md\n")
     (check-true (all-waves-complete? tmp)))
   (lambda () (cleanup-tmp tmp))))

;; ============================================================
;; Tests: archive-path-for-plan (#2155)
;; ============================================================

(test-case "archive-path-for-plan: generates correct path"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (define result (archive-path-for-plan tmp "My Cool Plan"))
                  (check-true (string-suffix? (path->string result) "my-cool-plan")))
                (lambda () (cleanup-tmp tmp))))

(test-case "archive-path-for-plan: collision appends timestamp"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  ;; Create existing archive dir
                  (define first-path (archive-path-for-plan tmp "Test Plan"))
                  (make-directory* first-path)
                  ;; Now ask for same plan — should get timestamp suffix
                  (define second-path (archive-path-for-plan tmp "Test Plan"))
                  (check-not-equal? first-path second-path)
                  (check-true (string-contains? (path->string second-path) "test-plan-")))
                (lambda () (cleanup-tmp tmp))))

;; ============================================================
;; Tests: archive-completed-plan! (#2155)
;; ============================================================

(test-case "archive-completed-plan!: archives all files"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     (write-plan! tmp)
     (write-state! tmp)
     (write-wave! tmp 0 "setup" "DONE" "## Root Cause\nSetup needed.\n## Action\nDo setup.")
     (write-wave! tmp 1 "implement" "DONE" "## Root Cause\nNeed impl.\n## Action\nImplement.")

     (define result (archive-completed-plan! tmp))
     (check-true (gsd-command-result-success result))
     (check-true (string-contains? (hash-ref (gsd-command-result-data result) 'archive-path)
                                   "archive"))
     ;; PLAN.md should no longer exist in .planning/
     (check-false (file-exists? (build-path tmp ".planning" "PLAN.md")))
     ;; Archive dir should exist with files
     (define archive-dir (string->path (hash-ref (gsd-command-result-data result) 'archive-path)))
     (check-true (directory-exists? archive-dir))
     (check-true (file-exists? (build-path archive-dir "PLAN.md"))))
   (lambda () (cleanup-tmp tmp))))

(test-case "archive-completed-plan!: fails when incomplete"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (write-plan-incomplete! tmp)
                  (define result (archive-completed-plan! tmp))
                  (check-false (gsd-command-result-success result))
                  (check-true (string-contains? (gsd-command-result-message result) "Not all waves")))
                (lambda () (cleanup-tmp tmp))))

(test-case "archive-completed-plan!: fails when no PLAN.md"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (define result (archive-completed-plan! tmp))
                  (check-false (gsd-command-result-success result))
                  (check-true (string-contains? (gsd-command-result-message result) "No PLAN.md")))
                (lambda () (cleanup-tmp tmp))))

;; ============================================================
;; Tests: reset-gsd-after-archive! (#2155)
;; ============================================================

(test-case "reset-gsd-after-archive!: resets state machine"
  ;; Transition to exploring first
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (check-eq? (gsm-current) 'exploring)
  ;; Reset
  (reset-gsd-after-archive!)
  (check-eq? (gsm-current) 'idle))

;; ============================================================
;; Tests: move-to-archive! (#2160)
;; ============================================================

(test-case "move-to-archive!: moves waves/ dir"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (write-plan! tmp)
                  (write-state! tmp)
                  (write-wave! tmp 0 "setup" "DONE" "Root cause.\nAction.\nVerify.")

                  (define archive-dir (build-path tmp ".planning" "archive" "test"))
                  (move-to-archive! tmp archive-dir)

                  ;; waves/ should have moved to archive
                  (check-true (directory-exists? (build-path archive-dir "waves")))
                  (check-false (directory-exists? (build-path tmp ".planning" "waves"))))
                (lambda () (cleanup-tmp tmp))))

(test-case "move-to-archive!: handles missing files gracefully"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (write-plan! tmp)
                  ;; No STATE.md, no wave docs
                  (define archive-dir (build-path tmp ".planning" "archive" "minimal"))
                  (move-to-archive! tmp archive-dir)
                  ;; Just PLAN.md moved
                  (check-true (file-exists? (build-path archive-dir "PLAN.md")))
                  (check-false (file-exists? (build-path archive-dir "STATE.md"))))
                (lambda () (cleanup-tmp tmp))))

;; ============================================================
;; Tests: mark-wave-status! updates PLAN.md (#2157)
;; ============================================================

(test-case "mark-wave-status! updates PLAN.md status marker"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     (write-plan!
      tmp
      #:status-lines
      "- [Inbox] W0: setup → waves/W0-setup.md\n- [Inbox] W1: implement → waves/W1-implement.md\n")
     ;; Mark W0 as DONE
     (mark-wave-status! tmp 0 "DONE")
     ;; Read PLAN.md and verify marker changed
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[DONE] W0:"))
     (check-true (string-contains? plan-text "[Inbox] W1:")))
   (lambda () (cleanup-tmp tmp))))

;; ============================================================
;; Tests: cleanup-empty-subdirs! (#2160)
;; ============================================================

(test-case "cleanup-empty-subdirs!: removes empty dirs"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  ;; Create an empty subdir
                  (define empty-dir (build-path tmp ".planning" "waves"))
                  (make-directory empty-dir)
                  (check-true (directory-exists? empty-dir))
                  ;; Cleanup
                  (cleanup-empty-subdirs! (build-path tmp ".planning"))
                  ;; Should be removed
                  (check-false (directory-exists? empty-dir)))
                (lambda () (cleanup-tmp tmp))))

(test-case "cleanup-empty-subdirs!: preserves non-empty dirs"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  ;; Create a subdir with a file
                  (define nonempty-dir (build-path tmp ".planning" "waves"))
                  (make-directory nonempty-dir)
                  (call-with-output-file (build-path nonempty-dir "W0.md")
                                         (lambda (out) (display "test" out)))
                  (cleanup-empty-subdirs! (build-path tmp ".planning"))
                  ;; Should still exist
                  (check-true (directory-exists? nonempty-dir)))
                (lambda () (cleanup-tmp tmp))))

(test-case "cleanup-empty-subdirs!: preserves archive dir"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  ;; Create empty archive subdir
                  (define archive-dir (build-path tmp ".planning" "archive"))
                  (make-directory archive-dir)
                  (cleanup-empty-subdirs! (build-path tmp ".planning"))
                  ;; archive should still exist even though empty
                  (check-true (directory-exists? archive-dir)))
                (lambda () (cleanup-tmp tmp))))

;; ============================================================
;; Tests: ensure-state-md! auto-creates STATE.md (#2164)
;; ============================================================

(test-case "ensure-state-md! creates STATE.md when missing"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (define state-path (build-path tmp ".planning" "STATE.md"))
                  (check-false (file-exists? state-path))
                  (ensure-state-md! tmp)
                  (check-true (file-exists? state-path))
                  (define content (file->string state-path))
                  (check-true (string-contains? content "# Project State")))
                (lambda () (cleanup-tmp tmp))))

;; ============================================================
;; Tests: sync-executor-to-plan! (v0.32.0)
;; ============================================================

(test-case "sync-executor-to-plan! updates PLAN.md from gsm-completed-waves"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create plan with all Inbox waves
     (write-plan!
      tmp
      #:status-lines
      "- [Inbox] W0: setup \u2192 waves/W0-setup.md\n- [Inbox] W1: implement \u2192 waves/W1-implement.md\n")
     ;; Mark waves complete in state machine (simulating /wave-done calls)
     (gsm-mark-wave-complete! 0)
     (gsm-mark-wave-complete! 1)
     ;; Sync state to PLAN.md
     (sync-executor-to-plan! tmp)
     ;; Verify PLAN.md now shows [DONE] for both waves
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[DONE] W0:"))
     (check-true (string-contains? plan-text "[DONE] W1:"))
     ;; Verify all-waves-complete? now returns #t
     (check-true (all-waves-complete? tmp)))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

(test-case "sync-executor-to-plan! normalizes mixed-case Done to DONE"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create plan with mixed-case [Done] markers (LLM-written)
     (write-plan!
      tmp
      #:status-lines
      "- [Done] W0: setup \u2192 waves/W0-setup.md\n- [Done] W1: implement \u2192 waves/W1-implement.md\n")
     ;; Sync should still normalize [Done] \u2192 [DONE]
     (sync-executor-to-plan! tmp)
     ;; Verify PLAN.md now shows [DONE]
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[DONE] W0:"))
     (check-true (string-contains? plan-text "[DONE] W1:"))
     ;; Verify all-waves-complete? now returns #t
     (check-true (all-waves-complete? tmp)))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

(test-case "archive-completed-plan! auto-syncs before check"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create plan with mixed-case [Done] + [Inbox] (simulating LLM execution)
     (write-plan!
      tmp
      #:status-lines
      "- [Done] W0: setup \u2192 waves/W0-setup.md\n- [Inbox] W1: implement \u2192 waves/W1-implement.md\n")
     (write-state! tmp)
     ;; Mark W1 as complete in state machine (simulating /wave-done for W1 only)
     (gsm-mark-wave-complete! 1)
     ;; Archive should succeed without --force:
     ;; - W0: [Done] normalized to [DONE] by normalize-plan-status-markers!
     ;; - W1: [Inbox] \u2192 [DONE] by gsm-completed-waves sync
     (define result (archive-completed-plan! tmp))
     (check-true (gsd-command-result-success result))
     ;; PLAN.md should be gone from .planning/
     (check-false (file-exists? (build-path tmp ".planning" "PLAN.md"))))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

(test-case "all-waves-complete? is case-insensitive"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Mixed-case Done should be treated as DONE
     (write-plan!
      tmp
      #:status-lines
      "- [Done] W0: setup \u2192 waves/W0-setup.md\n- [done] W1: implement \u2192 waves/W1-implement.md\n")
     (check-true (all-waves-complete? tmp)))
   (lambda () (cleanup-tmp tmp))))

(test-case "sync-executor-to-plan! no-ops when nothing to sync"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (write-plan! tmp)
                  ;; No completed waves, no mixed-case markers
                  (sync-executor-to-plan! tmp)
                  ;; PLAN.md unchanged
                  (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
                  (check-true (string-contains? plan-text "[DONE] W0:")))
                (lambda ()
                  (reset-gsm!)
                  (cleanup-tmp tmp))))

(test-case "v0.99.80 GC-5: no auto-complete in executing mode with partial completions"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     (write-plan!
      tmp
      #:status-lines
      "- [Done] W0: setup \u2192 waves/W0-setup.md\n- [Inbox] W1: impl \u2192 waves/W1-impl.md\n- [Inbox] W2: test \u2192 waves/W2-test.md\n")
     (gsm-transition! 'exploring)
     (gsm-transition! 'plan-written)
     (gsm-transition! 'executing)
     (gsm-mark-wave-complete! 0)
     ;; v0.99.80 W1 (GC-5): sync no longer auto-completes.
     (sync-executor-to-plan! tmp)
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[DONE] W0:"))
     (check-true (string-contains? plan-text "[Inbox] W1:")
                 "GC-5: executing mode does not auto-complete W1")
     (check-true (string-contains? plan-text "[Inbox] W2:")
                 "GC-5: executing mode does not auto-complete W2")
     (check-false (all-waves-complete? tmp) "GC-5: partial completion is not all-complete"))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))
(test-case "no auto-complete in idle mode"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     (write-plan!
      tmp
      #:status-lines
      "- [Done] W0: setup \u2192 waves/W0-setup.md\n- [Inbox] W1: impl \u2192 waves/W1-impl.md\n")
     ;; Stay in idle mode (not executing)
     ;; Only mark W0 complete
     (gsm-mark-wave-complete! 0)
     (sync-executor-to-plan! tmp)
     ;; W0 should be [DONE], but W1 should stay [Inbox]
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[DONE] W0:"))
     (check-true (string-contains? plan-text "[Inbox] W1:"))
     (check-false (all-waves-complete? tmp)))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

;; ── v0.45.20 regression: /done fails when LLM skipped /wave-done ──
;; BUG: auto-complete-inbox-waves! required at least one /wave-done completion.
;; v0.99.80 GC-5 regression: doc existence never implies completion.
(test-case "GC-5: no auto-complete when LLM skipped /wave-done but wave docs exist"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create plan with 3 waves, all [Inbox]
     (write-plan!
      tmp
      #:status-lines
      "- [Inbox] W0: setup \u2192 waves/W0-setup.md\n- [Inbox] W1: impl \u2192 waves/W1-impl.md\n- [Inbox] W2: test \u2192 waves/W2-test.md\n")
     ;; Create all wave doc files (evidence of execution)
     (write-wave! tmp 0 "setup" "Inbox" "Setup content")
     (write-wave! tmp 1 "impl" "Inbox" "Impl content")
     (write-wave! tmp 2 "test" "Inbox" "Test content")
     ;; Set a wave executor (simulates /go having been called)
     (gsm-set-wave-executor! 'fake-executor)
     ;; Do NOT call gsm-mark-wave-complete! for any wave
     ;; Do NOT transition to executing mode
     ;; v0.99.80 W1 (GC-5): doc existence + executor no longer auto-completes.
     (sync-executor-to-plan! tmp)
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[Inbox] W0:")
                 "GC-5: doc existence does not auto-complete W0")
     (check-true (string-contains? plan-text "[Inbox] W1:")
                 "GC-5: doc existence does not auto-complete W1")
     (check-true (string-contains? plan-text "[Inbox] W2:")
                 "GC-5: doc existence does not auto-complete W2")
     (check-false (all-waves-complete? tmp)))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

(test-case "no auto-complete when no wave docs and no completions"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create plan with 2 waves, all [Inbox]
     (write-plan!
      tmp
      #:status-lines
      "- [Inbox] W0: setup \u2192 waves/W0-setup.md\n- [Inbox] W1: impl \u2192 waves/W1-impl.md\n")
     ;; Do NOT create wave doc files
     ;; Do NOT set wave executor
     ;; Do NOT mark any wave complete
     (sync-executor-to-plan! tmp)
     ;; Waves should stay [Inbox]
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[Inbox] W0:"))
     (check-true (string-contains? plan-text "[Inbox] W1:"))
     (check-false (all-waves-complete? tmp)))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

;; N1: Guard 2 requires BOTH executor AND wave docs.
;; Wave docs alone (without executor) must NOT trigger auto-complete.
(test-case "no auto-complete when wave docs exist but no executor set (N1)"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create plan with 2 waves, all [Inbox]
     (write-plan! tmp
                  #:status-lines
                  "- [Inbox] W0: setup → waves/W0-setup.md
- [Inbox] W1: impl → waves/W1-impl.md
")
     ;; Create wave doc files (but do NOT set executor)
     (write-wave! tmp 0 "setup" "Inbox" "Setup content")
     (write-wave! tmp 1 "impl" "Inbox" "Impl content")
     ;; Do NOT set wave executor
     ;; Do NOT mark any wave complete
     (sync-executor-to-plan! tmp)
     ;; Waves should stay [Inbox] — docs alone are not enough
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[Inbox] W0:"))
     (check-true (string-contains? plan-text "[Inbox] W1:"))
     (check-false (all-waves-complete? tmp)))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

;; L2: Auto-complete requires ALL wave docs, not just some.
;; 3 waves with only 2 docs should NOT trigger auto-complete.
(test-case "no auto-complete when partial wave docs exist (L2)"
  (reset-gsm!)
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create plan with 3 waves, all [Inbox]
     (write-plan!
      tmp
      #:status-lines
      "- [Inbox] W0: setup → waves/W0-setup.md
- [Inbox] W1: impl → waves/W1-impl.md
- [Inbox] W2: test → waves/W2-test.md
")
     ;; Create only 2 of 3 wave doc files
     (write-wave! tmp 0 "setup" "Inbox" "Setup content")
     (write-wave! tmp 1 "impl" "Inbox" "Impl content")
     ;; W2 doc intentionally missing
     ;; Set a wave executor (simulates /go having been called)
     (gsm-set-wave-executor! 'fake-executor)
     (sync-executor-to-plan! tmp)
     ;; Waves should stay [Inbox] — partial docs are not enough
     (define plan-text (file->string (build-path tmp ".planning" "PLAN.md")))
     (check-true (string-contains? plan-text "[Inbox] W0:"))
     (check-true (string-contains? plan-text "[Inbox] W1:"))
     (check-true (string-contains? plan-text "[Inbox] W2:"))
     (check-false (all-waves-complete? tmp)))
   (lambda ()
     (reset-gsm!)
     (cleanup-tmp tmp))))

(test-case "ensure-state-md! does not overwrite existing STATE.md"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (define state-path (build-path tmp ".planning" "STATE.md"))
                  (call-with-output-file state-path (lambda (out) (display "existing content" out)))
                  (ensure-state-md! tmp)
                  (define content (file->string state-path))
                  (check-equal? content "existing content"))
                (lambda () (cleanup-tmp tmp))))

(test-case "F-6: reset-state-md-from-plan! regenerates STATE.md from PLAN.md"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Write a PLAN.md with 3 waves
     (call-with-output-file
      (build-path tmp ".planning" "PLAN.md")
      (lambda (out)
        (display
         "# Plan: New Campaign\n\n## Waves\n\
- [Inbox] W0: Alpha → waves/W0-alpha.md\n\
- [Inbox] W1: Beta → waves/W1-beta.md\n\
- [Inbox] W2: Gamma → waves/W2-gamma.md\n"
         out))
      #:exists 'truncate)
     ;; Write stale STATE.md (old campaign with different waves)
     (call-with-output-file (build-path tmp ".planning" "STATE.md")
                            (lambda (out) (display "# Old State\n| W0 | OldZero | DONE |\n" out))
                            #:exists 'truncate)
     ;; Reset STATE.md from PLAN.md
     (reset-state-md-from-plan! tmp)
     ;; Verify STATE.md now has the new wave list
     (define content (file->string (build-path tmp ".planning" "STATE.md")))
     (check-true (string-contains? content "Alpha") "STATE.md has wave 0 title")
     (check-true (string-contains? content "Beta") "STATE.md has wave 1 title")
     (check-true (string-contains? content "Gamma") "STATE.md has wave 2 title")
     (check-true (string-contains? content "PENDING") "STATE.md has PENDING status")
     (check-false (string-contains? content "OldZero") "STATE.md no longer has old campaign waves")
     (check-false (string-contains? content "DONE") "STATE.md no longer has old statuses"))
   (lambda () (cleanup-tmp tmp))))

(test-case "F-6: reset-state-md-from-plan! works when STATE.md does not exist"
  (define tmp (make-tmp-planning-dir))
  (dynamic-wind void
                (lambda ()
                  (call-with-output-file
                   (build-path tmp ".planning" "PLAN.md")
                   (lambda (out)
                     (display "# Plan: Fresh\n\n## Waves\n\
- [Inbox] W0: Only → waves/W0-only.md\n"
                              out))
                   #:exists 'truncate)
                  ;; No STATE.md exists yet
                  (reset-state-md-from-plan! tmp)
                  (define content (file->string (build-path tmp ".planning" "STATE.md")))
                  (check-true (string-contains? content "Only")))
                (lambda () (cleanup-tmp tmp))))
