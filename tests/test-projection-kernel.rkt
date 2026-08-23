#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; BOUNDARY: integration

;; tests/test-projection-kernel.rkt — Pure GSD projection kernel tests
;;
;; v0.99.89 W2 "Plan/State Projection Kernel": prove that PLAN.md / wave-doc /
;; STATE.md projections are computed purely (projection-kernel.rkt) and
;; applied atomically (projection-effects.rkt), byte-identical to the legacy
;; mark-wave-status!/update-state-table! writers, and that the crash-repair
;; reconciliation restores stale projections from the durable campaign record
;; (golden-trace oracle finding #2).

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/port
         racket/list
         "helpers/arch-utils.rkt"
         "../extensions/gsd/projection-kernel.rkt"
         "../extensions/gsd/projection-effects.rkt"
         (only-in "../extensions/gsd/wave-docs.rkt" update-plan-index-text mark-wave-status!)
         (only-in "../extensions/gsd/wave-completion.rkt" update-state-table!))

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (seed-fixture dir)
  (define planning (build-path dir ".planning"))
  (make-directory* (build-path planning "waves"))
  (call-with-output-file (build-path planning "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Test Campaign\n\n## Waves\n\n" out)
                           (displayln "- [Inbox] W0: Alpha → waves/W0-alpha.md" out)
                           (displayln "- [Inbox] W1: Beta → waves/W1-beta.md" out))
                         #:exists 'truncate)
  (call-with-output-file
   (build-path planning "STATE.md")
   (lambda (out)
     (display "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n" out)
     (displayln "| W0 | Alpha | PENDING |" out)
     (displayln "| W1 | Beta | PENDING |" out))
   #:exists 'truncate)
  (call-with-output-file (build-path planning "waves" "W0-alpha.md")
                         (lambda (out)
                           (display "# Wave 0\nStatus: Inbox\n\nDeterministic alpha body.\n" out))
                         #:exists 'truncate)
  (call-with-output-file (build-path planning "waves" "W1-beta.md")
                         (lambda (out)
                           (display "# Wave 1\nStatus: Inbox\n\nDeterministic beta body.\n" out))
                         #:exists 'truncate)
  dir)

(define (make-fixture)
  (seed-fixture (make-temporary-file "gsd-proj-~a" 'directory)))

(define (read-text path)
  (call-with-input-file path port->string))

(define (slug-of idx)
  (case idx
    [(0) "alpha"]
    [(1) "beta"]
    [else #f]))

(define (slug-map)
  (hash 0 "alpha" 1 "beta"))

(define fixture-plan
  "# Plan: Test Campaign\n\n## Waves\n\n- [Inbox] W0: Alpha → waves/W0-alpha.md\n- [Inbox] W1: Beta → waves/W1-beta.md\n")

(define fixture-state
  "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | PENDING |\n| W1 | Beta | PENDING |")

(define (strip-final-newline s)
  (if (and (> (string-length s) 0) (char=? #\newline (string-ref s (sub1 (string-length s)))))
      (substring s 0 (sub1 (string-length s)))
      s))

(define fixture-doc-0 "# Wave 0\nStatus: Inbox\n\nDeterministic alpha body.\n")
(define fixture-doc-1 "# Wave 1\nStatus: Inbox\n\nDeterministic beta body.\n")

;; ============================================================
;; Pure transform tables
;; ============================================================

(test-case "plan-index: each status marker is applied to the wave line"
  (define header "# Plan: Test Campaign\n\n## Waves\n\n")
  (define expected
    `(("DONE" . ,(string-append
                  header
                  "- [DONE] W0: Alpha → waves/W0-alpha.md\n- [Inbox] W1: Beta → waves/W1-beta.md"))
      ("FAILED" .
                ,(string-append
                  header
                  "- [FAILED] W0: Alpha → waves/W0-alpha.md\n- [Inbox] W1: Beta → waves/W1-beta.md"))
      ("DEFERRED"
       . ,(string-append
           header
           "- [DEFERRED] W0: Alpha → waves/W0-alpha.md\n- [Inbox] W1: Beta → waves/W1-beta.md"))
      ("In-Progress"
       . ,(string-append
           header
           "- [In-Progress] W0: Alpha → waves/W0-alpha.md\n- [Inbox] W1: Beta → waves/W1-beta.md"))))
  (for ([pair expected])
    (check-equal? (project-plan-index-update fixture-plan 0 (car pair))
                  (cdr pair)
                  (format "status ~a" (car pair)))))

(test-case "plan-index: unknown wave index leaves text unchanged"
  ;; The transform normalizes the trailing newline (legacy split+join), so
  ;; compare against the input minus its trailing newline.
  (check-equal? (project-plan-index-update fixture-plan 9 "DONE") (strip-final-newline fixture-plan)))

(test-case "plan-index: matches legacy update-plan-index-text byte-for-byte"
  (for ([status '("DONE" "FAILED" "DEFERRED" "In-Progress" "Inbox")])
    (check-equal? (project-plan-index-update fixture-plan 1 status)
                  (update-plan-index-text fixture-plan 1 status)
                  (format "status ~a" status))))

(test-case "wave-doc: header is rebuilt, body preserved"
  (define projected (project-wave-doc-update fixture-doc-0 0 "DONE"))
  (check-equal? projected "# Wave 0\nStatus: DONE\n\nDeterministic alpha body.\n"))

(test-case "wave-doc: idempotent when header already matches"
  (define once (project-wave-doc-update fixture-doc-1 1 "FAILED"))
  (check-equal? (project-wave-doc-update once 1 "FAILED") once))

(test-case "state-row: PENDING → DONE/FAILED/DEFERRED"
  (check-equal?
   (project-state-row-update fixture-state 0 "DONE")
   "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | DONE |\n| W1 | Beta | PENDING |")
  (check-equal?
   (project-state-row-update fixture-state 1 "FAILED")
   "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | PENDING |\n| W1 | Beta | FAILED |")
  (check-equal?
   (project-state-row-update fixture-state 1 "DEFERRED")
   "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | PENDING |\n| W1 | Beta | DEFERRED |"))

(test-case "state-row: unknown wave index leaves text unchanged"
  (check-equal? (project-state-row-update fixture-state 7 "DONE")
                (strip-final-newline fixture-state)))

(test-case "state-row: idempotent when row already matches"
  (define once (project-state-row-update fixture-state 0 "DONE"))
  (check-equal? (project-state-row-update once 0 "DONE") once))

;; ============================================================
;; Status mapping
;; ============================================================

(test-case "status mapping: durable symbols → display strings"
  (check-equal? (wave-status->projection-string 'done) "DONE")
  (check-equal? (wave-status->projection-string 'failed) "FAILED")
  (check-equal? (wave-status->projection-string 'deferred) "DEFERRED")
  (check-equal? (wave-status->projection-string 'pending) "Inbox")
  (check-equal? (wave-status->projection-string 'in-progress) "Inbox")
  (check-equal? (wave-status->projection-string 'verifying) "Inbox")
  (check-equal? (wave-status->state-string 'done) "DONE")
  (check-equal? (wave-status->state-string 'failed) "FAILED")
  (check-equal? (wave-status->state-string 'deferred) "DEFERRED")
  (check-equal? (wave-status->state-string 'pending) "PENDING"))

;; ============================================================
;; Projection sets
;; ============================================================

(test-case "project-wave-status-set: complete plan-index + wave-doc + state-table"
  (define set (project-wave-status-set fixture-plan fixture-doc-0 fixture-state 0 "DONE"))
  (check-true (projection-set? set))
  (check-equal? (length set) 3)
  (check-equal? (map projection-entry-kind set) '(plan-index wave-doc state-table))
  (check-equal? (projection-entry-wave-idx (second set)) 0)
  (check-equal?
   (projection-entry-content (first set))
   "# Plan: Test Campaign\n\n## Waves\n\n- [DONE] W0: Alpha → waves/W0-alpha.md\n- [Inbox] W1: Beta → waves/W1-beta.md")
  (check-equal?
   (projection-entry-content (third set))
   "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | DONE |\n| W1 | Beta | PENDING |"))

(test-case "project-wave-status-set: missing doc omits wave-doc entry"
  (define set (project-wave-status-set fixture-plan #f fixture-state 0 "DONE"))
  (check-equal? (map projection-entry-kind set) '(plan-index state-table)))

(test-case "project-wave-status-set: missing state omits state-table entry"
  (define set (project-wave-status-set fixture-plan fixture-doc-0 #f 0 "DONE"))
  (check-equal? (map projection-entry-kind set) '(plan-index wave-doc)))

;; ============================================================
;; Reconciliation
;; ============================================================

(test-case "reconciliation: stale projections are restored to durable statuses"
  ;; Durable truth: W0 done, W1 failed. Fixture shows both pending (crash).
  (define set
    (project-reconciliation-set '((0 . done) (1 . failed))
                                fixture-plan
                                (hash 0 fixture-doc-0 1 fixture-doc-1)
                                fixture-state))
  (check-true (projection-set? set))
  (define plan-entry (first (filter (lambda (e) (eq? (projection-entry-kind e) 'plan-index)) set)))
  (check-equal?
   (projection-entry-content plan-entry)
   "# Plan: Test Campaign\n\n## Waves\n\n- [DONE] W0: Alpha → waves/W0-alpha.md\n- [FAILED] W1: Beta → waves/W1-beta.md")
  (define state-entry (first (filter (lambda (e) (eq? (projection-entry-kind e) 'state-table)) set)))
  (check-equal?
   (projection-entry-content state-entry)
   "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | DONE |\n| W1 | Beta | FAILED |")
  (define doc0
    (first (filter (lambda (e)
                     (and (eq? (projection-entry-kind e) 'wave-doc)
                          (= (projection-entry-wave-idx e) 0)))
                   set)))
  (check-equal? (projection-entry-content doc0)
                "# Wave 0\nStatus: DONE\n\nDeterministic alpha body.\n"))

(test-case "reconciliation: in-sync fixture projects to itself (idempotent)"
  (define in-sync
    (project-reconciliation-set
     '((0 . done) (1 . failed))
     (project-plan-index-update (project-plan-index-update fixture-plan 0 "DONE") 1 "FAILED")
     (hash 0
           (project-wave-doc-update fixture-doc-0 0 "DONE")
           1
           (project-wave-doc-update fixture-doc-1 1 "FAILED"))
     (project-state-row-update (project-state-row-update fixture-state 0 "DONE") 1 "FAILED")))
  ;; The plan entry still gets recomputed; its content must equal the input.
  (define plan-entry
    (first (filter (lambda (e) (eq? (projection-entry-kind e) 'plan-index)) in-sync)))
  (define in-sync-plan
    (project-plan-index-update (project-plan-index-update fixture-plan 0 "DONE") 1 "FAILED"))
  (check-equal? (projection-entry-content plan-entry) in-sync-plan))

(test-case "reconciliation: missing wave doc is skipped, not fabricated"
  (define set
    (project-reconciliation-set '((0 . done) (1 . failed))
                                fixture-plan
                                (hash 0 fixture-doc-0)
                                fixture-state))
  (check-false (for/or ([e set])
                 (and (eq? (projection-entry-kind e) 'wave-doc)
                      (= (projection-entry-wave-idx e) 1)))))

;; ============================================================
;; Shell: atomic application
;; ============================================================

(test-case "apply-wave-status-projections!: writes plan + doc + state"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define written (apply-wave-status-projections! dir 0 "DONE" slug-of))
     (check-equal? (length written) 3)
     (check-equal?
      (read-text (build-path dir ".planning" "PLAN.md"))
      "# Plan: Test Campaign\n\n## Waves\n\n- [DONE] W0: Alpha → waves/W0-alpha.md\n- [Inbox] W1: Beta → waves/W1-beta.md")
     (check-equal?
      (read-text (build-path dir ".planning" "STATE.md"))
      "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | DONE |\n| W1 | Beta | PENDING |")
     (check-equal? (read-text (build-path dir ".planning" "waves" "W0-alpha.md"))
                   "# Wave 0\nStatus: DONE\n\nDeterministic alpha body.\n"))
   (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "apply-wave-status-projections!: idempotent second call writes nothing"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (apply-wave-status-projections! dir 0 "DONE" slug-of)
                  (check-equal? (apply-wave-status-projections! dir 0 "DONE" slug-of) '()))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "shell: byte-identical to legacy mark-wave-status! + update-state-table!"
  ;; Two identical fixtures: one driven by the legacy writers, one by the
  ;; kernel + shell. The three projection files must be byte-identical.
  (define legacy-dir (make-fixture))
  (define shell-dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (mark-wave-status! legacy-dir 0 "DONE")
                  (mark-wave-status! legacy-dir 1 "FAILED")
                  (update-state-table! legacy-dir 0 "DONE")
                  (update-state-table! legacy-dir 1 "FAILED")
                  (apply-wave-status-projections! shell-dir 0 "DONE" slug-of)
                  (apply-wave-status-projections! shell-dir 1 "FAILED" slug-of)
                  (for ([f '("PLAN.md" "STATE.md")])
                    (check-equal? (read-text (build-path legacy-dir ".planning" f))
                                  (read-text (build-path shell-dir ".planning" f))
                                  f))
                  (for ([f '("W0-alpha.md" "W1-beta.md")])
                    (check-equal? (read-text (build-path legacy-dir ".planning" "waves" f))
                                  (read-text (build-path shell-dir ".planning" "waves" f))
                                  f)))
                (lambda ()
                  (delete-directory/files legacy-dir #:must-exist? #f)
                  (delete-directory/files shell-dir #:must-exist? #f))))

(test-case "reconcile-projections-from-waves!: repairs crash-stale projections"
  ;; Simulate oracle finding #2: durable W0 done + W1 failed committed, but
  ;; the projections were never applied (crash). Reconcile must repair.
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define repaired (reconcile-projections-from-waves! dir '((0 . done) (1 . failed)) (slug-map)))
     (check-equal? (length repaired) 4)
     (check-equal?
      (read-text (build-path dir ".planning" "PLAN.md"))
      "# Plan: Test Campaign\n\n## Waves\n\n- [DONE] W0: Alpha → waves/W0-alpha.md\n- [FAILED] W1: Beta → waves/W1-beta.md")
     (check-equal?
      (read-text (build-path dir ".planning" "STATE.md"))
      "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Alpha | DONE |\n| W1 | Beta | FAILED |")
     (check-equal? (read-text (build-path dir ".planning" "waves" "W0-alpha.md"))
                   "# Wave 0\nStatus: DONE\n\nDeterministic alpha body.\n")
     ;; Second reconcile: already in sync → no writes.
     (check-equal? (reconcile-projections-from-waves! dir '((0 . done) (1 . failed)) (slug-map)) '()))
   (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "reconcile: missing PLAN.md is a no-op"
  (define dir (make-temporary-file "gsd-proj-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (check-equal? (reconcile-projections-from-waves! dir '((0 . done)) (slug-map)) '()))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "reconcile: missing STATE.md still repairs PLAN.md and docs (W2 MINOR #1)"
  ;; A plan with PLAN.md but no STATE.md (migrate-campaign! supports this)
  ;; must not crash the whole reconcile: the state-table entry is skipped and
  ;; the plan-index + wave-doc repairs still happen.
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (delete-file (build-path dir ".planning" "STATE.md"))
     (define repaired (reconcile-projections-from-waves! dir '((0 . done) (1 . failed)) (slug-map)))
     (check-equal? (length repaired) 3)
     (check-equal?
      (read-text (build-path dir ".planning" "PLAN.md"))
      "# Plan: Test Campaign\n\n## Waves\n\n- [DONE] W0: Alpha → waves/W0-alpha.md\n- [FAILED] W1: Beta → waves/W1-beta.md")
     (check-equal? (read-text (build-path dir ".planning" "waves" "W0-alpha.md"))
                   "# Wave 0\nStatus: DONE\n\nDeterministic alpha body.\n")
     (check-false (file-exists? (build-path dir ".planning" "STATE.md"))))
   (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "reconcile: interrupted durable status maps back to pending rows"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (reconcile-projections-from-waves! dir '((0 . interrupted) (1 . pending)) (slug-map))
     ;; interrupted and pending both map to the initial Inbox/PENDING rows.
     ;; (Row transforms follow the legacy string-split+join writers, which
     ;; normalize the trailing newline — semantically unchanged.)
     (define plan (read-text (build-path dir ".planning" "PLAN.md")))
     (define state (read-text (build-path dir ".planning" "STATE.md")))
     (check-true (string-contains? plan "- [Inbox] W0: Alpha"))
     (check-true (string-contains? plan "- [Inbox] W1: Beta"))
     (check-true (string-contains? state "| W0 | Alpha | PENDING |"))
     (check-true (string-contains? state "| W1 | Beta | PENDING |")))
   (lambda () (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; Kernel purity fitness
;; ============================================================

(define allowed-kernel-imports '("racket/base" "racket/string"))

(define (spec-module-path spec)
  (cond
    [(symbol? spec) (symbol->string spec)]
    [(string? spec) spec]
    [(pair? spec)
     (case (car spec)
       [(only-in prefix-in rename-in except-in)
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (cadr spec)
            #f)]
       [else #f])]
    [else #f]))

(test-case "kernel purity: projection-kernel.rkt imports only base + string"
  (define kernel-path (build-path q-dir "extensions" "gsd" "projection-kernel.rkt"))
  (check-true (file-exists? kernel-path) "kernel file exists on disk")
  (define reqs (extract-requires (path->string kernel-path)))
  (define imports
    (for/list ([spec (in-list reqs)]
               #:when (spec-module-path spec))
      (spec-module-path spec)))
  (check-true (pair? imports) "kernel has require forms")
  (define violations
    (for/list ([i (in-list imports)]
               #:unless (member i allowed-kernel-imports))
      i))
  (check-equal? violations '() (format "kernel imports forbidden modules: ~a" violations)))
