#lang racket

;; @speed fast
;; @suite testing
;; @boundary integration
;;
;; tests/test-qt-quick-working-tree.rkt — W2 (review 7002474250): the SINGLE
;; base ref form of `./qt quick --changed BASE` is the working-tree selection
;; path, distinct from `--changed BASE..HEAD` (a commit range). Pins, at the
;; git-fixture level:
;;   - the working-tree diff sees staged, unstaged, untracked, deleted and
;;     renamed files;
;;   - direct @covers attribution, transitive graph impacts, config/global
;;     escalation, unmapped escalation and the doc-only no-op all select the
;;     same way for a working-tree diff as for a committed range (one
;;     selection machinery, two diff inputs);
;;   - the report label states WHICH input produced the selection.
;; One plan-only CLI probe pins the real-checkout working-tree path
;; end-to-end (`./qt quick --changed HEAD --explain`); it executes nothing.

(require rackunit
         json
         racket/runtime-path
         "../scripts/testing/full-plan.rkt")

(define-runtime-path qt "../qt")
(define-runtime-path repo-root "..")

;; ---------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------

(define git-exe (find-executable-path "git"))

(define (git! dir . args)
  (parameterize ([current-directory dir]
                 [current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (eq? #t (apply system* git-exe args))))

(define (git-commit! dir message)
  (git! dir
        "-c"
        "user.email=fixture@example.invalid"
        "-c"
        "user.name=fixture"
        "-c"
        "commit.gpgsign=false"
        "commit"
        "-q"
        "-m"
        message))

(define (write-file path text)
  (make-parent-directory* path)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; A committed two-module fixture: util/a.rkt requires util/b.rkt.
(define (make-git-fixture)
  (define dir (make-temporary-file "qt-quick-wt-~a" 'directory))
  (write-file (build-path dir "info.rkt") "#lang info\n(define version \"0.0.0\")\n")
  (write-file (build-path dir "util" "a.rkt")
              "#lang racket/base\n(require \"b.rkt\")\n(provide a)\n(define (a) (b))\n")
  (write-file (build-path dir "util" "b.rkt") "#lang racket/base\n(provide b)\n(define (b) 2)\n")
  (write-file (build-path dir "util" "solo.rkt") "#lang racket/base\n(provide s)\n(define (s) 3)\n")
  (git! dir "init" "-q")
  (git! dir "add" "-A")
  (git-commit! dir "fixture baseline")
  dir)

;; Subprocess helper: drain both pipes concurrently, bounded (same shape as
;; tests/test-qt-quick.rkt).
(define (invoke-command timeout cwd executable . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-directory cwd]
                                 [subprocess-group-enabled #t])
                    (define-values (child out in err) (apply subprocess #f #f #f executable args))
                    (set! proc child)
                    (close-output-port in)
                    (define stdout #f)
                    (define stderr #f)
                    (define out-thread (thread (lambda () (set! stdout (port->string out)))))
                    (define err-thread (thread (lambda () (set! stderr (port->string err)))))
                    (define done
                      (thread (lambda ()
                                (subprocess-wait child)
                                (thread-wait out-thread)
                                (thread-wait err-thread))))
                    (unless (sync/timeout timeout done)
                      (error 'invoke-command "subprocess timed out after ~a seconds" timeout))
                    (values (subprocess-status child) stdout stderr)))
                (lambda ()
                  (when proc
                    (when (eq? (subprocess-status proc) 'running)
                      (subprocess-kill proc #t))
                    (subprocess-wait proc))
                  (custodian-shutdown-all cust))))

(module+ test
  (test-case "the working-tree diff sees unstaged, staged, untracked, deleted and renamed files"
    (if git-exe
        (let ([dir (make-git-fixture)])
          (dynamic-wind
           void
           (lambda ()
             ;; Unstaged modification.
             (write-file (build-path dir "util" "b.rkt")
                         "#lang racket/base\n(provide b)\n(define (b) 22)\n")
             (define-values (rels-1 label-1 _out-1) (working-tree-changed "HEAD" dir))
             (check-equal? label-1 "HEAD" "a single base ref labels the working-tree diff")
             (check-not-false (member "util/b.rkt" rels-1) "unstaged modification is seen")
             ;; Staged modification (same diff command covers index + worktree).
             (git! dir "add" "util/b.rkt")
             (define-values (rels-2 _label-2 _out-2) (working-tree-changed "HEAD" dir))
             (check-not-false (member "util/b.rkt" rels-2) "staged modification is seen")
             ;; Staged NEW file.
             (write-file (build-path dir "util" "new.rkt") "#lang racket/base\n")
             (git! dir "add" "util/new.rkt")
             (define-values (rels-3 _l3 _o3) (working-tree-changed "HEAD" dir))
             (check-not-false (member "util/new.rkt" rels-3) "staged new file is seen")
             ;; Untracked file (never committed, never staged).
             (write-file (build-path dir "util" "scratch.txt") "untracked\n")
             (define-values (rels-4 _l4 _o4) (working-tree-changed "HEAD" dir))
             (check-not-false (member "util/scratch.txt" rels-4) "untracked file is seen")
             ;; Deleted file.
             (delete-file (build-path dir "util" "solo.rkt"))
             (define-values (rels-5 _l5 _o5) (working-tree-changed "HEAD" dir))
             (check-not-false (member "util/solo.rkt" rels-5) "deleted file is seen")
             ;; Renamed file (--no-renames: the old path is deleted, the new is added).
             (git! dir "mv" "util/a.rkt" "util/a2.rkt")
             (define-values (rels-6 _l6 _o6) (working-tree-changed "HEAD" dir))
             (check-not-false (member "util/a.rkt" rels-6) "renamed file: the old path is seen")
             (check-not-false (member "util/a2.rkt" rels-6) "renamed file: the new path is seen"))
           (lambda () (delete-directory/files dir #:must-exist? #f))))
        (displayln "SKIP working-tree diff kinds: git unavailable")))

  (test-case "a commit range on the same fixture reports its range label"
    (if git-exe
        (let ([dir (make-git-fixture)])
          (dynamic-wind void
                        (lambda ()
                          (write-file (build-path dir "util" "b.rkt")
                                      "#lang racket/base\n(provide b)\n(define (b) 22)\n")
                          (git! dir "add" "-A")
                          (git-commit! dir "second")
                          (define-values (rels label _out) (commits-changed "HEAD~1" "HEAD" dir))
                          (check-equal? label "HEAD~1..HEAD")
                          (check-equal? rels '("util/b.rkt")))
                        (lambda () (delete-directory/files dir #:must-exist? #f))))
        (displayln "SKIP commits-changed label: git unavailable")))

  (test-case "the selection is identical machinery for a working-tree diff and a commit range"
    ;; Direct @covers attribution, transitive graph impact, config escalation,
    ;; unmapped escalation and the doc-only no-op behave the same for both
    ;; diff inputs; only the report label differs.
    (define dir (make-temporary-file "qt-quick-wt-map-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (write-file
        (build-path dir "tests" ".coverage-manifest.json")
        (jsexpr->string
         (hash
          'schema
          "coverage-manifest/v1"
          'entries
          (list
           (hash 'test "tests/owner-a.rkt" 'covers (list "util/a.rkt") 'source "manual-review")
           (hash 'test "tests/owner-b.rkt" 'covers (list "util/b.rkt") 'source "manual-review")))))
       (write-file (build-path dir "util" "a.rkt")
                   "#lang racket/base\n(require \"b.rkt\")\n(provide a)\n(define (a) (b))\n")
       (write-file (build-path dir "util" "b.rkt") "#lang racket/base\n(provide b)\n(define (b) 2)\n")
       (write-file (build-path dir "util" "zzz.rkt")
                   "#lang racket/base\n(provide z)\n(define (z) 9)\n")
       (define owners '("tests/owner-a.rkt" "tests/owner-b.rkt" "tests/owner-c.rkt"))
       ;; Direct cover: util/a.rkt -> owner-a (owner-c is the unattributed rider).
       (define r-direct (quick-changed-owners dir owners '("util/a.rkt") #:base "HEAD"))
       (check-equal? (change-report-included r-direct) '("tests/owner-a.rkt" "tests/owner-c.rkt"))
       (check-equal? (change-report-excluded r-direct) '("tests/owner-b.rkt"))
       ;; Transitive impact: util/b.rkt reaches owner-a through util/a.rkt's require.
       (define r-transitive (quick-changed-owners dir owners '("util/b.rkt") #:base "HEAD"))
       (check-not-false (member "tests/owner-a.rkt" (change-report-included r-transitive))
                        "transitive dependent's cover selects the owner")
       (check-not-false (member "tests/owner-b.rkt" (change-report-included r-transitive)))
       ;; A working-tree diff and the same change as a committed range select
       ;; the SAME owners (one selection machinery, two diff inputs).
       (define r-range (quick-changed-owners dir owners '("util/a.rkt") #:base "HEAD~1..HEAD"))
       (check-equal? (change-report-included r-range) (change-report-included r-direct))
       ;; Config/global change escalates to the whole audited lane.
       (define r-config (quick-changed-owners dir owners '("info.rkt") #:base "HEAD"))
       (check-equal? (change-report-included r-config) (sort owners string<?))
       (check-true (for/or ([l (in-list (change-report-lines r-config))])
                     (string-contains? l "config-change"))
                   "the config escalation is named")
       ;; Unmapped production change escalates by name.
       (define r-unmapped (quick-changed-owners dir owners '("util/zzz.rkt") #:base "HEAD"))
       (check-equal? (change-report-included r-unmapped) (sort owners string<?))
       (check-true (for/or ([l (in-list (change-report-lines r-unmapped))])
                     (string-contains? l "no @covers mapping reaches"))
                   "the unmapped escalation is named")
       ;; A doc-only change selects only the fail-open riders; no escalation.
       (define r-doc (quick-changed-owners dir owners '("docs/readme.md") #:base "HEAD"))
       (check-equal? (change-report-included r-doc) '("tests/owner-c.rkt"))
       (check-equal? (change-report-escalations r-doc) '() "no escalation for a doc-only change")
       ;; The report label states WHICH diff input produced the selection.
       (check-regexp-match #rx"working-tree diff against HEAD"
                           (string-join (change-report-lines r-direct) "\n"))
       (check-regexp-match #rx"commit range HEAD~1\\.\\.HEAD"
                           (string-join (change-report-lines r-range) "\n")))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "./qt quick --changed HEAD --explain selects the real working tree (plan only)"
    ;; Untracked probe file at the repository root: a 'doc-category change that
    ;; reaches no owner, so the selection is exactly the fail-open riders (and
    ;; whatever else the local working tree carries). The probe asserts the
    ;; working-tree label, the probe file itself, a non-empty selection and
    ;; that nothing executed. Cleaned up in the same test.
    (if git-exe
        (let ([probe (build-path repo-root "qt-w2-working-tree-probe.txt")])
          (dynamic-wind
           void
           (lambda ()
             (write-file probe
                         "working-tree probe (deleted by tests/test-qt-quick-working-tree.rkt)\n")
             (define-values (status out err)
               (invoke-command 120
                               repo-root
                               (find-executable-path "racket")
                               qt
                               "quick"
                               "--changed"
                               "HEAD"
                               "--explain"))
             (check-equal? status 0 (string-append out err))
             (check-regexp-match #rx"changed: working-tree diff against HEAD" out)
             (check-true (string-contains? out "qt-w2-working-tree-probe.txt")
                         "the untracked probe file is part of the selection input")
             (check-regexp-match #rx"  included: tests/test-" out "the audited selection is printed")
             (check-false (regexp-match? #rx"quick passed:" out) "--explain never executes"))
           (lambda ()
             (when (file-exists? probe)
               (delete-file probe)))))
        (displayln "SKIP ./qt quick --changed HEAD: git unavailable")))

  (test-case "quick --changed with no base fails and names both accepted forms"
    (define-values (status _out err)
      (invoke-command 120 repo-root (find-executable-path "racket") qt "quick" "--changed"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"quick --changed requires BASE\\.\\.HEAD or a single base ref" err)))
