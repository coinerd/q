#!/usr/bin/env racket
#lang racket/base

;; scripts/release-closeout.rkt — One-command release close-out (BUG-0014).
;;
;; Performs the full close-out sequence for a release tag with a per-stage
;; report, replacing the manual multi-system verification described in
;; .planning/bugs/BUG-0014-manual-release-closeout.md:
;;
;;   1. NOTES      assemble release-note input from closed milestone issues
;;                 + bug-registry rows; lint the changelog entry
;;   2. READINESS  final run of scripts/release-preflight.rkt <tag> --readiness
;;   3. TAG        annotated tag + push (idempotent: existing annotated tag is
;;                 reused, never re-tagged)
;;   4. WORKFLOW   watch the release.yml run for the tag until it concludes
;;   5. ARCHIVE    archive shipped planning/registry artifacts
;;   6. REGISTRY   re-run check-registry.rkt so INDEX derived counts stay true
;;   7. MILESTONE  close the milestone (no-op if already closed)
;;
;; Usage:
;;   racket scripts/release-closeout.rkt v1.00.01 [--dry-run]
;;        [--skip-workflow-wait]   ; stage 4 reports only the latest run
;;
;; Dry mode prints every stage with its sources (issues, registry rows) and
;; performs NO writes: no tag, no push, no gh mutation, no file changes.
;;
;; Exit codes: 0 = all stages green (or dry-run plan printed)
;;             1 = a stage failed; the report says which and why

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/port
         racket/string
         json)

;; ---------------------------------------------------------------- runtime

(define dry-run? #f)
(define skip-workflow-wait? #f)

;; racket/cmdline stops recognizing flags after the first positional
;; argument, so `release-closeout.rkt v1.00.01 --dry-run` would silently
;; ignore --dry-run. Hoist our known flags in front of the positionals so
;; both orderings behave identically.
(define (normalize-argv argv)
  (define flags '("--dry-run" "--skip-workflow-wait"))
  (define (flag? s)
    (and (member s flags) #t))
  (define args (vector->list argv))
  (list->vector (append (filter flag? args) (filter (compose not flag?) args))))

(define tag
  (command-line
   #:program "release-closeout"
   #:argv (normalize-argv (current-command-line-arguments))
   #:once-each [("--dry-run") "print every stage, make no writes" (set! dry-run? #t)]
   [("--skip-workflow-wait") "do not block on the workflow run" (set! skip-workflow-wait? #t)]
   #:args (tag . _)
   tag))

(define version-regex #rx"^[0-9]+\\.[0-9]+\\.[0-9]+")

(unless (and (string? tag)
             (> (string-length tag) 1)
             (eqv? (string-ref tag 0) #\v)
             (regexp-match? version-regex (substring tag 1)))
  (raise-user-error 'release-closeout "invalid tag: ~a (expected e.g. v1.00.01)" tag))

(define version (substring tag 1)) ; strip leading v

;; ------------------------------------------------------------ subprocesses

;; Run a command, capture stdout/stderr, return (exit-code out err).
(define (run-capture exe . args)
  (with-handlers ([exn:fail? (lambda (e)
                               (values 127 "" (format "cannot run ~a: ~a" exe (exn-message e))))])
    (define-values (sp out in err)
      (apply subprocess #f #f #f (or (find-executable-path exe) exe) args))
    (close-output-port in)
    (define out-s (port->string out))
    (close-input-port out)
    (define err-s (port->string err))
    (close-input-port err)
    (subprocess-wait sp)
    (values (subprocess-status sp) out-s err-s)))

(define (exe-available? exe)
  (define-values (ec _o _e) (run-capture exe "--version"))
  (zero? ec))

(define racket-exe
  (or (find-executable-path "racket")
      (find-executable-path "racket.exe")
      (raise-user-error 'release-closeout "racket executable not found")))

;; Call gh, parse its stdout as a single JSON document.
(define (gh-json . args)
  (define-values (ec out err) (apply run-capture "gh" args))
  (if (zero? ec)
      (with-handlers ([exn:fail? (lambda (_e) (raise-user-error 'gh "bad JSON from gh: ~a" out))])
        (string->jsexpr (string-trim out)))
      (raise-user-error 'gh
                        "gh ~a failed (exit ~a): ~a"
                        (string-join args " ")
                        ec
                        (string-trim err))))

;; Call gh expecting plain scalar output (e.g. with -q '.field').
(define (gh-text . args)
  (define-values (ec out err) (apply run-capture "gh" args))
  (if (zero? ec)
      (string-trim out)
      (raise-user-error 'gh
                        "gh ~a failed (exit ~a): ~a"
                        (string-join args " ")
                        ec
                        (string-trim err))))

(define (repo-slug)
  (define-values (ec out _e)
    (run-capture "gh" "repo" "view" "--json" "nameWithOwner" "-q" ".nameWithOwner"))
  (if (zero? ec)
      (string-trim out)
      "?"))

;; ----------------------------------------------------------------- layout

;; This script lives in <repo>/scripts/.
(define-values (script-dir-path _script-file _dir2)
  (split-path (simplify-path (find-system-path 'run-file))))

(define repo-root (simplify-path (build-path script-dir-path 'up)))

;; Planning dir: prefer the one next to the repo (<repo-parent>/.planning,
;; where the durable bug registry lives), else <repo>/.planning.
(define planning-dir
  (simplify-path (cond
                   [(directory-exists? (build-path repo-root 'up ".planning"))
                    (build-path repo-root 'up ".planning")]
                   [else (build-path repo-root ".planning")])))

(define registry-dir (build-path planning-dir "bugs"))

;; Helper scripts may live next to this script or next to the planning dir.
(define (find-script name)
  (define candidates
    (list (build-path script-dir-path name)
          (build-path planning-dir 'up "scripts" name)
          (build-path repo-root "scripts" name)))
  (for/or ([c (in-list candidates)]
           #:when (file-exists? c))
    c))

;; ---------------------------------------------------------------- helpers

(define stage-failures '())

(define (fail! stage msg)
  (set! stage-failures (cons (cons stage msg) stage-failures))
  (printf "  FAIL: ~a\n" msg))

;; Run a helper Racket script; returns #t on success.
(define (run-racket-script stage path . args)
  (cond
    [(not path)
     (fail! stage (format "~a script not found" stage))
     #f]
    [else
     (printf "  $ racket ~a~a\n"
             (path->string path)
             (if (null? args)
                 ""
                 (format " ~a" (string-join args " "))))
     (cond
       [dry-run?
        (printf "  [dry-run] skipped\n")
        (void)]
       [else
        (define full-args (cons (path->string path) args))
        (define-values (ec out err) (apply run-capture racket-exe full-args))
        (printf "~a~a" out err)
        (cond
          [(zero? ec) #t]
          [else
           (fail! stage (format "exit ~a" ec))
           #f])])]))

;; ------------------------------------------------- stage 1: release notes

;; Find the milestone number for the version by title. Milestones are sorted
;; by due date, so a single page can miss the target (BUG-0014: "nine pages of
;; closed milestones"); walk pages of 100 until a title match or an empty page.
(define (find-milestone-number)
  (let loop ([page 1])
    (define ms
      (gh-json "api"
               (format "repos/{owner}/{repo}/milestones?state=all&per_page=100&page=~a"
                       page)))
    (cond
      [(not (list? ms)) #f]
      [(null? ms) #f]
      [else
       (define hits
         (filter (lambda (m) (equal? (hash-ref m 'title "") (string-append "v" version)))
                 ms))
       (or (and (pair? hits) (hash-ref (car hits) 'number #f))
           (loop (add1 page)))])))

;; Closed issues in the milestone: the note sources from the tracker.
(define (milestone-issues mn)
  (gh-json "issue"
           "list"
           "--milestone"
           (~a mn)
           "--state"
           "closed"
           "--limit"
           "200"
           "--json"
           "number,title,url"))

;; Registry rows marked fixed in this version: the note sources from
;; .planning/bugs/INDEX.md ("Fixed in" column mentions the version).
;; Row shape: | ID | Reported | Title | Component | Severity | Status | Fixed in | File |
(define index-row-rx #px"^\\| (BUG-\\d{4}) \\|[^|]*\\|([^|]*)\\|[^|]*\\|[^|]*\\|[^|]*\\|([^|]*)\\|")

(define (registry-rows-for-version)
  (define index (build-path registry-dir "INDEX.md"))
  (cond
    [(not (file-exists? index)) '()]
    [else
     (define matched
       (for/list ([line (in-list (file->lines index))]
                  #:when (regexp-match? index-row-rx line))
         (regexp-match index-row-rx line)))
     (for/list ([row (in-list matched)]
                #:when (string-contains? (list-ref row 3) version))
       (hasheq 'id
               (string-trim (list-ref row 1))
               'title
               (string-trim (list-ref row 2))
               'fixed-in
               (string-trim (list-ref row 3))))]))

(define (stage-notes mn)
  (printf "== [1/7] NOTES - assemble release notes (issues + registry rows)\n")
  (define issues
    (if (and mn gh-available?)
        (with-handlers ([exn:fail? (lambda (e)
                                     (printf "  (issue lookup failed: ~a)\n" (exn-message e))
                                     '())])
          (milestone-issues mn))
        (begin
          (printf "  (issue sources unavailable: ~a)\n" (if mn "gh not available" "no milestone"))
          '())))
  (define rows (registry-rows-for-version))
  (printf "  sources: ~a closed milestone issue(s), ~a registry row(s) fixed in ~a\n"
          (length issues)
          (length rows)
          version)
  (for ([i (in-list issues)])
    (printf "    issue #~a ~a - ~a\n" (hash-ref i 'number) (hash-ref i 'title) (hash-ref i 'url #f)))
  (for ([r (in-list rows)])
    (printf "    registry ~a ~a (fixed in: ~a)\n"
            (hash-ref r 'id)
            (hash-ref r 'title)
            (hash-ref r 'fixed-in)))
  ;; CHANGELOG.md stays the reviewed source of notes; gen/lint check it.
  (run-racket-script "gen-release-notes" (find-script "gen-release-notes.rkt") version)
  (run-racket-script "lint-release-notes" (find-script "lint-release-notes.rkt") version))

;; ---------------------------------------------- stage 2: readiness gate

(define (stage-readiness)
  (printf "== [2/7] READINESS - final release-preflight gate\n")
  (run-racket-script "release-preflight" (find-script "release-preflight.rkt") tag "--readiness"))

;; --------------------------------------------------- stage 3: tag + push

(define (remote-tag-exists?)
  (define-values (ec out _e) (run-capture "git" "ls-remote" "--tags" "origin" tag))
  (and (zero? ec) (non-empty-string? (string-trim out))))

(define (stage-tag)
  (printf "== [3/7] TAG - annotated tag + push (idempotent)\n")
  (define remote-tag (remote-tag-exists?))
  (cond
    [remote-tag (printf "  tag ~a already on origin - reusing (no re-tag)\n" tag)]
    [else
     (printf "  $ git tag -a ~a -m \"Release ~a\"\n" tag tag)
     (cond
       [dry-run? (printf "  [dry-run] skipped\n")]
       [else
        (define-values (ec-t _o e-t)
          (run-capture "git" "tag" "-a" tag "-m" (format "Release ~a" tag)))
        (unless (zero? ec-t)
          (fail! "tag" (format "git tag failed: ~a" (string-trim e-t))))
        (printf "  $ git push origin ~a\n" tag)
        (define-values (ec-p _o2 e-p) (run-capture "git" "push" "origin" tag))
        (unless (zero? ec-p)
          (fail! "push" (format "git push failed: ~a" (string-trim e-p))))])])
  (cond
    [(or dry-run? remote-tag) (void)]
    [(remote-tag-exists?) (printf "  ok: origin has ~a\n" tag)]
    [else (fail! "tag" "tag not present on origin after stage")]))

;; -------------------------------------------------- stage 4: CI workflow

(define (workflow-run-for-tag)
  (gh-json "run"
           "list"
           "--workflow=release.yml"
           "--limit"
           "15"
           "--json"
           "databaseId,status,conclusion,headBranch,url"
           "--jq"
           (format "[.[] | select(.headBranch == \"~a\")]" tag)))

(define (stage-workflow)
  (printf "== [4/7] WORKFLOW - release.yml run for ~a\n" tag)
  (cond
    [(or dry-run? skip-workflow-wait?)
     (define runs
       (with-handlers ([exn:fail? (lambda (_e) '())])
         (workflow-run-for-tag)))
     (cond
       [(null? runs) (printf "  (no run found yet for ~a)\n" tag)]
       [else
        (for ([r (in-list runs)])
          (printf "    run ~a: ~a/~a - ~a\n"
                  (hash-ref r 'databaseId)
                  (hash-ref r 'status)
                  (hash-ref r 'conclusion #f)
                  (hash-ref r 'url)))])]
    [else
     (let poll ([n 0])
       (cond
         [(> n 120) (fail! "workflow" "timed out waiting for the tag run to conclude")]
         [else
          (define runs
            (with-handlers ([exn:fail? (lambda (_e) '())])
              (workflow-run-for-tag)))
          (define done
            (and (pair? runs)
                 (for/or ([r (in-list runs)])
                   (and (string=? (hash-ref r 'status) "completed") r))))
          (cond
            [done
             (printf "  run ~a completed: ~a - ~a\n"
                     (hash-ref done 'databaseId)
                     (hash-ref done 'conclusion)
                     (hash-ref done 'url))
             (unless (string=? (hash-ref done 'conclusion) "success")
               (fail! "workflow" (format "conclusion=~a" (hash-ref done 'conclusion))))]
            [else
             (printf "  ... waiting for run on ~a (~a)\n"
                     tag
                     (if (pair? runs)
                         (hash-ref (car runs) 'status)
                         "not started"))
             (sleep 20)
             (poll (add1 n))])]))]))

;; ---------------------------------------- stage 5: archive planning rows

(define (stage-archive)
  (printf "== [5/7] ARCHIVE - archive shipped planning/registry artifacts\n")
  (printf "  planning dir: ~a\n" planning-dir)
  (apply run-racket-script
         "archive-planning"
         (find-script "archive-planning.rkt")
         `("--planning-dir" ,(path->string planning-dir)
                            ,@(if dry-run?
                                  '("--dry-run")
                                  '()))))

;; ------------------------------------------ stage 6: registry consistency

(define (stage-registry)
  (printf "== [6/7] REGISTRY - recompute INDEX derived counts\n")
  (define p (find-script "check-registry.rkt"))
  (cond
    [(not p) (fail! "check-registry" "script not found")]
    [dry-run? (printf "  [dry-run] would run check-registry.rkt on ~a\n" registry-dir)]
    [else
     (printf "  $ racket ~a --registry ~a\n" (path->string p) (path->string registry-dir))
     (define-values (ec out err)
       (run-capture racket-exe (path->string p) "--registry" (path->string registry-dir)))
     (printf "~a~a" out err)
     (unless (zero? ec)
       (fail! "check-registry" (format "exit ~a" ec)))]))

;; --------------------------------------------- stage 7: close milestone

(define (stage-milestone-close mn)
  (printf "== [7/7] MILESTONE - close milestone ~a (v~a)\n" mn version)
  (define state (gh-text "api" (format "repos/{owner}/{repo}/milestones/~a" mn) "-q" ".state"))
  (cond
    [(string=? (string-trim state) "closed") (printf "  already closed - no-op\n")]
    [dry-run? (printf "  [dry-run] would PATCH state=closed\n")]
    [else
     (gh-json "api" (format "repos/{owner}/{repo}/milestones/~a" mn) "-X" "PATCH" "-f" "state=closed")
     (printf "  closed\n")]))

;; ------------------------------------------------------------------- main

(unless (exe-available? "gh")
  (if dry-run?
      (eprintf "warning: gh CLI not available - issue/milestone sources will be skipped\n")
      (raise-user-error 'release-closeout "gh CLI not available")))
(unless (exe-available? "git")
  (if dry-run?
      (eprintf "warning: git not available - tag checks will be skipped\n")
      (raise-user-error 'release-closeout "git not available")))

(define gh-available? (exe-available? "gh"))
(define git-available? (exe-available? "git"))

(define milestone-number
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (find-milestone-number)))

(when dry-run?
  (printf "DRY RUN - no writes will be made.\n"))
(printf "release-closeout: tag=~a version=~a repo=~a milestone=~a\n"
        tag
        version
        (repo-slug)
        (or milestone-number "NOT FOUND"))

(cond
  [(not milestone-number)
   (if dry-run?
       (printf "note: no milestone titled *~a* found; issue-based sources will be skipped\n\n"
               version)
       (fail! "milestone" (format "no milestone titled *~a* found" version)))]
  [else (void)])

(stage-notes milestone-number)
(stage-readiness)
(stage-tag)
(stage-workflow)
(stage-archive)
(stage-registry)
(when milestone-number
  (stage-milestone-close milestone-number))

;; ------------------------------------------------------------------ report

(printf "\n---- close-out report (~a) ----\n" tag)
(define known-stages
  '("gen-release-notes" "lint-release-notes"
                        "release-preflight"
                        "tag"
                        "push"
                        "workflow"
                        "archive-planning"
                        "check-registry"
                        "milestone"))
(for ([s (in-list known-stages)])
  (define failed (assoc s stage-failures))
  (printf "  ~a ~a~a\n"
          (if failed "FAIL" "ok")
          s
          (if failed
              (format " - ~a" (cdr failed))
              "")))

(cond
  [(null? stage-failures)
   (printf "ALL STAGES GREEN\n")
   (exit 0)]
  [else
   (printf "~a stage(s) FAILED\n" (length stage-failures))
   (exit 1)])
