#lang racket/base

;; p3-collect.rkt — prospective case collection for CL5 P3 (#9982).
;;
;; Governed by docs/reports/CL5-P3-PREREG.md (rev 2). This module records cases; it
;; does not decide anything about efficiency, does not select tests for production
;; and does not change behaviour. Collection authorises no GO decision.
;;
;; Fail-closed rules:
;;   - a case failing any eligibility condition is recorded as `unavailable` WITH
;;     the failing condition; nothing is dropped silently;
;;   - the output file is append-only: an existing landed_sha is never rewritten,
;;     and re-collecting the same sha appends nothing;
;;   - strata come from the corpus classifier (`scripts/cl5/w3-corpus.rkt`) executed
;;     with the CASE TREE as its root, so a case is classified with its own coverage
;;     manifest and module graph, never today's;
;;   - no eligibility input is inferred from a later revision.
;;
;; Evaluation-only.

(require json
         racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/port
         racket/set
         racket/string
         racket/system
         "ci-evidence.rkt")

(provide collect-one
         case-record
         case-record-status
         case-record-reason
         case-record-landed-sha
         case-record-strata
         case->jsexpr
         append-cases!
         existing-shas
         changed-files->version-bump-only?
         engine-present?
         run-eligible?
         parse-explain-files
         comprehensive-shape
         strata-of-classification
         runs-for-sha-paged
         required-job-names
         version-bump-modules)

(define schema "cl5-p3-case/1")

(define engine-paths (list "qt" "scripts/testing/quick-audit.rkt" "scripts/testing/full-plan.rkt"))

(define manifest-path "tests/.coverage-manifest.json")

;; The smoke producer's executed set is the file named by the workflow's own command
;; (pinned by tests/test-cl5-baseline.rkt), not a lane selection.
(define smoke-files (list "tests/test-provider-smoke.rkt"))

(define version-bump-modules '("info.rkt" "util/version.rkt"))

;; The pinned workflow definition: the required CI job names, single-sourced from
;; `required-ci-producers` (scripts/testing/full-plan.rkt) and re-pinned by
;; tests/test-cl5-baseline.rkt; never parsed from YAML here.
(define required-job-names
  (list "test (0)"
        "test (1)"
        "test (2)"
        "test-platform"
        "security"
        "workflows (0)"
        "workflows (1)"
        "smoke (ubuntu-latest)"))

;; ---- pure helpers ------------------------------------------------------------------

;; E6: a version-bump-only change touches nothing but the version stamp.
(define (changed-files->version-bump-only? paths)
  (and (pair? paths)
       (for/and ([p (in-list paths)])
         (and (member p version-bump-modules equal?) #t))))

;; E2: all engine paths present (partial presence is absence).
(define (engine-present? presence)
  (and (for/and ([p (in-list engine-paths)])
         (hash-ref presence p #f))
       #t))

;; E5: a successful `CI` push run whose required jobs all ran and passed.
(define (run-eligible? run jobs)
  (and (hash? run)
       (equal? (hash-ref run 'name "") "CI")
       (equal? (hash-ref run 'event "") "push")
       (equal? (hash-ref run 'conclusion #f) "success")
       (for/and ([needed (in-list required-job-names)])
         (for/or ([j (in-list jobs)])
           (and (equal? (hash-ref j 'name "") needed)
                (equal? (hash-ref j 'conclusion #f) "success"))))))

;; `--explain` output: the planned test files, one per line.
(define (parse-explain-files text)
  (for/list ([line (in-list (string-split text "\n"))]
             #:when (regexp-match? #px"^tests/[^ ]+\\.rkt$" line))
    line))

;; The surface shape a case's own plans describe: unique files, producer slots.
(define (comprehensive-shape producer-file-lists)
  (define all (append* producer-file-lists))
  (hash 'unique_files (length (remove-duplicates all)) 'slot_count (length all)))

;; The classifier writes one JSON manifest; a case is the single entry in `cases`.
(define (strata-of-classification manifest)
  (define cases (hash-ref manifest 'cases '()))
  (cond
    [(null? cases) (hash 'stratum #f 'production_modules '() 'cone_keys '())]
    [else
     (define c (car cases))
     (hash 'stratum
           (hash-ref c 'stratum #f)
           'production_modules
           (hash-ref c 'production-modules '())
           'cone_keys
           (hash-ref c 'cone-keys '()))]))

;; The runs endpoint is newest-first and a busy commit can have more than one page of
;; workflow_run entries, so a first page alone can hide the CI run. Page until
;; `total_count` is reached (bounded), then filter by workflow name.
(define (runs-for-sha-paged fetch sha)
  (let loop ([page 1]
             [acc '()])
    (define payload (fetch (format "actions/runs?head_sha=~a&per_page=100&page=~a" sha page)))
    (define runs (hash-ref payload 'workflow_runs '()))
    (define total (hash-ref payload 'total_count (length runs)))
    (define acc* (append acc runs))
    (cond
      [(null? runs) acc*]
      [(>= (length acc*) total) acc*]
      [(>= page 10) acc*]
      [else (loop (add1 page) acc*)])))

;; ---- records -----------------------------------------------------------------------

(struct case-record
        (landed-sha tree-sha
                    observed-at
                    cohort ; "P" | "H"
                    subject
                    changed-files
                    version-bump-only
                    engine
                    manifest-sha
                    strata
                    ci
                    comprehensive
                    status ; "eligible" | "unavailable"
                    reason ; first failing condition, or #f
                    detail)
  #:transparent)

(define (case->jsexpr c)
  (hash 'schema
        schema
        'landed_sha
        (case-record-landed-sha c)
        'tree_sha
        (case-record-tree-sha c)
        'observed_at
        (case-record-observed-at c)
        'cohort
        (case-record-cohort c)
        'subject
        (case-record-subject c)
        'changed_files
        (case-record-changed-files c)
        'version_bump_only
        (case-record-version-bump-only c)
        'engine
        (case-record-engine c)
        'manifest_sha
        (case-record-manifest-sha c)
        'strata
        (let ([s (case-record-strata c)])
          (hash 'stratum
                (let ([v (hash-ref s 'stratum #f)])
                  (cond
                    [(not v) 'null]
                    [(symbol? v) (symbol->string v)]
                    [else v]))
                'production_modules
                (hash-ref s 'production_modules '())
                'cone_keys
                (hash-ref s 'cone_keys '())))
        'ci
        (case-record-ci c)
        'comprehensive
        (case-record-comprehensive c)
        'stratum_source
        "scripts/cl5/w3-corpus.rkt executed with the case tree as root"
        'status
        (case-record-status c)
        'reason
        (or (case-record-reason c) 'null)
        'detail
        (let ([d (case-record-detail c)]) (if (equal? d #f) 'null d))))

;; ---- append-only storage -----------------------------------------------------------

(define (existing-shas path)
  (if (file-exists? path)
      (for/set ([line (in-list (file->lines path))]
                #:when (not (string=? (string-trim line) "")))
        (hash-ref (with-input-from-string line read-json) 'landed_sha))
      (set)))

;; Appends only shas not already present; returns the appended records.
(define (append-cases! path records)
  (define seen (existing-shas path))
  (define fresh
    (for/list ([c (in-list records)]
               #:unless (set-member? seen (case-record-landed-sha c)))
      c))
  (when (pair? fresh)
    (call-with-output-file path
                           #:exists 'append
                           (lambda (out)
                             (for ([c (in-list fresh)])
                               (write-json (case->jsexpr c) out)
                               (newline out)))))
  fresh)

;; ---- process helpers (captured, so output never leaks into the JSONL) --------------

(define (run-captured dir program args)
  (define exe (or (find-executable-path program) program))
  (define-values (status stdout stderr) (capture dir exe args))
  (values status stdout stderr))

(define (git-line root . args)
  (define-values (status stdout _stderr) (run-captured root "git" args))
  (unless (zero? status)
    (error 'p3-collect "git ~a failed: ~a" args (string-trim (string-append _stderr))))
  (string-trim (string-append stdout "")))

(define (blob-at? root sha path)
  (define-values (status stdout _stderr)
    (run-captured root "git" (list "cat-file" "-t" (format "~a:~a" sha path))))
  (and (zero? status) (equal? (string-trim (string-append stdout "")) "blob")))

(define (timestamp)
  (define d (seconds->date (current-seconds) #t))
  (define (pad n)
    (if (< n 10)
        (format "0~a" n)
        (format "~a" n)))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad (date-month d))
          (pad (date-day d))
          (pad (date-hour d))
          (pad (date-minute d))
          (pad (date-second d))))

;; ---- one case ----------------------------------------------------------------------

(define producer-invocations
  (list (list "fast" 0 3)
        (list "fast" 1 3)
        (list "fast" 2 3)
        (list "platform" 0 1)
        (list "security" 0 1)
        (list "workflows" 0 2)
        (list "workflows" 1 2)))

(define (collect-one #:root root #:worktree wt #:sha sha #:cohort cohort #:fetch fetch)
  (define tree (git-line root "rev-parse" (string-append sha "^{tree}")))
  (define subject (git-line root "log" "-1" "--format=%s" sha))
  (define parent (git-line root "rev-parse" (string-append sha "^")))
  (define changed
    (for/list ([l (in-list (string-split
                            (git-line root "diff-tree" "-r" "--no-commit-id" "--name-only" parent sha)
                            "\n"))]
               #:when (not (string=? l "")))
      l))
  (define bump? (changed-files->version-bump-only? changed))
  (define presence
    (for/hash ([p (in-list (append engine-paths (list manifest-path)))])
      (values p (blob-at? root sha p))))
  (define engine? (engine-present? presence))
  (define manifest? (hash-ref presence manifest-path #f))
  (define manifest-blob
    (and manifest? (git-line root "rev-parse" (format "~a:~a" sha manifest-path))))
  (define (mk status reason detail strata ci shape)
    (case-record sha
                 tree
                 (timestamp)
                 cohort
                 subject
                 changed
                 bump?
                 (for/hash ([p (in-list engine-paths)]
                            [k (in-list '(qt quick-audit full-plan))])
                   (values k (hash-ref presence p #f)))
                 manifest-blob
                 strata
                 ci
                 shape
                 status
                 reason
                 detail))
  (cond
    [bump? (mk "unavailable" "E6" "version-bump-only change" (hash) (hash) (hash))]
    [(not engine?)
     (mk "unavailable" "E2" "selection engine absent from the tree" (hash) (hash) (hash))]
    [(not manifest?)
     (mk "unavailable" "E3" "coverage manifest absent from the tree" (hash) (hash) (hash))]
    [else
     (define runs (runs-for-sha-paged fetch sha))
     (define ci-run
       (for/first ([r (in-list runs)]
                   #:when (equal? (hash-ref r 'name "") "CI")
                   #:when (equal? (hash-ref r 'event "") "push"))
         r))
     (define jobs
       (if ci-run
           (hash-ref (fetch (format "actions/runs/~a/jobs?per_page=100" (hash-ref ci-run 'id)))
                     'jobs
                     '())
           '()))
     (define ci
       (hash 'run_id
             (and ci-run (hash-ref ci-run 'id))
             'conclusion
             (and ci-run (hash-ref ci-run 'conclusion #f))
             'event
             (and ci-run (hash-ref ci-run 'event #f))
             'jobs
             (for/list ([j (in-list jobs)])
               (hash 'name (hash-ref j 'name "") 'conclusion (hash-ref j 'conclusion #f)))))
     (cond
       [(not (run-eligible? ci-run jobs))
        (mk "unavailable" "E5" "no successful comprehensive CI run for this sha" (hash) ci (hash))]
       [else
        (define plans
          (for/list ([spec (in-list producer-invocations)])
            (define lane (first spec))
            (define index (second spec))
            (define total (third spec))
            (define-values (status stdout _stderr)
              (run-captured wt
                            (path->string (build-path wt "qt"))
                            (list "--lane"
                                  lane
                                  "--shard-index"
                                  (number->string index)
                                  "--shard-total"
                                  (number->string total)
                                  "--explain")))
            (if (zero? status)
                (parse-explain-files stdout)
                '())))
        (cond
          [(for/or ([l (in-list plans)])
             (null? l))
           (mk "unavailable" "E4" "a producer's selection is empty at this tree" (hash) ci (hash))]
          [else
           ;; Strata from the corpus classifier executed with the CASE TREE as root,
           ;; so its own coverage manifest and module graph are used.
           (define classify-out (build-path wt ".p3-classify.json"))
           (define-values (cstatus _cout _cerr)
             (parameterize ([current-directory (path->complete-path wt)])
               (run-captured wt
                             (or (find-executable-path "racket") "racket")
                             (list (path->string (build-path root "scripts/cl5/w3-corpus.rkt"))
                                   "-r"
                                   sha
                                   "-n"
                                   "1"
                                   "--out"
                                   (path->string classify-out)))))
           (define classification
             (and (zero? cstatus)
                  (file-exists? classify-out)
                  (with-input-from-file classify-out read-json)))
           (define strata
             (if classification
                 (strata-of-classification classification)
                 (hash 'stratum #f)))
           (cond
             [(not (hash-ref strata 'stratum))
              (mk "unavailable"
                  "E7"
                  "the corpus classifier excludes this change (no production module)"
                  strata
                  ci
                  (comprehensive-shape (cons smoke-files plans)))]
             [else
              (mk "eligible" #f #f strata ci (comprehensive-shape (cons smoke-files plans)))])])])]))

;; ---- CLI (module+ main: requiring this file has no side effects) -------------

(module+ main
  (require racket/cmdline)
  ;; ---- CLI ---------------------------------------------------------------------------

  (define from-sha (make-parameter #f))
  (define to-sha (make-parameter "main"))
  (define cohort (make-parameter "P"))
  (define out-path (make-parameter #f))
  (define limit (make-parameter #f))
  (define dry-run? (make-parameter #f))
  (define workspace (make-parameter "."))
  (define worktree-dir (make-parameter #f))

  (command-line
   #:program "p3-collect.rkt"
   #:once-each [("--from") v "Start of the window (exclusive)" (from-sha v)]
   [("--to") v "End of the window (inclusive, default main)" (to-sha v)]
   [("--cohort") v "Cohort label: P (prospective) or H (engine-era historical)" (cohort v)]
   [("--out") v "Append-only JSONL output" (out-path v)]
   [("--limit") v "Collect at most N landed changes" (limit (string->number v))]
   [("--workspace") v "Workspace root holding scripts/gh_helpers.py" (workspace v)]
   [("--worktree-dir") v "Scratch directory for per-case worktrees" (worktree-dir v)]
   [("--dry-run") "Collect and print, write nothing" (dry-run? #t)]
   #:args ()
   (unless (from-sha)
     (error 'p3-collect "--from is required"))
   (unless (member (cohort) '("P" "H") equal?)
     (error 'p3-collect "--cohort must be P or H"))
   (unless (or (dry-run?) (out-path))
     (error 'p3-collect "--out is required unless --dry-run"))
   (define root (path->complete-path (current-directory)))
   (define wt-root (or (worktree-dir) (find-system-path 'temp-dir)))
   (define shas
     (for/list ([l (in-list
                    (string-split
                     (git-line root "rev-list" "--first-parent" (format "~a..~a" (from-sha) (to-sha)))
                     "\n"))]
                #:when (not (string=? l "")))
       l))
   (define selected
     (if (limit)
         (take shas (min (limit) (length shas)))
         shas))
   (printf "p3-collect: ~a landed change(s) in ~a..~a; collecting ~a (cohort ~a)~n"
           (length shas)
           (from-sha)
           (to-sha)
           (length selected)
           (cohort))
   (define fetch (lambda (path) (gh-json (workspace) "GET" path)))
   (define base (build-path wt-root "p3-case-wt"))
   (define cases
     (for/list ([sha (in-list selected)])
       (printf "  ~a ... " (substring sha 0 8))
       (flush-output)
       (when (directory-exists? base)
         (run-captured root "git" (list "worktree" "remove" "--force" (path->string base))))
       (define-values (add-status _o add-err)
         (run-captured root "git" (list "worktree" "add" "--detach" (path->string base) sha)))
       ;; Every exit path removes the scratch worktree: a failed collection must not
       ;; leave a registered worktree behind, or the next run would skip the case.
       (define c
         (dynamic-wind
          void
          (lambda ()
            (cond
              [(not (zero? add-status))
               (case-record sha
                            #f
                            (timestamp)
                            (cohort)
                            (git-line root "log" "-1" "--format=%s" sha)
                            '()
                            #f
                            (hash)
                            #f
                            (hash)
                            (hash)
                            (hash)
                            "unavailable"
                            "worktree"
                            (format "could not create a worktree: ~a"
                                    (string-trim (string-append add-err ""))))]
              [else
               (with-handlers ([exn:fail? (lambda (e)
                                            (eprintf "error: ~a~n" (exn-message e))
                                            ;; Fail closed: an unexpected failure is recorded as
                                            ;; unavailable with its message, never dropped and never
                                            ;; turned into a silent skip.
                                            (case-record sha
                                                         #f
                                                         (timestamp)
                                                         (cohort)
                                                         (git-line root "log" "-1" "--format=%s" sha)
                                                         '()
                                                         #f
                                                         (hash)
                                                         #f
                                                         (hash)
                                                         (hash)
                                                         (hash)
                                                         "unavailable"
                                                         "collection-error"
                                                         (exn-message e)))])
                 (collect-one #:root root
                              #:worktree base
                              #:sha sha
                              #:cohort (cohort)
                              #:fetch fetch))]))
          (lambda ()
            (run-captured root "git" (list "worktree" "remove" "--force" (path->string base))))))
       (when (case-record? c)
         ;; Append as each case is collected: a long run that is interrupted must not
         ;; lose already-observed cases, and a re-run resumes from the file.
         (unless (dry-run?)
           (append-cases! (out-path) (list c)))
         (printf "~a~a~n"
                 (case-record-status c)
                 (if (case-record-reason c)
                     (format " (~a)" (case-record-reason c))
                     "")))
       c))
   (define kept (filter case-record? cases))
   (cond
     [(dry-run?)
      (for ([c (in-list kept)])
        (printf "~a~n" (jsexpr->string (case->jsexpr c))))]
     [else
      (define fresh (append-cases! (out-path) kept))
      (printf "collected ~a case(s); appended ~a new line(s) to ~a~n"
              (length kept)
              (length fresh)
              (out-path))])
   (define eligible
     (for/list ([c (in-list kept)]
                #:when (equal? (case-record-status c) "eligible"))
       c))
   (printf "eligible: ~a of ~a (strata: ~a)~n"
           (length eligible)
           (length kept)
           (for/fold ([h (hash)]) ([c (in-list eligible)])
             (hash-update h (hash-ref (case-record-strata c) 'stratum "unknown") add1 0)))))
