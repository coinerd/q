#!/usr/bin/env racket
#lang racket/base

;; scripts/archive-planning.rkt — Archive stale .planning/ files by milestone.
;;
;; Moves completed milestone files from .planning/ to .planning/archive/<version>/.
;; Keeps active files: PLAN.md, STATE.md, HANDOFF.json, VALIDATION.md,
;; current milestone files, and files without a milestone prefix.
;;
;; Usage:
;;   cd q/ && racket scripts/archive-planning.rkt            # archive stale files
;;   cd q/ && racket scripts/archive-planning.rkt --dry-run  # preview only

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string)

(define args (vector->list (current-command-line-arguments)))
(define dry-run? (member "--dry-run" args))

;; Allow overriding planning dir for testing
(define planning-dir-arg
  (for/or ([a (in-list args)])
    (define m (regexp-match #rx"^--planning-dir=(.+)$" a))
    (and m (cadr m))))

;; --- Protected files (never archive) ---

(define protected-files '("PLAN.md" "STATE.md" "HANDOFF.json" "VALIDATION.md" "SUMMARY.md"))

(define (protected? filename)
  (member filename protected-files))

;; --- Extract milestone group from filename ---

(define (milestone-group filename)
  ;; "V0113_REMEDIATION_PLAN.md" -> "v0113"
  ;; "v0113_foo.md" -> "v0113"
  ;; "BUG_PLAN.md" -> #f (no version)
  ;; "ARCH_REVIEW.md" -> #f
  (define prefix-rxs (list #rx"^([Vv]0[0-9]+)_" #rx"^([Vv]0[0-9][0-9]+)"))
  (for/or ([rx (in-list prefix-rxs)])
    (define m (regexp-match rx filename))
    (and m (string-downcase (cadr m)))))

;; --- Get current milestone version from STATE.md ---

(define (current-milestone planning-dir)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define content (file->string (build-path planning-dir "STATE.md")))
    (define m (regexp-match #rx"#.*v[0-9]+\\.[0-9]+\\.[0-9]+" content))
    (and m
         (let* ([s (car m)]
                [ver-m (regexp-match #rx"v([0-9]+)\\.([0-9]+)\\.([0-9]+)" s)])
           (and ver-m (format "v~a~a~a" (cadr ver-m) (caddr ver-m) (cadddr ver-m)))))))

;; --- Collect archive candidates ---

(define (collect-candidates planning-dir current-ms)
  (define files (directory-list planning-dir))
  (define candidates '())
  (for ([f (in-list files)])
    (define name (path->string f))
    (define full (build-path planning-dir name))
    (cond
      ;; Skip directories (including archive/)
      [(directory-exists? full) (void)]
      ;; Skip protected files
      [(protected? name) (void)]
      [else
       (define group (milestone-group name))
       (cond
         ;; No milestone prefix — keep
         [(not group) (void)]
         ;; Current milestone — keep
         [(and current-ms (string=? group current-ms)) (void)]
         ;; Old milestone — archive candidate
         [else (set! candidates (cons (cons name group) candidates))])]))
  (reverse candidates))

;; --- Main ---

(define (main)
  (define planning-dir
    (simplify-path (if planning-dir-arg
                       (string->path planning-dir-arg)
                       "../.planning")))

  (unless (directory-exists? planning-dir)
    (printf "ERROR: ~a not found~n" planning-dir)
    (exit 1))

  (printf "=== .planning/ Archive ===~n~n")

  (define current-ms (current-milestone planning-dir))
  (printf "Current milestone: ~a~n" (or current-ms "(unknown)"))

  (define candidates (collect-candidates planning-dir current-ms))

  (if (null? candidates)
      (begin
        (printf "~nNo archive candidates found.~n")
        (exit 0))
      (printf "~nFound ~a archive candidates:~n~n" (length candidates)))

  ;; Group by milestone
  (define groups (make-hash))
  (for ([c (in-list candidates)])
    (hash-update! groups (cdr c) (λ (lst) (cons (car c) lst)) '()))

  (for ([(group files) (in-hash groups)])
    (printf "  [~a] (~a files)~n" group (length files))
    (for ([f (in-list (sort files string<?))])
      (printf "    ~a~n" f)))

  (when dry-run?
    (printf "~nDry run — no files moved.~n")
    (exit 0))

  ;; Move files
  (printf "~nMoving files...~n")
  (for ([(group files) (in-hash groups)])
    (define archive-dir (build-path planning-dir "archive" group))
    (make-directory* archive-dir)
    (for ([f (in-list files)])
      (define src (build-path planning-dir f))
      (define dst (build-path archive-dir f))
      (rename-file-or-directory src dst)
      (printf "  ~a → archive/~a/~a~n" f group f)))

  (printf "~nArchive complete. ~a files moved.~n" (length candidates)))

(main)
