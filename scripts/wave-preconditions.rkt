#!/usr/bin/env racket
#lang racket/base

;; wave-preconditions.rkt — mandated pre-wave applicability gate (BUG-0012).
;;
;; Run BEFORE starting any wave. Verifies, against the remote source of truth
;; (never the local mirror alone):
;;   1. local main == origin/main  (git fetch; fail if behind or diverged)
;;   2. the wave being executed exists; prerequisite waves modified after it
;;      was written are flagged ADVISORY (state may have moved under the plan)
;;   3. the registry INDEX is self-consistent (delegates to check-registry.rkt --check)
;;
;; Prints a structured verdict:
;;   PRECONDITIONS: READY <hint>
;;   PRECONDITIONS: NOT-READY <reason>   (exit 1)
;;
;; Usage: racket scripts/wave-preconditions.rkt [repo-root] [wave-file]

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system)

(define repo-root (make-parameter #f))
(define wave-file (make-parameter #f))

(command-line
 #:args ([root (path->string (current-directory))] [wave #f])
 (repo-root (path->complete-path (string->path root)))
 (wave-file (and wave (path->complete-path (string->path wave)))))

(define (git . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (parameterize ([current-output-port out] [current-error-port err]
                 [current-directory (repo-root)])
    (apply system* (find-executable-path "git") args))
  (values (string-trim (get-output-string out)) (string-trim (get-output-string err))))

(define reasons '()) ; collected NOT-READY reasons

(define (not-ready! fmt . args)
  (set! reasons (cons (apply format fmt args) reasons)))

;; --------------------------------------------------------------- verdict

(define (print-verdict!)
  (cond [(null? reasons)
         (displayln "PRECONDITIONS: READY — mirror fresh, wave present, registry consistent")]
        [else
         (for ([r (in-list (reverse reasons))])
           (printf "PRECONDITIONS: NOT-READY ~a\n" r))
         (exit 1)]))

;; ----------------------------------------------------- 1. mirror freshness

;; git fetch origin; then compare local main with origin/main.
;; Behind   → NOT-READY (state has moved on; the plan may be superseded).
;; Diverged → NOT-READY (local-only commits must be reconciled first).
;; No upstream → NOT-READY (no source of truth to validate against).
(define-values (_fo _fe) (git "fetch" "origin"))

(define-values (local-sha rev-err) (git "rev-parse" "main"))
(define-values (remote-sha upstream-err) (git "rev-parse" "origin/main"))

(cond
  [(non-empty-string? (or rev-err upstream-err))
   (not-ready! "cannot resolve main/origin/main (~a / ~a) — no source of truth"
               (if (non-empty-string? rev-err) rev-err "ok")
               (if (non-empty-string? upstream-err) upstream-err "ok"))]
  [(string=? local-sha remote-sha)
   (printf "  fresh: main == origin/main @ ~a\n" (substring local-sha 0 10))]
  [else
   ;; one call: "ahead<TAB>behind" for local...origin/main
   (define-values (lr-out lr-err)
     (git "rev-list" "--left-right" "--count"
          (format "~a...~a" local-sha remote-sha)))
   (define parts (string-split lr-out "\t"))
   (cond
     [(and (= (length parts) 2)
           (string->number (first parts)) (string->number (second parts)))
      (define ahead (string->number (first parts)))
      (define behind (string->number (second parts)))
      (cond
        [(= ahead 0)
         (not-ready! "local main is BEHIND origin/main by ~a commit(s) (~a → ~a): re-sync before executing the wave (plan may be superseded)"
                     behind (substring local-sha 0 10) (substring remote-sha 0 10))]
        [(= behind 0)
         (not-ready! "local main has ~a commit(s) not on origin/main — push or reconcile before executing"
                     ahead)]
        [else
         (not-ready! "local main DIVERGED from origin/main (~a ahead, ~a behind) — reconcile before executing"
                     ahead behind)])]
     [else
      (not-ready! "cannot compare main with origin/main (rev-list said '~a' / '~a') — treat state as unknown"
                  lr-out lr-err)])])

;; --------------------------------------------------- 2. wave state check

;; The wave being executed must exist; prerequisite waves (lower W numbers)
;; modified after this plan was written indicate state moved under the plan —
;; the classic BUG-0012 failure. Future waves (higher numbers, pre-written in
;; this plan) are excluded by design: they are expected to be present.
(define (wave-num f)
  (define m (regexp-match #px"^W(\\d+)-" (path->string f)))
  (and m (string->number (cadr m))))

(define waves-dir (build-path (repo-root) ".planning" "waves"))
(cond
  [(not (directory-exists? waves-dir))
   (printf "  waves: no ~a directory (skipping wave check)\n" waves-dir)]
  [else
   (define (wave-mtime p) (file-or-directory-modify-seconds p))
   (define plans
     (filter (lambda (f) (regexp-match? #px"^W\\d+-.*\\.md$" (path->string f)))
             (directory-list waves-dir)))
   (cond
     [(null? plans)
      (not-ready! "no wave documents found under .planning/waves/ — plan state unknown")]
     [(wave-file)
      (define target (path->string (file-name-from-path (wave-file))))
      (if (file-exists? (wave-file))
          (let ([target-num (wave-num (string->path target))])
            ;; advisory: a prerequisite wave edited after this plan was written
            ;; may have moved state the plan does not account for.
            (define prereqs
              (filter (lambda (f) (and (wave-num f) target-num (>= target-num (wave-num f))
                                       (not (string=? (path->string f) target))))
                      plans))
            (for ([p (in-list prereqs)]
                  #:when (> (wave-mtime (build-path waves-dir p))
                            (wave-mtime (wave-file))))
              (printf "  ADVISORY: prerequisite ~a was modified after this plan was written — re-verify the wave still applies (see .planning/bugs/README.md)\n"
                      (path->string p)))
            (printf "  waves: executing ~a (~a plan(s) present)\n" target (length plans)))
          ;; wave file missing → hard NOT-READY
          (not-ready! "wave file ~a does not exist — plan state unknown" target))]
     [else
      (printf "  waves: ~a plan(s) present; newest = ~a\n"
              (length plans)
              (path->string
               (argmax (lambda (f) (wave-mtime (build-path waves-dir f))) plans)))])
   ;; report any wave files that git reports as modified/untracked (in-flight edits)
   (define-values (status-out _se) (git "status" "--porcelain" "--" ".planning/waves/"))
   (when (non-empty-string? status-out)
     (printf "  note: working tree has uncommitted wave changes:\n~a\n" status-out))])

;; ------------------------------------------------- 3. registry consistency

;; Planning-artifact invariants (BUG-0013): INDEX counts must match rows.
;; Delegates to check-registry.rkt --check; inconsistency → NOT-READY so a
;; stale/conflicted registry is healed before the wave mutates it.
(define checker (build-path (repo-root) "scripts" "check-registry.rkt"))
(cond
  [(not (file-exists? checker))
   (printf "  registry: no scripts/check-registry.rkt (skipping registry check)\n")]
  [else
   (define code
     (parameterize ([current-output-port (open-output-nowhere)]
                    [current-error-port (open-output-nowhere)])
       (apply system*/exit-code (find-executable-path "racket")
              (list (path->string checker) "--registry"
                    (path->string (build-path (repo-root) ".planning" "bugs")) "--check"))))
   (cond
     [(= code 0) (printf "  registry: INDEX consistent\n")]
     [else
      (not-ready! "registry INDEX inconsistent — run `racket scripts/check-registry.rkt` to heal before starting the wave")])])

(print-verdict!)
