#!/usr/bin/env racket
#lang racket/base

;; scripts/bump-version.rkt <new-version>
;;
;; Bump the pinned version strings for a release:
;;   - q/info.rkt        : (define version "<new>")
;;   - q/util/version.rkt: (define q-version "<new>")
;;
;; Idempotent: a file already at <new-version> is reported "ok" and left
;; untouched. Exits non-zero if a version form cannot be found in a file it
;; was asked to update. Does NOT edit CHANGELOG.md or create tags — the
;; release notes and tag are produced by the close-out flow
;; (scripts/release-closeout.rkt), not by the bump.

(require racket/string
         racket/path
         racket/file)

(define (usage+exit code)
  (displayln "usage: racket scripts/bump-version.rkt <new-version>   (e.g. 1.00.02)")
  (exit code))

(define new-version
  (let ([args (vector->list (current-command-line-arguments))])
    (cond
      [(and (= (length args) 1) (regexp-match? #rx"^[0-9]+\\.[0-9]+\\.[0-9]+$" (car args)))
       (car args)]
      [else (usage+exit 2)])))

;; Locate the q root: nearest ancestor (from the script's dir, then cwd)
;; containing both info.rkt and util/version.rkt. Works through the repo-root
;; scripts/ symlink as well.
(define (find-q-root start)
  (let loop ([dir (simplify-path (path->complete-path start))])
    (cond
      [(and (file-exists? (build-path dir "info.rkt"))
            (file-exists? (build-path dir "util" "version.rkt")))
       dir]
      [(equal? dir (simplify-path (build-path dir 'up))) #f] ;; reached filesystem root
      [else (loop (simplify-path (build-path dir 'up)))])))

(define q-root
  (or (find-q-root (path-only (find-system-path 'run-file)))
      (find-q-root (find-system-path 'orig-dir))
      (raise-user-error 'bump-version "cannot locate q root (info.rkt + util/version.rkt)")))

(define targets (list (build-path q-root "info.rkt") (build-path q-root "util" "version.rkt")))

(define version-form-regex #px"^\\(define\\s+(?:q-)?version\\s+\"[0-9]+\\.[0-9]+\\.[0-9]+\"\\)")

(for ([target (in-list targets)])
  (define lines (file->lines target))
  (define changed? #f)
  (define new-lines
    (for/list ([line (in-list lines)])
      (cond
        [(regexp-match? version-form-regex line)
         ;; NB: replacement is a procedure — a plain "\\1<ver>\\2" string would
         ;; misparse when <ver> starts with a digit ("\11" = backref group 11).
         (define updated
           (regexp-replace #px"^(\\(define\\s+(?:q-)?version\\s+\")[0-9]+\\.[0-9]+\\.[0-9]+(\")"
                           line
                           (lambda (m pre post) (string-append pre new-version post))))
         (unless (string=? updated line)
           (set! changed? #t))
         updated]
        [else line])))
  (cond
    [(and (not changed?)
          (for/or ([line (in-list lines)])
            (and (regexp-match? version-form-regex line)
                 (string-contains? line (string-append "\"" new-version "\"")))))
     (printf "~a: already at ~a\n" (find-relative-path q-root target) new-version)]
    [changed?
     (call-with-output-file* target
                             (lambda (out)
                               (for ([l (in-list new-lines)])
                                 (displayln l out)))
                             #:exists 'truncate)
     (printf "~a: bumped to ~a\n" (find-relative-path q-root target) new-version)]
    [else
     (eprintf "~a: no (define [q]version \"…\") form found\n" (find-relative-path q-root target))
     (exit 1)]))

(printf "version bump complete: ~a\n" new-version)
