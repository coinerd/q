#lang racket/base

;; util/path-filters.rkt — Shared path filtering predicates
;;
;; Canonical implementations of hidden-file, VCS-directory,
;; and skip-directory checks. Replaces duplicated definitions
;; across find, grep, and ls builtins.

(provide hidden-name?
         vcs-dir?
         skip-dirs
         should-skip-entry?
         path-component-hidden?)

;; Directories to skip during traversal (VCS + common noise)
(define skip-dirs '(".git" ".hg" ".svn" "node_modules"))

;; Is the name a VCS or noise directory?
(define (vcs-dir? name)
  (member name skip-dirs))

;; Does the name start with a dot?
(define (hidden-name? name)
  (and (string? name)
       (> (string-length name) 0)
       (char=? (string-ref name 0) #\.)))

;; Should this directory entry be skipped?
;; Skips VCS/noise dirs and hidden files (unless showing hidden).
(define (should-skip-entry? name #:show-hidden? [show-hidden? #f])
  (or (vcs-dir? name)
      (and (hidden-name? name) (not show-hidden?))))

;; Does any component of the absolute path start with a dot?
(define (path-component-hidden? abs-path)
  (for/or ([s (in-list (cdr (explode-path abs-path)))])
    (hidden-name? (path->string s))))
