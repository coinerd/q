#lang racket/base

;; tests/helpers/w2-mini-git-repo.rkt — Minimal hermetic git repository
;; fixture for the W2 evidence-integrity tests and register guards. A tiny
;; purpose-built builder (init, config, commit) rather than the heavier
;; pristine-git fixture: these tests only need a two-commit history with
;; known paths.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string)

(provide hermetic-git-env
         make-mini-repo!
         mini-git!
         mini-commit-file!
         mini-read-file)

;; Neutralize ambient git state (GIT_DIR, GIT_WORK_TREE, GIT_INDEX_FILE, ...)
;; so a test repo can never be re-targeted by its surroundings.
(define (hermetic-git-env)
  (define clean (make-environment-variables))
  (for ([kv (in-list (environment-variables-names (current-environment-variables)))]
        #:unless (regexp-match? #px"(?i:^git_)" kv))
    (environment-variables-set! clean
                                kv
                                (environment-variables-ref (current-environment-variables) kv)))
  clean)

(define (make-mini-repo! tag)
  (define root (make-temporary-file (format "q-w2-~a-~~a" tag) 'directory))
  (define repo (build-path root "repo"))
  (make-directory* repo)
  (mini-git! repo "init" "-b" "main" ".")
  (mini-git! repo "config" "user.email" "w2-fixture@example.invalid")
  (mini-git! repo "config" "user.name" "W2 Fixture")
  repo)

(define (mini-git! repo . args)
  ;; Every git invocation must be hermetic: a pre-commit hook (or any outer
  ;; git process) exports GIT_DIR / GIT_INDEX_FILE / GIT_OBJECT_DIRECTORY,
  ;; which would otherwise re-target these subprocesses at the outer
  ;; repository and corrupt or read foreign object stores.
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define out (open-output-bytes))
    (define err (open-output-bytes))
    (define-values (proc stdout stdin stderr)
      (apply subprocess
             #f
             #f
             #f
             (find-executable-path "git")
             "-C"
             (path->string (path->complete-path repo))
             args))
    (close-output-port stdin)
    (define out-bytes (port->bytes stdout))
    (define err-bytes (port->bytes stderr))
    (subprocess-wait proc)
    (close-input-port stdout)
    (close-input-port stderr)
    (unless (zero? (subprocess-status proc))
      (error 'mini-git!
             "git ~a failed: ~a"
             (first args)
             (string-trim (bytes->string/utf-8 err-bytes))))
    (string-trim (bytes->string/utf-8 out-bytes))))

;; Write rel with content, stage it, and commit. Returns the new head SHA.
(define (mini-commit-file! repo rel content msg)
  (define path (build-path repo rel))
  (make-directory* (path-only path))
  (display-to-file content path #:exists 'replace)
  (mini-git! repo "add" "-A" ".")
  (mini-git! repo "commit" "-q" "-m" msg)
  (mini-git! repo "rev-parse" "HEAD"))

(define (mini-read-file repo rel)
  (file->string (build-path repo rel)))
