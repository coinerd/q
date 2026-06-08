#lang racket/base

;; @speed slow  ;; @suite security

;; tests/helpers/test-sandbox.rkt — Canonical test sandbox for environment isolation.
;;
;; Provides `with-test-sandbox` that creates isolated temp directories for
;; project, session, home, and quarantine roots. Parameterizes current-directory
;; and environment variables, restoring everything on exit (normal or exceptional).
;;
;; This module does NOT migrate existing tests — it provides a clean API for
;; new and pilot-converted tests.

(require racket/file
         racket/match)

(provide make-test-sandbox
         test-sandbox?
         test-sandbox-project-dir
         test-sandbox-session-dir
         test-sandbox-home-dir
         test-sandbox-q-dir
         test-sandbox-quarantine-dir
         test-sandbox-temp-root
         with-test-sandbox)

;; ---------------------------------------------------------------------------
;; Struct
;; ---------------------------------------------------------------------------

(struct test-sandbox
        (project-dir ; temp project directory
         session-dir ; temp session directory
         home-dir ; temp HOME directory
         q-dir ; temp q config directory (inside home)
         quarantine-dir ; temp quarantine directory (inside project)
         temp-root) ; parent temp root for cleanup
  #:transparent)

;; ---------------------------------------------------------------------------
;; Constructor
;; ---------------------------------------------------------------------------

(define (make-test-sandbox #:preserve? [preserve? #f])
  "Create a fresh test-sandbox with isolated temp directories.
   If preserve? is #t, directories will NOT be cleaned up on exit."
  (define temp-root (make-temporary-file "q-sandbox-~a" 'directory))
  (define project-dir (build-path temp-root "project"))
  (define session-dir (build-path temp-root "sessions"))
  (define home-dir (build-path temp-root "home"))
  (define q-dir (build-path home-dir ".config" "q"))
  (define quarantine-dir (build-path project-dir ".quarantine"))
  (make-directory* project-dir)
  (make-directory* session-dir)
  (make-directory* home-dir)
  (make-directory* q-dir)
  (make-directory* quarantine-dir)
  (test-sandbox project-dir session-dir home-dir q-dir quarantine-dir temp-root))

;; ---------------------------------------------------------------------------
;; Cleanup
;; ---------------------------------------------------------------------------

(define (cleanup-sandbox! sb #:preserve? [preserve? #f])
  (unless preserve?
    (define root (test-sandbox-temp-root sb))
    (when (directory-exists? root)
      (delete-directory/files root #:must-exist? #f))))

;; ---------------------------------------------------------------------------
;; Env var save/restore
;; ---------------------------------------------------------------------------

(define (save-env vars)
  "Save current values of env var names. Returns an alist."
  (for/list ([v (in-list vars)])
    (cons v (getenv v))))

(define (restore-env! saved)
  "Restore env vars from saved alist. Unsets vars that were #f."
  (for ([p (in-list saved)])
    (define var (car p))
    (define val (cdr p))
    (if val
        (putenv var val)
        (putenv var ""))))

;; ---------------------------------------------------------------------------
;; with-test-sandbox
;; ---------------------------------------------------------------------------

(define (with-test-sandbox thunk
                           #:preserve? [preserve? #f]
                           #:isolate-home? [isolate-home? #t]
                           #:isolate-cwd? [isolate-cwd? #t])
  "Run thunk with a fresh test sandbox. Creates isolated directories,
   parameterizes current-directory and optionally HOME env, and
   guarantees cleanup on normal or exceptional exit."
  (define sb (make-test-sandbox #:preserve? preserve?))
  (define saved-cwd (current-directory))
  (define env-vars-to-isolate
    (if isolate-home?
        '("HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME")
        '()))
  (define saved-env (save-env env-vars-to-isolate))

  (dynamic-wind
   (lambda ()
     (when isolate-cwd?
       (current-directory (test-sandbox-project-dir sb)))
     (when isolate-home?
       (putenv "HOME" (path->string (test-sandbox-home-dir sb)))
       (putenv "XDG_CONFIG_HOME" (path->string (build-path (test-sandbox-home-dir sb) ".config")))
       (putenv "XDG_DATA_HOME"
               (path->string (build-path (test-sandbox-home-dir sb) ".local" "share")))
       (putenv "XDG_CACHE_HOME" (path->string (build-path (test-sandbox-home-dir sb) ".cache")))))
   (lambda () (thunk sb))
   (lambda ()
     (when isolate-cwd?
       (current-directory saved-cwd))
     (restore-env! saved-env)
     (cleanup-sandbox! sb #:preserve? preserve?))))
