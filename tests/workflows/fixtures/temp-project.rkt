#lang racket/base

;; tests/workflows/fixtures/temp-project.rkt — temp project/session lifecycle
;;
;; Creates temporary directories for testing with RAII cleanup.
;; Provides both functional and macro-based APIs.

(require racket/file
         racket/path)

(provide make-temp-project
         cleanup-temp-project!
         with-temp-project
         make-temp-session-dir)

;; ============================================================
;; make-temp-project
;; ============================================================

;; make-temp-project : (listof (cons path-string? string?)) -> (values path? path?)
;; Creates a temp directory with the given files.
;; files is a list of (relative-path . content) pairs.
;; Returns (values project-dir session-dir).
;; Caller must call cleanup-temp-project! when done.

(define (make-temp-project files)
  (define base (make-temporary-file "q-wf-project-~a" 'directory))
  (define session-dir (make-temporary-file "q-wf-sessions-~a" 'directory))
  ;; Create files
  (for ([f (in-list files)])
    (define rel-path (car f))
    (define content (cdr f))
    (define full-path (build-path base rel-path))
    (define-values (parent _name _must-be-dir?) (split-path full-path))
    (when parent
      (make-directory* parent))
    (call-with-output-file full-path
                           (lambda (out) (display content out))
                           #:exists 'replace))
  (values base session-dir))

;; ============================================================
;; cleanup-temp-project!
;; ============================================================

(define (cleanup-temp-project! project-dir session-dir)
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (when (and project-dir (directory-exists? project-dir))
      (delete-directory/files project-dir)))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (when (and session-dir (directory-exists? session-dir))
      (delete-directory/files session-dir))))

;; ============================================================
;; make-temp-session-dir
;; ============================================================

;; Just a temp dir for sessions, no project files.
(define (make-temp-session-dir)
  (make-temporary-file "q-wf-sessions-~a" 'directory))

;; ============================================================
;; with-temp-project macro (syntax-rules)
;; ============================================================

(define-syntax with-temp-project
  (syntax-rules ()
    [(_ ((project-dir session-dir) files-expr) body ...)
     (let-values ([(project-dir session-dir) (make-temp-project files-expr)])
       (dynamic-wind
         void
         (lambda () body ...)
         (lambda () (cleanup-temp-project! project-dir session-dir))))]))
