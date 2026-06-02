#lang racket/base

;; runtime/session-manager.rkt — unified session manager interface (#1321)
;;
;; Provides:
;;   session-manager?          — predicate for any session manager
;;   make-persistent-session-manager — wraps session-store functions
;;   sm-append!                — append entry to session
;;   sm-load                   — load session entries
;;   sm-list                   — list session IDs
;;   sm-fork!                  — fork a session
;;
;; In-memory session manager (from session-store.rkt) also satisfies
;; session-manager? via the in-memory-session-manager struct.

(require racket/contract
         racket/file
         racket/path
         "session-store.rkt"
         (only-in "../util/message.rkt" message?)
         (only-in "../util/errors.rkt" raise-session-error))

(provide (contract-out [session-manager? (-> any/c boolean?)]
                       [persistent-session-manager? (-> any/c boolean?)]
                       [in-memory-session-manager? (-> any/c boolean?)]
                       [make-persistent-session-manager (-> path-string? session-manager?)]
                       [make-in-memory-session-manager (-> session-manager?)]
                       [sm-append! (-> session-manager? string? message? void?)]
                       [sm-load (-> session-manager? string? list?)]
                       [sm-list (-> session-manager? (listof string?))]
                       [sm-fork! (->* (session-manager? string? string?) (string?) any/c)]
                       ;; Re-export in-memory operations for backward compat
                       [in-memory-append! (-> in-memory-session-manager? string? message? void?)]
                       [in-memory-append-entries! (-> in-memory-session-manager? string? list? void?)]
                       [in-memory-load (-> in-memory-session-manager? string? list?)]
                       [in-memory-list-sessions (-> in-memory-session-manager? (listof string?))]
                       [in-memory-fork!
                        (->* (in-memory-session-manager? string? string?) (string?) string?)]))

;; ============================================================
;; Persistent session manager — wraps file-backed session-store
;; ============================================================

(struct persistent-session-manager (base-dir) #:transparent)

(define (make-persistent-session-manager base-dir)
  (make-directory* base-dir)
  (persistent-session-manager base-dir))

;; Unified dispatch: check type and delegate

(define (session-manager? x)
  (or (persistent-session-manager? x) (in-memory-session-manager? x)))

(define (sm-append! mgr session-id msg)
  (cond
    [(persistent-session-manager? mgr)
     (define dir (build-path (persistent-session-manager-base-dir mgr) session-id))
     (make-directory* dir)
     (append-entry! (build-path dir "session.jsonl") msg)]
    [(in-memory-session-manager? mgr) (in-memory-append! mgr session-id msg)]
    [else (raise-session-error (format "not a session manager: ~a" mgr) #f)]))

(define (sm-load mgr session-id)
  (cond
    [(persistent-session-manager? mgr)
     (define log-path
       (build-path (persistent-session-manager-base-dir mgr) session-id "session.jsonl"))
     (if (file-exists? log-path)
         (load-session-log log-path)
         '())]
    [(in-memory-session-manager? mgr) (in-memory-load mgr session-id)]
    [else (raise-session-error (format "not a session manager: ~a" mgr) #f)]))

(define (sm-list mgr)
  (cond
    [(persistent-session-manager? mgr)
     (define base (persistent-session-manager-base-dir mgr))
     (if (directory-exists? base)
         (for/list ([d (in-list (directory-list base))]
                    #:when (directory-exists? (build-path base d)))
           (path->string d))
         '())]
    [(in-memory-session-manager? mgr) (in-memory-list-sessions mgr)]
    [else (raise-session-error (format "not a session manager: ~a" mgr) #f)]))

(define (sm-fork! mgr src-id dest-id [entry-id #f])
  (cond
    [(persistent-session-manager? mgr)
     (define base (persistent-session-manager-base-dir mgr))
     (define src-path (build-path base src-id "session.jsonl"))
     (define dest-dir (build-path base dest-id))
     (make-directory* dest-dir)
     (define dest-path (build-path dest-dir "session.jsonl"))
     (fork-session! src-path entry-id dest-path)]
    [(in-memory-session-manager? mgr) (in-memory-fork! mgr src-id dest-id entry-id)]
    [else (raise-session-error (format "not a session manager: ~a" mgr) #f)]))
