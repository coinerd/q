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
         "session-repository.rkt"
         (only-in "../../util/message/message.rkt" message?)
         (only-in "../../util/error/errors.rkt" raise-session-error)
         "session-path.rkt")

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
                        (->* (in-memory-session-manager? string? string?) (string?) string?)])
         persistent-session-manager-base-dir
         persistent-session-manager-repository)

;; ============================================================
;; Persistent session manager — wraps file-backed session-store
;; ============================================================

(struct persistent-session-manager (base-dir repository) #:transparent)

(define (make-persistent-session-manager base-dir)
  (make-directory* base-dir)
  ;; W3 cutover: open the no-follow root capability. Falls back to #f on any
  ;; failure (unsupported backend / swapped root) so the manager degrades to
  ;; path-based persistence rather than escaping.
  (define repo
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (and (repository-backend-supported?) (make-session-repository base-dir))))
  (persistent-session-manager base-dir repo))

;; Unified dispatch: check type and delegate

(define (session-manager? x)
  (or (persistent-session-manager? x) (in-memory-session-manager? x)))
(define (validate-manager-session-id! who session-id)
  (unless (valid-session-id? session-id)
    (raise-arguments-error who
                           "expected a nonempty one-component session identifier"
                           "session-id"
                           session-id)))

(define (sm-append! mgr session-id msg)
  (validate-manager-session-id! 'sm-append! session-id)
  (cond
    [(persistent-session-manager? mgr)
     (define base (persistent-session-manager-base-dir mgr))
     (define repo (persistent-session-manager-repository mgr))
     (cond
       ;; W3 cutover: route through the no-follow repository capability.
       [repo (repository-append-event! repo session-id msg)]
       [else
        (define dir (resolve-session-path base session-id))
        (define log-path (resolve-session-path base session-id "session.jsonl"))
        ;; append-entry! uses this write-ahead sidecar internally; validate it at
        ;; the same containment boundary before any directory or file effect.
        (resolve-session-path base session-id "session.jsonl.pending")
        (make-directory* dir)
        (append-entry! log-path msg)])]
    [(in-memory-session-manager? mgr) (in-memory-append! mgr session-id msg)]
    [else (raise-session-error (format "not a session manager: ~a" mgr) #f)]))

(define (sm-load mgr session-id)
  (validate-manager-session-id! 'sm-load session-id)
  (cond
    [(persistent-session-manager? mgr)
     (define log-path
       (resolve-session-path (persistent-session-manager-base-dir mgr) session-id "session.jsonl"))
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
                    #:do [(define sid (path->string d))]
                    #:when (and (valid-session-id? sid)
                                (with-handlers ([exn:fail? (lambda (_) #f)])
                                  (directory-exists? (resolve-session-path base sid)))))
           sid)
         '())]
    [(in-memory-session-manager? mgr) (in-memory-list-sessions mgr)]
    [else (raise-session-error (format "not a session manager: ~a" mgr) #f)]))

(define (sm-fork! mgr src-id dest-id [entry-id #f])
  (validate-manager-session-id! 'sm-fork! src-id)
  (validate-manager-session-id! 'sm-fork! dest-id)
  (cond
    [(persistent-session-manager? mgr)
     (define base (persistent-session-manager-base-dir mgr))
     (define repo (persistent-session-manager-repository mgr))
     (cond
       [repo (repository-fork-event-log! repo src-id dest-id entry-id)]
       [else
        (define src-path (resolve-session-path base src-id "session.jsonl"))
        (define dest-path (resolve-session-path base dest-id "session.jsonl"))
        ;; fork-session! loads and validates the source before creating the destination.
        (fork-session! src-path entry-id dest-path)])]
    [(in-memory-session-manager? mgr) (in-memory-fork! mgr src-id dest-id entry-id)]
    [else (raise-session-error (format "not a session manager: ~a" mgr) #f)]))
