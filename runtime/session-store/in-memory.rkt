#lang racket/base

;; runtime/session-store/in-memory.rkt — In-memory session manager
;; Extracted from session-store.rkt (W3 v0.72.5)

(require racket/match
         "../../util/protocol-types.rkt")

(provide in-memory-session-manager?
         in-memory-session-manager
         make-in-memory-session-manager
         in-memory-append!
         in-memory-append-entries!
         in-memory-load
         in-memory-list-sessions
         in-memory-fork!
         append-custom-entry!
         load-custom-entries)

;; ============================================================
;; In-memory session manager (GC-18)
;; ============================================================

(struct in-memory-session-manager (sessions-box))

(define (make-in-memory-session-manager)
  (in-memory-session-manager (box (hash))))

(define (in-memory-append! mgr session-id msg)
  (define box (in-memory-session-manager-sessions-box mgr))
  (define sessions (unbox box))
  (define existing (hash-ref sessions session-id '()))
  ;; RA-17: prepend (O(1)) instead of append (O(n)) to avoid quadratic growth
  (set-box! box (hash-set sessions session-id (cons msg existing))))

(define (in-memory-append-entries! mgr session-id msgs)
  (for ([msg (in-list msgs)])
    (in-memory-append! mgr session-id msg)))

(define (in-memory-load mgr session-id)
  (define sessions (unbox (in-memory-session-manager-sessions-box mgr)))
  ;; Reverse to restore chronological order (entries are stored newest-first)
  (reverse (hash-ref sessions session-id '())))

(define (in-memory-list-sessions mgr)
  (hash-keys (unbox (in-memory-session-manager-sessions-box mgr))))

(define (in-memory-fork! mgr src-id dest-id [entry-id #f])
  (define entries (in-memory-load mgr src-id))
  (define to-copy
    (if entry-id
        (let loop ([es entries]
                   [acc '()])
          (match es
            ['() (reverse acc)]
            [(cons (? (lambda (e) (equal? (message-id e) entry-id)) found) _)
             (reverse (cons found acc))]
            [(cons e rest) (loop rest (cons e acc))]))
        entries))
  (define box (in-memory-session-manager-sessions-box mgr))
  (define sessions (unbox box))
  ;; Store in reverse-chronological order to match internal representation
  (set-box! box (hash-set sessions dest-id (reverse to-copy)))
  dest-id)

;; ============================================================
;; Custom entry helpers (#1147)
;; ============================================================

(define (append-custom-entry! mgr session-id extension-name key data)
  (define entry (make-custom-entry extension-name key data))
  (in-memory-append! mgr session-id entry))

(define (load-custom-entries mgr session-id extension-name [key #f])
  (define entries (in-memory-load mgr session-id))
  (filter (lambda (e)
            (and (custom-entry? e)
                 (equal? (custom-entry-extension e) extension-name)
                 (or (not key) (equal? (custom-entry-key e) key))))
          entries))
