#lang racket/base

;; examples/extensions/session-state.rkt — persistent state across sessions (#1216)
;;
;; Demonstrates how to persist extension state across sessions using
;; custom entries in the session store. On session load, reconstructs
;; state from previous entries. On turn end, saves state.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/session-state.rkt")
;;
;; Note: Access session store via (ctx-session-store ctx) in hook handlers.
;; Use append-custom-entry! to save and load-custom-entries to retrieve.

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; The extension tracks a counter and last-seen timestamp across sessions.
;; State shape: (hasheq 'counter N 'last-session-id "..." 'total-turns M)

;; Reconstruct state from custom entries.
;; In production: (load-custom-entries mgr session-id "session-state")
(define (reconstruct-state entries)
  (for/fold ([state (hasheq 'counter 0 'total-turns 0)])
            ([entry (in-list entries)])
    (define data (hash-ref entry 'data (hasheq)))
    (hasheq 'counter (+ (hash-ref state 'counter 0)
                        (hash-ref data 'increment 1))
            'total-turns (+ (hash-ref state 'total-turns 0) 1)
            'last-session-id (hash-ref data 'session-id "unknown"))))

(define the-extension
  (extension "session-state"
             "1.0.0"
             "1"
             (hasheq 'session.loaded
                     (lambda (payload)
                       ;; payload has: session-id
                       ;; In production:
                       ;;   (define store (ctx-session-store ctx))
                       ;;   (define entries (load-custom-entries store session-id "session-state"))
                       ;;   (define state (reconstruct-state entries))
                       (define session-id (hash-ref payload 'session-id "new"))
                       (log-info (format "session-state: Loading state for session ~a" session-id))
                       (hook-pass payload))

                     'turn-end
                     (lambda (payload)
                       ;; Save state at end of each turn.
                       ;; In production:
                       ;;   (define store (ctx-session-store ctx))
                       ;;   (define session-id (hash-ref payload 'session-id))
                       ;;   (append-custom-entry! store session-id "session-state" "turn"
                       ;;     (hasheq 'increment 1 'session-id session-id))
                       (log-info "session-state: Saving turn entry")
                       (hook-pass payload))

                     'session.created
                     (lambda (payload)
                       ;; Initialize state for brand new sessions
                       (log-info "session-state: New session initialized")
                       (hook-pass payload)))))

;; Key concepts:
;;   1. append-custom-entry! stores keyed data: (mgr session-id ext-name key data)
;;   2. load-custom-entries retrieves all or filtered: (mgr session-id ext-name [key])
;;   3. Entries are namespaced by extension name — no collisions between extensions
;;   4. Reconstruct complex state by folding over entry history
;;   5. Access the session store via (ctx-session-store ctx) in hook handlers
;;   6. Use 'session.loaded for state restoration, 'turn-end for incremental saves
