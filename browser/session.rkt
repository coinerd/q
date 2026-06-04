#lang racket

;; browser/session.rkt — Browser session lifecycle manager
;;
;; Manages browser sessions: creation, lookup, destruction,
;; action counting, and artifact directory management.

(require racket/match
         "types.rkt"
         "../util/error/errors.rkt")

(provide
 ;; Session struct
 browser-session?
 browser-session-id
 browser-session-action-count
 browser-session-created-at
 browser-session-last-activity

 ;; Manager
 make-browser-session-manager
 browser-session-manager-create!
 browser-session-manager-destroy!
 browser-session-manager-get
 browser-session-manager-list
 browser-session-manager-record-action!
 browser-session-manager-count)

;; ---------------------------------------------------------------------------
;; Internal session struct
;; ---------------------------------------------------------------------------

(struct browser-session
  (id              ; string?
   session-info   ; browser-session-info?
   action-count    ; box? (exact-nonnegative-integer?)
   created-at      ; exact-nonnegative-integer? (ms)
   last-activity   ; box? (exact-nonnegative-integer? (ms))
   artifact-dir    ; (or/c string? #f)
   custodian)      ; custodian?
  #:transparent)

;; ---------------------------------------------------------------------------
;; Session manager (stateful)
;; ---------------------------------------------------------------------------

(struct browser-session-manager
  (sessions       ; (hash/c string? browser-session?)
   max-sessions   ; exact-nonnegative-integer?
   max-actions)   ; exact-nonnegative-integer?
  #:transparent)

(define (make-browser-session-manager #:max-sessions [max-sessions 3]
                                       #:max-actions [max-actions 100])
  (browser-session-manager (make-hash) max-sessions max-actions))

;; ---------------------------------------------------------------------------
;; Create session
;; ---------------------------------------------------------------------------

(define (browser-session-manager-create! mgr id info
                                          #:artifact-dir [artifact-dir #f]
                                          #:custodian [custodian (make-custodian)])
  (define sessions (browser-session-manager-sessions mgr))
  (define max-sessions (browser-session-manager-max-sessions mgr))
  (when (>= (hash-count sessions) max-sessions)
    (raise-browser-error
     (format "max browser sessions reached (~a)" max-sessions)
     'session-expired
     (hash 'max max-sessions 'current (hash-count sessions))))
  (when (hash-has-key? sessions id)
    (raise-browser-error
     (format "browser session ~a already exists" id)
     'session-expired
     (hash 'session-id id)))
  (define now (current-milliseconds))
  (define session (browser-session id info (box 0) now (box now) artifact-dir custodian))
  (hash-set! sessions id session)
  session)

;; ---------------------------------------------------------------------------
;; Destroy session
;; ---------------------------------------------------------------------------

(define (browser-session-manager-destroy! mgr id)
  (define sessions (browser-session-manager-sessions mgr))
  (define session (hash-ref sessions id #f))
  (unless session
    (raise-browser-error
     (format "browser session ~a not found" id)
     'session-expired
     (hash 'session-id id)))
  (custodian-shutdown-all (browser-session-custodian session))
  (hash-remove! sessions id))

;; ---------------------------------------------------------------------------
;; Get session
;; ---------------------------------------------------------------------------

(define (browser-session-manager-get mgr id)
  (hash-ref (browser-session-manager-sessions mgr) id #f))

;; ---------------------------------------------------------------------------
;; List sessions
;; ---------------------------------------------------------------------------

(define (browser-session-manager-list mgr)
  (hash-values (browser-session-manager-sessions mgr)))

;; ---------------------------------------------------------------------------
;; Record action + enforce max-actions
;; ---------------------------------------------------------------------------

(define (browser-session-manager-record-action! mgr id)
  (define session (browser-session-manager-get mgr id))
  (unless session
    (raise-browser-error
     (format "browser session ~a not found for action" id)
     'session-expired
     (hash 'session-id id)))
  (define count-box (browser-session-action-count session))
  (define max-actions (browser-session-manager-max-actions mgr))
  (define new-count (add1 (unbox count-box)))
  (when (> new-count max-actions)
    (raise-browser-error
     (format "max actions (~a) exceeded for session ~a" max-actions id)
     'session-expired
     (hash 'session-id id 'max-actions max-actions 'current new-count)))
  (set-box! count-box new-count)
  (set-box! (browser-session-last-activity session) (current-milliseconds))
  new-count)

;; ---------------------------------------------------------------------------
;; Count
;; ---------------------------------------------------------------------------

(define (browser-session-manager-count mgr)
  (hash-count (browser-session-manager-sessions mgr)))
