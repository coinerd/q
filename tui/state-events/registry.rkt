#lang racket/base

;; tui/state-events/registry.rkt -- Event reducer registry
;;
;; Thread-safe registry for event-type -> handler mappings.
;; W-07: Registry-based event reducers replace monolithic case dispatch.

(require racket/string
         racket/match
         racket/list
         (only-in "../../util/event/event.rkt" event event-ev event?))

(provide apply-event-to-state
         register-event-reducer!
         call-with-test-registry
         event-reducer-registered?
         current-event-reducers
         get-event-reducers)

;; ============================================================
;; Event reducer registry
;; ============================================================

;; Mutable hash for registration-time population.
;; Thread-safe: registration guarded by semaphore.
(define event-reducers (make-hash))
(define event-reducers-lock (make-semaphore 1))

;; Parameter-based registry for test isolation.
;; When set, tests use a fresh hash instead of the global registry.
;; Write-once registration — re-registering an existing type is a no-op.
(define current-event-reducers (make-parameter #f))

(define (get-event-reducers)
  (or (current-event-reducers) event-reducers))

(define (call-with-test-registry thunk)
  (parameterize ([current-event-reducers (make-hash)])
    (thunk)))

(define (register-event-reducer! type-string handler)
  ;; Write-once: re-registering an existing type is a no-op.
  (call-with-semaphore event-reducers-lock
                       (lambda ()
                         (unless (hash-has-key? (get-event-reducers) type-string)
                           (hash-set! (get-event-reducers) type-string handler)))))

(define (event-reducer-registered? type-string)
  (call-with-semaphore event-reducers-lock
                       (lambda () (hash-has-key? (get-event-reducers) type-string))))

(define (apply-event-to-state state evt)
  (define ev (event-ev evt))
  (define handler
    (call-with-semaphore event-reducers-lock (lambda () (hash-ref (get-event-reducers) ev #f))))
  (if handler
      (handler state evt)
      state))
