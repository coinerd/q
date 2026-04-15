#lang racket/base

;; extensions/events.rkt — Extension-friendly event bus API (#685)
;;
;; Provides convenience wrappers for extensions to subscribe to and
;; publish events on the shared event bus, with per-extension
;; subscription tracking and auto-cleanup.
;;
;; Extensions receive the event bus through their extension context
;; (ctx-event-bus). These wrappers add:
;;   - Automatic subscription tracking per extension name
;;   - Cleanup when an extension is unloaded
;;   - Filtered subscription support
;;   - Safe publish with error isolation

(require racket/contract
         "../agent/event-bus.rkt"
         "../agent/types.rkt"
         "api.rkt")

(provide
 (contract-out
  [ext-subscribe!   (->* (event-bus? string? procedure?)
                         (#:filter (or/c procedure? #f))
                         exact-nonnegative-integer?)]
  [ext-publish!     (-> event-bus? any/c any/c)]
  [ext-unsubscribe! (-> event-bus? string? exact-nonnegative-integer? void?)])
 ;; Bulk cleanup
 ext-unsubscribe-all!
 ;; Query
 ext-subscription-ids)

;; ============================================================
;; Per-extension subscription tracking
;; ============================================================

;; Maps extension-name -> (listof sub-id)
;; Used to auto-cleanup when an extension is unloaded.
(define ext-subscriptions (make-hash))
(define ext-subscriptions-sem (make-semaphore 1))

;; Track a subscription for an extension
(define (track-subscription! ext-name sub-id)
  (call-with-semaphore ext-subscriptions-sem
    (lambda ()
      (define existing (hash-ref ext-subscriptions ext-name '()))
      (hash-set! ext-subscriptions ext-name (cons sub-id existing)))))

;; Remove a subscription from tracking
(define (untrack-subscription! ext-name sub-id)
  (call-with-semaphore ext-subscriptions-sem
    (lambda ()
      (define existing (hash-ref ext-subscriptions ext-name '()))
      (hash-set! ext-subscriptions ext-name
                 (filter (lambda (id) (not (= id sub-id))) existing)))))

;; ============================================================
;; ext-subscribe! : event-bus? string? procedure? #:filter -> sub-id
;; ============================================================

;; Subscribe to the event bus, tracking the subscription under the
;; extension name for auto-cleanup.
;; ext-name: the name of the subscribing extension (for tracking)
(define (ext-subscribe! bus ext-name handler #:filter [filter #f])
  (define sub-id (subscribe! bus handler #:filter filter))
  (track-subscription! ext-name sub-id)
  sub-id)

;; ============================================================
;; ext-publish! : event-bus? event? -> event?
;; ============================================================

;; Publish an event to the bus. Wraps publish! for API clarity.
;; Errors in subscribers are isolated by the event bus itself.
(define (ext-publish! bus evt)
  (publish! bus evt))

;; ============================================================
;; ext-unsubscribe! : event-bus? sub-id -> void?
;; ============================================================

;; ext-unsubscribe! : event-bus? string? sub-id -> void?
;; Unsubscribe from the event bus and remove from tracking.
(define (ext-unsubscribe! bus ext-name sub-id)
  (untrack-subscription! ext-name sub-id)
  (unsubscribe! bus sub-id))

;; ============================================================
;; ext-unsubscribe-all! : event-bus? string? -> void?
;; ============================================================

;; Unsubscribe all tracked subscriptions for an extension.
;; Called during extension unload for cleanup.
(define (ext-unsubscribe-all! bus ext-name)
  (define ids
    (call-with-semaphore ext-subscriptions-sem
      (lambda ()
        (define ids (hash-ref ext-subscriptions ext-name '()))
        (hash-remove! ext-subscriptions ext-name)
        ids)))
  (for ([id (in-list ids)])
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (unsubscribe! bus id))))

;; ============================================================
;; ext-subscription-ids : string? -> (listof exact-nonnegative-integer?)
;; ============================================================

;; Return all tracked subscription IDs for an extension (for testing).
(define (ext-subscription-ids ext-name)
  (call-with-semaphore ext-subscriptions-sem
    (lambda ()
      (hash-ref ext-subscriptions ext-name '()))))
