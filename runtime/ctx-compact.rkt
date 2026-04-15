#lang racket/base

;; runtime/ctx-compact.rkt — Programmatic Compaction from Extensions (#708-#709)
;;
;; Allows extensions to trigger compaction with custom summarization
;; instructions, on-complete/on-error callbacks, and rate limiting.
;;
;; Provides:
;;   ctx-compact          — trigger compaction with custom instructions
;;   ctx-compact?         — predicate for compact request
;;   rate-limit-allowed?  — check if compaction is rate-limited
;;   reset-rate-limit!    — reset the rate limit counter

(require racket/contract
         racket/match
         racket/list
         "../agent/event-bus.rkt"
         "../runtime/compactor.rkt"
         "../runtime/compaction-hooks.rkt"
         "../util/protocol-types.rkt")

(provide
 ;; Compact request struct
 (struct-out ctx-compact-request)
 ctx-compact?

 ;; Main API
 ctx-compact
 ctx-compact-async

 ;; Rate limiting
 rate-limit-allowed?
 reset-rate-limit!
 current-compact-rate-limit
 current-compact-rate-window

 ;; Callbacks
 compact-callback-result
 compact-callback-result?
 compact-callback-result-success?
 compact-callback-result-summary
 compact-callback-result-error
 compact-callback-result-removed-count)

;; ============================================================
;; Parameters
;; ============================================================

;; Maximum compaction requests per window (default 5)
(define current-compact-rate-limit (make-parameter 5))

;; Rate limit window in seconds (default 60)
(define current-compact-rate-window (make-parameter 60))

;; ============================================================
;; Structs
;; ============================================================

;; A compaction request from an extension
(struct ctx-compact-request
  (instructions       ; string or #f — custom summarization instructions
   on-complete        ; (or/c procedure? #f) — called with compact-callback-result on success
   on-error           ; (or/c procedure? #f) — called with compact-callback-result on failure
   requested-at       ; exact-positive-integer — timestamp
   )
  #:transparent)

;; Shorter alias
(define ctx-compact? ctx-compact-request?)

;; Result passed to callbacks
(struct compact-callback-result
  (success?        ; boolean
   summary         ; string or #f
   error           ; string or #f
   removed-count   ; integer
   )
  #:transparent)

;; ============================================================
;; Rate limiting state
;; ============================================================

(define rate-limit-timestamps (box '()))

;; Check if a new compaction request is allowed within rate limit.
(define (rate-limit-allowed?)
  (define now (current-seconds))
  (define window (current-compact-rate-window))
  (define limit (current-compact-rate-limit))
  (define recent
    (filter (lambda (ts) (> (+ ts window) now))
            (unbox rate-limit-timestamps)))
  (< (length recent) limit))

;; Record a compaction request for rate limiting.
(define (record-rate-limit!)
  (define now (current-seconds))
  (define window (current-compact-rate-window))
  (define recent
    (filter (lambda (ts) (> (+ ts window) now))
            (unbox rate-limit-timestamps)))
  (set-box! rate-limit-timestamps (cons now recent)))

;; Reset the rate limit counter (for testing or admin override).
(define (reset-rate-limit!)
  (set-box! rate-limit-timestamps '()))

;; ============================================================
;; Main API
;; ============================================================

;; Trigger compaction synchronously with custom instructions.
;; Returns compact-callback-result.
(define (ctx-compact messages
                      strategy
                      #:instructions [instructions #f]
                      #:bus [bus #f]
                      #:session-id [session-id #f]
                      #:on-complete [on-complete #f]
                      #:on-error [on-error #f]
                      #:provider [provider #f]
                      #:hook-dispatcher [hook-dispatcher #f])
  ;; Check rate limit
  (if (not (rate-limit-allowed?))
      (let ([result (compact-callback-result #f #f
                       (format "rate limited: max ~a compactions per ~a seconds"
                               (current-compact-rate-limit)
                               (current-compact-rate-window))
                       0)])
        (when on-error (on-error result))
        result)
      (begin
        (record-rate-limit!)
        (perform-compact messages strategy instructions bus session-id
                         on-complete on-error provider hook-dispatcher))))

  ;; Internal compaction logic (after rate limit check)
(define (perform-compact messages strategy instructions bus session-id
                          on-complete on-error provider hook-dispatcher)
  ;; Publish compaction start event
  (when bus
    (publish-compaction-start! bus 'extension
                                (length messages)
                                0  ;; tokens-before (estimated)
                                session-id
                                "extension-triggered"))

  ;; Attempt compaction
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define result (compact-callback-result #f #f
                                      (exn-message e) 0))
                     (when bus
                       (publish-compaction-end! bus 'extension 0 0 0
                                                 session-id "extension-triggered"
                                                 #:summary-generated? #f))
                     (when on-error (on-error result))
                     result)])
    ;; Build enriched payload for the hook
    (define enriched-payload
      (build-enriched-compact-payload messages strategy
                                       #:session-id session-id))

    ;; Dispatch enriched before-compact hook
    (define-values (hook-res enriched)
      (dispatch-enriched-before-compact hook-dispatcher messages strategy
                                         #:session-id session-id))

    ;; Check if hook provides custom summary
    (define custom-summary (maybe-use-custom-summary hook-res))

    ;; Use custom summary or fall back to strategy-based compaction
    (define result
      (if custom-summary
          ;; Use extension-provided summary directly
          (compaction-result
           (make-message (format "summary-~a" (current-milliseconds))
                         #f 'system 'text
                         (list (make-text-part custom-summary))
                         (current-seconds)
                         (hasheq 'type 'compaction-summary
                                 'source 'extension
                                 'instructions instructions))
           (max 0 (- (length messages) (compaction-strategy-keep-recent-count strategy)))
           (take (reverse messages)
                 (min (compaction-strategy-keep-recent-count strategy)
                      (length messages))))
          ;; Use standard compaction
          (compact-history messages #:strategy strategy)))

    ;; Publish compaction end event
    (when bus
      (publish-compaction-end! bus 'extension
                                (compaction-result-removed-count result)
                                0  ;; tokens-before
                                0  ;; tokens-after
                                session-id "extension-triggered"
                                #:summary-generated? #t))

    ;; Build callback result
    (define cb-result
      (compact-callback-result #t
                                (or custom-summary
                                    (format "compacted ~a messages"
                                            (compaction-result-removed-count result)))
                                #f
                                (compaction-result-removed-count result)))

    (when on-complete (on-complete cb-result))
    cb-result))

;; Trigger compaction asynchronously (returns immediately).
;; Callbacks are invoked on completion/error.
(define (ctx-compact-async messages
                            strategy
                            #:instructions [instructions #f]
                            #:bus [bus #f]
                            #:session-id [session-id #f]
                            #:on-complete [on-complete #f]
                            #:on-error [on-error #f]
                            #:provider [provider #f]
                            #:hook-dispatcher [hook-dispatcher #f])
  (thread
   (lambda ()
     (ctx-compact messages strategy
                   #:instructions instructions
                   #:bus bus
                   #:session-id session-id
                   #:on-complete on-complete
                   #:on-error on-error
                   #:provider provider
                   #:hook-dispatcher hook-dispatcher))))
