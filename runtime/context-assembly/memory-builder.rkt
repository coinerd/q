#lang racket/base
;; runtime/context-assembly/memory-builder.rkt — Observe-only memory retrieval for context assembly
;;
;; v0.95.6: Queries memory backend during context assembly but does NOT inject
;; into prompts. This is the observe-only phase that measures retrieval
;; quality/latency without changing model behavior.
;;
;; Design:
;;   - Reads from current-memory-backend parameter (shared with tools)
;;   - Respects config-memory-enabled? gate
;;   - Returns telemetry (count, latency-ms, token-estimate) alongside results
;;   - Backend failures fail closed (empty results, logged telemetry)
;;   - Token estimate uses ~4 chars/token heuristic
;;   - All output is jsexpr-safe (numbers, strings, booleans only)

(require racket/match
         "../../runtime/memory/types.rkt"
         "../../runtime/memory/protocol.rkt"
         (only-in "../../tools/builtins/memory-tools.rkt" current-memory-backend)
         (only-in "../../runtime/session/session-config.rkt" config-memory-enabled?))

;; ---------------------------------------------------------------------------
;; Feature flags
;; ---------------------------------------------------------------------------

;; Observe-only mode: query memory but don't inject into prompts.
;; Default #t when memory is enabled — this is always safe (no prompt mutation).
(define current-memory-observe-mode? (make-parameter #t))

;; Token budget for memory retrieval (observe-only doesn't enforce, just reports)
(define current-memory-token-budget (make-parameter 500))

;; Timeout for memory retrieval in milliseconds
(define current-memory-retrieval-timeout-ms (make-parameter 2000))

;; ---------------------------------------------------------------------------
;; Telemetry struct
;; ---------------------------------------------------------------------------

(struct memory-telemetry
        (retrieved-count ; exact-nonnegative-integer
         latency-ms ; exact-nonnegative-integer (0 if no query)
         token-estimate ; exact-nonnegative-integer
         backend-available? ; boolean
         timed-out? ; boolean
         error-message ; (or/c string? #f)
         )
  #:transparent)

;; ---------------------------------------------------------------------------
;; Token estimation
;; ---------------------------------------------------------------------------

;; Rough 4 chars/token estimate (conservative)
(define (estimate-tokens text)
  (if (and (string? text) (> (string-length text) 0))
      (quotient (string-length text) 4)
      0))

(define (estimate-tokens-for-items items)
  (for/sum ([item items]) (estimate-tokens (memory-item-content item))))

;; ---------------------------------------------------------------------------
;; Internal: run retrieval and process result
;; ---------------------------------------------------------------------------

(define (process-retrieval-result qr start-ms)
  (match qr
    [(memory-result #t items _ _)
     (define tok-est (estimate-tokens-for-items items))
     (define latency (inexact->exact (round (- (current-inexact-milliseconds) start-ms))))
     (cons items (memory-telemetry (length items) latency tok-est #t #f #f))]
    [(memory-result #f _ err _)
     (define latency (inexact->exact (round (- (current-inexact-milliseconds) start-ms))))
     (define err-msg
       (if (hash? err)
           (hash-ref err 'message (format "~a" err))
           (format "~a" err)))
     (cons '() (memory-telemetry 0 latency 0 #t #f err-msg))]
    [_ (cons '() (memory-telemetry 0 0 0 #t #f "unexpected result type"))]))

;; ---------------------------------------------------------------------------
;; Observe-only retrieval
;; ---------------------------------------------------------------------------

;; Retrieve memory for context assembly, returning results + telemetry.
;; Does NOT modify prompts. Returns (cons items telemetry).
;; Session-config must be provided to check enabled flag.
(define (observe-memory-for-context session-config
                                    #:scope [scope 'session]
                                    #:session-id [session-id #f]
                                    #:project [project #f]
                                    #:limit [limit 20])
  (define enabled? (and session-config (config-memory-enabled? session-config)))
  (define backend (current-memory-backend))
  (cond
    ;; Memory disabled — return empty with telemetry showing disabled
    [(not enabled?) (cons '() (memory-telemetry 0 0 0 #f #f #f))]
    ;; No backend configured
    [(not backend) (cons '() (memory-telemetry 0 0 0 #f #f "no backend configured"))]
    [else
     ;; Try retrieval with timing
     (define start-ms (current-inexact-milliseconds))
     (with-handlers ([exn:fail? (lambda (e)
                                  (cons '()
                                        (memory-telemetry
                                         0
                                         (inexact->exact (round (- (current-inexact-milliseconds)
                                                                   start-ms)))
                                         0
                                         #t
                                         #f
                                         (exn-message e))))])
       (define query
         (memory-query #f ; text (no text search in observe)
                       scope
                       project ; project-root
                       session-id
                       #f ; types (all types)
                       #f ; tags (all tags)
                       limit
                       #f)) ; include-expired?
       (define qr (gen:retrieve-memory backend query))
       (process-retrieval-result qr start-ms))]))

;; ---------------------------------------------------------------------------
;; Pure observe: returns telemetry only, discards items
;; ---------------------------------------------------------------------------

(define (observe-memory-telemetry session-config
                                  #:scope [scope 'session]
                                  #:session-id [session-id #f]
                                  #:project [project #f]
                                  #:limit [limit 20])
  (define result
    (observe-memory-for-context session-config
                                #:scope scope
                                #:session-id session-id
                                #:project project
                                #:limit limit))
  (cdr result))

;; ---------------------------------------------------------------------------
;; Telemetry -> jsexpr conversion
;; ---------------------------------------------------------------------------

(define (memory-telemetry->jsexpr tel)
  (hasheq 'retrieved_count
          (memory-telemetry-retrieved-count tel)
          'latency_ms
          (memory-telemetry-latency-ms tel)
          'token_estimate
          (memory-telemetry-token-estimate tel)
          'backend_available
          (memory-telemetry-backend-available? tel)
          'timed_out
          (memory-telemetry-timed-out? tel)
          'error_message
          (memory-telemetry-error-message tel)))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

;; Feature flags
(provide current-memory-observe-mode?
         current-memory-token-budget
         current-memory-retrieval-timeout-ms
         ;; Telemetry
         (struct-out memory-telemetry)
         memory-telemetry->jsexpr
         ;; Retrieval
         observe-memory-for-context
         observe-memory-telemetry
         ;; Token estimation
         estimate-tokens
         estimate-tokens-for-items)
