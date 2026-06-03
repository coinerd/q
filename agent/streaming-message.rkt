#lang racket/base

;; agent/streaming-message.rkt — FEAT-71: Structured streaming accumulator
;; STABILITY: stable
;;
;; Replaces the fragile (box "") / (box '()) mutable streaming state in loop.rkt
;; with a structured streaming-message accumulator that encapsulates all streaming
;; state in one place.
;;
;; v0.47.3: Made opaque — callers use exported functions only, not struct fields.
;; v0.80.5 (T3-1): Replaced 5 boxes with #:mutable struct fields. Cleaner API,
;;   fewer allocations, no unbox/set-box! noise.

(require racket/contract
         racket/list
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part make-tool-call-part))

;; ============================================================
;; Struct (opaque — not exported via struct-out)
;; ============================================================

;; A structured accumulator for streaming LLM responses.
;; Uses #:mutable fields instead of boxes for clean state updates.
(struct streaming-message
        (text ; string — accumulated text
         tool-calls ; (listof tool-call-delta) — accumulated tool calls
         thinking ; string — accumulated thinking/reasoning (FEAT-72)
         chunks ; (listof stream-chunk) — all chunks for replay
         state ; symbol — FSM state: not-started | streaming | done | blocked | cancelled
         message-id ; string — unique message identifier
         )
  #:transparent
  #:mutable)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-streaming-message message-id)
  (streaming-message "" '() "" '() 'not-started message-id))

;; ============================================================
;; Accumulator operations
;; ============================================================

(define (streaming-message-append-text! sm text)
  (set-streaming-message-text! sm (string-append (streaming-message-text sm) text)))

(define (streaming-message-append-tool-call! sm tc)
  (set-streaming-message-tool-calls! sm (cons tc (streaming-message-tool-calls sm))))

(define (streaming-message-append-thinking! sm text)
  (set-streaming-message-thinking! sm (string-append (streaming-message-thinking sm) text)))

(define (streaming-message-append-chunk! sm chunk)
  (set-streaming-message-chunks! sm (cons chunk (streaming-message-chunks sm))))

(define (streaming-message-set-cancelled! sm)
  (set-streaming-message-state! sm 'cancelled))

(define (streaming-message-set-blocked! sm)
  (set-streaming-message-state! sm 'blocked))

(define (streaming-message-set-message-started! sm)
  (set-streaming-message-state! sm 'streaming))

;; FSM state accessor
(define (streaming-message-fsm-state sm)
  (streaming-message-state sm))

;; ============================================================
;; Value accessors
;; ============================================================

(define (streaming-message-get-tool-calls sm)
  (reverse (streaming-message-tool-calls sm)))

;; Backward compat alias — old name for get-tool-calls (returns reversed list)
(define streaming-message-tool-calls* streaming-message-get-tool-calls)

(define (streaming-message-cancelled? sm)
  (eq? (streaming-message-state sm) 'cancelled))

(define (streaming-message-blocked? sm)
  (eq? (streaming-message-state sm) 'blocked))

(define (streaming-message-message-started? sm)
  (and (memq (streaming-message-state sm) '(streaming done blocked cancelled)) #t))

;; ============================================================
;; Finalization
;; ============================================================

(define (streaming-message-finalize sm)
  (define text (streaming-message-text sm))
  (define tool-calls (streaming-message-get-tool-calls sm))
  (define content-parts
    (append (if (string=? text "")
                '()
                (list (make-text-part text)))
            (for/list ([tc (in-list tool-calls)])
              (make-tool-call-part (or (hash-ref tc 'id #f) "")
                                   (hash-ref tc 'name "unknown")
                                   (hash-ref tc 'arguments (hasheq))))))
  (make-message (streaming-message-message-id sm)
                #f ; parent-id
                'assistant
                'text
                content-parts
                (current-seconds)
                (hasheq 'cancelled?
                        (streaming-message-cancelled? sm)
                        'stream-blocked?
                        (streaming-message-blocked? sm)
                        'thinking
                        (streaming-message-thinking sm))))

(define (streaming-message->hash sm)
  (hasheq 'text
          (streaming-message-text sm)
          'tool-calls
          (streaming-message-get-tool-calls sm)
          'thinking
          (streaming-message-thinking sm)
          'all-chunks
          (streaming-message-chunks sm)
          'cancelled?
          (streaming-message-cancelled? sm)
          'stream-blocked?
          (streaming-message-blocked? sm)))

;; ============================================================
;; Provide — explicit exports, no struct-out (v0.47.3)
;; ============================================================

;; Predicate
(provide streaming-message?
         ;; Constructor
         (contract-out [make-streaming-message (-> string? streaming-message?)])
         ;; Accumulators (mutation)
         (contract-out [streaming-message-append-text! (-> streaming-message? string? void?)]
                       [streaming-message-append-tool-call! (-> streaming-message? hash? void?)]
                       [streaming-message-append-thinking! (-> streaming-message? string? void?)]
                       [streaming-message-append-chunk! (-> streaming-message? any/c void?)]
                       [streaming-message-set-cancelled! (-> streaming-message? void?)]
                       [streaming-message-set-blocked! (-> streaming-message? void?)]
                       [streaming-message-set-message-started! (-> streaming-message? void?)])
         ;; Read accessors
         (contract-out [streaming-message-fsm-state (-> streaming-message? symbol?)]
                       [streaming-message-text (-> streaming-message? string?)]
                       [streaming-message-get-tool-calls (-> streaming-message? (listof any/c))]
                       [streaming-message-thinking (-> streaming-message? string?)]
                       [streaming-message-chunks (-> streaming-message? (listof any/c))]
                       [streaming-message-cancelled? (-> streaming-message? boolean?)]
                       [streaming-message-blocked? (-> streaming-message? boolean?)]
                       [streaming-message-message-started? (-> streaming-message? boolean?)]
                       [streaming-message-message-id (-> streaming-message? string?)])
         ;; Finalization
         (contract-out [streaming-message-finalize (-> streaming-message? any/c)]
                       [streaming-message->hash (-> streaming-message? hash?)])
         ;; Backward compat — same as get-tool-calls (returns reversed list)
         streaming-message-tool-calls*)
