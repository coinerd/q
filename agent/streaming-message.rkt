#lang racket/base

;; agent/streaming-message.rkt — FEAT-71: Structured streaming accumulator
;; STABILITY: stable
;;
;; Replaces the fragile (box "") / (box '()) mutable streaming state in loop.rkt
;; with a structured streaming-message accumulator that encapsulates all streaming
;; state in one place.
;;
;; v0.47.3: Made opaque — callers use exported functions only, not struct fields.
;; Internal boxes are hidden from external access.

(require racket/contract
         racket/list
         "../util/protocol-types.rkt")

;; ============================================================
;; Struct (opaque — not exported via struct-out)
;; ============================================================

;; A structured accumulator for streaming LLM responses.
;; Replaces the scattered (box ...) pattern in loop.rkt.
(struct streaming-message
        (text-box ; (box string) — accumulated text
         tool-calls-box ; (box (listof tool-call-delta)) — accumulated tool calls
         thinking-box ; (box string) — accumulated thinking/reasoning (FEAT-72)
         chunks-box ; (box (listof stream-chunk)) — all chunks for replay
         state-box ; (box symbol) — FSM state: not-started | streaming | done | blocked | cancelled
         message-id ; string — unique message identifier
         )
  #:transparent) ;; Keep transparent internally for debugging

;; ============================================================
;; Constructor
;; ============================================================

(define (make-streaming-message message-id)
  (streaming-message (box "") (box '()) (box "") (box '()) (box 'not-started) message-id))

;; ============================================================
;; Accumulator operations
;; ============================================================

(define (streaming-message-append-text! sm text)
  (define b (streaming-message-text-box sm))
  (set-box! b (string-append (unbox b) text)))

(define (streaming-message-append-tool-call! sm tc)
  (define b (streaming-message-tool-calls-box sm))
  (set-box! b (cons tc (unbox b))))

(define (streaming-message-append-thinking! sm text)
  (define b (streaming-message-thinking-box sm))
  (set-box! b (string-append (unbox b) text)))

(define (streaming-message-append-chunk! sm chunk)
  (define b (streaming-message-chunks-box sm))
  (set-box! b (cons chunk (unbox b))))

(define (streaming-message-set-cancelled! sm)
  (set-box! (streaming-message-state-box sm) 'cancelled))

(define (streaming-message-set-blocked! sm)
  (set-box! (streaming-message-state-box sm) 'blocked))

(define (streaming-message-set-message-started! sm)
  (set-box! (streaming-message-state-box sm) 'streaming))

;; FSM state accessor
(define (streaming-message-fsm-state sm)
  (unbox (streaming-message-state-box sm)))

;; ============================================================
;; Value accessors (unbox)
;; ============================================================

(define (streaming-message-text sm)
  (unbox (streaming-message-text-box sm)))

(define (streaming-message-tool-calls sm)
  (reverse (unbox (streaming-message-tool-calls-box sm))))

(define (streaming-message-thinking sm)
  (unbox (streaming-message-thinking-box sm)))

(define (streaming-message-chunks sm)
  (unbox (streaming-message-chunks-box sm)))

(define (streaming-message-cancelled? sm)
  (eq? (unbox (streaming-message-state-box sm)) 'cancelled))

(define (streaming-message-blocked? sm)
  (eq? (unbox (streaming-message-state-box sm)) 'blocked))

(define (streaming-message-message-started? sm)
  (and (memq (unbox (streaming-message-state-box sm)) '(streaming done blocked cancelled)) #t))

;; ============================================================
;; Finalization
;; ============================================================

(define (streaming-message-finalize sm)
  (define text (streaming-message-text sm))
  (define tool-calls (streaming-message-tool-calls sm))
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
          (streaming-message-tool-calls sm)
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
                       [streaming-message-tool-calls (-> streaming-message? (listof any/c))]
                       [streaming-message-thinking (-> streaming-message? string?)]
                       [streaming-message-chunks (-> streaming-message? (listof any/c))]
                       [streaming-message-cancelled? (-> streaming-message? boolean?)]
                       [streaming-message-blocked? (-> streaming-message? boolean?)]
                       [streaming-message-message-started? (-> streaming-message? boolean?)]
                       [streaming-message-message-id (-> streaming-message? string?)])
         ;; Finalization
         (contract-out [streaming-message-finalize (-> streaming-message? any/c)]
                       [streaming-message->hash (-> streaming-message? hash?)]))
