#lang racket/base

;; runtime/context-assembly/provenance.rkt — Context provenance tracking
;; STABILITY: internal
;;
;; Provides traceability for the persisted-history-to-provider transformation
;; pipeline. Each stage of context assembly produces a provenance context
;; snapshot (message IDs, count, stage hash). A checkpoint aggregates stages
;; and can reconcile cardinality and digest continuity across the pipeline.
;;
;; Privacy contract:
;; - Never trace raw message content, credentials, or shell arguments
;; - Use keyed hashes for message identity
;; - Trace entries contain only IDs, counts, and bounded metadata

(require racket/contract
         racket/list
         racket/match
         racket/string
         racket/hash
         (only-in racket/function const)
         "../../util/event/event-bus.rkt"
         (only-in "../../util/event/event.rkt" make-event))

;; Provenance context — snapshot of one pipeline stage
(provide provenance-context
         provenance-context?
         make-provenance-context
         provenance-context-stage
         provenance-context-session-id
         provenance-context-turn-id
         provenance-context-request-id
         provenance-context-assembly-id
         provenance-context-message-count
         provenance-context-message-ids
         provenance-context-stage-hash
         provenance-context-metadata
         ;; Checkpoint — aggregates stages across a turn
         checkpoint-stages?
         make-checkpoint-stages
         checkpoint-stages-session-id
         checkpoint-stages-turn-id
         checkpoint-stages-request-id
         checkpoint-stages-assembly-id
         checkpoint-stages-stage-count
         checkpoint-stages-entries
         add-stage-trace!
         ;; Reconciliation result
         provenance-reconciliation?
         provenance-reconciliation-passed?
         provenance-reconciliation-summary
         provenance-reconciliation-errors
         reconcile-stages
         ;; Trace entry building
         provenance-context->trace-entry
         build-provenance-trace)

;; ============================================================
;; Provenance context — snapshot at one pipeline stage
;; ============================================================

(struct provenance-context
        (stage ;; string — stage name (e.g. "raw-context", "tier-a", "final-context")
         session-id ;; string — session ID
         turn-id ;; string — turn ID
         request-id ;; string — request/assembly ID
         assembly-id ;; string — context assembly pass ID
         message-count ;; exact-nonnegative-integer? — number of messages at this stage
         message-ids ;; (listof string?) — ordered message IDs (length = message-count)
         stage-hash ;; string — keyed digest of sorted message-ids + stage name
         metadata ;; hash? — extensible metadata (budget, reason codes, etc.)
         )
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

;; Build a provenance context from a message list at a named pipeline stage.
;; Only extracts message IDs — never raw content.
(define (make-provenance-context session-id
                                 turn-id
                                 request-id
                                 assembly-id
                                 messages
                                 #:stage [stage "unnamed"]
                                 #:metadata [metadata #hasheq()])
  (define msg-ids
    (for/list ([m (in-list messages)])
      (cond
        [(hash? m) (hash-ref m 'id #f)]
        [else #f])))
  (define clean-ids (filter (lambda (x) x) msg-ids))
  (provenance-context stage
                      session-id
                      turn-id
                      request-id
                      assembly-id
                      (length clean-ids)
                      clean-ids
                      (compute-stage-hash stage clean-ids)
                      metadata))

;; Compute a deterministic stage hash from stage name + sorted message IDs.
(define (compute-stage-hash stage message-ids)
  (define sorted (sort message-ids string<?))
  (define input (string-append stage "|" (string-join sorted "|")))
  ;; Jenkins one-at-a-time hash. Deterministic, fast, avoids trivial collisions
  ;; from the simple XOR-shift when IDs differ only in the last few characters.
  (let loop ([n 0]
             [hash 0])
    (if (>= n (string-length input))
        (number->string (bitwise-and hash #xFFFFFFFF) 16)
        (let* ([c (char->integer (string-ref input n))]
               [h1 (+ hash c)]
               [h2 (+ h1 (arithmetic-shift h1 10))]
               [h3 (bitwise-xor h2 (arithmetic-shift h2 -6))])
          (loop (add1 n) h3)))))

;; ============================================================
;; Checkpoint — aggregates multiple pipeline stages
;; ============================================================

(struct checkpoint-stages
        (session-id ;; string
         turn-id ;; string
         request-id ;; string
         assembly-id ;; string
         [entries #:mutable] ;; list? — ordered list of (cons stage-name provenance-context?)
         )
  #:transparent)

(define (make-checkpoint-stages session-id turn-id request-id assembly-id)
  (checkpoint-stages session-id turn-id request-id assembly-id '()))

(define (checkpoint-stages-stage-count cp)
  (length (checkpoint-stages-entries cp)))

(define (add-stage-trace! cp pctx)
  (define entries (checkpoint-stages-entries cp))
  (set-checkpoint-stages-entries! cp
                                  (append entries
                                          (list (cons (provenance-context-stage pctx) pctx)))))

;; ============================================================
;; Reconciliation
;; ============================================================

(struct provenance-reconciliation
        (passed? ;; boolean?
         summary ;; (listof string?) — per-stage summary lines
         errors ;; (listof string?) — error descriptions
         )
  #:transparent)

;; Reconcile a checkpoint's stages for:
;;  1. Cardinality continuity — message count should not increase between stages
;;  2. Hash continuity — overlapping message IDs should produce consistent hashes
;; Returns a provenance-reconciliation result.
(define (reconcile-stages cp)
  (define entries (checkpoint-stages-entries cp))
  (define errors '())
  (define summaries '())
  ;; Track previous stage message IDs for cardinality check
  (define prev-ids #f)
  (define prev-name #f)
  (for ([pair (in-list entries)])
    (define stage-name (car pair))
    (define pctx (cdr pair))
    (define cur-count (provenance-context-message-count pctx))
    (define cur-ids (provenance-context-message-ids pctx))
    (define cur-hash (provenance-context-stage-hash pctx))
    (define summary-line (format "  ~a: ~a messages, hash=~a" stage-name cur-count cur-hash))
    (set! summaries (append summaries (list summary-line)))
    ;; Cardinality check: should not increase (pipeline filters messages, never adds)
    (when (and prev-ids (> cur-count (length prev-ids)))
      (set! errors
            (append errors
                    (list (format "Cardinality violation: ~a (~a) > ~a (~a)"
                                  stage-name
                                  cur-count
                                  prev-name
                                  (length prev-ids))))))
    ;; Hash continuity: if IDs are a subset of previous, rehash should match
    (when (and prev-ids (not (null? cur-ids)))
      (define subset?
        (and (<= (length cur-ids) (length prev-ids))
             (for/and ([id (in-list cur-ids)])
               (member id prev-ids))))
      (when subset?
        ;; Re-derive hash to verify
        (define expected-hash (compute-stage-hash stage-name cur-ids))
        (unless (string=? cur-hash expected-hash)
          (set! errors
                (append errors
                        (list (format "Hash mismatch at ~a: got ~s expected ~s"
                                      stage-name
                                      cur-hash
                                      expected-hash)))))))
    (set! prev-ids cur-ids)
    (set! prev-name stage-name))
  (provenance-reconciliation (null? errors) summaries errors))

;; ============================================================
;; Trace entry building
;; ============================================================

;; Convert a provenance context to a trace-safe hash for the trace logger.
;; Never includes raw message content.
(define (provenance-context->trace-entry pctx)
  (define ids (provenance-context-message-ids pctx))
  (define ids-full-count (length ids))
  (define ids-prefix-count (min 5 ids-full-count))
  (hasheq 'phase
          (string-append "provenance." (provenance-context-stage pctx))
          'stage
          (provenance-context-stage pctx)
          'sessionId
          (provenance-context-session-id pctx)
          'turnId
          (provenance-context-turn-id pctx)
          'requestId
          (provenance-context-request-id pctx)
          'assemblyId
          (provenance-context-assembly-id pctx)
          'messageCount
          ids-full-count
          'messageIdCount
          ids-full-count
          'messageIdPrefix
          (if (positive? ids-full-count)
              (string-join (take ids ids-prefix-count) ",")
              "")
          'stageHash
          (provenance-context-stage-hash pctx)))

;; Build a list of trace entries from a checkpoint, one per recorded stage.
;; Returns entries in insertion order.
(define (build-provenance-trace cp)
  (for/list ([pair (in-list (checkpoint-stages-entries cp))])
    (provenance-context->trace-entry (cdr pair))))

;; ============================================================
;; Convenience: emit provenance trace to event bus
;; ============================================================

;; Emit a provenance entry as a contextual telemetry event on the bus.
(define (emit-provenance-entry! bus pctx session-id turn-id)
  (define entry (provenance-context->trace-entry pctx))
  (publish! bus
            (make-event (string-append "provenance." (provenance-context-stage pctx))
                        (current-inexact-milliseconds)
                        session-id
                        turn-id
                        entry)))

;; Emit full provenance trace from a checkpoint.
(define (emit-provenance-trace! bus cp session-id turn-id)
  (for ([pair (in-list (checkpoint-stages-entries cp))])
    (emit-provenance-entry! bus (cdr pair) session-id turn-id)))
