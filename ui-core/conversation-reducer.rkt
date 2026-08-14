#lang racket/base

;; q/ui-core/conversation-reducer.rkt — Pure idempotent conversation-artifact reducer
;;
;; STABILITY: internal
;;
;; Normalizes model stream and completion facts into immutable artifact
;; deltas keyed by (session-id, turn-id, artifact-kind), independent of
;; provider event ordering.
;;
;; Identity rule: exactly one reasoning artifact per (session, turn, 'thinking).
;; No duplicates even if model.stream.completed precedes assistant.message.completed.
;;
;; The reducer NEVER discards the live reasoning buffer in a completion event
;; until it has either persisted or deliberately rejected it under policy.
;;
;; Byte limits are enforced ONLY at persistence boundaries, never mid-stream.
;;
;; Telemetry counters are emitted for duplicate/missing/oversized artifacts.
;; Counters are rate-limited and safe for logs (never crash on log failure).

(require racket/contract
         racket/string
         racket/format
         "conversation-artifact.rkt"
         "feature-flags.rkt")

;; ──────────────────────────────────────────────────────
;; Telemetry counters (rate-limited, log-safe)
;; ──────────────────────────────────────────────────────

(define duplicate-count (box 0))
(define missing-count (box 0))
(define oversized-count (box 0))
(define last-telemetry-ms (box 0))
(define telemetry-interval-ms 5000) ; rate-limit: emit at most once per 5 seconds

(define (telemetry-duplicate!)
  (set-box! duplicate-count (add1 (unbox duplicate-count)))
  (maybe-emit-telemetry!))

(define (telemetry-missing!)
  (set-box! missing-count (add1 (unbox missing-count)))
  (maybe-emit-telemetry!))

(define (telemetry-oversized!)
  (set-box! oversized-count (add1 (unbox oversized-count)))
  (maybe-emit-telemetry!))

(define (maybe-emit-telemetry!)
  (define now (current-inexact-milliseconds))
  (when (> (- now (unbox last-telemetry-ms)) telemetry-interval-ms)
    (set-box! last-telemetry-ms now)
    ;; Safe log: never crash if logging fails
    (with-handlers ([exn:fail? void])
      (log-message (current-logger)
                   'info
                   (format "conversation-reducer telemetry: duplicates=~a missing=~a oversized=~a"
                           (unbox duplicate-count)
                           (unbox missing-count)
                           (unbox oversized-count))
                   #f))))

(define (reset-telemetry-counters!)
  (set-box! duplicate-count 0)
  (set-box! missing-count 0)
  (set-box! oversized-count 0)
  (set-box! last-telemetry-ms 0))

;; ──────────────────────────────────────────────────────
;; Artifact store: keyed by (session-id, turn-id, kind)
;;
;; The store is a hash mapping key → conversation-artifact.
;; Only one artifact per (session, turn, 'thinking) is ever created.
;; ──────────────────────────────────────────────────────

(define (make-artifact-key session-id turn-id kind)
  (vector session-id turn-id kind))

(define (artifact-key-session k)
  (vector-ref k 0))
(define (artifact-key-turn k)
  (vector-ref k 1))
(define (artifact-key-kind k)
  (vector-ref k 2))

;; Generate a unique artifact ID from session/turn/kind.
;; Idempotent: same (session, turn, kind) always produces the same ID.
(define (make-artifact-id session-id turn-id kind)
  (format "~a:~a:~a" session-id turn-id kind))

;; ──────────────────────────────────────────────────────
;; Reducer state
;; ──────────────────────────────────────────────────────

;; The reducer state is a hash: key → conversation-artifact
;; plus a set of completed turn-ids to track completion.

(struct reducer-state (artifacts completed-turns) #:transparent)

(define (make-reducer-state)
  (reducer-state (hash) (hash)))

(define (reducer-get rs session-id turn-id kind)
  (hash-ref (reducer-state-artifacts rs) (make-artifact-key session-id turn-id kind) #f))

(define (reducer-put rs art)
  (define key
    (make-artifact-key (conversation-artifact-session-id art)
                       (conversation-artifact-turn-id art)
                       (conversation-artifact-kind art)))
  (reducer-state (hash-set (reducer-state-artifacts rs) key art) (reducer-state-completed-turns rs)))

(define (reducer-mark-turn-completed rs turn-id)
  (reducer-state (reducer-state-artifacts rs)
                 (hash-set (reducer-state-completed-turns rs) turn-id #t)))

(define (reducer-turn-completed? rs turn-id)
  (hash-has-key? (reducer-state-completed-turns rs) turn-id))

;; ──────────────────────────────────────────────────────
;; Get or create an artifact (idempotent — no duplicates)
;; ──────────────────────────────────────────────────────

(define (reducer-get-or-create rs session-id turn-id kind [provider-tag #f])
  (define existing (reducer-get rs session-id turn-id kind))
  (if existing
      (values existing rs)
      (let ([art (make-conversation-artifact #:id (make-artifact-id session-id turn-id kind)
                                             #:turn-id turn-id
                                             #:session-id session-id
                                             #:kind kind
                                             #:provider-tag provider-tag)])
        (values art (reducer-put rs art)))))

;; ──────────────────────────────────────────────────────
;; Event fact types
;;
;; The reducer accepts normalized event facts.  Each fact is a hash with
;; at least 'session-id and 'turn-id keys.
;; ──────────────────────────────────────────────────────

;; model.stream.thinking — append reasoning delta
(define (reduce-model-stream-thinking rs fact)
  (define session-id (hash-ref fact 'session-id ""))
  (define turn-id (hash-ref fact 'turn-id ""))
  (define delta (hash-ref fact 'delta ""))
  (define provider-tag (hash-ref fact 'provider-capability-tag #f))
  (define-values (art rs1) (reducer-get-or-create rs session-id turn-id 'thinking provider-tag))
  ;; Only append if still in streaming lifecycle
  (define art2
    (if (eq? (conversation-artifact-lifecycle art) 'streaming)
        (artifact-append-body art delta)
        art))
  (reducer-put rs1 art2))

;; model.stream.delta — append assistant text delta
(define (reduce-model-stream-delta rs fact)
  (define session-id (hash-ref fact 'session-id ""))
  (define turn-id (hash-ref fact 'turn-id ""))
  (define delta (hash-ref fact 'delta ""))
  (define-values (art rs1) (reducer-get-or-create rs session-id turn-id 'assistant))
  (define art2
    (if (eq? (conversation-artifact-lifecycle art) 'streaming)
        (artifact-append-body art delta)
        art))
  (reducer-put rs1 art2))

;; model.stream.completed — mark thinking and assistant as completed
;; CRITICAL: does NOT discard the live reasoning buffer.
;; The artifact transitions to 'completed lifecycle; the body is preserved.
;; Byte limits enforced ONLY here (persistence boundary).
(define (reduce-model-stream-completed rs fact)
  (define session-id (hash-ref fact 'session-id ""))
  (define turn-id (hash-ref fact 'turn-id ""))
  (define max-bytes (ui-reasoning-artifacts-max-bytes))

  ;; Mark thinking artifact as completed (if it exists and still streaming)
  (define thinking-art (reducer-get rs session-id turn-id 'thinking))
  (define rs1
    (if (and thinking-art (eq? (conversation-artifact-lifecycle thinking-art) 'streaming))
        (let ([completed (artifact-set-lifecycle thinking-art 'completed)])
          ;; Check oversized at persistence boundary
          (when (artifact-oversized? completed max-bytes)
            (telemetry-oversized!))
          (reducer-put rs completed))
        (begin
          (when (not thinking-art)
            (telemetry-missing!))
          rs)))

  ;; Mark assistant artifact as completed (if it exists)
  (define assistant-art (reducer-get rs1 session-id turn-id 'assistant))
  (define rs2
    (if assistant-art
        (reducer-put rs1 (artifact-set-lifecycle assistant-art 'completed))
        rs1))

  ;; Mark turn as completed
  (reducer-mark-turn-completed rs2 turn-id))

;; assistant.message.completed — mark assistant as retained
;; Idempotent: if model.stream.completed already marked it 'completed,
;; this transitions to 'retained without losing body or creating duplicates.
(define (reduce-assistant-message-completed rs fact)
  (define session-id (hash-ref fact 'session-id ""))
  (define turn-id (hash-ref fact 'turn-id ""))
  (define content (hash-ref fact 'content ""))

  ;; If there's no assistant artifact yet (model.stream.delta events weren't
  ;; seen), create one now with the completed content.
  (define-values (assistant-art rs1)
    (let ([existing (reducer-get rs session-id turn-id 'assistant)])
      (if existing
          (values existing rs)
          (let ([art (make-conversation-artifact #:id (make-artifact-id session-id turn-id 'assistant)
                                                 #:turn-id turn-id
                                                 #:session-id session-id
                                                 #:kind 'assistant
                                                 #:body content
                                                 #:lifecycle 'completed)])
            (values art (reducer-put rs art))))))

  ;; Transition to 'retained (idempotent — if already 'retained, no-op)
  (define retained-art (artifact-set-lifecycle assistant-art 'retained))
  (define rs2 (reducer-put rs1 retained-art))

  ;; If thinking artifact exists and is 'completed, transition to 'retained
  (define thinking-art (reducer-get rs2 session-id turn-id 'thinking))
  (define rs3
    (if (and thinking-art
             (memq (conversation-artifact-lifecycle thinking-art) '(streaming completed)))
        (reducer-put rs2 (artifact-set-lifecycle thinking-art 'retained))
        rs2))

  ;; Check oversized at persistence boundary
  (when (and thinking-art (artifact-oversized? thinking-art (ui-reasoning-artifacts-max-bytes)))
    (telemetry-oversized!))

  (reducer-mark-turn-completed rs3 turn-id))

;; Cancellation / error — mark artifacts as 'rejected
(define (reduce-cancellation rs fact)
  (define session-id (hash-ref fact 'session-id ""))
  (define turn-id (hash-ref fact 'turn-id ""))
  (define thinking-art (reducer-get rs session-id turn-id 'thinking))
  (define rs1
    (if thinking-art
        (reducer-put rs (artifact-set-lifecycle thinking-art 'rejected))
        rs))
  (define assistant-art (reducer-get rs1 session-id turn-id 'assistant))
  (define rs2
    (if assistant-art
        (reducer-put rs1 (artifact-set-lifecycle assistant-art 'rejected))
        rs1))
  rs2)

(define (reduce-error rs fact)
  (define session-id (hash-ref fact 'session-id ""))
  (define turn-id (hash-ref fact 'turn-id ""))
  (define thinking-art (reducer-get rs session-id turn-id 'thinking))
  (define rs1
    (if thinking-art
        (reducer-put rs (artifact-set-lifecycle thinking-art 'rejected))
        rs))
  (define assistant-art (reducer-get rs1 session-id turn-id 'assistant))
  (define rs2
    (if assistant-art
        (reducer-put rs1 (artifact-set-lifecycle assistant-art 'rejected))
        rs1))
  rs2)

;; ──────────────────────────────────────────────────────
;; Dispatch: reduce a single event fact
;; ──────────────────────────────────────────────────────

(define (reduce-event rs fact)
  (define event-type (hash-ref fact 'event-type #f))
  (cond
    [(equal? event-type "model.stream.thinking") (reduce-model-stream-thinking rs fact)]
    [(equal? event-type "model.stream.delta") (reduce-model-stream-delta rs fact)]
    [(equal? event-type "model.stream.completed") (reduce-model-stream-completed rs fact)]
    [(equal? event-type "assistant.message.completed") (reduce-assistant-message-completed rs fact)]
    [(equal? event-type "cancellation") (reduce-cancellation rs fact)]
    [(equal? event-type "error") (reduce-error rs fact)]
    [else rs]))

;; ──────────────────────────────────────────────────────
;; Collect artifacts for a turn
;; ──────────────────────────────────────────────────────

(define (reducer-artifacts-for-turn rs session-id turn-id)
  (for/list ([(k art) (in-hash (reducer-state-artifacts rs))]
             #:when (and (equal? (artifact-key-session k) session-id)
                         (equal? (artifact-key-turn k) turn-id)))
    art))

(define (reducer-thinking-artifact rs session-id turn-id)
  (reducer-get rs session-id turn-id 'thinking))

(define (reducer-assistant-artifact rs session-id turn-id)
  (reducer-get rs session-id turn-id 'assistant))

;; ──────────────────────────────────────────────────────
;; Convenience: reduce a sequence of events
;; ──────────────────────────────────────────────────────

(define (reduce-events rs facts)
  (for/fold ([state rs]) ([fact (in-list facts)])
    (reduce-event state fact)))

;; ──────────────────────────────────────────────────────
;; Provide
;; ──────────────────────────────────────────────────────

(provide (struct-out reducer-state)
         (contract-out [make-reducer-state (-> reducer-state?)]
                       [reduce-event (-> reducer-state? hash? reducer-state?)]
                       [reduce-events (-> reducer-state? (listof hash?) reducer-state?)]
                       [reducer-get
                        (-> reducer-state? string? string? symbol? (or/c conversation-artifact? #f))]
                       [reducer-get-or-create
                        (->* (reducer-state? string? string? symbol?)
                             ((or/c symbol? string? #f))
                             (values conversation-artifact? reducer-state?))]
                       [reducer-artifacts-for-turn
                        (-> reducer-state? string? string? (listof conversation-artifact?))]
                       [reducer-thinking-artifact
                        (-> reducer-state? string? string? (or/c conversation-artifact? #f))]
                       [reducer-assistant-artifact
                        (-> reducer-state? string? string? (or/c conversation-artifact? #f))]
                       [reducer-mark-turn-completed (-> reducer-state? string? reducer-state?)]
                       [reducer-turn-completed? (-> reducer-state? string? boolean?)]
                       [reset-telemetry-counters! (-> void?)]
                       [make-artifact-id (-> string? string? symbol? string?)]))

;; ──────────────────────────────────────────────────────
;; Submodule: tests
;; ──────────────────────────────────────────────────────

(module+ test
  (require rackunit)

  (reset-telemetry-counters!)

  (test-case "single thinking event creates one artifact"
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-event rs0
                    (hasheq 'event-type
                            "model.stream.thinking"
                            'session-id
                            "s1"
                            'turn-id
                            "t1"
                            'delta
                            "Thinking...")))
    (define art (reducer-thinking-artifact rs1 "s1" "t1"))
    (check-true (conversation-artifact? art))
    (check-equal? (conversation-artifact-body art) "Thinking...")
    (check-eq? (conversation-artifact-lifecycle art) 'streaming))

  (test-case "multiple thinking events append to same artifact"
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events
       rs0
       (list
        (hasheq 'event-type "model.stream.thinking" 'session-id "s1" 'turn-id "t1" 'delta "Hello")
        (hasheq 'event-type "model.stream.thinking" 'session-id "s1" 'turn-id "t1" 'delta " world"))))
    (define art (reducer-thinking-artifact rs1 "s1" "t1"))
    (check-equal? (conversation-artifact-body art) "Hello world"))

  (test-case "model.stream.completed does NOT discard thinking body"
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events
       rs0
       (list (hasheq 'event-type
                     "model.stream.thinking"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'delta
                     "Important reasoning")
             (hasheq 'event-type "model.stream.completed" 'session-id "s1" 'turn-id "t1"))))
    (define art (reducer-thinking-artifact rs1 "s1" "t1"))
    (check-true (conversation-artifact? art))
    (check-equal? (conversation-artifact-body art) "Important reasoning")
    (check-eq? (conversation-artifact-lifecycle art) 'completed))

  (test-case "completed-before-assistant retains thinking exactly once"
    ;; model.stream.completed arrives BEFORE assistant.message.completed
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events
       rs0
       (list (hasheq 'event-type
                     "model.stream.thinking"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'delta
                     "Reasoning A")
             (hasheq 'event-type "model.stream.completed" 'session-id "s1" 'turn-id "t1")
             (hasheq 'event-type
                     "assistant.message.completed"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'content
                     "Answer"))))
    (define thinking-art (reducer-thinking-artifact rs1 "s1" "t1"))
    (check-true (conversation-artifact? thinking-art))
    (check-equal? (conversation-artifact-body thinking-art) "Reasoning A")
    (check-eq? (conversation-artifact-lifecycle thinking-art) 'retained)
    ;; Verify exactly one thinking artifact (no duplicates)
    (define all-arts (reducer-artifacts-for-turn rs1 "s1" "t1"))
    (define thinking-arts
      (filter (lambda (a) (eq? (conversation-artifact-kind a) 'thinking)) all-arts))
    (check-equal? (length thinking-arts) 1))

  (test-case "assistant-before-completed retains thinking exactly once"
    ;; assistant.message.completed arrives BEFORE model.stream.completed
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events
       rs0
       (list (hasheq 'event-type
                     "model.stream.thinking"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'delta
                     "Reasoning B")
             (hasheq 'event-type
                     "assistant.message.completed"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'content
                     "Answer")
             (hasheq 'event-type "model.stream.completed" 'session-id "s1" 'turn-id "t1"))))
    (define thinking-art (reducer-thinking-artifact rs1 "s1" "t1"))
    (check-true (conversation-artifact? thinking-art))
    (check-equal? (conversation-artifact-body thinking-art) "Reasoning B")
    (check-eq? (conversation-artifact-lifecycle thinking-art) 'retained)
    ;; Verify exactly one thinking artifact
    (define all-arts (reducer-artifacts-for-turn rs1 "s1" "t1"))
    (define thinking-arts
      (filter (lambda (a) (eq? (conversation-artifact-kind a) 'thinking)) all-arts))
    (check-equal? (length thinking-arts) 1))

  (test-case "reasoning + assistant text → both persist separately"
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events
       rs0
       (list (hasheq 'event-type
                     "model.stream.thinking"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'delta
                     "Let me think")
             (hasheq 'event-type
                     "model.stream.delta"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'delta
                     "Here is the answer")
             (hasheq 'event-type "model.stream.completed" 'session-id "s1" 'turn-id "t1")
             (hasheq 'event-type
                     "assistant.message.completed"
                     'session-id
                     "s1"
                     'turn-id
                     "t1"
                     'content
                     "Here is the answer"))))
    (define thinking-art (reducer-thinking-artifact rs1 "s1" "t1"))
    (define assistant-art (reducer-assistant-artifact rs1 "s1" "t1"))
    (check-true (conversation-artifact? thinking-art))
    (check-true (conversation-artifact? assistant-art))
    (check-equal? (conversation-artifact-body thinking-art) "Let me think")
    (check-equal? (conversation-artifact-body assistant-art) "Here is the answer")
    (check-eq? (conversation-artifact-lifecycle thinking-art) 'retained)
    (check-eq? (conversation-artifact-lifecycle assistant-art) 'retained))

  (test-case "cancellation marks artifacts as rejected"
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events rs0
                     (list (hasheq 'event-type
                                   "model.stream.thinking"
                                   'session-id
                                   "s1"
                                   'turn-id
                                   "t1"
                                   'delta
                                   "Partial reasoning")
                           (hasheq 'event-type "cancellation" 'session-id "s1" 'turn-id "t1"))))
    (define art (reducer-thinking-artifact rs1 "s1" "t1"))
    (check-true (conversation-artifact? art))
    (check-equal? (conversation-artifact-body art) "Partial reasoning")
    (check-eq? (conversation-artifact-lifecycle art) 'rejected))

  (test-case "error marks artifacts as rejected"
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events rs0
                     (list (hasheq 'event-type
                                   "model.stream.thinking"
                                   'session-id
                                   "s1"
                                   'turn-id
                                   "t1"
                                   'delta
                                   "Partial reasoning")
                           (hasheq 'event-type "error" 'session-id "s1" 'turn-id "t1"))))
    (define art (reducer-thinking-artifact rs1 "s1" "t1"))
    (check-true (conversation-artifact? art))
    (check-eq? (conversation-artifact-lifecycle art) 'rejected))

  (test-case "different turns produce independent artifacts"
    (define rs0 (make-reducer-state))
    (define rs1
      (reduce-events rs0
                     (list (hasheq 'event-type
                                   "model.stream.thinking"
                                   'session-id
                                   "s1"
                                   'turn-id
                                   "t1"
                                   'delta
                                   "Turn 1 reasoning")
                           (hasheq 'event-type
                                   "model.stream.thinking"
                                   'session-id
                                   "s1"
                                   'turn-id
                                   "t2"
                                   'delta
                                   "Turn 2 reasoning"))))
    (define art1 (reducer-thinking-artifact rs1 "s1" "t1"))
    (define art2 (reducer-thinking-artifact rs1 "s1" "t2"))
    (check-equal? (conversation-artifact-body art1) "Turn 1 reasoning")
    (check-equal? (conversation-artifact-body art2) "Turn 2 reasoning"))

  (test-case "idempotent: re-applying same events produces same state"
    (define facts
      (list
       (hasheq 'event-type "model.stream.thinking" 'session-id "s1" 'turn-id "t1" 'delta "Reasoning")
       (hasheq 'event-type "model.stream.completed" 'session-id "s1" 'turn-id "t1")
       (hasheq 'event-type
               "assistant.message.completed"
               'session-id
               "s1"
               'turn-id
               "t1"
               'content
               "Answer")))
    (define rs1 (reduce-events (make-reducer-state) facts))
    (define rs2 (reduce-events rs1 facts))
    ;; State should be the same — thinking artifact body unchanged
    (define art1 (reducer-thinking-artifact rs1 "s1" "t1"))
    (define art2 (reducer-thinking-artifact rs2 "s1" "t1"))
    (check-equal? (conversation-artifact-body art1) (conversation-artifact-body art2))
    (check-eq? (conversation-artifact-lifecycle art1) (conversation-artifact-lifecycle art2)))

  (test-case "telemetry counters don't crash"
    (reset-telemetry-counters!)
    (define rs0 (make-reducer-state))
    ;; No thinking artifact when model.stream.completed arrives → missing counter
    (define rs1
      (reduce-event rs0 (hasheq 'event-type "model.stream.completed" 'session-id "s1" 'turn-id "t1")))
    (check-true (reducer-state? rs1))))
