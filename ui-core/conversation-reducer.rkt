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
         racket/list
         racket/set
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

(define (canonical-artifact-identity? session-id turn-id)
  (and (string? session-id)
       (not (string=? session-id ""))
       (string? turn-id)
       (not (string=? turn-id ""))))

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
  ;; Length-prefix both free-form IDs so delimiters inside either component
  ;; cannot alias another (session, turn) pair.
  (format "~a:~a~a:~a:~a" (string-length session-id) session-id (string-length turn-id) turn-id kind))

;; ──────────────────────────────────────────────────────
;; Reducer state
;; ──────────────────────────────────────────────────────

;; The reducer state is a hash: key → conversation-artifact
;; plus a set of completed turn-ids to track completion.

(define reducer-max-artifacts 500)

;; Orders are oldest-first and let the live reducer prune deterministically at
;; the same retention boundary as TUI/GUI scrollback.
(struct reducer-state (artifacts completed-turns artifact-order completion-order) #:transparent)

(define (make-reducer-state)
  (reducer-state (hash) (hash) '() '()))

(define (reducer-get rs session-id turn-id kind)
  (hash-ref (reducer-state-artifacts rs) (make-artifact-key session-id turn-id kind) #f))

(define (reducer-put rs art)
  (define key
    (make-artifact-key (conversation-artifact-session-id art)
                       (conversation-artifact-turn-id art)
                       (conversation-artifact-kind art)))
  (define order (append (remove key (reducer-state-artifact-order rs)) (list key)))
  (define overflow (max 0 (- (length order) reducer-max-artifacts)))
  (define evicted (take order overflow))
  (define kept-order (drop order overflow))
  (define artifacts
    (for/fold ([current (hash-set (reducer-state-artifacts rs) key art)])
              ([old-key (in-list evicted)])
      (hash-remove current old-key)))
  (reducer-state artifacts
                 (reducer-state-completed-turns rs)
                 kept-order
                 (reducer-state-completion-order rs)))

(define (completion-key session-id turn-id)
  (vector session-id turn-id))

(define (reducer-mark-turn-completed rs session-id turn-id [source 'completed])
  (if (canonical-artifact-identity? session-id turn-id)
      (let* ([key (completion-key session-id turn-id)]
             [sources (hash-ref (reducer-state-completed-turns rs) key (set))])
        (define order (append (remove key (reducer-state-completion-order rs)) (list key)))
        (define overflow (max 0 (- (length order) reducer-max-artifacts)))
        (define evicted (take order overflow))
        (define completions
          (for/fold ([current
                      (hash-set (reducer-state-completed-turns rs) key (set-add sources source))])
                    ([old-key (in-list evicted)])
            (hash-remove current old-key)))
        (reducer-state (reducer-state-artifacts rs)
                       completions
                       (reducer-state-artifact-order rs)
                       (drop order overflow)))
      rs))

(define (reducer-turn-completed? rs session-id turn-id [source #f])
  (define sources
    (and (canonical-artifact-identity? session-id turn-id)
         (hash-ref (reducer-state-completed-turns rs) (completion-key session-id turn-id) #f)))
  (and sources
       (if source
           (set-member? sources source)
           (not (set-empty? sources)))))

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
  (define session-id (hash-ref fact 'session-id #f))
  (define turn-id (hash-ref fact 'turn-id #f))
  (cond
    [(not (canonical-artifact-identity? session-id turn-id)) rs]
    [else
     (define delta (hash-ref fact 'delta ""))
     (define provider-tag (hash-ref fact 'provider-capability-tag #f))
     (define-values (art rs1) (reducer-get-or-create rs session-id turn-id 'thinking provider-tag))
     (define appended
       (if (eq? (conversation-artifact-lifecycle art) 'streaming)
           (artifact-append-body art delta)
           art))
     (define art2
       (cond
         [(reducer-turn-completed? rs1 session-id turn-id 'assistant)
          (artifact-set-lifecycle appended 'retained)]
         [(reducer-turn-completed? rs1 session-id turn-id 'model)
          (artifact-set-lifecycle appended 'completed)]
         [else appended]))
     (reducer-put rs1 art2)]))

;; model.stream.delta — append assistant text delta
(define (reduce-model-stream-delta rs fact)
  (define session-id (hash-ref fact 'session-id #f))
  (define turn-id (hash-ref fact 'turn-id #f))
  (cond
    [(not (canonical-artifact-identity? session-id turn-id)) rs]
    [else
     (define delta (hash-ref fact 'delta ""))
     (define-values (art rs1) (reducer-get-or-create rs session-id turn-id 'assistant))
     (define appended
       (if (eq? (conversation-artifact-lifecycle art) 'streaming)
           (artifact-append-body art delta)
           art))
     (define art2
       (if (reducer-turn-completed? rs1 session-id turn-id)
           (artifact-set-lifecycle appended 'retained)
           appended))
     (reducer-put rs1 art2)]))

;; model.stream.completed — mark thinking and assistant as completed
;; CRITICAL: does NOT discard the live reasoning buffer.
;; The artifact transitions to 'completed lifecycle; the body is preserved.
;; Byte limits enforced ONLY here (persistence boundary).
(define (reduce-model-stream-completed rs fact)
  (define session-id (hash-ref fact 'session-id #f))
  (define turn-id (hash-ref fact 'turn-id #f))
  (cond
    [(not (canonical-artifact-identity? session-id turn-id)) rs]
    [else
     (define max-bytes (ui-reasoning-artifacts-max-bytes))

     ;; Mark thinking artifact as completed (if it exists and still streaming)
     (define thinking-art (reducer-get rs session-id turn-id 'thinking))
     (define rs1
       (if (and thinking-art (eq? (conversation-artifact-lifecycle thinking-art) 'streaming))
           (let* ([completed (artifact-set-lifecycle thinking-art 'completed)]
                  [oversized? (artifact-oversized? completed max-bytes)]
                  [persistable (artifact-limit-body completed max-bytes)])
             (when oversized?
               (telemetry-oversized!))
             (reducer-put rs persistable))
           (begin
             (when (not thinking-art)
               (telemetry-missing!))
             rs)))

     ;; Mark assistant artifact as completed (if it exists)
     (define assistant-art (reducer-get rs1 session-id turn-id 'assistant))
     (define rs2
       (if (and assistant-art (eq? (conversation-artifact-lifecycle assistant-art) 'streaming))
           (reducer-put rs1 (artifact-set-lifecycle assistant-art 'completed))
           rs1))

     ;; Mark completion by the full canonical identity.  Preserve a retained
     ;; lifecycle when assistant completion arrived first.
     (reducer-mark-turn-completed rs2 session-id turn-id 'model)]))

;; assistant.message.completed — mark assistant as retained
;; Idempotent: if model.stream.completed already marked it 'completed,
;; this transitions to 'retained without losing body or creating duplicates.
(define (reduce-assistant-message-completed rs fact)
  (define session-id (hash-ref fact 'session-id #f))
  (define turn-id (hash-ref fact 'turn-id #f))
  (cond
    [(not (canonical-artifact-identity? session-id turn-id)) rs]
    [else
     (define content (hash-ref fact 'content ""))

     ;; Completion itself is the durable assistant-message boundary. Preserve
     ;; an empty assistant artifact for tool-only turns and empty provider
     ;; responses; projection code decides whether an empty artifact is shown.
     (define existing-assistant (reducer-get rs session-id turn-id 'assistant))
     (define rs2
       (if existing-assistant
           (reducer-put rs (artifact-set-lifecycle existing-assistant 'retained))
           (reducer-put rs
                        (make-conversation-artifact #:id
                                                    (make-artifact-id session-id turn-id 'assistant)
                                                    #:turn-id turn-id
                                                    #:session-id session-id
                                                    #:kind 'assistant
                                                    #:body (if (string? content) content "")
                                                    #:lifecycle 'retained))))

     ;; If thinking artifact exists and is 'completed, transition to 'retained
     (define thinking-art (reducer-get rs2 session-id turn-id 'thinking))
     (define max-bytes (ui-reasoning-artifacts-max-bytes))
     (define oversized? (and thinking-art (artifact-oversized? thinking-art max-bytes)))
     (define rs3
       (if (and thinking-art
                (memq (conversation-artifact-lifecycle thinking-art) '(streaming completed)))
           (reducer-put rs2
                        (artifact-limit-body (artifact-set-lifecycle thinking-art 'retained)
                                             max-bytes))
           rs2))

     (when oversized?
       (telemetry-oversized!))

     (reducer-mark-turn-completed rs3 session-id turn-id 'assistant)]))

;; Cancellation / error — mark artifacts as 'rejected
(define (reject-turn-artifacts rs session-id turn-id)
  (cond
    [(not (canonical-artifact-identity? session-id turn-id)) rs]
    [else
     (define thinking-art (reducer-get rs session-id turn-id 'thinking))
     (define rs1
       (if thinking-art
           (reducer-put rs (artifact-set-lifecycle thinking-art 'rejected))
           rs))
     (define assistant-art (reducer-get rs1 session-id turn-id 'assistant))
     (if assistant-art
         (reducer-put rs1 (artifact-set-lifecycle assistant-art 'rejected))
         rs1)]))

(define (reduce-cancellation rs fact)
  (reject-turn-artifacts rs (hash-ref fact 'session-id #f) (hash-ref fact 'turn-id #f)))

(define (reduce-error rs fact)
  (reject-turn-artifacts rs (hash-ref fact 'session-id #f) (hash-ref fact 'turn-id #f)))

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
                       [reducer-mark-turn-completed
                        (->* (reducer-state? string? string?) (symbol?) reducer-state?)]
                       [reducer-turn-completed?
                        (->* (reducer-state? string? string?) ((or/c symbol? #f)) boolean?)]
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
