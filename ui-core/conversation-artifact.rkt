#lang racket/base

;; q/ui-core/conversation-artifact.rkt — Immutable conversation artifact contract
;;
;; STABILITY: internal
;;
;; Pure data contract for a durable, first-class conversation artifact.
;; A reasoning trace, assistant message, or tool call result is represented
;; as a `conversation-artifact` value that is independent of provider event
;; ordering, ANSI escape codes, GUI widget concerns, or event-bus subscriptions.
;;
;; The artifact is keyed by (session-id, turn-id, kind).  Exactly one
;; artifact per (session, turn, 'thinking) is the identity invariant
;; enforced by the companion reducer module.

(require racket/contract
         racket/string
         "feature-flags.rkt")

;; ──────────────────────────────────────────────────────
;; Contracts
;; ──────────────────────────────────────────────────────

(define artifact-id/c string?)
(define turn-id/c string?)
(define session-id/c string?)
(define artifact-kind/c (or/c 'thinking 'assistant 'tool 'tool-start 'tool-end 'system 'user 'error))
(define artifact-lifecycle/c (or/c 'streaming 'completed 'retained 'rejected))
(define persistence-class/c (or/c 'session 'scrollback 'never))
(define artifact-body/c string?)
(define artifact-summary/c string?)
(define artifact-metadata/c hash?)

;; ──────────────────────────────────────────────────────
;; Struct definition
;; ──────────────────────────────────────────────────────

(struct conversation-artifact
        (id ; string — globally unique artifact ID
         turn-id ; string — the turn this artifact belongs to
         session-id ; string — the session this artifact belongs to
         kind ; symbol — 'thinking, 'assistant, 'tool, etc.
         body ; string — full body text (never truncated mid-stream)
         summary ; string — human-readable summary (first non-empty line or neutral label)
         lifecycle ; symbol — 'streaming / 'completed / 'retained / 'rejected
         persistence ; symbol — 'session / 'scrollback / 'never
         metadata) ; hash — line-count, byte-size, provider-capability-tag, redaction-marker
  #:transparent)

;; ──────────────────────────────────────────────────────
;; Smart constructors and helpers
;; ──────────────────────────────────────────────────────

;; Compute metadata from a body string + optional provider tag.
(define (compute-metadata body [provider-tag #f] [redacted? #f])
  (define lines (string-split body "\n"))
  (hasheq 'line-count
          (length lines)
          'byte-size
          (bytes-length (string->bytes/utf-8 body))
          'provider-capability-tag
          (or provider-tag 'unknown)
          'redaction-marker
          redacted?))

;; Generate a summary from the body: first non-empty line, truncated
;; to 120 chars for display.  If body is empty, return neutral label.
(define (compute-summary body [kind 'thinking])
  (define trimmed (string-trim body))
  (cond
    [(string=? trimmed "")
     (case kind
       [(thinking) "Reasoning"]
       [(assistant) "Response"]
       [(tool tool-start tool-end) "Tool"]
       [(system) "System"]
       [(user) "User"]
       [(error) "Error"]
       [else "Artifact"])]
    [else
     (define first-line (car (string-split trimmed "\n")))
     (if (> (string-length first-line) 120)
         (string-append (substring first-line 0 117) "...")
         first-line)]))

;; Create a new artifact in the 'streaming lifecycle.
(define (make-conversation-artifact #:id id
                                    #:turn-id turn-id
                                    #:session-id session-id
                                    #:kind kind
                                    #:body [body ""]
                                    #:summary [summary #f]
                                    #:lifecycle [lifecycle 'streaming]
                                    #:persistence [persistence 'session]
                                    #:provider-tag [provider-tag #f]
                                    #:redacted? [redacted? #f])
  (conversation-artifact id
                         turn-id
                         session-id
                         kind
                         body
                         (or summary (compute-summary body kind))
                         lifecycle
                         persistence
                         (compute-metadata body provider-tag redacted?)))

;; ──────────────────────────────────────────────────────
;; Functional updaters (return new artifact, immutable)
;; ──────────────────────────────────────────────────────

;; Append text to the body (used during streaming).
(define (artifact-append-body art delta)
  (define new-body (string-append (conversation-artifact-body art) delta))
  (struct-copy conversation-artifact
               art
               [body new-body]
               [summary (compute-summary new-body (conversation-artifact-kind art))]
               [metadata
                (compute-metadata
                 new-body
                 (hash-ref (conversation-artifact-metadata art) 'provider-capability-tag #f)
                 (hash-ref (conversation-artifact-metadata art) 'redaction-marker #f))]))

;; Transition lifecycle state.
(define (artifact-set-lifecycle art lifecycle)
  (struct-copy conversation-artifact art [lifecycle lifecycle]))

;; Bound a body at a persistence boundary without splitting a UTF-8 sequence.
;; Streaming append remains lossless; callers invoke this only when the body
;; becomes durable.
(define (artifact-limit-body art max-bytes)
  (define body (conversation-artifact-body art))
  (if (not (artifact-oversized? art max-bytes))
      art
      (let loop ([chars (string->list body)]
                 [used 0]
                 [kept '()])
        (cond
          [(null? chars) art]
          [else
           (define width (bytes-length (string->bytes/utf-8 (string (car chars)))))
           (if (> (+ used width) max-bytes)
               (let ([bounded (list->string (reverse kept))])
                 (struct-copy conversation-artifact
                              art
                              [body bounded]
                              [summary (compute-summary bounded (conversation-artifact-kind art))]
                              [metadata
                               (compute-metadata bounded
                                                 (hash-ref (conversation-artifact-metadata art)
                                                           'provider-capability-tag
                                                           #f)
                                                 #t)]))
               (loop (cdr chars) (+ used width) (cons (car chars) kept)))]))))

;; Set persistence class.
(define (artifact-set-persistence art persistence)
  (struct-copy conversation-artifact art [persistence persistence]))

;; Mark as redacted (updates metadata).
(define (artifact-mark-redacted art)
  (define old-meta (conversation-artifact-metadata art))
  (struct-copy conversation-artifact art [metadata (hash-set old-meta 'redaction-marker #t)]))

;; ──────────────────────────────────────────────────────
;; Serialization (JSON-compatible hash, for scrollback)
;; ──────────────────────────────────────────────────────

(define (validate-artifact who art)
  (unless (and (conversation-artifact? art)
               (string? (conversation-artifact-id art))
               (not (string=? (conversation-artifact-id art) ""))
               (string? (conversation-artifact-turn-id art))
               (not (string=? (conversation-artifact-turn-id art) ""))
               (string? (conversation-artifact-session-id art))
               (not (string=? (conversation-artifact-session-id art) ""))
               (memq (conversation-artifact-kind art)
                     '(thinking assistant tool tool-start tool-end system user error))
               (string? (conversation-artifact-body art))
               (string? (conversation-artifact-summary art))
               (memq (conversation-artifact-lifecycle art) '(streaming completed retained rejected))
               (memq (conversation-artifact-persistence art) '(session scrollback never))
               (hash? (conversation-artifact-metadata art)))
    (raise-argument-error who "schema-valid conversation-artifact?" art))
  art)

(define (artifact->jsexpr art)
  (validate-artifact 'artifact->jsexpr art)
  (define persisted
    (if (eq? (conversation-artifact-kind art) 'thinking)
        (artifact-limit-body art (ui-reasoning-artifacts-max-bytes))
        art))
  (hasheq 'schema
          "conversation-artifact"
          'schema-version
          1
          'id
          (conversation-artifact-id persisted)
          'turn-id
          (conversation-artifact-turn-id persisted)
          'session-id
          (conversation-artifact-session-id persisted)
          'kind
          (symbol->string (conversation-artifact-kind persisted))
          'body
          (conversation-artifact-body persisted)
          'summary
          (conversation-artifact-summary persisted)
          'lifecycle
          (symbol->string (conversation-artifact-lifecycle persisted))
          'persistence
          (symbol->string (conversation-artifact-persistence persisted))
          'metadata
          (metadata->jsexpr (conversation-artifact-metadata persisted))))

(define (metadata->jsexpr h)
  (for/hasheq ([(k v) (in-hash h)])
    (values k
            (cond
              [(symbol? v) (symbol->string v)]
              [(hash? v) (metadata->jsexpr v)]
              [else v]))))

(define (jsexpr->metadata h)
  (for/hasheq ([(k v) (in-hash h)])
    (values k
            (cond
              [(and (string? v) (memq k '(provider-capability-tag redaction-marker)))
               (if (string=? v "#t")
                   #t
                   (if (string=? v "#f")
                       #f
                       (string->symbol v)))]
              [(hash? v) (jsexpr->metadata v)]
              [else v]))))

(define (jsexpr->artifact h)
  (unless (and (hash? h)
               (equal? (hash-ref h 'schema #f) "conversation-artifact")
               (equal? (hash-ref h 'schema-version #f) 1)
               (string? (hash-ref h 'id #f))
               (string? (hash-ref h 'turn-id #f))
               (string? (hash-ref h 'session-id #f))
               (string? (hash-ref h 'kind #f))
               (string? (hash-ref h 'body #f))
               (string? (hash-ref h 'summary #f))
               (string? (hash-ref h 'lifecycle #f))
               (string? (hash-ref h 'persistence #f))
               (hash? (hash-ref h 'metadata #f)))
    (raise-argument-error 'jsexpr->artifact "conversation-artifact schema version 1 jsexpr" h))
  (validate-artifact 'jsexpr->artifact
                     (conversation-artifact (hash-ref h 'id)
                                            (hash-ref h 'turn-id)
                                            (hash-ref h 'session-id)
                                            (string->symbol (hash-ref h 'kind))
                                            (hash-ref h 'body)
                                            (hash-ref h 'summary)
                                            (string->symbol (hash-ref h 'lifecycle))
                                            (string->symbol (hash-ref h 'persistence))
                                            (jsexpr->metadata (hash-ref h 'metadata)))))

;; ──────────────────────────────────────────────────────
;; Byte-size check (used at persistence boundaries only)
;; ──────────────────────────────────────────────────────

(define (artifact-oversized? art max-bytes)
  (> (bytes-length (string->bytes/utf-8 (conversation-artifact-body art))) max-bytes))

;; ──────────────────────────────────────────────────────
;; Provide
;; ──────────────────────────────────────────────────────

(provide (struct-out conversation-artifact)
         (contract-out
          [make-conversation-artifact
           (->* (#:id string? #:turn-id string? #:session-id string? #:kind artifact-kind/c)
                (#:body string?
                        #:summary (or/c string? #f)
                        #:lifecycle artifact-lifecycle/c
                        #:persistence persistence-class/c
                        #:provider-tag (or/c symbol? string? #f)
                        #:redacted? boolean?)
                conversation-artifact?)]
          [artifact-append-body (-> conversation-artifact? string? conversation-artifact?)]
          [artifact-set-lifecycle
           (-> conversation-artifact? artifact-lifecycle/c conversation-artifact?)]
          [artifact-limit-body
           (-> conversation-artifact? exact-nonnegative-integer? conversation-artifact?)]
          [artifact-set-persistence
           (-> conversation-artifact? persistence-class/c conversation-artifact?)]
          [artifact-mark-redacted (-> conversation-artifact? conversation-artifact?)]
          [artifact->jsexpr (-> conversation-artifact? hash?)]
          [jsexpr->artifact (-> hash? conversation-artifact?)]
          [artifact-oversized? (-> conversation-artifact? exact-nonnegative-integer? boolean?)]
          [compute-summary (->* (string?) (symbol?) string?)]
          [compute-metadata (->* (string?) ((or/c symbol? string? #f) boolean?) hash?)]))

;; ──────────────────────────────────────────────────────
;; Submodule: tests
;; ──────────────────────────────────────────────────────

(module+ test
  (require rackunit)

  (test-case "make-conversation-artifact creates streaming artifact"
    (define art
      (make-conversation-artifact #:id "art-1"
                                  #:turn-id "turn-1"
                                  #:session-id "sess-1"
                                  #:kind 'thinking
                                  #:body "Let me think about this."))
    (check-equal? (conversation-artifact-id art) "art-1")
    (check-equal? (conversation-artifact-kind art) 'thinking)
    (check-equal? (conversation-artifact-lifecycle art) 'streaming)
    (check-equal? (conversation-artifact-body art) "Let me think about this.")
    (check-equal? (conversation-artifact-summary art) "Let me think about this."))

  (test-case "artifact-append-body appends and recomputes summary"
    (define art
      (make-conversation-artifact #:id "art-1"
                                  #:turn-id "turn-1"
                                  #:session-id "sess-1"
                                  #:kind 'thinking
                                  #:body "Hello"))
    (define art2 (artifact-append-body art " world"))
    (check-equal? (conversation-artifact-body art2) "Hello world")
    (check-equal? (conversation-artifact-summary art2) "Hello world"))

  (test-case "compute-summary uses first non-empty line"
    (check-equal? (compute-summary "\n\nFirst line\nSecond" 'thinking) "First line"))

  (test-case "compute-summary returns neutral label for empty body"
    (check-equal? (compute-summary "" 'thinking) "Reasoning")
    (check-equal? (compute-summary "   " 'assistant) "Response"))

  (test-case "artifact-set-lifecycle transitions state"
    (define art
      (make-conversation-artifact #:id "art-1" #:turn-id "t" #:session-id "s" #:kind 'thinking))
    (check-eq? (conversation-artifact-lifecycle (artifact-set-lifecycle art 'completed)) 'completed)
    (check-eq? (conversation-artifact-lifecycle (artifact-set-lifecycle art 'retained)) 'retained))

  (test-case "artifact round-trip via jsexpr preserves all fields"
    (define art
      (make-conversation-artifact #:id "art-1"
                                  #:turn-id "turn-1"
                                  #:session-id "sess-1"
                                  #:kind 'thinking
                                  #:body "Thinking content"
                                  #:lifecycle 'retained
                                  #:persistence 'scrollback))
    (define js (artifact->jsexpr art))
    (check-equal? (hash-ref js 'schema) "conversation-artifact")
    (check-equal? (hash-ref js 'schema-version) 1)
    (define restored (jsexpr->artifact js))
    (check-equal? (conversation-artifact-id restored) "art-1")
    (check-equal? (conversation-artifact-body restored) "Thinking content")
    (check-equal? (conversation-artifact-kind restored) 'thinking)
    (check-equal? (conversation-artifact-lifecycle restored) 'retained))

  (test-case "artifact-oversized? checks byte size"
    (define art
      (make-conversation-artifact #:id "a"
                                  #:turn-id "t"
                                  #:session-id "s"
                                  #:kind 'thinking
                                  #:body "short"))
    (check-false (artifact-oversized? art 100))
    (check-true (artifact-oversized? art 3)))

  (test-case "artifact-mark-redacted sets redaction marker"
    (define art (make-conversation-artifact #:id "a" #:turn-id "t" #:session-id "s" #:kind 'thinking))
    (define redacted (artifact-mark-redacted art))
    (check-equal? (hash-ref (conversation-artifact-metadata redacted) 'redaction-marker) #t)))
