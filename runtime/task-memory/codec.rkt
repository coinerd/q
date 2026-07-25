#lang racket

;; runtime/task-memory/codec.rkt — Ledger event serialization
;; STABILITY: internal
;;
;; JSON codec for task-ledger-event and task-checkpoint structs.
;; Provides hash round-trip and JSONL (newline-delimited JSON) format
;; for append-only storage.

(require racket/match
         racket/hash
         racket/string
         json
         (only-in racket/function const)
         "types.rkt")

(provide ledger-event->hash
         hash->ledger-event
         ledger-event->json
         json->ledger-event
         checkpoint->hash
         hash->checkpoint
         checkpoint->json
         json->checkpoint
         events->jsonl
         jsonl->events)

;; ============================================================
;; Ledger event -> hash
;; ============================================================

;; Serialize a task-ledger-event to a flat hash suitable for JSON.
(define (ledger-event->hash e)
  (define (h field accessor)
    (cons field (accessor e)))
  (make-hasheq (list (h 'schema-version task-ledger-event-schema-version)
                     (h 'session-seq task-ledger-event-session-seq)
                     (h 'event-id task-ledger-event-event-id)
                     (h 'session-id task-ledger-event-session-id)
                     (h 'project-id task-ledger-event-project-id)
                     (h 'task-id task-ledger-event-task-id)
                     (cons 'parent-task-id (task-ledger-event-parent-task-id e))
                     (h 'branch-id task-ledger-event-branch-id)
                     (h 'turn-id task-ledger-event-turn-id)
                     (h 'request-id task-ledger-event-request-id)
                     (h 'assembly-id task-ledger-event-assembly-id)
                     (h 'correlation-id task-ledger-event-correlation-id)
                     (cons 'causation-id (task-ledger-event-causation-id e))
                     ;; Symbols serialized as strings for JSON compatibility
                     (cons 'source-class (symbol->string (task-ledger-event-source-class e)))
                     (cons 'event-kind (symbol->string (task-ledger-event-event-kind e)))
                     (h 'payload task-ledger-event-payload)
                     (cons 'timestamp (task-ledger-event-timestamp e))
                     (cons 'evidence-refs (task-ledger-event-evidence-refs e))
                     (h 'content-digest task-ledger-event-content-digest))))

;; Deserialize a flat hash back to a task-ledger-event.
(define (hash->ledger-event h)
  (define (s key)
    (hash-ref h key (lambda () (error (format "codec: missing required field ~a" key)))))
  (define (json-null->false v)
    (if (eq? v 'null) #f v))
  (make-task-ledger-event (s 'schema-version)
                          (s 'session-seq)
                          (s 'event-id)
                          (s 'session-id)
                          (s 'project-id)
                          (s 'task-id)
                          (json-null->false (hash-ref h 'parent-task-id #f))
                          (s 'branch-id)
                          (s 'turn-id)
                          (s 'request-id)
                          (s 'assembly-id)
                          (s 'correlation-id)
                          (json-null->false (hash-ref h 'causation-id #f))
                          (string->symbol (s 'source-class))
                          (string->symbol (s 'event-kind))
                          (s 'payload)
                          (json-null->false (hash-ref h 'timestamp #f))
                          (hash-ref h 'evidence-refs '())
                          (s 'content-digest)))

;; ============================================================
;; JSON round-trip
;; ============================================================

;; Serialize to JSON string.
(define (ledger-event->json e)
  (let ([h (ledger-event->hash e)]) (jsexpr->string h)))

;; Deserialize from JSON string.
(define (json->ledger-event json-str)
  (define h (string->jsexpr json-str))
  (hash->ledger-event h))

;; ============================================================
;; Checkpoint -> hash / JSON
;; ============================================================

(define (checkpoint->hash c)
  (make-hasheq (list (cons 'session-id (task-checkpoint-session-id c))
                     (cons 'task-id (task-checkpoint-task-id c))
                     (cons 'branch-id (task-checkpoint-branch-id c))
                     (cons 'last-event-id (task-checkpoint-last-event-id c))
                     (cons 'seq-count (task-checkpoint-seq-count c))
                     (cons 'payload (task-checkpoint-payload c))
                     (cons 'event-count (task-checkpoint-event-count c))
                     (cons 'content-digest (task-checkpoint-content-digest c))
                     (cons 'timestamp (task-checkpoint-timestamp c)))))

(define (hash->checkpoint h)
  (define (s key)
    (hash-ref h key (lambda () (error (format "codec: missing required field ~a" key)))))
  (make-task-checkpoint (s 'session-id)
                        (s 'task-id)
                        (s 'branch-id)
                        (s 'last-event-id)
                        (s 'seq-count)
                        (s 'payload)
                        (s 'event-count)
                        (s 'content-digest)
                        (hash-ref h 'timestamp #f)))

(define (checkpoint->json c)
  (let ([h (checkpoint->hash c)]) (jsexpr->string h)))

(define (json->checkpoint json-str)
  (define h (string->jsexpr json-str))
  (hash->checkpoint h))

;; ============================================================
;; JSONL (newline-delimited JSON) — append-only format
;; ============================================================

;; Serialize a list of events to JSONL string with trailing newline.
(define (events->jsonl events)
  (string-join (map ledger-event->json events) "\n" #:after-last "\n"))

;; Parse a JSONL string (newline-delimited JSON) back to events.
(define (jsonl->events jsonl-str)
  (define lines
    (filter (lambda (l) (and (string? l) (positive? (string-length (string-trim l)))))
            (string-split jsonl-str "\n")))
  (map json->ledger-event lines))
