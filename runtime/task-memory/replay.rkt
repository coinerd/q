#lang racket

;; runtime/task-memory/replay.rkt — Ledger event replay and checkpointing
;; STABILITY: internal
;;
;; Replays a list of ledger events deterministically to produce a
;; task-checkpoint. Supports branch-aware walking, duplicate
;; deduplication, and legacy JSONL store replay.

(require racket/match
         racket/hash
         racket/sequence
         racket/string
         json
         (only-in racket/function const)
         "types.rkt"
         "codec.rkt")

(provide replay-events
         branch-walk-events
         legacy-store-replay
         write-checkpoint-to-file
         read-checkpoint-from-file)

;; ============================================================
;; Event deduplication: keep only the first of each (session-seq, event-id)
;; ============================================================

;; Dedup events preserving order, keeping first occurrence.
;; Input events are sorted first, then deduplicated by (session-seq, event-id).
(define (deduplicate-events events)
  (define sorted (sort events ledger-event-before?))
  (define seen (make-hash)) ;; equal?-based for pair keys
  (for/list ([e (in-list sorted)]
             #:unless
             (hash-ref seen
                       (cons (task-ledger-event-session-seq e) (task-ledger-event-event-id e))
                       #f))
    (hash-set! seen (cons (task-ledger-event-session-seq e) (task-ledger-event-event-id e)) #t)
    e))

;; ============================================================
;; Replay event list to checkpoint
;; ============================================================

;; replay-events : (listof task-ledger-event?) string string string [task-checkpoint?] -> task-checkpoint?
;;
;; Replay a list of events for a given session/task/branch.
;; Events are sorted by (session-seq, event-id) deterministically.
;; Deduplicates by (session-seq, event-id).
;; Produces a checkpoint with accumulated state.
;; Optionally continues from an existing checkpoint.
(define (replay-events events session-id task-id branch-id #:from-checkpoint [base-cp #f])
  (define sorted-unique (deduplicate-events events))
  (define init-cp
    (or base-cp
        (make-task-checkpoint session-id
                              task-id
                              branch-id
                              "" ;; last-event-id
                              0 ;; seq-count
                              (make-hasheq)
                              0 ;; event-count
                              "none" ;; content-digest (no digest yet)
                              #f))) ;; timestamp
  (for/fold ([cp init-cp])
            ([e (in-list sorted-unique)]
             #:when (or (equal? (task-ledger-event-branch-id e) branch-id)
                        (equal? branch-id "*"))) ;; "*" means all branches
    (apply-event-to-checkpoint cp e)))

;; Apply a single event to an existing checkpoint, returning a new checkpoint.
(define (apply-event-to-checkpoint cp e)
  (define prev-payload (task-checkpoint-payload cp))
  (define event-payload (task-ledger-event-payload e))
  (define merged-payload (hash-union event-payload prev-payload #:combine (lambda (a b) a)))
  (make-task-checkpoint (task-checkpoint-session-id cp)
                        (task-checkpoint-task-id cp)
                        (task-checkpoint-branch-id cp)
                        (task-ledger-event-event-id e)
                        (task-ledger-event-session-seq e)
                        merged-payload
                        (+ (task-checkpoint-event-count cp) 1)
                        (task-ledger-event-content-digest e)
                        (task-ledger-event-timestamp e)))

;; ============================================================
;; Branch-aware walk
;; ============================================================

;; branch-walk-events : (listof task-ledger-event?) string string string [task-checkpoint?] -> task-checkpoint?
;;
;; Filter events by branch-id and replay them. Optionally starts from
;; an existing checkpoint.
(define (branch-walk-events events session-id task-id branch-id #:from-checkpoint [base-cp #f])
  (define branch-events
    (filter (lambda (e) (equal? (task-ledger-event-branch-id e) branch-id)) events))
  (if base-cp
      ;; Start from existing checkpoint: only apply events after it
      (replay-events (filter-events-after-checkpoint branch-events base-cp)
                     session-id
                     task-id
                     branch-id
                     #:from-checkpoint base-cp)
      (replay-events branch-events session-id task-id branch-id)))

;; Filter events that come after the checkpoint's last event,
;; handling dedup via compare.
(define (filter-events-after-checkpoint events cp)
  (define cp-seq (task-checkpoint-seq-count cp))
  (define cp-last-id (task-checkpoint-last-event-id cp))
  (for/list ([e (in-list events)]
             #:when (or (> (task-ledger-event-session-seq e) cp-seq)
                        (and (= (task-ledger-event-session-seq e) cp-seq)
                             (string>? (task-ledger-event-event-id e) cp-last-id))))
    e))

;; ============================================================
;; Legacy store bridge
;; ============================================================

;; legacy-store-replay : string string string string -> task-checkpoint?
;;
;; Parse JSONL string, deserialize each line as a ledger event,
;; and replay them for the given session/task/branch.
(define (legacy-store-replay jsonl-str session-id task-id branch-id)
  (define events
    (for/list ([line (in-list (string-split jsonl-str "\n"))]
               #:when (and (string? line) (positive? (string-length (string-trim line)))))
      (with-handlers ([exn:fail? (lambda (exn) #f)])
        (json->ledger-event line))))
  (define valid-events (filter (lambda (e) (and e (task-ledger-event? e))) events))
  (replay-events valid-events session-id task-id branch-id))

;; ============================================================
;; Checkpoint file I/O
;; ============================================================

;; Write a checkpoint to a JSON file.
(define (write-checkpoint-to-file cp file-path)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (call-with-output-file file-path
                           (lambda (out)
                             (write-string (checkpoint->json cp) out)
                             (newline out))
                           #:exists 'replace)
    #t))

;; Read a checkpoint from a JSON file.
(define (read-checkpoint-from-file file-path)
  (unless (file-exists? file-path)
    (error "checkpoint file not found" file-path))
  (call-with-input-file file-path
                        (lambda (in)
                          (define json-str (port->string in))
                          (json->checkpoint (string-trim json-str)))))
