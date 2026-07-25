#lang racket/base

;; tests/test-task-ledger-codec.rkt — Codec serialization tests
;; STABILITY: internal
;;
;; Tests JSON serialization/deserialization of task-ledger-event structs.

(require rackunit
         racket/match
         racket/hash
         racket/string
         (only-in racket/function thunk)
         "../runtime/task-memory/types.rkt"
         "../runtime/task-memory/codec.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-sample-event meta)
  ;; Build a ledger event with defaults overridden by meta hash
  (define (h key default)
    (hash-ref meta key (lambda () default)))
  (make-task-ledger-event (h 'schema-version 1)
                          (h 'session-seq 1)
                          (h 'event-id "evt-1")
                          (h 'session-id "ses-1")
                          (h 'project-id "proj-1")
                          (h 'task-id "task-1")
                          (h 'parent-task-id #f)
                          (h 'branch-id "branch-1")
                          (h 'turn-id "turn-1")
                          (h 'request-id "req-1")
                          (h 'assembly-id "asm-1")
                          (h 'correlation-id "corr-1")
                          (h 'causation-id #f)
                          (h 'source-class 'runtime-observed)
                          (h 'event-kind 'task-started)
                          (h 'payload (make-hasheq '((description . "Start task"))))
                          (h 'timestamp 1234567890)
                          (h 'evidence-refs '())
                          (h 'content-digest "abc123")))

;; ============================================================
;; 1. Basic round-trip: event -> hash -> event
;; ============================================================

(test-case "ledger-event->hash produces a hash with all fields"
  (define e (make-sample-event #hasheq()))
  (define h (ledger-event->hash e))
  (check-true (hash? h))
  (check-equal? (hash-ref h 'event-id) "evt-1")
  (check-equal? (hash-ref h 'session-id) "ses-1")
  ;; Symbols are serialized as strings in the hash for JSON compatibility
  (check-equal? (hash-ref h 'source-class) "runtime-observed")
  (check-equal? (hash-ref h 'event-kind) "task-started"))

(test-case "hash->ledger-event round-trips to equal event"
  (define e (make-sample-event #hasheq()))
  (define h (ledger-event->hash e))
  (define e2 (hash->ledger-event h))
  ;; Structural eq (transparent struct with same fields)
  (check-equal? e e2))

(test-case "round-trip with optional parent-task-id"
  (define e (make-sample-event #hasheq((parent-task-id . "parent-1"))))
  (define h (ledger-event->hash e))
  (define e2 (hash->ledger-event h))
  (check-equal? e e2))

(test-case "round-trip with causation-id"
  (define e (make-sample-event #hasheq((causation-id . "cause-1"))))
  (define h (ledger-event->hash e))
  (define e2 (hash->ledger-event h))
  (check-equal? e e2))

(test-case "round-trip with evidence-refs"
  (define e (make-sample-event #hasheq((evidence-refs . ("ref-1" "ref-2")))))
  (define h (ledger-event->hash e))
  (define e2 (hash->ledger-event h))
  (check-equal? e e2))

(test-case "round-trip with timestamp #f"
  (define e (make-sample-event #hasheq((timestamp . #f))))
  (define h (ledger-event->hash e))
  (define e2 (hash->ledger-event h))
  (check-equal? e e2))

;; ============================================================
;; 2. JSON serialization
;; ============================================================

(test-case "ledger-event->json produces string"
  (define e (make-sample-event #hasheq()))
  (define json-str (ledger-event->json e))
  (check-true (string? json-str))
  (check-true (positive? (string-length json-str))))

(test-case "ledger-event->json round-trips through hash"
  (define e (make-sample-event #hasheq()))
  (define json-str (ledger-event->json e))
  (define e2 (json->ledger-event json-str))
  (check-equal? (task-ledger-event-event-id e2) (task-ledger-event-event-id e))
  (check-equal? (task-ledger-event-session-id e2) (task-ledger-event-session-id e))
  (check-equal? (task-ledger-event-source-class e2) (task-ledger-event-source-class e))
  (check-equal? (task-ledger-event-event-kind e2) (task-ledger-event-event-kind e))
  (check-equal? (task-ledger-event-schema-version e2) (task-ledger-event-schema-version e))
  (check-equal? (task-ledger-event-session-seq e2) (task-ledger-event-session-seq e))
  (check-equal? (task-ledger-event-timestamp e2) (task-ledger-event-timestamp e))
  (check-equal? (task-ledger-event-evidence-refs e2) (task-ledger-event-evidence-refs e))
  (check-equal? (task-ledger-event-content-digest e2) (task-ledger-event-content-digest e))
  (check-equal? (task-ledger-event-branch-id e2) (task-ledger-event-branch-id e))
  (check-equal? (task-ledger-event-turn-id e2) (task-ledger-event-turn-id e))
  (check-equal? (task-ledger-event-request-id e2) (task-ledger-event-request-id e))
  (check-equal? (task-ledger-event-assembly-id e2) (task-ledger-event-assembly-id e))
  (check-equal? (task-ledger-event-correlation-id e2) (task-ledger-event-correlation-id e))
  (check-equal? (task-ledger-event-parent-task-id e2) (task-ledger-event-parent-task-id e))
  (check-equal? (task-ledger-event-causation-id e2) (task-ledger-event-causation-id e))
  (check-equal? (task-ledger-event-project-id e2) (task-ledger-event-project-id e))
  (check-equal? (task-ledger-event-task-id e2) (task-ledger-event-task-id e)))

(test-case "round-trip via JSON preserves content-digest"
  (define e (make-sample-event #hasheq((content-digest . "sha256:deadbeef"))))
  (define json-str (ledger-event->json e))
  (define e2 (json->ledger-event json-str))
  (check-equal? (task-ledger-event-content-digest e2) "sha256:deadbeef"))

(test-case "round-trip via JSON preserves causality chain"
  (define e (make-sample-event #hasheq((causation-id . "evt-0") (event-id . "evt-1"))))
  (define e2 (json->ledger-event (ledger-event->json e)))
  (check-equal? (task-ledger-event-causation-id e2) "evt-0")
  (check-equal? (task-ledger-event-event-id e2) "evt-1"))

;; ============================================================
;; 3. Invalid input handling
;; ============================================================

(test-case "json->ledger-event with missing field raises error"
  (check-exn #rx"codec: missing required field"
             (thunk (json->ledger-event "{\"event-id\":\"evt-1\"}"))))

(test-case "json->ledger-event with invalid JSON raises error"
  (check-exn exn:fail? (thunk (json->ledger-event "not-json"))))

(test-case "json->ledger-event with wrong type for field raises error"
  (check-exn
   #rx"task-ledger-event: invalid field"
   (thunk
    (json->ledger-event
     "{\"schema-version\":\"wrong\",\"session-seq\":1,\"event-id\":\"e1\",\"session-id\":\"s1\",\"project-id\":\"p1\",\"task-id\":\"t1\",\"parent-task-id\":null,\"branch-id\":\"b1\",\"turn-id\":\"tu1\",\"request-id\":\"r1\",\"assembly-id\":\"a1\",\"correlation-id\":\"c1\",\"causation-id\":null,\"source-class\":\"runtime-observed\",\"event-kind\":\"task-started\",\"payload\":{},\"timestamp\":1,\"evidence-refs\":[],\"content-digest\":\"d1\"}"))))

;; ============================================================
;; 4. JSONL format (append-only line format)
;; ============================================================

(test-case "events->jsonl concatenates JSON lines"
  (define e1 (make-sample-event #hasheq((event-id . "evt-1"))))
  (define e2 (make-sample-event #hasheq((event-id . "evt-2"))))
  (define line1 (string-trim (ledger-event->json e1)))
  (define line2 (string-trim (ledger-event->json e2)))
  (define jsonl-str (events->jsonl (list e1 e2)))
  (check-true (string-suffix? jsonl-str "\n"))
  (check-equal? (string-trim (car (string-split jsonl-str "\n"))) line1))

(test-case "jsonl->events reads back list"
  (define e1 (make-sample-event #hasheq((event-id . "evt-1"))))
  (define e2 (make-sample-event #hasheq((event-id . "evt-2"))))
  (define jsonl-str (events->jsonl (list e1 e2)))
  (define events (jsonl->events jsonl-str))
  (check-equal? (length events) 2)
  (check-equal? (task-ledger-event-event-id (car events)) "evt-1")
  (check-equal? (task-ledger-event-event-id (cadr events)) "evt-2"))

(test-case "jsonl->events handles trailing newlines"
  (define e1 (make-sample-event #hasheq((event-id . "evt-1"))))
  (define jsonl-str (events->jsonl (list e1)))
  (check-equal? (length (jsonl->events (string-append jsonl-str "\n\n"))) 1)
  (check-equal? (length (jsonl->events (string-append jsonl-str "\n"))) 1))

;; ============================================================
;; 5. Multi-event round-trip fidelity
;; ============================================================

(test-case "batch round-trip preserves ordering"
  (define events
    (for/list ([i (in-range 3)])
      (make-sample-event (hasheq (quote event-id)
                                 (format "evt-~a" i)
                                 (quote session-seq)
                                 (+ 1 i)
                                 (quote timestamp)
                                 i))))
  (define jsonl (events->jsonl events))
  (define restored (jsonl->events jsonl))
  (for ([e1 events]
        [e2 restored])
    (check-equal? (task-ledger-event-event-id e1) (task-ledger-event-event-id e2))
    (check-equal? (task-ledger-event-session-seq e1) (task-ledger-event-session-seq e2))
    (check-equal? (task-ledger-event-timestamp e1) (task-ledger-event-timestamp e2))
    (check-equal? (task-ledger-event-content-digest e1) (task-ledger-event-content-digest e2))
    (check-equal? (task-ledger-event-branch-id e1) (task-ledger-event-branch-id e2))
    (check-equal? (task-ledger-event-source-class e1) (task-ledger-event-source-class e2))
    (check-equal? (task-ledger-event-event-kind e1) (task-ledger-event-event-kind e2)))

  ;; ============================================================
  ;; 6. Checkpoint round-trip
  ;; ============================================================

  (test-case "checkpoint->hash round-trips"
    (define c
      (make-task-checkpoint "ses-1"
                            "task-1"
                            "branch-1"
                            "evt-10"
                            10
                            (make-hasheq '((status . "active")))
                            5
                            "sha256:check"
                            1234567890))
    (define h (checkpoint->hash c))
    (define c2 (hash->checkpoint h))
    (check-equal? (task-checkpoint-session-id c2) (task-checkpoint-session-id c))
    (check-equal? (task-checkpoint-task-id c2) (task-checkpoint-task-id c))
    (check-equal? (task-checkpoint-branch-id c2) (task-checkpoint-branch-id c))
    (check-equal? (task-checkpoint-last-event-id c2) (task-checkpoint-last-event-id c))
    (check-equal? (task-checkpoint-seq-count c2) (task-checkpoint-seq-count c))
    (check-equal? (task-checkpoint-event-count c2) (task-checkpoint-event-count c))
    (check-equal? (task-checkpoint-content-digest c2) (task-checkpoint-content-digest c))
    (check-equal? (task-checkpoint-timestamp c2) (task-checkpoint-timestamp c)))

  (test-case "checkpoint->json round-trips"
    (define c
      (make-task-checkpoint "ses-1"
                            "task-1"
                            "branch-1"
                            "evt-10"
                            10
                            (make-hasheq '((status . "active")))
                            5
                            "sha256:check"
                            1234567890))
    (define c2 (json->checkpoint (checkpoint->json c)))
    (check-equal? (task-checkpoint-session-id c2) (task-checkpoint-session-id c))
    (check-equal? (task-checkpoint-task-id c2) (task-checkpoint-task-id c))
    (check-equal? (task-checkpoint-branch-id c2) (task-checkpoint-branch-id c))
    (check-equal? (task-checkpoint-last-event-id c2) (task-checkpoint-last-event-id c))
    (check-equal? (task-checkpoint-seq-count c2) (task-checkpoint-seq-count c))
    (check-equal? (task-checkpoint-event-count c2) (task-checkpoint-event-count c))
    (check-equal? (task-checkpoint-content-digest c2) (task-checkpoint-content-digest c))
    (check-equal? (task-checkpoint-timestamp c2) (task-checkpoint-timestamp c)))

  (test-case "checkpoint round-trip preserves timestamp #f"
    (define c
      (make-task-checkpoint "ses-1"
                            "task-1"
                            "branch-1"
                            "evt-10"
                            10
                            (make-hasheq '((status . "active")))
                            5
                            "sha256:check"
                            #f))
    (define c2 (json->checkpoint (checkpoint->json c)))
    (check-equal? (task-checkpoint-session-id c2) (task-checkpoint-session-id c))
    (check-equal? (task-checkpoint-timestamp c2) #f))

  (test-case "checkpoint->json with full fields produces valid JSON"
    (define c
      (make-task-checkpoint "ses-1"
                            "task-1"
                            "branch-1"
                            "evt-10"
                            10
                            (make-hasheq '((status . "active")))
                            5
                            "sha256:check"
                            1234567890))
    (define json-str (checkpoint->json c))
    (check-true (string? json-str))
    (check-true (positive? (string-length json-str)))
    ;; Valid JSON — read it back
    (define c2 (json->checkpoint json-str))
    (check-equal? (task-checkpoint-session-id c2) (task-checkpoint-session-id c))
    (check-equal? (task-checkpoint-seq-count c2) 10)
    (check-equal? (task-checkpoint-timestamp c2) 1234567890)))
