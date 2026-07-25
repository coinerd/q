#lang racket/base

;; runtime/task-memory/outcome-capture.rkt
;; STABILITY: internal
;;
;; W3B (#8940): Runtime capture — typed-tool-outcome → task-ledger-event.
;;
;; This module is the runtime-side translator that converts safe typed
;; outcomes (produced at the tool boundary by util/outcome/outcome-types.rkt)
;; into durable, append-only task-ledger-events.
;;
;; Layering: this is a RUNTIME module. It imports util/outcome (safe) and
;; runtime/task-memory/types (the ledger). Tools never see this module —
;; they only emit typed-tool-outcomes. The wiring layer calls this translator.
;;
;; Kind mapping (outcome-kind, status) → event-kind:
;;   artifact + success  → artifact-modified
;;   test + success      → verification-passed
;;   test + error        → verification-failed
;;   commit + success    → commit-created
;;   push + success      → push-completed
;;   git-status + *      → tool-invoked
;;   conclusion + *      → objective-set
;;   <any> + error       → error-occurred

(require racket/contract
         racket/list
         file/sha1
         racket/port
         "../../util/ids.rkt"
         "../../util/outcome/outcome-types.rkt"
         "types.rkt")

(provide capture-context
         capture-context?
         capture-context-session-id
         capture-context-project-id
         capture-context-task-id
         capture-context-parent-task-id
         capture-context-branch-id
         capture-context-turn-id
         capture-context-request-id
         capture-context-assembly-id
         capture-context-session-seq
         make-capture-context
         outcome-kind->event-kind
         capture-outcome->event
         capture-outcomes->events
         compute-payload-digest)

;; ============================================================
;; Capture context
;; ============================================================

;; Holds the identifying context needed to construct a task-ledger-event
;; from a typed-tool-outcome. All fields are strings (or #f for parent).
(struct capture-context
        (session-id project-id
                    task-id
                    parent-task-id
                    branch-id
                    turn-id
                    request-id
                    assembly-id
                    session-seq)
  #:transparent
  #:constructor-name make-capture-context-internal)

(define (make-capture-context #:session-id session-id
                              #:project-id project-id
                              #:task-id task-id
                              #:parent-task-id parent-task-id
                              #:branch-id branch-id
                              #:turn-id turn-id
                              #:request-id request-id
                              #:assembly-id assembly-id
                              #:session-seq session-seq)
  (unless (and (string? session-id)
               (string? project-id)
               (string? task-id)
               (or (not parent-task-id) (string? parent-task-id))
               (string? branch-id)
               (string? turn-id)
               (string? request-id)
               (string? assembly-id)
               (exact-positive-integer? session-seq))
    (error 'make-capture-context "invalid context fields"))
  (make-capture-context-internal session-id
                                 project-id
                                 task-id
                                 parent-task-id
                                 branch-id
                                 turn-id
                                 request-id
                                 assembly-id
                                 session-seq))

;; ============================================================
;; Kind mapping
;; ============================================================

;; Map an (outcome-kind, status) pair to a canonical event-kind.
;; Test errors are verification failures (the test ran and failed);
;; other errors produce error-occurred.
(define (outcome-kind->event-kind kind status)
  (cond
    [(and (eq? kind 'test) (eq? status 'error)) 'verification-failed]
    [(eq? status 'error) 'error-occurred]
    [(eq? kind 'artifact) 'artifact-modified]
    [(eq? kind 'test) 'verification-passed]
    [(eq? kind 'commit) 'commit-created]
    [(eq? kind 'push) 'push-completed]
    [(eq? kind 'git-status) 'tool-invoked]
    [(eq? kind 'conclusion) 'objective-set]
    [(eq? kind 'prompt) 'objective-set]
    [(eq? kind 'task-transition) 'phase-changed]
    [(eq? kind 'checkpoint) 'checkpoint-created]
    [(eq? kind 'archive) 'state-archived]
    [else 'tool-invoked]))

;; ============================================================
;; Payload digest
;; ============================================================

;; Compute a content-digest for a payload hash.
;; Uses sha1 of a canonical serialization (key-sorted) for integrity.
;; This is an integrity checksum, not a security hash.
(define (compute-payload-digest payload)
  (define keys (sort (hash-keys payload) symbol<?))
  (define parts
    (for/list ([k (in-list keys)])
      (format "~a=~a" k (hash-ref payload k))))
  (define canonical (string-join parts "&"))
  (bytes->hex-string (sha1-bytes (open-input-string canonical))))

;; Helper: symbol<? for sorting
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;; Helper: string-join (avoid requiring racket/string for minimal deps)
(define (string-join parts sep)
  (if (null? parts)
      ""
      (let loop ([xs (cdr parts)]
                 [acc (car parts)])
        (if (null? xs)
            acc
            (loop (cdr xs) (string-append acc sep (car xs)))))))

;; ============================================================
;; Outcome → Event translation
;; ============================================================

;; Translate a single typed-tool-outcome into a task-ledger-event.
;; Returns #f if the outcome is malformed in a way that prevents capture.
(define (capture-outcome->event ctx outcome)
  (define kind (typed-tool-outcome-kind outcome))
  (define status (typed-tool-outcome-status outcome))
  (define event-kind (outcome-kind->event-kind kind status))
  (define payload (typed-tool-outcome-payload outcome))
  (define digest (compute-payload-digest payload))
  (define ts (or (typed-tool-outcome-timestamp outcome) (current-seconds)))
  (make-task-ledger-event 1 ; schema-version
                          (capture-context-session-seq ctx)
                          (generate-id) ; event-id
                          (capture-context-session-id ctx)
                          (capture-context-project-id ctx)
                          (capture-context-task-id ctx)
                          (capture-context-parent-task-id ctx)
                          (capture-context-branch-id ctx)
                          (capture-context-turn-id ctx)
                          (capture-context-request-id ctx)
                          (capture-context-assembly-id ctx)
                          (or (typed-tool-outcome-tool-call-id outcome)
                              (generate-id)) ; correlation-id
                          #f ; causation-id
                          'runtime-observed ; source-class
                          event-kind
                          payload
                          ts
                          '() ; evidence-refs
                          digest))

;; Translate a list of outcomes into a list of events (preserving order).
(define (capture-outcomes->events ctx outcomes)
  (for/list ([o (in-list outcomes)]
             #:when (typed-tool-outcome? o))
    (capture-outcome->event ctx o)))
