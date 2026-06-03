#lang racket/base
;; runtime/session/lifecycle-state.rkt — Lifecycle state extracted from agent-session
;; A1-05: God Struct Decomposition — extracted mutable lifecycle fields
;; STABILITY: stable

(provide (struct-out lifecycle-state)
         make-lifecycle-state)

;; Lifecycle state: mutable flags that track session lifecycle.
;; Extracted from agent-session (24 fields → 15 fields + this struct).
;; Only lifecycle code (compaction, shutdown, prompt-running, task-fsm)
;; should access these fields directly.

(struct lifecycle-state
        ([compacting? #:mutable]          ; boolean — guard against recursive compaction
         [last-compaction-time #:mutable]  ; integer or #f — timestamp of last compaction
         [persisted? #:mutable]            ; boolean — #f until directory + first write
         [shutdown-requested? #:mutable]   ; boolean — graceful shutdown flag
         [force-shutdown? #:mutable]       ; boolean — force immediate shutdown
         [prompt-running? #:mutable]       ; boolean — is a prompt currently executing?
         [task-fsm-state #:mutable]        ; symbol or #f — current task FSM state
         [task-conclusions #:mutable]      ; (listof task-conclusion?) — agent task conclusions
         [recent-tool-calls #:mutable])    ; (listof symbol?) — recent tool call history
  #:transparent)

;; Constructor: create a lifecycle-state with safe defaults.
(define (make-lifecycle-state)
  (lifecycle-state #f #f #f #f #f #f 'idle '() '()))
