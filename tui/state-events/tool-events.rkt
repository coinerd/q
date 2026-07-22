#lang racket/base

;; tui/state-events/tool-events.rkt -- Tool event handlers
;; STABILITY: internal
;;
;; Extracted from core-handlers.rkt. Handles tool call lifecycle,
;; tool execution progress, and auto-retry events.
;; Registration side-effects happen at module load time.

(require racket/string
         racket/match
         racket/list
         (only-in "../../util/event/event.rkt" event event-ev event-payload event-time event?)
         (only-in "../../util/message/message.rkt" message)
         (only-in "../../util/content/content-helpers.rkt" tool-result-content->string)
         "../state-types.rkt"
         "handler-helpers.rkt"
         "helpers.rkt"
         "registry.rkt")

;; ============================================================
;; Tool handlers
;; ============================================================

(define (handle-tool-call-started state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'name "?"))
  ;; MF2 (v0.99.5): Clear stale streaming text when transitioning to tool
  ;; execution. Without this, old streaming-text triggers false watchdog
  ;; stall detection during tool calls.
  (define cleared-stream (clear-streaming state))
  (if (recent-tool-start? state name)
      (set-streaming-phase (set-pending-tool-name (set-busy cleared-stream #t) name) 'tool-pending)
      (let* ([args-raw (hash-ref payload 'arguments #f)]
             [arg-summary (if args-raw
                              (extract-arg-summary args-raw)
                              "")]
             [text arg-summary]
             [ts (event-time evt)]
             [meta (hasheq 'name name 'arguments (or args-raw ""))]
             [new-state (append-entry cleared-stream (make-entry 'tool-start text ts meta))])
        (if (ui-state-pending-tool-name state)
            (set-busy new-state #t)
            (set-pending-tool-name (set-busy new-state #t) name)))))

(define (handle-tool-execution-started state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'toolName "?"))
  (if (recent-tool-start? state name)
      (set-streaming-phase (set-pending-tool-name (set-busy state #t) name) 'tool-pending)
      (let* ([args-raw (hash-ref payload 'arguments #f)]
             [arg-summary (if args-raw
                              (extract-arg-summary args-raw)
                              "")]
             [text arg-summary]
             [ts (event-time evt)]
             [meta (hasheq 'name name 'arguments (or args-raw ""))]
             [new-state (append-entry state (make-entry 'tool-start text ts meta))])
        (if (ui-state-pending-tool-name state)
            (set-busy new-state #t)
            (set-pending-tool-name (set-busy new-state #t) name)))))

(define (handle-tool-execution-completed state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'toolName (lambda () (hash-ref payload 'name "?"))))
  (define result-summary
    (hash-ref payload 'resultSummary (lambda () (if (hash-ref payload 'error #f) 'error 'completed))))
  (define result-raw (hash-ref payload 'result #f))
  (define error-raw (or (hash-ref payload 'resultError #f) (hash-ref payload 'error #f)))
  (define ts (event-time evt))
  (if (recent-tool-end? state name)
      (set-pending-tool-name state #f)
      (if (eq? result-summary 'completed)
          (let* ([result-text
                  (if result-raw
                      (string-replace (truncate-string (tool-result-content->string result-raw) 80)
                                      "
"
                                      " | ")
                      "")]
                 [text result-text]
                 [meta (hasheq 'name name 'result (or result-raw ""))])
            (set-pending-tool-name (append-entry state (make-entry 'tool-end text ts meta)) #f))
          (let* ([err (or error-raw "tool failed")]
                 [text (string-replace err "
" " | ")]
                 [meta (hasheq 'name name 'error err)])
            (set-pending-tool-name (append-entry state (make-entry 'tool-fail text ts meta)) #f)))))

;; B3 fix: Show progress during long-running tool batches
(define (handle-tool-execution-update state evt)
  (define payload (event-payload evt))
  (define tool-name (hash-ref payload 'toolName "?"))
  (define progress (hash-ref payload 'progress (hasheq)))
  (define total (hash-ref progress 'total 0))
  (define running (hash-ref progress 'running 0))
  (define status-text (tool-progress-status-text tool-name total running))
  (set-status-message state status-text))

(define (handle-tool-call-blocked state evt)
  (define payload (event-payload evt))
  (define name (hash-ref payload 'name "?"))
  (define reason (hash-ref payload 'reason "blocked by extension"))
  (set-pending-tool-name (append-entry state
                                       (make-entry 'system
                                                   (format "[tool blocked: ~a -- ~a]" name reason)
                                                   (event-time evt)
                                                   (hasheq 'name name)))
                         #f))

(define (handle-auto-retry state evt)
  (define payload (event-payload evt))
  (define attempt (hash-ref payload 'attempt "?"))
  (define max-attempts (hash-ref payload 'max-attempts "?"))
  (define error-type (hash-ref payload 'error-type #f))
  (define type-label (retry-error-type-label error-type))
  (define msg
    (if type-label
        (format "[retry: ~a, ~a/~a...]" type-label attempt max-attempts)
        (format "[retry: attempt ~a/~a]" attempt max-attempts)))
  (clear-streaming (append-entry state (make-entry 'system msg (event-time evt) (hash)))))

(define (handle-auto-retry-lifecycle state evt)
  (define ev (event-ev evt))
  (define payload (event-payload evt))
  (match ev
    ["auto-retry.start"
     (define attempt (hash-ref payload 'attempt "?"))
     (define max-attempts (hash-ref payload 'maxRetries "?"))
     (define error-type (hash-ref payload 'errorType #f))
     (define type-label (retry-error-type-label error-type))
     (define msg
       (if type-label
           (format "[retry: ~a, ~a/~a...]" type-label attempt max-attempts)
           (format "[retry: attempt ~a/~a]" attempt max-attempts)))
     (clear-streaming (append-entry state (make-entry 'system msg (event-time evt) (hash))))]
    ["auto-retry.context-reduced"
     (define original (hash-ref payload 'original-messages 0))
     (define reduced (hash-ref payload 'reduced-messages 0))
     (append-entry state
                   (make-entry 'system
                               (format "[retry: reduced context ~a -> ~a messages]" original reduced)
                               (event-time evt)
                               (hash)))]
    [_ state]))

;; ============================================================
;; Register handlers at module load time
;; ============================================================

(register-event-reducer! "tool.call.started" handle-tool-call-started)
(register-event-reducer! "tool.execution.started" handle-tool-execution-started)
(register-event-reducer! "tool.execution.completed" handle-tool-execution-completed)
(register-event-reducer! "tool.execution.updated" handle-tool-execution-update)
(register-event-reducer! "tool.call.blocked" handle-tool-call-blocked)
(register-event-reducer! "auto-retry" handle-auto-retry)
(register-event-reducer! "auto-retry.start" handle-auto-retry-lifecycle)
(register-event-reducer! "auto-retry.context-reduced" handle-auto-retry-lifecycle)
