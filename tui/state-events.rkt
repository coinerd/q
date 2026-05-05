#lang racket/base

;; tui/state-events.rkt — Event→state reduction for UI state
;;
;; Pure function: apply-event-to-state and helpers.
;; Split from state.rkt (v0.22.6 W2) to keep each module under 400 lines.

(require racket/string
         racket/list
         "../util/protocol-types.rkt"
         "../util/cost-tracker.rkt"
         "../util/content-helpers.rkt"
         (only-in "../extensions/gsd/state-machine.rkt" gsm-current)
         "state-types.rkt")

(provide apply-event-to-state)

;; Local helper (avoids circular dependency with state-ui)
(define (ui-model-label state)
  (or (ui-state-model-name state) "no model"))

;; Truncate error messages for status bar display
(define (truncate-status-msg msg)
  (define clean (string-replace (string-trim msg) "\n" " "))
  (if (> (string-length clean) 40)
      (string-append (substring clean 0 37) "...")
      clean))

;; ============================================================
;; Event reduction
;; ============================================================

(define (apply-event-to-state state evt)
  (define ev (event-ev evt))
  (define payload (event-payload evt))

  (define (append-entry st entry)
    (define-values (id-entry st1) (assign-entry-id entry st))
    (struct-copy ui-state st1 [transcript (cons id-entry (ui-state-transcript st1))]))

  ;; v0.29.17 W1: Dedup guard — prevent duplicate tool-start entries when
  ;; both raw "tool.call.started" and typed "tool-execution-start" fire.
  (define (recent-tool-start? st name)
    (define transcript (ui-state-transcript st))
    (and (not (null? transcript))
         (let ([last (car transcript)])
           (and (eq? (transcript-entry-kind last) 'tool-start)
                (equal? (hash-ref (transcript-entry-meta last) 'name "") name)))))

  (case ev
    [("assistant.message.completed")
     (define streamed (ui-state-streaming-text state))
     (define content (or streamed (hash-ref payload 'content "")))
     (define ts (event-time evt))
     ;; v0.28.21 W0: Persist thinking when content is empty (tool-call turn)
     (define thinking (ui-state-streaming-thinking state))
     (define s0
       (if (and thinking
                (> (string-length (string-trim thinking)) 0)
                (string=? (string-trim content) ""))
           (append-entry state (make-entry 'thinking thinking ts (hash)))
           state))
     (struct-copy ui-state
                  (append-entry s0 (make-entry 'assistant content ts (hash)))
                  [busy? #f]
                  [pending-tool-name #f]
                  [streaming-text #f]
                  [streaming-thinking #f])]

    [("tool.call.started")
     (let* ([name (hash-ref payload 'name "?")])
       (if (recent-tool-start? state name)
           (struct-copy ui-state state (busy? #t) (pending-tool-name name))
           (let* ([args-raw (hash-ref payload 'arguments #f)]
                  [arg-summary (if args-raw
                                   (extract-arg-summary args-raw)
                                   "")]
                  [text (if (string=? arg-summary "")
                            (format "[TOOL: ~a]" name)
                            (format "[TOOL: ~a] ~a" name arg-summary))]
                  [ts (event-time evt)]
                  [meta (hasheq 'name name 'arguments (or args-raw ""))]
                  [new-state (append-entry state (make-entry 'tool-start text ts meta))])
             (if (ui-state-pending-tool-name state)
                 (struct-copy ui-state new-state (busy? #t))
                 (struct-copy ui-state new-state (busy? #t) (pending-tool-name name))))))]

    [("tool-execution-start")
     (let* ([name (hash-ref payload 'tool-name "?")])
       (if (recent-tool-start? state name)
           (struct-copy ui-state state (busy? #t) (pending-tool-name name))
           (let* ([args-raw (hash-ref payload 'arguments #f)]
                  [arg-summary (if args-raw
                                   (extract-arg-summary args-raw)
                                   "")]
                  [text (if (string=? arg-summary "")
                            (format "[TOOL: ~a]" name)
                            (format "[TOOL: ~a] ~a" name arg-summary))]
                  [ts (event-time evt)]
                  [meta (hasheq 'name name 'arguments (or args-raw ""))]
                  [new-state (append-entry state (make-entry 'tool-start text ts meta))])
             (if (ui-state-pending-tool-name state)
                 (struct-copy ui-state new-state (busy? #t))
                 (struct-copy ui-state new-state (busy? #t) (pending-tool-name name))))))]

    [("tool-execution-end")
     (define name (hash-ref payload 'tool-name "?"))
     (define result-summary (hash-ref payload 'result-summary 'error))
     (define ts (event-time evt))
     (if (eq? result-summary 'completed)
         (let* ([text (format "[OK: ~a]" name)]
                [meta (hasheq 'name name)])
           (struct-copy ui-state
                        (append-entry state (make-entry 'tool-end text ts meta))
                        [pending-tool-name #f]))
         (let* ([err "tool failed"]
                [text (string-replace (format "[FAIL: ~a] ~a" name err) "\n" " \u23ce ")]
                [meta (hasheq 'name name 'error err)])
           (struct-copy ui-state
                        (append-entry state (make-entry 'tool-fail text ts meta))
                        [pending-tool-name #f])))]

    [("runtime.error")
     (define err (hash-ref payload 'error "unknown error"))
     (define ts (event-time evt))
     (define error-type
       (hash-ref
        payload
        'errorType
        (lambda ()
          (cond
            [(regexp-match? #rx"[Tt]imeout|timed out" err) 'timeout]
            [(regexp-match? #rx"429|[Rr]ate.?[Ll]imit" err) 'rate-limit]
            [(regexp-match? #rx"401|403|[Aa]uth|[Uu]nauthorized" err) 'auth]
            [(regexp-match? #rx"context.*overflow|[Tt]oo.*long|[Mm]ax.*tokens" err) 'context-overflow]
            [else 'provider-error]))))
     (define retries-attempted (hash-ref payload 'retries-attempted #f))
     (define error-history (hash-ref payload 'errorHistory '()))
     (define history-types (remove-duplicates error-history))
     (define hint
       (cond
         [(and retries-attempted (> retries-attempted 0))
          (cond
            [(and (member 'timeout history-types) (member 'rate-limit history-types))
             (format "Provider timed out, then rate limited (~a retries). Wait 30s, then type /retry."
                     retries-attempted)]
            [(> (length history-types) 1)
             (format "Mixed errors after ~a retries. Wait a moment, then type /retry."
                     retries-attempted)]
            [else
             (case error-type
               [(rate-limit)
                (format "Rate limit persisted after ~a retries. Wait a moment, then type /retry."
                        retries-attempted)]
               [(timeout)
                (format "Provider timed out after ~a retries. Type /retry to resubmit."
                        retries-attempted)]
               [else
                (format "Error persisted after ~a retries. Type /retry to resubmit."
                        retries-attempted)])])]
         [else
          (case error-type
            [(timeout) "Provider timed out. Type /retry to resubmit your prompt."]
            [(rate-limit) "Rate limited. Will retry automatically."]
            [(auth) "API key error. Check ~/.q/config.json"]
            [(context-overflow) "Context too long. Use /compact to reduce, then /retry."]
            [(max-iterations) "Max iterations reached. Simplify your request or use /compact."]
            [(internal-error) "Internal error occurred. Type /retry to resubmit your prompt."]
            [else "Type /retry to resubmit your prompt."])]))
     (define streamed (ui-state-streaming-text state))
     (define s0
       (if (and streamed (> (string-length (string-trim streamed)) 0))
           (append-entry state (make-entry 'assistant streamed ts (hasheq 'partial #t)))
           state))
     (define s1
       (struct-copy ui-state
                    s0
                    [busy? #f]
                    [pending-tool-name #f]
                    [streaming-text #f]
                    [streaming-thinking #f]
                    [status-message (truncate-status-msg err)]))
     (define s2 (append-entry s1 (make-entry 'error (format "Error: ~a" err) ts (hash))))
     (append-entry s2 (make-entry 'system hint ts (hash)))]

    [("session.started")
     (define sid (hash-ref payload 'sessionId ""))
     (define s1 (struct-copy ui-state state [session-id sid]))
     (append-entry s1
                   (make-entry 'system (format "Session started: ~a" sid) (event-time evt) (hash)))]

    [("session.resumed")
     (define sid (hash-ref payload 'sessionId ""))
     (define s1 (struct-copy ui-state state [session-id sid]))
     (append-entry s1
                   (make-entry 'system (format "Session resumed: ~a" sid) (event-time evt) (hash)))]

    [("model.stream.delta")
     (define delta (hash-ref payload 'delta ""))
     (define current-streaming (ui-state-streaming-text state))
     (define new-streaming (string-append (or current-streaming "") delta))
     (struct-copy ui-state state [streaming-text new-streaming] [busy? #t])]

    [("turn.started")
     (struct-copy ui-state
                  state
                  [busy? #t]
                  [busy-since (event-time evt)]
                  [pending-tool-name #f]
                  [streaming-text #f]
                  [streaming-thinking #f]
                  [status-message #f])]

    [("turn.completed")
     ;; v0.28.20 T9: Enforce minimum 500ms busy duration to prevent flicker
     (define min-busy-ms 500)
     (define since (ui-state-busy-since state))
     (define elapsed
       (if since
           (- (event-time evt) since)
           min-busy-ms))
     (if (< elapsed min-busy-ms)
         (struct-copy ui-state state [streaming-text #f] [streaming-thinking #f])
         (struct-copy ui-state
                      state
                      [busy? #f]
                      [busy-since #f]
                      [streaming-text #f]
                      [streaming-thinking #f]
                      [pending-tool-name #f]))]

    [("turn.cancelled")
     (struct-copy ui-state
                  state
                  [busy? #f]
                  [streaming-text #f]
                  [streaming-thinking #f]
                  [pending-tool-name #f])]

    [("compaction.warning")
     (define tc (hash-ref payload 'tokenCount "?"))
     (append-entry
      state
      (make-entry 'system (format "[compaction warning: ~a tokens]" tc) (event-time evt) (hash)))]

    [("session.forked")
     (define new-sid (hash-ref payload 'newSessionId ""))
     (append-entry
      state
      (make-entry 'system (format "[session forked: ~a]" new-sid) (event-time evt) (hash)))]

    [("compaction")
     (define reason (hash-ref payload 'reason ""))
     (if (string=? reason "compaction-complete")
         (struct-copy ui-state state [status-message #f])
         (struct-copy ui-state state [status-message "Compacting..."]))]

    [("auto-retry")
     (define attempt (hash-ref payload 'attempt "?"))
     (define max-attempts (hash-ref payload 'max-attempts "?"))
     (define error-type (hash-ref payload 'error-type #f))
     (define type-label
       (case error-type
         [(timeout) "LLM timeout"]
         [(rate-limit) "rate limited"]
         [(context-overflow) "context too large"]
         [(provider-error) "server error"]
         [else #f]))
     (define msg
       (if type-label
           (format "[retry: ~a, ~a/~a...]" type-label attempt max-attempts)
           (format "[retry: attempt ~a/~a]" attempt max-attempts)))
     (struct-copy ui-state
                  (append-entry state (make-entry 'system msg (event-time evt) (hash)))
                  [streaming-text #f]
                  [streaming-thinking #f])]

    [("injection")
     (define content-type (hash-ref payload 'content-type "unknown"))
     (define msg (format "[injected ~a message]" content-type))
     (struct-copy ui-state
                  (append-entry state (make-entry 'system msg (event-time evt) (hash)))
                  [streaming-text #f]
                  [streaming-thinking #f])]

    [("model.request.started") (struct-copy ui-state state [busy? #t])]

    [("context.built")
     (define tok (hash-ref payload 'tokenCount #f))
     (if tok
         (struct-copy ui-state state [context-tokens tok])
         state)]

    [("tool.call.blocked")
     (define name (hash-ref payload 'name "?"))
     (define reason (hash-ref payload 'reason "blocked by extension"))
     (struct-copy ui-state
                  (append-entry state
                                (make-entry 'system
                                            (format "[tool blocked: ~a — ~a]" name reason)
                                            (event-time evt)
                                            (hasheq 'name name)))
                  [pending-tool-name #f])]

    [("queue.status-update") (struct-copy ui-state state [queue-counts payload])]

    [("iteration.soft-warning")
     (define iter (hash-ref payload 'iteration "?"))
     (define remaining (hash-ref payload 'remaining "?"))
     (define label (if (eq? (gsm-current) 'executing) "executing" "exploring"))
     (append-entry
      state
      (make-entry 'system
                  (format "[~a... iteration ~a, ~a remaining before hard stop]" label iter remaining)
                  (event-time evt)
                  (hash)))]

    [("exploration.progress")
     (define count (hash-ref payload 'consecutive-tools "?"))
     (define tool-names (hash-ref payload 'tool-names '()))
     (define label (if (eq? (gsm-current) 'executing) "executing" "exploring"))
     (append-entry state
                   (make-entry 'system
                               (format "[~a... ~a tool calls: ~a]"
                                       label
                                       (string-join (map (lambda (s)
                                                           (if (string? s)
                                                               s
                                                               (format "~a" s)))
                                                         tool-names)
                                                    ", "))
                               (event-time evt)
                               (hash)))]

    [("gsd.plan.archived")
     (define path (hash-ref payload 'path "?"))
     (append-entry
      state
      (make-entry 'system (format "\u2705 Plan archived to ~a" path) (event-time evt) (hash)))]

    [("context.mid-turn-over-budget")
     (define estimated (hash-ref payload 'estimated-tokens "?"))
     (define budget (hash-ref payload 'budget "?"))
     (append-entry state
                   (make-entry 'system
                               (format "[context growing: ~a/~a tokens used]" estimated budget)
                               (event-time evt)
                               (hash)))]

    ;; Backward-compatible handlers for old raw event topics (tests + legacy)
    [("tool.call.completed")
     (define name (hash-ref payload 'name "?"))
     (define result-raw (hash-ref payload 'result #f))
     (define result-summary
       (if result-raw
           (string-replace (truncate-string (tool-result-content->string result-raw) 80)
                           "\n"
                           " \u23ce ")
           ""))
     (define text
       (if (string=? result-summary "")
           (format "[OK: ~a]" name)
           (format "[OK: ~a] ~a" name result-summary)))
     (define ts (event-time evt))
     (define meta (hasheq 'name name 'result (or result-raw "")))
     (struct-copy ui-state
                  (append-entry state (make-entry 'tool-end text ts meta))
                  [pending-tool-name #f])]

    [("tool.call.failed")
     (define name (hash-ref payload 'name "?"))
     (define err (hash-ref payload 'error "unknown"))
     (define ts (event-time evt))
     (struct-copy
      ui-state
      (append-entry state
                    (make-entry 'tool-fail
                                (string-replace (format "[FAIL: ~a] ~a" name err) "\n" " \u23ce ")
                                ts
                                (hasheq 'name name 'error err)))
      [pending-tool-name #f])]

    [("compaction.started") (struct-copy ui-state state [status-message "Compacting..."])]
    [("compaction.start") (struct-copy ui-state state [status-message "Compacting..."])]
    [("compaction.completed") (struct-copy ui-state state [status-message #f])]
    [("compaction.end") (struct-copy ui-state state [status-message #f])]

    [("auto-retry.start")
     (define attempt (hash-ref payload 'attempt "?"))
     (define max-attempts (hash-ref payload 'max-retries "?"))
     (define error-type (hash-ref payload 'errorType #f))
     (define type-label
       (case error-type
         [(timeout) "LLM timeout"]
         [(rate-limit) "rate limited"]
         [(context-overflow) "context too large"]
         [(provider-error) "server error"]
         [else #f]))
     (define msg
       (if type-label
           (format "[retry: ~a, ~a/~a...]" type-label attempt max-attempts)
           (format "[retry: attempt ~a/~a]" attempt max-attempts)))
     (struct-copy ui-state
                  (append-entry state (make-entry 'system msg (event-time evt) (hash)))
                  [streaming-text #f]
                  [streaming-thinking #f])]

    [("auto-retry.context-reduced")
     (define original (hash-ref payload 'original-messages 0))
     (define reduced (hash-ref payload 'reduced-messages 0))
     (append-entry state
                   (make-entry 'system
                               (format "[retry: reduced context ~a -> ~a messages]" original reduced)
                               (event-time evt)
                               (hash)))]

    [("model.stream.thinking")
     (define delta (hash-ref payload 'delta ""))
     (define current-thinking (ui-state-streaming-thinking state))
     (define new-thinking (string-append (or current-thinking "") delta))
     (struct-copy ui-state state [streaming-thinking new-thinking] [busy? #t])]

    [("model.stream.completed")
     (define usage (hash-ref payload 'usage (hasheq)))
     (define in-tok (hash-ref usage 'prompt_tokens (hash-ref usage 'input_tokens 0)))
     (define out-tok (hash-ref usage 'completion_tokens (hash-ref usage 'output_tokens 0)))
     (define ct (ui-state-cost-tracker state))
     (when ct
       (cost-tracker-update! ct in-tok out-tok (ui-model-label state)))
     (struct-copy ui-state state [streaming-text #f] [streaming-thinking #f])]

    [else state]))
