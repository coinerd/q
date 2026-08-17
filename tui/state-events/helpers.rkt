#lang racket/base

;; tui/state-events/helpers.rkt -- Shared helpers for event->state reduction
;;
;; Extracted from state-events.rkt to keep the reducer module focused.

(require racket/string
         racket/match
         racket/list
         "../state-types.rkt")

(provide classify-error-type
         format-error-hint
         append-entry
         recent-tool-start?
         recent-tool-end?
         ui-model-label
         truncate-status-msg)

;; Dedup window size — check last N entries for duplicate tool events
(define dedup-window-size 10)
(define transcript-max-entries 500)

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
;; Helper: append entry with auto-id
;; ============================================================

(define (append-entry st entry)
  (define-values (id-entry st1) (assign-entry-id entry st))
  (define entries (cons id-entry (ui-state-transcript st1)))
  (struct-copy ui-state
               st1
               [transcript (take entries (min transcript-max-entries (length entries)))]))

;; Dedup guard: prevent duplicate tool-start entries.
;; BUG-0015: key on the tool-call id when available; fall back to name-only
;; matching for events that carry no id (legacy/test payloads). Without the id
;; key, a 2-read burst would collapse the second read onto the first because
;; both share the name "read".
(define (recent-tool-start? st name [tool-call-id #f])
  (define transcript (ui-state-transcript st))
  (define recent (take transcript (min dedup-window-size (length transcript))))
  (for/or ([entry (in-list recent)])
    (and (eq? (transcript-entry-kind entry) 'tool-start)
         (let ([entry-name (hash-ref (transcript-entry-meta entry) 'name "")])
           (if tool-call-id
               (and (equal? entry-name name)
                    (equal? (hash-ref (transcript-entry-meta entry) 'tool-call-id #f) tool-call-id))
               (equal? entry-name name))))))

;; Dedup guard: prevent duplicate tool-end entries.
;; BUG-0015: the typed tool.execution.completed event carries no tool-call id,
;; so tool-end dedup keys on (name . result-key). Two distinct calls return
;; different results and both render; a genuine duplicate completion returns
;; the same result and is suppressed. Falls back to name-only matching when no
;; result-key is supplied (legacy/test payloads).
(define (recent-tool-end? st name [result-key #f])
  (define transcript (ui-state-transcript st))
  (define recent (take transcript (min dedup-window-size (length transcript))))
  (for/or ([entry (in-list recent)])
    (and (memq (transcript-entry-kind entry) '(tool-end tool-fail))
         (let ([entry-name (hash-ref (transcript-entry-meta entry) 'name "")])
           (if result-key
               (and (equal? entry-name name)
                    (equal? (hash-ref (transcript-entry-meta entry) 'result-key #f) result-key))
               (equal? entry-name name))))))

;; M-09: Extracted error classification (pure function)
(define (classify-error-type err payload)
  (hash-ref payload
            'errorType
            (lambda ()
              (match err
                [(regexp #rx"[Tt]imeout|timed out") 'timeout]
                [(regexp #rx"429|[Rr]ate.?[Ll]imit") 'rate-limit]
                [(regexp #rx"401|403|[Aa]uth|[Uu]nauthorized") 'auth]
                [(regexp #rx"context.*overflow|[Tt]oo.*long|[Mm]ax.*tokens") 'context-overflow]
                [_ 'provider-error]))))

;; M-09: Extracted error hint generation (pure function)
(define (format-error-hint error-type retries-attempted history-types)
  (cond
    [(and retries-attempted (> retries-attempted 0))
     (cond
       [(and (member 'timeout history-types) (member 'rate-limit history-types))
        (format "Provider timed out, then rate limited (~a retries). Wait 30s, then type /retry."
                retries-attempted)]
       [(> (length history-types) 1)
        (format "Mixed errors after ~a retries. Wait a moment, then type /retry." retries-attempted)]
       [else
        (case error-type
          [(rate-limit)
           (format "Rate limit persisted after ~a retries. Wait a moment, then type /retry."
                   retries-attempted)]
          [(timeout)
           (format "Provider timed out after ~a retries. Type /retry to resubmit." retries-attempted)]
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
