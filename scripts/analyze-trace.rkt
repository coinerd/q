#lang racket/base

;; scripts/analyze-trace.rkt — trace analysis tooling for development telemetry
;;
;; Parses JSONL session trace files and extracts metrics:
;;   - Tool calls (count, names, success/failure)
;;   - Iterations (count, termination reason)
;;   - Steering events
;;   - Context sizes (messages per turn)
;;   - Timing (timestamps)
;;
;; Output: JSON + human-readable report
;;
;; Usage:
;;   racket scripts/analyze-trace.rkt <trace.jsonl>
;;   racket scripts/analyze-trace.rkt --json <trace.jsonl>
;;   racket scripts/analyze-trace.rkt --summary <trace.jsonl>

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         (only-in "../runtime/session-store.rkt" load-session-log)
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-role
                  message-content
                  message-kind
                  message-timestamp
                  message-meta
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-name
                  tool-call-part-id))

(provide extract-metrics
         format-report
         metrics->json)

;; ============================================================
;; Metrics extraction
;; ============================================================

;; extract-metrics : (listof message?) -> hash
;; Extract all metrics from a list of session log messages.
(define (extract-metrics messages)
  (define tool-calls (extract-tool-calls messages))
  (define roles (count-roles messages))
  (define turns (count-turns messages))
  (define timing (extract-timing messages))
  (define content-types (count-content-types messages))
  (hasheq 'total-messages
          (length messages)
          'tool-calls
          tool-calls
          'roles
          roles
          'turns
          turns
          'timing
          timing
          'content-types
          content-types))

;; extract-tool-calls : (listof message?) -> hash
(define (extract-tool-calls messages)
  (define all-tc
    (for*/list ([msg (in-list messages)]
                [part (in-list (message-content msg))]
                #:when (tool-call-part? part))
      (tool-call-part-name part)))
  (define counts (count-strings all-tc))
  (hasheq 'total (length all-tc) 'unique (hash-keys counts) 'by-tool counts))

;; count-roles : (listof message?) -> hash
(define (count-roles messages)
  (define roles (map message-role messages))
  (count-symbols roles))

;; count-turns : (listof message?) -> hash
(define (count-turns messages)
  (define user-count (length (filter (lambda (m) (eq? (message-role m) 'user)) messages)))
  (define assistant-count (length (filter (lambda (m) (eq? (message-role m) 'assistant)) messages)))
  (hasheq 'user-messages
          user-count
          'assistant-messages
          assistant-count
          'estimated-turns
          (min user-count assistant-count)))

;; extract-timing : (listof message?) -> hash
(define (extract-timing messages)
  (define timestamps (filter values (map message-timestamp messages)))
  (if (< (length timestamps) 2)
      (hasheq 'duration-ms 0 'first-timestamp #f 'last-timestamp #f)
      (let ([first-ts (car timestamps)]
            [last-ts (last timestamps)])
        (hasheq 'duration-ms
                (if (and (number? first-ts) (number? last-ts))
                    (- last-ts first-ts)
                    0)
                'first-timestamp
                first-ts
                'last-timestamp
                last-ts))))

;; count-content-types : (listof message?) -> hash
(define (count-content-types messages)
  (define types
    (for*/list ([msg (in-list messages)]
                [part (in-list (message-content msg))])
      (cond
        [(text-part? part) 'text]
        [(tool-call-part? part) 'tool-call]
        [else 'other])))
  (count-symbols types))

;; ============================================================
;; Helpers
;; ============================================================

(define (count-symbols syms)
  (for/fold ([acc (hasheq)]) ([s (in-list syms)])
    (hash-update acc s add1 0)))

(define (count-strings strs)
  (for/fold ([acc (hasheq)]) ([s (in-list strs)])
    (hash-update acc s add1 0)))

;; ============================================================
;; Report formatting
;; ============================================================

;; format-report : hash -> string
(define (format-report metrics)
  (define lines '())
  (define (add! line)
    (set! lines (cons line lines)))
  (add! "═══════════════════════════════════════════")
  (add! "          TRACE ANALYSIS REPORT")
  (add! "═══════════════════════════════════════════")
  (add! "")
  (add! (format "  Total messages: ~a" (hash-ref metrics 'total-messages)))
  (add! "")

  ;; Roles
  (define roles (hash-ref metrics 'roles))
  (add! "  Messages by role:")
  (for ([(role count) (in-hash roles)])
    (add! (format "    ~a: ~a" role count)))
  (add! "")

  ;; Turns
  (define turns (hash-ref metrics 'turns))
  (add! (format "  Estimated turns: ~a" (hash-ref turns 'estimated-turns)))
  (add! (format "  User messages: ~a" (hash-ref turns 'user-messages)))
  (add! (format "  Assistant messages: ~a" (hash-ref turns 'assistant-messages)))
  (add! "")

  ;; Tool calls
  (define tc (hash-ref metrics 'tool-calls))
  (add! (format "  Tool calls: ~a total" (hash-ref tc 'total)))
  (when (> (hash-ref tc 'total) 0)
    (add! "  By tool:")
    (for ([(name count) (in-hash (hash-ref tc 'by-tool))])
      (add! (format "    ~a: ~a" name count))))
  (add! "")

  ;; Content types
  (define ct (hash-ref metrics 'content-types))
  (add! "  Content parts:")
  (for ([(type count) (in-hash ct)])
    (add! (format "    ~a: ~a" type count)))
  (add! "")

  ;; Timing
  (define timing (hash-ref metrics 'timing))
  (add! (format "  Duration: ~a ms" (hash-ref timing 'duration-ms)))
  (add! (format "  First timestamp: ~a" (hash-ref timing 'first-timestamp)))
  (add! (format "  Last timestamp: ~a" (hash-ref timing 'last-timestamp)))
  (add! "")
  (add! "═══════════════════════════════════════════")

  (string-join (reverse lines) "\n"))

;; ============================================================
;; JSON output
;; ============================================================

(define (metrics->json metrics)
  (define (val->json v)
    (cond
      [(hash? v)
       (define pairs
         (for/list ([(k v) (in-hash v)])
           (format "~s: ~a" k (val->json v))))
       (format "{~a}" (string-join pairs ", "))]
      [(list? v) (format "[~a]" (string-join (map val->json v) ", "))]
      [(symbol? v) (format "~s" (symbol->string v))]
      [(string? v) (format "~s" v)]
      [(number? v) (~a v)]
      [else (~a v)]))
  (val->json metrics))

;; ============================================================
;; Main (command-line entry point)
;; ============================================================

(module+ main
  (require racket/cmdline)

  (define output-json? #f)

  (define trace-file
    (command-line #:once-each [("--json") "Output as JSON" (set! output-json? #t)]
                  [("--summary") "Output human-readable summary (default)" (void)]
                  #:args (trace-path)
                  trace-path))

  (unless (file-exists? trace-file)
    (displayln (format "Error: file not found: ~a" trace-file))
    (exit 1))

  (define messages (load-session-log trace-file))
  (define metrics (extract-metrics messages))

  (if output-json?
      (displayln (metrics->json metrics))
      (displayln (format-report metrics))))
