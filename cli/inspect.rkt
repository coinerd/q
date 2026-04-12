#lang racket/base

;; cli/inspect.rkt — session trace analysis
;;
;; Provides:
;;   inspect-session         — analyze session log, return structured stats
;;   inspect-session-stats   — extended stats (messages/min, per-tool arg sizes)
;;   inspect-session-safe    — error-handling wrapper for corrupted/missing sessions
;;   format-inspection       — human-readable multi-line string for CLI output

(require racket/match
         racket/list
         racket/string
         racket/format
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt")

(provide inspect-session
         inspect-session-stats
         inspect-session-safe
         format-inspection)

;; ── Core analysis ──

(define (inspect-session path)
  ;; Analyze a session log at `path` and return a hash of stats.
  ;; Keys: entry-count, time-span, duration-seconds, models, tool-calls,
  ;;       tool-call-total, token-usage, role-counts, branch-count,
  ;;       error-count, integrity
  (define messages (load-session-log path))
  (define integrity (verify-session-integrity path))
  (define n (length messages))

  (define time-span
    (if (zero? n)
        #f
        (cons (message-timestamp (first messages))
              (message-timestamp (last messages)))))

  (define duration-seconds
    (if time-span
        (- (cdr time-span) (car time-span))
        0))

  ;; Collect unique model names from meta
  (define models
    (remove-duplicates
     (filter values
             (for/list ([m (in-list messages)])
               (hash-ref (message-meta m) 'model #f)))))

  ;; Flatten all content parts across all messages
  (define all-parts (apply append (map message-content messages)))

  ;; Count tool calls by name
  ;; Uses `hash' (equal?-based) because tool names are strings.
  (define tool-calls
    (for/fold ([acc (hash)])
              ([cp (in-list all-parts)])
      (cond
        [(tool-call-part? cp)
         (hash-update acc (tool-call-part-name cp) add1 0)]
        [else acc])))

  (define tool-call-total
    (for/sum ([(_ v) (in-hash tool-calls)]) v))

  ;; Sum token usage from meta
  (define-values (prompt-total completion-total)
    (for/fold ([prompt-tot 0] [completion-tot 0])
              ([m (in-list messages)])
      (define usage (hash-ref (message-meta m) 'usage #f))
      (if usage
          (values (+ prompt-tot (hash-ref usage 'prompt 0))
                  (+ completion-tot (hash-ref usage 'completion 0)))
          (values prompt-tot completion-tot))))
  (define token-usage
    (hasheq 'prompt prompt-total 'completion completion-total))

  ;; Count by role
  (define role-counts
    (for/fold ([acc (hasheq)])
              ([m (in-list messages)])
      (hash-update acc (message-role m) add1 0)))

  ;; Branch count: distinct non-#f parentIds
  (define branch-count
    (length (remove-duplicates
             (filter values
                     (map message-parent-id messages)))))

  ;; Error count: tool-result parts with is-error? = #t
  (define error-count
    (for/sum ([cp (in-list all-parts)])
      (if (and (tool-result-part? cp) (tool-result-part-is-error? cp))
          1 0)))

  (hasheq 'entry-count n
          'time-span time-span
          'duration-seconds duration-seconds
          'models models
          'tool-calls tool-calls
          'tool-call-total tool-call-total
          'token-usage token-usage
          'role-counts role-counts
          'branch-count branch-count
          'error-count error-count
          'integrity integrity))

;; ── Extended stats ──

(define (inspect-session-stats path)
  ;; Returns inspect-session result plus:
  ;;   'messages-per-minute — rate of messages over the session duration
  ;;   'per-tool-avg-arg-size — hash of tool-name → average argument string length
  (define base (inspect-session path))
  (define messages (load-session-log path))
  (define n (hash-ref base 'entry-count))
  (define dur (hash-ref base 'duration-seconds))

  (define messages-per-minute
    (if (and (> dur 0) (> n 0))
        (exact->inexact (/ (* n 60) dur))
        0.0))

  ;; Per-tool average argument size
  (define tool-args
    (for/fold ([acc (hasheq)])
              ([m (in-list messages)])
      (for/fold ([acc acc])
                ([cp (in-list (message-content m))])
        (if (tool-call-part? cp)
            (let* ([name (tool-call-part-name cp)]
                   [arg (tool-call-part-arguments cp)]
                   [arg-size (if (string? arg) (string-length arg) 0)])
              (hash-update acc name
                           (lambda (existing)
                             (cons (add1 (car existing))
                                   (+ (cdr existing) arg-size)))
                           (cons 0 0)))
            acc))))

  (define per-tool-avg-arg-size
    (for/hasheq ([(name counts) (in-hash tool-args)])
      (values name
              (if (> (car counts) 0)
                  (exact->inexact (/ (cdr counts) (car counts)))
                  0.0))))

  (hash-set (hash-set base 'messages-per-minute messages-per-minute)
            'per-tool-avg-arg-size per-tool-avg-arg-size))

;; ── Safe wrapper ──

(define (inspect-session-safe path)
  ;; Wraps inspect-session with error handling.
  ;; Returns partial results even for corrupted sessions.
  ;; Missing file → (hasheq 'error "file not found" 'entry-count 0)
  ;; Partial failure → adds 'partial? #t
  (cond
    [(not (file-exists? path))
     (hasheq 'error "file not found" 'entry-count 0)]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        ;; Try to return whatever we can
                        (with-handlers ([exn:fail? (lambda (_) (hasheq 'error (exn-message e) 'partial? #t))])
                          (define partial-msgs
                            (with-handlers ([exn:fail? (lambda (_) '())])
                              (load-session-log path)))
                          (hasheq 'entry-count (length partial-msgs)
                                  'partial? #t
                                  'error (exn-message e))))])
       (inspect-session path))]))

;; ── Formatting ──

(define (format-inspection h)
  ;; Format an inspection result hash as a human-readable multi-line string.
  (define lines
    (list
     (format "Entry count:    ~a" (hash-ref h 'entry-count 0))
     (format "Duration:       ~a seconds" (hash-ref h 'duration-seconds 0))
     (let ([ts (hash-ref h 'time-span #f)])
       (if ts
           (format "Time span:      ~a → ~a" (car ts) (cdr ts))
           "Time span:      (empty session)"))
     (format "Models:         ~a" (string-join (map ~a (hash-ref h 'models '())) ", "))
     (format "Tool call total: ~a" (hash-ref h 'tool-call-total 0))
     (let ([tc (hash-ref h 'tool-calls (hasheq))])
       (if (hash-empty? tc)
           "Tool calls:     (none)"
           (format "Tool calls:     ~a"
                   (string-join
                    (for/list ([(name count) (in-hash tc)])
                      (format "~a: ~a" name count))
                    ", "))))
     (let ([tu (hash-ref h 'token-usage (hasheq))])
       (format "Token usage:    prompt=~a completion=~a"
               (hash-ref tu 'prompt 0)
               (hash-ref tu 'completion 0)))
     (let ([rc (hash-ref h 'role-counts (hasheq))])
       (format "Role counts:    ~a"
               (string-join
                (for/list ([(role count) (in-hash rc)])
                  (format "~a: ~a" role count))
                ", ")))
     (format "Branch count:   ~a" (hash-ref h 'branch-count 0))
     (format "Error count:    ~a" (hash-ref h 'error-count 0))
     (format "Integrity:      valid=~a/~a order-ok?=~a truncated?=~a"
             (hash-ref (hash-ref h 'integrity (hasheq)) 'valid-entries 0)
             (hash-ref (hash-ref h 'integrity (hasheq)) 'total-entries 0)
             (hash-ref (hash-ref h 'integrity (hasheq)) 'entry-order-valid? #t)
             (hash-ref (hash-ref h 'integrity (hasheq)) 'truncated-at-end? #f))
     (let ([err (hash-ref h 'error #f)])
       (if err
           (format "Error:          ~a" err)
           ""))))

  (string-join (filter (lambda (s) (> (string-length s) 0)) lines) "\n"))
