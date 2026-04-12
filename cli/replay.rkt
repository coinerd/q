#lang racket/base

;; cli/replay.rkt — deterministic session replay and drift detection
;;
;; Replays tool calls from a session log and detects behavioral drift.
;; Does NOT re-execute LLM calls or tools — validates internal consistency
;; of tool-call / tool-result pairs in the session log.
;;
;; Provides:
;;   replay-result          — struct for individual tool-call analysis
;;   replay-tool-calls      — analyze tool call/result pairs from messages
;;   replay-session-report  — full session replay report (path → hash)
;;   replay-session-safe    — error-handling wrapper
;;   format-replay-report   — human-readable CLI output

(require racket/list
         racket/match
         racket/string
         racket/format
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt")

(provide
 (struct-out replay-result)
 replay-tool-calls
 replay-session-report
 replay-session-safe
 format-replay-report)

;; ============================================================
;; replay-result struct
;; ============================================================

(struct replay-result (tool-name arguments original-result drifted? reason)
  #:transparent)
;; tool-name       : string?
;; arguments        : string?
;; original-result  : string?
;; drifted?         : boolean?
;; reason           : (or/c #f string?)

;; ============================================================
;; Non-deterministic tool names
;; ============================================================

;; Tools whose results depend on mutable filesystem state.
;; These are validated for structural consistency but flagged.
(define NON-DETERMINISTIC-TOOLS '("bash" "grep" "find" "ls"))

(define (non-deterministic-tool? name)
  (member name NON-DETERMINISTIC-TOOLS))

;; ============================================================
;; replay-tool-calls : (listof message?) -> (listof replay-result?)
;; ============================================================

(define (replay-tool-calls messages)
  ;; Extract all tool-call + tool-result pairs from messages.
  ;; Walk in timestamp order. Match tool-call-part to tool-result-part
  ;; by tool-call-id. Report drift for unmatched entries.

  ;; Sort messages by timestamp for deterministic walk
  (define sorted
    (sort messages < #:key message-timestamp))

  ;; Collect all tool-call-parts and tool-result-parts
  (define call-parts
    (for*/list ([msg (in-list sorted)]
                [cp  (in-list (message-content msg))]
                #:when (tool-call-part? cp))
      cp))

  (define result-parts
    (for*/list ([msg (in-list sorted)]
                [cp  (in-list (message-content msg))]
                #:when (tool-result-part? cp))
      cp))

  ;; Index results by tool-call-id
  (define results-by-id
    (for/fold ([acc (hash)])
              ([rp (in-list result-parts)])
      (hash-set acc (tool-result-part-tool-call-id rp) rp)))

  ;; Track which result-ids have been consumed
  (define consumed (make-hash))

  ;; Process each call-part
  (define call-results
    (for/list ([tc (in-list call-parts)])
      (define tc-id (tool-call-part-id tc))
      (define matched (hash-ref results-by-id tc-id #f))
      (cond
        [matched
         (hash-set! consumed tc-id #t)
         (replay-result
          (tool-call-part-name tc)
          (tool-call-part-arguments tc)
          (tool-result-part-content matched)
          #f
          #f)]
        [else
         ;; No matching result — drift
         (replay-result
          (tool-call-part-name tc)
          (tool-call-part-arguments tc)
          ""
          #t
          "missing result")])))

  ;; Find orphan results (results with no matching call)
  (define orphan-results
    (for/list ([rp (in-list result-parts)]
               #:when (not (hash-has-key? consumed
                                           (tool-result-part-tool-call-id rp))))
      (replay-result
       ""
       ""
       (tool-result-part-content rp)
       #t
       "orphan result")))

  (append call-results orphan-results))

;; ============================================================
;; replay-session-report : path-string? -> hash?
;; ============================================================

(define (replay-session-report path)
  ;; Load session, analyze tool call/result pairs.
  ;; Returns hash with:
  ;;   'total-entries    — message count
  ;;   'tool-call-count  — number of tool calls
  ;;   'drift-count      — number of inconsistencies
  ;;   'drifts           — list of drift description strings
  ;;   'non-deterministic — tool names that are non-deterministic
  ;;   'skipped          — count of non-deterministic tools skipped
  ;;   'replay-results   — list of replay-result structs

  (define messages (load-session-log path))
  (define results (replay-tool-calls messages))

  (define drifted (filter replay-result-drifted? results))

  ;; Collect non-deterministic tool names that appear in this session
  (define nd-in-session
    (remove-duplicates
     (for/list ([r (in-list results)]
                #:when (non-deterministic-tool? (replay-result-tool-name r)))
       (replay-result-tool-name r))))

  ;; Count non-deterministic tool calls (skipped from deterministic analysis)
  (define skipped
    (for/sum ([r (in-list results)])
      (if (non-deterministic-tool? (replay-result-tool-name r)) 1 0)))

  ;; Build drift descriptions
  (define drift-descriptions
    (for/list ([r (in-list drifted)])
      (format "~a: ~a"
              (replay-result-tool-name r)
              (replay-result-reason r))))

  (hasheq 'total-entries (length messages)
          'tool-call-count (length results)
          'drift-count (length drifted)
          'drifts drift-descriptions
          'non-deterministic nd-in-session
          'skipped skipped
          'replay-results results))

;; ============================================================
;; replay-session-safe : path-string? -> hash?
;; ============================================================

(define (replay-session-safe path)
  ;; Wraps replay-session-report with error handling.
  ;; Returns partial results for corrupted/missing sessions.

  (cond
    [(not (file-exists? path))
     (hasheq 'error "file not found"
             'total-entries 0
             'tool-call-count 0
             'drift-count 0
             'drifts '()
             'non-deterministic '()
             'skipped 0
             'replay-results '())]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (hasheq 'error (exn-message e)
                                'total-entries 0
                                'tool-call-count 0
                                'drift-count 0
                                'drifts '()
                                'non-deterministic '()
                                'skipped 0
                                'replay-results '()))])
       (replay-session-report path))]))

;; ============================================================
;; format-replay-report : hash? -> string?
;; ============================================================

(define (format-replay-report h)
  ;; Formats the replay report as a human-readable CLI string.

  (define lines
    (list
     (format "Entries:        ~a" (hash-ref h 'total-entries 0))
     (format "Tool calls:     ~a" (hash-ref h 'tool-call-count 0))
     (format "Drift detected: ~a" (hash-ref h 'drift-count 0))
     (format "Skipped (ND):   ~a" (hash-ref h 'skipped 0))
     (let ([nd (hash-ref h 'non-deterministic '())])
       (if (null? nd)
           "Non-deterministic: (none)"
           (format "Non-deterministic: ~a" (string-join nd ", "))))
     (let ([drifts (hash-ref h 'drifts '())])
       (if (null? drifts)
           "Drifts:         (none)"
           (format "Drifts:\n~a"
                   (string-join
                    (for/list ([d (in-list drifts)])
                      (format "  - ~a" d))
                    "\n"))))
     (let ([results (hash-ref h 'replay-results '())])
       (if (null? results)
           ""
           (format "Results:\n~a"
                   (string-join
                    (for/list ([r (in-list results)])
                      (format "  [~a] ~a ~a → ~a~a"
                              (if (replay-result-drifted? r) "DRIFT" "OK")
                              (replay-result-tool-name r)
                              (let ([args (replay-result-arguments r)])
                                (if (> (string-length args) 40)
                                    (string-append (substring args 0 40) "...")
                                    args))
                              (let ([res (replay-result-original-result r)])
                                (if (> (string-length res) 40)
                                    (string-append (substring res 0 40) "...")
                                    res))
                              (let ([reason (replay-result-reason r)])
                                (if reason (format " (~a)" reason) ""))))
                    "\n"))))))

  (string-join (filter (lambda (s) (> (string-length s) 0)) lines) "\n"))
