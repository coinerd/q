#lang racket/base

;; interfaces/sessions.rkt — shared session management CLI logic
;;
;; Provides session listing, info display, and deletion for both
;; the `q sessions` CLI subcommand and the `/sessions` interactive command.
;;
;; Sessions are stored as <session-dir>/<session-id>/session.jsonl
;; where session-dir defaults to ~/.q/sessions/.

(require racket/match
         racket/string
         racket/format
         racket/file
         racket/list
         racket/path
         json
         "../util/jsonl.rkt"
         "../runtime/settings.rkt"
         "../runtime/session-store.rkt"
         (only-in "../cli/args.rkt"
                  cli-config?
                  cli-config-sessions-subcommand
                  cli-config-sessions-args
                  cli-config-session-dir))

;; Listing
(provide sessions-list
         sessions-list->strings
         ;; Info
         sessions-info
         sessions-info->string
         ;; Delete
         sessions-delete
         ;; CLI dispatch
         run-sessions-command
         ;; Internal helpers (for testing)
         scan-session-dirs
         read-session-metadata
         ;; Trace display (v0.15.0)
         read-trace-entries
         display-trace-summary)

;; ============================================================
;; Session scanning
;; ============================================================

;; Scan a session directory for session subdirectories.
;; Returns list of (list session-id dir-path) sorted by most recent first.
(define (scan-session-dirs session-dir)
  (if (not (directory-exists? session-dir))
      '()
      (let* ([entries (directory-list session-dir #:build? #t)]
             [session-paths (filter (lambda (p)
                                      (and (directory-exists? p)
                                           (file-exists? (build-path p "session.jsonl"))))
                                    entries)])
        ;; Sort by modification time of session.jsonl, newest first
        (define with-time
          (for/list ([p (in-list session-paths)])
            (define jsonl (build-path p "session.jsonl"))
            (define mtime
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-warning "sessions: failed to get mtime for ~a: ~a"
                                                        jsonl
                                                        (exn-message e))
                                           0)])
                (file-or-directory-modify-seconds jsonl)))
            (list (path->string (file-name-from-path p)) p mtime)))
        (map (lambda (t) (list (car t) (cadr t))) (sort with-time > #:key caddr)))))

;; Read metadata from a session.jsonl file.
;; Returns a hash with keys:
;;   'id, 'message-count, 'model, 'provider, 'size-bytes, 'mtime
(define (read-session-metadata session-id session-path)
  (define jsonl-path (build-path session-path "session.jsonl"))
  (define size-bytes
    (if (file-exists? jsonl-path)
        (file-size jsonl-path)
        0))
  (define mtime
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "sessions: failed to get mtime for ~a: ~a"
                                              jsonl-path
                                              (exn-message e))
                                 0)])
      (file-or-directory-modify-seconds jsonl-path)))
  ;; Count entries and extract model from first few messages
  (define entries
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-warning "sessions: failed to read ~a: ~a" jsonl-path (exn-message e))
                       '())])
      (jsonl-read-all-valid jsonl-path)))
  (define message-count (length entries))
  ;; Try to find model from first assistant message
  (define model
    (let loop ([es entries])
      (cond
        [(null? es) "unknown"]
        [else
         (define e (car es))
         (define m (hash-ref e 'model #f))
         (if m
             m
             (loop (cdr es)))])))
  (hasheq 'id
          session-id
          'message-count
          message-count
          'model
          model
          'size-bytes
          size-bytes
          'mtime
          mtime))

;; ============================================================
;; sessions-list — list sessions
;; ============================================================

;; List sessions from the given session directory.
;; Returns list of metadata hashes (see read-session-metadata).
;; Keyword args:
;;   #:limit — maximum number of sessions to return (default: 20)
;;   #:sort  — 'by-date (default) or 'by-size
(define (sessions-list session-dir #:limit [limit 20] #:sort [sort-mode 'by-date])
  (define dirs (scan-session-dirs session-dir))
  ;; Read metadata for each
  (define metadata
    (for/list ([d (in-list dirs)])
      (read-session-metadata (car d) (cadr d))))
  ;; Sort
  (define sorted
    (case sort-mode
      [(by-size) (sort metadata > #:key (lambda (m) (hash-ref m 'size-bytes 0)))]
      [(by-date) metadata] ; already sorted by mtime from scan
      [else metadata]))
  ;; Limit
  (take sorted (min limit (length sorted))))

;; Format session metadata list to a table of strings.
;; Returns list of strings (one per line including header).
(define (sessions-list->strings sessions)
  (define header
    (format "~a ~a ~a ~a ~a"
            (pad-right "ID" 16)
            (pad-right "Model" 18)
            (pad-right "Date" 22)
            (pad-right "Messages" 10)
            "Size"))
  (define rows
    (for/list ([s (in-list sessions)])
      (define id (hash-ref s 'id "?"))
      (define model (hash-ref s 'model "unknown"))
      (define msg-count (hash-ref s 'message-count 0))
      (define size-bytes (hash-ref s 'size-bytes 0))
      (define mtime (hash-ref s 'mtime 0))
      (define date-str
        (if (> mtime 0)
            (let ([d (seconds->date mtime #f)])
              (format "~a-~a-~a ~a:~a"
                      (date-year d)
                      (pad2 (date-month d))
                      (pad2 (date-day d))
                      (pad2 (date-hour d))
                      (pad2 (date-minute d))))
            "unknown"))
      (define size-str (format-size size-bytes))
      (format "~a ~a ~a ~a ~a"
              (pad-right id 16)
              (pad-right model 18)
              (pad-right date-str 22)
              (pad-right (~a msg-count) 10)
              size-str)))
  (cons header rows))

;; ============================================================
;; sessions-info — show session details
;; ============================================================

;; Get detailed info for a specific session.
;; Returns a hash with session metadata, or #f if not found.
(define (sessions-info session-dir session-id)
  (define session-path (build-path session-dir session-id))
  (cond
    [(not (directory-exists? session-path)) #f]
    [else
     (define meta (read-session-metadata session-id session-path))
     ;; Count tool calls from messages
     (define jsonl-path (build-path session-path "session.jsonl"))
     (define entries
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (log-warning "sessions: failed to read ~a: ~a" jsonl-path (exn-message e))
                          '())])
         (jsonl-read-all-valid jsonl-path)))
     (define tool-call-count
       (for/sum ([e (in-list entries)])
                (define content (hash-ref e 'content '()))
                (if (list? content)
                    (for/sum ([part (in-list content)] #:when (equal? (hash-ref part 'type #f)
                                                                      "tool-call"))
                             1)
                    0)))
     ;; Check for branch info
     (define branch-count
       (for/sum ([e (in-list entries)])
                (define parent-id (hash-ref e 'parentId #f))
                (if parent-id 1 0)))
     (hash-set* meta
                'tool-call-count
                tool-call-count
                'branch-count
                branch-count
                'path
                session-path)]))

;; Format session info to a string.
(define (sessions-info->string info)
  (if (not info)
      "Session not found."
      (let ([id (hash-ref info 'id)]
            [model (hash-ref info 'model "unknown")]
            [msg-count (hash-ref info 'message-count 0)]
            [tool-count (hash-ref info 'tool-call-count 0)]
            [size (hash-ref info 'size-bytes 0)]
            [mtime (hash-ref info 'mtime 0)]
            [branches (hash-ref info 'branch-count 0)]
            [path (hash-ref info 'path "")])
        (define date-str
          (if (> mtime 0)
              (let ([d (seconds->date mtime #f)])
                (format "~a-~a-~a ~a:~a:~a"
                        (date-year d)
                        (pad2 (date-month d))
                        (pad2 (date-day d))
                        (pad2 (date-hour d))
                        (pad2 (date-minute d))
                        (pad2 (date-second d))))
              "unknown"))
        (string-append (format "Session: ~a\n" id)
                       (format "  Model:       ~a\n" model)
                       (format "  Last modified: ~a\n" date-str)
                       (format "  Messages:    ~a\n" msg-count)
                       (format "  Tool calls:  ~a\n" tool-count)
                       (format "  File size:   ~a\n" (format-size size))
                       (format "  Branches:    ~a\n" branches)
                       (format "  Path:        ~a" path)))))

;; ============================================================
;; sessions-delete — delete a session
;; ============================================================

;; Delete a session by ID. Returns 'ok, 'not-found, or 'cancelled.
;; If confirm? is #t, prompts for confirmation via the given ports.
(define (sessions-delete session-dir
                         session-id
                         #:confirm? [confirm? #f]
                         #:in [in (current-input-port)]
                         #:out [out (current-output-port)])
  (define session-path (build-path session-dir session-id))
  (cond
    [(not (directory-exists? session-path)) 'not-found]
    [confirm?
     (display (format "Delete session ~a? [y/N] " session-id) out)
     (flush-output out)
     (define answer (string-trim (or (read-line in) "")))
     (if (or (string=? answer "y") (string=? answer "Y"))
         (begin
           (delete-directory/files session-path)
           'ok)
         'cancelled)]
    [else
     (delete-directory/files session-path)
     'ok]))

;; ============================================================
;; Formatting helpers
;; ============================================================

(define (pad-right s len)
  (define str
    (if (string? s)
        s
        (~a s)))
  (if (>= (string-length str) len)
      str
      (string-append str (make-string (- len (string-length str)) #\space))))

(define (pad2 n)
  (if (< n 10)
      (format "0~a" n)
      (~a n)))

(define (format-size bytes)
  (cond
    [(< bytes 1024) (format "~aB" bytes)]
    [(< bytes (* 1024 1024)) (format "~aKB" (quotient bytes 1024))]
    [else (format "~aMB" (quotient bytes (* 1024 1024)))]))

;; ============================================================
;; ============================================================
;; Trace display (v0.15.0)
;; ============================================================

(define (read-trace-entries path)
  (filter-map (lambda (line)
                (if (and (string? line) (> (string-length line) 0))
                    (with-handlers ([exn:fail? (lambda (e) #f)])
                      (string->jsexpr line))
                    #f))
              (file->lines path)))

(define (display-trace-formatted path)
  (define entries (read-trace-entries path))
  (for ([e (in-list entries)])
    (define phase (hash-ref e 'phase "?"))
    (define seq (hash-ref e 'seq 0))
    (define ts (hash-ref e 'ts ""))
    (define sid (hash-ref e 'sessionId ""))
    (printf "[~a] ~a ~a session=~a" seq ts phase sid)
    (define data (hash-ref e 'data #f))
    (when data
      (define parts
        (for/list ([(k v) (in-hash data)]
                   #:when (and v (not (equal? v 'null))))
          (format "~a=~a" k v)))
      (when (pair? parts)
        (printf " ~a" (string-join parts " "))))
    (newline)))

(define (display-trace-summary path)
  (define entries (read-trace-entries path))
  (define phases (map (lambda (e) (hash-ref e 'phase "?")) entries))
  (define iteration-count (count (lambda (p) (equal? p "iteration.decision")) phases))
  (define llm-requests (count (lambda (p) (equal? p "model.request.started")) phases))
  (define tool-calls (count (lambda (p) (equal? p "tool.call.started")) phases))
  (define errors (count (lambda (p) (equal? p "runtime.error")) phases))
  ;; Extract finish reasons
  (define finish-reasons
    (filter-map (lambda (e)
                  (if (equal? (hash-ref e 'phase #f) "model.stream.completed")
                      (hash-ref (hash-ref e 'data (hasheq)) 'finish_reason "?")
                      #f))
                entries))
  (printf "Session trace summary\n")
  (printf "  Events: ~a\n" (length entries))
  (printf "  Iterations: ~a\n" iteration-count)
  (printf "  LLM requests: ~a\n" llm-requests)
  (printf "  Tool calls: ~a\n" tool-calls)
  (printf "  Errors: ~a\n" errors)
  (when (pair? finish-reasons)
    (printf "  Finish reasons: ~a\n"
            (string-join (map (lambda (s) (format "~a" s)) finish-reasons) ", "))))

;; CLI dispatch — run-sessions-command
;; ============================================================

;; Run a `q sessions` CLI command.
;; Dispatches to sessions list/info/delete based on cli-config.
;; cfg must provide: cli-config-sessions-subcommand, cli-config-sessions-args, cli-config-session-dir
(define (run-sessions-command cfg)
  (define subcmd (cli-config-sessions-subcommand cfg))
  (define args (cli-config-sessions-args cfg))
  (define session-dir
    (or (cli-config-session-dir cfg)
        (let ([s (load-settings)]) (path->string (session-dir-from-settings s)))))
  (case subcmd
    [(list)
     (define limit
       (or (and (>= (length args) 1) (let ([n (string->number (car args))]) (and n n))) 20))
     (define sess-list (sessions-list session-dir #:limit limit))
     (for-each displayln (sessions-list->strings sess-list))]
    [(info)
     (define sid
       (if (>= (length args) 1)
           (car args)
           #f))
     (if sid
         (displayln (sessions-info->string (sessions-info session-dir sid)))
         (displayln "Usage: q sessions info <id>"))]
    [(delete)
     (define sid
       (if (>= (length args) 1)
           (car args)
           #f))
     (if sid
         (let ([result (sessions-delete session-dir sid #:confirm? #t)])
           (case result
             [(ok) (displayln (format "Session ~a deleted." sid))]
             [(not-found) (displayln (format "Session not found: ~a" sid))]
             [(cancelled) (displayln "Cancelled.")]))
         (displayln "Usage: q sessions delete <id>"))]
    [(verify)
     ;; q sessions verify <path> [--repair]
     ;; or q verify-session <path> [--repair]
     (define path-arg
       (if (>= (length args) 1)
           (car args)
           #f))
     (define repair? (member "--repair" args))
     (if path-arg
         (let* ([log-path path-arg]
                [chain-report (verify-hash-chain log-path)]
                [struct-report (verify-session-integrity log-path)])
           (printf "Session: ~a\n" path-arg)
           (printf "  Entries: ~a\n" (hash-ref struct-report 'total-entries 0))
           (printf "  Valid (structure): ~a\n" (hash-ref struct-report 'valid-entries 0))
           (printf "  Hash chain: ~a\n" (if (hash-ref chain-report 'valid?) "OK" "BROKEN"))
           (when (hash-ref chain-report 'has-hashes? #f)
             (printf "  Chain length: ~a\n" (hash-ref chain-report 'chain-length 0))
             (printf "  Broken links: ~a\n" (hash-ref chain-report 'broken-links 0)))
           (cond
             [(not (hash-ref chain-report 'has-hashes? #f))
              (displayln "  (Legacy log — no hash chain)")
              (exit 2)]
             [(not (hash-ref chain-report 'valid?))
              (if repair?
                  (let ([rr (repair-session-log! log-path)])
                    (displayln "  Repairing...")
                    (printf "  Repaired: ~a kept, ~a removed\n"
                            (hash-ref rr 'entries-kept 0)
                            (hash-ref rr 'entries-removed 0))
                    (exit 0))
                  (exit 1))]
             [else (exit 0)]))
         (begin
           (displayln "Usage: q verify-session <path> [--repair]")
           (displayln "       q sessions verify <path> [--repair]")
           (exit 1)))]
    [(trace)
     ;; q sessions trace <session-id> [--json] [--summary]
     (define sid
       (if (>= (length args) 1)
           (car args)
           #f))
     (define json? (member "--json" args))
     (define summary? (member "--summary" args))
     (if sid
         (let ([trace-path (build-path session-dir sid "trace.jsonl")])
           (cond
             [(not (file-exists? trace-path))
              (displayln (format "No trace file for session: ~a" sid))]
             [summary? (display-trace-summary trace-path)]
             [json? (displayln (file->string trace-path))]
             [else (display-trace-formatted trace-path)]))
         (displayln "Usage: q sessions trace <session-id> [--json] [--summary]"))]
    [else (displayln "Usage: q sessions <list|info|delete|verify|trace> [args]")]))
