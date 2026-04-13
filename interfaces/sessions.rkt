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
         read-session-metadata)

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
              (with-handlers ([exn:fail? (lambda (_) 0)])
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
    (with-handlers ([exn:fail? (lambda (_) 0)])
      (file-or-directory-modify-seconds jsonl-path)))
  ;; Count entries and extract model from first few messages
  (define entries
    (with-handlers ([exn:fail? (lambda (_) '())])
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
       (with-handlers ([exn:fail? (lambda (_) '())])
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
    [else (displayln "Usage: q sessions <list|info|delete> [args]")]))
