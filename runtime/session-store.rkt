#lang racket/base

;;; runtime/session-store.rkt — append-only JSONL session storage
;;;
;;; Provides:
;;;   append-entry!            — append a single message to session log
;;;   append-entries!          — append multiple messages atomically
;;;   load-session-log         — load all valid messages from JSONL file
;;;   replay-session           — replay session log, return messages in order
;;;   verify-session-integrity — scan a session log and return integrity report
;;;   repair-session-log!      — remove truncated/invalid entries, rewrite cleanly
;;;   has-pending-marker?      — check if write-ahead marker exists
;;;   pending-marker-path      — get path of write-ahead marker file
;;;
;;; Crash safety:
;;;   - Write-ahead marker created before append, removed after
;;;   - Truncated entry detection via newline check
;;;   - Integrity verification scans for structural problems
;;;   - Repair rewrites log with only valid entries

(require racket/contract
         racket/file
         racket/string
         racket/sequence
         json
         "../util/protocol-types.rkt"
         "../util/jsonl.rkt")

(provide append-entry!
         append-entries!
         load-session-log
         replay-session
         verify-session-integrity
         repair-session-log!
         has-pending-marker?
         pending-marker-path)

;; ── Write-ahead marker ──

(define (pending-marker-path session-log-path)
  ;; Return the path of the write-ahead marker for the given session log.
  ;; Marker lives at <session-log>.pending
  (define base
    (if (path? session-log-path)
        (path->string session-log-path)
        session-log-path))
  (string->path (string-append base ".pending")))

(define (write-pending-marker! session-log-path entry-count)
  ;; Create the write-ahead marker file before appending.
  (define marker-path (pending-marker-path session-log-path))
  (ensure-parent-dirs* marker-path)
  (call-with-output-file marker-path
                         (lambda (out) (write entry-count out))
                         #:mode 'text
                         #:exists 'truncate))

(define (remove-pending-marker! session-log-path)
  ;; Remove the write-ahead marker after successful append.
  (define marker-path (pending-marker-path session-log-path))
  (when (file-exists? marker-path)
    (delete-file marker-path)))

(define (has-pending-marker? session-log-path)
  ;; Check if a write-ahead marker exists for the session log.
  ;; If #t, the last entry may be truncated due to an interrupted write.
  ;; This is an advisory check, not a hard gate.
  (file-exists? (pending-marker-path session-log-path)))

(define (ensure-parent-dirs* path)
  ;; Create parent directories of path if they don't exist.
  (define-values (dir name must-be-dir?) (split-path path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir)))

;; ── Append operations ──

;; path-string? message? -> void?
(define (append-entry! path msg)
  ;; Append a single message entry to the session JSONL log.
  ;; Uses write-ahead marker for crash safety.
  (write-pending-marker! path 1)
  (jsonl-append! path (message->jsexpr msg))
  (remove-pending-marker! path))

;; path-string? (listof message?) -> void?
(define (append-entries! path msgs)
  ;; Append multiple message entries atomically per commit boundary.
  ;; All entries are written in one filesystem operation.
  ;; If msgs is empty, does nothing (and does not create the file).
  (when (null? msgs)
    (void))
  (unless (null? msgs)
    (write-pending-marker! path (length msgs))
    (define jsexprs (map message->jsexpr msgs))
    (jsonl-append-entries! path jsexprs)
    (remove-pending-marker! path)))

;; ── Load / replay ──

;; path-string? -> (listof message?)
(define (load-session-log path [session-id #f])
  ;; Load all valid entries for a session from JSONL file.
  ;; Skips corrupted/partial lines (crash recovery).
  ;; Returns empty list if file does not exist.
  ;; Logs a warning to stderr if any corrupted lines were skipped.
  (define-values (raw corrupted-count) (jsonl-read-all-valid-with-count path))
  (when (and (> corrupted-count 0) session-id)
    (fprintf
     (current-error-port)
     "WARNING: Skipped ~a corrupted lines in session ~a. Run 'q sessions repair <id>' to fix.\n"
     corrupted-count
     session-id))
  (when (> corrupted-count 0)
    (fprintf
     (current-error-port)
     "WARNING: Skipped ~a corrupted lines in session log ~a. Run 'q sessions repair' to fix.\n"
     corrupted-count
     path))
  (map jsexpr->message raw))

;; path-string? -> (listof message?)
(define (replay-session path)
  ;; Replay a session log and return messages in order.
  ;; Semantically identical to load-session-log — entries come back
  ;; in append order, preserving tree structure via parentId fields.
  (load-session-log path))

;; ── Integrity verification ──

(define (verify-session-integrity session-log-path)
  ;; Scan a session log and return an integrity report hash.
  ;; Checks: valid JSON per line, required fields, chronological order,
  ;;         no duplicate IDs, file ends with newline.
  ;; Returns:
  ;;   'total-entries      — count of non-empty lines
  ;;   'valid-entries      — count of entries passing all checks
  ;;   'invalid-entries    — list of (hash 'line-number N 'reason "...")
  ;;   'truncated-at-end?  — #t if file doesn't end with newline (non-empty file)
  ;;   'entry-order-valid? — #t if timestamps are non-decreasing among valid entries
  (cond
    [(not (file-exists? session-log-path))
     (hasheq 'total-entries
             0
             'valid-entries
             0
             'invalid-entries
             '()
             'truncated-at-end?
             #f
             'entry-order-valid?
             #t)]
    [else
     ;; Check if file ends with newline (byte 10)
     (define raw-bytes (file->bytes session-log-path))
     (define truncated-at-end?
       (and (> (bytes-length raw-bytes) 0)
            (not (= (bytes-ref raw-bytes (- (bytes-length raw-bytes) 1)) 10))))

     ;; Read all lines with line numbers
     (define lines
       (call-with-input-file session-log-path
                             (lambda (in) (sequence->list (in-lines in)))
                             #:mode 'text))

     ;; Numbered non-empty lines
     (define numbered-lines
       (for/list ([line (in-list lines)]
                  [i (in-naturals 1)]
                  #:when (> (string-length (string-trim line)) 0))
         (cons i line)))

     (define total-entries (length numbered-lines))

     ;; Validate each line: valid JSON → required fields → duplicate IDs
     (define-values (valid-entries invalid-entries _seen-ids)
       (for/fold ([valid '()]
                  [invalid '()]
                  [seen (hash)])
                 ([nl (in-list numbered-lines)])
         (define line-num (car nl))
         (define line-text (cdr nl))
         (cond
           [(not (jsonl-line-valid? line-text))
            (values valid
                    (append invalid (list (hasheq 'line-number line-num 'reason "invalid JSON")))
                    seen)]
           [else
            (define parsed
              (with-handlers ([exn:fail? (lambda (e) #f)])
                (read-json (open-input-string line-text))))
            (define required-fields '(id role kind content timestamp))
            (define missing
              (if parsed
                  (filter (lambda (f) (not (hash-has-key? parsed f))) required-fields)
                  required-fields))
            (cond
              [(not parsed)
               (values valid
                       (append invalid
                               (list (hasheq 'line-number line-num 'reason "failed to parse JSON")))
                       seen)]
              [(not (null? missing))
               (values valid
                       (append invalid
                               (list (hasheq 'line-number
                                             line-num
                                             'reason
                                             (format "missing required fields: ~a" missing))))
                       seen)]
              [(hash-has-key? seen (hash-ref parsed 'id))
               (values valid
                       (append invalid
                               (list (hasheq 'line-number
                                             line-num
                                             'reason
                                             (format "duplicate entry id: ~a"
                                                     (hash-ref parsed 'id)))))
                       seen)]
              [else
               (values (append valid (list (cons line-num parsed)))
                       invalid
                       (hash-set seen (hash-ref parsed 'id) #t))])])))

     ;; Check chronological order of valid entries (by timestamp)
     (define timestamps (map (lambda (p) (hash-ref (cdr p) 'timestamp)) valid-entries))
     (define entry-order-valid?
       (or (< (length timestamps) 2)
           (for/and ([a (in-list timestamps)]
                     [b (in-list (cdr timestamps))])
             (<= a b))))

     (hasheq 'total-entries
             total-entries
             'valid-entries
             (length valid-entries)
             'invalid-entries
             invalid-entries
             'truncated-at-end?
             truncated-at-end?
             'entry-order-valid?
             entry-order-valid?)]))

;; ── Repair ──

(define (repair-session-log! session-log-path)
  ;; Remove truncated/invalid entries and rewrite the log cleanly.
  ;; Preserves order of valid entries. Deduplicates by entry ID (keeps first).
  ;; Returns (hasheq 'entries-kept N 'entries-removed M).
  ;; If no entries need removal, the file is left unchanged (no-op).
  (cond
    [(not (file-exists? session-log-path)) (hasheq 'entries-kept 0 'entries-removed 0)]
    [else
     (define lines
       (call-with-input-file session-log-path
                             (lambda (in) (sequence->list (in-lines in)))
                             #:mode 'text))

     (define-values (valid-entries removed _seen-ids)
       (for/fold ([valid '()]
                  [rmvd 0]
                  [seen (hash)])
                 ([line (in-list lines)])
         (cond
           ;; Skip empty lines silently (not counted as removed)
           [(<= (string-length (string-trim line)) 0) (values valid rmvd seen)]
           [(not (jsonl-line-valid? line)) (values valid (+ rmvd 1) seen)]
           [else
            (define parsed
              (with-handlers ([exn:fail? (lambda (e) #f)])
                (read-json (open-input-string line))))
            (define required-fields '(id role kind content timestamp))
            (define missing
              (if parsed
                  (filter (lambda (f) (not (hash-has-key? parsed f))) required-fields)
                  required-fields))
            (define entry-id (and parsed (hash-ref parsed 'id #f)))
            (cond
              [(not parsed) (values valid (+ rmvd 1) seen)]
              [(not (null? missing)) (values valid (+ rmvd 1) seen)]
              [(and entry-id (hash-has-key? seen entry-id)) (values valid (+ rmvd 1) seen)]
              [else (values (append valid (list parsed)) rmvd (hash-set seen entry-id #t))])])))

     ;; Only rewrite if something was actually removed
     ;; Preserve original as .bak for forensic recovery (SEC-09)
     (define bak-path #f)
     (when (> removed 0)
       (set! bak-path (string-append (path->string session-log-path) ".bak"))
       (when (file-exists? bak-path)
         (delete-file bak-path))
       (rename-file-or-directory session-log-path bak-path #t)
       (unless (null? valid-entries)
         (jsonl-append-entries! session-log-path valid-entries)))

     ;; Always clean up pending marker after repair
     (remove-pending-marker! session-log-path)

     ;; Report repair results
     (when (> removed 0)
       (fprintf (current-error-port)
                "Session repaired: ~a lines recovered, ~a lines removed. Backup saved to ~a.\n"
                (length valid-entries)
                removed
                (or bak-path "N/A")))

     (hasheq 'entries-kept (length valid-entries) 'entries-removed removed)]))
