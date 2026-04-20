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
         racket/port
         file/sha1
         json
         "../util/protocol-types.rkt"
         "../util/jsonl.rkt"
         (only-in "../util/message-helpers.rkt" ensure-parent-dirs!))

(provide (contract-out [append-entry! (path-string? message? . -> . void?)]
                       [append-entries! (path-string? (listof message?) . -> . void?)]
                       [load-session-log (->* (path-string?) ((or/c #f string?)) (listof message?))]
                       [replay-session (path-string? . -> . (listof message?))]
                       [verify-session-integrity (path-string? . -> . hash?)]
                       [repair-session-log! (path-string? . -> . hash?)]
                       [has-pending-marker? (path-string? . -> . boolean?)]
                       [pending-marker-path (path-string? . -> . path?)])
         ;; Session versioning (#499)
         CURRENT-SESSION-VERSION
         write-session-version-header!
         ensure-session-version-header!
         migrate-session-log!
         ;; Session forking (#500)
         fork-session!
         ;; Session import (#1113)
         import-session!
         ;; Session naming
         write-session-name!
         ;; In-memory session manager (GC-18)
         in-memory-session-manager?
         make-in-memory-session-manager
         in-memory-append!
         in-memory-append-entries!
         in-memory-load
         in-memory-list-sessions
         in-memory-fork!
         ;; Custom entry helpers (#1147)
         append-custom-entry!
         load-custom-entries
         ;; Tree store operations (#1315)
         append-tree-entry!
         load-tree
         get-tree-branch
         get-children
         resolve-active-branch
         tree-info
         ;; Hash chain (#1287)
         verify-hash-chain
         compute-event-hash
         GENESIS-HASH)

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
  (ensure-parent-dirs! marker-path)
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

;; ensure-parent-dirs! imported from util/message-helpers.rkt

;; ── Hash chain (#1287) ──

;; Sentinel value for the first entry in a hash chain.
(define GENESIS-HASH "genesis")

;; Compute the SHA-256 hash for a JSONL event entry.
;; Hash input: canonical JSON of the entry with prev_hash field,
;; but with 'hash' field removed (so we don't hash our own output).
;; Canonical JSON serialization: sort keys alphabetically for deterministic hashing.
;; Manually builds JSON string to guarantee key ordering independent of hash impl.
(define (canonical-jsexpr->string v)
  (cond
    [(hash? v)
     (define sorted-keys (sort (hash-keys v) symbol<?))
     (define pairs
       (for/list ([k (in-list sorted-keys)])
         (format "\"~a\": ~a" k (canonical-jsexpr->string (hash-ref v k)))))
     (format "{~a}" (string-join pairs ", "))]
    [(list? v) (format "[~a]" (string-join (map canonical-jsexpr->string v) ", "))]
    [(string? v) (jsexpr->string v)]
    [(number? v) (number->string v)]
    [(boolean? v) (if v "true" "false")]
    [(eq? v 'null) "null"]
    [(symbol? v) (format "\"~a\"" v)]
    [else (format "~a" v)]))

(define (compute-event-hash entry prev-hash)
  (define with-prev (hash-set entry 'prev_hash prev-hash))
  (define without-hash (hash-remove with-prev 'hash))
  (define canonical (canonical-jsexpr->string without-hash))
  (bytes->hex-string (sha256-bytes (open-input-string canonical))))

;; Read the hash of the last entry in a JSONL file (or GENESIS-HASH if empty/missing).
(define (read-last-hash path)
  (define last-entries (jsonl-read-last path 1))
  (cond
    [(null? last-entries) GENESIS-HASH]
    [else
     (define last (car last-entries))
     (if (hash? last)
         (hash-ref last 'hash GENESIS-HASH)
         GENESIS-HASH)]))

;; Recompute hash chain for a list of jsexpr entries.
;; Returns list of entries with valid prev_hash and hash fields.
(define (recompute-hash-chain entries)
  (define prev-box (box GENESIS-HASH))
  (for/list ([e (in-list entries)])
    (define prev (unbox prev-box))
    (define h (compute-event-hash e prev))
    (set-box! prev-box h)
    (hash-set* e 'prev_hash prev 'hash h)))

;; ── Append operations ──

;; path-string? message? -> void?
(define (append-entry! path msg)
  ;; Append a single message entry to the session JSONL log.
  ;; Uses write-ahead marker for crash safety.
  ;; Includes hash chain for tamper detection (#1287).
  ;; 3.4: dynamic-wind guarantees marker removal on write failure.
  (write-pending-marker! path 1)
  (dynamic-wind void
                (lambda ()
                  (define prev-hash (read-last-hash path))
                  (define entry (message->jsexpr msg))
                  (define h (compute-event-hash entry prev-hash))
                  (define chained-entry (hash-set* entry 'prev_hash prev-hash 'hash h))
                  (jsonl-append! path chained-entry))
                (lambda () (remove-pending-marker! path))))

;; path-string? (listof message?) -> void?
(define (append-entries! path msgs)
  ;; Append multiple message entries atomically per commit boundary.
  ;; All entries are written in one filesystem operation.
  ;; If msgs is empty, does nothing (and does not create the file).
  ;; Includes hash chain for tamper detection (#1287).
  (when (null? msgs)
    (void))
  (unless (null? msgs)
    (write-pending-marker! path (length msgs))
    (dynamic-wind void
                  (lambda ()
                    (define prev-hash (read-last-hash path))
                    (define prev-box (box prev-hash))
                    (define jsexprs
                      (for/list ([msg (in-list msgs)])
                        (define entry (message->jsexpr msg))
                        (define prev (unbox prev-box))
                        (define h (compute-event-hash entry prev))
                        (set-box! prev-box h)
                        (hash-set* entry 'prev_hash prev 'hash h)))
                    (jsonl-append-entries! path jsexprs))
                  (lambda () (remove-pending-marker! path)))))

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
  ;; [ADMIN-ONLY] Remove truncated/invalid entries and rewrite the log cleanly.
  ;; This function modifies the append-only session log. It MUST only be called
  ;; by admin tooling or manual recovery procedures, never automatically.
  ;; A .bak backup is created before any rewrite (SEC-09).
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

     ;; Recompute hash chain on repaired entries
     (when (> removed 0)
       (when (file-exists? session-log-path)
         (define repaired-entries (jsonl-read-all session-log-path))
         (define chained (recompute-hash-chain repaired-entries))
         (delete-file session-log-path)
         (for ([e (in-list chained)])
           (jsonl-append! session-log-path e))))

     (hasheq 'entries-kept (length valid-entries) 'entries-removed removed)]))

;; ── Hash chain verification (#1287) ──

(define (verify-hash-chain session-log-path)
  ;; Verify the hash chain of a session log.
  ;; Returns a hash with:
  ;;   'valid?        — #t if chain is intact (or empty)
  ;;   'has-hashes?   — #t if any entry has hash fields (false for legacy logs)
  ;;   'chain-length  — number of entries with hash fields
  ;;   'broken-links  — count of broken prev_hash → hash links
  ;;   'broken-at     — list of line numbers where chain breaks
  (cond
    [(not (file-exists? session-log-path))
     (hasheq 'valid? #t 'has-hashes? #f 'chain-length 0 'broken-links 0 'broken-at '())]
    [else
     (define entries (jsonl-read-all session-log-path))
     (cond
       [(null? entries)
        (hasheq 'valid? #t 'has-hashes? #f 'chain-length 0 'broken-links 0 'broken-at '())]
       [else
        (define numbered
          (for/list ([e (in-list entries)]
                     [i (in-naturals 1)])
            (cons i e)))
        (define has-any-hashes?
          (for/or ([ne (in-list numbered)])
            (and (hash? (cdr ne)) (hash-has-key? (cdr ne) 'hash))))
        (cond
          [(not has-any-hashes?)
           (hasheq 'valid? #t 'has-hashes? #f 'chain-length 0 'broken-links 0 'broken-at '())]
          [else
           ;; Walk the chain
           (define-values (broken-links broken-at prev-hash)
             (for/fold ([broken 0]
                        [at '()]
                        [prev GENESIS-HASH])
                       ([ne (in-list numbered)])
               (define i (car ne))
               (define e (cdr ne))
               (define entry-prev (hash-ref e 'prev_hash #f))
               (define entry-hash (hash-ref e 'hash #f))
               (cond
                 ;; Entry without hash — skip, chain continues from prev
                 [(not entry-hash) (values broken at prev)]
                 ;; Broken link!
                 [(not (equal? entry-prev prev)) (values (add1 broken) (cons i at) entry-hash)]
                 ;; Valid link
                 [else (values broken at entry-hash)])))
           (hasheq 'valid?
                   (= broken-links 0)
                   'has-hashes?
                   #t
                   'chain-length
                   (length entries)
                   'broken-links
                   broken-links
                   'broken-at
                   (reverse broken-at))])])]))

;; ============================================================
;; Session versioning (#499)
;; ============================================================

;; Current session log format version.
;; Version 1: original format (no version header).
;; Version 2: version header as first entry (kind=session-info, meta.version=2).
(define CURRENT-SESSION-VERSION 2)

;; ============================================================
;; Session forking (#500)
;; ============================================================

;; ============================================================
;; Session naming
;; ============================================================

;; ============================================================
;; Session import (#1113)
;; ============================================================

(define (import-session! source-path dest-session-dir)
  ;; Import a session from an external JSONL file into a new session directory.
  ;; Validates source JSONL line-by-line, generates a new session ID,
  ;; copies valid entries to a new session file with a version header.
  ;; Returns the new session ID.
  ;;
  ;; source-path: path to source JSONL file
  ;; dest-session-dir: directory for the new session
  (cond
    [(not (file-exists? source-path))
     (error 'import-session! "Source file not found: ~a" source-path)]
    [else
     ;; Validate source JSONL
     (define-values (raw corrupted-count) (jsonl-read-all-valid-with-count source-path))
     (when (> corrupted-count 0)
       (fprintf (current-error-port)
                "WARNING: ~a corrupted lines skipped during import from ~a\n"
                corrupted-count
                source-path))
     (when (null? raw)
       (error 'import-session! "No valid entries found in source: ~a" source-path))
     ;; Generate new session ID
     (define new-session-id (format "imported-~a" (current-inexact-milliseconds)))
     ;; Create destination directory
     (define dest-path (build-path dest-session-dir (format "~a.jsonl" new-session-id)))
     (ensure-parent-dirs! dest-path)
     ;; Write version header + imported entries
     (define header-msg
       (make-message (format "session-version-header-~a" (current-inexact-milliseconds))
                     #f
                     'system
                     'session-info
                     '()
                     (current-seconds)
                     (hasheq 'version
                             CURRENT-SESSION-VERSION
                             'imported-from
                             (path->string source-path)
                             'imported-at
                             (current-seconds))))
     (define messages (map jsexpr->message raw))
     (define all-entries (cons header-msg messages))
     (jsonl-append-entries! dest-path (map message->jsexpr all-entries))
     new-session-id]))

(define (write-session-name! log-path name)
  ;; Persist session name as a session-info entry.
  (define name-msg
    (make-message (format "session-name-~a" (current-inexact-milliseconds))
                  #f
                  'system
                  'session-info
                  (list (make-text-part (format "Session renamed: ~a" name)))
                  (current-seconds)
                  (hasheq 'name name)))
  (append-entry! log-path name-msg))

(define (fork-session! source-path source-entry-id dest-path)
  ;; Extract root→entry path from source session to a new JSONL file.
  ;; The new session starts with a version header and all entries from
  ;; the extracted path. Original session is unchanged.
  ;; Returns the number of entries copied.
  ;;
  ;; source-path: path to source session JSONL
  ;; source-entry-id: id of the entry to fork at (inclusive)
  ;; dest-path: path for the new forked session JSONL
  (define entries (load-session-log source-path))
  ;; Build by-id map
  (define by-id (make-hash))
  (for ([e (in-list entries)])
    (hash-set! by-id (message-id e) e))
  ;; Walk from entry to root
  (define (walk-up id acc)
    (define e (hash-ref by-id id #f))
    (cond
      [(not e) acc]
      [else
       (define new-acc (cons e acc))
       (define pid (message-parent-id e))
       (if pid
           (walk-up pid new-acc)
           new-acc)]))
  (define path-entries (walk-up source-entry-id '()))
  ;; Write version header + path entries to dest
  (ensure-parent-dirs! dest-path)
  (define header-msg
    (make-message
     (format "session-version-header-~a" (current-inexact-milliseconds))
     #f
     'system
     'session-info
     '()
     (current-seconds)
     (hasheq 'version CURRENT-SESSION-VERSION 'parentSession (path->string source-path))))
  (define all-entries (cons header-msg path-entries))
  ;; Write dest file
  (jsonl-append-entries! dest-path (map message->jsexpr all-entries))
  ;; Return count (excluding header)
  (length path-entries))

(define (write-session-version-header! log-path)
  ;; Write a version header entry as the first line of a new session log.
  ;; Only writes if the file doesn't exist or is empty.
  (ensure-parent-dirs! log-path)
  (cond
    [(and (file-exists? log-path) (> (file-size log-path) 0))
     (void)] ;; Already has content — don't prepend
    [else
     (define header-msg
       (make-message (format "session-version-header-~a" (current-inexact-milliseconds))
                     #f
                     'system
                     'session-info
                     '()
                     (current-seconds)
                     (hasheq 'version CURRENT-SESSION-VERSION)))
     ;; Write directly (don't use append-entry! to avoid marker overhead)
     (call-with-output-file log-path
                            (lambda (out)
                              (write-json (message->jsexpr header-msg) out)
                              (newline out))
                            #:mode 'text
                            #:exists 'truncate)]))

(define (read-first-log-entry log-path)
  ;; Read only the first line of a JSONL log file.
  ;; Returns the first message or #f if file is empty/missing.
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (call-with-input-file log-path
                          (lambda (in)
                            (define line (read-line in))
                            (if (eof-object? line)
                                #f
                                (jsexpr->message (string->jsexpr line))))
                          #:mode 'text)))

(define (ensure-session-version-header! log-path)
  ;; Check if session log has a version header; add one if not.
  ;; Returns the detected version number.
  (cond
    [(not (file-exists? log-path))
     (write-session-version-header! log-path)
     CURRENT-SESSION-VERSION]
    [(= (file-size log-path) 0)
     (write-session-version-header! log-path)
     CURRENT-SESSION-VERSION]
    [else
     ;; Check first line for version header (Fix #517: O(1) instead of O(n))
     (define first-entry (read-first-log-entry log-path))
     (cond
       [(not first-entry)
        (write-session-version-header! log-path)
        CURRENT-SESSION-VERSION]
       [(session-info-entry? first-entry) (hash-ref (message-meta first-entry) 'version 1)]
       [else
        ;; Old format (v1) — no version header. Run migration.
        (migrate-session-log! log-path 1 CURRENT-SESSION-VERSION)
        CURRENT-SESSION-VERSION])]))

(define (migrate-session-log! log-path from-version to-version)
  ;; Migrate session log from one version to another.
  ;; Currently only supports 1->2 (add version header).
  (when (< from-version to-version)
    (define entries (load-session-log log-path))
    (define header-msg
      (make-message (format "session-version-header-~a" (current-inexact-milliseconds))
                    #f
                    'system
                    'session-info
                    '()
                    (current-seconds)
                    (hasheq 'version to-version)))
    ;; Rewrite with header prepended
    (define all-entries (cons header-msg entries))
    ;; Backup original
    (define bak-path (format "~a.v~a.bak" log-path from-version))
    (when (file-exists? bak-path)
      (delete-file bak-path))
    (rename-file-or-directory log-path bak-path #t)
    ;; Write new version
    (jsonl-append-entries! log-path (map message->jsexpr all-entries))
    (fprintf (current-error-port)
             "Session migrated v~a -> v~a: ~a entries. Backup: ~a\n"
             from-version
             to-version
             (length entries)
             bak-path)))

;; ============================================================
;; In-memory session manager (GC-18)
;; ============================================================

;; In-memory store: maps session-id -> (box (listof message?))
;; Provides the same interface as file-backed storage but no disk I/O.
;; Useful for SDK unit tests and extension testing.

(struct in-memory-session-manager (sessions-box) #:transparent)

(define (make-in-memory-session-manager)
  (in-memory-session-manager (box (hash))))

;; Append a message to an in-memory session.
(define (in-memory-append! mgr session-id msg)
  (define box (in-memory-session-manager-sessions-box mgr))
  (define sessions (unbox box))
  (define existing (hash-ref sessions session-id '()))
  (set-box! box (hash-set sessions session-id (append existing (list msg)))))

;; Append multiple messages atomically.
(define (in-memory-append-entries! mgr session-id msgs)
  (for ([msg (in-list msgs)])
    (in-memory-append! mgr session-id msg)))

;; Load all messages from an in-memory session.
(define (in-memory-load mgr session-id)
  (define sessions (unbox (in-memory-session-manager-sessions-box mgr)))
  (hash-ref sessions session-id '()))

;; List all session IDs.
(define (in-memory-list-sessions mgr)
  (hash-keys (unbox (in-memory-session-manager-sessions-box mgr))))

;; Fork: copy entries up to entry-id (or all) into a new session.
(define (in-memory-fork! mgr src-id dest-id [entry-id #f])
  (define entries (in-memory-load mgr src-id))
  (define to-copy
    (if entry-id
        (let loop ([es entries]
                   [acc '()])
          (cond
            [(null? es) (reverse acc)]
            [(equal? (message-id (car es)) entry-id) (reverse (cons (car es) acc))]
            [else (loop (cdr es) (cons (car es) acc))]))
        entries))
  (define box (in-memory-session-manager-sessions-box mgr))
  (define sessions (unbox box))
  (set-box! box (hash-set sessions dest-id to-copy))
  dest-id)

;; ============================================================
;; Custom entry helpers (#1147)
;; ============================================================

;; Append a custom entry to an in-memory session.
;; extension-name, key, data are stored in the message meta field.
(define (append-custom-entry! mgr session-id extension-name key data)
  (define entry (make-custom-entry extension-name key data))
  (in-memory-append! mgr session-id entry))

;; Load custom entries for a specific extension from an in-memory session.
;; Optionally filter by key.
(define (load-custom-entries mgr session-id extension-name [key #f])
  (define entries (in-memory-load mgr session-id))
  (filter (lambda (e)
            (and (custom-entry? e)
                 (equal? (custom-entry-extension e) extension-name)
                 (or (not key) (equal? (custom-entry-key e) key))))
          entries))

;; ============================================================
;; Tree store operations (#1315)
;; ============================================================
;; Tree operations work on top of the JSONL session log.
;; Tree structure is derived from parentId fields.
;; All tree entries are message structs with specific kinds.

(define (append-tree-entry! path entry #:before-hook [before-hook #f] #:after-hook [after-hook #f])
  ;; Append a tree entry (branch/navigation/summary) to the JSONL log.
  ;; Uses the same append-entry! path for hash chain integrity.
  ;; #:before-hook: (symbol? any/c -> void?) called before append; errors block the append.
  ;; #:after-hook: (symbol? any/c -> void?) called after successful append.
  (when before-hook
    (before-hook 'session-before-tree
                 (hasheq 'entry-type
                         (message-kind entry)
                         'entry-id
                         (message-id entry)
                         'parent-id
                         (message-parent-id entry))))
  (append-entry! path entry)
  (when after-hook
    (after-hook 'session-tree
                (hasheq 'entry-type
                        (message-kind entry)
                        'entry-id
                        (message-id entry)
                        'parent-id
                        (message-parent-id entry)))))

(define (load-tree path)
  ;; Load all entries from JSONL and build a tree map.
  ;; Returns a hash with:
  ;;   '%%entries%% -> (listof message?) — all entries in order
  ;;   id -> (listof message?) — direct children of id
  (define entries (load-session-log path))
  (define children-map (make-hash))
  ;; Initialize all IDs with empty children lists
  (for ([e (in-list entries)])
    (hash-set! children-map (message-id e) '()))
  ;; Build parent -> children mapping
  (for ([e (in-list entries)])
    (define pid (message-parent-id e))
    (when pid
      (define existing (hash-ref children-map pid '()))
      (hash-set! children-map pid (append existing (list e)))))
  ;; Store raw entries for by-id lookup
  (hash-set! children-map '%%entries%% entries)
  children-map)

(define (get-tree-branch tree target-id)
  ;; Get the path from root to target-id (inclusive).
  ;; Returns a list of messages from root to target.
  (define entries (hash-ref tree '%%entries%% '()))
  (define by-id (make-hash))
  (for ([e (in-list entries)])
    (hash-set! by-id (message-id e) e))
  (define target (hash-ref by-id target-id #f))
  (cond
    [(not target) '()]
    [else
     (define (walk-up entry acc)
       (define new-acc (cons entry acc))
       (define pid (message-parent-id entry))
       (cond
         [(not pid) new-acc]
         [else
          (define parent (hash-ref by-id pid #f))
          (if parent
              (walk-up parent new-acc)
              new-acc)]))
     (walk-up target '())]))

(define (get-children tree entry-id)
  ;; Get direct children of an entry.
  (hash-ref tree entry-id '()))

(define (resolve-active-branch tree)
  ;; Find the active branch (path from root to the last entry).
  ;; tree: the tree map from load-tree
  (define entries (hash-ref tree '%%entries%% '()))
  (cond
    [(null? entries) '()]
    [else
     (define by-id (make-hash))
     (for ([e (in-list entries)])
       (hash-set! by-id (message-id e) e))
     (define last-entry (list-ref entries (sub1 (length entries))))
     (define (walk-up entry acc)
       (define new-acc (cons entry acc))
       (define pid (message-parent-id entry))
       (cond
         [(not pid) new-acc]
         [else
          (define parent (hash-ref by-id pid #f))
          (if parent
              (walk-up parent new-acc)
              new-acc)]))
     (walk-up last-entry '())]))

(define (tree-info tree)
  ;; Return metadata about the tree structure.
  ;; Returns a hash with:
  ;;   'total-entries — total number of entries
  ;;   'branch-count — number of branch entries
  ;;   'navigation-count — number of navigation entries
  ;;   'summary-count — number of summary entries
  ;;   'leaf-ids — list of entry IDs with no children
  (define entries (hash-ref tree '%%entries%% '()))
  (define branch-count 0)
  (define navigation-count 0)
  (define summary-count 0)
  (for ([e (in-list entries)])
    (case (message-kind e)
      [(branch) (set! branch-count (add1 branch-count))]
      [(tree-navigation) (set! navigation-count (add1 navigation-count))]
      [(branch-summary) (set! summary-count (add1 summary-count))]
      [else (void)]))
  ;; Leaf IDs: entries that have no children in the tree
  (define leaf-ids
    (for/list ([e (in-list entries)]
               #:when (null? (hash-ref tree (message-id e) '())))
      (message-id e)))
  (hasheq 'total-entries
          (length entries)
          'branch-count
          branch-count
          'navigation-count
          navigation-count
          'summary-count
          summary-count
          'leaf-ids
          leaf-ids))
