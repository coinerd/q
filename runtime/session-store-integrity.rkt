#lang racket/base
;; STABILITY: evolving

;; runtime/session-store-integrity.rkt — hash chain, verification, and repair
;;
;; Extracted from session-store.rkt (v0.22.9 W2).
;; Provides:
;;   Write-ahead marker: pending-marker-path, has-pending-marker?,
;;                        write-pending-marker!, remove-pending-marker!
;;   Hash chain: GENESIS-HASH, compute-event-hash, verify-hash-chain,
;;               read-last-hash, recompute-hash-chain
;;   Integrity: verify-session-integrity, repair-session-log!

(require racket/contract
         racket/hash
         racket/match
         racket/file
         racket/string
         racket/sequence
         racket/port
         file/sha1
         json
         "../util/protocol-types.rkt"
         "../util/jsonl.rkt"
         (only-in "../util/message-helpers.rkt" ensure-parent-dirs!))

(provide GENESIS-HASH
         compute-event-hash
         canonical-jsexpr->string
         verify-hash-chain
         read-last-hash
         recompute-hash-chain
         verify-session-integrity
         repair-session-log!
         ;; Write-ahead marker
         pending-marker-path
         has-pending-marker?
         (contract-out [write-pending-marker! (path-string? exact-nonnegative-integer? . -> . void?)]
                       [remove-pending-marker! (path-string? . -> . void?)]))

;; ── Write-ahead marker ──

(define (pending-marker-path session-log-path)
  (define base
    (if (path? session-log-path)
        (path->string session-log-path)
        session-log-path))
  (string->path (string-append base ".pending")))

(define (write-pending-marker! session-log-path entry-count)
  (define marker-path (pending-marker-path session-log-path))
  (ensure-parent-dirs! marker-path)
  (call-with-output-file marker-path
                         (lambda (out) (write entry-count out))
                         #:mode 'text
                         #:exists 'truncate))

(define (remove-pending-marker! session-log-path)
  (define marker-path (pending-marker-path session-log-path))
  (when (file-exists? marker-path)
    (delete-file marker-path)))

(define (has-pending-marker? session-log-path)
  (file-exists? (pending-marker-path session-log-path)))

;; ── Hash chain (#1287) ──

(define GENESIS-HASH "genesis")

(define (canonical-jsexpr->string v)
  (match v
    [(? hash?)
     (define sorted-keys (sort (hash-keys v) symbol<?))
     (define pairs
       (for/list ([k (in-list sorted-keys)])
         (format "\"~a\": ~a" k (canonical-jsexpr->string (hash-ref v k)))))
     (format "{~a}" (string-join pairs ", "))]
    [(? list?) (format "[~a]" (string-join (map canonical-jsexpr->string v) ", "))]
    [(? string?) (jsexpr->string v)]
    [(? number?) (number->string v)]
    [(? boolean?) (if v "true" "false")]
    ['null "null"]
    [(? symbol?) (format "\"~a\"" v)]
    [_ (format "~a" v)]))

(define (compute-event-hash entry prev-hash)
  (define with-prev (hash-set entry 'prev_hash prev-hash))
  (define without-hash (hash-remove with-prev 'hash))
  (define canonical (canonical-jsexpr->string without-hash))
  (bytes->hex-string (sha256-bytes (open-input-string canonical))))

(define (read-last-hash path)
  (define last-entries (jsonl-read-last path 1))
  (match last-entries
    ['() GENESIS-HASH]
    [_
     (define last (car last-entries))
     (if (hash? last)
         (hash-ref last 'hash GENESIS-HASH)
         GENESIS-HASH)]))

(define (recompute-hash-chain entries)
  (define prev-box (box GENESIS-HASH))
  (for/list ([e (in-list entries)])
    (define prev (unbox prev-box))
    (define h (compute-event-hash e prev))
    (set-box! prev-box h)
    (hash-set* e 'prev_hash prev 'hash h)))

;; ── Hash chain verification (#1287) ──

(define (verify-hash-chain session-log-path)
  (match (file-exists? session-log-path)
    [#f (hasheq 'valid? #t 'has-hashes? #f 'chain-length 0 'broken-links 0 'broken-at '())]
    [_
     (define entries (jsonl-read-all session-log-path))
     (match entries
       ['() (hasheq 'valid? #t 'has-hashes? #f 'chain-length 0 'broken-links 0 'broken-at '())]
       [_
        (define numbered
          (for/list ([e (in-list entries)]
                     [i (in-naturals 1)])
            (cons i e)))
        (define has-any-hashes?
          (for/or ([ne (in-list numbered)])
            (and (hash? (cdr ne)) (hash-has-key? (cdr ne) 'hash))))
        (match has-any-hashes?
          [#f (hasheq 'valid? #t 'has-hashes? #f 'chain-length 0 'broken-links 0 'broken-at '())]
          [_
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
                 [(not entry-hash) (values broken at prev)]
                 [(not (equal? entry-prev prev)) (values (add1 broken) (cons i at) entry-hash)]
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

;; ── Integrity verification ──

(define (verify-session-integrity session-log-path)
  (match (file-exists? session-log-path)
    [#f
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
    [_
     (define raw-bytes (file->bytes session-log-path))
     (define truncated-at-end?
       (and (> (bytes-length raw-bytes) 0)
            (not (= (bytes-ref raw-bytes (- (bytes-length raw-bytes) 1)) 10))))

     (define lines
       (call-with-input-file session-log-path
                             (lambda (in) (sequence->list (in-lines in)))
                             #:mode 'text))

     (define numbered-lines
       (for/list ([line (in-list lines)]
                  [i (in-naturals 1)]
                  #:when (> (string-length (string-trim line)) 0))
         (cons i line)))

     (define total-entries (length numbered-lines))

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
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-warning (format "session-store: ~a" (exn-message e)))
                                           #f)])
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
  (match (file-exists? session-log-path)
    [#f (hasheq 'entries-kept 0 'entries-removed 0)]
    [_
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
           [(<= (string-length (string-trim line)) 0) (values valid rmvd seen)]
           [(not (jsonl-line-valid? line)) (values valid (+ rmvd 1) seen)]
           [else
            (define parsed
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-warning (format "session-store: ~a" (exn-message e)))
                                           #f)])
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

     (define bak-path #f)
     (when (> removed 0)
       (set! bak-path (string-append (path->string session-log-path) ".bak"))
       (when (file-exists? bak-path)
         (delete-file bak-path))
       (rename-file-or-directory session-log-path bak-path #t)
       (unless (null? valid-entries)
         (jsonl-append-entries! session-log-path valid-entries)))

     (remove-pending-marker! session-log-path)

     (when (> removed 0)
       (fprintf (current-error-port)
                "Session repaired: ~a lines recovered, ~a lines removed. Backup saved to ~a.\n"
                (length valid-entries)
                removed
                (or bak-path "N/A")))

     (when (> removed 0)
       (when (file-exists? session-log-path)
         (define repaired-entries (jsonl-read-all session-log-path))
         (define chained (recompute-hash-chain repaired-entries))
         (delete-file session-log-path)
         (for ([e (in-list chained)])
           (jsonl-append! session-log-path e))))

     (hasheq 'entries-kept (length valid-entries) 'entries-removed removed)]))
