#lang racket/base

(require racket/file
         racket/list
         "state.rkt"
         "../util/jsonl.rkt"
         json)

;; Conversion
(provide transcript-entry->jsexpr
         jsexpr->transcript-entry

         ;; Persistence
         save-scrollback
         load-scrollback

         ;; Testing support
         reset-scrollback-id-counter!) ;; for test isolation

;; Maximum number of scrollback entries to keep on disk.
(define scrollback-max-entries 500)

;; Global counter for assigning IDs to deserialized entries.
;; Ensures reloaded entries get unique IDs so the render cache works.
(define scrollback-id-counter (box 0))

;; Reset the counter (for test isolation).
(define (reset-scrollback-id-counter!)
  (set-box! scrollback-id-counter 0))

;; Assign a unique ID from the scrollback counter.
(define (next-scrollback-id)
  (define id (unbox scrollback-id-counter))
  (set-box! scrollback-id-counter (add1 id))
  id)

;; Serialize a transcript-entry to a JSON-compatible hash.
(define (transcript-entry->jsexpr entry)
  (hasheq 'kind
          (symbol->string (transcript-entry-kind entry))
          'text
          (transcript-entry-text entry)
          'timestamp
          (transcript-entry-timestamp entry)
          'id
          (or (transcript-entry-id entry) 0)
          'meta
          (hash->jsexpr-deep (transcript-entry-meta entry))))

;; Deserialize a jsexpr hash back to a transcript-entry.
;; Assigns a unique ID so the render cache can track the entry.
(define (jsexpr->transcript-entry h)
  (transcript-entry (string->symbol (hash-ref h 'kind "system"))
                    (hash-ref h 'text "")
                    (hash-ref h 'timestamp 0)
                    (jsexpr->hash-deep (hash-ref h 'meta (hash)))
                    (next-scrollback-id)))

;; Save transcript-entries to a JSONL file.
;; Atomically rewrites with only the last scrollback-max-entries entries
;; to prevent unbounded file growth.
;; Accepts both string? and path? for the path argument.
(define (save-scrollback entries path)
  (define path-str
    (if (path? path)
        (path->string path)
        path))
  (define trimmed
    (if (> (length entries) scrollback-max-entries)
        (take-right entries scrollback-max-entries)
        entries))
  (define jsexprs (map transcript-entry->jsexpr trimmed))
  ;; Atomic rewrite: write to temp then rename
  (define tmp-path (string-append path-str ".tmp"))
  (call-with-output-file tmp-path
                         (lambda (out)
                           (for ([entry (in-list jsexprs)])
                             (write-json entry out)
                             (newline out)))
                         #:mode 'text
                         #:exists 'replace)
  (rename-file-or-directory tmp-path path-str #t))

;; Load transcript-entries from a JSONL file.
;; Returns '() if the file does not exist.
(define (load-scrollback path)
  (define raw (jsonl-read-last path 500))
  (map jsexpr->transcript-entry raw))

;; Deep hash → nested jsexpr conversion (handles nested hashes)
(define (hash->jsexpr-deep h)
  (for/hash ([(k v) (in-hash h)])
    (values k
            (if (hash? v)
                (hash->jsexpr-deep v)
                v))))

;; Deep jsexpr → nested hash conversion
(define (jsexpr->hash-deep h)
  (cond
    [(hash? h)
     (for/hash ([(k v) (in-hash h)])
       (values k (jsexpr->hash-deep v)))]
    [else h]))
