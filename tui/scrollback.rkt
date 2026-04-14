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
         load-scrollback)

;; Maximum number of scrollback entries to keep on disk.
(define scrollback-max-entries 500)

;; Serialize a transcript-entry to a JSON-compatible hash.
(define (transcript-entry->jsexpr entry)
  (hasheq 'kind
          (symbol->string (transcript-entry-kind entry))
          'text
          (transcript-entry-text entry)
          'timestamp
          (transcript-entry-timestamp entry)
          'meta
          (hash->jsexpr-deep (transcript-entry-meta entry))))

;; Deserialize a jsexpr hash back to a transcript-entry.
(define (jsexpr->transcript-entry h)
  (transcript-entry (string->symbol (hash-ref h 'kind "system"))
                    (hash-ref h 'text "")
                    (hash-ref h 'timestamp 0)
                    (jsexpr->hash-deep (hash-ref h 'meta (hash)))
                    #f))

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
