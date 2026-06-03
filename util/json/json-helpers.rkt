#lang racket/base

;; util/json-helpers.rkt — shared JSON/argument helpers
;;
;; Extracted from runtime/iteration.rkt and tools/tool.rkt to eliminate
;; duplication (QUAL-01).  The version with the warning fprintf is canonical.

(require racket/contract
         racket/format
         racket/string
         json
         (only-in "../error/errors.rkt" raise-tool-error))

;; JSON argument normalization
(provide (contract-out [ensure-hash-args (->* (any/c) (#:graceful? boolean?) hash?)]
                       [read-json-file (-> path-string? any/c)]
                       [write-json-file (->* (path-string? any/c) (#:exists symbol?) void?)]
                       [read-json-from-port (-> input-port? any/c)]
                       [write-json-to-port (-> output-port? any/c void?)]))

;; Parse tool-call arguments from string to hash if needed.
;; Streaming produces arguments as JSON strings; tools expect hashes.
;; RA-02: On parse failure, raises tool-error instead of returning poisoned hash.
;; #:graceful? #t returns empty hash on failure for backward-compatible callers.
(define (ensure-hash-args args #:graceful? [graceful? #f])
  (cond
    [(hash? args) args]
    [(string? args)
     (define cleaned (string-trim args))
     (if (or (string=? cleaned "") (string=? cleaned "{}"))
         (hash)
         (with-handlers ([exn:fail? (lambda (e)
                                      (if graceful?
                                          (hash)
                                          (raise-tool-error
                                           (format "Failed to parse tool arguments JSON: ~a" args)
                                           "ensure-hash-args"
                                           (hasheq 'raw args 'error (exn-message e)))))])
           (define parsed (string->jsexpr cleaned))
           (if (hash? parsed)
               parsed
               (if graceful?
                   (hash)
                   (raise-tool-error (format "Tool arguments parsed to non-hash: ~a" args)
                                     "ensure-hash-args"
                                     (hasheq 'raw args 'parsed parsed))))))]
    [else
     (if graceful?
         (hash)
         (raise-tool-error (format "Tool arguments must be hash or JSON string, got: ~a" args)
                           "ensure-hash-args"
                           (hasheq 'raw (format "~a" args))))]))

;; Read and parse a JSON file, returning the parsed value.
(define (read-json-file path)
  (call-with-input-file path read-json-from-port #:mode 'text))

;; Write a value as JSON to a file.
;; #:exists controls the file-exists behavior (default 'truncate).
(define (write-json-file path data #:exists [mode 'truncate])
  (call-with-output-file path
                         (lambda (out) (write-json-to-port out data))
                         #:mode 'text
                         #:exists mode))

;; R-01/R-02: Port-based I/O — callers manage port lifecycle.
(define (read-json-from-port in)
  (read-json in))

(define (write-json-to-port out data)
  (write-json data out))
