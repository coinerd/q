#lang racket/base

;; util/json-helpers.rkt — shared JSON/argument helpers
;;
;; Extracted from runtime/iteration.rkt and tools/tool.rkt to eliminate
;; duplication (QUAL-01).  The version with the warning fprintf is canonical.

(require racket/format
         racket/string
         json)

;; JSON argument normalization
(provide ensure-hash-args
         ;; JSON file I/O
         read-json-file
         write-json-file)

;; Parse tool-call arguments from string to hash if needed.
;; Streaming produces arguments as JSON strings; tools expect hashes.
(define (ensure-hash-args args)
  (cond
    [(hash? args) args]
    [(string? args)
     (define cleaned (string-trim args))
     (if (or (string=? cleaned "") (string=? cleaned "{}"))
         (hash)
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (fprintf (current-error-port)
                                     "warning: ensure-hash-args: failed to parse JSON args: ~a~n"
                                     args)
                            (hasheq '_parse_failed #t '_raw_args args))])
           (define parsed (string->jsexpr cleaned))
           (if (hash? parsed)
               parsed
               (hasheq '_parse_failed #t '_raw_args args))))]
    [else (hasheq '_parse_failed #t '_raw_args (format "~a" args))]))

;; Read and parse a JSON file, returning the parsed value.
(define (read-json-file path)
  (call-with-input-file path (lambda (in) (read-json in)) #:mode 'text))

;; Write a value as JSON to a file.
;; #:exists controls the file-exists behavior (default 'truncate).
(define (write-json-file path data #:exists [mode 'truncate])
  (call-with-output-file path (lambda (out) (write-json data out)) #:mode 'text #:exists mode))
