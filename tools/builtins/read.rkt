#lang racket/base

;; tools/builtins/read.rkt — File read tool
;;
;; Layer: tools (interface layer consumer)
;; Purpose: Read file contents with encoding detection, line range support,
;; and safe-mode path validation. Handles text files, images, and binary
;; content with appropriate encoding. Primary tool for source exploration.
;;
;; v0.33.2 W0: Converted to define-tool macro.

(require racket/port
         racket/string
         racket/file
         racket/list
         racket/dict
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../define-tool.rkt" define-tool)
         (only-in "../../util/path/path-helpers.rkt"
                  contains-null-bytes?
                  bytes->display-lines
                  expand-home-path)
         (only-in "../../util/truncation.rkt" truncate-output MAX-OUTPUT-BYTES MAX-OUTPUT-LINES)
         (only-in "builtin-helpers.rkt" require-safe-path!))

;; Format a single numbered line
(define (format-line n text)
  (format "~a| ~a" n text))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (read-handler args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define path-str (and raw-path (expand-home-path raw-path)))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]
    [else
     (define offset (hash-ref args 'offset 1))
     (define limit (hash-ref args 'limit #f))

     ;; 1. Safe-mode defense-in-depth path check (SEC-01)
     (define safe-err (require-safe-path! path-str "read"))
     (cond
       [safe-err (make-error-result safe-err)]
       [(not (file-exists? path-str)) (make-error-result (format "File not found: ~a" path-str))]

       [else
        ;; 2. Read bytes and check for binary
        (define raw-bytes (file->bytes path-str))

        (cond
          [(contains-null-bytes? raw-bytes)
           (make-error-result (format "File appears to be binary: ~a" path-str))]

          [else
           ;; 3. Convert to text, split into lines
           (define-values (display-lines total-lines) (bytes->display-lines raw-bytes))

           ;; 4. Empty file
           (cond
             [(zero? total-lines)
              (make-success-result '()
                                   (hasheq 'total-lines 0 'start-line 0 'end-line 0 'path path-str))]

             [else
              ;; 5. Apply offset/limit
              (define max-lines (or limit MAX-OUTPUT-LINES))
              (define start-idx (max 0 (sub1 offset)))
              (define end-idx (min total-lines (+ start-idx max-lines)))

              (define sliced
                (for/list ([i (in-range start-idx end-idx)])
                  (cons (add1 i) (list-ref display-lines i))))

              (cond
                [(null? sliced)
                 (make-success-result
                  '()
                  (hasheq 'total-lines total-lines 'start-line 0 'end-line 0 'path path-str))]

                [else
                 (define formatted
                   (for/list ([(n line) (in-dict sliced)])
                     (format-line n line)))

                 (define joined (string-join formatted "\n"))
                 (define result-text
                   (if (> (string-length joined) MAX-OUTPUT-BYTES)
                       (string-append (substring joined 0 MAX-OUTPUT-BYTES)
                                      (format "\n[Output truncated. ~a bytes omitted]"
                                              (- (string-length joined) MAX-OUTPUT-BYTES)))
                       joined))

                 (make-success-result (list (string-append result-text "\n"))
                                      (hasheq 'total-lines
                                              total-lines
                                              'start-line
                                              (car (first sliced))
                                              'end-line
                                              (car (last sliced))
                                              'path
                                              path-str))])])])])]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool read
  #:description "Read file contents with line numbers. Supports offset/limit for large files. Detects binary content."
  #:required ("path")
  #:properties
    [(path "string" "Path to the file to read")
     (offset "integer" "Starting line number (1-indexed, default 1)")
     (limit "integer" "Maximum number of lines to read (default all)")]
  read-handler)

(provide read)
