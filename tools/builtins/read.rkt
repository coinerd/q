#lang racket/base

(require racket/port
         racket/string
         racket/file
         racket/list
         racket/dict
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../util/path-helpers.rkt" contains-null-bytes? bytes->display-lines expand-home-path))

(provide tool-read)

;; Default maximum number of lines to return
(define DEFAULT-MAX-LINES 2000)

;; Maximum total bytes for output content
(define DEFAULT-MAX-BYTES 50000)

;; Format a single numbered line
(define (format-line n text)
  (format "~a| ~a" n text))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-read args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define path-str (and raw-path (expand-home-path raw-path)))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]
    [else
     (define offset (hash-ref args 'offset 1))
     (define limit (hash-ref args 'limit #f))

     ;; 1. File existence check
     ;; (safe-mode path check is done by scheduler, not here)
     (cond
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
              (define max-lines (or limit DEFAULT-MAX-LINES))
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
                   (if (> (string-length joined) DEFAULT-MAX-BYTES)
                       (string-append (substring joined 0 DEFAULT-MAX-BYTES)
                                      "\n[SYS] Output truncated at "
                                      (number->string DEFAULT-MAX-BYTES)
                                      " bytes]")
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
