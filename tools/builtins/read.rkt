#lang racket/base

(require racket/port
         racket/string
         racket/file
         racket/list
         racket/dict
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../util/path-helpers.rkt"
                  contains-null-bytes?
                  bytes->display-lines
                  expand-home-path)
         (only-in "../../util/safe-mode-predicates.rkt"
                  safe-mode?
                  allowed-path?
                  safe-mode-project-root)
         (only-in "../../util/truncation.rkt" truncate-output MAX-OUTPUT-BYTES MAX-OUTPUT-LINES))

(provide tool-read)

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

     ;; 1. Safe-mode defense-in-depth path check (SEC-01)
     (cond
       [(and (safe-mode?) (not (allowed-path? path-str)))
        (make-error-result (format "read: path '~a' outside project root (~a)"
                                   path-str
                                   (safe-mode-project-root)))]
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
