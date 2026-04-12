#lang racket/base

(require racket/port
         racket/string
         racket/file
         racket/list
         racket/dict
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../runtime/safe-mode.rkt" allowed-path?))

(provide tool-read)

;; Default maximum number of lines to return
(define DEFAULT-MAX-LINES 2000)

;; Maximum total bytes for output content
(define DEFAULT-MAX-BYTES 50000)

;; Check if a byte string contains null bytes (binary indicator)
(define (contains-null-bytes? bs)
  (for/or ([b (in-bytes bs)])
    (= b 0)))

;; Format a single numbered line
(define (format-line n text)
  (format "~a| ~a" n text))

;; --------------------------------------------------
;; Result helpers (local) - return tool-result structs
;; --------------------------------------------------

(define (ok content details)
  (make-success-result content details))

(define (err msg)
  (make-error-result msg))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-read args [exec-ctx #f])
  (define path-str (hash-ref args 'path #f))
  (cond
    [(not path-str) (err "Missing required argument: path")]
    [else
     (define offset (hash-ref args 'offset 1))
     (define limit (hash-ref args 'limit #f))

     ;; 0. Path validation (safe-mode)
     (cond
       [(not (allowed-path? path-str))
        (err (format "Access denied: path outside project root: ~a" path-str))]

       ;; 1. File existence check
       [(not (file-exists? path-str)) (err (format "File not found: ~a" path-str))]

       [else
        ;; 2. Read bytes and check for binary
        (define raw-bytes (file->bytes path-str))

        (cond
          [(contains-null-bytes? raw-bytes) (err (format "File appears to be binary: ~a" path-str))]

          [else
           ;; 3. Convert to text, split into lines
           (define text (bytes->string/utf-8 raw-bytes #\?))
           (define all-lines (string-split text "\n" #:trim? #f))

           ;; Trim trailing empty element from trailing newline
           (define has-trailing-newline
             (and (> (string-length text) 0)
                  (char=? (string-ref text (sub1 (string-length text))) #\newline)))
           (define display-lines
             (if has-trailing-newline
                 (drop-right all-lines 1)
                 all-lines))
           (define total-lines (length display-lines))

           ;; 4. Empty file
           (cond
             [(zero? total-lines)
              (ok '() (hasheq 'total-lines 0 'start-line 0 'end-line 0 'path path-str))]

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
                 (ok '() (hasheq 'total-lines total-lines 'start-line 0 'end-line 0 'path path-str))]

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

                 (ok (list (string-append result-text "\n"))
                     (hasheq 'total-lines
                             total-lines
                             'start-line
                             (car (first sliced))
                             'end-line
                             (car (last sliced))
                             'path
                             path-str))])])])])]))
