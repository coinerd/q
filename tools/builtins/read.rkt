#lang racket/base

;; tools/builtins/read.rkt — File read tool
;;
;; Layer: tools (interface layer consumer)
;; Purpose: Read file contents with safe-mode path validation, returning a
;; BOUNDED page of numbered display lines. Handles text files, images, and
;; binary content with appropriate encoding. Primary tool for source
;; exploration.
;;
;; v0.33.2 W0: Converted to define-tool macro.
;;
;; R96 (BUG-0096) — honest bounded read, frozen byte/line oracle:
;; - Pagination: positive 1-based offset (decoded display lines), positive
;;   limit; omitted limit = 2000; effective limit = min(requested, 2000).
;; - Byte cap: a page is additionally capped at 50000 UTF-8 bytes.
;;   `returned-byte` in details is exactly this oracle: the UTF-8 encoding of
;;   the concatenated returned text-content strings in order, with no implicit
;;   separator. A generated nonempty page joins the numbered lines (and the
;;   omission/continuation notice, when present) with LF and appends ONE final
;;   LF. Separators, the final LF and the notice bytes are counted; the JSON
;;   envelope and details are excluded.
;; - Complete lines only — a page never cuts mid-line. If the FIRST requested
;;   line cannot fit (including the notice reserve a following truncation
;;   would require), the tool returns a structured oversized-line error naming
;;   the line number with guidance. A LATER oversized line returns the
;;   preceding progress (success with notice + next-offset); the next call at
;;   that offset then errors — never a non-progress continuation loop.
;; - Display rules — documented INTENTIONAL compatibility changes vs
;;   util/path-helpers.rkt bytes->display-lines, implemented locally HERE
;;   (the shared helper is unchanged):
;;     * empty bytes -> zero lines / empty content / returned-byte 0
;;       (not truncated)
;;     * a terminal LF creates no phantom extra line
;;     * consecutive LFs preserve intervening empty lines
;;     * CRLF normalizes to LF (CR stripped only before LF); a lone CR is
;;       unchanged
;;     * invalid UTF-8 keeps the '?' replacement character
;; - offset past EOF is an EMPTY SUCCESS (no fake continuation). Actual
;;   start-line/end-line are 0/0 when no lines are returned.
;; - The omission/continuation notice is visible in the content AND in the
;;   details ('notice); next-offset appears only when the page made progress
;;   AND more lines remain.
;; - This is an OUTPUT bound only: the existing file->bytes read still loads
;;   the whole input; no streaming/resource architecture is promised.
;; - Binary/missing/unreadable and safe-mode/symlink path controls are
;;   unchanged.

(require racket/file
         racket/list
         racket/match
         racket/string
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../define-tool.rkt" define-tool)
         (only-in "../../util/path/path-helpers.rkt" contains-null-bytes? expand-home-path)
         (only-in "../../util/truncation.rkt" MAX-OUTPUT-BYTES MAX-OUTPUT-LINES)
         (only-in "builtin-helpers.rkt" require-safe-path!))

;; Format a single numbered line
(define (format-line n text)
  (format "~a| ~a" n text))

;; UTF-8 byte length (oracle byte accounting)
(define (utf8-byte-length s)
  (bytes-length (string->bytes/utf-8 s)))

;; R96 local display-line decoding (see module header for the frozen rules).
;; Deliberately NOT util/path-helpers.rkt bytes->display-lines: read documents
;; its own compatibility rules locally; the shared helper stays untouched.
;; Returns (values display-lines total-lines).
(define (bytes->read-lines raw-bytes)
  (define text (bytes->string/utf-8 raw-bytes #\?))
  ;; CRLF -> LF: strip CR only immediately before LF; lone CR untouched.
  (define normalized (regexp-replace* #rx"\r\n" text "\n"))
  (if (string=? normalized "")
      (values '() 0)
      (let* ([parts (string-split normalized "\n" #:trim? #f)]
             ;; terminal LF creates no phantom line; consecutive LFs keep
             ;; their intervening empty lines
             [lines (if (and (pair? parts) (string=? (last parts) ""))
                        (drop-right parts 1)
                        parts)])
        (values lines (length lines)))))

;; Visible omission/continuation notice (single line, no embedded LF).
(define (truncation-notice start end total next)
  (format "[read truncated: showing lines ~a-~a of ~a; call read again with offset=~a to continue]"
          start
          end
          total
          next))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (read-handler args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define path-str (and raw-path (expand-home-path raw-path)))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]
    [else
     (define offset-arg (hash-ref args 'offset 1))
     (define limit-arg (hash-ref args 'limit #f))

     (cond
       ;; Positive 1-based offset / positive limit (R96)
       [(not (and (exact-integer? offset-arg) (>= offset-arg 1)))
        (make-error-result "read: offset must be a positive integer (1-based line number)")]
       [(not (or (not limit-arg) (and (exact-integer? limit-arg) (>= limit-arg 1))))
        (make-error-result "read: limit must be a positive integer (maximum lines per page)")]
       [else
        (define offset offset-arg)
        ;; Omitted limit = 2000; effective limit = min(requested, 2000)
        (define effective-limit (min (or limit-arg MAX-OUTPUT-LINES) MAX-OUTPUT-LINES))

        ;; 1. Safe-mode defense-in-depth path check (SEC-01)
        (define safe-err (require-safe-path! path-str "read"))
        (cond
          [safe-err (make-error-result safe-err)]
          [(not (file-exists? path-str)) (make-error-result (format "File not found: ~a" path-str))]

          [else
           ;; 2. Read bytes and check for binary (unchanged controls)
           (define raw-bytes (file->bytes path-str))

           (cond
             [(contains-null-bytes? raw-bytes)
              (make-error-result (format "File appears to be binary: ~a" path-str))]

             [else
              ;; 3. Decode into display lines (R96 local display rules)
              (define-values (lines total-lines) (bytes->read-lines raw-bytes))

              (define empty-success
                (make-success-result '()
                                     (hasheq 'path
                                             path-str
                                             'total-lines
                                             total-lines
                                             'start-line
                                             0
                                             'end-line
                                             0
                                             'truncated?
                                             #f
                                             'reasons
                                             '()
                                             'returned-byte
                                             0)))

              ;; 4. Empty file: zero lines / empty content / zero bytes
              (cond
                [(zero? total-lines) empty-success]
                ;; 5. Offset past EOF: empty success, no fake continuation
                [(> offset total-lines) empty-success]

                [else
                 (define start-idx (sub1 offset)) ; 0-based index of first line

                 ;; Build the page. The loop tracks UTF-8 bytes of the joined
                 ;; items (each item contributes its text + one LF: the LF
                 ;; separators plus the single final LF). While more file
                 ;; lines exist beyond a candidate line, the bytes of the
                 ;; notice a stop there would emit are reserved, so the final
                 ;; page text (lines + notice) always fits the cap.
                 (define page
                   (let loop ([idx start-idx]
                              [count 0]
                              [acc-bytes 0]
                              [rev-items '()])
                     (cond
                       ;; line budget exhausted
                       [(>= count effective-limit)
                        (if (< idx total-lines)
                            (list 'truncated (reverse rev-items) acc-bytes 'line idx)
                            (list 'complete (reverse rev-items) acc-bytes))]
                       ;; all remaining lines fit
                       [(>= idx total-lines) (list 'complete (reverse rev-items) acc-bytes)]
                       [else
                        (define line-no (add1 idx))
                        (define item (format-line line-no (list-ref lines idx)))
                        (define item-bytes (+ (utf8-byte-length item) 1))
                        (define notice-reserve
                          (if (< line-no total-lines)
                              (+ (utf8-byte-length (truncation-notice (add1 start-idx)
                                                                      line-no
                                                                      total-lines
                                                                      (add1 line-no)))
                                 1)
                              0))
                        (cond
                          ;; complete lines only: never cut mid-line
                          [(> (+ acc-bytes item-bytes notice-reserve) MAX-OUTPUT-BYTES)
                           (if (zero? count)
                               ;; FIRST requested line cannot fit: structured
                               ;; oversized-line error with number + guidance
                               (list 'oversized line-no (utf8-byte-length (list-ref lines idx)))
                               ;; later oversized line: stop with progress
                               (list 'truncated (reverse rev-items) acc-bytes 'byte idx))]
                          [else
                           (loop (add1 idx)
                                 (add1 count)
                                 (+ acc-bytes item-bytes)
                                 (cons item rev-items))])])))

                 (match page
                   ;; whole requested range returned, nothing omitted
                   [(list 'complete items _acc)
                    (define text (string-append (string-join items "\n") "\n"))
                    (make-success-result (list text)
                                         (hasheq 'path
                                                 path-str
                                                 'total-lines
                                                 total-lines
                                                 'start-line
                                                 (add1 start-idx)
                                                 'end-line
                                                 (+ start-idx (length items))
                                                 'truncated?
                                                 #f
                                                 'reasons
                                                 '()
                                                 'returned-byte
                                                 (utf8-byte-length text)))]

                   ;; page stopped early: visible notice in content AND details
                   [(list 'truncated items _acc reason next-idx)
                    (define notice
                      (truncation-notice (add1 start-idx)
                                         (+ start-idx (length items))
                                         total-lines
                                         (add1 next-idx)))
                    (define text (string-append (string-join (append items (list notice)) "\n") "\n"))
                    (make-success-result (list text)
                                         (hasheq 'path
                                                 path-str
                                                 'total-lines
                                                 total-lines
                                                 'start-line
                                                 (add1 start-idx)
                                                 'end-line
                                                 (+ start-idx (length items))
                                                 'truncated?
                                                 #t
                                                 'reasons
                                                 (list reason)
                                                 'returned-byte
                                                 (utf8-byte-length text)
                                                 'next-offset
                                                 (add1 next-idx)
                                                 'notice
                                                 notice))]

                   ;; first requested line cannot fit: honest structured error
                   [(list 'oversized line-no line-bytes)
                    (make-error-result
                     (format
                      (string-append "read: line ~a is too large to display (~a UTF-8 bytes; "
                                     "page cap ~a bytes including the truncation notice). "
                                     "No content returned. Use grep to search the file or bash "
                                     "with head/cut to inspect a portion of this line.")
                      line-no
                      line-bytes
                      MAX-OUTPUT-BYTES))])])])])])]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 read
 #:description
 "Read one bounded page: caps 2000 lines and 50000 UTF-8 bytes; offset/limit paginate; follow the next-offset notice; oversized single lines error."
 #:required ("path")
 #:properties
 [(path "string" "Path to the file to read")
  (offset "integer" "1-based line number to start from (default 1). Must be a positive integer.")
  (limit
   "integer"
   "Maximum display lines to return per page (default 2000, hard cap 2000). Must be a positive integer.")]
 read-handler)

(provide read)
