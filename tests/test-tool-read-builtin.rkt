#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; tests/test-tool-read-builtin.rkt — T-2 tests for tools/builtins/read.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../tools/tool-struct.rkt" tool-execute)
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?)
         (only-in "../tools/builtins/read.rkt" read))

;; R96 (BUG-0096) oracle helpers --------------------------------------------
;; Byte oracle: returned-byte is the UTF-8 byte length of the concatenated
;; returned text-content strings in order (a generated nonempty page joins
;; numbered lines/notice with LF and appends ONE final LF; JSON envelope and
;; details are excluded).
(define (utf8-len s) (bytes-length (string->bytes/utf-8 s)))
(define (page-text result) (apply string-append (tool-result-content result)))

;; File with n lines "line 1".."line n", each terminated by suffix (default LF).
(define (make-lines-file n [suffix "\n"])
  (define p (make-temporary-file "q-read-r96-~a"))
  (call-with-output-file p
    #:exists 'truncate
    (lambda (out) (for ([i (in-range 1 (add1 n))]) (fprintf out "line ~a~a" i suffix))))
  p)

;; Collect the absolute 1-based line numbers shown on a page (drops any
;; trailing notice line so pagination-reconstruction checks are honest).
(define (page-line-numbers result)
  (for/list ([ln (in-list (string-split (page-text result) "\n"))]
             #:when (regexp-match? #px"^\\d+\\| " ln))
    (string->number (car (regexp-match #px"^\\d+" ln)))))

;; Helper: invoke read tool with args
(define (call-read args)
  ((tool-execute read) args #f))

;; Helper: create temp file with content, return path
(define (make-temp-file content)
  (define path (make-temporary-file "q-read-test-~a"))
  (call-with-output-file path #:exists 'truncate (lambda (p) (display content p)))
  path)

(define (cleanup-path p)
  (when (file-exists? p)
    (delete-file p)))

;; Helper: extract error text from error result content
(define (error-text result)
  (define c (tool-result-content result))
  (if (and (pair? c) (hash? (car c)))
      (hash-ref (car c) 'text "")
      (format "~a" c)))

(define read-suite
  (test-suite "Read builtin tests"

    (test-case "read entire file returns correct content"
      (define p (make-temp-file "hello world\nline two\n"))
      (dynamic-wind (lambda () (void))
                    (lambda ()
                      (define result (call-read (hasheq 'path p)))
                      (check-pred tool-result? result)
                      (check-false (tool-result-is-error? result))
                      (check-equal? (hash-ref (tool-result-details result) 'total-lines) 2))
                    (lambda () (cleanup-path p))))

    (test-case "tool-read-builtin: read non-existent file returns error"
      (define result (call-read (hasheq 'path "/tmp/nonexistent-q-test-file-xyz")))
      (check-pred tool-result? result)
      (check-true (tool-result-is-error? result))
      (check-true (string-contains? (error-text result) "not found")))

    (test-case "read with offset starts at correct line"
      (define p (make-temp-file "line1\nline2\nline3\nline4\n"))
      (dynamic-wind (lambda () (void))
                    (lambda ()
                      (define result (call-read (hasheq 'path p 'offset 3)))
                      (check-pred tool-result? result)
                      (check-false (tool-result-is-error? result))
                      ;; Content is list of strings, first should contain "3| line3"
                      (define content (tool-result-content result))
                      (check-true (and (pair? content) (string-contains? (car content) "line3"))))
                    (lambda () (cleanup-path p))))

    (test-case "read with limit caps output"
      (define p (make-temp-file "line1\nline2\nline3\nline4\nline5\n"))
      (dynamic-wind (lambda () (void))
                    (lambda ()
                      (define result (call-read (hasheq 'path p 'limit 2)))
                      (check-pred tool-result? result)
                      (check-false (tool-result-is-error? result))
                      (define details (tool-result-details result))
                      (check-equal? (hash-ref details 'end-line) 2))
                    (lambda () (cleanup-path p))))

    (test-case "tool-read-builtin: read binary file returns error"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda ()
                      (call-with-output-file p
                                             #:exists 'truncate
                                             (lambda (port) (write-bytes #"\x00\x01\x02\x03" port))))
                    (lambda ()
                      (define result (call-read (hasheq 'path p)))
                      (check-pred tool-result? result)
                      (check-true (tool-result-is-error? result))
                      (check-true (string-contains? (error-text result) "binary")))
                    (lambda () (cleanup-path p))))

    (test-case "read empty file returns empty success"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda () (void))
                    (lambda ()
                      (define result (call-read (hasheq 'path p)))
                      (check-pred tool-result? result)
                      (check-false (tool-result-is-error? result))
                      (define details (tool-result-details result))
                      (check-equal? (hash-ref details 'total-lines) 0))
                    (lambda () (cleanup-path p))))

    ;; ========================================================
    ;; R96 (BUG-0096): honest bounded read — frozen oracle
    ;; ========================================================

    (test-case "R96: 2000-line file (exact line cap) is not truncated"
      (define p (make-lines-file 2000))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (define d (tool-result-details r))
                      (check-equal? (hash-ref d 'total-lines) 2000)
                      (check-equal? (hash-ref d 'start-line) 1)
                      (check-equal? (hash-ref d 'end-line) 2000)
                      (check-equal? (hash-ref d 'truncated?) #f)
                      (check-equal? (hash-ref d 'reasons) '())
                      (check-false (hash-has-key? d 'next-offset))
                      (check-false (hash-has-key? d 'notice))
                      (check-true (string-contains? (page-text r) "2000| line 2000"))
                      ;; byte-count oracle equality
                      (check-equal? (length (tool-result-content r)) 1)
                      (check-equal? (hash-ref d 'returned-byte) (utf8-len (page-text r))))
                    (lambda () (cleanup-path p))))

    (test-case "R96: 2001-line file truncates at 2000 with line reason, notice, next-offset"
      (define p (make-lines-file 2001))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (define d (tool-result-details r))
                      (check-equal? (hash-ref d 'truncated?) #t)
                      (check-equal? (hash-ref d 'reasons) '(line))
                      (check-equal? (hash-ref d 'end-line) 2000)
                      (check-equal? (hash-ref d 'next-offset) 2001)
                      (define notice (hash-ref d 'notice #f))
                      (check-true (string? notice) "notice present in details")
                      (check-true (string-contains? notice "offset=2001")
                                  "notice names the continuation offset")
                      ;; notice is visible in content too
                      (check-true (string-contains? (page-text r) notice))
                      (check-equal? (length (page-line-numbers r)) 2000)
                      (check-equal? (hash-ref d 'returned-byte) (utf8-len (page-text r))))
                    (lambda () (cleanup-path p))))

    (test-case "R96: explicit smaller limit (5) honors the request"
      (define p (make-lines-file 50))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p 'limit 5)))
                      (check-false (tool-result-is-error? r))
                      (define d (tool-result-details r))
                      (check-equal? (hash-ref d 'end-line) 5)
                      (check-equal? (hash-ref d 'truncated?) #t)
                      (check-equal? (hash-ref d 'reasons) '(line))
                      (check-equal? (hash-ref d 'next-offset) 6))
                    (lambda () (cleanup-path p))))

    (test-case "R96: explicit larger limit (99999) is capped at 2000"
      (define p (make-lines-file 2500))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p 'limit 99999)))
                      (check-false (tool-result-is-error? r))
                      (define d (tool-result-details r))
                      (check-equal? (hash-ref d 'end-line) 2000)
                      (check-equal? (hash-ref d 'truncated?) #t)
                      (check-equal? (hash-ref d 'reasons) '(line))
                      (check-equal? (hash-ref d 'next-offset) 2001))
                    (lambda () (cleanup-path p))))

    (test-case "R96: offset past EOF is an empty success without fake continuation"
      (define p (make-lines-file 10))
      (dynamic-wind void
                    (lambda ()
                      (for ([off (in-list '(11 999999))])
                        (define r (call-read (hasheq 'path p 'offset off)))
                        (check-false (tool-result-is-error? r)
                                     (format "offset ~a past EOF succeeds" off))
                        (define d (tool-result-details r))
                        (check-equal? (tool-result-content r) '())
                        (check-equal? (hash-ref d 'start-line) 0)
                        (check-equal? (hash-ref d 'end-line) 0)
                        (check-equal? (hash-ref d 'total-lines) 10)
                        (check-equal? (hash-ref d 'truncated?) #f)
                        (check-equal? (hash-ref d 'returned-byte) 0)
                        (check-false (hash-has-key? d 'next-offset))))
                    (lambda () (cleanup-path p))))

    (test-case "R96: empty file is not truncated, zero returned bytes"
      (define p (make-temp-file ""))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (define d (tool-result-details r))
                      (check-equal? (hash-ref d 'total-lines) 0)
                      (check-equal? (hash-ref d 'start-line) 0)
                      (check-equal? (hash-ref d 'end-line) 0)
                      (check-equal? (hash-ref d 'truncated?) #f)
                      (check-equal? (hash-ref d 'returned-byte) 0)
                      (check-equal? (tool-result-content r) '()))
                    (lambda () (cleanup-path p))))

    (test-case "R96: terminal LF creates no phantom line"
      (define p (make-temp-file "a\nb\n"))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (check-equal? (hash-ref (tool-result-details r) 'total-lines) 2)
                      (check-true (string-contains? (page-text r) "2| b"))
                      (check-false (string-contains? (page-text r) "3| ")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: consecutive LFs preserve intervening empty lines"
      (define p (make-temp-file "a\n\nb\n"))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (check-equal? (hash-ref (tool-result-details r) 'total-lines) 3)
                      (check-true (string-contains? (page-text r) "2| \n")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: CRLF normalizes to LF (CR stripped only before LF)"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda ()
                      (call-with-output-file p #:exists 'truncate
                        (lambda (out) (write-bytes #"a\r\nb\r\n" out))))
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (check-equal? (hash-ref (tool-result-details r) 'total-lines) 2)
                      (check-false (string-contains? (page-text r) "\r")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: lone CR is unchanged"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda ()
                      (call-with-output-file p #:exists 'truncate
                        (lambda (out) (write-bytes #"a\rb\n" out))))
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (check-equal? (hash-ref (tool-result-details r) 'total-lines) 1)
                      (check-true (string-contains? (page-text r) "1| a\rb")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: invalid UTF-8 at boundary is replaced with ?"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda ()
                      (call-with-output-file p #:exists 'truncate
                        (lambda (out) (write-bytes #"caf\xE9-line\nnext\n" out))))
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      (check-equal? (hash-ref (tool-result-details r) 'total-lines) 2)
                      (check-true (string-contains? (page-text r) "1| caf?-line"))
                      (check-true (string-contains? (page-text r) "2| next")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: emoji and combining chars count as their real UTF-8 bytes"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda ()
                      (call-with-output-file p #:exists 'truncate
                        (lambda (out)
                          (write-string "x\U0001F600y\n" out)
                          (write-string "e\u0301\n" out))))
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r))
                      ;; "1| x\U0001F600y" = 3 + (1+4+1) + LF = 10;
                      ;; "2| e\u0301" = 3 + (1+2) + LF = 7
                      (check-equal? (hash-ref (tool-result-details r) 'returned-byte)
                                    (+ 10 7))
                      (check-equal? (hash-ref (tool-result-details r) 'returned-byte)
                                    (utf8-len (page-text r)))
                      (check-true (string-contains? (page-text r) "x\U0001F600y")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: many numbered lines carry absolute numbers"
      (define p (make-lines-file 12))
      (dynamic-wind void
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-true (string-contains? (page-text r) "10| line 10"))
                      (check-true (string-contains? (page-text r) "12| line 12")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: exact 50000-byte boundary including notice returns exactly the cap"
      ;; Probe: 3 lines where line 2 is oversized -> byte-truncation notice for
      ;; (end=1, next=2, total=3). The target file stops at line 1 of a 3-line
      ;; file, so the emitted notice has identical digit widths.
      (define probe (make-temp-file ""))
      (define target (make-temp-file ""))
      (dynamic-wind void
                    (lambda ()
                      (call-with-output-file probe #:exists 'truncate
                        (lambda (out)
                          (write-string "s1\n" out)
                          (write-string (make-string 60000 #\b) out)
                          (write-string "\ns3\n" out)))
                      (define probe-r (call-read (hasheq 'path probe)))
                      (check-false (tool-result-is-error? probe-r))
                      (define notice (hash-ref (tool-result-details probe-r) 'notice #f))
                      (check-true (string? notice) "probe produced a truncation notice")
                      (when (string? notice)
                        (define notice-bytes (utf8-len notice))
                        ;; "1| " (3) + LF (1) + notice + its LF (notice-bytes+1) = 50000
                        (define a-len (- 50000 3 1 notice-bytes 1))
                        (check-true (> a-len 0))
                        (call-with-output-file target #:exists 'truncate
                          (lambda (out)
                            (write-string (make-string a-len #\a) out)
                            (write-string "\ns2\ns3\n" out)))
                        (define r (call-read (hasheq 'path target)))
                        (check-false (tool-result-is-error? r))
                        (define d (tool-result-details r))
                        (check-equal? (hash-ref d 'truncated?) #t)
                        (check-equal? (hash-ref d 'reasons) '(byte))
                        (check-equal? (hash-ref d 'start-line) 1)
                        (check-equal? (hash-ref d 'end-line) 1)
                        (check-equal? (hash-ref d 'next-offset) 2)
                        (check-equal? (hash-ref d 'returned-byte) 50000)
                        (check-equal? (utf8-len (page-text r)) 50000)))
                    (lambda ()
                      (cleanup-path probe)
                      (cleanup-path target))))

    (test-case "R96: first requested line larger than cap errors with line number + guidance"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda ()
                      (call-with-output-file p #:exists 'truncate
                        (lambda (out) (write-string (make-string 60000 #\b) out))))
                    (lambda ()
                      (define r (call-read (hasheq 'path p)))
                      (check-true (tool-result-is-error? r))
                      (check-true (string-contains? (error-text r) "line 1"))
                      (check-true (or (string-contains? (error-text r) "grep")
                                      (string-contains? (error-text r) "bash"))
                                  "error includes guidance"))
                    (lambda () (cleanup-path p))))

    (test-case "R96: later oversized line -> progress, then structured error at that offset"
      (define p (make-temp-file ""))
      (dynamic-wind (lambda ()
                      (call-with-output-file p #:exists 'truncate
                        (lambda (out)
                          (write-string "s1\n" out)
                          (write-string (make-string 60000 #\b) out)
                          (write-string "\ns3\n" out))))
                    (lambda ()
                      ;; Page 1 makes progress and points at the oversized line.
                      (define r1 (call-read (hasheq 'path p)))
                      (check-false (tool-result-is-error? r1))
                      (define d1 (tool-result-details r1))
                      (check-equal? (hash-ref d1 'truncated?) #t)
                      (check-equal? (hash-ref d1 'end-line) 1)
                      (check-equal? (hash-ref d1 'next-offset) 2)
                      ;; Following the notice yields a structured error, not a
                      ;; non-progress continuation loop.
                      (define r2 (call-read (hasheq 'path p 'offset 2)))
                      (check-true (tool-result-is-error? r2))
                      (check-true (string-contains? (error-text r2) "line 2")))
                    (lambda () (cleanup-path p))))

    (test-case "R96: paginated pages reconstruct all lines without gaps or overlap"
      (define p (make-lines-file 5000))
      (dynamic-wind void
                    (lambda ()
                      (let loop ([offset 1] [seen '()])
                        (define r (call-read (hasheq 'path p 'offset offset)))
                        (check-false (tool-result-is-error? r))
                        (define nums (append seen (page-line-numbers r)))
                        (define next (hash-ref (tool-result-details r) 'next-offset #f))
                        (if next
                            (loop next nums)
                            (begin
                              (check-equal? nums (build-list 5000 add1)
                                            "pages cover 1..5000 exactly, in order")))))
                    (lambda () (cleanup-path p))))

    (test-case "R96: non-positive offset/limit are rejected"
      (for ([args (in-list (list (hasheq 'path "x" 'offset 0)
                                 (hasheq 'path "x" 'offset -1)
                                 (hasheq 'path "x" 'offset 1 'limit 0)
                                 (hasheq 'path "x" 'offset 1 'limit -5)))])
        (define r (call-read args))
        (check-true (tool-result-is-error? r)
                    (format "invalid args rejected: ~v" args))))

    (test-case "read with no path returns error"
      (define result (call-read (hasheq)))
      (check-pred tool-result? result)
      (check-true (tool-result-is-error? result))
      (check-true (string-contains? (error-text result) "Missing")))))

(run-tests read-suite)
