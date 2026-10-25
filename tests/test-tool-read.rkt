#lang racket
;; @covers tools/builtins/read.rkt

;; @speed fast
;; @suite default
;; @boundary integration

;; BOUNDARY: integration

(require rackunit
         "../tools/builtins/read.rkt"
         "../tools/tool.rkt"
         "../util/safe-mode/safe-mode-state.rkt"
         racket/file)

;; ============================================================
;; tool-read — basic read
;; ============================================================

(test-case "tool-read reads a file"
  (define tmp (make-temporary-file "q-test-read-~a.txt"))
  (display-to-file "line1\nline2\nline3" tmp #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp)))
  (check-false (tool-result-is-error? result))
  ;; Content is a list of strings; verify something was returned
  (check-not-false (tool-result-content result))
  (delete-file tmp))

(test-case "tool-read returns error for missing file"
  (define result (tool-read (hasheq 'path "/nonexistent/file.txt")))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "not found")))

(test-case "tool-read respects offset"
  (define tmp (make-temporary-file "q-test-read-~a.txt"))
  (display-to-file "line1\nline2\nline3\nline4\nline5" tmp #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp 'offset 3)))
  (check-false (tool-result-is-error? result))
  (check-true (string-contains? (car (tool-result-content result)) "line3"))
  (delete-file tmp))

(test-case "tool-read respects limit"
  (define tmp (make-temporary-file "q-test-read-~a.txt"))
  (display-to-file "line1\nline2\nline3\nline4\nline5" tmp #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp 'limit 2)))
  (check-false (tool-result-is-error? result))
  ;; Only lines 1-2 should be present
  (check-false (string-contains? (car (tool-result-content result)) "line3"))
  (delete-file tmp))

(test-case "tool-read returns error for binary file"
  (define tmp (make-temporary-file "q-test-read-~a.bin"))
  (call-with-output-file tmp (λ (out) (write-bytes (bytes 0 1 2 0 4 5) out)) #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "binary"))
  (delete-file tmp))

(test-case "tool-read handles empty file"
  (define tmp (make-temporary-file "q-test-read-~a.txt"))
  (display-to-file "" tmp #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp)))
  (check-false (tool-result-is-error? result))
  (delete-file tmp))

(test-case "tool-read details include total-lines"
  (define tmp (make-temporary-file "q-test-read-~a.txt"))
  (display-to-file "a\nb\nc" tmp #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp)))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'total-lines) 3)
  (delete-file tmp))

;; ============================================================
;; TEST-02: Additional coverage — path traversal, symlink, large file
;; ============================================================

(test-case "tool-read: path traversal returns error in safe mode"
  (define project-root-dir (make-temporary-file "q-read-root-~a" 'directory))
  (define outside-file (make-temporary-file "q-read-outside-~a.txt"))
  (display-to-file "outside" outside-file #:exists 'replace)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t
                                                                                  #:project-root
                                                                                  project-root-dir)])
                    (define result (tool-read (hasheq 'path outside-file)))
                    (check-pred tool-result-is-error? result)))
                (lambda ()
                  (delete-file outside-file)
                  (delete-directory/files project-root-dir))))

(test-case "tool-read: symlink resolves correctly"
  (define tmp (make-temporary-file "q-test-symlink-~a.txt"))
  (display-to-file "symlink-target-content" tmp #:exists 'replace)
  (define link-path (make-temporary-file "q-test-link-~a.txt"))
  (delete-file link-path)
  (make-file-or-directory-link tmp link-path)
  (define result (tool-read (hasheq 'path link-path)))
  (check-false (tool-result-is-error? result))
  (check-true (string-contains? (car (tool-result-content result)) "symlink-target-content"))
  (delete-file link-path)
  (delete-file tmp))

(test-case "tool-read: large file (1000+ lines) works"
  (define tmp (make-temporary-file "q-test-large-~a.txt"))
  (define lines
    (for/list ([i (in-range 1200)])
      (format "line ~a" i)))
  (display-to-file (string-join lines "\n") tmp #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp)))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'total-lines) 1200)
  (delete-file tmp))

(test-case "tool-read: missing path arg returns error"
  (define result (tool-read (hasheq)))
  (check-pred tool-result-is-error? result))

;; ============================================================
;; R96 (BUG-0096): honest bounded read
;; ============================================================

(define (utf8-len s) (bytes-length (string->bytes/utf-8 s)))

(test-case "tool-read: 2001-line file truncates with honest metadata"
  (define tmp (make-temporary-file "q-read-r96-~a.txt"))
  (call-with-output-file tmp
    #:exists 'replace
    (lambda (out) (for ([i (in-range 1 2002)]) (fprintf out "line ~a\n" i))))
  (define result (tool-read (hasheq 'path tmp)))
  (check-false (tool-result-is-error? result))
  (define d (tool-result-details result))
  (check-equal? (hash-ref d 'total-lines) 2001)
  (check-equal? (hash-ref d 'end-line) 2000)
  (check-equal? (hash-ref d 'truncated?) #t)
  (check-equal? (hash-ref d 'reasons) '(line))
  (check-equal? (hash-ref d 'next-offset) 2001)
  (define notice (hash-ref d 'notice #f))
  (check-true (string? notice))
  (check-true (string-contains? notice "offset=2001"))
  ;; notice visible in content AND details
  (check-true (string-contains? (car (tool-result-content result)) notice))
  ;; byte-count oracle equality (single text-content string, UTF-8)
  (check-equal? (length (tool-result-content result)) 1)
  (check-equal? (hash-ref d 'returned-byte)
                (utf8-len (car (tool-result-content result))))
  (delete-file tmp))

(test-case "tool-read: offset past EOF is an empty success"
  (define tmp (make-temporary-file "q-read-r96-~a.txt"))
  (display-to-file "a\nb\n" tmp #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp 'offset 3)))
  (check-false (tool-result-is-error? result))
  (define d (tool-result-details result))
  (check-equal? (tool-result-content result) '())
  (check-equal? (hash-ref d 'start-line) 0)
  (check-equal? (hash-ref d 'end-line) 0)
  (check-equal? (hash-ref d 'truncated?) #f)
  (check-equal? (hash-ref d 'returned-byte) 0)
  (check-false (hash-has-key? d 'next-offset))
  (delete-file tmp))

(test-case "tool-read: CRLF file has no phantom line and no stray CR"
  (define tmp (make-temporary-file "q-read-r96-~a.txt"))
  (call-with-output-file tmp #:exists 'replace
    (lambda (out) (write-bytes #"a\r\nb\r\n" out)))
  (define result (tool-read (hasheq 'path tmp)))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'total-lines) 2)
  (check-false (string-contains? (car (tool-result-content result)) "\r"))
  (delete-file tmp))

(test-case "tool-read: lone CR is preserved"
  (define tmp (make-temporary-file "q-read-r96-~a.txt"))
  (call-with-output-file tmp #:exists 'replace
    (lambda (out) (write-bytes #"a\rb\n" out)))
  (define result (tool-read (hasheq 'path tmp)))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'total-lines) 1)
  (check-true (string-contains? (car (tool-result-content result)) "1| a\rb"))
  (delete-file tmp))

(test-case "tool-read: oversized single line errors with line number and guidance"
  (define tmp (make-temporary-file "q-read-r96-~a.txt"))
  (call-with-output-file tmp #:exists 'replace
    (lambda (out) (write-string (make-string 60000 #\b) out)))
  (define result (tool-read (hasheq 'path tmp)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "line 1"))
  (delete-file tmp))

(test-case "tool-read: safe mode denies symlink escaping the project root"
  (define project-root-dir (make-temporary-file "q-read-root-~a" 'directory))
  (define outside-file (make-temporary-file "q-read-outside-~a.txt"))
  (display-to-file "outside" outside-file #:exists 'replace)
  (define link-path (build-path project-root-dir "escape-link.txt"))
  (make-file-or-directory-link outside-file link-path)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t
                                                                                  #:project-root
                                                                                  project-root-dir)])
                    ;; escaping symlink denied
                    (define result (tool-read (hasheq 'path (path->string link-path))))
                    (check-pred tool-result-is-error? result)
                    ;; inside-root file allowed
                    (define inside (build-path project-root-dir "inside.txt"))
                    (display-to-file "inside" inside #:exists 'replace)
                    (define ok (tool-read (hasheq 'path (path->string inside))))
                    (check-false (tool-result-is-error? ok))
                    (delete-file inside)))
                (lambda ()
                  (delete-file link-path)
                  (delete-file outside-file)
                  (delete-directory/files project-root-dir))))

(test-case "tool-read: invalid offset/limit rejected"
  (check-pred tool-result-is-error? (tool-read (hasheq 'path "/tmp/x" 'offset 0)))
  (check-pred tool-result-is-error? (tool-read (hasheq 'path "/tmp/x" 'offset -3)))
  (check-pred tool-result-is-error? (tool-read (hasheq 'path "/tmp/x" 'limit 0)))
  (check-pred tool-result-is-error? (tool-read (hasheq 'path "/tmp/x" 'limit -1))))
