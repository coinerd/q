#lang racket

(require rackunit
         "../tools/builtins/read.rkt"
         "../tools/tool.rkt"
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
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text)
                                 "not found")))

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
  (call-with-output-file tmp
    (λ (out) (write-bytes (bytes 0 1 2 0 4 5) out))
    #:exists 'replace)
  (define result (tool-read (hasheq 'path tmp)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text)
                                 "binary"))
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
