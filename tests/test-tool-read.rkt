#lang racket

;; @speed fast
;; @suite default

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
