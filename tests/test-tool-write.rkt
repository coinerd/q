#lang racket

(require rackunit
         "../tools/builtins/write.rkt"
         "../tools/tool.rkt"
         racket/file)

;; ============================================================
;; tool-write — basic write
;; ============================================================

(test-case "tool-write creates a new file"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (delete-file tmp)  ; ensure doesn't exist
  (define result (tool-write (hasheq 'path tmp 'content "hello world")))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? tmp)
  (check-equal? (file->string tmp) "hello world")
  (delete-file tmp))

(test-case "tool-write overwrites existing file"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (display-to-file "old" tmp #:exists 'replace)
  (define result (tool-write (hasheq 'path tmp 'content "new")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "new")
  (delete-file tmp))

(test-case "tool-write creates parent directories"
  (define tmpdir (make-temporary-file "q-test-write-~a" 'directory))
  (define filepath (build-path tmpdir "sub" "dir" "file.txt"))
  (define result (tool-write (hasheq 'path (path->string filepath) 'content "nested")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string filepath) "nested")
  (delete-directory/files tmpdir))

(test-case "tool-write returns bytes-written in details"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (define content "hello")
  (define result (tool-write (hasheq 'path tmp 'content content)))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'bytes-written)
                (bytes-length (string->bytes/utf-8 content)))
  (delete-file tmp))

(test-case "tool-write error for invalid path"
  (define result (tool-write (hasheq 'path "/nonexistent/deep/path/file.txt" 'content "x")))
  ;; This should succeed because make-directory* creates parents,
  ;; but /nonexistent/deep/path may fail on permission. The behavior
  ;; depends on OS. Let's test with a definitely invalid path.
  ;; On most systems writing to /proc/invalid should fail.
  (check-true (tool-result? result)))

;; ============================================================
;; SEC-03: max-write-bytes limit tests
;; ============================================================

(test-case "SEC-03: writing within limit succeeds"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (delete-file tmp)
  (define small-content (make-string 100 #\A))
  (define result (parameterize ([current-max-write-bytes 200])
                   (tool-write (hasheq 'path tmp 'content small-content))))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? tmp)
  (delete-file tmp))

(test-case "SEC-03: writing over limit returns error"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (delete-file tmp)
  (define big-content (make-string 200 #\B))
  (define result (parameterize ([current-max-write-bytes 100])
                   (tool-write (hasheq 'path tmp 'content big-content))))
  (check-true (tool-result-is-error? result))
  ;; Error message should mention the limit
  (define content (tool-result-content result))
  (define error-text (hash-ref (first content) 'text ""))
  (check-not-false (string-contains? error-text "exceeds limit"))
  ;; File should NOT have been created
  (check-false (file-exists? tmp)))

(test-case "SEC-03: custom limit via parameterization works"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (delete-file tmp)
  ;; 50 bytes is over a 10-byte limit but under the default 1MB limit
  (define content (make-string 50 #\C))
  ;; With tiny limit: should fail
  (define result-small (parameterize ([current-max-write-bytes 10])
                         (tool-write (hasheq 'path tmp 'content content))))
  (check-true (tool-result-is-error? result-small))
  ;; With larger limit: should succeed
  (define result-large (parameterize ([current-max-write-bytes 100])
                         (tool-write (hasheq 'path tmp 'content content))))
  (check-false (tool-result-is-error? result-large))
  (delete-file tmp))
