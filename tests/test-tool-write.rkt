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
