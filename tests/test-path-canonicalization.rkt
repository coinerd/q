#lang racket

;; BOUNDARY: integration

;; tests/test-path-canonicalization.rkt — SEC-02/SEC-03 path canonicalization tests
;;
;; Verifies that tool-write and tool-edit canonicalize paths containing
;; .. components and handle absolute / deeply nested paths correctly.

(require rackunit
         racket/file
         (only-in "../tools/builtins/write.rkt"
                  tool-write
                  reset-cumulative-writes!
                  init-session-writes!)
         (only-in "../tools/builtins/edit.rkt" tool-edit)
         (only-in "../tools/tool.rkt" tool-result-is-error? tool-result-details))

;; ============================================================
;; SEC-02/SEC-03: Path canonicalization tests
;; ============================================================

(test-case "write to path with .. resolves correctly (SEC-02)"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define tmpdir (make-temporary-file "q-test-canonical-~a" 'directory))
  (make-directory (build-path tmpdir "sub"))
  (define raw-path (path->string (build-path tmpdir "sub" ".." "canonical.txt")))
  (define result (tool-write (hasheq 'path raw-path 'content "canonicalized")))
  (check-false (tool-result-is-error? result))
  ;; After canonicalization, file should be at tmpdir/canonical.txt (not tmpdir/sub/../canonical.txt)
  (check-pred file-exists? (build-path tmpdir "canonical.txt"))
  (check-equal? (file->string (build-path tmpdir "canonical.txt")) "canonicalized")
  (delete-directory/files tmpdir))

(test-case "edit with .. path canonicalizes (SEC-03)"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define tmpdir (make-temporary-file "q-test-canonical-edit-~a" 'directory))
  (make-directory (build-path tmpdir "sub"))
  (define target (build-path tmpdir "edit-me.txt"))
  ;; Write initial content
  (display-to-file "hello world" target #:exists 'replace)
  ;; Edit via .. path: tmpdir/sub/../edit-me.txt
  (define raw-path (path->string (build-path tmpdir "sub" ".." "edit-me.txt")))
  (define result (tool-edit (hasheq 'path raw-path 'old-text "hello" 'new-text "goodbye")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string target) "goodbye world")
  (delete-directory/files tmpdir))

(test-case "write to absolute path works (SEC-02)"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define tmpdir (make-temporary-file "q-test-abs-~a" 'directory))
  (define target (build-path tmpdir "abs-write.txt"))
  (define abs-path (path->string target))
  (define result (tool-write (hasheq 'path abs-path 'content "absolute path")))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? target)
  (check-equal? (file->string target) "absolute path")
  (delete-directory/files tmpdir))

(test-case "write to deeply nested path creates dirs (SEC-02)"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define tmpdir (make-temporary-file "q-test-deep-~a" 'directory))
  (define target (build-path tmpdir "a" "b" "c" "d" "deep.txt"))
  (define result (tool-write (hasheq 'path (path->string target) 'content "deeply nested")))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? target)
  (check-equal? (file->string target) "deeply nested")
  (delete-directory/files tmpdir))
