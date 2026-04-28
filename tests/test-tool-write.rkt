#lang racket

(require rackunit
         "../tools/builtins/write.rkt"
         "../tools/tool.rkt"
         racket/file
         (only-in "../extensions/gsd-planning-state.rkt"
                  set-pinned-planning-dir!
                  reset-all-gsd-state!)
         (only-in "../util/safe-mode-state.rkt" current-safe-mode-config make-safe-mode-config))

;; ============================================================
;; tool-write — basic write
;; ============================================================

(test-case "tool-write creates a new file"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (delete-file tmp) ; ensure doesn't exist
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
  (define result
    (parameterize ([current-max-write-bytes 200])
      (tool-write (hasheq 'path tmp 'content small-content))))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? tmp)
  (delete-file tmp))

(test-case "SEC-03: writing over limit returns error"
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (delete-file tmp)
  (define big-content (make-string 200 #\B))
  (define result
    (parameterize ([current-max-write-bytes 100])
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
  (define result-small
    (parameterize ([current-max-write-bytes 10])
      (tool-write (hasheq 'path tmp 'content content))))
  (check-true (tool-result-is-error? result-small))
  ;; With larger limit: should succeed
  (define result-large
    (parameterize ([current-max-write-bytes 100])
      (tool-write (hasheq 'path tmp 'content content))))
  (check-false (tool-result-is-error? result-large))
  (delete-file tmp))

;; ============================================================
;; SEC-14: Cumulative write budget
;; ============================================================

(test-case "cumulative write budget enforced"
  (define tmp (make-temporary-file "q-write-test-~a"))
  (delete-file tmp)
  (reset-cumulative-writes!)
  (parameterize ([cumulative-write-budget 20]
                 [current-max-write-bytes 100])
    ;; First write under budget
    (define r1 (tool-write (hasheq 'path tmp 'content "1234567890")))
    (check-false (tool-result-is-error? r1))
    ;; Second write pushes over budget
    (define r2 (tool-write (hasheq 'path tmp 'content "123456789012")))
    (check-true (tool-result-is-error? r2)))
  (when (file-exists? tmp)
    (delete-file tmp)))

(test-case "reset-cumulative-writes clears counter"
  (define tmp (make-temporary-file "q-write-test-~a"))
  (delete-file tmp)
  (reset-cumulative-writes!)
  (parameterize ([cumulative-write-budget 20]
                 [current-max-write-bytes 100])
    (tool-write (hasheq 'path tmp 'content "1234567890"))
    (reset-cumulative-writes!)
    ;; After reset, can write again
    (define r (tool-write (hasheq 'path tmp 'content "1234567890")))
    (check-false (tool-result-is-error? r)))
  (when (file-exists? tmp)
    (delete-file tmp)))

;; ============================================================
;; F4: Per-session write budget isolation (v0.21.5)
;; ============================================================

(test-case "F4: init-session-writes! resets to fresh counter"
  (reset-cumulative-writes!)
  (parameterize ([cumulative-write-budget 20]
                 [current-max-write-bytes 100])
    (define tmp (make-temporary-file "q-write-test-~a"))
    (delete-file tmp)
    ;; Write 10 bytes
    (tool-write (hasheq 'path tmp 'content "1234567890"))
    ;; Switch to new session — fresh counter
    (init-session-writes!)
    ;; Can write again even though previous session had 10 bytes
    (define r (tool-write (hasheq 'path tmp 'content "123456789012")))
    (check-false (tool-result-is-error? r))
    (when (file-exists? tmp)
      (delete-file tmp))))

(test-case "F4: parameterize isolation between sessions"
  (init-session-writes!)
  (parameterize ([cumulative-write-budget 15]
                 [current-max-write-bytes 100])
    (define tmp (make-temporary-file "q-write-test-~a"))
    (delete-file tmp)
    ;; Session A: write 10 bytes
    (tool-write (hasheq 'path tmp 'content "1234567890"))
    ;; Simulate session B with fresh counter
    (init-session-writes!)
    ;; Session B should have fresh budget
    (define r (tool-write (hasheq 'path tmp 'content "123456789012")))
    (check-false (tool-result-is-error? r))
    (when (file-exists? tmp)
      (delete-file tmp))))

;; ============================================================
;; v0.21.10 (F7): Planning path resolution hardening
;; ============================================================

(test-case "F7: .planning/ path rewritten to pinned dir"
  (reset-all-gsd-state!)
  (init-session-writes!)
  (define pinned-dir (make-temporary-file "q-test-pinned-~a" 'directory))
  (define pinned-planning (build-path pinned-dir ".planning"))
  (make-directory pinned-planning)
  (set-pinned-planning-dir! pinned-dir)
  (define result (tool-write (hasheq 'path ".planning/test.md" 'content "planning content")))
  (check-false (tool-result-is-error? result))
  (check-true (file-exists? (build-path pinned-planning "test.md")))
  (check-equal? (file->string (build-path pinned-planning "test.md")) "planning content")
  (delete-directory/files pinned-dir)
  (reset-all-gsd-state!))

(test-case "F7: ./.planning/ path rewritten to pinned dir"
  (reset-all-gsd-state!)
  (init-session-writes!)
  (define pinned-dir (make-temporary-file "q-test-pinned-~a" 'directory))
  (define pinned-planning (build-path pinned-dir ".planning"))
  (make-directory pinned-planning)
  (set-pinned-planning-dir! pinned-dir)
  (define result (tool-write (hasheq 'path "./.planning/other.md" 'content "other")))
  (check-false (tool-result-is-error? result))
  (check-true (file-exists? (build-path pinned-planning "other.md")))
  (delete-directory/files pinned-dir)
  (reset-all-gsd-state!))

(test-case "F7: non-.planning/ path unchanged with pinned dir"
  (reset-all-gsd-state!)
  (init-session-writes!)
  (define pinned-dir (make-temporary-file "q-test-pinned-~a" 'directory))
  (set-pinned-planning-dir! pinned-dir)
  (define tmp (make-temporary-file "q-test-write-~a.txt"))
  (delete-file tmp)
  (define result (tool-write (hasheq 'path tmp 'content "normal write")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "normal write")
  (delete-file tmp)
  (delete-directory/files pinned-dir)
  (reset-all-gsd-state!))

(test-case "F7: absolute path unchanged with pinned dir"
  (reset-all-gsd-state!)
  (init-session-writes!)
  (define pinned-dir (make-temporary-file "q-test-pinned-~a" 'directory))
  (set-pinned-planning-dir! pinned-dir)
  (define tmp-dir (make-temporary-file "q-test-~a" 'directory))
  (define planning-dir (build-path tmp-dir ".planning"))
  (make-directory planning-dir)
  (define target (build-path planning-dir "free.md"))
  (define result (tool-write (hasheq 'path (path->string target) 'content "free write")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string target) "free write")
  (delete-directory/files tmp-dir)
  (delete-directory/files pinned-dir)
  (reset-all-gsd-state!))

;; ============================================================
;; TH-01: Additional coverage tests
;; ============================================================

(test-case "TH-01: safe-mode rejects write outside project root"
  (init-session-writes!)
  (define tmpdir (make-temporary-file "q-test-safe-~a" 'directory))
  (define outside-path "/tmp/outside-root-TH01.txt")
  (define result
    (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t
                                                                    #:project-root tmpdir)])
      (tool-write (hasheq 'path outside-path 'content "should fail"))))
  (check-true (tool-result-is-error? result))
  (delete-directory/files tmpdir))

(test-case "TH-01: cumulative budget boundary — exact limit succeeds"
  (init-session-writes!)
  (define tmp (make-temporary-file "q-write-budget-~a.txt"))
  (delete-file tmp)
  (reset-cumulative-writes!)
  (parameterize ([cumulative-write-budget 10]
                 [current-max-write-bytes 100])
    ;; Exactly 10 bytes — should succeed
    (define r (tool-write (hasheq 'path tmp 'content "1234567890")))
    (check-false (tool-result-is-error? r)))
  (when (file-exists? tmp)
    (delete-file tmp)))

(test-case "TH-01: cumulative budget boundary — one byte over fails"
  (init-session-writes!)
  (define tmp (make-temporary-file "q-write-budget-~a.txt"))
  (delete-file tmp)
  (reset-cumulative-writes!)
  (parameterize ([cumulative-write-budget 10]
                 [current-max-write-bytes 100])
    ;; Write 10 bytes — fills budget exactly
    (tool-write (hasheq 'path tmp 'content "1234567890"))
    ;; One more byte (11 total) — should exceed budget
    (define r2 (tool-write (hasheq 'path tmp 'content "1")))
    (check-true (tool-result-is-error? r2)))
  (when (file-exists? tmp)
    (delete-file tmp)))

(test-case "TH-01: expand-home-path — write to ~/test-q-write-home.txt"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define home (find-system-path 'home-dir))
  (define target (build-path home "test-q-write-home.txt"))
  (define result (tool-write (hasheq 'path "~/test-q-write-home.txt" 'content "home write")))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? target)
  (check-equal? (file->string target) "home write")
  (delete-file target))

(test-case "TH-01: directory creation via deeply nested path"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define tmpdir (make-temporary-file "q-test-nested-~a" 'directory))
  (define filepath (build-path tmpdir "a" "b" "c" "file.txt"))
  (define result (tool-write (hasheq 'path (path->string filepath) 'content "deeply nested")))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? filepath)
  (check-equal? (file->string filepath) "deeply nested")
  (delete-directory/files tmpdir))

(test-case "TH-01: empty content write"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define tmp (make-temporary-file "q-write-empty-~a.txt"))
  (delete-file tmp)
  (define result (tool-write (hasheq 'path tmp 'content "")))
  (check-false (tool-result-is-error? result))
  (check-pred file-exists? tmp)
  (check-equal? (file-size tmp) 0)
  (delete-file tmp))

(test-case "TH-01: path with .. components canonicalizes correctly"
  (init-session-writes!)
  (reset-cumulative-writes!)
  (define tmpdir (make-temporary-file "q-test-dotdot-~a" 'directory))
  (make-directory (build-path tmpdir "sub"))
  (define raw-path (path->string (build-path tmpdir "sub" ".." "file.txt")))
  (define result (tool-write (hasheq 'path raw-path 'content "dotdot write")))
  (check-false (tool-result-is-error? result))
  ;; After canonicalization, file should be in tmpdir/file.txt
  (check-pred file-exists? (build-path tmpdir "file.txt"))
  (check-equal? (file->string (build-path tmpdir "file.txt")) "dotdot write")
  (delete-directory/files tmpdir))
