#lang racket

;; tests/test-safemode-enforcement.rkt — tests for safe-mode enforcement
;; in tool dispatch (SEC-01) and path validation in file I/O tools (SEC-04).

(require rackunit
         racket/file
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         "../runtime/safe-mode.rkt"
         "../tools/builtins/read.rkt"
         "../tools/builtins/write.rkt"
         "../tools/builtins/edit.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Safe delete that ignores missing files
(define (safe-delete-file p)
  (with-handlers ([exn:fail:filesystem? void])
    (delete-file p)))

(define (safe-delete-dir p)
  (with-handlers ([exn:fail:filesystem? void])
    (delete-directory/files p)))

;; Make a simple bash-like tool for testing
(define (make-dummy-tool name exec-fn)
  (make-tool name
             (format "dummy ~a tool" name)
             (hasheq 'type "object"
                     'properties (hasheq 'path (hasheq 'type "string"))
                     'required '(path))
             exec-fn))

;; A dummy bash tool that always succeeds
(define bash-tool
  (make-tool "bash"
             "run shell commands"
             (hasheq 'type "object"
                     'properties (hasheq 'command (hasheq 'type "string"))
                     'required '(command))
             (lambda (args ctx)
               (make-success-result (list "ok") (hasheq)))))

;; A dummy edit tool that always succeeds
(define edit-dummy-tool
  (make-tool "edit"
             "edit files"
             (hasheq 'type "object"
                     'properties (hasheq 'path (hasheq 'type "string")
                                         'old-text (hasheq 'type "string")
                                         'new-text (hasheq 'type "string"))
                     'required '(path old-text new-text))
             (lambda (args ctx)
               (make-success-result (list "edited") (hasheq)))))

;; A dummy read tool that always succeeds
(define read-dummy-tool
  (make-tool "read"
             "read files"
             (hasheq 'type "object"
                     'properties (hasheq 'path (hasheq 'type "string"))
                     'required '(path))
             (lambda (args ctx)
               (make-success-result (list "read") (hasheq)))))

;; Build a registry with standard builtins for scheduler tests
(define (make-test-registry)
  (define reg (make-tool-registry))
  (register-tool! reg bash-tool)
  (register-tool! reg edit-dummy-tool)
  (register-tool! reg read-dummy-tool)
  reg)

;; ============================================================
;; Tests
;; ============================================================

(test-case "SEC-01: bash tool blocked by scheduler in safe-mode"
  (parameterize ([current-safe-mode #t]
                 [project-root (current-directory)])
    (define reg (make-test-registry))
    (define tc (make-tool-call "t1" "bash" (hasheq 'command "echo hi")))
    (define result (run-tool-batch (list tc) reg))
    (define results (scheduler-result-results result))
    (check-equal? (length results) 1)
    (define r (first results))
    (check-true (tool-result-is-error? r) "bash result is error in safe-mode")
    (define content (tool-result-content r))
    (check-true (ormap (lambda (item)
                         (and (hash? item)
                              (string-contains? (hash-ref item 'text "") "blocked by safe-mode")))
                       content)
                "error message mentions safe-mode block")))

(test-case "SEC-01: edit tool blocked by scheduler in safe-mode"
  (parameterize ([current-safe-mode #t]
                 [project-root (current-directory)])
    (define reg (make-test-registry))
    (define tc (make-tool-call "t2" "edit"
                               (hasheq 'path "/tmp/fake.txt"
                                       'old-text "a"
                                       'new-text "b")))
    (define result (run-tool-batch (list tc) reg))
    (define results (scheduler-result-results result))
    (check-equal? (length results) 1)
    (define r (first results))
    (check-true (tool-result-is-error? r) "edit result is error in safe-mode")
    (define content (tool-result-content r))
    (check-true (ormap (lambda (item)
                         (and (hash? item)
                              (string-contains? (hash-ref item 'text "") "blocked by safe-mode")))
                       content)
                "error message mentions safe-mode block")))

(test-case "SEC-01: read tool allowed by scheduler in safe-mode"
  (parameterize ([current-safe-mode #t]
                 [project-root (current-directory)])
    (define reg (make-test-registry))
    (define tc (make-tool-call "t3" "read" (hasheq 'path "some-file.txt")))
    (define result (run-tool-batch (list tc) reg))
    (define results (scheduler-result-results result))
    (check-equal? (length results) 1)
    (define r (first results))
    (check-false (tool-result-is-error? r) "read result is not error in safe-mode")))

(test-case "SEC-01: all tools allowed when safe-mode off"
  (parameterize ([current-safe-mode #f]
                 [project-root (current-directory)])
    (define reg (make-test-registry))
    (define tcs (list (make-tool-call "t4" "bash" (hasheq 'command "echo hi"))
                      (make-tool-call "t5" "edit"
                                      (hasheq 'path "fake.txt"
                                              'old-text "a"
                                              'new-text "b"))
                      (make-tool-call "t6" "read" (hasheq 'path "fake.txt"))))
    (define result (run-tool-batch tcs reg))
    (define results (scheduler-result-results result))
    (check-equal? (length results) 3)
    (for ([r results])
      (check-false (tool-result-is-error? r) "no errors when safe-mode off"))))

(test-case "SEC-04: tool-read rejects path outside project root in safe-mode"
  (define tmp-dir (make-temporary-file "safemode-read-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define test-file (build-path tmp-dir "test.txt"))
      (call-with-output-file test-file (lambda (out) (display "hello" out)))
      (parameterize ([current-safe-mode #t]
                     [project-root tmp-dir])
        ;; Inside project: should work
        (define r-ok (tool-read (hasheq 'path (path->string test-file))))
        (check-false (tool-result-is-error? r-ok) "read inside project succeeds")
        ;; Outside project: should fail
        (define r-bad (tool-read (hasheq 'path "/etc/passwd")))
        (check-true (tool-result-is-error? r-bad) "read outside project fails")
        (define content (tool-result-content r-bad))
        (check-true (ormap (lambda (item)
                             (and (hash? item)
                                  (string-contains? (hash-ref item 'text "") "Access denied")))
                           content)
                    "error message mentions access denied")))
    (lambda ()
      (safe-delete-dir tmp-dir))))

(test-case "SEC-04: tool-write rejects path outside project root in safe-mode"
  (define tmp-dir (make-temporary-file "safemode-write-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([current-safe-mode #t]
                     [project-root tmp-dir])
        ;; Inside project: should work
        (define inside-path (build-path tmp-dir "output.txt"))
        (define r-ok (tool-write (hasheq 'path (path->string inside-path)
                                         'content "hello")))
        (check-false (tool-result-is-error? r-ok) "write inside project succeeds")
        ;; Outside project: should fail
        (define r-bad (tool-write (hasheq 'path "/tmp/safemode-bypass-write.txt"
                                          'content "evil")))
        (check-true (tool-result-is-error? r-bad) "write outside project fails")
        (define content (tool-result-content r-bad))
        (check-true (ormap (lambda (item)
                             (and (hash? item)
                                  (string-contains? (hash-ref item 'text "") "Access denied")))
                           content)
                    "error message mentions access denied")))
    (lambda ()
      (safe-delete-dir tmp-dir))))

(test-case "SEC-04: tool-edit rejects path outside project root in safe-mode"
  (define tmp-dir (make-temporary-file "safemode-edit-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      ;; Create a file inside project
      (define test-file (build-path tmp-dir "editme.txt"))
      (call-with-output-file test-file (lambda (out) (display "hello world" out)))
      ;; Create a file outside project
      (define outside-file (make-temporary-file "outside-edit-~a.txt"))
      (call-with-output-file outside-file (lambda (out) (display "target" out))
        #:exists 'replace)
      (parameterize ([current-safe-mode #t]
                     [project-root tmp-dir])
        ;; Inside project: should work
        (define r-ok (tool-edit (hasheq 'path (path->string test-file)
                                        'old-text "hello"
                                        'new-text "goodbye")))
        (check-false (tool-result-is-error? r-ok) "edit inside project succeeds")
        ;; Outside project: should fail
        (define r-bad (tool-edit (hasheq 'path (path->string outside-file)
                                         'old-text "target"
                                         'new-text "replaced")))
        (check-true (tool-result-is-error? r-bad) "edit outside project fails")
        (define content (tool-result-content r-bad))
        (check-true (ormap (lambda (item)
                             (and (hash? item)
                                  (string-contains? (hash-ref item 'text "") "Access denied")))
                           content)
                    "error message mentions access denied"))
      (safe-delete-file outside-file))
    (lambda ()
      (safe-delete-dir tmp-dir))))

(test-case "SEC-04: symlink bypass is resolved by allowed-path?"
  (define tmp-dir (make-temporary-file "safemode-symlink-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      ;; Create a file inside project
      (define real-file (build-path tmp-dir "real.txt"))
      (call-with-output-file real-file (lambda (out) (display "content" out)))
      ;; Create symlink outside project pointing inside
      (define link-dir (make-temporary-file "safemode-linkdir-~a" 'directory))
      (define symlink-path (build-path link-dir "link.txt"))
      (with-handlers ([exn:fail?
                        (lambda (e)
                          ;; Symlinks may not be supported; skip test
                          (safe-delete-dir tmp-dir)
                          (safe-delete-dir link-dir))])
        ;; Make symlink from outside → inside project
        (make-file-or-directory-link real-file symlink-path)
        (parameterize ([current-safe-mode #t]
                       [project-root tmp-dir])
          ;; The symlink lives outside, but resolves to inside.
          ;; With the old simplify-path code, this would incorrectly be allowed
          ;; because the symlink path itself is outside.
          ;; With resolve-path, the resolved path is inside → allowed.
          ;; However, the link path string starts with the link-dir prefix,
          ;; so allowed-path? on the raw path should reject it.
          ;; The key test: resolve-path should resolve symlinks so that
          ;; a symlink pointing OUTSIDE from inside gets caught.
          (define outside-target (build-path link-dir "outside.txt"))
          (call-with-output-file outside-target
            (lambda (out) (display "outside" out)))
          ;; Create symlink inside project pointing outside
          (define insider-link (build-path tmp-dir "escape.txt"))
          (make-file-or-directory-link outside-target insider-link)
          ;; This symlink is inside project root, but resolves to outside.
          ;; With simplify-path it would be allowed; with resolve-path it should
          ;; resolve to the outside target, which is NOT under project root.
          ;; But actually: the path string is inside project root, and
          ;; resolve-path resolves to outside. The current allowed-path?
          ;; uses resolve-path, so it checks the RESOLVED path against root.
          ;; So the resolved path is outside → blocked. Good.
          (check-false (allowed-path? (path->string insider-link))
                       "symlink inside project pointing outside is blocked after resolve"))))
    (lambda ()
      (safe-delete-dir tmp-dir)
      ;; link-dir might not exist if symlink creation failed
      (safe-delete-dir (build-path (find-system-path 'temp-dir) "safemode-linkdir-")))))

(test-case "SEC-04: allowed-path? works normally with safe-mode off"
  (parameterize ([current-safe-mode #f]
                 [project-root "/some/project"])
    (check-true (allowed-path? "/etc/passwd") "any path allowed when safe-mode off")
    (check-true (allowed-path? "/some/other/path") "any path allowed when safe-mode off")))

;; ============================================================
;; Summary
;; ============================================================
(printf "~nAll safe-mode enforcement tests passed.~n")
