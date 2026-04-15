#lang racket

(require rackunit
         "../util/path-helpers.rkt"
         "../tools/builtins/read.rkt"
         "../tools/builtins/ls.rkt"
         "../tools/tool.rkt"
         racket/file)

;; ============================================================
;; Test suite: util/path-helpers.rkt — path-only
;; ============================================================

(test-case "path-only: returns directory for path with file"
  (check-equal? (path-only "foo/bar.txt") (string->path "foo/")))

(test-case "path-only: returns #f for simple filename (relative)"
  (check-equal? (path-only "simple.txt") #f))

(test-case "path-only: nested path returns parent directory"
  (check-equal? (path-only "a/b/c.txt") (string->path "a/b/")))

;; ============================================================
;; Test suite: expand-home-path
;; ============================================================

(test-case "expand-home-path: expands ~/ to home directory"
  (define home (path->string (find-system-path 'home-dir)))
  (check-equal? (expand-home-path "~/foo/bar.txt")
                (string-append home "/foo/bar.txt")))

(test-case "expand-home-path: expands bare ~ to home directory"
  (define home (path->string (find-system-path 'home-dir)))
  (check-equal? (expand-home-path "~") home))

(test-case "expand-home-path: leaves absolute paths unchanged"
  (check-equal? (expand-home-path "/tmp/foo.txt") "/tmp/foo.txt"))

(test-case "expand-home-path: leaves relative paths unchanged"
  (check-equal? (expand-home-path "src/main.rkt") "src/main.rkt"))

(test-case "expand-home-path: leaves empty string unchanged"
  (check-equal? (expand-home-path "") ""))

(test-case "expand-home-path: handles ~/ alone"
  (define home (path->string (find-system-path 'home-dir)))
  (check-equal? (expand-home-path "~/") (string-append home "/")))

;; Integration test: verify tilde-expanded paths resolve correctly
(test-case "expand-home-path: expanded home actually exists"
  (define expanded (expand-home-path "~"))
  (check-true (directory-exists? expanded)))

(test-case "expand-home-path: tilde file read roundtrip"
  (define tmp-file (build-path (find-system-path 'home-dir) ".q-tilde-test-tmp"))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (call-with-output-file tmp-file (lambda (out) (display "hello" out)) #:exists 'replace))
  (define expanded (expand-home-path "~/.q-tilde-test-tmp"))
  (check-true (file-exists? expanded))
  (check-equal? (file->string expanded) "hello")
  (delete-file tmp-file))

;; Integration: tool-read with tilde path
(test-case "tool-read: reads file via tilde path"
  (define tmp-file (build-path (find-system-path 'home-dir) ".q-read-tilde-test"))
  (call-with-output-file tmp-file (lambda (out) (display "tilde works" out)) #:exists 'replace)
  (define result (tool-read (hasheq 'path "~/.q-read-tilde-test")))
  (check-false (tool-result-is-error? result))
  (delete-file tmp-file))

;; Integration: tool-ls with tilde path
(test-case "tool-ls: lists home directory via tilde path"
  (define result (tool-ls (hasheq 'path "~")))
  (check-false (tool-result-is-error? result)))
