#lang racket

;; tests/test-racket-tooling.rkt — tests for racket-tooling extension
;;
;; Covers:
;;   - racket-check: format, syntax, test modes
;;   - racket-edit: replace mode (exact text replacement with validation)
;;   - racket-edit: skeleton mode (new file generation)
;;   - racket-edit: rewrite-form mode (line range replacement)
;;   - racket-codemod: dry run and apply
;;   - Error cases: missing args, file not found, non-unique oldText

(require rackunit
         racket/file
         racket/string
         json
         "../extensions/racket-tooling.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-rkt content)
  (define dir (make-temporary-file "racket-tool-test-~a" 'directory))
  (define path (build-path dir "test.rkt"))
  (call-with-output-file path (lambda (out) (display content out)))
  path)

(define (cleanup-path path)
  (define dir (path-only path))
  (when (and dir (directory-exists? dir))
    (delete-directory/files dir)))

(define sample-rkt
  "#lang racket/base\n\n(define (hello name)\n  (format \"Hello, ~a!\" name))\n\n(provide hello)\n")

;; ============================================================
;; racket-check tests
;; ============================================================

(test-case "racket-check syntax mode on valid file"
  (define path (make-temp-rkt sample-rkt))
  (define result (handle-racket-check (hasheq 'path (path->string path) 'mode "syntax")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (define parsed (string->jsexpr text))
  (check-true (hash-ref parsed 'all-pass?))
  (cleanup-path path))

(test-case "racket-check format mode on valid file"
  (define path (make-temp-rkt sample-rkt))
  (define result (handle-racket-check (hasheq 'path (path->string path) 'mode "format")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (cleanup-path path))

(test-case "racket-check default mode (syntax)"
  (define path (make-temp-rkt sample-rkt))
  (define result (handle-racket-check (hasheq 'path (path->string path))))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (cleanup-path path))

(test-case "racket-check on non-existent file errors"
  (with-handlers ([exn:fail? (lambda (e) (check-true #t))])
    (handle-racket-check (hasheq 'path "/nonexistent/file.rkt"))
    (check-false "Should have raised an error" #t)))

(test-case "racket-check missing path errors"
  (with-handlers ([exn:fail? (lambda (e) (check-true #t))])
    (handle-racket-check (hasheq))
    (check-false "Should have raised an error" #t)))

;; ============================================================
;; racket-edit replace mode tests
;; ============================================================

(test-case "racket-edit replace mode replaces text"
  (define path (make-temp-rkt "#lang racket/base\n\n(define (foo) 42)\n\n(provide foo)\n"))
  (define result
    (handle-racket-edit (hasheq 'file
                                (path->string path)
                                'mode
                                "replace"
                                'oldText
                                "(define (foo) 42)"
                                'newText
                                "(define (foo) 99)")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define new-content (call-with-input-file path port->string))
  (check-true (string-contains? new-content "99"))
  (check-false (string-contains? new-content "42"))
  (cleanup-path path))

(test-case "racket-edit replace non-unique text returns error"
  (define path (make-temp-rkt "#lang racket/base\n\n(define x 1)\n(define x 2)\n\n(provide x)\n"))
  (define result
    (handle-racket-edit (hasheq 'file (path->string path) 'mode "replace" 'oldText "x" 'newText "y")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result))
  (cleanup-path path))

(test-case "racket-edit replace missing oldText errors"
  (with-handlers ([exn:fail? (lambda (e) (check-true #t))])
    (handle-racket-edit (hasheq 'file "/tmp/test.rkt" 'mode "replace"))
    (check-false "Should have raised error" #t)))

(test-case "racket-edit replace text not found returns error"
  (define path (make-temp-rkt sample-rkt))
  (define result
    (handle-racket-edit (hasheq 'file
                                (path->string path)
                                'mode
                                "replace"
                                'oldText
                                "NOT_IN_FILE_AT_ALL"
                                'newText
                                "replacement")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result))
  (cleanup-path path))

;; ============================================================
;; racket-edit skeleton mode tests
;; ============================================================

(test-case "racket-edit skeleton creates new file"
  (define dir (make-temporary-file "racket-tool-test-~a" 'directory))
  (define path (build-path dir "new.rkt"))
  (define result
    (handle-racket-edit (hasheq 'file
                                (path->string path)
                                'mode
                                "skeleton"
                                'lang
                                "racket/base"
                                'requires
                                "racket/string"
                                'provides
                                "my-fn"
                                'signatures
                                "(define (my-fn x) x)")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (check-true (file-exists? path))
  (define content (call-with-input-file path port->string))
  (check-true (string-contains? content "#lang racket/base"))
  (check-true (string-contains? content "racket/string"))
  (check-true (string-contains? content "my-fn"))
  (delete-directory/files dir))

;; ============================================================
;; racket-edit rewrite-form mode tests
;; ============================================================

(test-case "racket-edit rewrite-form replaces lines"
  (define path
    (make-temp-rkt (string-append "#lang racket/base\n\n"
                                  "(define (foo) 1)\n\n"
                                  "(define (bar) 2)\n\n"
                                  "(provide foo bar)\n")))
  (define result
    (handle-racket-edit (hasheq 'file
                                (path->string path)
                                'mode
                                "rewrite-form"
                                'startLine
                                3
                                'endLine
                                3
                                'content
                                "(define (foo) 42)")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define content (call-with-input-file path port->string))
  (check-true (string-contains? content "42"))
  (cleanup-path path))

;; ============================================================
;; racket-codemod tests
;; ============================================================

(test-case "racket-codemod dry run shows matches"
  (define path (make-temp-rkt "#lang racket/base\n\n(define (foo) 42)\n\n(provide foo)\n"))
  (define result
    (handle-racket-codemod (hasheq 'file
                                   (path->string path)
                                   'pattern
                                   "(define @@NAME @@BODY)"
                                   'template
                                   "(define @@NAME 0)")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "Dry run"))
  (cleanup-path path))

(test-case "racket-codemod write applies changes"
  (define path (make-temp-rkt "#lang racket/base\n\n(define (foo) 42)\n\n(provide foo)\n"))
  (define result
    (handle-racket-codemod (hasheq 'file
                                   (path->string path)
                                   'pattern
                                   "(define @@NAME @@BODY)"
                                   'template
                                   "(define @@NAME 99)"
                                   'write
                                   #t)))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define content (call-with-input-file path port->string))
  (check-true (string-contains? content "99"))
  (cleanup-path path))

(test-case "racket-codemod no match returns info"
  (define path (make-temp-rkt "#lang racket/base\n\n(define (foo) 42)\n\n(provide foo)\n"))
  (define result
    (handle-racket-codemod (hasheq 'file
                                   (path->string path)
                                   'pattern
                                   "(nonexistent-matcher @@X)"
                                   'template
                                   "(replaced @@X)")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "No matches"))
  (cleanup-path path))

;; ============================================================
;; Unknown mode tests
;; ============================================================

(test-case "racket-edit unknown mode returns error"
  (define path (make-temp-rkt sample-rkt))
  (define result (handle-racket-edit (hasheq 'file (path->string path) 'mode "nonexistent")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result))
  (cleanup-path path))

(test-case "racket-check on file with syntax error"
  (define path (make-temp-rkt "#lang racket/base\n\n(define (foo\n"))
  (define result (handle-racket-check (hasheq 'path (path->string path) 'mode "syntax")))
  (check-pred tool-result? result)
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (define parsed (string->jsexpr text))
  (check-false (hash-ref parsed 'all-pass?))
  (cleanup-path path))
