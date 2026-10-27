#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-tool-path-resolution.rkt — W2 canonical file resolution.
;;
;; find returns absolute openable paths; read/edit/write report the same
;; canonical absolute (symlink-resolved) spelling and round-trip through
;; relative and absolute inputs.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?)
         (only-in "../tools/builtins/find.rkt" tool-find)
         (only-in "../tools/builtins/read.rkt" tool-read)
         (only-in "../tools/builtins/edit.rkt" tool-edit)
         (only-in "../tools/builtins/write.rkt" tool-write)
         (only-in "../tools/exec-context.rkt" make-exec-context))

(define (success-details result)
  (unless (tool-result? result)
    (error 'test "not a tool result"))
  (unless (not (tool-result-is-error? result))
    (error 'test "expected success, got: ~a" (tool-result-content result)))
  (tool-result-details result))

(define path-suite
  (test-suite "canonical file resolution"

    (test-case "relative inputs resolve to one absolute canonical path"
      (define dir (make-temporary-file "q-path-resolve-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define target (build-path dir "probe.rkt"))
         (display-to-file "#lang racket/base\n(define x 1)\n" target #:exists 'replace)
         (parameterize ([current-directory dir])
           (define expected (path->string (simplify-path (resolve-path target) #f)))
           ;; read: relative input, absolute canonical details
           (define read-details (success-details (tool-read (hasheq 'path "probe.rkt"))))
           (check-equal? (hash-ref read-details 'path) expected)
           ;; find: absolute, directly passable to read
           (define found (success-details (tool-find (hasheq 'path "." 'name "probe.rkt"))))
           (define results (tool-result-content (tool-find (hasheq 'path "." 'name "probe.rkt"))))
           (check-equal? (hash-ref found 'path-base) 'absolute)
           (check-true (and (pair? results) (absolute-path? (string->path (car results))))
                       "find results stay absolute")
           (define roundtrip (success-details (tool-read (hasheq 'path (car results)))))
           (check-equal? (hash-ref roundtrip 'path) expected)
           ;; edit: relative input, absolute canonical details
           (define edit-result
             (tool-edit (hasheq 'path "probe.rkt" 'old-text "(define x 1)" 'new-text "(define x 2)")
                        #f))
           (check-equal? (hash-ref (success-details edit-result) 'path) expected)
           ;; write: relative input, absolute canonical details
           (define write-details
             (success-details (tool-write (hasheq 'path "probe.rkt" 'content "rewritten\n"))))
           (check-equal? (hash-ref write-details 'path) expected)))
       (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "a STRING execution working directory resolves like a path"
      ;; The scheduler/campaign execution context may carry its working
      ;; directory as a string (BUG-0089 string tolerance). read/edit/write must
      ;; resolve it, never raise a contract violation that turns an ordinary
      ;; edit into an unreadable tool crash.
      (define dir (make-temporary-file "q-path-string-wd-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define target (build-path dir "probe.rkt"))
         (display-to-file "alpha\n" target #:exists 'replace)
         (define expected (path->string (simplify-path (resolve-path target) #f)))
         (define ctx (make-exec-context #:working-directory (path->string dir)))
         (define edit-result
           (tool-edit (hasheq 'path "probe.rkt" 'old-text "alpha" 'new-text "beta") ctx))
         (check-equal? (hash-ref (success-details edit-result) 'path) expected)
         (check-equal? (file->string target) "beta\n")
         (define read-details (success-details (tool-read (hasheq 'path "probe.rkt") ctx)))
         (check-equal? (hash-ref read-details 'path) expected)
         ;; write resolves the SAME base: a relative path lands in
         ;; the execution working directory, not in the process cwd.
         (define write-details
           (success-details (tool-write (hasheq 'path "probe.rkt" 'content "gamma\n") ctx)))
         (check-equal? (hash-ref write-details 'path) expected)
         (check-equal? (file->string target) "gamma\n"))
       (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "a PATH execution working directory resolves for read/edit/write"
      ;; Same contract with the path form of the working directory, so both
      ;; accepted representations behave identically.
      (define dir (make-temporary-file "q-path-path-wd-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (make-directory* (build-path dir "sub"))
         (define target (build-path dir "sub" "probe.rkt"))
         (define expected (path->string (simplify-path (resolve-path target) #f)))
         (define ctx (make-exec-context #:working-directory dir))
         (define write-details
           (success-details (tool-write (hasheq 'path "sub/probe.rkt" 'content "delta\n") ctx)))
         (check-equal? (hash-ref write-details 'path) expected)
         (check-equal? (file->string target) "delta\n")
         (define edit-details
           (success-details
            (tool-edit (hasheq 'path "sub/probe.rkt" 'old-text "delta" 'new-text "epsilon") ctx)))
         (check-equal? (hash-ref edit-details 'path) expected)
         (define read-details (success-details (tool-read (hasheq 'path "sub/probe.rkt") ctx)))
         (check-equal? (hash-ref read-details 'path) expected))
       (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "symlinked components resolve to the physical canonical path"
      (define dir (make-temporary-file "q-path-link-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define real (build-path dir "real.rkt"))
                      (display-to-file "hello\n" real #:exists 'replace)
                      (define link (build-path dir "link.rkt"))
                      (make-file-or-directory-link real link)
                      (define details
                        (success-details (tool-read (hasheq 'path (path->string link)))))
                      (check-equal? (hash-ref details 'path)
                                    (path->string (simplify-path (resolve-path real) #f))
                                    "read reports the resolved file, not the link spelling"))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))))

(module+ test
  (run-tests path-suite))

(module+ main
  (run-tests path-suite))
