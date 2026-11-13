#lang racket

;; @speed fast
;; @suite fast
;; @boundary integration
;; Compile package-visible modules without executing their top-level code.
;; Package-wide discovery and setup remain covered by the package setup gate.

(require rackunit
         rackunit/text-ui
         racket/runtime-path)

(define-runtime-path repo-root "..")

(define (module-compiles? path)
  (define-values (proc out in err) (subprocess #f #f #f (find-executable-path "raco") "make" path))
  (close-output-port in)
  ;; Drain both pipes while the compiler runs; waiting first can deadlock on
  ;; a sufficiently large diagnostic or compiler output.
  (define stdout #f)
  (define stderr #f)
  (define stdout-thread (thread (lambda () (set! stdout (port->string out)))))
  (define stderr-thread (thread (lambda () (set! stderr (port->string err)))))
  (subprocess-wait proc)
  (thread-wait stdout-thread)
  (thread-wait stderr-thread)
  (close-input-port out)
  (close-input-port err)
  (values (zero? (subprocess-status proc)) (format "~a~a" stdout stderr)))

(define-test-suite
 package-compile-boundary-tests
 (test-case "certificate generator compiles"
   (define-values (ok? output)
     (module-compiles? (build-path repo-root "cli" "generate-certificates.rkt")))
   (check-true ok? output))
 (test-case "SDK integration script compiles without running it"
   (define-values (ok? output)
     (module-compiles? (build-path repo-root "scripts" "sdk-gsd-integration-test.rkt")))
   (check-true ok? output))
 (test-case "invalid modules are rejected"
   (define dir (make-temporary-file "package-compile-~a" 'directory))
   (dynamic-wind void
                 (lambda ()
                   (define path (build-path dir "invalid.rkt"))
                   (call-with-output-file
                    path
                    (lambda (out)
                      (display "#lang racket/base\n(unbound-package-compile-control)\n" out)))
                   (define-values (ok? output) (module-compiles? path))
                   (check-false ok?)
                   (check-regexp-match #rx"unbound-package-compile-control" output))
                 (lambda () (delete-directory/files dir)))))

(define (run-compile-tests!)
  (unless (zero? (run-tests package-compile-boundary-tests))
    (error 'package-compile-boundary "compilation checks failed")))

(module+ test
  (run-compile-tests!))

(module+ main
  (run-compile-tests!))
