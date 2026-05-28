#lang racket

;; q/tests/test-custom-renderer.rkt — Tests for gui/extension-slots/custom-renderer.rkt

(require rackunit
         rackunit/text-ui
         "../gui/extension-slots/custom-renderer.rkt")

(define-test-suite test-custom-renderer
                   (test-case "make-renderer-registry creates empty registry"
                     (define r (make-renderer-registry))
                     (check-true (renderer-registry? r)))
                   (test-case "register-renderer! adds renderer"
                     (define r (make-renderer-registry))
                     (register-renderer! r 'code (lambda (d) (hash 'rendered 'code 'data d)))
                     (check-not-false (lookup-renderer r 'code)))
                   (test-case "lookup-renderer returns #f for unregistered type"
                     (define r (make-renderer-registry))
                     (check-false (lookup-renderer r 'nonexistent)))
                   (test-case "render-with-registry uses registered renderer"
                     (define r (make-renderer-registry))
                     (register-renderer! r 'code (lambda (d) (string-append "CODE: " d)))
                     (check-equal? (render-with-registry r 'code "hello") "CODE: hello"))
                   (test-case "render-with-registry passes through without renderer"
                     (define r (make-renderer-registry))
                     (check-equal? (render-with-registry r 'code "hello") "hello"))
                   (test-case "unregister-renderer! removes renderer"
                     (define r (make-renderer-registry))
                     (register-renderer! r 'code (lambda (d) d))
                     (unregister-renderer! r 'code)
                     (check-false (lookup-renderer r 'code)))
                   (test-case "multiple renderers coexist"
                     (define r (make-renderer-registry))
                     (register-renderer! r 'code (lambda (d) (string-append "C:" d)))
                     (register-renderer! r 'image (lambda (d) (string-append "I:" d)))
                     (check-equal? (render-with-registry r 'code "x") "C:x")
                     (check-equal? (render-with-registry r 'image "x") "I:x"))
                   (test-case "register-renderer! replaces existing"
                     (define r (make-renderer-registry))
                     (register-renderer! r 'code (lambda (d) "v1"))
                     (register-renderer! r 'code (lambda (d) "v2"))
                     (check-equal? (render-with-registry r 'code "x") "v2")))

(run-tests test-custom-renderer)
