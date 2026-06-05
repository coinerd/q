#lang racket

;; q/tests/test-ui-render-hooks.rkt — Render hook schema and isolation tests
;;
;; W7.1–W7.4 (v0.94.7): Verify hook construction, validation, safe application,
;; error isolation, and timeout behavior.

(require rackunit
         rackunit/text-ui
         "../ui-core/render-hooks.rkt")

(define-test-suite
 test-ui-render-hooks

 ;; ── Construction ───

 (test-case "make-render-hook creates valid hook"
   (define h (make-render-hook 'test-pre 'pre-render identity))
   (check-eq? (render-hook-name h) 'test-pre)
   (check-eq? (render-hook-phase h) 'pre-render)
   (check-equal? (render-hook-timeout-ms h) 100))

 (test-case "custom timeout"
   (define h (make-render-hook 'slow 'post-render identity #:timeout-ms 500))
   (check-equal? (render-hook-timeout-ms h) 500))

 (test-case "custom validator"
   (define h (make-render-hook 'v 'pre-render identity
                               #:validator string?))
   (check-not-false (render-hook-validator h)))

 ;; ── Phase validation ───

 (test-case "valid phases"
   (check-true (render-hook-phase? 'pre-render))
   (check-true (render-hook-phase? 'post-render)))

 (test-case "invalid phases"
   (check-false (render-hook-phase? 'mid-render))
   (check-false (render-hook-phase? 'invalid)))

 ;; ── Apply render hook ───

 (test-case "apply-render-hook transforms data"
   (define h (make-render-hook 'up 'pre-render
                               (lambda (d) (string-upcase d))))
   (check-equal? (apply-render-hook h "hello") "HELLO"))

 (test-case "apply-render-hook with identity returns same data"
   (define h (make-render-hook 'id 'pre-render identity))
   (define data (hash 'x 1 'y 2))
   (check-eq? (apply-render-hook h data) data))

 ;; ── Safe application ───

 (test-case "apply-render-hook-safe returns transformed data on success"
   (define h (make-render-hook 'up 'pre-render
                               (lambda (d) (string-upcase d))))
   (define-values (result ok?) (apply-render-hook-safe h "hello"))
   (check-true ok?)
   (check-equal? result "HELLO"))

 (test-case "apply-render-hook-safe returns original on handler error"
   (define h (make-render-hook 'boom 'pre-render
                               (lambda (d) (error "boom"))))
   (define-values (result ok?) (apply-render-hook-safe h "original"))
   (check-false ok?)
   (check-equal? result "original"))

 (test-case "apply-render-hook-safe returns original on validator failure"
   (define h (make-render-hook 'v 'pre-render
                               (lambda (d) 42)
                               #:validator string?))
   (define-values (result ok?) (apply-render-hook-safe h "input"))
   (check-false ok?)
   (check-equal? result "input"))

 (test-case "apply-render-hook-safe passes when no validator"
   (define h (make-render-hook 'nv 'post-render
                               (lambda (d) (* d 2))))
   (define-values (result ok?) (apply-render-hook-safe h 5))
   (check-true ok?)
   (check-equal? result 10))

 ;; ── Error isolation ───

 (test-case "multiple hooks: error in one doesn't affect others"
   (define h1 (make-render-hook 'good 'pre-render (lambda (d) (string-append d "!"))))
   (define h2 (make-render-hook 'bad 'pre-render (lambda (d) (error "fail"))))
   (define h3 (make-render-hook 'also-good 'pre-render (lambda (d) (string-append d "?"))))

   (define-values (r1 ok1?) (apply-render-hook-safe h1 "x"))
   (check-true ok1?)
   (check-equal? r1 "x!")

   (define-values (r2 ok2?) (apply-render-hook-safe h2 "x"))
   (check-false ok2?)
   (check-equal? r2 "x")

   (define-values (r3 ok3?) (apply-render-hook-safe h3 "x"))
   (check-true ok3?)
   (check-equal? r3 "x?"))

 ;; ── Validation with complex data ───

 (test-case "validator with hash data"
   (define h (make-render-hook 'hash-hook 'pre-render
                               (lambda (d) (hash-set d 'processed #t))
                               #:validator hash?))
   (define-values (result ok?) (apply-render-hook-safe h (hash 'x 1)))
   (check-true ok?)
   (check-true (hash-ref result 'processed)))

 (test-case "validator rejects non-hash"
   (define h (make-render-hook 'strict 'pre-render
                               (lambda (d) "not a hash")
                               #:validator hash?))
   (define-values (result ok?) (apply-render-hook-safe h (hash 'x 1)))
   (check-false ok?)
   (check-true (hash? result))))

(run-tests test-ui-render-hooks)
