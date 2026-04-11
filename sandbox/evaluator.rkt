#lang racket/base

;; sandbox/evaluator.rkt — controlled evaluation for Racket code
;;
;; Uses Racket's built-in racket/sandbox module to evaluate Racket code
;; safely with timeout and resource limits.

(require racket/sandbox)

(provide (struct-out eval-result)
         eval-in-sandbox)

;; --------------------------------------------------
;; Result struct
;; --------------------------------------------------

(struct eval-result
  (value        ; any — the result value (or #f on error)
   output       ; string — captured stdout
   error        ; string or #f — error message
   elapsed-ms)  ; number
  #:transparent)

;; --------------------------------------------------
;; Safe output extraction
;; --------------------------------------------------

(define (safe-get-output evaluator)
  (with-handlers ([exn:fail? (lambda (_) "")])
    (let ([out (get-output evaluator)])
      (if (string? out) out ""))))

;; --------------------------------------------------
;; Main evaluator
;; --------------------------------------------------

(define (eval-in-sandbox code-string
                          #:timeout [timeout 30]
                          #:language [lang 'racket]
                          #:allow-for-load [allow-load '()])
  (define start-ms (current-inexact-milliseconds))

  ;; Outer handler catches creation errors
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (eval-result
           #f
           ""
           (exn-message e)
           (inexact->exact (round (- (current-inexact-milliseconds) start-ms)))))])
    ;; Create sandbox evaluator
    (define evaluator
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 256]
                     [sandbox-eval-limits (list timeout 256)]
                     [sandbox-network-guard #f]
                     [sandbox-path-permissions '()])
        (make-evaluator lang)))

    ;; Try to evaluate
    (define result-val
      (with-handlers
          ([exn:fail?
            (lambda (e)
              (define elapsed (inexact->exact (round (- (current-inexact-milliseconds) start-ms))))
              (eval-result #f (safe-get-output evaluator) (exn-message e) elapsed))])
        (define result (evaluator code-string))
        (define out (safe-get-output evaluator))
        (define elapsed (inexact->exact (round (- (current-inexact-milliseconds) start-ms))))
        (eval-result result out #f elapsed)))

    result-val))
