#lang racket/base

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

(require rackunit
         "../extensions/hooks.rkt"
         "../extensions/combinators.rkt"
         "../extensions/api.rkt"
         "../util/hook-types.rkt")

(test-case "hook violation downgrades invalid action to pass"
  ;; turn-end only allows pass/amend — block should be downgraded to pass
  (define reg (make-extension-registry))
  (define ext
    (extension "blocker" "1.0" "1.0" (hash 'turn-end (lambda (payload) (hook-block "blocked!")))))
  (register-extension! reg ext)
  (define result (dispatch-hooks 'turn-end "test-payload" reg))
  (check-equal? (hook-result-action result) 'pass))

(test-case "hook dispatch with valid action returns normally"
  (define reg (make-extension-registry))
  (define ext
    (extension "good-ext"
               "1.0"
               "1.0"
               (hash 'model-request-pre
                     (lambda (payload) (hook-amend (string-append payload " amended"))))))
  (register-extension! reg ext)
  (define result (dispatch-hooks 'model-request-pre "test" reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "test amended"))

;; v0.28.11: Violation callback test
(test-case "violation callback is invoked on invalid action"
  (define violations '())
  (parameterize ([current-hook-violation-callback
                  (lambda (ext-name hook-point action schema-version)
                    (set! violations
                          (cons (list ext-name hook-point action schema-version) violations)))])
    (define reg (make-extension-registry))
    (define ext (extension "bad-ext" "1.0" "1.0" (hash 'turn-end (lambda (p) (hook-block "nope")))))
    (register-extension! reg ext)
    (define result (dispatch-hooks 'turn-end "payload" reg))
    (check-equal? (hook-result-action result) 'pass)
    (check-equal? (length violations) 1)
    (check-equal? (caar violations) "bad-ext")
    (check-equal? (cadr (car violations)) 'turn-end)
    (check-equal? (caddr (car violations)) 'block)))

(test-case "violation callback is not invoked on valid action"
  (define violations '())
  (parameterize ([current-hook-violation-callback (lambda args
                                                    (set! violations (cons args violations)))])
    (define reg (make-extension-registry))
    (define ext (extension "good-ext" "1.0" "1.0" (hash 'turn-end (lambda (p) (hook-pass p)))))
    (register-extension! reg ext)
    (define result (dispatch-hooks 'turn-end "payload" reg))
    (check-equal? (hook-result-action result) 'pass)
    (check-equal? violations '())))

(test-case "default violation callback is #f"
  (check-false (current-hook-violation-callback)))
