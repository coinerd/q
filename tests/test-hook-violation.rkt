#lang racket/base

(require rackunit
         "../extensions/hooks.rkt"
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
