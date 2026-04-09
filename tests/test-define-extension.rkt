#lang racket

(require rackunit
         "../extensions/define-extension.rkt"
         "../extensions/api.rkt")

;; ============================================================
;; define-q-extension macro — basic
;; ============================================================

(test-case "define-q-extension creates extension struct"
  (define-q-extension test-ext
    #:version "1.0.0"
    #:api-version "2")
  (check-true (extension? test-ext))
  (check-equal? (extension-name test-ext) "test-ext")
  (check-equal? (extension-version test-ext) "1.0.0")
  (check-equal? (extension-api-version test-ext) "2"))

(test-case "define-q-extension defaults version and api-version"
  (define-q-extension simple-ext)
  (check-equal? (extension-name simple-ext) "simple-ext")
  (check-equal? (extension-version simple-ext) "0.1.0")
  (check-equal? (extension-api-version simple-ext) "1"))

(test-case "define-q-extension with hook handlers"
  (define-q-extension hooked-ext
    #:version "2.0"
    #:on before-send (λ (p) p)
    #:on after-receive (λ (p) p))
  (check-true (hash-has-key? (extension-hooks hooked-ext) 'before-send))
  (check-true (hash-has-key? (extension-hooks hooked-ext) 'after-receive)))

(test-case "define-q-extension hooks are callable"
  (define-q-extension callable-ext
    #:on test-hook (λ (p) (string-append p "-modified")))
  (define handler (hash-ref (extension-hooks callable-ext) 'test-hook))
  (check-equal? (handler "input") "input-modified"))

(test-case "define-q-extension can be registered in registry"
  (define-q-extension reg-ext
    #:version "1.0"
    #:on before-send (λ (p) p))
  (define reg (make-extension-registry))
  (register-extension! reg reg-ext)
  (check-equal? (lookup-extension reg "reg-ext") reg-ext))
