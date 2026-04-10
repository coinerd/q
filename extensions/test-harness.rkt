#lang racket/base

;; extensions/test-harness.rkt — extension integration test harness
;;
;; Provides:
;;   - with-extension-test: macro that creates an isolated registry
;;   - inject-hook-event: single hook dispatch into a registry
;;   - capture-hook-results: multi-payload hook dispatch, collect results
;;   - check-hook-passed, check-hook-amended, check-hook-blocked: rackunit assertions
;;   - check-hook-payload: payload assertion
;;   - make-test-extension: convenience constructor for test extensions

(require (for-syntax racket/base)
         rackunit
         "api.rkt"
         "hooks.rkt")

(provide
 ;; Macro: isolated test context with fresh registry
 with-extension-test
 ;; Hook injection
 inject-hook-event
 capture-hook-results
 ;; Rackunit-style assertions on hook results
 check-hook-passed
 check-hook-amended
 check-hook-blocked
 check-hook-payload
 ;; Convenience: minimal extension for testing
 make-test-extension)

;; ============================================================
;; with-extension-test macro
;; ============================================================

(define-syntax (with-extension-test stx)
  (syntax-case stx ()
    [(_ ext-expr body ...)
     (with-syntax ([test-reg (datum->syntax stx 'test-registry)])
       #'(let ([test-reg (make-extension-registry)])
           (register-extension! test-reg ext-expr)
           body ...))]))

;; ============================================================
;; inject-hook-event : extension-registry? symbol? any/c -> hook-result?
;; ============================================================

(define (inject-hook-event registry hook-point payload)
  (dispatch-hooks hook-point payload registry))

;; ============================================================
;; capture-hook-results : extension-registry? symbol? (listof any/c) -> (listof hook-result?)
;; ============================================================

(define (capture-hook-results registry hook-point payloads)
  (for/list ([p payloads])
    (inject-hook-event registry hook-point p)))

;; ============================================================
;; Rackunit-style assertions on hook results
;; ============================================================

(define (check-hook-passed result [message #f])
  (with-check-info ([(if message 'message 'check) (or message "expected hook result action to be 'pass")])
    (check-equal? (hook-result-action result) 'pass)))

(define (check-hook-amended result [message #f])
  (with-check-info ([(if message 'message 'check) (or message "expected hook result action to be 'amend")])
    (check-equal? (hook-result-action result) 'amend)))

(define (check-hook-blocked result [message #f])
  (with-check-info ([(if message 'message 'check) (or message "expected hook result action to be 'block")])
    (check-equal? (hook-result-action result) 'block)))

(define (check-hook-payload result expected [message #f])
  (with-check-info ([(if message 'message 'check) (or message "expected hook result payload to match")])
    (check-equal? (hook-result-payload result) expected)))

;; ============================================================
;; make-test-extension : -> extension?
;; ============================================================

(define (make-test-extension #:name name
                             #:hooks [hooks (hasheq)])
  (extension name "0.0.1" "1" hooks))
