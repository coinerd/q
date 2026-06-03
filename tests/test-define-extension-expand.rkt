#lang racket/base

;; tests/test-define-extension-expand.rkt — define-q-extension macro tests (M-4)
;; BOUNDARY: pure
;;
;; Tests the define-q-extension MACRO, not just the underlying struct.
;; Verifies macro expansion produces correct extension values with
;; hooks, defaults, and hook-point validation.

(require racket/function
         rackunit
         "../extensions/define-extension.rkt"
         "../util/extension/extensions.rkt")

;; ============================================================
;; W2-T1 (M-4): Test the macro, not just the struct
;; ============================================================

;; Basic invocation with all fields
(define-q-extension test-ext-basic
                    #:version "2.0.0"
                    #:api-version "2"
                    #:on message-start
                    (lambda (payload) 'hook-result-1))

(test-case "define-q-extension creates extension with correct name"
  (check-equal? (extension-name test-ext-basic) "test-ext-basic"))

(test-case "define-q-extension sets version from #:version"
  (check-equal? (extension-version test-ext-basic) "2.0.0"))

(test-case "define-q-extension sets api-version from #:api-version"
  (check-equal? (extension-api-version test-ext-basic) "2"))

(test-case "define-q-extension registers hook handler"
  (define hooks (extension-hooks test-ext-basic))
  (check-not-false (hash-has-key? hooks 'message-start))
  (check-equal? ((hash-ref hooks 'message-start) 'ignored) 'hook-result-1))

;; Extension with defaults (no #:version, no #:api-version)
(define-q-extension test-ext-defaults #:on turn-start (lambda (payload) 'turn-hook-result))

(test-case "define-q-extension defaults version to 0.1.0"
  (check-equal? (extension-version test-ext-defaults) "0.1.0"))

(test-case "define-q-extension defaults api-version to 1"
  (check-equal? (extension-api-version test-ext-defaults) "1"))

;; Extension with multiple hooks
(define-q-extension test-ext-multi
                    #:version "1.5.0"
                    #:on model-request-pre
                    (lambda (p) 'pre)
                    #:on tool-call
                    (lambda (p) 'tool)
                    #:on turn-end
                    (lambda (p) 'end))

(test-case "define-q-extension with multiple hooks"
  (define hooks (extension-hooks test-ext-multi))
  (check-equal? (hash-count hooks) 3)
  (check-not-false (hash-has-key? hooks 'model-request-pre))
  (check-not-false (hash-has-key? hooks 'tool-call))
  (check-not-false (hash-has-key? hooks 'turn-end)))
