#lang racket

;; BOUNDARY: integration

;; test-errors.rkt — Tests for q/util/errors.rkt domain-specific exception types
;;
;; Note: provider-error tests removed — canonical provider-error is in
;; llm/provider-errors.rkt (tested in tests/test-provider-errors.rkt).

(require rackunit
         rackunit/text-ui
         "../util/errors.rkt")

(define errors-tests
  (test-suite "errors.rkt — domain-specific exception types"

    ;; q-error struct has message and context
    (test-case "q-error has message and context"
      (define e
        (with-handlers ([q-error? identity])
          (raise-q-error "test message" (hasheq 'key 'val))))
      (check-equal? (exn-message e) "test message")
      (check-equal? (hash-ref (q-error-context e) 'key) 'val))

    ;; tool-error has tool-name
    (test-case "tool-error has tool-name"
      (define e
        (with-handlers ([tool-error? identity])
          (raise-tool-error "oops" "firecrawl")))
      (check-equal? (exn-message e) "oops")
      (check-equal? (tool-error-tool-name e) "firecrawl"))

    ;; session-error has session-id
    (test-case "session-error has session-id"
      (define e
        (with-handlers ([session-error? identity])
          (raise-session-error "dead" "sess-123")))
      (check-equal? (exn-message e) "dead")
      (check-equal? (session-error-session-id e) "sess-123"))

    ;; raise-q-error actually raises q-error
    (test-case "raise-q-error raises q-error"
      (check-exn q-error? (lambda () (raise-q-error "boom"))))

    ;; raise-tool-error actually raises tool-error
    (test-case "raise-tool-error raises tool-error"
      (check-exn tool-error? (lambda () (raise-tool-error "boom" "my-tool"))))

    ;; Errors are exn:fail? (catchable with with-handlers)
    (test-case "domain errors are exn:fail?"
      (for ([raise-fn (list (lambda () (raise-q-error "x"))
                            (lambda () (raise-tool-error "x" "t"))
                            (lambda () (raise-session-error "x" "s")))])
        (define e
          (with-handlers ([exn:fail? identity])
            (raise-fn)))
        (check-pred exn:fail? e)))

    ;; Context defaults to empty hash
    (test-case "context defaults to empty hash"
      (define e
        (with-handlers ([q-error? identity])
          (raise-q-error "msg")))
      (check-equal? (q-error-context e) (hash)))

    ;; Errors are transparent structs
    (test-case "errors are transparent structs"
      (define e
        (with-handlers ([q-error? identity])
          (raise-q-error "t" (hasheq))))
      (check-not-exn (lambda () (format "~a" e))))

    ;; ui-error has component
    (test-case "ui-error has component"
      (define e
        (with-handlers ([ui-error? identity])
          (raise-ui-error "render failed" "status-bar")))
      (check-equal? (exn-message e) "render failed")
      (check-equal? (ui-error-component e) "status-bar"))

    ;; extension-error has extension-name and hook-point
    (test-case "extension-error has extension-name and hook-point"
      (define e
        (with-handlers ([extension-error? identity])
          (raise-extension-error "hook crashed" "github" "pre-tool-call")))
      (check-equal? (exn-message e) "hook crashed")
      (check-equal? (extension-error-extension-name e) "github")
      (check-equal? (extension-error-hook-point e) "pre-tool-call"))

    ;; policy-error has policy-name and violation
    (test-case "policy-error has policy-name and violation"
      (define e
        (with-handlers ([policy-error? identity])
          (raise-policy-error "budget exceeded" "tool-budget" "max-turns-10")))
      (check-equal? (exn-message e) "budget exceeded")
      (check-equal? (policy-error-policy-name e) "tool-budget")
      (check-equal? (policy-error-violation e) "max-turns-10"))

    ;; New error types are also exn:fail?
    (test-case "new domain errors are exn:fail?"
      (for ([raise-fn (list (lambda () (raise-ui-error "x" "comp"))
                            (lambda () (raise-extension-error "x" "ext" "hook"))
                            (lambda () (raise-policy-error "x" "pol" "viol")))])
        (define e
          (with-handlers ([exn:fail? identity])
            (raise-fn)))
        (check-pred exn:fail? e)))

    ;; credential-error has backend and details
    (test-case "credential-error has backend and details"
      (define e
        (with-handlers ([credential-error? identity])
          (raise-credential-error "auth failed" "oauth" "token expired")))
      (check-equal? (exn-message e) "auth failed")
      (check-equal? (credential-error-backend e) "oauth")
      (check-equal? (credential-error-details e) "token expired"))

    ;; credential-error defaults details to #f
    (test-case "credential-error defaults details to #f"
      (define e
        (with-handlers ([credential-error? identity])
          (raise-credential-error "no key" "api-key")))
      (check-equal? (credential-error-details e) #f))

    ;; New errors are q-error subtypes
    (test-case "new domain errors are q-error subtypes"
      (check-exn q-error? (lambda () (raise-ui-error "x" "c")))
      (check-exn q-error? (lambda () (raise-extension-error "x" "e" "h")))
      (check-exn q-error? (lambda () (raise-policy-error "x" "p" "v")))
      (check-exn q-error? (lambda () (raise-credential-error "x" "b" "d"))))))

(run-tests errors-tests 'verbose)
