#lang racket

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
      (check-not-exn (lambda () (format "~a" e))))))

(run-tests errors-tests 'verbose)
