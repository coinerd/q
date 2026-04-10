#lang racket

;; tests/test-extension-harness.rkt — tests for the extension test harness
;;
;; Covers:
;;   1. with-extension-test — isolated registry creation
;;   2. inject-hook-event — single hook dispatch
;;   3. capture-hook-results — multi-payload collection
;;   4. check-hook-passed / check-hook-amended / check-hook-blocked — assertions
;;   5. check-hook-payload — payload assertion
;;   6. make-test-extension — convenience constructor
;;   7. Registry isolation between tests

(require rackunit
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../extensions/test-harness.rkt")

;; ============================================================
;; 1. with-extension-test creates an isolated registry
;; ============================================================

(test-case "with-extension-test binds test-registry and runs body"
  (with-extension-test (make-test-extension #:name "iso-ext")
    (check-pred extension-registry? test-registry)
    (check-not-false (lookup-extension test-registry "iso-ext"))))

(test-case "with-extension-test body can reference test-registry"
  (with-extension-test (make-test-extension #:name "ref-check")
    (define ext (lookup-extension test-registry "ref-check"))
    (check-equal? (extension-name ext) "ref-check")))

;; ============================================================
;; 2. inject-hook-event returns hook-result
;; ============================================================

(test-case "inject-hook-event returns hook-result from dispatch"
  (with-extension-test
      (make-test-extension #:name "inj-ext"
                           #:hooks (hasheq 'tool-call (λ (p) (hook-amend (string-append p "!!")))))
    (define result (inject-hook-event test-registry 'tool-call "hello"))
    (check-pred hook-result? result)
    (check-equal? (hook-result-action result) 'amend)
    (check-equal? (hook-result-payload result) "hello!!")))

(test-case "inject-hook-event returns pass when no handler registered"
  (with-extension-test
      (make-test-extension #:name "no-handler")
    (define result (inject-hook-event test-registry 'nonexistent-point 'data))
    (check-equal? (hook-result-action result) 'pass)))

;; ============================================================
;; 3. check-hook-passed passes for pass result
;; ============================================================

(test-case "check-hook-passed succeeds on pass result"
  (with-extension-test
      (make-test-extension #:name "pass-ext"
                           #:hooks (hasheq 'context (λ (p) (hook-pass p))))
    (define result (inject-hook-event test-registry 'context "payload"))
    (check-hook-passed result)))

;; ============================================================
;; 4. check-hook-amended passes for amend result
;; ============================================================

(test-case "check-hook-amended succeeds on amend result"
  (with-extension-test
      (make-test-extension #:name "amend-ext"
                           #:hooks (hasheq 'context (λ (p) (hook-amend "changed"))))
    (define result (inject-hook-event test-registry 'context "original"))
    (check-hook-amended result)))

;; ============================================================
;; 5. check-hook-blocked passes for block result
;; ============================================================

(test-case "check-hook-blocked succeeds on block result"
  (with-extension-test
      (make-test-extension #:name "block-ext"
                           #:hooks (hasheq 'tool-call (λ (p) (hook-block "denied"))))
    (define result (inject-hook-event test-registry 'tool-call "try"))
    (check-hook-blocked result)))

;; ============================================================
;; 6. check-hook-payload verifies payload
;; ============================================================

(test-case "check-hook-payload succeeds with correct payload"
  (with-extension-test
      (make-test-extension #:name "payload-ext"
                           #:hooks (hasheq 'context (λ (p) (hook-amend (list 'modified p)))))
    (define result (inject-hook-event test-registry 'context "data"))
    (check-hook-payload result '(modified "data"))))

;; ============================================================
;; 7. make-test-extension creates valid extension
;; ============================================================

(test-case "make-test-extension creates extension with defaults"
  (define ext (make-test-extension #:name "test-ext"))
  (check-pred extension? ext)
  (check-equal? (extension-name ext) "test-ext")
  (check-equal? (extension-version ext) "0.0.1")
  (check-equal? (extension-api-version ext) "1"))

(test-case "make-test-extension with custom hooks"
  (define ext (make-test-extension #:name "hooked"
                                   #:hooks (hasheq 'turn-start (λ (p) (hook-pass p)))))
  (check-equal? (hash-count (extension-hooks ext)) 1)
  (define result ((hash-ref (extension-hooks ext) 'turn-start) 'x))
  (check-equal? (hook-result-action result) 'pass))

;; ============================================================
;; 8. capture-hook-results collects multiple results
;; ============================================================

(test-case "capture-hook-results collects results from multiple payloads"
  (with-extension-test
      (make-test-extension #:name "cap-ext"
                           #:hooks (hasheq 'context (λ (p) (hook-amend (string-append p "!")))))
    (define results (capture-hook-results test-registry 'context (list "a" "b" "c")))
    (check-equal? (length results) 3)
    (check-equal? (hook-result-payload (car results)) "a!")
    (check-equal? (hook-result-payload (cadr results)) "b!")
    (check-equal? (hook-result-payload (caddr results)) "c!")))

(test-case "capture-hook-results returns empty list for no payloads"
  (with-extension-test
      (make-test-extension #:name "empty-cap")
    (define results (capture-hook-results test-registry 'context '()))
    (check-equal? results '())))

;; ============================================================
;; 9. Registry isolation — two tests don't interfere
;; ============================================================

(test-case "registry isolation: first test's extensions don't leak"
  (with-extension-test
      (make-test-extension #:name "isolated-a")
    (check-equal? (length (list-extensions test-registry)) 1)
    (check-not-false (lookup-extension test-registry "isolated-a"))))

(test-case "registry isolation: second test has its own registry"
  (with-extension-test
      (make-test-extension #:name "isolated-b")
    ;; "isolated-a" should NOT be in this registry
    (check-false (lookup-extension test-registry "isolated-a"))
    (check-not-false (lookup-extension test-registry "isolated-b"))
    (check-equal? (length (list-extensions test-registry)) 1)))
