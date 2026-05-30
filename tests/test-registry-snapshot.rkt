#lang racket/base
;; BOUNDARY: pure

;; tests/test-registry-snapshot.rkt — with-registry-snapshot tests (F12)

(require rackunit
         rackunit/text-ui
         "../tools/registry.rkt"
         "../tools/tool-struct.rkt")

(define (make-test-tool name)
  (tool name
        (string-append "Test tool " name)
        (hasheq 'type "function" 'function (hasheq 'name name 'parameters (hasheq)))
        (lambda args (void))
        #f
        '()
        #f
        #f
        #f
        #f))

(define snapshot-tests
  (test-suite "registry-snapshot"

    (test-case "snapshot reads registry contents"
      (define reg (make-tool-registry))
      (register-tool! reg (make-test-tool "foo"))
      (register-tool! reg (make-test-tool "bar"))
      (define result (sort (with-registry-snapshot reg (lambda (snap) (hash-keys snap))) string<?))
      (check-not-false (member "foo" result))
      (check-not-false (member "bar" result)))

    (test-case "snapshot is isolated from mutations"
      (define reg (make-tool-registry))
      (register-tool! reg (make-test-tool "before"))
      (define snap-tools
        (sort (with-registry-snapshot reg (lambda (snap) (hash-keys snap))) string<?))
      (register-tool! reg (make-test-tool "after"))
      (define snap-after
        (sort (with-registry-snapshot reg (lambda (snap) (hash-keys snap))) string<?))
      ;; First snapshot should not contain "after"
      (check-false (member "after" snap-tools))
      ;; Second snapshot should contain both
      (check-not-false (member "after" snap-after)))))

(run-tests snapshot-tests)
