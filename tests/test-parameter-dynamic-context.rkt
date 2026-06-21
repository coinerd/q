#lang racket/base

;; @speed fast
;; @suite fast

;; W7 v0.99.37: Dynamic-context tests for parameter ownership semantics.
;; Validates that parameterize properly isolates, nests, and restores parameter
;; values — the contract all parameter ownership patterns depend on.

(require rackunit
         rackunit/text-ui
         racket/runtime-path)

(define-runtime-path audit-path "../scripts/abstraction-audit.rkt")
(define-runtime-path lint-io-path "../scripts/lint-version-io.rkt")

(define audit-cache (make-hash))
(define (audit-ref sym)
  (hash-ref! audit-cache sym (lambda () (dynamic-require audit-path sym))))

(define lint-cache (make-hash))
(define (lint-ref sym)
  (hash-ref! lint-cache sym (lambda () (dynamic-require lint-io-path sym))))

;; ============================================================
;; Suite 1: parameterize isolation (doesn't leak to parent scope)
;; ============================================================

(define-test-suite parameterize-isolation-tests
                   (test-case "parameterize doesn't leak to parent scope"
                     (define test-param (make-parameter 'outer))
                     (parameterize ([test-param 'inner])
                       (check-equal? (test-param) 'inner "inside parameterize sees inner"))
                     (check-equal? (test-param) 'outer "outside parameterize sees outer"))
                   (test-case "multiple parameters isolated independently"
                     (define p1 (make-parameter 1))
                     (define p2 (make-parameter 2))
                     (parameterize ([p1 10])
                       (parameterize ([p2 20])
                         (check-equal? (p1) 10 "p1 unchanged by p2 parameterize")
                         (check-equal? (p2) 20 "p2 sees its own value"))
                       (check-equal? (p2) 2 "p2 restored")))
                   (test-case "parameterize with #f default"
                     (define p (make-parameter #f))
                     (parameterize ([p 'active])
                       (check-equal? (p) 'active))
                     (check-false (p) "restored to #f")))

;; ============================================================
;; Suite 2: parameterize nesting (inner shadows outer)
;; ============================================================

(define-test-suite parameterize-nesting-tests
                   (test-case "inner parameterize shadows outer"
                     (define p (make-parameter 'root))
                     (parameterize ([p 'level1])
                       (check-equal? (p) 'level1)
                       (parameterize ([p 'level2])
                         (check-equal? (p) 'level2 "inner shadows outer"))
                       (check-equal? (p) 'level1 "restored to level1")))
                   (test-case "deep nesting preserves all levels"
                     (define p (make-parameter 0))
                     (parameterize ([p 1])
                       (parameterize ([p 2])
                         (parameterize ([p 3])
                           (check-equal? (p) 3 "deepest level"))
                         (check-equal? (p) 2))
                       (check-equal? (p) 1))
                     (check-equal? (p) 0 "root restored")))

;; ============================================================
;; Suite 3: parameterize restoration (value restored after exit)
;; ============================================================

(define-test-suite parameterize-restoration-tests
                   (test-case "value restored after parameterize exits normally"
                     (define p (make-parameter 'original))
                     (parameterize ([p 'temporary])
                       (void))
                     (check-equal? (p) 'original))
                   (test-case "value restored after parameterize exits via exception"
                     (define p (make-parameter 'original))
                     (with-handlers ([exn:fail? (lambda (_) (void))])
                       (parameterize ([p 'temporary])
                         (error "boom")))
                     (check-equal? (p) 'original "restored even after exception")))

;; ============================================================
;; Suite 4: direct mutation persists (anti-pattern demonstration)
;; ============================================================

(define-test-suite direct-mutation-tests
                   (test-case "direct mutation persists (anti-pattern)"
                     (define p (make-parameter 0))
                     (p 42)
                     (check-equal? (p) 42 "direct mutation persists")
                     ;; Clean up for test isolation
                     (p 0))
                   (test-case "direct mutation inside parameterize overrides the scoped binding"
                     ;; Direct mutation (p v) modifies the CURRENT thread-local binding,
                     ;; which is the parameterize binding when inside parameterize.
                     ;; After parameterize exits, the value reverts to what it was before entry.
                     (define p (make-parameter 'root))
                     (parameterize ([p 'scoped])
                       (p 'mutated)
                       (check-equal? (p) 'mutated "mutation overrides parameterize binding"))
                     (check-equal? (p) 'root "root restored after parameterize exits")))

;; ============================================================
;; Suite 5: thread-locality (parameters are thread-local)
;; ============================================================

(require racket/match)

(define-test-suite thread-locality-tests
                   (test-case "child thread inherits parameterize values"
                     ;; Racket threads inherit the creating thread's parameter values
                     ;; at creation time. This is key for the ownership model.
                     (define p (make-parameter 'main))
                     (define result (box #f))
                     (parameterize ([p 'main-scoped])
                       (define thd
                         (thread (lambda ()
                                   ;; Child thread inherits the parameterized value
                                   (set-box! result (p)))))
                       (thread-wait thd))
                     (check-equal? (unbox result) 'main-scoped "child inherits parent parameterize"))
                   (test-case "child thread parameterize doesn't affect parent"
                     (define p (make-parameter 'parent))
                     (define thd
                       (thread (lambda ()
                                 (parameterize ([p 'child])
                                   (sleep 0.05)))))
                     (sleep 0.01)
                     (check-equal? (p) 'parent "parent unaffected by child parameterize")
                     (thread-wait thd)))

;; ============================================================
;; Suite 6: I/O parameter isolation (real module parameters)
;; ============================================================

(define-test-suite
 io-parameter-isolation-tests
 (test-case "audit-content works without I/O parameter"
   ;; Default I/O parameter should not interfere with pure function
   (define finding ((audit-ref 'audit-content) "test.rkt" "(define x 1)"))
   (check-true (hash? finding) "audit-content returns hash"))
 (test-case "current-audit-file->lines can be mocked"
   (define mock-reader (lambda (path) '("line1" "line2")))
   (define param (audit-ref 'current-audit-file->lines))
   (parameterize ([param mock-reader])
     (define finding ((audit-ref 'audit-module) "mock.rkt"))
     (check-true (hash? finding) "mocked audit-module returns hash")
     (check-equal? (hash-ref finding 'line-count) 2 "2 mock lines")))
 (test-case "lint I/O parameters can be mocked"
   (define mock-exists (lambda (path) #t))
   (define mock-read (lambda (path) "test content"))
   (define exists-param (lint-ref 'current-lint-file-exists?))
   (define read-param (lint-ref 'current-lint-file->string))
   (parameterize ([exists-param mock-exists]
                  [read-param mock-read])
     (check-true ((exists-param) "any") "mocked exists returns #t")
     (check-equal? ((read-param) "any") "test content" "mocked read returns content"))))

;; ============================================================
;; Run all tests
;; ============================================================

(define-test-suite all-parameter-dynamic-context-tests
                   parameterize-isolation-tests
                   parameterize-nesting-tests
                   parameterize-restoration-tests
                   direct-mutation-tests
                   thread-locality-tests
                   io-parameter-isolation-tests)

(module+ test
  (run-tests all-parameter-dynamic-context-tests))

(module+ main
  (run-tests all-parameter-dynamic-context-tests))
