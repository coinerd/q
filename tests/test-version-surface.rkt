#lang racket

;; @speed fast
;; @suite default

;; test-version-surface.rkt — Tests for centralized version surface registry
;; W2 (#8415): Consolidates version-parsing logic from 4 scripts.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/runtime-path)

(define-runtime-path script-path "../scripts/version-surface.rkt")
(define-runtime-path q-root "..")

(define (vs-ref sym)
  (dynamic-require script-path sym))

;; ---------------------------------------------------------------------------
;; Content-level parsing tests
;; ---------------------------------------------------------------------------

(define-test-suite version-surface-tests
                   (test-case "parse-q-version-from-content: basic"
                     (define parse (vs-ref 'parse-q-version-from-content))
                     (check-equal? (parse "(define q-version \"1.2.3\")") "1.2.3"))
                   (test-case "parse-q-version-from-content: typed racket multiline"
                     (define parse (vs-ref 'parse-q-version-from-content))
                     (define content "#lang typed/racket\n(define q-version : String\n  \"0.99.36\")")
                     (check-equal? (parse content) "0.99.36"))
                   (test-case "parse-q-version-from-content: returns #f when not found"
                     (define parse (vs-ref 'parse-q-version-from-content))
                     (check-false (parse "(define other-thing \"1.0\")")))
                   (test-case "parse-info-version-from-content: basic"
                     (define parse (vs-ref 'parse-info-version-from-content))
                     (check-equal? (parse "(define version \"2.3.4\")") "2.3.4"))
                   (test-case "parse-info-version-from-content: returns #f when not found"
                     (define parse (vs-ref 'parse-info-version-from-content))
                     (check-false (parse "(define q-version \"2.3.4\")")))
                   ;; ---------------------------------------------------------------------------
                   ;; Version comparison tests
                   ;; ---------------------------------------------------------------------------
                   (test-case "version<=?: basic comparisons"
                     (define v<=? (vs-ref 'version<=?))
                     (check-true (v<=? "0.55.0" "0.56.0"))
                     (check-true (v<=? "0.55.0" "0.55.0"))
                     (check-false (v<=? "0.56.0" "0.55.0"))
                     (check-true (v<=? "0.58.0" "1.0.0"))
                     (check-true (v<=? "0.1.0" "0.2.0")))
                   (test-case "version<?: strict ordering"
                     (define v<? (vs-ref 'version<?))
                     (check-true (v<? "0.55.0" "0.56.0"))
                     (check-false (v<? "0.55.0" "0.55.0"))
                     (check-false (v<? "0.56.0" "0.55.0")))
                   (test-case "version=?: equality"
                     (define v=? (vs-ref 'version=?))
                     (check-true (v=? "1.2.3" "1.2.3"))
                     (check-false (v=? "1.2.3" "1.2.4")))
                   ;; ---------------------------------------------------------------------------
                   ;; Component parsing tests
                   ;; ---------------------------------------------------------------------------
                   (test-case "parse-version-components: basic"
                     (define pvc (vs-ref 'parse-version-components))
                     (check-equal? (pvc "1.2.3") '(1 2 3))
                     (check-equal? (pvc "0.0.0") '(0 0 0)))
                   ;; ---------------------------------------------------------------------------
                   ;; Version formatting tests
                   ;; ---------------------------------------------------------------------------
                   (test-case "version->string: round-trip"
                     (define v->s (vs-ref 'version->string))
                     (check-equal? (v->s '(1 2 3)) "1.2.3"))
                   ;; ---------------------------------------------------------------------------
                   ;; File-level I/O tests
                   ;; ---------------------------------------------------------------------------
                   (test-case "read-canonical-version: reads from actual version.rkt"
                     (define rcv (vs-ref 'read-canonical-version))
                     (define v (rcv (path->string q-root)))
                     (check-true (and (regexp-match? #rx"[0-9]+\\.[0-9]+\\.[0-9]+" v) #t)))
                   (test-case "read-canonical-version/strict: returns string for valid file"
                     (define rcvs (vs-ref 'read-canonical-version/strict))
                     (define v (rcvs (path->string q-root)))
                     (check-true (string? v))
                     (check-true (and (regexp-match? #rx"[0-9]+\\.[0-9]+\\.[0-9]+" v) #t)))
                   (test-case "read-canonical-version: returns fallback for nonexistent dir"
                     (define rcv (vs-ref 'read-canonical-version))
                     (define v (rcv "/nonexistent/path"))
                     (check-equal? v "0.0.0"))
                   (test-case "read-canonical-version/strict: returns #f for nonexistent dir"
                     (define rcvs (vs-ref 'read-canonical-version/strict))
                     (define v (rcvs "/nonexistent/path"))
                     (check-false v))
                   ;; v0.99.37 W5: Contract enforcement tests
                   (test-case "contract: parse-q-version-from-content rejects non-string"
                     (define parse (vs-ref 'parse-q-version-from-content))
                     (check-exn exn:fail:contract? (λ () (parse 42))))
                   (test-case "contract: version->string rejects non-list"
                     (define v->s (vs-ref 'version->string))
                     (check-exn exn:fail:contract? (λ () (v->s "not-a-list")))))

(module+ test
  (run-tests version-surface-tests))

(module+ main
  (run-tests version-surface-tests))
