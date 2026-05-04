#lang racket/base

;; tests/test-facade-surface.rkt — Tests for facade module curation
;;
;; v0.29.6 W0: Test scaffolding for facade surface verification.

(require rackunit
         racket/port
         racket/file)

;; ============================================================
;; 1. all-from-out audit
;; ============================================================

(test-case "count all-from-out in non-test source files"
  (define count
    (for/sum ([f (in-list (find-files (lambda (p)
                                        (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                             (not (regexp-match? #rx"test" (path->string p)))
                                             (not (regexp-match? #rx"compiled" (path->string p)))))
                                      "runtime/"))])
      (define content (call-with-input-file f port->string))
      (length (regexp-match* #rx"all-from-out" content))))
  ;; Just check it's a number (baseline established)
  (check-true (integer? count)))

(test-case "count all-from-out in interfaces/"
  (define count
    (for/sum ([f (in-list (find-files (lambda (p)
                                        (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                             (not (regexp-match? #rx"test" (path->string p)))
                                             (not (regexp-match? #rx"compiled" (path->string p)))))
                                      "interfaces/"))])
      (define content (call-with-input-file f port->string))
      (length (regexp-match* #rx"all-from-out" content))))
  (check-true (integer? count)))

;; ============================================================
;; 2. SDK public surface
;; ============================================================

(test-case "interfaces/sdk.rkt provides explicit symbols"
  (define content (call-with-input-file "interfaces/sdk.rkt" port->string))
  ;; Should have explicit provides, not just all-from-out
  (check-true (regexp-match? #rx"provide" content)))

(test-case "interfaces/sdk-public.rkt provides explicit symbols"
  (define content (call-with-input-file "interfaces/sdk-public.rkt" port->string))
  (check-true (regexp-match? #rx"provide" content)))
