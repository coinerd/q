#lang racket

;; tests/test-browser-playwright-adapter.rkt — Playwright sidecar adapter tests

(require rackunit
         "../browser/adapters/playwright-sidecar.rkt"
         "../browser/types.rkt"
         "../browser/adapter.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (find-project-root)
  (define candidate (build-path ".."))
  (if (file-exists? (build-path candidate "main.rkt"))
      (simplify-path candidate)
      (build-path ".")))

(define sidecar-js
  (let ([p (build-path (find-project-root) "sidecars" "playwright" "q-playwright-sidecar.js")])
    (if (file-exists? p) p #f)))

;; ---------------------------------------------------------------------------
;; Unit tests (no sidecar needed)
;; ---------------------------------------------------------------------------

(test-case "playwright-sidecar-state struct is transparent"
  (define st (playwright-sidecar-state #f #f #f (make-hash) #f #f (hasheq) #f))
  (check-true (playwright-sidecar-state? st))
  (check-false (playwright-sidecar-state-process st)))

(test-case "uuid-string produces valid-looking UUID"
  (define u (uuid-string))
  (check-true (string? u))
  (check-equal? (string-length u) 36)
  (check-equal? (string-ref u 8) #\-)
  (check-equal? (string-ref u 13) #\-))

(test-case "uuid-string produces unique values"
  (define u1 (uuid-string))
  (define u2 (uuid-string))
  (check-not-equal? u1 u2))

(test-case "make-playwright-adapter is a procedure"
  (check-true (procedure? make-playwright-adapter)))

;; ---------------------------------------------------------------------------
;; Integration tests (skipped if no Node.js)
;; ---------------------------------------------------------------------------

(define node-available?
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define-values (sp out in)
      (subprocess #f #f #f "node" "--version"))
    (define status (subprocess-status sp))
    (close-input-port out)
    (close-output-port in)
    (eq? status 0)))

(unless node-available?
  (displayln "SKIP: Node.js not available — skipping Playwright integration tests"))

(when (and node-available? sidecar-js)
  (test-case "sidecar launches and responds to ping"
    (define state (launch-sidecar! (path->string sidecar-js)
                                   #:timeout-ms 5000))
    (check-true (playwright-sidecar-state? state))
    (define result (send-command! state "ping" (hasheq)))
    (check-equal? (hash-ref result 'status) "ok")
    (shutdown-sidecar! state #:timeout-ms 3000))

  (test-case "sidecar handles unknown command"
    (define state (launch-sidecar! (path->string sidecar-js)
                                   #:timeout-ms 5000))
    (check-exn exn:fail?
               (lambda ()
                 (send-command! state "fly" (hasheq) #:timeout-ms 3000)))
    (shutdown-sidecar! state #:timeout-ms 3000))

  (test-case "make-playwright-adapter creates valid adapter"
    (define adapter (make-playwright-adapter (path->string sidecar-js)
                                             #:timeout-ms 5000))
    (check-true (browser-adapter? adapter))))
