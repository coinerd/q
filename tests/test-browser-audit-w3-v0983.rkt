#lang racket

;; tests/test-browser-audit-w3-v0983.rkt — Tests for Wave 3 audit fixes (v0.98.3)
;; DEAD: unused imports removed
;; SEC-03: session count under lock
;; SEC-08: send-command! requires pending-sema

(require rackunit
         (only-in racket/base make-semaphore)
         "../browser/session.rkt"
         "../browser/types.rkt"
         "../browser/adapters/playwright-sidecar.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; SEC-03: browser-session-manager-count is thread-safe
;; ---------------------------------------------------------------------------

(test-case "SEC-03: count is consistent under lock"
  (define mgr (make-browser-session-manager #:max-sessions 5))
  (check-equal? (browser-session-manager-count mgr) 0)
  (browser-session-manager-create! mgr
                                   "s1"
                                   (browser-session-info "s1" 'active 0 0 'ephemeral "/tmp")
                                   #:artifact-dir "/tmp")
  (check-equal? (browser-session-manager-count mgr) 1))

;; ---------------------------------------------------------------------------
;; SEC-08: send-command! requires pending-sema
;; ---------------------------------------------------------------------------

(test-case "SEC-08: send-command! raises when pending-sema is missing"
  (define state (playwright-sidecar-state #f #f #f (make-hash) #f #f (hasheq 'timeout-ms 1000) #f #f))
  (check-exn q-browser-error? (lambda () (send-command! state "ping" (hasheq) #:timeout-ms 1))))
