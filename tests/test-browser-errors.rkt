#lang racket

;; tests/test-browser-errors.rkt — Browser error hierarchy tests
;;
;; Tests for q-browser-error branch in util/error/errors.rkt.

(require rackunit
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Struct construction & hierarchy
;; ---------------------------------------------------------------------------

(test-case "q-browser-error construction"
  (define e (q-browser-error "test" (current-continuation-marks) (hash) 'adapter-unavailable))
  (check-true (q-browser-error? e))
  (check-true (q-error? e))
  (check-equal? (q-browser-error-category e) 'adapter-unavailable))

(test-case "q-browser-error inherits from q-error"
  (define e (q-browser-error "msg" (current-continuation-marks) (hash) 'page-load-failed))
  (check-true (exn:fail? e))
  (check-true (q-error? e))
  (check-true (q-browser-error? e)))

(test-case "q-browser-error context preserved"
  (define ctx (hash 'url "https://example.com" 'code 404))
  (define e (q-browser-error "not found" (current-continuation-marks) ctx 'page-load-failed))
  (check-equal? (q-error-context e) ctx))

;; ---------------------------------------------------------------------------
;; raise-browser-error
;; ---------------------------------------------------------------------------

(test-case "raise-browser-error raises q-browser-error"
  (check-exn
   q-browser-error?
   (lambda ()
     (raise-browser-error "test error" 'action-failed))))

(test-case "raise-browser-error with context"
  (check-exn
   (lambda (e)
     (and (q-browser-error? e)
          (equal? (q-browser-error-category e) 'navigation-blocked)
          (equal? (hash-ref (q-error-context e) 'url) "https://blocked.com")))
   (lambda ()
     (raise-browser-error "blocked" 'navigation-blocked
                          (hash 'url "https://blocked.com")))))

;; ---------------------------------------------------------------------------
;; Category predicates (7 categories)
;; ---------------------------------------------------------------------------

(test-case "browser-adapter-unavailable? predicate"
  (check-true (browser-adapter-unavailable?
               (q-browser-error "x" (current-continuation-marks) (hash) 'adapter-unavailable)))
  (check-false (browser-adapter-unavailable?
                (q-browser-error "x" (current-continuation-marks) (hash) 'page-load-failed))))

(test-case "browser-page-load-failed? predicate"
  (check-true (browser-page-load-failed?
               (q-browser-error "x" (current-continuation-marks) (hash) 'page-load-failed))))

(test-case "browser-action-failed? predicate"
  (check-true (browser-action-failed?
               (q-browser-error "x" (current-continuation-marks) (hash) 'action-failed))))

(test-case "browser-navigation-blocked? predicate"
  (check-true (browser-navigation-blocked?
               (q-browser-error "x" (current-continuation-marks) (hash) 'navigation-blocked))))

(test-case "browser-screenshot-failed? predicate"
  (check-true (browser-screenshot-failed?
               (q-browser-error "x" (current-continuation-marks) (hash) 'screenshot-failed))))

(test-case "browser-sidecar-crashed? predicate"
  (check-true (browser-sidecar-crashed?
               (q-browser-error "x" (current-continuation-marks) (hash) 'sidecar-crashed))))

(test-case "browser-session-expired? predicate"
  (check-true (browser-session-expired?
               (q-browser-error "x" (current-continuation-marks) (hash) 'session-expired))))

(test-case "predicates reject other q-error types"
  (check-false (browser-adapter-unavailable?
                (q-error "x" (current-continuation-marks) (hash)))))

(test-case "browser-url-blocked-error? predicate"
  (check-true (browser-url-blocked-error?
               (q-browser-error "blocked" (current-continuation-marks) (hash) 'url-blocked)))
  (check-false (browser-url-blocked-error?
                (q-browser-error "other" (current-continuation-marks) (hash) 'navigation-blocked))))

(test-case "browser-adapter-error? predicate"
  (check-true (browser-adapter-error?
               (q-browser-error "fail" (current-continuation-marks) (hash) 'adapter-error)))
  (check-false (browser-adapter-error?
                (q-browser-error "other" (current-continuation-marks) (hash) 'sidecar-crashed))))
