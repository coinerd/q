#lang racket

;; tests/test-sdk-gsd.rkt — tests for v0.20.5 W2: GSD Convenience API
;;
;; Covers:
;;   - q:plan dispatches /plan through extension registry
;;   - q:go dispatches /go through extension registry
;;   - q:gsd-status returns snapshot or 'no-active-session
;;   - q:reset-gsd! resets all GSD state
;;   - Edge cases: no extension registry, no submit text, no session

(require rackunit
         racket/file
         "../interfaces/sdk.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../extensions/gsd-planning.rkt"
         "../agent/event-bus.rkt"
         "helpers/mock-provider.rkt"
         "helpers/temp-fs.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-gsd-runtime #:with-ext-reg? [with-ext? #f]
                          #:with-session? [with-sess? #f])
  (define tmp (make-temporary-file "/tmp/sdk-gsd-test-~a" 'directory))
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text"
                                                     'text "done"))))))
  (define ext-reg (and with-ext? (make-extension-registry)))
  (when with-ext?
    (register-extension! ext-reg the-extension))
  (define rt (make-runtime #:provider prov
                           #:session-dir tmp
                           #:extension-registry ext-reg
                           #:register-default-tools? #f))
  (define opened (if with-sess? (open-session rt) rt))
  (values opened tmp))

(define (cleanup-gsd! tmp)
  (reset-all-gsd-state!)
  (delete-directory/files tmp #:must-exist? #f))

;; ============================================================
;; Tests
;; ============================================================

(test-case "W2: q:gsd-status returns 'no-active-session when inactive"
  (reset-all-gsd-state!)
  (check-equal? (q:gsd-status) 'no-active-session))

(test-case "W2: q:reset-gsd! clears all state"
  (reset-all-gsd-state!)
  ;; Set some state
  (set-gsd-mode! 'planning)
  (check-equal? (gsd-mode) 'planning)
  ;; Reset via SDK
  (q:reset-gsd!)
  (check-false (gsd-mode)))

(test-case "W2: q:plan returns 'no-extension-registry without ext-reg"
  (define-values (rt tmp) (make-gsd-runtime))
  (define-values (rt2 result) (q:plan rt "test task"))
  (check-equal? result 'no-extension-registry)
  (cleanup-gsd! tmp))

(test-case "W2: q:go returns 'no-extension-registry without ext-reg"
  (define-values (rt tmp) (make-gsd-runtime))
  (define-values (rt2 result) (q:go rt))
  (check-equal? result 'no-extension-registry)
  (cleanup-gsd! tmp))

(test-case "W2: q:plan dispatches through extension and returns submit text (no session)"
  (reset-all-gsd-state!)
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t))
  (define-values (rt2 result) (q:plan rt "build a foo"))
  ;; The GSD extension should return a hook-amend with submit text
  ;; Since no session, we get back the submit text string
  (check-pred string? result "q:plan should return submit text string when no session")
  (check-true (string-contains? result "build a foo")
              "submit text should contain the task")
  (cleanup-gsd! tmp))

(test-case "W2: q:go dispatches through extension and returns submit text (no session)"
  (reset-all-gsd-state!)
  ;; Need a PLAN.md for /go to work — write one via planning-write
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t))
  ;; First write a plan via planning-write (directly)
  (define plan-dir (build-path tmp ".planning"))
  (make-directory* plan-dir)
  (call-with-output-file (build-path plan-dir "PLAN.md")
    (lambda (out) (display "## Wave 0: Test\n- Do something\n" out))
    #:exists 'truncate)
  ;; Pin the planning dir so the extension finds it
  (set-pinned-planning-dir! tmp)
  (define-values (rt2 result) (q:go rt))
  ;; /go should find the plan and return submit text
  (check-pred string? result "q:go should return submit text when no session")
  (check-true (string-contains? result "IMPLEMENT")
              "submit text should contain implementation instructions")
  (cleanup-gsd! tmp))

(test-case "W2: q:plan with session runs prompt through provider"
  (reset-all-gsd-state!)
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t #:with-session? #t))
  (define-values (rt2 result) (q:plan rt "build a bar"))
  ;; Should have run the prompt through the mock provider
  (check-not-equal? result 'no-extension-registry)
  (check-not-equal? result 'no-active-session)
  (cleanup-gsd! tmp))

(test-case "W2: q:go with session and plan runs prompt through provider"
  (reset-all-gsd-state!)
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t #:with-session? #t))
  ;; Write a plan
  (define plan-dir (build-path tmp ".planning"))
  (make-directory* plan-dir)
  (call-with-output-file (build-path plan-dir "PLAN.md")
    (lambda (out) (display "## Wave 0: Test\n- Do something\n" out))
    #:exists 'truncate)
  (set-pinned-planning-dir! tmp)
  (define-values (rt2 result) (q:go rt))
  (check-not-equal? result 'no-extension-registry)
  (check-not-equal? result 'no-active-session)
  (cleanup-gsd! tmp))
