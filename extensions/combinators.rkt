#lang racket/base

;; extensions/combinators.rkt — HOF combinators for control pattern extraction
;;
;; STABILITY: stable
;;
;; Provides reusable higher-order functions to eliminate repeated control patterns
;; in extensions code, per v3 architecture audit findings 3.2.1, 3.9.1, 3.9.4.
;;
;; Combinators:
;;   - with-timeout: Wrap computation with timeout + thread management
;;   - with-error-policy: Wrap computation with criticality-based error policy
;;   - with-hook-validation: Validate hook results + handle violations

(require racket/contract
         racket/async-channel
         "api.rkt"
         "context.rkt"
         "../util/hook-types.rkt")

;; Violation callback — called when a hook handler returns an invalid action.
;; Receives (ext-name hook-point action schema-version).
;; Default: #f (no callback). Set to a procedure to enable structured violation reporting.
(define current-hook-violation-callback (make-parameter #f))

(provide (contract-out [with-timeout
                        (->* (exact-positive-integer? (-> any/c))
                             (#:on-timeout (-> any/c) #:on-error (-> exn? any/c))
                             any/c)]
                       [with-error-policy (-> boolean? (-> any/c) (-> any/c) any/c)]
                       [with-hook-validation
                        (-> (or/c symbol? string?) symbol? any/c (-> hook-result?) hook-result?)])
         current-hook-violation-callback)

;; ============================================================
;; with-timeout combinator
;; ============================================================

;; Runs `thunk` with a timeout of `timeout-ms` milliseconds.
;; Returns the result of thunk, or calls on-timeout/on-error handlers.
;; Properly kills thread on timeout to prevent leaks (#447).
;;
;; Parameters:
;;   timeout-ms: milliseconds before timeout (positive integer)
;;   thunk: computation to run
;;   #:on-timeout: called when timeout occurs (default: returns #f)
;;   #:on-error: called when exception occurs (default: re-raises)
;;
;; Returns:
;;   Result of thunk, or on-timeout return, or on-error return.
(define (with-timeout timeout-ms
                      thunk
                      #:on-timeout [on-timeout (lambda () #f)]
                      #:on-error [on-error (lambda (e) (raise e))])
  (define chan (make-channel))
  (define thd
    (thread (lambda ()
              (channel-put chan
                           (with-handlers ([exn:fail? (lambda (e) (cons 'error e))])
                             (cons 'ok (thunk)))))))
  (define maybe (sync/timeout (/ timeout-ms 1000.0) chan))
  (unless maybe
    (kill-thread thd)) ; Prevent thread leak
  (cond
    [(not maybe) (on-timeout)]
    [(eq? (car maybe) 'error) (on-error (cdr maybe))]
    [else (cdr maybe)]))

;; ============================================================
;; with-error-policy combinator
;; ============================================================

;; Wraps computation with criticality-based error policy.
;; Critical hooks default to 'block on error (safety-first).
;; Advisory hooks default to 'pass on error (liveness-first).
;;
;; Parameters:
;;   critical?: #t if hook is critical
;;   thunk: computation to run
;;   error-default-thunk: thunk called to get default value on error
;;
;; Returns:
;;   Result of thunk, or result of error-default-thunk on exception.
(define (with-error-policy critical? thunk error-default-thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-debug "hook handler failed: ~a" (exn-message e))
                               (error-default-thunk))])
    (thunk)))

;; ============================================================
;; with-hook-validation combinator
;; ============================================================

;; Validates hook result and handles violations.
;; Returns valid hook-result, or default on violation.
;;
;; Parameters:
;;   ext-name: extension name (for logging)
;;   hook-point: hook point symbol
;;   raw-result: result from handler (any value)
;;   default-result: thunk that returns default hook-result on violation
;;
;; Returns:
;;   Valid hook-result? value.
(define (with-hook-validation ext-name hook-point raw-result default-result)
  (if (hook-result? raw-result)
      ;; Validate action is valid for this hook-point
      (let ([valid? (validate-hook-result hook-point raw-result)])
        (if valid?
            raw-result
            ;; Invalid action — log violation
            (begin
              (log-warning "Hook violation: ~a returned invalid action '~a' for ~a (schema v~a)"
                           ext-name
                           (hook-result-action raw-result)
                           hook-point
                           (hook-schema-version))
              (let ([cb (current-hook-violation-callback)])
                (when cb
                  (cb ext-name hook-point (hook-result-action raw-result) (hook-schema-version))))
              (default-result))))
      ;; Non-hook-result returned
      (begin
        (log-warning "Hook handler ~a for ~a returned non-hook-result: ~v"
                     ext-name
                     hook-point
                     raw-result)
        (default-result))))

;; ============================================================
;; Internal: hook validation (re-exported from hooks.rkt context)
;; ============================================================

;; Validate that hook-result action is valid for hook-point.
;; Returns #t if valid, #f otherwise.
(define (validate-hook-result hook-point result)
  (member (hook-result-action result) '(pass amend block)))
