#lang racket/base

;; tests/tui/state-assertions.rkt — Reusable assertion functions for TUI state
;;
;; Provides composable predicates and assertion helpers for verifying
;; ui-state properties in tests. Used by error-scenarios.rkt and
;; individual test files.

(require rackunit
         racket/list
         racket/string
         "../../tui/state.rkt")

(provide assert-idle?
         assert-error-displayed?
         assert-error-type?
         assert-retry-hint?
         assert-retry-count?
         assert-tool-complete?
         assert-no-orphan-streaming?
         assert-recovery-hint?
         assert-transcript-contains?
         assert-state-clean?
         ;; Predicates (return #t/#f, don't raise)
         state-idle?
         state-has-error?
         state-has-retry?
         state-has-recovery-hint?
         transcript-contains-text?)

;; ============================================================
;; Predicates (pure, return #t/#f)
;; ============================================================

;; State is idle: not busy, not streaming, no pending tool
(define (state-idle? state)
  (and (not (ui-state-busy? state))
       (not (ui-state-streaming-text state))
       (not (ui-state-pending-tool-name state))))

;; Error entry exists in transcript
(define (state-has-error? state)
  (for/or ([e (in-list (ui-state-transcript state))])
    (eq? (transcript-entry-kind e) 'error)))

;; Retry entry exists in transcript
(define (state-has-retry? state)
  (for/or ([e (in-list (ui-state-transcript state))])
    (and (eq? (transcript-entry-kind e) 'system)
         (string-contains? (transcript-entry-text e) "retry"))))

;; Recovery hint exists (system entry with /retry, /compact, or config.json)
(define (state-has-recovery-hint? state)
  (for/or ([e (in-list (ui-state-transcript state))])
    (and (eq? (transcript-entry-kind e) 'system)
         (or (string-contains? (transcript-entry-text e) "/retry")
             (string-contains? (transcript-entry-text e) "/compact")
             (string-contains? (transcript-entry-text e) "config.json")))))

;; Transcript contains given text
(define (transcript-contains-text? state text)
  (for/or ([e (in-list (ui-state-transcript state))])
    (string-contains? (transcript-entry-text e) text)))

;; ============================================================
;; Assertion functions (raise on failure)
;; ============================================================

(define (assert-idle? state [msg "state should be idle"])
  (check-true (state-idle? state) msg))

(define (assert-error-displayed? state [msg "error should be displayed"])
  (check-true (state-has-error? state) msg))

(define (assert-error-type? state expected-type [msg "error type should match"])
  (define entries
    (filter (lambda (e) (eq? (transcript-entry-kind e) 'error)) (ui-state-transcript state)))
  (check-true (pair? entries) (string-append msg " (no error entries)")))

(define (assert-retry-hint? state [msg "retry hint should be visible"])
  (check-true (state-has-retry? state) msg))

(define (assert-retry-count? state expected-count [msg "retry count should match"])
  (define retry-entries
    (filter (lambda (e)
              (and (eq? (transcript-entry-kind e) 'system)
                   (regexp-match? #rx"attempt [0-9]+" (transcript-entry-text e))))
            (ui-state-transcript state)))
  (check-equal? (length retry-entries) expected-count msg))

(define (assert-tool-complete? state [msg "tool execution should be complete"])
  (define entries (ui-state-transcript state))
  (define tool-starts (filter (lambda (e) (eq? (transcript-entry-kind e) 'tool-start)) entries))
  (define tool-ends (filter (lambda (e) (eq? (transcript-entry-kind e) 'tool-result)) entries))
  (check-equal? (length tool-starts) (length tool-ends) msg))

(define (assert-no-orphan-streaming? state [msg "streaming text should be cleared"])
  (check-false (ui-state-streaming-text state) msg))

(define (assert-recovery-hint? state [msg "recovery hint should be visible"])
  (check-true (state-has-recovery-hint? state) msg))

(define (assert-transcript-contains? state text [msg "transcript should contain text"])
  (check-true (transcript-contains-text? state text) msg))

(define (assert-state-clean? state [msg "state should be clean"])
  (check-true (state-idle? state) (string-append msg " (not idle)"))
  (check-false (ui-state-streaming-text state) (string-append msg " (streaming text left)"))
  (check-false (ui-state-streaming-thinking state) (string-append msg " (streaming thinking left)"))
  (check-false (ui-state-pending-tool-name state) (string-append msg " (pending tool left)")))
