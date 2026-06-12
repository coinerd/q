#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-state-types.rkt — Regression tests for state-types module (AXIS1-F05)
;; STABILITY: evolving
;;
;; Tests render cache helpers, streaming update helpers, entry helpers,
;; and backward-compatible accessors. These functions all operate on
;; ui-state / streaming-state / cache-state structs.

(require rackunit
         rackunit/text-ui
         (only-in "../tui/state-types.rkt"
                  ;; Constructor
                  initial-ui-state
                  ;; Entry helpers
                  make-entry
                  make-system-entry
                  make-error-entry
                  assign-entry-id
                  next-entry-id
                  ;; Structs
                  ui-state?
                  ui-state-next-entry-id
                  ui-state-streaming
                  ui-state-cache
                  streaming-state?
                  streaming-state-busy?
                  streaming-state-streaming-text
                  streaming-state-streaming-thinking
                  streaming-state
                  cache-state?
                  cache-state-entries
                  ;; Render cache
                  rendered-cache-ref
                  rendered-cache-set
                  rendered-cache-clear
                  rendered-cache-invalidate-entry
                  rendered-cache-width-valid?
                  rendered-cache-set-width
                  ui-state-rendered-cache
                  ui-state-rendered-cache-width
                  ;; Streaming accessors
                  ui-state-busy?
                  ui-state-status-message
                  ui-state-streaming-text
                  ui-state-streaming-thinking
                  ui-state-streaming-phase
                  ui-state-busy-since
                  ;; Streaming updates
                  update-streaming
                  set-busy
                  set-streaming-text
                  set-streaming-thinking
                  set-streaming-phase
                  set-busy-since
                  clear-streaming
                  set-status-message
                  set-pending-tool-name
                  ;; String helpers
                  extract-arg-summary))

(define suite
  (test-suite "State-types regression (AXIS1-F05)"

    ;; ── initial-ui-state ──
    (test-case "initial-ui-state: creates valid state with defaults"
      (define s (initial-ui-state))
      (check-true (ui-state? s))
      (check-false (ui-state-busy? s))
      (check-equal? (ui-state-streaming-phase s) 'idle)
      (check-equal? (next-entry-id s) 0)
      (check-true (cache-state? (ui-state-cache s))))

    (test-case "initial-ui-state: accepts keyword args"
      (define s (initial-ui-state #:session-id "test" #:model-name "gpt-4" #:mode 'single))
      (check-true (ui-state? s)))

    ;; ── Entry helpers ──
    (test-case "make-entry: creates transcript-entry with #f id"
      (define e (make-entry 'user "hello" 1000 (hash)))
      (check-equal? (transcript-entry-kind e) 'user)
      (check-equal? (transcript-entry-text e) "hello")
      (check-false (transcript-entry-id e)))

    (test-case "make-system-entry: creates system entry"
      (define e (make-system-entry "connected"))
      (check-equal? (transcript-entry-kind e) 'system))

    (test-case "make-error-entry: creates error entry"
      (define e (make-error-entry "oops"))
      (check-equal? (transcript-entry-kind e) 'error))

    (test-case "assign-entry-id: assigns and increments"
      (define s (initial-ui-state))
      (define e (make-entry 'user "hi" 0 (hash)))
      (define-values (e2 s2) (assign-entry-id e s))
      (check-equal? (transcript-entry-id e2) 0)
      (check-equal? (next-entry-id s2) 1))

    ;; ── Render cache ──
    (test-case "rendered-cache-ref: returns #f for missing entry"
      (define s (initial-ui-state))
      (check-false (rendered-cache-ref s 0)))

    (test-case "rendered-cache-set/ref roundtrip"
      (define s (initial-ui-state))
      (define lines (list (list (cons 'text "hello"))))
      (define s2 (rendered-cache-set s 0 lines))
      (check-equal? (rendered-cache-ref s2 0) lines))

    (test-case "rendered-cache-clear empties cache"
      (define s (initial-ui-state))
      (define s2 (rendered-cache-set s 0 (list (list (cons 'text "x")))))
      (define s3 (rendered-cache-clear s2))
      (check-false (rendered-cache-ref s3 0)))

    (test-case "rendered-cache-invalidate-entry removes specific entry"
      (define s (initial-ui-state))
      (define s2 (rendered-cache-set s 0 (list (list (cons 'text "a")))))
      (define s3 (rendered-cache-set s2 1 (list (list (cons 'text "b")))))
      (define s4 (rendered-cache-invalidate-entry s3 0))
      (check-false (rendered-cache-ref s4 0))
      (check-not-false (rendered-cache-ref s4 1)))

    (test-case "rendered-cache-width-valid?: initial state has #f width"
      (define s (initial-ui-state))
      (check-false (rendered-cache-width-valid? s 80)))

    (test-case "rendered-cache-set-width then width-valid?"
      (define s (initial-ui-state))
      (define s2 (rendered-cache-set-width s 80))
      (check-true (rendered-cache-width-valid? s2 80))
      (check-false (rendered-cache-width-valid? s2 40)))

    (test-case "ui-state-rendered-cache returns entries hash"
      (define s (initial-ui-state))
      (check-equal? (ui-state-rendered-cache s) (hash)))

    ;; ── Streaming accessors ──
    (test-case "ui-state-busy?: initial state is not busy"
      (define s (initial-ui-state))
      (check-false (ui-state-busy? s)))

    (test-case "ui-state-streaming-text: initial is #f"
      (define s (initial-ui-state))
      (check-false (ui-state-streaming-text s)))

    ;; ── Streaming updates ──
    (test-case "set-busy: marks busy and sets busy-since"
      (define s (initial-ui-state))
      (define s2 (set-busy s #t))
      (check-true (ui-state-busy? s2))
      (check-not-false (ui-state-busy-since s2)))

    (test-case "set-busy: clears busy-since when clearing busy"
      (define s (initial-ui-state))
      (define s2 (set-busy s #t))
      (define s3 (set-busy s2 #f))
      (check-false (ui-state-busy? s3))
      (check-false (ui-state-busy-since s3)))

    (test-case "set-streaming-text updates streaming text"
      (define s (initial-ui-state))
      (define s2 (set-streaming-text s "hello"))
      (check-equal? (ui-state-streaming-text s2) "hello"))

    (test-case "clear-streaming clears text and thinking"
      (define s (initial-ui-state))
      (define s2 (set-streaming-text s "text"))
      (define s3 (set-streaming-thinking s2 "think"))
      (define s4 (clear-streaming s3))
      (check-false (ui-state-streaming-text s4))
      (check-false (ui-state-streaming-thinking s4)))

    (test-case "set-streaming-phase updates phase"
      (define s (initial-ui-state))
      (define s2 (set-streaming-phase s 'thinking))
      (check-equal? (ui-state-streaming-phase s2) 'thinking))

    (test-case "update-streaming: custom update function"
      (define s (initial-ui-state))
      (define s2 (update-streaming s (lambda (st) (struct-copy streaming-state st [busy? #t]))))
      (check-true (ui-state-busy? s2)))

    ;; ── extract-arg-summary ──
    (test-case "extract-arg-summary: hash with content key"
      (check-true (string? (extract-arg-summary (hash 'content "hello world")))))

    (test-case "extract-arg-summary: hash with query key"
      (check-true (string? (extract-arg-summary (hash 'query "find me")))))

    (test-case "extract-arg-summary: empty hash"
      (check-true (string? (extract-arg-summary (hash)))))

    (test-case "extract-arg-summary: string input"
      (check-true (string? (extract-arg-summary "plain text"))))))

;; ── Need transcript-entry accessors ──
(require (only-in "../tui/state-types.rkt"
                  transcript-entry-kind
                  transcript-entry-text
                  transcript-entry-id))

(run-tests suite)
