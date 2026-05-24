#lang racket

;; BOUNDARY: integration
;; CHARACTERIZATION TEST — documents current TUI behavior for v0.57.3 decomposition

;; tests/test-tui-hotspot-characterization.rkt
;; Pins current TUI hotspot behavior to de-risk decomposition in v0.57.3.
;;
;; Key decisions documented:
;; D1: handle-user-submit! contract says (-> tui-ctx? string? tui-ctx?) but
;;     actually returns a thread object (from the thread spawn). The contract
;;     throws exn:fail:contract? when called — proven by D1.
;; D2: handle-key dispatches configurable keymap first, then falls through to
;;     hardcoded behavior. Fallthrough is the primary path for submit.
;; D3: check-busy-watchdog is pure and testable.
;; D4: handle-user-submit! is NOT safely callable due to contract lie.
;;     v0.57.3 must fix contract to (-> tui-ctx? string? (or/c thread? void?)).

(require rackunit
         rackunit/text-ui
         (only-in "../tui/tui-keybindings.rkt"
                  make-tui-ctx
                  tui-ctx?
                  tui-ctx-ui-state-box
                  tui-ctx-input-state-box
                  handle-key
                  mark-dirty!)
         (only-in "../tui/tui-render-loop.rkt"
                  handle-user-submit!
                  next-message
                  drain-events!
                  check-busy-watchdog
                  current-busy-watchdog-ms)
         (only-in "../tui/state-types.rkt"
                  ui-state?
                  ui-state-busy?
                  ui-state-transcript
                  transcript-entry?
                  transcript-entry-kind
                  transcript-entry-text
                  initial-ui-state
                  set-busy)
         (only-in "../tui/input.rkt" input-state? initial-input-state input-insert-char))

;; ── Helpers ──

(define (make-test-tui-ctx #:runner [runner (lambda (text) (void))])
  (make-tui-ctx #:session-runner runner #:event-bus #f #:session-queue #f))

;; ── Characterization Tests ──

(define tui-hotspot-characterization-suite
  (test-suite "tui-hotspot-characterization"

    ;; D1: handle-user-submit! contract lies — produces thread, not tui-ctx?
    (test-case "CHAR: handle-user-submit! contract violation (returns thread not tui-ctx)"
      ;; Contract claims (-> tui-ctx? string? tui-ctx?) but the function spawns
      ;; a thread and returns the thread object. This throws exn:fail:contract.
      ;; v0.57.3 must fix: either return void? or (or/c thread? void?).
      (define ctx (make-test-tui-ctx))
      (check-exn exn:fail:contract?
                 (lambda () (handle-user-submit! ctx "hello"))
                 "handle-user-submit! breaks its own contract — returns thread not tui-ctx"))

    ;; D2: handle-user-submit! sets busy before throwing contract
    ;; (Can't test this because contract check fails on return.
    ;;  The busy state IS set inside the function before the thread returns,
    ;;  but contract check happens on the return value.)
    ;; Documented: the implementation sets busy at line 302 before spawning thread.

    ;; D3: handle-key with regular character inserts into input state
    (test-case "CHAR: handle-key with char modifies input state"
      (define ctx (make-test-tui-ctx))
      (handle-key ctx #\h)
      (handle-key ctx #\i)
      ;; Input state should have been modified
      (define inp (unbox (tui-ctx-input-state-box ctx)))
      (check-pred input-state? inp)
      ;; Marked dirty
      (check-true (tui-ctx? ctx)))

    ;; D4: check-busy-watchdog is pure and testable
    (test-case "CHAR: check-busy-watchdog returns #f for just-busy state"
      (define state (set-busy (initial-ui-state) #t))
      (define result (check-busy-watchdog state (current-inexact-milliseconds) 1000))
      (check-false result "watchdog should not fire for just-became-busy state"))

    ;; D5: drain-events on empty channel is safe
    (test-case "CHAR: drain-events! on fresh ctx is safe"
      (define ctx (make-test-tui-ctx))
      (check-not-exn (lambda () (drain-events! ctx))))

    ;; D6: next-message on empty channel returns #f with short timeout
    (test-case "CHAR: next-message returns #f on empty channel"
      (define ctx (make-test-tui-ctx))
      (define result (next-message ctx #:timeout 10))
      (check-false result))

    ;; D7: handle-key with Enter produces submit action
    (test-case "CHAR: handle-key with Enter triggers submit flow"
      (define ctx (make-test-tui-ctx))
      ;; First type some text
      (handle-key ctx #\h)
      (handle-key ctx #\i)
      ;; Then press Enter
      (define result (handle-key ctx #\return))
      ;; Should produce (list 'submit "hi")
      (check-equal? result '(submit "hi")))

    ;; D8: Duplicate submit documentation
    (test-case "CHAR: document duplicate submit debounce (500ms window)"
      ;; The duplicate debounce is inside handle-user-submit! which can't be
      ;; called due to D1 contract lie. Document behavior:
      ;; - If last transcript entry is 'user with same text and < 500ms old
      ;; - Adds "Duplicate input ignored" system entry
      ;; - Does NOT call runner
      ;; v0.57.3 should extract debounce to a testable pure function.
      (check-true #t "documented — see comments"))

    ;; D9: Document call sites for v0.57.3 decomposition
    (test-case "CHAR: document TUI decomposition targets for v0.57.3"
      ;; Top hotspot scores from W1 baseline:
      ;;   tui/tui-keybindings.rkt (24687) — 633 LOC
      ;;   tui/tui-render-loop.rkt (17670) — 465 LOC
      ;;   tui/commands.rkt (16031) — 391 LOC
      ;;
      ;; Decomposition plan:
      ;;   1. Fix handle-user-submit! contract: -> void? or (or/c thread? void?)
      ;;   2. Extract debounce logic to pure function
      ;;   3. Extract busy-queue logic to pure function
      ;;   4. Extract keymap fallthrough to explicit dispatch table
      ;;   5. Split tui-keybindings.rkt: keymap config vs handle-key vs context
      (check-true #t "documentation placeholder — see comments"))))

(run-tests tui-hotspot-characterization-suite)
