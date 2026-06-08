#lang racket

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/test-hook-golden.rkt — Hook golden payload shape tests (v0.22.8 W4 T1)
;;
;; Freeze representative payloads for each hook point to catch accidental
;; field removal, type changes, or constructor arity changes.
;;
;; These tests are deliberately simple: construct each payload type and
;; verify expected keys/fields exist. They serve as regression canaries.

(require rackunit
         rackunit/text-ui
         "../util/event/event-payloads.rkt"
         "../util/hook-types.rkt"
         "../extensions/hooks.rkt")

(define hook-golden-tests
  (test-suite "Hook Golden Payloads (v0.22.8 W4)"

    ;; ── session-start-payload ─────────────────────────────────
    (test-case "session-start-payload: constructable with expected fields"
      (define p (session-start-payload "sess-1" (hasheq 'model "gpt-4") 'new))
      (check-equal? (session-start-payload-session-id p) "sess-1")
      (check-equal? (session-start-payload-reason p) 'new)
      (check-pred values (session-start-payload-config p)))

    (test-case "session-start-payload: payload->hash round-trip"
      (define p (session-start-payload "sess-1" (hasheq) 'resume))
      (define h (payload->hash p))
      (check-pred values (hash-ref h 'session-id #f))
      (check-pred values (hash-ref h 'config #f))
      (check-pred values (hash-ref h 'reason #f)))

    ;; ── session-end-payload ───────────────────────────────────
    (test-case "session-end-payload: constructable with expected fields"
      (define p (session-end-payload "sess-1" 42.5))
      (check-equal? (session-end-payload-session-id p) "sess-1")
      (check-equal? (session-end-payload-duration p) 42.5))

    (test-case "session-end-payload: payload->hash round-trip"
      (define p (session-end-payload "sess-1" 'completed))
      (define h (payload->hash p))
      (check-pred values (hash-ref h 'session-id #f))
      (check-pred values (hash-ref h 'duration #f)))

    ;; ── session-switch-payload ────────────────────────────────
    (test-case "session-switch-payload: constructable with expected fields"
      (define p (session-switch-payload "sess-1" 'resume))
      (check-equal? (session-switch-payload-session-id p) "sess-1")
      (check-equal? (session-switch-payload-operation p) 'resume))

    (test-case "session-switch-payload: payload->hash round-trip"
      (define p (session-switch-payload "sess-1" 'fork))
      (define h (payload->hash p))
      (check-pred values (hash-ref h 'session-id #f))
      (check-pred values (hash-ref h 'operation #f)))

    ;; ── tool-call-event-payload ───────────────────────────────
    (test-case "tool-call-event-payload: constructable with expected fields"
      (define p (tool-call-event-payload "sess-1" "turn-1" "bash" "tc-1"))
      (check-equal? (tool-call-event-payload-session-id p) "sess-1")
      (check-equal? (tool-call-event-payload-turn-id p) "turn-1")
      (check-equal? (tool-call-event-payload-tool-name p) "bash")
      (check-equal? (tool-call-event-payload-tool-call-id p) "tc-1"))

    (test-case "tool-call-event-payload: payload->hash round-trip"
      (define p (tool-call-event-payload "s" "t" "read" "tc"))
      (define h (payload->hash p))
      (check-pred values (hash-ref h 'session-id #f))
      (check-pred values (hash-ref h 'turn-id #f))
      (check-pred values (hash-ref h 'tool-name #f))
      (check-pred values (hash-ref h 'tool-call-id #f)))

    ;; ── session-id-payload ────────────────────────────────────
    (test-case "session-id-payload: constructable with expected fields"
      (define p (session-id-payload "sess-1"))
      (check-equal? (session-id-payload-session-id p) "sess-1"))

    (test-case "session-id-payload: payload->hash round-trip"
      (define h (payload->hash (session-id-payload "sess-1")))
      (check-pred values (hash-ref h 'sessionId #f)))

    ;; ── error-payload ─────────────────────────────────────────
    (test-case "error-payload: constructable with expected fields"
      (define p (error-payload "something failed" 'runtime))
      (check-equal? (error-payload-error p) "something failed")
      (check-equal? (error-payload-error-type p) 'runtime))

    (test-case "error-payload: payload->hash round-trip"
      (define h (payload->hash (error-payload "err" 'io)))
      (check-pred values (hash-ref h 'error #f))
      (check-pred values (hash-ref h 'errorType #f)))

    ;; ── input-payload ─────────────────────────────────────────
    (test-case "input-payload: constructable with expected fields"
      (define p (input-payload "sess-1" "hello world"))
      (check-equal? (input-payload-session-id p) "sess-1")
      (check-equal? (input-payload-message p) "hello world"))

    (test-case "input-payload: payload->hash round-trip"
      (define h (payload->hash (input-payload "s" "msg")))
      (check-pred values (hash-ref h 'session-id #f))
      (check-pred values (hash-ref h 'message #f)))

    ;; ── gsd-mode-payload ──────────────────────────────────────
    (test-case "gsd-mode-payload: constructable with expected fields"
      (define p (gsd-mode-payload 'planning 'implementing))
      (check-equal? (gsd-mode-payload-old-mode p) 'planning)
      (check-equal? (gsd-mode-payload-new-mode p) 'implementing))

    (test-case "gsd-mode-payload: payload->hash round-trip"
      (define h (payload->hash (gsd-mode-payload 'idle 'planning)))
      (check-pred values (hash-ref h 'old-mode #f))
      (check-pred values (hash-ref h 'new-mode #f)))

    ;; ── hook-result (from hook-types.rkt) ─────────────────────
    (test-case "hook-result: pass action"
      (define r (hook-pass))
      (check-equal? (hook-result-action r) 'pass)
      (check-false (hook-result-payload r)))

    (test-case "hook-result: amend action with payload"
      (define r (hook-amend (hasheq 'modified #t)))
      (check-equal? (hook-result-action r) 'amend)
      (check-true (hash-ref (hook-result-payload r) 'modified)))

    (test-case "hook-result: block action with reason"
      (define r (hook-block "too dangerous"))
      (check-equal? (hook-result-action r) 'block)
      (check-equal? (hook-result-payload r) "too dangerous"))

    ;; ── before-agent-start hash payload (iteration.rkt) ───────
    (test-case "before-agent-start: hasheq payload has required keys"
      (define h (hasheq 'session-id "s1" 'max-iterations 10 'context-message-count 20))
      (check-pred values (hash-ref h 'session-id #f))
      (check-pred values (hash-ref h 'max-iterations #f))
      (check-pred values (hash-ref h 'context-message-count #f)))))

(run-tests hook-golden-tests)
