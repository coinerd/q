#!/usr/bin/env racket
#lang racket/base

;; lint-ivg.rkt — Mechanical Integration Verification Gate
;; Enforces dead-code and wiring expectations from v0.29.x audit remediation.
;; Run as part of lint-all.rkt.
;;
;; Exit codes:
;;   0 — all IVG checks pass
;;   1 — one or more failures

(require racket/port
         racket/system
         racket/string)

;; ── Grep helper ──

(define (grep-count pattern file)
  ;; Count lines matching pattern in file via grep -c
  (define-values (sp out in err)
    (subprocess #f #f #f (find-executable-path "grep") "-cE" pattern file))
  (close-output-port in)
  (define result (string-trim (port->string out)))
  (close-input-port out)
  (close-input-port err)
  (subprocess-wait sp)
  (if (zero? (subprocess-status sp))
      (string->number result)
      0))

(define (grep-count-dir pattern dir)
  ;; Count lines matching pattern recursively in dir
  (define-values (sp out in err)
    (subprocess #f #f #f (find-executable-path "grep") "-rnE" pattern dir))
  (close-output-port in)
  (define lines (port->string out))
  (close-input-port out)
  (close-input-port err)
  (subprocess-wait sp)
  (length (filter non-empty-string? (string-split lines "\n"))))

;; ── IVG checks ──
;; Each: (name expected? description)

(define checks
  (list
   ;; No inline hook block patterns in loop-stream.rkt (must use handle-hook-result)
   (list "no-inline-hook-block"
         (lambda () (zero? (grep-count "eq\\?.*hook-result-action.*'block" "agent/loop-stream.rkt")))
         "inline hook block patterns remain in loop-stream.rkt — use handle-hook-result instead")
   ;; No termination-decision references in runtime/
   (list "no-termination-decision"
         (lambda () (zero? (grep-count-dir "termination-decision" "runtime/")))
         "termination-decision still referenced in runtime/ — dead code removed in v0.29.9")
   ;; decide-next-action must be wired (≥1 call site)
   (list "decide-next-action-wired"
         (lambda () (>= (grep-count "decide-next-action" "runtime/iteration.rkt") 2))
         "decide-next-action has fewer than 2 references in iteration.rkt")
   ;; v0.32.4: classify-hook-result (replaced handle-hook-result) in loop-stream.rkt (≥3 calls)
   (list "classify-hook-result-stream"
         (lambda () (>= (grep-count "classify-hook-result" "agent/loop-stream.rkt") 3))
         "classify-hook-result has fewer than 3 calls in loop-stream.rkt")
   ;; v0.32.4: classify-hook-result (replaced handle-hook-result) in loop.rkt (≥3 calls)
   (list "classify-hook-result-loop"
         (lambda () (>= (grep-count "classify-hook-result" "agent/loop.rkt") 2))
         "classify-hook-result has fewer than 2 calls in loop.rkt")
   ;; emit-typed-event! must have NOTE comment in event-emitter.rkt (deferred dead code)
   (list "emit-typed-event-note"
         (lambda () (>= (grep-count "NOTE.*v0.29.14.*emit-typed-event" "agent/event-emitter.rkt") 1))
         "emit-typed-event! missing NOTE comment documenting production callers")
   ;; session-bytes-written must have DEPRECATED comment
   (list "session-bytes-written-deprecated"
         (lambda ()
           (>= (grep-count "DEPRECATED.*session-bytes-written" "tools/builtins/write.rkt") 1))
         "session-bytes-written missing DEPRECATED comment")
   ;; v0.29.13 W2: session-switch must use emit-typed-event! (not raw make-event)
   (list "session-switch-typed-events"
         (lambda () (>= (grep-count "emit-typed-event!" "runtime/session-switch.rkt") 2))
         "session-switch.rkt must use emit-typed-event! (≥2 calls)")
   ;; v0.29.14 W2: tool-coordinator must use emit-typed-event! (≥2 calls)
   (list "tool-coordinator-typed-events"
         (lambda () (>= (grep-count "emit-typed-event!" "runtime/tool-coordinator.rkt") 2))
         "tool-coordinator.rkt must use emit-typed-event! (≥2 calls)")
   ;; v0.29.16 W2: iteration typed events must be wired (≥1 each in runtime/ + extensions/)
   (list
    "iteration-events-wired"
    (lambda ()
      (and (>= (grep-count-dir "make-compaction-event" "runtime/") 1)
           (>= (grep-count-dir "make-injection-event" "extensions/") 1)))
    "make-compaction-event and make-injection-event must each have ≥1 call site in runtime/ and extensions/")))

;; ── Runner ──

(define (main)
  (define passed 0)
  (define failed 0)

  (for ([c (in-list checks)])
    (define name (car c))
    (define check-fn (cadr c))
    (define desc (caddr c))
    (cond
      [(check-fn)
       (printf "  [PASS] IVG: ~a~n" name)
       (set! passed (add1 passed))]
      [else
       (printf "  [FAIL] IVG: ~a — ~a~n" name desc)
       (set! failed (add1 failed))]))

  (printf "~n── IVG Summary: ~a passed, ~a failed ──~n" passed failed)
  (exit (if (positive? failed) 1 0)))

(main)
