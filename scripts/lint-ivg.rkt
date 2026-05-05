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
    (subprocess #f #f #f
                (find-executable-path "grep")
                "-cE" pattern file))
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
    (subprocess #f #f #f
                (find-executable-path "grep")
                "-rnE" pattern dir))
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
         (lambda ()
           (zero? (grep-count "eq\\?.*hook-result-action.*'block"
                              "agent/loop-stream.rkt")))
         "inline hook block patterns remain in loop-stream.rkt — use handle-hook-result instead")
   ;; No termination-decision references in runtime/
   (list "no-termination-decision"
         (lambda ()
           (zero? (grep-count-dir "termination-decision" "runtime/")))
         "termination-decision still referenced in runtime/ — dead code removed in v0.29.9")
   ;; decide-next-action must be wired (≥1 call site)
   (list "decide-next-action-wired"
         (lambda ()
           (>= (grep-count "decide-next-action" "runtime/iteration.rkt") 2))
         "decide-next-action has fewer than 2 references in iteration.rkt")
   ;; handle-hook-result must be used in loop-stream.rkt (≥3 calls)
   (list "handle-hook-result-stream"
         (lambda ()
           (>= (grep-count "handle-hook-result" "agent/loop-stream.rkt") 3))
         "handle-hook-result has fewer than 3 calls in loop-stream.rkt")
   ;; handle-hook-result must be used in loop.rkt (≥3 calls)
   (list "handle-hook-result-loop"
         (lambda ()
           (>= (grep-count "handle-hook-result" "agent/loop.rkt") 3))
         "handle-hook-result has fewer than 3 calls in loop.rkt")
   ;; emit-typed-event! must have NOTE comment in event-emitter.rkt (deferred dead code)
   (list "emit-typed-event-note"
         (lambda ()
           (>= (grep-count "NOTE.*v0.29.12.*emit-typed-event" "agent/event-emitter.rkt") 1))
         "emit-typed-event! missing NOTE comment documenting deferred status")
   ;; session-bytes-written must have DEPRECATED comment
   (list "session-bytes-written-deprecated"
         (lambda ()
           (>= (grep-count "DEPRECATED.*session-bytes-written" "tools/builtins/write.rkt") 1))
         "session-bytes-written missing DEPRECATED comment")))

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
