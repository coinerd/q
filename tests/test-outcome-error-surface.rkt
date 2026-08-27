#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-outcome-error-surface.rkt
;; BUG-0043 characterization pin (v1.00.21 W0; FLIPPED by W2).
;;
;; TODAY a wave-execution-outcome with kind != 'done delivers its
;; message through the conversation/message surface: the failure text
;; rides the generic gsd.wave.failed telemetry event as plain data and
;; renders in the TUI as ordinary (dim [SYS]) system text. No typed
;; [SYS] [ERROR] transcript event exists anywhere. Every assertion
;; below PASSES against today's red behavior; W2 flips them once
;; outcome errors route to a real transcript error event.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/string
         racket/path
         racket/runtime-path
         "../extensions/gsd/wave-runner-port.rkt"
         "../extensions/gsd/events.rkt")

(define-runtime-path tui-render-dir "../tui/render")

(define outcome-error-surface-suite
  (test-suite "BUG-0043 characterization: outcome errors ride the generic message surface (W0 pin; W2 flips)"

    ;; ── The outcome port accepts non-'done kinds ──────────────────

    (test-case "wave-execution-outcome guard accepts kind != 'done"
      (define oc (wave-execution-outcome 'failed "wave failed: boom"))
      (check-equal? (wave-execution-outcome-kind oc) 'failed)
      (check-equal? (wave-execution-outcome-message oc) "wave failed: boom")
      (define oc2 (wave-execution-outcome 'cancelled "cancelled by operator"))
      (check-equal? (wave-execution-outcome-kind oc2) 'cancelled)
      (define oc3 (wave-execution-outcome 'interrupted "interrupted"))
      (check-not-equal? (wave-execution-outcome-kind oc3) 'done))

    ;; ── Event-surface seam (what the TUI reducer consumes) ────────

    (test-case "no error event kind exists in the GSD event taxonomy (absent seam)"
      (check-true (and (list? gsd-event-names) (pair? gsd-event-names)))
      (check-false (for/or ([n (in-list gsd-event-names)])
                     (and (symbol? n) (string-contains? (symbol->string n) "error")))))

    (test-case "terminal failure carries the outcome message as plain data, not an error-typed event"
      (define-values (collect! query) (make-event-collector))
      (set-gsd-event-bus! collect!)
      (define oc (wave-execution-outcome 'failed "stall detected: signature repeated 15 times"))
      (emit-gsd-event! 'gsd.wave.failed (hasheq 'message (wave-execution-outcome-message oc)))
      ;; exactly one event on the bus — the generic telemetry event itself
      (check-equal? (length (collector-events query)) 1)
      (define ev (first (collector-events query)))
      (check-equal? (hash-ref ev 'event) 'gsd.wave.failed)
      ;; the message rides the event's data payload as plain text …
      (define data (hash-ref ev 'data))
      (check-equal? (hash-ref data 'message) "stall detected: signature repeated 15 times")
      ;; … with no error classification anywhere on the wrapped event the
      ;; TUI reducer consumes — no level/severity/error typing to branch on.
      (check-false (hash-has-key? ev 'error))
      (check-false (hash-has-key? ev 'level))
      (check-false (hash-has-key? ev 'severity))
      (check-false (hash-has-key? data 'error))
      (check-false (hash-has-key? data 'level))
      (check-false (hash-has-key? data 'kind)))

    ;; ── TUI message-layout seam ───────────────────────────────────

    (test-case "TUI render source has no [SYS] [ERROR] event variant"
      ;; System text renders as dim [SYS] lines only; the error-styled
      ;; system event the bug report asks for does not exist yet.
      (check-true (pair? (find-files (lambda (p)
                                       (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                            (not (string-contains? (path->string p) "compiled"))))
                                     tui-render-dir))
                  "tui/render sources must exist to scan")
      (check-false
       (for/or ([f (in-list (find-files (lambda (p)
                                          (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                               (not (string-contains? (path->string p) "compiled"))))
                                        tui-render-dir))])
         (string-contains? (file->string f) "[SYS] [ERROR]"))))))

(module+ main
  (exit (run-tests outcome-error-surface-suite)))
