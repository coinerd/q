#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-outcome-error-surface.rkt
;; BUG-0043 fix verification (v1.00.21 W2; flips the W0 pin).
;;
;; A wave-execution-outcome with kind != 'done now surfaces as a typed
;; [SYS] [ERROR] transcript event on the TUI error surface (exactly one
;; copy of the outcome text) instead of riding the conversation/message
;; surface as plain system text. Every assertion below PASSES against
;; the fixed behavior; the W0 characterization asserted the exact
;; opposite of each seam and has been replaced.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/string
         racket/path
         racket/runtime-path
         "../extensions/gsd/wave-runner-port.rkt"
         "../extensions/gsd/events.rkt"
         "../tui/state.rkt"
         (only-in "tui/event-simulator.rkt"
                  make-test-event
                  transcript-types
                  transcript-texts
                  transcript-length))

(define-runtime-path tui-render-dir "../tui/render")

(define (fresh-state)
  (initial-ui-state #:session-id "outcome-err-sess" #:model-name "test-model"))

(define outcome-error-surface-suite
  (test-suite "BUG-0043 W2: outcome errors ride the typed [SYS] [ERROR] transcript surface"

    ;; ── The outcome port accepts non-'done kinds (unchanged from W0) ──

    (test-case "wave-execution-outcome guard accepts kind != 'done"
      (define oc (wave-execution-outcome 'failed "wave failed: boom"))
      (check-equal? (wave-execution-outcome-kind oc) 'failed)
      (check-equal? (wave-execution-outcome-message oc) "wave failed: boom")
      (define oc2 (wave-execution-outcome 'cancelled "cancelled by operator"))
      (check-equal? (wave-execution-outcome-kind oc2) 'cancelled)
      (define oc3 (wave-execution-outcome 'interrupted "interrupted"))
      (check-not-equal? (wave-execution-outcome-kind oc3) 'done))

    ;; ── Event-surface seam (what the TUI reducer consumes) ────────

    (test-case "typed error event kind exists in the GSD event taxonomy (W2 flip)"
      (check-true (and (list? gsd-event-names) (pair? gsd-event-names)))
      (check-true (for/or ([n (in-list gsd-event-names)])
                    (and (symbol? n) (string=? (symbol->string n) "gsd.wave.outcome-error")))
                  "taxonomy must contain gsd.wave.outcome-error"))

    (test-case "terminal failure emits an error-classified event with kind + message verbatim (W2 flip)"
      (define-values (collect! query) (make-event-collector))
      (set-gsd-event-bus! collect!)
      (emit-gsd-event!
       'gsd.wave.outcome-error
       (hasheq 'wave 1 'kind 'stalled 'level "error" 'message "hard stall: no forward progress"))
      (check-equal? (length (collector-events query)) 1)
      (define ev (first (collector-events query)))
      (check-equal? (hash-ref ev 'event) 'gsd.wave.outcome-error)
      (define data (hash-ref ev 'data))
      ;; error typing the TUI reducer branches on: level + kind, verbatim message
      (check-equal? (hash-ref data 'level) "error")
      (check-equal? (hash-ref data 'kind) 'stalled)
      (check-equal? (hash-ref data 'message) "hard stall: no forward progress")
      (check-equal? (hash-ref data 'wave) 1))

    ;; ── TUI reducer seam: exactly one transcript entry, none in the
    ;;    conversation/message surface ──────────────────────────────

    (test-case "injected stall-kill outcome → one 'system-error transcript entry, no message-surface copy"
      (define st (fresh-state))
      (define evt
        (make-test-event "gsd.wave.outcome-error"
                         (hasheq 'wave
                                 1
                                 'kind
                                 'stalled
                                 'level
                                 "error"
                                 'message
                                 "hard stall: killed after 15 identical signatures")))
      (define next (apply-event-to-state st evt))
      ;; exactly ONE entry total: the error event itself — the
      ;; conversation/message surface got nothing.
      (check-equal? (transcript-length next) 1)
      (check-equal? (transcript-types next) '(system-error))
      (define text (first (transcript-texts next)))
      ;; kind + message verbatim, wave index included
      (check-true (string-contains? text "stalled") "text must carry the outcome kind")
      (check-true (string-contains? text "hard stall: killed after 15 identical signatures")
                  "text must carry the outcome message verbatim")
      (check-true (string-contains? text "wave 1") "text must name the wave"))

    (test-case "outcome-error without wave index degrades to kind + message"
      (define st (fresh-state))
      (define next
        (apply-event-to-state
         st
         (make-test-event
          "gsd.wave.outcome-error"
          (hasheq 'kind 'infra-failed 'level "error" 'message "provider/network failure persisted"))))
      (check-equal? (transcript-types next) '(system-error))
      (check-true (string-contains? (first (transcript-texts next))
                                    "provider/network failure persisted")))

    (test-case "done-class outcomes are unaffected: no error entry appears"
      (define st (fresh-state))
      (define next
        (apply-event-to-state st
                              (make-test-event "gsd.wave.completed" (hasheq 'wave 1 'status 'done))))
      (check-false (for/or ([k (in-list (transcript-types next))])
                     (eq? k 'system-error))
                   "done outcomes must not produce [SYS] [ERROR] entries"))

    ;; ── TUI message-layout seam ───────────────────────────────────

    (test-case "TUI render source has the [SYS] [ERROR] event variant (W2 flip)"
      ;; The error-styled system event renders as a distinct red
      ;; [SYS] [ERROR] line, visually separable from assistant output.
      (check-true
       (for/or ([f (in-list (find-files (lambda (p)
                                          (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                               (not (string-contains? (path->string p) "compiled"))))
                                        tui-render-dir))])
         (string-contains? (file->string f) "[SYS] [ERROR]"))
       "message-layout must render a [SYS] [ERROR] variant"))))

(module+ main
  (exit (run-tests outcome-error-surface-suite)))
