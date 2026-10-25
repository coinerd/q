#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-outcome-error-surface.rkt
;; BUG-0043 fix verification (this release W2; flips the W0 pin).
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
         racket/hash
         racket/list
         racket/string
         racket/path
         racket/runtime-path
         "../extensions/gsd/wave-runner-port.rkt"
         "../extensions/gsd/events.rkt"
         (only-in "../extensions/gsd/core.rkt" reset-all-gsd-state!)
         (only-in "../extensions/gsd/wave-outcome-reporting.rkt"
                  emit-wave-outcome-error!)
         (only-in "../extensions/gsd-planning.rkt" gsd-planning-extension)
         (only-in "../extensions/api.rkt" extension-hooks make-extension-registry)
         (only-in "../extensions/context.rkt" make-extension-ctx)
         (only-in "../tools/tool.rkt" make-tool-registry)
         (only-in "../util/event/event-bus.rkt" make-event-bus subscribe!)
         (only-in "../util/event/event.rkt"
                  event?
                  event-ev
                  event-payload
                  event-time
                  make-event)
         "../tui/state.rkt"
         (only-in "tui/event-simulator.rkt"
                  make-test-event
                  transcript-types
                  transcript-texts
                  transcript-length))

(define-runtime-path tui-render-dir "../tui/render")

(define (fresh-state)
  (initial-ui-state #:session-id "outcome-err-sess" #:model-name "test-model"))

;; ============================================================
;; BUG-0093 (R93): the REAL production route
;;
;; Production shape: wave-outcome-reporting.rkt's emit-wave-outcome-error!
;; → extensions/gsd/events.rkt wraps the outcome fields under a data
;; envelope → the bridge installed by the gsd-planning extension's
;; register-tools hook forwards that WRAPPER as the runtime event payload
;; on the session bus → the TUI reducer applies it via
;; apply-event-to-state. The helpers below drive exactly that route:
;; register the actual extension against a fixture context/session bus,
;; subscribe through the installed bridge, emit, and feed the captured
;; runtime event through the reducer.
;; ============================================================

;; A long multiline recovery-guidance tail: the diagnostic must carry it
;; verbatim (the whole BUG is that the tail was lost behind "unknown").
(define r93-recovery-tail
  (string-append
   "wave runner died after 3 identical stall signatures.\n"
   "Recovery guidance:\n"
   "  1. Inspect .planning/HANDOFF.json for the last recorded wave state.\n"
   "  2. Re-run /go to resume from the pending wave; completed waves are not re-executed.\n"
   "  3. If the stall persists, narrow the wave's File: list to a single file and retry.\n"
   "  4. Escalate to the operator once the slow-lane retry patience is exhausted."))

(define (r93-capture-outcome-error-event! wave kind message)
  (define bus (make-event-bus))
  (define ctx (make-extension-ctx #:session-id "outcome-err-route"
                                  #:session-dir "/tmp"
                                  #:event-bus bus
                                  #:extension-registry (make-extension-registry)
                                  #:tool-registry (make-tool-registry)
                                  #:command-registry (box (hash))))
  (define captured (box #f))
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     ;; install the REAL production bridge (the extension's register-tools
     ;; hook wires extensions/gsd/events.rkt to the session bus) ...
     ((hash-ref (extension-hooks gsd-planning-extension) 'register-tools) ctx (hasheq))
     ;; ... subscribe through it, capturing the runtime event the TUI consumes
     (subscribe! bus (lambda (evt) (set-box! captured evt)))
     (emit-wave-outcome-error! wave kind message)
     (unbox captured))
   (lambda ()
     ;; restore module-load defaults so later cases are unaffected
     (set-gsd-event-bus! void)
     (reset-all-gsd-state!))))

;; Simulates the JSON transport round trip: every object key arrives as a
;; string at every nesting level (wrapper AND inner outcome fields).
(define (r93-keys->strings v)
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values (if (symbol? k) (symbol->string k) k)
               (r93-keys->strings val)))]
    [(pair? v) (map r93-keys->strings v)]
    [else v]))

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

    ;; ── BUG-0067: worker-side events cross the JSON IPC boundary ──
    ;;
    ;; JSON objects have no keyword keys, so a worker-emitted outcome
    ;; error arrives with STRING keys. The keyword-only hash-ref used to
    ;; render every such event as "wave outcome: unknown" — the exact
    ;; confusion from the 2026-09-10 launch-day incidents. Both key
    ;; shapes must render kind + message.

    (test-case "BUG-0067: string-keyed (JSON IPC) payloads render kind + message"
      (define st (fresh-state))
      (define next
        (apply-event-to-state
         st
         (make-test-event "gsd.wave.outcome-error"
                          (hasheq "wave"
                                  2
                                  "kind"
                                  "infra-failed"
                                  "level"
                                  "error"
                                  "message"
                                  "provider/network failure persisted after 3 automatic retries"))))
      (check-equal? (transcript-types next) '(system-error))
      (define text (first (transcript-texts next)))
      (check-true (string-contains? text "infra-failed")
                  "string-keyed kind must surface, not fall back to unknown")
      (check-false (string-contains? text "wave outcome: unknown")
                   "the confusing unknown fallback must be gone for IPC payloads")
      (check-true (string-contains? text "provider/network failure persisted")))

    (test-case "BUG-0067: genuinely missing kind still degrades honestly"
      (define st (fresh-state))
      (define next (apply-event-to-state st (make-test-event "gsd.wave.outcome-error" (hasheq))))
      (check-equal? (transcript-types next) '(system-error))
      (check-true (string-contains? (first (transcript-texts next)) "unknown")))

    ;; ── BUG-0093 (R93): the real production route ───────────────
    ;;
    ;; The GSD extension emits wave/kind/message nested under a data
    ;; wrapper and the installed bridge forwards that wrapper as the
    ;; runtime payload. RED-first: these cases FAILED before the fix
    ;; (reducer read only flat fields → "wave outcome: unknown"),
    ;; swallowing the wave index, the kind and the full recovery tail.

    (test-case "BUG-0093: emitted outcome error through the installed bridge renders full wave/kind/message incl recovery tail"
      (define evt (r93-capture-outcome-error-event! 3 'infra-failed r93-recovery-tail))
      (check-true (event? evt) "bridge must forward a runtime event")
      (check-equal? (event-ev evt) 'gsd.wave.outcome-error)
      (define next (apply-event-to-state (fresh-state) evt))
      ;; exactly ONE diagnostic entry: the typed error surface, nothing else
      (check-equal? (transcript-length next) 1)
      (check-equal? (transcript-types next) '(system-error))
      (define text (first (transcript-texts next)))
      (check-false (string-contains? text "wave outcome: unknown")
                   "the unknown fallback must be gone on the production route")
      (check-true (string-contains? text "wave 3") "text must name the wave index")
      (check-true (string-contains? text "infra-failed") "text must carry the outcome kind")
      (check-true (string-contains? text r93-recovery-tail)
                  "text must carry the FULL message incl multiline recovery tail verbatim"))

    (test-case "BUG-0093: transport-shaped string keys at wrapper AND inner levels render identically"
      (define evt (r93-capture-outcome-error-event! 3 'infra-failed r93-recovery-tail))
      (define transported
        (make-event (event-ev evt) (event-time evt) #f #f (r93-keys->strings (event-payload evt))))
      (define next (apply-event-to-state (fresh-state) transported))
      (check-equal? (transcript-types next) '(system-error))
      (check-equal? (transcript-length next) 1)
      (define text (first (transcript-texts next)))
      (check-false (string-contains? text "wave outcome: unknown")
                   "string-keyed wrapper+inner must NOT fall back to unknown")
      (check-true (string-contains? text "wave 3"))
      (check-true (string-contains? text "infra-failed"))
      (check-true (string-contains? text r93-recovery-tail)))

    ;; ── BUG-0093 controls: malformed data must not crash, flat paths unchanged ──

    (test-case "BUG-0093: malformed nested data never crashes and keeps the honest fallback"
      (for ([payload (in-list (list (hasheq 'data 42)
                                    (hasheq 'data (hasheq))
                                    (hasheq 'data (hasheq 'unrelated "junk"))
                                    (hasheq "data" "garbage")
                                    ;; non-hash payloads used to crash the flat hash-ref
                                    "not-a-hash-at-all"))])
        (define next
          (apply-event-to-state (fresh-state)
                                (make-test-event "gsd.wave.outcome-error" payload)))
        (check-equal? (transcript-types next) '(system-error) (format "payload ~v" payload))
        (check-true (string-contains? (first (transcript-texts next)) "wave outcome: unknown")
                    (format "honest unknown fallback for payload ~v" payload))))

    (test-case "BUG-0093: flat symbol payload control is unchanged by the envelope unwrap"
      (define st (fresh-state))
      (define next
        (apply-event-to-state
         st
         (make-test-event "gsd.wave.outcome-error"
                          (hasheq 'wave 1 'kind 'stalled 'level "error" 'message "flat control"))))
      (check-equal? (transcript-types next) '(system-error))
      (define text (first (transcript-texts next)))
      (check-true (string-contains? text "wave 1"))
      (check-true (string-contains? text "stalled"))
      (check-true (string-contains? text "flat control")))

    (test-case "BUG-0093: flat string payload control is unchanged by the envelope unwrap"
      (define st (fresh-state))
      (define next
        (apply-event-to-state
         st
         (make-test-event "gsd.wave.outcome-error"
                          (hasheq "wave" 1 "kind" "stalled" "level" "error" "message" "flat string control"))))
      (check-equal? (transcript-types next) '(system-error))
      (define text (first (transcript-texts next)))
      (check-true (string-contains? text "wave 1"))
      (check-true (string-contains? text "stalled"))
      (check-true (string-contains? text "flat string control")))

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
