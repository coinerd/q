#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-campaign-notifier.rkt
;; BUG-0040 (v1.00.21 W6): the W0 characterization pin FLIPPED.
;;
;; W0 pinned the absent surface (no notifier sink, no gsd.notify.*
;; keys, no module). W6 delivered the terminal-transition notifier:
;; extensions/gsd/notify.rkt + go-orchestrator hooks. These tests now
;; pin the DELIVERED behavior:
;;   - terminal-transition kinds are exactly the six wave/campaign
;;     terminal kinds (no per-tool/per-iteration spam kinds)
;;   - message format carries campaign id, wave idx, kind, one-line
;;     reason, and spend when W5 recorded one
;;   - sinks resolve from settings: tmux only under $TMUX, desktop /
;;     webhook opt-in, misconfigured URLs warn-once + skip
;;   - a fake (recording) sink observes done / failed / stall paths
;;   - a raising sink NEVER fails the transition (best-effort contract)

(require rackunit
         rackunit/text-ui
         racket/string
         racket/system
         racket/port
         racket/file
         "../runtime/settings-core.rkt"
         "../extensions/gsd/notify.rkt")

(define go-orchestrator-src
  (path->string (build-path (or (current-directory) ".") "extensions" "gsd" "go-orchestrator.rkt")))

(define (source-contains? needle [hay go-orchestrator-src])
  (call-with-input-file hay (lambda (in) (string-contains? (port->string in) needle))))

;; Force deterministic sink resolution regardless of the host tmux.
(putenv "TMUX" "")

(define campaign-notifier-suite
  (test-suite "BUG-0040 W6 flip: terminal-transition notifier surface (delivered)"

    ;; ── Kinds: terminal transitions ONLY (no spam) ──────────────

    (test-case "terminal-transition kinds are exactly the six terminal kinds"
      (check-equal?
       TERMINAL-TRANSITION-KINDS
       '(wave-done wave-failed campaign-complete campaign-cancelled stall-terminal budget-pause))
      (for ([k TERMINAL-TRANSITION-KINDS])
        (check-true (terminal-transition-kind? k) (format "~a terminal" k)))
      ;; Non-terminal kinds are rejected — per-tool/iteration spam can
      ;; never enter this surface.
      (for ([k '(tool-call iteration progress wave-started)])
        (check-false (terminal-transition-kind? k) (format "~a rejected" k))))

    ;; ── Notification value + message format ─────────────────────

    (test-case "message format: campaign id, wave idx, kind, reason, spend"
      (define n
        (make-gsd-notification "deadbeef" 3 'wave-done #:reason "wave completed" #:spend 0.42))
      (define msg (gsd-notification-message n))
      (check-true (string-contains? msg "deadbeef") "campaign id")
      (check-true (string-contains? msg "wave 3") "wave idx")
      (check-true (string-contains? msg "wave done") "outcome kind")
      (check-true (string-contains? msg "wave completed") "reason")
      (check-true (string-contains? msg "spend $0.42") "spend when recorded"))

    (test-case "message stays one line; missing pieces degrade gracefully"
      (define n (make-gsd-notification "cafe42" #f 'campaign-complete #:reason "line one\nline two"))
      (define msg (gsd-notification-message n))
      (check-false (string-contains? msg "\n") "reason collapsed to one line")
      (check-false (string-contains? msg "wave ") "campaign-level: no wave idx")
      (check-false (string-contains? msg "$") "no spend recorded"))

    (test-case "non-terminal kind is a contract violation"
      (check-exn exn:fail:contract? (lambda () (make-gsd-notification "x" 0 'tool-call))))

    ;; ── Settings → sink resolution ──────────────────────────────

    (test-case "silent default: no sinks outside tmux with no opt-ins"
      (check-equal? (gsd-notify-sinks-from-settings #f) (list)))

    (test-case "opt-in sinks resolve from settings"
      (define sinks
        (gsd-notify-sinks-from-settings
         (q-settings (hash)
                     (hash)
                     (hasheq 'gsd
                             (hasheq 'notify
                                     (hasheq 'desktop-command
                                             "echo {message}"
                                             'webhook-url
                                             "https://hooks.example/x"))))))
      (define ids (map gsd-notify-sink-id sinks))
      (check-equal? ids '(desktop-command webhook))
      ;; desktop-command renders the {message} template end to end.
      ;; (subprocess output goes to the OS stdout, not a parameterized
      ;; port — so route it to a scratch file instead of a string port.)
      (define scratch "tmp/notify-desktop-test.out")
      (with-handlers ([exn:fail? void])
        (delete-file scratch))
      (define cmd-sink
        (gsd-notify-sinks-from-settings
         (q-settings (hash)
                     (hash)
                     (hasheq 'gsd
                             (hasheq 'notify
                                     (hasheq 'desktop-command
                                             (string-append "echo {message} >> " scratch)))))))
      (notify-terminal-transition!
       cmd-sink
       (make-gsd-notification "c1" 0 'wave-done #:reason "wave completed"))
      (check-true (and (file-exists? scratch)
                       (string-contains? (file->string scratch) "gsd[c1] wave 0"))
                  "desktop template got the rendered message")
      (with-handlers ([exn:fail? void])
        (delete-file scratch)))

    (test-case "misconfigured webhook URL: warn once, sink skipped"
      (reset-gsd-notify-warnings!)
      (define sinks
        (gsd-notify-sinks-from-settings
         (q-settings (hash)
                     (hash)
                     (hasheq 'gsd (hasheq 'notify (hasheq 'webhook-url "ftp://nope"))))))
      (check-equal? sinks (list) "non-http(s) URL adds no sink"))

    ;; ── Fake sink records done / failed / stall emissions ───────

    (test-case "recording sink observes done / failed / stall transitions"
      (define events (box (list)))
      (define sink (make-recording-sink events))
      (parameterize ([current-gsd-notify-sinks (list sink)])
        (notify-terminal-transition*! "camp9" 0 'wave-done #:reason "wave completed")
        (notify-terminal-transition*! "camp9" 1 'wave-failed #:reason "delivery verify failed")
        (notify-terminal-transition*! "camp9"
                                      2
                                      'stall-terminal
                                      #:reason "watchdog killed runner"
                                      #:spend 1.25))
      (define seen (reverse (unbox events)))
      (check-equal? (map gsd-notification-kind seen) '(wave-done wave-failed stall-terminal))
      (check-equal? (map gsd-notification-campaign-id seen) '("camp9" "camp9" "camp9"))
      (check-equal? (map gsd-notification-wave-idx seen) '(0 1 2))
      (check-equal? (gsd-notification-spend (caddr seen)) 1.25))

    (test-case "raising sink never fails the transition"
      (define events (box (list)))
      (define good (make-recording-sink events))
      (parameterize ([current-gsd-notify-sinks (list (make-raising-sink) good)])
        (notify-terminal-transition*! "campR" 5 'budget-pause #:reason "budget exhausted"))
      ;; The raising sink raised mid-fan-out and was absorbed; the
      ;; next sink still ran and the call returned normally.
      (check-equal? (map gsd-notification-kind (unbox events)) '(budget-pause)))

    (test-case "hook sites exist in go-orchestrator terminal paths"
      ;; Source-level pin (pure boundary): the orchestrator fans out at
      ;; the terminal sites, incl. the W5 budget-pause kind.
      (check-true (source-contains? "notify-terminal-transition*!")
                  "orchestrator calls the fan-out helper")
      (check-true (source-contains? "'stall-terminal") "stall site wired")
      (check-true (source-contains? "'budget-pause") "budget-pause site wired")
      (check-true (source-contains? "'campaign-complete") "completion wired")
      (check-true (source-contains? "'campaign-cancelled") "cancellation wired"))))

(module+ main
  (exit (run-tests campaign-notifier-suite)))
