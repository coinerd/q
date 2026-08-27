#lang racket/base

;; ============================================================
;; GSD campaign notification surface (v1.00.22 W6 — BUG-0040)
;;
;; `gsd.campaign.*` events were consumed in-process only: detached
;; campaigns (tmux, CI, remote) emitted NOTHING at terminal
;; transitions, so supervision degraded to 10-minute polling and
;; watchdog-killed campaigns sat idle for ~25 minutes.
;;
;; This module owns the TERMINAL-transition fan-out ONLY:
;;   wave-done, wave-failed, campaign-complete, campaign-cancelled,
;;   stall-terminal, budget-pause (W5 kinds included).
;; No per-tool, per-iteration, or progress spam — only transitions
;; that end a wave or the whole campaign.
;;
;; Sinks:
;;   tmux       — default when $TMUX is set: `tmux display-message`
;;                plus a terminal bell. Silent otherwise (opt-in via
;;                settings for everything else).
;;   desktop    — gsd.notify.desktop-command: shell template with
;;                {message} {kind} {campaign} {wave} {reason} {spend}.
;;   webhook    — gsd.notify.webhook-url: curl POST of a small JSON
;;                payload (campaign, wave, kind, reason, spend).
;;
;; CONTRACT: notifications are BEST-EFFORT. Every sink emission runs
;; inside a per-sink exception handler AND a hard wall-clock timeout
;; (custodian shutdown kills the worker thread + subprocess). A sink
;; that fails, hangs, or is misconfigured is warned about ONCE and
;; skipped — it can NEVER affect the campaign outcome.
;; ============================================================

(require racket/contract
         racket/format
         json
         racket/match
         racket/string
         racket/system
         (only-in "../../runtime/settings-query.rkt"
                  gsd-notify-desktop-command
                  gsd-notify-webhook-url)
         (only-in "events.rkt" emit-gsd-event!))

(provide (contract-out
          [TERMINAL-TRANSITION-KINDS (listof symbol?)]
          [terminal-transition-kind? (-> any/c boolean?)]
          [gsd-notification? (-> any/c boolean?)]
          [gsd-notification-campaign-id (-> gsd-notification? string?)]
          [gsd-notification-wave-idx (-> gsd-notification? (or/c #f exact-nonnegative-integer?))]
          [gsd-notification-kind (-> gsd-notification? symbol?)]
          [gsd-notification-reason (-> gsd-notification? string?)]
          [gsd-notification-spend (-> gsd-notification? (or/c #f (and/c real? positive?)))]
          [make-gsd-notification
           (->* (string? (or/c #f exact-nonnegative-integer?) symbol?)
                (#:reason string? #:spend (or/c #f (and/c real? positive?)))
                gsd-notification?)]
          [gsd-notification-message (-> gsd-notification? string?)]
          [gsd-notify-sink? (-> any/c boolean?)]
          [gsd-notify-sink-id (-> gsd-notify-sink? symbol?)]
          [gsd-notify-sinks-from-settings (-> (or/c #f any/c) (listof gsd-notify-sink?))]
          [notify-terminal-transition! (-> (listof gsd-notify-sink?) gsd-notification? void?)]
          [current-gsd-notify-sinks (parameter/c (listof gsd-notify-sink?))]
          [notify-terminal-transition*!
           (->* (string? (or/c #f exact-nonnegative-integer?) symbol?)
                (#:reason string? #:spend (or/c #f (and/c real? positive?)))
                void?)]
          ;; test doubles (the W0 pin flips onto these)
          [make-recording-sink (-> (box/c list?) gsd-notify-sink?)]
          [make-raising-sink (-> gsd-notify-sink?)]
          [reset-gsd-notify-warnings! (-> void?)]))

;; ── Kinds ────────────────────────────────────────────────────

(define TERMINAL-TRANSITION-KINDS
  '(wave-done wave-failed campaign-complete campaign-cancelled stall-terminal budget-pause))

(define (terminal-transition-kind? v)
  (and (memq v TERMINAL-TRANSITION-KINDS) #t))

(define KIND-LABELS
  '((wave-done . "wave done") (wave-failed . "wave-failed")
                              (campaign-complete . "campaign complete")
                              (campaign-cancelled . "campaign cancelled")
                              (stall-terminal . "stall-terminal")
                              (budget-pause . "budget pause")))

;; ── Notification value ───────────────────────────────────────

(struct gsd-notification (campaign-id wave-idx kind reason spend timestamp) #:transparent)

(define (make-gsd-notification campaign-id wave-idx kind #:reason [reason ""] #:spend [spend #f])
  (unless (terminal-transition-kind? kind)
    (raise-argument-error 'make-gsd-notification "terminal transition kind" kind))
  (gsd-notification campaign-id wave-idx kind (one-line reason) spend (current-seconds)))

(define (one-line s)
  (string-trim (string-normalize-spaces (string-replace s "\n" " "))))

;; Message format (BUG-0040 action 4): campaign id, wave idx, outcome
;; kind, one-line reason, spend when W5 recorded one.
(define (gsd-notification-message n)
  (format "gsd[~a] ~a~a: ~a~a"
          (gsd-notification-campaign-id n)
          (cond
            [(gsd-notification-wave-idx n)
             =>
             (lambda (idx) (format "wave ~a " idx))]
            [else ""])
          (cdr (assq (gsd-notification-kind n) KIND-LABELS))
          (if (string=? (gsd-notification-reason n) "")
              "no reason recorded"
              (gsd-notification-reason n))
          (cond
            [(gsd-notification-spend n)
             =>
             (lambda (s) (format " (spend $~a)" (~r s #:precision '(= 2))))]
            [else ""])))

;; ── Warn-once registry ───────────────────────────────────────
;; Unknown/misconfigured/hanging sinks warn ONCE per key per process
;; and are skipped thereafter — no log spam, no campaign impact.

(define warned-keys (make-hasheq))

(define (warn-once! key fmt . args)
  (unless (hash-ref warned-keys key #f)
    (hash-set! warned-keys key #t)
    (log-warning (apply format (string-append "gsd-notify: " fmt) args)))
  (void))

(define (reset-gsd-notify-warnings!)
  (hash-clear! warned-keys))

;; ── Sink protocol ────────────────────────────────────────────
;; id      — warn-once key + telemetry name
;; emitter — gsd-notification? -> any; failures/hangs are caught by
;;           notify-terminal-transition! (never by the emitter).

(struct gsd-notify-sink (id emitter) #:transparent)

;; Per-sink hard wall-clock bound (BUG-0040 action 5). Generous for
;; curl/network, short enough that a hung sink never stalls a
;; terminal transition for long.
(define NOTIFY-TIMEOUT-SECONDS 10)

;; Runs the emitter in a custodian-owned worker thread. On timeout
;; the custodian is shut down: the thread AND any subprocess it
;; spawned (tmux/sh/curl) are killed. Returns #t on success.
(define (emit-with-timeout! sink notif)
  (define cust (make-custodian))
  (define worker
    (parameterize ([current-custodian cust])
      (thread (lambda ()
                (parameterize ([current-custodian cust])
                  ((gsd-notify-sink-emitter sink) notif))))))
  (define finished (sync/timeout NOTIFY-TIMEOUT-SECONDS worker))
  (custodian-shutdown-all cust)
  (cond
    [(not finished)
     (warn-once! (gsd-notify-sink-id sink)
                 "sink ~a timed out after ~as — skipped"
                 (gsd-notify-sink-id sink)
                 NOTIFY-TIMEOUT-SECONDS)
     #f]
    ;; Thread died without finishing = raised (handler already
    ;; warned) — count as attempted-but-failed.
    [(thread-dead? worker) #f]
    [else #t]))

(define (notify-terminal-transition! sinks notif)
  (unless (gsd-notification? notif)
    (raise-argument-error 'notify-terminal-transition! "gsd-notification?" notif))
  (for ([sink (in-list sinks)])
    (with-handlers ([exn:fail? (lambda (e)
                                 (warn-once! (gsd-notify-sink-id sink)
                                             "sink ~a failed: ~a"
                                             (gsd-notify-sink-id sink)
                                             (exn-message e)))])
      (emit-with-timeout! sink notif)))
  ;; In-process fan-out parity (BUG-0040): exactly one telemetry
  ;; event per terminal transition, and only when a sink could have
  ;; observed it — silent default stays silent.
  (unless (null? sinks)
    (with-handlers ([exn:fail? void])
      (emit-gsd-event! 'gsd.campaign.notified
                       (hasheq 'kind
                               (gsd-notification-kind notif)
                               'wave
                               (gsd-notification-wave-idx notif)
                               'reason
                               (gsd-notification-reason notif)
                               'spend
                               (gsd-notification-spend notif)
                               'sinks
                               (map gsd-notify-sink-id sinks)))))
  (void))

;; ── Concrete sinks ───────────────────────────────────────────

;; tmux display-message + terminal bell. Active ONLY when $TMUX is
;; set (BUG-0040 action 2): inside tmux it needs zero configuration;
;; outside tmux there is no default side effect at all.
(define (tmux-sink)
  (gsd-notify-sink 'tmux
                   (lambda (notif)
                     (define tmux-bin (find-executable-path "tmux"))
                     (unless tmux-bin
                       (error 'gsd-notify/tmux "$TMUX is set but no tmux binary on PATH"))
                     (define-values (p _out _in _err)
                       (apply subprocess
                              #f
                              #f
                              #f
                              tmux-bin
                              (list "display-message"
                                    "-d"
                                    (format "~a" (* 10 NOTIFY-TIMEOUT-SECONDS))
                                    (gsd-notification-message notif))))
                     (subprocess-wait p)
                     (define code (subprocess-status p))
                     (unless (eqv? code 0)
                       (error 'gsd-notify/tmux "tmux display-message exited ~a" code))
                     ;; Terminal bell: byte 7 on the campaign's stdout. Best-effort;
                     ;; a closed/dead port must not fail the transition.
                     (with-handlers ([exn:fail? void])
                       (fprintf (current-output-port) "~a" (integer->char 7))
                       (flush-output (current-output-port))))))

;; {message} {kind} {campaign} {wave} {reason} {spend} template.
(define (render-template tmpl notif)
  (define (sub s placeholder value)
    (string-replace s placeholder (format "~a" value)))
  (let* ([s (sub tmpl "{message}" (gsd-notification-message notif))]
         [s (sub s "{kind}" (gsd-notification-kind notif))]
         [s (sub s "{campaign}" (gsd-notification-campaign-id notif))]
         [s (sub s "{wave}" (or (gsd-notification-wave-idx notif) "campaign"))]
         [s (sub s "{reason}" (gsd-notification-reason notif))]
         [s (sub s
                 "{spend}"
                 (or (and (gsd-notification-spend notif)
                          (~r (gsd-notification-spend notif) #:precision '(= 2)))
                     "unknown"))])
    s))

(define (shell-sink id command-template)
  (gsd-notify-sink
   id
   (lambda (notif)
     (define sh (or (find-executable-path "sh") (find-executable-path "/bin/sh")))
     (unless sh
       (error 'gsd-notify/shell "no shell available to run ~a" id))
     (define-values (p _out _in _err)
       (apply subprocess #f #f #f sh (list "-c" (render-template command-template notif))))
     (subprocess-wait p)
     (define code (subprocess-status p))
     (unless (eqv? code 0)
       (error 'gsd-notify/shell "~a exited ~a" id code)))))

(define (notification->jsexpr notif)
  (hasheq 'campaign
          (gsd-notification-campaign-id notif)
          'wave
          (gsd-notification-wave-idx notif)
          'kind
          (gsd-notification-kind notif)
          'reason
          (gsd-notification-reason notif)
          'spend
          (gsd-notification-spend notif)
          'timestamp
          (gsd-notification-timestamp notif)))

;; POST a small JSON payload via curl (present everywhere this repo
;; already shells out; keeps the module stdlib-only). Our own
;; wall-clock guard bounds it even if curl's -m is bypassed.
(define (webhook-sink url)
  (gsd-notify-sink 'webhook
                   (lambda (notif)
                     (define curl (find-executable-path "curl"))
                     (unless curl
                       (error 'gsd-notify/webhook "gsd.notify.webhook-url set but no curl on PATH"))
                     (define p
                       (apply subprocess
                              #f
                              #f
                              #f
                              curl
                              (list "-sS"
                                    "-m"
                                    (format "~a" (sub1 NOTIFY-TIMEOUT-SECONDS))
                                    "-o"
                                    "/dev/null"
                                    "-w"
                                    "%{http_code}"
                                    "-X"
                                    "POST"
                                    "-H"
                                    "Content-Type: application/json"
                                    "-d"
                                    (jsexpr->string (notification->jsexpr notif))
                                    url)))
                     (subprocess-wait p)
                     (define code (subprocess-status p))
                     (unless (eqv? code 0)
                       (error 'gsd-notify/webhook "curl exited ~a" code)))))

;; Test doubles.
(define (make-recording-sink box)
  (gsd-notify-sink 'fake-record (lambda (notif) (set-box! box (cons notif (unbox box))))))

(define (make-raising-sink)
  (gsd-notify-sink 'raising (lambda (notif) (error 'raising-sink "configured to fail (test)"))))

;; ── Settings → sinks ─────────────────────────────────────────

(define (gsd-notify-sinks-from-settings settings)
  (define sinks '())
  ;; tmux default: silent OFF outside tmux (BUG-0040 action 2).
  (define tmux-env (getenv "TMUX"))
  (when (and tmux-env (positive? (string-length tmux-env)))
    (set! sinks (cons (tmux-sink) sinks)))
  (define desktop-cmd (gsd-notify-desktop-command settings))
  (when desktop-cmd
    (set! sinks (cons (shell-sink 'desktop-command desktop-cmd) sinks)))
  (define hook-url (gsd-notify-webhook-url settings))
  (when hook-url
    (cond
      [(or (string-prefix? hook-url "http://") (string-prefix? hook-url "https://"))
       (set! sinks (cons (webhook-sink hook-url) sinks))]
      [else
       ;; Misconfigured sink: warn once, skip, keep going.
       (warn-once! 'webhook
                   "gsd.notify.webhook-url ~s is not an http(s) URL — sink skipped"
                   hook-url)]))
  (reverse sinks))

;; ── Orchestrator convenience ─────────────────────────────────
;; go-orchestrator parameterizes current-gsd-notify-sinks at
;; campaign start (sinks resolved ONCE from project settings); every
;; terminal-transition site calls notify-terminal-transition*!,
;; which can never raise into the campaign.

(define current-gsd-notify-sinks (make-parameter '()))

(define (notify-terminal-transition*! campaign-id
                                      wave-idx
                                      kind
                                      #:reason [reason ""]
                                      #:spend [spend #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "gsd-notify: transition ~a fan-out failed: ~a"
                                                    kind
                                                    (exn-message e))))])
    (define notif (make-gsd-notification campaign-id wave-idx kind #:reason reason #:spend spend))
    (notify-terminal-transition! (current-gsd-notify-sinks) notif)))
