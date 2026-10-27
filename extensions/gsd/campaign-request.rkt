#lang racket/base

;; extensions/gsd/campaign-request.rkt — the interface-safe /go request value.
;;
;; W3 extraction: the campaign-request value, its constructor, and the
;; process-local request-token registry moved OUT of go-orchestrator.rkt so the
;; interpreter stays under its size pin. Nothing here decides or routes: the
;; struct carries durable campaign identity plus the callbacks the interpreter
;; needs (one wave prompt builder, one verifier, one authenticated
;; delivery-reader, one delivery coordinator, one automatic-review runner), and
;; the registry only maps an opaque token to a value so TUI/GUI/SDK hook
;; payloads never carry higher-order values.
;;
;; Execution stays in go-orchestrator.rkt (execute-campaign-request! /
;; execute-campaign-token! / run-campaign!); this module is the value boundary
;; plus the ONE attempt-prompt wrapper that turns the request's scratch token
;; into this attempt's advertised root.

(require racket/format
         racket/string
         (only-in "campaign-state.rkt" campaign-plan-id)
         (only-in "delivery-handoff.rkt" delivery-readback)
         ;; W2: the ONE trusted per-session scratch allocation. The token lives on
         ;; the request; the allocation itself belongs to tools/scratch.rkt.
         (only-in "../../tools/scratch.rkt"
                  allocate-session-scratch-root!
                  call-with-adopted-session-scratch-root
                  release-session-scratch-root!))

(provide campaign-request
         campaign-request?
         make-campaign-request
         campaign-request-base-dir
         campaign-request-record
         campaign-request-prompt-for-wave
         campaign-request-verifier
         campaign-request-timeout-sec
         campaign-request-allow-stale?
         campaign-request-delivery-reader
         campaign-request-delivery-coordinator
         campaign-request-auto-review-runner
         campaign-request-scratch-token
         request-attempt-scratch-key
         call-with-request-scratch-prompt
         register-campaign-request!
         lookup-campaign-request
         unregister-campaign-request!)

;; A campaign request is the interface-safe execution boundary for /go.  It
;; carries durable campaign identity plus callbacks that build one wave prompt
;; and verify one completed attempt; interfaces supply only the prompt runner.
;; delivery-reader: authenticated delivered-proof reader (default
;; delivery-readback) — the DI seam so tests inject proofs without network.
;; timeout-sec: per-campaign override for the wave budget (current-gsd-wave-
;; timeout-seconds), resolved at /go time from the --wave-timeout=SECONDS flag,
;; then ~/.q/config.json wave-timeout-seconds, then the parameter default.
;; #f → use the current parameter value at execution time. Carried on the
;; request (not the parameter) because the campaign runs in a separate thread.
;; scratch-token: the opaque runtime-allocated token for this /go campaign's
;; executor scratch ownership (tools/scratch.rkt). The campaign interpreter
;; allocates exactly one root per executor attempt from it, advertises that
;; root in the prompt, and adopts it for the very session that prompt creates;
;; nothing about scratch ever comes from model text or a process-wide default.
(struct campaign-request
        (base-dir record
                  prompt-for-wave
                  verifier
                  timeout-sec
                  allow-stale?
                  delivery-reader
                  delivery-coordinator
                  auto-review-runner
                  scratch-token)
  #:transparent
  #:constructor-name make-campaign-request/internal)

(define (make-campaign-request base-dir
                               record
                               prompt-for-wave
                               verifier
                               #:timeout-sec [timeout-sec #f]
                               #:allow-stale? [allow-stale? #f]
                               #:delivery-reader [delivery-reader delivery-readback]
                               #:delivery-coordinator [delivery-coordinator #f]
                               #:auto-review-runner [auto-review-runner #f]
                               #:scratch-token [scratch-token #f])
  (make-campaign-request/internal base-dir
                                  record
                                  prompt-for-wave
                                  verifier
                                  timeout-sec
                                  allow-stale?
                                  delivery-reader
                                  delivery-coordinator
                                  auto-review-runner
                                  scratch-token))

;; ============================================================
;; One attempt's prompt, with the request's scratch ownership
;; ============================================================

;; The registry key that holds ONE attempt's allocation for exactly as long as
;; that attempt runs. It is bookkeeping only -- never a path component -- and is
;; derived from the request's opaque token plus the wave index so the attempt can
;; release exactly the entry it created. A random, never-released key would leak
;; one registry entry per retry and keep an otherwise-dead root listed as live.
(define (request-attempt-scratch-key scratch-token wave-idx)
  (unless (and (string? scratch-token) (positive? (string-length scratch-token)))
    (raise-argument-error 'request-attempt-scratch-key "non-empty-string?" scratch-token))
  (format "campaign-attempt:~a:~a" scratch-token wave-idx))

;; W2/W5: the ONE place that turns a request's scratch token into THIS
;; attempt's advertised root. The root is allocated fresh per attempt (a retry
;; never inherits the previous attempt's directory), rendered into the prompt
;; only when the builder accepts the root argument, and adopted single-shot
;; around the runner so exactly the session the interface creates for this
;; attempt can reach it. Nothing is process-wide: a request without a token has
;; no scratch at all. The allocation effect is delegated to tools/scratch.rkt.
;;
;; The temporary registry entry exists only for the duration of the attempt (it
;; is released in the dynamic-wind exit, so no attempt leaks an entry). The
;; session that adopts the root is bound by the runtime under its OWN session id
;; (runtime/agent-session.rkt), so releasing the temporary entry leaves the
;; adopted session's binding -- the real ownership -- untouched.
;;
;; The runner protocol is preserved: runtime/session runners return either a
;; single loop-result or (values updated-session result); the session belongs to
;; the interface, so the result is what comes back.
(define (call-with-request-scratch-prompt request wave-idx run-prompt)
  (define scratch-token (campaign-request-scratch-token request))
  (define prompt-for-wave (campaign-request-prompt-for-wave request))
  (define (dispatch! scratch-root)
    (define prompt
      (if (and scratch-root (procedure-arity-includes? prompt-for-wave 2))
          (prompt-for-wave wave-idx scratch-root)
          (prompt-for-wave wave-idx)))
    (define returned-values
      (call-with-values (lambda ()
                          (if scratch-root
                              (call-with-adopted-session-scratch-root scratch-root
                                                                      (lambda () (run-prompt prompt)))
                              (run-prompt prompt)))
                        list))
    (if (= (length returned-values) 2)
        (cadr returned-values)
        (and (pair? returned-values) (car returned-values))))
  (cond
    [(not scratch-token) (dispatch! #f)]
    [else
     (define attempt-key (request-attempt-scratch-key scratch-token wave-idx))
     (define scratch-root (allocate-session-scratch-root! attempt-key))
     (dynamic-wind void
                   (lambda () (dispatch! scratch-root))
                   (lambda () (release-session-scratch-root! attempt-key)))]))

;; Hook payloads cross a Typed Racket Any boundary that intentionally rejects
;; higher-order values. Keep callbacks process-local and send only an opaque
;; token through TUI/GUI/SDK hook payloads.
(define campaign-request-registry (make-hash))
(define campaign-request-registry-lock (make-semaphore 1))

(define (register-campaign-request! request)
  (define token
    (format "~a-~a-~a"
            (campaign-plan-id (campaign-request-record request))
            (current-inexact-milliseconds)
            (random 1000000000)))
  (call-with-semaphore campaign-request-registry-lock
                       (lambda () (hash-set! campaign-request-registry token request)))
  token)

(define (lookup-campaign-request token)
  (call-with-semaphore campaign-request-registry-lock
                       (lambda () (hash-ref campaign-request-registry token #f))))

(define (unregister-campaign-request! token)
  (call-with-semaphore campaign-request-registry-lock
                       (lambda () (hash-remove! campaign-request-registry token))))
