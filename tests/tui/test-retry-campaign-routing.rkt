#lang racket/base

;; @speed fast  ;; @suite tui

;; BOUNDARY: contract

;; tests/tui/test-retry-campaign-routing.rkt — D2 remediation (issue #9351)
;;
;; Incident 01M05VA2: /retry of a GSD wave EXECUTE prompt resubmitted the
;; prompt in the main session. The wave completed there, but /retry has no
;; campaign wiring, so durable campaign state never advanced (record stayed
;; at "failed attempt-3", lease never rewritten, outbox stale, W3 never
;; dispatched) — the campaign wedged at the completion boundary.
;;
;; Fix: handle-retry-command detects `[gsd-planning] EXECUTE` prompts and
;; routes them through the extension /go pipeline via the cmd-ctx
;; ext-command-dispatcher seam (populated by tui-ctx->cmd-ctx). Plain
;; prompts keep the legacy resubmit path.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../../tui/state.rkt" initial-ui-state ui-state-transcript)
         (only-in "../../tui/commands/context.rkt"
                  cmd-ctx
                  cmd-ctx-state-box
                  cmd-ctx-last-prompt-box
                  cmd-ctx-session-runner
                  cmd-ctx-input-text-box
                  cmd-ctx-needs-redraw-box
                  ext-command-dispatcher-box)
         (only-in "../../tui/commands/runtime-control.rkt" handle-retry-command))

;; ── helpers ──

(define gsd-execute-prompt "[gsd-planning] EXECUTE the plan below. IMPLEMENT NOW — do NOT explore.")

(define plain-prompt "Fix the failing test in foo.rkt")

(define (make-test-cctx #:prompt [prompt #f] #:runner [runner #f])
  (cmd-ctx (box (initial-ui-state))
           (box #t) ;; running
           #f ;; event-bus
           #f ;; session-dir
           (box #f) ;; needs-redraw
           #f ;; model-registry-box
           (box prompt) ;; last-prompt-box
           runner ;; session-runner
           (box "") ;; input-text-box
           (box #f) ;; extension-registry-box
           #f ;; session-factory-runner
           (box #f) ;; agent-session-box
           (box #f))) ;; goal-cancel-box

;; The D2 seam is a module-level box; tests must set and restore it.
(define (with-dispatcher dispatcher thunk)
  (define saved (unbox ext-command-dispatcher-box))
  (dynamic-wind (lambda () (set-box! ext-command-dispatcher-box dispatcher))
                thunk
                (lambda () (set-box! ext-command-dispatcher-box saved))))

(define (transcript-joined cctx)
  (define entries (ui-state-transcript (unbox (cmd-ctx-state-box cctx))))
  (string-join (map (lambda (e) (format "~a" e)) entries) "\n"))

;; /retry spawns a thread for the legacy resubmit; poll the box briefly.
(define (wait-for! box [timeout-ms 2000])
  (define deadline (+ (current-inexact-milliseconds) timeout-ms))
  (let loop ()
    (when (and (not (unbox box)) (< (current-inexact-milliseconds) deadline))
      (sync (alarm-evt (+ (current-inexact-milliseconds) 10)))
      (loop))))

;; ── D2: campaign-aware /retry ──

(define retry-routing-suite
  (test-suite "retry campaign routing (D2, issue #9351)"

    (test-case "GSD EXECUTE prompt routes through the campaign dispatcher"
      (define dispatched (box #f))
      (define runner-called (box #f))
      (define cctx
        (make-test-cctx #:prompt gsd-execute-prompt
                        #:runner (lambda (_prompt) (set-box! runner-called #t))))
      (with-dispatcher (lambda (cctx _state)
                         (set-box! dispatched (unbox (cmd-ctx-input-text-box cctx))))
                       (lambda () (handle-retry-command cctx (unbox (cmd-ctx-state-box cctx)))))
      (check-equal? (unbox dispatched) "/go" "dispatcher must receive /go as input text")
      (check-false (unbox runner-called) "plain resubmit must be bypassed for GSD prompts")
      (check-true (regexp-match? #rx"campaign" (transcript-joined cctx))
                  "transcript must explain the campaign routing"))

    (test-case "plain prompt keeps legacy resubmit even with dispatcher present"
      (define dispatched (box #f))
      (define runner-called (box #f))
      (define cctx
        (make-test-cctx #:prompt plain-prompt
                        #:runner (lambda (_prompt) (set-box! runner-called #t))))
      (with-dispatcher (lambda (_cctx _state) (set-box! dispatched #t))
                       (lambda () (handle-retry-command cctx (unbox (cmd-ctx-state-box cctx)))))
      (wait-for! runner-called)
      (check-true (unbox runner-called) "plain prompts must resubmit via the session runner")
      (check-false (unbox dispatched) "dispatcher must not run for plain prompts"))

    (test-case "GSD prompt without dispatcher falls back to legacy resubmit"
      (define runner-called (box #f))
      (define cctx
        (make-test-cctx #:prompt gsd-execute-prompt
                        #:runner (lambda (_prompt) (set-box! runner-called #t))))
      (with-dispatcher #f (lambda () (handle-retry-command cctx (unbox (cmd-ctx-state-box cctx)))))
      (wait-for! runner-called)
      (check-true (unbox runner-called)
                  "without a dispatcher the legacy path must remain available"))))

(module+ test
  (void (run-tests retry-routing-suite)))
