#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-process-extension-command.rkt -- Tests for process-extension-command (N-10)

(require rackunit
         "../tui/commands.rkt"
         "../tui/state-types.rkt"
         (only-in "../tui/commands/runtime-control.rkt" handle-retry-command)
         (only-in "../extensions/gsd/campaign-state.rkt" migrate-campaign!)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  make-campaign-request
                  register-campaign-request!)
         (only-in "../util/loop-result.rkt" make-loop-result)
         racket/file
         racket/string)

;; ============================================================
;; N-10: process-extension-command basic interface tests
;; ============================================================

(define (make-test-cctx input-text #:factory [factory #f])
  (define state-box (box (initial-ui-state)))
  (define input-box (box input-text))
  (define redraw-box (box #f))
  (cmd-ctx state-box ;; state-box
           (box #f) ;; running-box
           #f ;; event-bus
           #f ;; session-dir
           redraw-box ;; needs-redraw-box
           (box #f) ;; model-registry-box
           (box #f) ;; last-prompt-box
           (box #f) ;; session-runner
           input-box ;; input-text-box
           (box #f) ;; extension-registry-box (no extensions)
           factory ;; session-factory-runner
           (box #f) ;; agent-session-box
           (box #f))) ;; goal-cancel-box

(test-case "process-extension-command is a procedure"
  (check-true (procedure? process-extension-command)))

(test-case "process-extension-command with no extensions returns 'continue"
  (define cctx (make-test-cctx ""))
  (define result (process-extension-command cctx (initial-ui-state)))
  (check-equal? result 'continue))

(test-case "process-extension-command with non-slash input returns 'continue"
  (define cctx (make-test-cctx "hello world"))
  (define result (process-extension-command cctx (initial-ui-state)))
  (check-equal? result 'continue))

(test-case "process-extension-command with slash but no extensions returns 'continue"
  (define cctx (make-test-cctx "/unknown-command arg1"))
  (define result (process-extension-command cctx (initial-ui-state)))
  (check-equal? result 'continue))

;; P1 regression: submit with no runner should produce visible error
;; Test the execute-extension-command indirectly by mocking the hook result.
;; When runner=#f and submit payload arrives, transcript must grow.
(test-case "execute-extension-command submit with no runner shows error"
  (define cctx (make-test-cctx "/plan test task"))
  (define initial-transcript (ui-state-transcript (unbox (cmd-ctx-state-box cctx))))
  ;; Simulate what process-extension-command does on amend with submit payload
  (execute-extension-command cctx
                             (unbox (cmd-ctx-state-box cctx))
                             (hasheq 'submit "plan the project" 'text "Planning: test task"))
  (define updated-transcript (ui-state-transcript (unbox (cmd-ctx-state-box cctx))))
  (check >
         (length updated-transcript)
         (length initial-transcript)
         "submit with no runner should add error to transcript"))

(test-case "campaign token fails closed without a fresh-session factory"
  (define cctx (make-test-cctx "/go"))
  (execute-extension-command cctx
                             (unbox (cmd-ctx-state-box cctx))
                             (hasheq 'campaign-token "unresolved-token" 'text "starting"))
  (define texts (map transcript-entry-text (ui-state-transcript (unbox (cmd-ctx-state-box cctx)))))
  (check-true (ormap (lambda (text) (string-contains? text "No fresh session factory")) texts)))

(test-case "campaign token creates one fresh runner per isolated wave"
  (define dir (make-temporary-file "tui-campaign-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning" "waves"))
     (call-with-output-file (build-path dir ".planning" "PLAN.md")
                            (lambda (out)
                              (display "# Plan: TUI\n- [Inbox] W0: One\n- [Inbox] W1: Two\n" out))
                            #:exists 'truncate)
     (define rec (migrate-campaign! dir))
     (define request
       (make-campaign-request dir rec (lambda (idx) (format "isolated-W~a" idx)) (lambda (_) #t)))
     (define token (register-campaign-request! request))
     (define prompt-channel (make-channel))
     (define factory-count 0)
     (define cctx
       (make-test-cctx "/go"
                       #:factory
                       (case-lambda
                         [()
                          (set! factory-count (add1 factory-count))
                          (lambda (prompt)
                            (channel-put prompt-channel prompt)
                            (values 'updated-session (make-loop-result '() 'completed (hasheq))))]
                         [(prompt) (error 'test "legacy path must not run: ~a" prompt)])))
     (execute-extension-command
      cctx
      (unbox (cmd-ctx-state-box cctx))
      (hasheq 'campaign-token token 'new-session "legacy-all-plan" 'text "starting"))
     (check-equal? (sync/timeout 2 prompt-channel) "isolated-W0")
     (check-equal? (sync/timeout 2 prompt-channel) "isolated-W1")
     (check-equal? factory-count 2))
   (lambda () (delete-directory/files dir #:must-exist? #f))))

;; v0.99.97 regression: after a /go campaign fails, /retry must be able to
;; resubmit the last wave prompt. Root cause: make-campaign-runner runs wave
;; prompts on a dedicated campaign session that is discarded when
;; execute-campaign-command restores the pre-campaign session. The wave prompt
;; therefore never reached the TUI last-prompt-box (slash commands don't set
;; it, and the restored pre-campaign session has no last-user-prompt), so
;; /retry reported "No previous prompt to retry." even though the
;; circuit-breaker explicitly told the user to type /retry.
(test-case "/retry resubmits wave prompt after failed campaign"
  (define last-prompt-box (box #f))
  (define submitted (box '()))
  ;; Simulate make-campaign-runner's returned closure (v0.99.97 fix): it
  ;; records each wave prompt into the shared last-prompt box before running
  ;; it on the campaign session.
  (define (campaign-runner prompt)
    (set-box! last-prompt-box prompt)
    (set-box! submitted (cons prompt (unbox submitted)))
    (values 'campaign-session (make-loop-result '() 'failed (hasheq 'reason "wave-failed"))))
  (define state-box (box (initial-ui-state)))
  (define cctx
    (cmd-ctx state-box
             (box #f)
             #f
             #f
             (box #f)
             (box #f)
             last-prompt-box
             (lambda (prompt) (set-box! submitted (cons prompt (unbox submitted))))
             (box "")
             (box #f)
             #f
             (box #f)
             (box #f)))
  ;; Simulate the campaign having run a wave and then failed:
  (campaign-runner "Execute wave W3 from the campaign plan...")
  (check-equal? (unbox last-prompt-box) "Execute wave W3 from the campaign plan...")
  ;; Now /retry must resubmit the recorded wave prompt, not report
  ;; "No previous prompt to retry."
  (handle-retry-command cctx (unbox state-box))
  (sleep 0.05)
  (define retried (unbox submitted))
  (check-true (pair? retried))
  (check-equal? (car retried) "Execute wave W3 from the campaign plan...")
  (define transcript (ui-state-transcript (unbox state-box)))
  (check-false (for/or ([e transcript])
                 (and (eq? (transcript-entry-kind e) 'error)
                      (string-contains? (transcript-entry-text e) "No previous prompt to retry.")))))
