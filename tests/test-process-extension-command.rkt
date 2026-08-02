#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-process-extension-command.rkt -- Tests for process-extension-command (N-10)

(require rackunit
         "../tui/commands.rkt"
         "../tui/state-types.rkt"
         (only-in "../extensions/gsd/campaign-state.rkt" migrate-campaign!)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  make-campaign-request
                  register-campaign-request!)
         (only-in "../util/loop-result.rkt" make-loop-result)
         racket/file)

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

(test-case "campaign token uses one dedicated runner for isolated waves"
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
     (check-equal? factory-count 1))
   (lambda () (delete-directory/files dir #:must-exist? #f))))
