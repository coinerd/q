#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 1200
;; Genuine retained proposal + keyboard broker flow. No live paths or model calls.
(require rackunit
         json
         racket/file
         racket/port
         racket/runtime-path
         "helpers/supplemental-go-fixture.rkt"
         "helpers/honest-delivery-fixture.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-operator.rkt"
         "../runtime/approval/broker.rkt"
         "../tui/state-types.rkt"
         "../tui/state-events/registry.rkt"
         (only-in "../tui/context.rkt" tui-ctx-input-state-box)
         (only-in "../tui/input.rkt" input-insert-string)
         (only-in "../tui/message-dispatch.rkt" process-tui-message!)
         (prefix-in shared: "../tui/tui-keybindings.rkt")
         "../tui/commands/evidence-owner.rkt"
         "../util/event/event.rkt"
         "../util/event/event-bus.rkt")
(define-runtime-path replan-cli "../scripts/gsd-evidence-replan.rkt")

(module+ test
  (test-case "CLI retains inert child; exact keyboard approval migrates without launching or resetting parent"
    ;; The standard fixture puts private storage under its campaign base. TUI
    ;; uses that base as cwd, so relocate the genuine store outside it before
    ;; invoking frontend APIs (the authority rightly refuses project-local storage).
    (define private-home (make-temporary-file "supplemental-frontend-private-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (call-with-supplemental-fixture
        (lambda (fx)
          (define base (supplemental-fixture-root fx))
          (define plan (supplemental-fixture-plan fx))
          (define private (build-path private-home "private"))
          (rename-file-or-directory (supplemental-fixture-private fx) private)
          (parameterize ([current-supplemental-authority-root private])
            (fixture-finalize-predecessor! fx)
            (define calls 0)
            (define fallback 0)
            (void (run-campaign!
                   base
                   (load-campaign-record base plan)
                   #:isolate? #f
                   #:timeout-sec 30
                   #:delivery-reader (test-delivered-reader)
                   #:delivery-coordinator (lambda _
                                            (set! fallback (add1 fallback))
                                            (error 'frontend "ordinary fallback forbidden"))
                   #:runner (lambda (_)
                              (set! calls (add1 calls))
                              'ok)
                   #:verifier (lambda _ (error 'frontend "ordinary verifier forbidden"))))
            (check-equal? calls 1)
            (check-equal? fallback 0)
            (define parent (load-supplemental-pointer base plan 1))
            (define parent-ref (hash-ref parent 'task-proposal))
            (define parent-approval (hash-ref parent 'task-approval))
            (define parent-op-path
              (build-path private
                          "supplemental-operations"
                          (string-append (hash-ref parent-approval 'sha256) ".json")))
            (define parent-bytes (file->bytes parent-op-path))
            (check-equal? (hash-ref (load-supplemental-operation parent-approval) 'state) "blocked")
            ;; CLI uses trusted-user defaults, not the fixture's in-process
            ;; proof-root parameter. Give its child process an isolated HOME
            ;; containing the authentic captured proof store at that default.
            (define proof-default (build-path private-home ".q" "agent" "owned-review-proofs"))
            (make-parent-directory* proof-default)
            (copy-directory/files (build-path base "owned-proofs") proof-default)
            (define env (environment-variables-copy (current-environment-variables)))
            (environment-variables-set! env #"HOME" (path->bytes private-home))
            (define-values (process stdout stdin stderr)
              (parameterize ([current-environment-variables env])
                (subprocess #f
                            #f
                            #f
                            (find-executable-path "racket")
                            replan-cli
                            "--authority-root"
                            private
                            "--verb"
                            "propose-recovery"
                            "--campaign-root"
                            base
                            "--plan"
                            plan
                            "--wave"
                            "1"
                            "--sha256"
                            (hash-ref parent-ref 'sha256)
                            "--expires-at"
                            (number->string (+ (current-seconds) 3600)))))
            (close-output-port stdin)
            (define output (port->string stdout))
            (define errors (port->string stderr))
            (subprocess-wait process)
            (check-equal? (subprocess-status process) 0 errors)
            (define retained (string->jsexpr output))
            (define child-ref
              (hasheq 'kind
                      "task-proposal"
                      'sha256
                      (string->immutable-string (hash-ref retained 'sha256))))
            (check-not-equal? child-ref parent-ref)
            (check-equal? (load-supplemental-pointer base plan 1) parent)
            (parameterize ([current-directory base])
              (define bus (make-event-bus))
              (define ctx
                (shared:make-tui-ctx #:session-runner void #:event-bus bus #:session-queue #f))
              (define sb (shared:tui-ctx-ui-state-box ctx))
              (define shown (make-semaphore 0))
              (define requests 0)
              (subscribe! bus
                          (lambda (evt)
                            (set-box! sb (apply-event-to-state (unbox sb) evt))
                            (when (equal? (event-ev evt) "supplemental-evidence.approval-requested")
                              (set! requests (add1 requests))
                              (semaphore-post shown))))
              (define raw
                (format "/evidence-replan authorize-recovery ~a 1 ~a"
                        plan
                        (hash-ref child-ref 'sha256)))
              (shared:process-slash-command ctx 'unknown raw)
              (check-equal? requests 0)
              (define ib (tui-ctx-input-state-box ctx))
              (set-box! ib (input-insert-string (unbox ib) raw))
              (process-tui-message! ctx '(key enter))
              (check-not-false (sync/timeout 2 shown))
              (define overlay (ui-state-active-overlay (unbox sb)))
              (check-not-false overlay)
              (when overlay
                (define extra (overlay-state-extra overlay))
                (check-equal? (hash-ref extra 'commitment-digest) (hash-ref child-ref 'sha256))
                (check-equal? (hash-ref (hash-ref extra 'approval-view) 'record-canonical-json)
                              (hash-ref (supplemental-approval-presentation child-ref)
                                        'record-canonical-json))
                (check-false
                 (approval-decide! (hash-ref extra 'request-id) (hash-ref parent-ref 'sha256) #t))
                (check-equal? (load-supplemental-pointer base plan 1) parent)
                (process-tui-message! ctx '(key #\y))
                (let wait ([tries 500])
                  (cond
                    [(equal? (hash-ref (load-supplemental-pointer base plan 1) 'task-proposal)
                             child-ref)
                     (void)]
                    [(positive? tries)
                     (sleep 0.01)
                     (wait (sub1 tries))]
                    [else
                     (fail (format "keyboard recovery did not install: ~s"
                                   (ui-state-transcript (unbox sb))))]))
                (define installed (load-supplemental-pointer base plan 1))
                (define approval (hash-ref installed 'task-approval))
                (check-not-false (validate-supplemental-approval approval child-ref))
                (check-false (load-supplemental-operation approval)
                             "approval must not initialize or launch child")
                (check-equal? calls 1 "approval is not /go")
                (check-equal? fallback 0)
                (check-equal? (file->bytes parent-op-path) parent-bytes)
                (handle-owner-evidence-command!
                 ctx
                 (format "/evidence-replan authorize-task ~a 1 ~a" plan (hash-ref child-ref 'sha256)))
                (check-equal? requests
                              1
                              "ordinary authorize-task must refuse recovery before asking broker")
                (check-equal? (pending-approval-count) 0)))))
        #:completed-author? #t
        #:physical-worktree? #t))
     (lambda () (delete-directory/files private-home)))))
