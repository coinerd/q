#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

(require rackunit
         rackunit/text-ui
         racket/string
         racket/class
         "../gui/gui-types.rkt"
         "../gui/slash-commands.rkt"
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/disclosure-state.rkt"
         "../runtime/session/session-types.rkt"
         (only-in "../runtime/session/lifecycle-state.rkt" make-lifecycle-state)
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../util/hook-types.rkt"
         "../agent/queue.rkt"
         (only-in "../runtime/agent-session.rkt" make-agent-session session-id)
         (only-in "../tools/tool.rkt" make-tool-registry)
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         "helpers/mock-provider.rkt"
         racket/file)

(define test-add-system-msg
  (test-suite "add-system-msg!"
    (test-case "adds a system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (add-system-msg! "hello" state-box lock)
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "system")
      (check-equal? (gui-message-text (car msgs)) "hello"))

    (test-case "gui-slash-commands: appends to existing messages"
      (define state-box (box (make-gui-state #:messages (list (make-gui-message "user" "hi")))))
      (define lock (make-semaphore 1))
      (add-system-msg! "sys" state-box lock)
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 2)
      (check-equal? (gui-message-role (cadr msgs)) "system"))

    (test-case "add-system-msg! calls notify callback"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define notify-called? (box #f))
      (add-system-msg! "hello" state-box lock (lambda () (set-box! notify-called? #t)))
      (check-true (unbox notify-called?))
      (check-equal? (length (gui-state-messages (unbox state-box))) 1))))

(define test-make-slash-command-handler
  (test-suite "make-slash-command-handler"
    (test-case "returns a procedure"
      (define handler (make-slash-command-handler #f (box (make-gui-state)) (make-semaphore 1)))
      (check-true (procedure? handler)))

    (test-case "returns #f for empty input"
      (define handler (make-slash-command-handler #f (box (make-gui-state)) (make-semaphore 1)))
      (check-equal? (handler "") #f))

    (test-case "L-05: handler returns #f for non-slash plain text"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-false (handler "hello world"))
      (check-equal? (length (gui-state-messages (unbox state-box))) 0))

    (test-case "handler /help calls notify"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define notify-called? (box #f))
      (define handler
        (make-slash-command-handler #f state-box lock (lambda () (set-box! notify-called? #t))))
      (handler "/help")
      (check-true (unbox notify-called?))
      (check-equal? (length (gui-state-messages (unbox state-box))) 1))))

(define test-known-commands
  (test-suite "known commands"
    (test-case "/clear clears messages"
      (define state-box (box (make-gui-state #:messages (list (make-gui-message "user" "x")))))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/clear"))
      (check-equal? (gui-state-messages (unbox state-box)) '()))

    (test-case "/help adds help system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/help"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "system")
      (check-true (string-contains? (gui-message-text (car msgs)) "Available commands")))

    (test-case "/toggle-detail expands the latest canonical reasoning artifact"
      (define artifact
        (make-conversation-artifact #:id "gui-artifact"
                                    #:session-id "gui-session"
                                    #:turn-id "gui-turn"
                                    #:kind 'thinking
                                    #:body "reason"
                                    #:lifecycle 'completed))
      (define state-box (box (gui-state-upsert-artifact (make-gui-state) artifact)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/toggle-detail"))
      (check-true (disclosure-expanded? (gui-state-disclosure (unbox state-box)) "gui-artifact")))

    (test-case "/compact adds system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/compact"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-true (string-contains? (gui-message-text (car msgs)) "compaction")))))

(define test-extension-dispatch
  (test-suite "extension dispatch"
    (test-case "try-extension-dispatch with #f session returns #f"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-false (try-extension-dispatch #f state-box lock "/foo")))

    (test-case "unknown command adds 'Unknown command' system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/unknowncommand"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "system")
      (check-true (string-contains? (gui-message-text (car msgs)) "Unknown command")))

    (test-case "handler returns #t even for unknown commands"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/nope")))))

(define test-new-session-dispatch
  (test-suite "new-session dispatch"
    (test-case "try-extension-dispatch handles new-session hook result"
      (define ext-reg (make-extension-registry))
      (define test-ext
        (extension "test-new-session"
                   "1.0"
                   "1"
                   (hasheq 'execute-command
                           (lambda (payload) (hook-amend (hasheq 'new-session "test prompt"))))))
      (register-extension! ext-reg test-ext)
      (define mock-sess
        (agent-session "test-session"
                       (string->path "/tmp/test")
                       #f
                       #f
                       #f
                       ext-reg
                       "test-model"
                       '()
                       #f
                       (make-queue)
                       (hash)
                       #t
                       0
                       #f
                       #f
                       (make-lifecycle-state)
                       #f))
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-true (try-extension-dispatch mock-sess state-box lock "/test-new-session"))
      ;; L-06: yield to let background thread finish; incomplete session produces captured error message
      (sync (system-idle-evt))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "system")
      (check-true (string-contains? (gui-message-text (car msgs)) "[ERROR] /go failed:")))

    (test-case "try-extension-dispatch handles submit hook result"
      (define ext-reg (make-extension-registry))
      (define test-ext
        (extension "test-submit"
                   "1.0"
                   "1"
                   (hasheq 'execute-command
                           (lambda (payload) (hook-amend (hasheq 'submit "test submit text"))))))
      (register-extension! ext-reg test-ext)
      (define mock-sess
        (agent-session "test-session"
                       (string->path "/tmp/test")
                       #f
                       #f
                       #f
                       ext-reg
                       "test-model"
                       '()
                       #f
                       (make-queue)
                       (hash)
                       #t
                       0
                       #f
                       #f
                       (make-lifecycle-state)
                       #f))
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-true (try-extension-dispatch mock-sess state-box lock "/test-submit"))
      ;; L-06: yield to let background thread finish before assertions
      (sync (system-idle-evt)))))

;; ═══════════════════════════════════════════════════════════
;; MF-08 (v0.98.9 W1): Contract-rejection tests
;; ═══════════════════════════════════════════════════════════
(define test-contract-rejection
  (test-suite "MF-08: contract rejection"
    (test-case "add-system-msg! rejects non-string first arg"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-exn exn:fail:contract? (lambda () (add-system-msg! 42 state-box lock))))

    (test-case "add-system-msg! rejects non-box state"
      (check-exn exn:fail:contract?
                 (lambda () (add-system-msg! "hello" "not-a-box" (make-semaphore 1)))))

    (test-case "make-slash-command-handler rejects non-semaphore"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (make-slash-command-handler #f (box (make-gui-state)) "not-a-semaphore"))))

    ;; M-01 (v0.98.10 W0): Tightened sess from any/c to (or/c agent-session? #f)
    (test-case "make-slash-command-handler rejects non-session, non-#f sess"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (make-slash-command-handler 42 (box (make-gui-state)) (make-semaphore 1)))))

    (test-case "try-extension-dispatch rejects non-session, non-#f sess"
      (check-exn
       exn:fail:contract?
       (lambda ()
         (try-extension-dispatch "not-a-session" (box (make-gui-state)) (make-semaphore 1) "/foo"))))

    (test-case "try-extension-dispatch rejects non-string input"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-exn exn:fail:contract? (lambda () (try-extension-dispatch #f state-box lock 12345))))

    (test-case "try-extension-dispatch rejects non-box state"
      (check-exn exn:fail:contract?
                 (lambda () (try-extension-dispatch #f "not-a-box" (make-semaphore 1) "/foo"))))))

;; ═══════════════════════════════════════════════════════════
;; L-07 (v0.98.10 W2): Document session-less command paths
;; ═══════════════════════════════════════════════════════════
(define test-session-less-commands
  (test-suite "session-less command paths"
    (test-case "/status with sess=#f raises contract error"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-exn exn:fail:contract? (lambda () (handler "/status"))))

    (test-case "/model with sess=#f raises contract error"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-exn exn:fail:contract? (lambda () (handler "/model"))))

    (test-case "/goal status with sess=#f returns no-active-goal message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/goal status"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-true (string-contains? (gui-message-text (car msgs)) "No active goal")))

    (test-case "/goal clear with sess=#f works without session access"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/goal clear"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-true (string-contains? (gui-message-text (car msgs)) "cancelled")))))

(define test-campaign-session
  (test-suite "dedicated GUI campaign session"
    (test-case "campaign runner factory creates a fresh session for each wave"
      (define dir (make-temporary-file "gui-campaign-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define initiating
           (make-agent-session (hasheq 'provider
                                       (make-simple-mock-provider "first" "second")
                                       'tool-registry
                                       (make-tool-registry)
                                       'event-bus
                                       (make-event-bus)
                                       'session-dir
                                       (path->string dir)
                                       'model-name
                                       "test")))
         (define-values (first-session first-runner) (make-gui-campaign-runner initiating))
         (define-values (second-session second-runner) (make-gui-campaign-runner initiating))
         (check-not-equal? (session-id first-session) (session-id initiating))
         (check-not-equal? (session-id second-session) (session-id initiating))
         (check-not-equal? (session-id first-session) (session-id second-session))
         (check-equal? (length (call-with-values (lambda () (first-runner "W0")) list)) 2)
         (check-equal? (length (call-with-values (lambda () (second-runner "W1")) list)) 2))
       (lambda () (delete-directory/files dir #:must-exist? #f))))))

(run-tests (test-suite "gui-slash-commands"
             test-add-system-msg
             test-make-slash-command-handler
             test-known-commands
             test-extension-dispatch
             test-new-session-dispatch
             test-contract-rejection
             test-session-less-commands
             test-campaign-session))
