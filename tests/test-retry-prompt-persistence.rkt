#lang racket

;; @speed fast
;; @suite provider

;; BOUNDARY: integration

;;; tests/test-retry-prompt-persistence.rkt — NR-2: /retry prompt persistence
;;;
;;; Tests that run-prompt! persists the last user prompt to session config
;;; so /retry can recover it when the TUI-local last-prompt-box is empty
;;; (e.g., after session resume or goal-runner-driven prompts).

(require rackunit
         rackunit/text-ui
         racket/file
         "../util/event/event-bus.rkt"
         "../tools/tool.rkt"
         "../runtime/agent-session.rkt"
         (only-in "../runtime/session/session-types.rkt" agent-session-config)
         (only-in "helpers/mock-provider.rkt" make-simple-mock-provider))

(define retry-prompt-persistence-tests
  (test-suite "NR-2: /retry prompt persistence"

    (test-case "run-prompt! stores last-user-prompt in session config"
      (define tmpdir (make-temporary-file "q-retry-~a" 'directory))
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define prov (make-simple-mock-provider "response"))
      (dynamic-wind void
                    (lambda ()
                      (define sess
                        (make-agent-session (hasheq 'provider
                                                    prov
                                                    'tool-registry
                                                    reg
                                                    'event-bus
                                                    bus
                                                    'session-dir
                                                    (path->string tmpdir)
                                                    'model-name
                                                    "test")))
                      (run-prompt! sess "list files in current directory")
                      (define cfg (agent-session-config sess))
                      (check-equal? (dict-ref cfg 'last-user-prompt #f)
                                    "list files in current directory"
                                    "session config should have last-user-prompt after run-prompt!"))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    (test-case "run-prompt! updates last-user-prompt on each call"
      (define tmpdir (make-temporary-file "q-retry-~a" 'directory))
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define prov (make-simple-mock-provider "response"))
      (dynamic-wind void
                    (lambda ()
                      (define sess
                        (make-agent-session (hasheq 'provider
                                                    prov
                                                    'tool-registry
                                                    reg
                                                    'event-bus
                                                    bus
                                                    'session-dir
                                                    (path->string tmpdir)
                                                    'model-name
                                                    "test")))
                      (run-prompt! sess "first prompt")
                      (run-prompt! sess "second prompt")
                      (define cfg (agent-session-config sess))
                      (check-equal? (dict-ref cfg 'last-user-prompt #f)
                                    "second prompt"
                                    "session config should reflect the most recent prompt"))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))))

(run-tests retry-prompt-persistence-tests)
