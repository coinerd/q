#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         rackunit/text-ui
         racket/file
         "../scripts/tmux-explore/executor.rkt"
         (only-in "../runtime/session/session-store.rkt" load-session-log)
         (only-in "../util/message/message.rkt" message-id)
         (only-in "../runtime/compaction/compactor.rkt"
                  compact-history
                  compaction-result-removed-count))

(define (make-workspace)
  (define dir (make-temporary-file "q-executor-cleanup-~a" 'directory))
  (call-with-output-file (build-path dir "copied-credentials.json")
                         (lambda (out) (display "secret fixture" out)))
  dir)

(define suite
  (test-suite "real explorer executor cleanup"

    (test-case "success deletes credential workspace and stops only named session"
      (define dir (make-workspace))
      (define stopped '())
      (define result
        (call-with-executor-cleanup dir
                                    "q-explore-owned"
                                    (lambda () 'completed)
                                    #:stop-session
                                    (lambda (name) (set! stopped (cons name stopped)))))
      (check-equal? result 'completed)
      (check-false (directory-exists? dir))
      (check-equal? stopped '("q-explore-owned")))

    (test-case "timeout result still deletes workspace and stops named session"
      (define dir (make-workspace))
      (define stopped '())
      (define result
        (call-with-executor-cleanup dir
                                    "q-explore-timeout"
                                    (lambda () (hash 'status 'timed-out))
                                    #:stop-session
                                    (lambda (name) (set! stopped (cons name stopped)))))
      (check-equal? (hash-ref result 'status) 'timed-out)
      (check-false (directory-exists? dir))
      (check-equal? stopped '("q-explore-timeout")))

    (test-case "exception deletes workspace and never targets unrelated sessions"
      (define dir (make-workspace))
      (define stopped '())
      (check-exn #rx"executor exploded"
                 (lambda ()
                   (call-with-executor-cleanup dir
                                               "q-explore-failed"
                                               (lambda () (error 'fixture "executor exploded"))
                                               #:stop-session
                                               (lambda (name) (set! stopped (cons name stopped))))))
      (check-false (directory-exists? dir))
      (check-equal? stopped '("q-explore-failed")))

    (test-case "stop failure is visible after credential workspace deletion"
      (define dir (make-workspace))
      (check-exn #rx"stop failed"
                 (lambda ()
                   (call-with-executor-cleanup dir
                                               "q-explore-stop-failure"
                                               void
                                               #:stop-session
                                               (lambda (_name) (error 'fixture "stop failed")))))
      (check-false (directory-exists? dir)))

    (test-case "compact fixture seeds a deterministic durable history without provider turns"
      (define dir (make-temporary-file "q-compact-seed-~a" 'directory))
      (define path (build-path dir "session.jsonl"))
      (dynamic-wind void
                    (lambda ()
                      (check-equal? (seed-compact-history! path 4) path)
                      (define messages (load-session-log path))
                      (check-equal? (length messages) 4)
                      (check-equal? (map message-id messages)
                                    '("explorer-compact-seed-0" "explorer-compact-seed-1"
                                                                "explorer-compact-seed-2"
                                                                "explorer-compact-seed-3")))
                    (lambda () (delete-directory/files dir))))

    (test-case "default compact fixture exceeds the production compaction threshold"
      (define dir (make-temporary-file "q-compact-threshold-~a" 'directory))
      (define path (build-path dir "session.jsonl"))
      (dynamic-wind void
                    (lambda ()
                      (seed-compact-history! path)
                      (check-true (positive? (compaction-result-removed-count
                                              (compact-history (load-session-log path))))))
                    (lambda () (delete-directory/files dir))))

    (test-case "compact scenario terminates on its lifecycle outcome without a model turn"
      (check-true (scenario-terminal-event? "compact" (hash 'phase "session.compact.completed")))
      (check-true (scenario-terminal-event? "compact" (hash 'phase "session.compact.failed")))
      (check-false (scenario-terminal-event? "compact" (hash 'phase "session.compact.started"))))

    (test-case "ordinary scenarios still require a completed model turn"
      (check-true (scenario-terminal-event? "memory" (hash 'phase "turn.completed")))
      (check-false (scenario-terminal-event? "memory" (hash 'phase "session.compact.completed"))))

    (test-case "interrupt cancellation is intermediate and recovery completion is terminal"
      (check-false (scenario-terminal-event? "interrupt" (hash 'phase "turn.cancelled")))
      (check-true (scenario-terminal-event? "interrupt" (hash 'phase "turn.completed"))))

    (test-case "no session name means no tmux cleanup call"
      (define dir (make-workspace))
      (define stopped '())
      (call-with-executor-cleanup dir
                                  #f
                                  void
                                  #:stop-session (lambda (name) (set! stopped (cons name stopped))))
      (check-false (directory-exists? dir))
      (check-equal? stopped '()))))

(run-tests suite)
