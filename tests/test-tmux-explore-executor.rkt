#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         rackunit/text-ui
         racket/file
         "../scripts/tmux-explore/executor.rkt")

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
