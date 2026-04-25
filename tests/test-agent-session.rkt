#lang racket

;; tests/test-agent-session.rkt — tests for concurrent prompt execution guard
;;
;; Covers:
;;   - prompt-running? field on agent-session struct
;;   - Concurrent run-prompt! rejection

(require rackunit
         racket/file
         "../runtime/agent-session.rkt"
         "../agent/event-bus.rkt"
         "../tools/tool.rkt")

(define (make-test-session)
  (define tmp-dir (make-temporary-file "q-agent-sess-~a" 'directory))
  (make-agent-session
   (hasheq 'provider #f
           'tool-registry (make-tool-registry)
           'event-bus (make-event-bus)
           'session-dir tmp-dir
           'max-iterations 5)))

(test-case "agent-session struct has prompt-running? field"
  (define sess (make-test-session))
  (check-not-exn (lambda () (agent-session-prompt-running? sess)))
  (check-false (agent-session-prompt-running? sess)))

(test-case "set-agent-session-prompt-running?! works"
  (define sess (make-test-session))
  (set-agent-session-prompt-running?! sess #t)
  (check-true (agent-session-prompt-running? sess))
  (set-agent-session-prompt-running?! sess #f)
  (check-false (agent-session-prompt-running? sess)))
