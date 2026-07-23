#lang racket

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration

;; tests/test-agent-session.rkt — tests for concurrent prompt execution guard
;;
;; Covers:
;;   - prompt-running? field on agent-session struct
;;   - Concurrent run-prompt! rejection

(require rackunit
         racket/format
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         (only-in "../runtime/session/session-mutation.rkt"
                  guarded-set-prompt-running!
                  try-claim-prompt!
                  release-prompt!)
         "../util/event/event-bus.rkt"
         "../tools/tool.rkt"
         (only-in "helpers/temp-fs.rkt" with-temp-dir))

(define (make-test-session dir)
  (make-agent-session (hasheq 'provider
                              #f
                              'tool-registry
                              (make-tool-registry)
                              'event-bus
                              (make-event-bus)
                              'session-dir
                              dir
                              'max-iterations
                              5)))

(with-temp-dir (dir)
               (test-case "agent-session struct has prompt-running? field"
                 (define sess (make-test-session dir))
                 (check-not-exn (lambda () (agent-session-prompt-running? sess)))
                 (check-false (agent-session-prompt-running? sess)))
               (test-case "guarded-set-prompt-running! works"
                 (define sess (make-test-session dir))
                 (guarded-set-prompt-running! sess #t)
                 (check-true (agent-session-prompt-running? sess))
                 (guarded-set-prompt-running! sess #f)
                 (check-false (agent-session-prompt-running? sess))))

(with-temp-dir
 (dir)
 (test-case "W1: concurrent prompt claim fails"
   (define sess (make-test-session dir))
   (check-true (try-claim-prompt! sess) "first claim should succeed")
   (check-false (try-claim-prompt! sess) "second claim should fail")
   (release-prompt! sess))
 (test-case "W1: error message pattern includes session ID and /interrupt"
   (define sid "TEST-SESSION-ID")
   (define reason
     (format "Prompt already running — session ~a is processing. Use /interrupt to cancel." sid))
   (check-true (string-contains? reason sid))
   (check-true (string-contains? reason "/interrupt"))
   (check-false (string-contains? reason "ignoring"))))
