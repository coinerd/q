#lang racket

;; tests/test-session-lifecycle-smoke.rkt — Session lifecycle smoke test
;;
;; Verifies the basic session lifecycle: create -> close -> resume.
;; Catches regressions in session directory creation, persistence,
;; and resume path.

(require rackunit
         rackunit/text-ui
         racket/file
         "../agent/event-bus.rkt"
         "../tools/tool.rkt"
         "../runtime/agent-session.rkt"
         (only-in "helpers/mock-provider.rkt" make-simple-mock-provider))

(define lifecycle-tests
  (test-suite
   "Session Lifecycle Smoke Tests"

   (test-case "create -> close -> resume round-trip"
     (define tmpdir (make-temporary-file "q-lifecycle-~a" 'directory))
     (define bus (make-event-bus))
     (define reg (make-tool-registry))
     (define prov (make-simple-mock-provider "hello"))
     (dynamic-wind
       void
       (lambda ()
         (define sess
           (make-agent-session (hasheq 'provider prov
                                        'tool-registry reg
                                        'event-bus bus
                                        'session-dir (path->string tmpdir)
                                        'model-name "test")))
         (define sid (session-id sess))
         (check-true (agent-session-persisted? sess))
         (check-true (directory-exists? (build-path tmpdir sid)))
         (close-session! sess)
         (check-false (session-active? sess))
         (define resumed
           (resume-agent-session sid
                                 (hasheq 'provider prov
                                         'tool-registry reg
                                         'event-bus bus
                                         'session-dir (path->string tmpdir)
                                         'model-name "test")))
         (check-true (session-active? resumed))
         (check-equal? (session-id resumed) sid))
       (lambda ()
         (delete-directory/files tmpdir #:must-exist? #f))))

   (test-case "create -> prompt -> close -> resume preserves messages"
     (define tmpdir (make-temporary-file "q-lifecycle-~a" 'directory))
     (define bus (make-event-bus))
     (define reg (make-tool-registry))
     (define prov (make-simple-mock-provider "response"))
     (dynamic-wind
       void
       (lambda ()
         (define sess
           (make-agent-session (hasheq 'provider prov
                                        'tool-registry reg
                                        'event-bus bus
                                        'session-dir (path->string tmpdir)
                                        'model-name "test")))
         (define sid (session-id sess))
         (run-prompt! sess "hello")
         (close-session! sess)
         (define resumed
           (resume-agent-session sid
                                 (hasheq 'provider prov
                                         'tool-registry reg
                                         'event-bus bus
                                         'session-dir (path->string tmpdir)
                                         'model-name "test")))
         (check-true (session-active? resumed))
         (define log-path (build-path tmpdir sid "session.jsonl"))
         (check-true (file-exists? log-path))
         (check-true (> (file-size log-path) 0)))
       (lambda ()
         (delete-directory/files tmpdir #:must-exist? #f))))

   (test-case "create without prompt still has directory for resume"
     (define tmpdir (make-temporary-file "q-lifecycle-~a" 'directory))
     (define bus (make-event-bus))
     (define reg (make-tool-registry))
     (define prov (make-simple-mock-provider "hi"))
     (dynamic-wind
       void
       (lambda ()
         (define sess
           (make-agent-session (hasheq 'provider prov
                                        'tool-registry reg
                                        'event-bus bus
                                        'session-dir (path->string tmpdir)
                                        'model-name "test")))
         (define sid (session-id sess))
         (close-session! sess)
         (define resumed
           (resume-agent-session sid
                                 (hasheq 'provider prov
                                         'tool-registry reg
                                         'event-bus bus
                                         'session-dir (path->string tmpdir)
                                         'model-name "test")))
         (check-true (session-active? resumed)))
       (lambda ()
         (delete-directory/files tmpdir #:must-exist? #f))))))

(run-tests lifecycle-tests)
