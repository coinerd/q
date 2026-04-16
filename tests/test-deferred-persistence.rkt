#lang racket

;;; tests/test-deferred-persistence.rkt — tests for deferred session persistence (#771)
;;;
;;; Verifies that session directory is not created until first assistant response
;;; and that buffered messages are correctly persisted.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt")

(define (make-temp-dir)
  (make-temporary-file "q-defer-test-~a" 'directory))

(test-case "session directory IS created at construction (eager persist)"
  (define tmpdir (make-temp-dir))
  (define sess
    (make-agent-session (hasheq 'session-dir
                                (path->string tmpdir)
                                'event-bus
                                (make-event-bus)
                                'provider
                                #f
                                'tool-registry
                                #f
                                'model-name
                                "test")))
  (define sid (session-id sess))
  (define session-dir (build-path tmpdir sid))
  ;; BUG-40 fix: ensure-persisted! is called eagerly so resume works immediately
  (check-true (agent-session-persisted? sess))
  (check-true (directory-exists? session-dir)))

(test-case "entries are written immediately after construction (eager persist)"
  (define tmpdir (make-temp-dir))
  (define sess
    (make-agent-session (hasheq 'session-dir
                                (path->string tmpdir)
                                'event-bus
                                (make-event-bus)
                                'provider
                                #f
                                'tool-registry
                                #f
                                'model-name
                                "test")))
  ;; Eager persist means persisted? is true immediately
  (check-true (agent-session-persisted? sess))
  (check-equal? (agent-session-pending-entries sess) '()))

(test-case "ensure-persisted! creates directory and sets flag"
  (define tmpdir (make-temp-dir))
  (define sess
    (make-agent-session (hasheq 'session-dir
                                (path->string tmpdir)
                                'event-bus
                                (make-event-bus)
                                'provider
                                #f
                                'tool-registry
                                #f
                                'model-name
                                "test")))
  (define sid (session-id sess))
  (ensure-persisted! sess)
  (check-true (agent-session-persisted? sess))
  (check-true (directory-exists? (build-path tmpdir sid))))

(test-case "buffer-or-append! writes immediately when eagerly persisted"
  (define tmpdir (make-temp-dir))
  (define sess
    (make-agent-session (hasheq 'session-dir
                                (path->string tmpdir)
                                'event-bus
                                (make-event-bus)
                                'provider
                                #f
                                'tool-registry
                                #f
                                'model-name
                                "test")))
  ;; Buffer a message — goes directly to log since session is already persisted
  (define msg
    (make-message "test-1"
                  #f
                  'user
                  'message
                  (list (make-text-part "hello"))
                  (current-seconds)
                  (hasheq)))
  (buffer-or-append! sess msg)
  ;; No pending entries since already persisted — written directly
  (check-equal? (agent-session-pending-entries sess) '())
  ;; File should contain the entry (plus version header)
  (define log-path (build-path tmpdir (session-id sess) "session.jsonl"))
  (check-true (file-exists? log-path)))

(test-case "ensure-persisted! is idempotent"
  (define tmpdir (make-temp-dir))
  (define sess
    (make-agent-session (hasheq 'session-dir
                                (path->string tmpdir)
                                'event-bus
                                (make-event-bus)
                                'provider
                                #f
                                'tool-registry
                                #f
                                'model-name
                                "test")))
  (ensure-persisted! sess)
  (ensure-persisted! sess)
  (check-true (agent-session-persisted? sess)))

(test-case "buffer-or-append! writes immediately when persisted"
  (define tmpdir (make-temp-dir))
  (define sess
    (make-agent-session (hasheq 'session-dir
                                (path->string tmpdir)
                                'event-bus
                                (make-event-bus)
                                'provider
                                #f
                                'tool-registry
                                #f
                                'model-name
                                "test")))
  (ensure-persisted! sess)
  (define msg
    (make-message "test-2"
                  #f
                  'user
                  'message
                  (list (make-text-part "immediate"))
                  (current-seconds)
                  (hasheq)))
  (buffer-or-append! sess msg)
  ;; Should be written directly, not buffered
  (check-equal? (agent-session-pending-entries sess) '()))
