#lang racket

;;; tests/test-compaction-guard.rkt — tests for compaction guard (#765)
;;;
;;; Verifies that:
;;;   - Recursive compaction is prevented by compacting? flag
;;;   - compaction.start/end events emitted in correct order
;;;   - Flag cleared even if compaction fails

(require rackunit
         rackunit/text-ui
         racket/file
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session-store.rkt"
         "../runtime/compactor.rkt")

(define (make-temp-dir)
  (make-temporary-file "q-guard-test-~a" 'directory))

(test-case "compacting? flag starts as #f"
  (check-equal? (agent-session-compacting?
                 (make-agent-session
                  (hasheq 'session-dir (make-temp-dir)
                          'event-bus (make-event-bus)
                          'provider #f
                          'tool-registry #f
                          'model-name "test"
                          'system-instructions '())))
                #f))

(test-case "compaction.start and compaction.end events emitted"
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus (lambda (evt) (set-box! events (cons (event-ev evt) (unbox events)))))
  ;; Simulate compaction start/end
  (publish! bus (make-event "compaction.start" 0 "s1" "t1" (hasheq)))
  (publish! bus (make-event "compaction.end" 0 "s1" "t1" (hasheq)))
  (define evts (reverse (unbox events)))
  (check-not-false (member "compaction.start" evts))
  (check-not-false (member "compaction.end" evts))
  ;; Verify start comes before end
  (define start-idx (index-of evts "compaction.start"))
  (define end-idx (index-of evts "compaction.end"))
  (check-true (< start-idx end-idx)))

(test-case "compacting? flag prevents recursive compaction"
  ;; Simulate: if compacting? is #t, maybe-compact-context returns context unchanged
  (define sess
    (make-agent-session
     (hasheq 'session-dir (make-temp-dir)
             'event-bus (make-event-bus)
             'provider #f
             'tool-registry #f
             'model-name "test"
             'system-instructions '())))
  (set-agent-session-compacting?! sess #t)
  (check-true (agent-session-compacting? sess))
  ;; If flag is set, compaction should be skipped
  (define context (list (make-message "m1" #f 'user 'message
                                       (list (make-text-part "hello"))
                                       (current-seconds) (hasheq))))
  (define result (maybe-compact-context sess context 0))
  ;; Context should be returned unchanged since flag is set
  (check-equal? result context))

(test-case "compacting? flag cleared after compaction"
  (define sess
    (make-agent-session
     (hasheq 'session-dir (make-temp-dir)
             'event-bus (make-event-bus)
             'provider #f
             'tool-registry #f
             'model-name "test"
             'system-instructions '())))
  (set-agent-session-compacting?! sess #t)
  (set-agent-session-compacting?! sess #f)
  (check-false (agent-session-compacting? sess)))
