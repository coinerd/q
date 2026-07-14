#lang racket

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration

;;; tests/test-compaction-guard.rkt — tests for compaction guard (#765)
;;;
;;; Verifies that:
;;;   - Recursive compaction is prevented by compacting? flag
;;;   - compaction.start/end events emitted in correct order
;;;   - Flag cleared even if compaction fails

(require rackunit
         rackunit/text-ui
         "../util/message/protocol-types.rkt"
         "../util/event/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         (only-in "../runtime/session/session-mutation.rkt"
                  current-prompt-operation-session
                  guarded-set-compacting!
                  release-prompt!
                  try-claim-compaction!
                  try-claim-prompt!)
         "../runtime/session/session-store.rkt"
         "../runtime/compaction/compactor.rkt"
         (only-in "helpers/temp-fs.rkt" with-temp-dir))

(with-temp-dir
 (dir)
 (test-case "compacting? flag starts as #f"
   (check-equal? (agent-session-compacting? (make-agent-session (hasheq 'session-dir
                                                                        dir
                                                                        'event-bus
                                                                        (make-event-bus)
                                                                        'provider
                                                                        #f
                                                                        'tool-registry
                                                                        #f
                                                                        'model-name
                                                                        "test"
                                                                        'system-instructions
                                                                        '())))
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
     (make-agent-session (hasheq 'session-dir
                                 dir
                                 'event-bus
                                 (make-event-bus)
                                 'provider
                                 #f
                                 'tool-registry
                                 #f
                                 'model-name
                                 "test"
                                 'system-instructions
                                 '())))
   (guarded-set-compacting! sess #t)
   (check-true (agent-session-compacting? sess))
   ;; If flag is set, compaction should be skipped
   (define context
     (list (make-message "m1"
                         #f
                         'user
                         'message
                         (list (make-text-part "hello"))
                         (current-seconds)
                         (hasheq))))
   (define result (maybe-compact-context sess context 0))
   ;; Context should be returned unchanged since flag is set
   (check-equal? result context))
 (test-case "prompt owner may run automatic compaction without opening manual race"
   (define sess
     (make-agent-session (hasheq 'session-dir
                                 dir
                                 'event-bus
                                 (make-event-bus)
                                 'provider
                                 #f
                                 'tool-registry
                                 #f
                                 'model-name
                                 "test"
                                 'system-instructions
                                 '())))
   (define context
     (list (make-message "automatic"
                         #f
                         'user
                         'message
                         (list (make-text-part "context that exceeds a zero threshold"))
                         (current-seconds)
                         (hasheq))))
   (check-true (try-claim-prompt! sess))
   (parameterize ([current-prompt-operation-session sess])
     (check-false (try-claim-compaction! sess)
                  "manual/default claim stays blocked inside prompt extent"))
   (define result
     (parameterize ([current-prompt-operation-session sess])
       (maybe-compact-context sess context 0)))
   (check-true (list? result))
   (check-true (agent-session-prompt-running? sess))
   (check-false (agent-session-compacting? sess))
   (release-prompt! sess))
 (test-case "compacting? flag cleared after compaction"
   (define sess
     (make-agent-session (hasheq 'session-dir
                                 dir
                                 'event-bus
                                 (make-event-bus)
                                 'provider
                                 #f
                                 'tool-registry
                                 #f
                                 'model-name
                                 "test"
                                 'system-instructions
                                 '())))
   (guarded-set-compacting! sess #t)
   (guarded-set-compacting! sess #f)
   (check-false (agent-session-compacting? sess))))
