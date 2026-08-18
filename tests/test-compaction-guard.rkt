#lang racket

;; @speed fast  ;; @suite runtime
;; @boundary unit

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
         (only-in "helpers/temp-fs.rkt" with-temp-dir)
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         (only-in "../util/event/event.rkt" event-payload)
         (only-in "../runtime/session/session-mutation.rkt" release-compaction!))

(define (make-automatic-session dir bus [ext-reg #f])
  (make-agent-session (hasheq 'session-dir
                              dir
                              'event-bus
                              bus
                              'provider
                              #f
                              'tool-registry
                              #f
                              'model-name
                              "test"
                              'system-instructions
                              '()
                              'extension-registry
                              ext-reg)))

(define (automatic-context [text "automatic compaction context"])
  (list (make-message "automatic"
                      #f
                      'user
                      'message
                      (list (make-text-part text))
                      (current-seconds)
                      (hasheq))))

(define (record-events bus)
  (define events (box '()))
  (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list evt)))))
  events)

(define (typed-compaction-reasons events)
  (for/list ([evt (in-list (unbox events))]
             #:when (string=? (event-ev evt) "compaction"))
    (hash-ref (event-payload evt) 'reason #f)))

(define (blocking-compaction-registry)
  (define reg (make-extension-registry))
  (register-extension! reg
                       (extension "block-compaction"
                                  "0.1.0"
                                  "1.0"
                                  (hasheq 'session-before-compact
                                          (lambda (_payload) (hook-block "blocked")))))
  reg)

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
 (test-case "automatic start publication failure releases ownership without success effects"
   (define bus (make-event-bus))
   (define events (record-events bus))
   (define sess (make-automatic-session dir bus))
   (define sentinel (exn:fail "automatic start failed" (current-continuation-marks)))
   (subscribe! bus
               (lambda (evt)
                 (when (and (string=? (event-ev evt) "compaction")
                            (equal? (hash-ref (event-payload evt) 'reason #f) "budget-exceeded"))
                   (raise sentinel))))
   (define raised (box #f))
   (parameterize ([current-event-bus-error-handler (lambda (_evt _handler exn) (raise exn))])
     (with-handlers ([exn:fail? (lambda (exn) (set-box! raised exn))])
       (maybe-compact-context sess (automatic-context) 0)))
   (check-eq? (unbox raised) sentinel)
   (check-false (agent-session-compacting? sess))
   (check-false (agent-session-last-compaction-time sess))
   (check-false (member "compaction-complete" (typed-compaction-reasons events)))
   (check-true (try-claim-compaction! sess))
   (release-compaction! sess))
 (test-case "automatic hook block emits no lifecycle and sets no cooldown"
   (define bus (make-event-bus))
   (define events (record-events bus))
   (define sess (make-automatic-session dir bus (blocking-compaction-registry)))
   (define context (automatic-context))
   (check-equal? (maybe-compact-context sess context 0) context)
   (check-false (agent-session-compacting? sess))
   (check-false (agent-session-last-compaction-time sess))
   (check-equal? (typed-compaction-reasons events) '()))
 (test-case "automatic body failure releases ownership and emits failure not success"
   (define bus (make-event-bus))
   (define events (record-events bus))
   (define sess (make-automatic-session dir bus))
   (define sentinel (exn:fail "automatic body failed" (current-continuation-marks)))
   (subscribe! bus
               (lambda (evt)
                 (when (string=? (event-ev evt) "compaction.warning")
                   (raise sentinel))))
   (define raised (box #f))
   (parameterize ([current-event-bus-error-handler (lambda (_evt _handler exn) (raise exn))])
     (with-handlers ([exn:fail? (lambda (exn) (set-box! raised exn))])
       (maybe-compact-context sess (automatic-context) 0)))
   (check-eq? (unbox raised) sentinel)
   (check-false (agent-session-compacting? sess))
   (check-false (agent-session-last-compaction-time sess))
   (check-equal? (typed-compaction-reasons events) '("budget-exceeded" "compaction-failed")))
 (test-case "automatic success sets cooldown and exactly one success completion"
   (define bus (make-event-bus))
   (define events (record-events bus))
   (define sess (make-automatic-session dir bus))
   (check-true (list? (maybe-compact-context sess (automatic-context) 0)))
   (check-false (agent-session-compacting? sess))
   (check-not-false (agent-session-last-compaction-time sess))
   (check-equal? (typed-compaction-reasons events) '("budget-exceeded" "compaction-complete")))
 (test-case "automatic success terminal publishes while ownership remains held"
   (define bus (make-event-bus))
   (define sess (make-automatic-session dir bus))
   (define held-during-terminal? (box #f))
   (subscribe! bus
               (lambda (evt)
                 (when (and (string=? (event-ev evt) "compaction")
                            (equal? (hash-ref (event-payload evt) 'reason #f) "compaction-complete"))
                   (set-box! held-during-terminal? (agent-session-compacting? sess)))))
   (maybe-compact-context sess (automatic-context) 0)
   (check-true (unbox held-during-terminal?))
   (check-false (agent-session-compacting? sess)))
 (test-case "success terminal publication failure still releases ownership last"
   (define bus (make-event-bus))
   (define sess (make-automatic-session dir bus))
   (define sentinel (exn:fail "success terminal failed" (current-continuation-marks)))
   (subscribe! bus
               (lambda (evt)
                 (when (and (string=? (event-ev evt) "compaction")
                            (equal? (hash-ref (event-payload evt) 'reason #f) "compaction-complete"))
                   (check-true (agent-session-compacting? sess)
                               "ownership must remain held during terminal publication")
                   (raise sentinel))))
   (define raised (box #f))
   (parameterize ([current-event-bus-error-handler (lambda (_evt _handler exn) (raise exn))])
     (with-handlers ([exn:fail? (lambda (exn) (set-box! raised exn))])
       (maybe-compact-context sess (automatic-context) 0)))
   (check-eq? (unbox raised) sentinel)
   (check-false (agent-session-compacting? sess))
   (check-true (try-claim-compaction! sess))
   (release-compaction! sess))
 (test-case "failure terminal publication cannot mask the original body failure"
   (define bus (make-event-bus))
   (define sess (make-automatic-session dir bus))
   (define body-sentinel (exn:fail "body failed first" (current-continuation-marks)))
   (define terminal-sentinel (exn:fail "failure terminal also failed" (current-continuation-marks)))
   (define failure-terminal-attempted? (box #f))
   (subscribe! bus
               (lambda (evt)
                 (cond
                   [(string=? (event-ev evt) "compaction.warning") (raise body-sentinel)]
                   [(and (string=? (event-ev evt) "compaction")
                         (equal? (hash-ref (event-payload evt) 'reason #f) "compaction-failed"))
                    (set-box! failure-terminal-attempted? #t)
                    (raise terminal-sentinel)])))
   (define raised (box #f))
   (parameterize ([current-event-bus-error-handler (lambda (_evt _handler exn) (raise exn))])
     (with-handlers ([exn:fail? (lambda (exn) (set-box! raised exn))])
       (maybe-compact-context sess (automatic-context) 0)))
   (check-true (unbox failure-terminal-attempted?))
   (check-eq? (unbox raised) body-sentinel)
   (check-false (agent-session-compacting? sess)))
 (test-case "denied automatic contender preserves active compaction owner"
   (define sess (make-automatic-session dir (make-event-bus)))
   (define context (automatic-context))
   (check-true (try-claim-compaction! sess))
   (check-equal? (maybe-compact-context sess context 0) context)
   (check-true (agent-session-compacting? sess))
   (release-compaction! sess))
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
