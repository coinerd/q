#lang racket

;; @speed fast  ;; @suite runtime
;; @boundary integration

(require rackunit
         racket/file
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         "../runtime/session/session-mutation.rkt"
         "../runtime/session/session-prompt-scope.rkt"
         "../runtime/context-assembly/rollback-actions.rkt"
         "../util/event/event-bus.rkt")

(define (make-test-session dir)
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

(define (with-test-session proc)
  (define dir (make-temporary-file "q-prompt-scope-~a" 'directory))
  (dynamic-wind void
                (lambda () (proc (make-test-session dir)))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "prompt scope preserves zero one two and four return values"
  (with-test-session (lambda (sess)
                       (for ([payload (in-list '(() (one) (one two) (one two three four)))])
                         (check-equal? (call-with-values (lambda ()
                                                           (call-with-session-prompt-scope
                                                            sess
                                                            (lambda () (apply values payload))))
                                                         list)
                                       payload)))))

(test-case "normal prompt scope saves inner rollback state before parameters unwind"
  (with-test-session (lambda (sess)
                       (define lifecycle (agent-session-lifecycle sess))
                       (define initial (rollback-state 1 #f 0 '()))
                       (define inner (rollback-state 2 #t 1 '(inner)))
                       (define outer (rollback-state 99 #f 7 '(outer)))
                       (set-lifecycle-state-rollback-st! lifecycle initial)
                       (parameterize ([current-prompt-operation-session #f]
                                      [current-rollback-state outer])
                         (check-equal? (call-with-session-prompt-scope
                                        sess
                                        (lambda ()
                                          (check-eq? (current-prompt-operation-session) sess)
                                          (check-eq? (current-rollback-state) initial)
                                          (current-rollback-state inner)
                                          'result))
                                       'result)
                         (check-eq? (lifecycle-state-rollback-st lifecycle) inner)
                         (check-false (current-prompt-operation-session))
                         (check-eq? (current-rollback-state) outer)))))

(test-case "exceptional prompt scope saves state and propagates exact exception after unwind"
  (with-test-session (lambda (sess)
                       (define lifecycle (agent-session-lifecycle sess))
                       (define initial (rollback-state 3 #f 0 '()))
                       (define inner (rollback-state 4 #t 2 '(inner-exception)))
                       (define outer (rollback-state 98 #f 6 '(outer-exception)))
                       (define sentinel
                         (exn:fail "prompt scope sentinel" (current-continuation-marks)))
                       (define caught (box #f))
                       (set-lifecycle-state-rollback-st! lifecycle initial)
                       (parameterize ([current-prompt-operation-session #f]
                                      [current-rollback-state outer])
                         (with-handlers ([exn:fail? (lambda (exn)
                                                      (check-false (current-prompt-operation-session))
                                                      (check-eq? (current-rollback-state) outer)
                                                      (set-box! caught exn))])
                           (call-with-session-prompt-scope sess
                                                           (lambda ()
                                                             (current-rollback-state inner)
                                                             (raise sentinel))))
                         (check-eq? (unbox caught) sentinel)
                         (check-eq? (lifecycle-state-rollback-st lifecycle) inner)))))

(test-case "prompt scope reuses one session state and isolates another session"
  (define dir-a (make-temporary-file "q-prompt-scope-a-~a" 'directory))
  (define dir-b (make-temporary-file "q-prompt-scope-b-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define sess-a (make-test-session dir-a))
     (define sess-b (make-test-session dir-b))
     (define state-a (rollback-state 5 #t 3 '(session-a)))
     (call-with-session-prompt-scope sess-a (lambda () (current-rollback-state state-a)))
     (call-with-session-prompt-scope sess-a (lambda () (check-eq? (current-rollback-state) state-a)))
     (call-with-session-prompt-scope
      sess-b
      (lambda ()
        (check-false (rollback-state-force-distill-active? (current-rollback-state)))
        (check-equal? (rollback-state-warning-count (current-rollback-state)) 0))))
   (lambda ()
     (delete-directory/files dir-a #:must-exist? #f)
     (delete-directory/files dir-b #:must-exist? #f))))
