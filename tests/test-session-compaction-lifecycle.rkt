#lang racket

;; @speed fast  ;; @suite runtime
;; v0.99.50 W4: durable /compact lifecycle truth.

(require rackunit
         rackunit/text-ui
         racket/file
         "../runtime/agent-session.rkt"
         "../runtime/session/session-events.rkt"
         "../runtime/session/session-types.rkt"
         (only-in "../runtime/session/session-mutation.rkt"
                  guarded-set-compacting!
                  release-prompt!
                  try-claim-prompt!)
         (only-in "../runtime/compaction/compactor.rkt"
                  compact-and-persist!
                  compaction-result
                  compaction-result-kept-messages
                  compaction-result-summary-message)
         (only-in "../runtime/compaction/token-compaction.rkt" token-compaction-config)
         (only-in "../runtime/session/session-store.rkt" append-entry! load-session-log)
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-id
                  message-kind)
         "../util/event/event-bus.rkt"
         (only-in "../util/event/event.rkt" event-ev event-payload)
         (only-in "../runtime/session-index/query.rkt" active-leaf)
         (only-in "../runtime/session/session-lifecycle.rkt" build-session-context-for-prompt)
         (only-in "../util/content/content-parts.rkt" text-part-text)
         (only-in "../util/message/message.rkt" message-content message-role)
         (only-in "../llm/provider.rkt" make-mock-provider)
         (only-in "../llm/model.rkt" make-model-response))

(define (make-test-session dir bus [provider #f])
  (make-agent-session (hasheq 'session-dir
                              dir
                              'event-bus
                              bus
                              'provider
                              provider
                              'tool-registry
                              #f
                              'model-name
                              "test"
                              'system-instructions
                              '())))

(define (record-session-compact-events bus)
  (define events (box '()))
  (subscribe! bus
              (lambda (evt) (set-box! events (append (unbox events) (list evt))))
              #:filter (lambda (evt) (string-prefix? (event-ev evt) "session.compact.")))
  events)

(define terminal-names
  '("session.compact.completed" "session.compact.nothing-to-compact"
                                "session.compact.already-running"
                                "session.compact.failed"))

(define (terminal-events events)
  (filter (lambda (evt) (member (event-ev evt) terminal-names)) events))

(define lifecycle-tests
  (test-suite "durable session compaction lifecycle"

    (test-case "success emits started then exactly one durable completed outcome with counts"
      (define dir (make-temporary-file "q-compact-life-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define bus (make-event-bus))
         (define sess (make-test-session dir bus))
         (define events (record-session-compact-events bus))
         (define seen-path (box #f))
         (define outcome
           (compact-session-durably! sess
                                     #:load-history (lambda (_) '(m1 m2 m3 m4 m5 m6 m7))
                                     #:compact-and-persist (lambda (_history path)
                                                             (set-box! seen-path path)
                                                             (compaction-result #f 5 '(m6 m7)))))
         (check-equal? outcome 'completed)
         (check-equal? (map event-ev (unbox events))
                       '("session.compact.started" "session.compact.completed"))
         (check-equal? (remove-duplicates (map (lambda (evt)
                                                 (hash-ref (event-payload evt) 'request-id))
                                               (unbox events)))
                       (list (hash-ref (event-payload (first (unbox events))) 'request-id)))
         (check-equal? (length (terminal-events (unbox events))) 1)
         (define payload (event-payload (last (unbox events))))
         (check-equal? (hash-ref payload 'removed-count) 5)
         (check-equal? (hash-ref payload 'kept-count) 2)
         (check-true (hash-ref payload 'persisted?))
         (check-equal? (unbox seen-path) (session-log-path-for sess))
         (check-false (agent-session-compacting? sess)))
       (lambda () (delete-directory/files dir))))

    (test-case "empty history emits exactly one nothing-to-compact terminal outcome"
      (define dir (make-temporary-file "q-compact-empty-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define bus (make-event-bus))
                      (define sess (make-test-session dir bus))
                      (define events (record-session-compact-events bus))
                      (define outcome (compact-session-durably! sess #:load-history (lambda (_) '())))
                      (check-equal? outcome 'nothing-to-compact)
                      (check-equal? (map event-ev (unbox events))
                                    '("session.compact.started" "session.compact.nothing-to-compact"))
                      (check-equal? (length (terminal-events (unbox events))) 1)
                      (check-false (agent-session-compacting? sess)))
                    (lambda () (delete-directory/files dir))))

    (test-case "already-running request is terminal and does not clear the active guard"
      (define dir (make-temporary-file "q-compact-running-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define bus (make-event-bus))
                      (define sess (make-test-session dir bus))
                      (define events (record-session-compact-events bus))
                      (guarded-set-compacting! sess #t)
                      (define outcome (compact-session-durably! sess))
                      (check-equal? outcome 'already-running)
                      (check-equal? (map event-ev (unbox events))
                                    '("session.compact.already-running"))
                      (check-equal? (length (terminal-events (unbox events))) 1)
                      (check-true (agent-session-compacting? sess))
                      (guarded-set-compacting! sess #f))
                    (lambda () (delete-directory/files dir))))

    (test-case "canonical path persists one summary and keeps every original log entry reconstructible"
      (define dir (make-temporary-file "q-compact-durable-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define bus (make-event-bus))
         ;; A real provider implementation selects the production tiered context path.
         (define provider
           (make-mock-provider (make-model-response
                                (list (hash 'type "text" 'text "requested answer"))
                                (hash)
                                "mock"
                                'stop)))
         (define sess (make-test-session dir bus provider))
         (define path (session-log-path-for sess))
         (define initial-log (load-session-log path))
         (define initial-parent (and (pair? initial-log) (message-id (last initial-log))))
         (define originals
           (for/list ([i (in-range 30)])
             (make-message (format "original-~a" i)
                           (if (zero? i)
                               initial-parent
                               (format "original-~a" (sub1 i)))
                           (if (even? i) 'user 'assistant)
                           'message
                           (list (make-text-part (make-string 80 #\x)))
                           (+ (current-inexact-milliseconds) i)
                           (hash))))
         (for ([message (in-list originals)])
           (append-entry! path message))
         (define compact-result-box (box #f))
         (define outcome
           (compact-session-durably! sess
                                     #:compact-and-persist
                                     (lambda (history log-path)
                                       (define result
                                         (compact-and-persist! history
                                                               log-path
                                                               #:token-config
                                                               (token-compaction-config 20 0 20)))
                                       (set-box! compact-result-box result)
                                       result)))
         (check-equal? outcome 'completed)
         (define trace-text
           (file->string (build-path (agent-session-session-dir sess) "trace.jsonl")))
         (check-true (string-contains? trace-text "session.compact.started"))
         (check-true (string-contains? trace-text "session.compact.completed"))
         (check-not-false (agent-session-index sess))
         (check-eq? (message-kind (active-leaf (agent-session-index sess))) 'compaction-summary)
         (define next-context
           (build-session-context-for-prompt sess
                                             "answer only this request"
                                             ensure-persisted!
                                             buffer-or-append!))
         (define context-payload
           (filter (lambda (message) (not (eq? (message-kind message) 'system-instruction)))
                   next-context))
         (define durable-result (unbox compact-result-box))
         (define expected-compacted-ids
           (cons (message-id (compaction-result-summary-message durable-result))
                 (map message-id (compaction-result-kept-messages durable-result))))
         (check-equal? (map message-id (drop-right context-payload 1)) expected-compacted-ids)
         (check-eq? (message-role (last context-payload)) 'user)
         (check-equal? (text-part-text (first (message-content (last next-context))))
                       "answer only this request")
         (define persisted (load-session-log path))
         (define persisted-ids (map message-id persisted))
         (for ([message (in-list originals)])
           (check-not-false (member (message-id message) persisted-ids)))
         (check-equal? (count (lambda (message) (eq? (message-kind message) 'compaction-summary))
                              persisted)
                       1))
       (lambda () (delete-directory/files dir))))

    (test-case "active prompt atomically rejects manual compaction"
      (define dir (make-temporary-file "q-compact-prompt-owner-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define bus (make-event-bus))
                      (define sess (make-test-session dir bus))
                      (define events (record-session-compact-events bus))
                      (check-true (try-claim-prompt! sess))
                      (check-equal? (compact-session-durably! sess #:request-id "blocked-by-prompt")
                                    'already-running)
                      (check-equal? (map event-ev (unbox events))
                                    '("session.compact.already-running"))
                      (release-prompt! sess))
                    (lambda () (delete-directory/files dir))))

    (test-case "concurrent requests have distinct correlated terminal outcomes"
      (define dir (make-temporary-file "q-compact-concurrent-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define bus (make-event-bus))
         (define sess (make-test-session dir bus))
         (define events (record-session-compact-events bus))
         (define entered (make-semaphore 0))
         (define release (make-semaphore 0))
         (define first-thread
           (thread (lambda ()
                     (compact-session-durably!
                      sess
                      #:request-id "request-one"
                      #:load-history (lambda (_) '(old-message))
                      #:compact-and-persist
                      (lambda (_history path)
                        (semaphore-post entered)
                        (semaphore-wait release)
                        (define summary
                          (make-message "concurrent-summary"
                                        #f
                                        'system
                                        'compaction-summary
                                        (list (make-text-part "internal summary"))
                                        (current-inexact-milliseconds)
                                        (hash)))
                        (append-entry! path summary)
                        (compaction-result summary 1 '()))))))
         (semaphore-wait entered)
         (check-exn #rx"compaction is active" (lambda () (run-prompt! sess "must wait")))
         (check-equal? (compact-session-durably! sess #:request-id "request-two") 'already-running)
         (semaphore-post release)
         (thread-wait first-thread)
         (define terminals (terminal-events (unbox events)))
         (check-equal? (length terminals) 2)
         (check-equal? (map (lambda (evt) (hash-ref (event-payload evt) 'request-id)) terminals)
                       '("request-two" "request-one"))
         (check-equal? (map event-ev terminals)
                       '("session.compact.already-running" "session.compact.completed"))
         (check-false (agent-session-compacting? sess)))
       (lambda () (delete-directory/files dir))))

    (test-case "exception emits one failed outcome and always resets compacting guard"
      (define dir (make-temporary-file "q-compact-fail-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define bus (make-event-bus))
                      (define sess (make-test-session dir bus))
                      (define events (record-session-compact-events bus))
                      (define outcome
                        (compact-session-durably! sess
                                                  #:load-history (lambda (_) '(m1))
                                                  #:compact-and-persist (lambda (_history _path)
                                                                          (error 'compact "boom"))))
                      (check-equal? outcome 'failed)
                      (check-equal? (map event-ev (unbox events))
                                    '("session.compact.started" "session.compact.failed"))
                      (check-equal? (length (terminal-events (unbox events))) 1)
                      (check-true (string-contains? (hash-ref (event-payload (last (unbox events)))
                                                              'error)
                                                    "boom"))
                      (check-false (agent-session-compacting? sess)))
                    (lambda () (delete-directory/files dir))))))

(run-tests lifecycle-tests)
