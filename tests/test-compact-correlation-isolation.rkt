#lang racket

;; @speed fast
;; @suite runtime
;; v0.99.51 W5 F-06: compact request→started→terminal correlation and
;; multi-session isolation. Red-first: the subscriber must filter by
;; target session, the TUI must track pending compact request/session
;; identity, terminal events must be correlated, and the verifier must
;; require a requested→started→terminal chain.

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
         (only-in "../runtime/compaction/compactor.rkt" compaction-result)
         (only-in "../util/message/protocol-types.rkt" make-message make-text-part)
         "../util/event/event-bus.rkt"
         (only-in "../util/event/event.rkt" make-event event-ev event-payload event-session-id)
         (only-in "../util/ids.rkt" generate-id))

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

(define (record-all-compact-events bus)
  (define events (box '()))
  (subscribe! bus
              (lambda (evt) (set-box! events (append (unbox events) (list evt))))
              #:filter (lambda (evt) (string-prefix? (event-ev evt) "session.compact.")))
  events)

(define terminal-names
  '("session.compact.completed" "session.compact.nothing-to-compact"
                                "session.compact.already-running"
                                "session.compact.failed"))

(define (terminal-events events-box)
  (filter (lambda (evt) (member (event-ev evt) terminal-names)) (unbox events-box)))

(define (compact-fn-removed5 _history path)
  (compaction-result #f 5 '(m6 m7)))

(define correlation-tests
  (test-suite "F-06: compact correlation and multi-session isolation"

    ;; ── Red test 1: subscriber ignores foreign-session requests ──
    ;; Two sessions share one bus. A compact request targeted at
    ;; session B must NOT cause session A to compact.
    (test-case "compact.requested for session B does not compact session A"
      (define dirA (make-temporary-file "q-compact-iso-A-~a" 'directory))
      (define dirB (make-temporary-file "q-compact-iso-B-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define bus (make-event-bus))
         ;; Create two sessions on the shared bus and wire handlers.
         (define sessA (make-test-session dirA bus))
         (define sessB (make-test-session dirB bus))
         (define sidA (agent-session-session-id sessA))
         (define sidB (agent-session-session-id sessB))
         (define all-events (record-all-compact-events bus))

         ;; Publish a compact request targeted at B only.
         (publish! bus
                   (make-event "session.compact.requested"
                               (current-inexact-milliseconds)
                               sidB
                               #f
                               (hasheq 'request-id "req-B" 'persist? #t 'target-session-id sidB)))
         (sleep 0.8) ; allow async subscriber threads

         ;; Only session B should have produced compact lifecycle events.
         ;; Session A must NOT appear in any lifecycle event.
         (define reacting-sessions
           (remove-duplicates (filter string?
                                      (map event-session-id
                                           (filter (lambda (evt)
                                                     (not (string=? (event-ev evt)
                                                                    "session.compact.requested")))
                                                   (unbox all-events))))))
         (check-false (member sidA reacting-sessions)
                      "session A must not react to session B's compact request"))
       (lambda ()
         (delete-directory/files dirA #:must-exist? #f)
         (delete-directory/files dirB #:must-exist? #f))))

    ;; ── Red test 2: subscriber processes matching-session requests ──
    (test-case "compact.requested with matching target-session-id compacts the target"
      (define dir (make-temporary-file "q-compact-match-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define bus (make-event-bus))
         (define sess (make-test-session dir bus))
         (define sid (agent-session-session-id sess))
         (define events (record-all-compact-events bus))

         ;; Publish a request with a matching target-session-id.
         (publish! bus
                   (make-event "session.compact.requested"
                               (current-inexact-milliseconds)
                               sid
                               #f
                               (hasheq 'request-id "req-1" 'persist? #t 'target-session-id sid)))
         (sleep 0.8)

         ;; The session should have produced at least started + one terminal.
         (define lifecycle (unbox events))
         (check-pred (lambda (es) (>= (length es) 2)) lifecycle)
         (check-equal? (length (terminal-events events)) 1))
       (lambda () (delete-directory/files dir #:must-exist? #f))))

    ;; ── Red test 3: request-id propagates through started→terminal chain ──
    (test-case "started and terminal events carry the same request-id"
      (define dir (make-temporary-file "q-compact-rid-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define bus (make-event-bus))
                      (define sess (make-test-session dir bus))
                      (define events (record-all-compact-events bus))
                      (compact-session-durably! sess
                                                #:request-id "req-correlated"
                                                #:load-history (lambda (_) '(m1 m2 m3 m4 m5 m6 m7))
                                                #:compact-and-persist compact-fn-removed5)
                      (define all-events (unbox events))
                      (check-pred (lambda (es) (>= (length es) 2)) all-events)
                      (define request-ids
                        (remove-duplicates (map (lambda (evt)
                                                  (hash-ref (event-payload evt) 'request-id #f))
                                                all-events)))
                      (check-equal? request-ids '("req-correlated")))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))

    ;; ── Red test 4: exactly one terminal per compact lifecycle ──
    (test-case "compact lifecycle produces exactly one terminal outcome"
      (define dir (make-temporary-file "q-compact-term-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define bus (make-event-bus))
                      (define sess (make-test-session dir bus))
                      (define events (record-all-compact-events bus))
                      (compact-session-durably! sess
                                                #:request-id "req-term"
                                                #:load-history (lambda (_) '(m1 m2 m3 m4 m5 m6 m7))
                                                #:compact-and-persist compact-fn-removed5)
                      (define terminals (terminal-events events))
                      (check-equal? (length terminals) 1))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))))

(run-tests correlation-tests)
