#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         rackunit/text-ui
         racket/string
         "../tui/state-types.rkt"
         (only-in "../tui/state-events/registry.rkt" event-reducer-registered? apply-event-to-state)
         (only-in "../tui/state-events/core-handlers.rkt"
                  handle-spawn-approval-requested
                  handle-spawn-approval-terminal)
         (only-in "../tui/render/message-layout.rkt"
                  styled-line?
                  styled-line-segments
                  styled-segment-text)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../runtime/approval/broker.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  register-approval-request-for-channel!
                  approval-decide!
                  cancel-approval-request!))

(define DIGEST-A (make-string 64 #\a))
(define DIGEST-B (make-string 64 #\b))
(define PRESENTATION-A (make-string 64 #\c))
(define PRESENTATION-B (make-string 64 #\d))

(struct approval-fixture (id commitment-digest presentation-digest) #:transparent)

(define (register-fixture capabilities
                          task-preview
                          #:commitment-digest [commitment-digest DIGEST-A]
                          #:presentation-digest [presentation-digest PRESENTATION-A])
  (define authoritative-view
    (hasheq 'plan-kind
            "batch"
            'capabilities
            capabilities
            'task-preview
            task-preview
            'model-preview
            "approved-model"
            'effective-tools
            '("read" "bash")
            'cwd-preview
            "approved-project"
            'provider-preview
            "approved-provider"
            'safe-mode
            #t
            'max-turns
            7
            'parent-session-preview
            "parent-session"
            'parent-call-preview
            "parent-call"
            'presentation-digest
            presentation-digest
            'jobs
            (list (hasheq 'batch-order
                          0
                          'job-id
                          "job-1"
                          'model-preview
                          "approved-model"
                          'max-turns
                          7
                          'effective-capabilities
                          capabilities
                          'effective-tools
                          '("read" "bash")
                          'tool-call-id
                          "call-1"
                          'child-id
                          "child-1"
                          'session-id
                          "session-1"))))
  (define id
    (register-approval-request-for-channel! (current-approval-channel)
                                            commitment-digest
                                            authoritative-view))
  (unless id
    (error 'register-fixture "active broker registration failed"))
  (approval-fixture id commitment-digest presentation-digest))

;; Normal request telemetry carries correlation and digests only. Presentation
;; is always obtained from the broker-owned immutable view.
(define (make-approval-event fixture)
  (make-event "mas.spawn-approval-requested"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'request-id
                      (approval-fixture-id fixture)
                      'commitment-digest
                      (approval-fixture-commitment-digest fixture)
                      'presentation-digest
                      (approval-fixture-presentation-digest fixture))))

(define (make-terminal-event fixture)
  (make-event "mas.spawn-approval-terminal"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'request-id
                      (approval-fixture-id fixture)
                      'commitment-digest
                      (approval-fixture-commitment-digest fixture))))

(define (make-initial-state)
  (initial-ui-state))

(define (with-approval-channel thunk)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
                thunk
                clear-approval-channel!))

(define-syntax-rule (approval-test-case name body ...)
  (test-case name
    (with-approval-channel (lambda ()
                             body ...))))

(define (overlay-text overlay)
  (apply string-append
         (for/list ([line (in-list (overlay-state-content overlay))])
           (apply string-append
                  (for/list ([segment (in-list (styled-line-segments line))])
                    (styled-segment-text segment))))))

(define suite
  (test-suite "TUI Approval Event Reducer (digest-bound broker)"

    (approval-test-case "handle-spawn-approval-requested creates approval-prompt overlay"
                        (define fixture (register-fixture '(shell-exec) "Run deploy script"))
                        (define new-state
                          (handle-spawn-approval-requested (make-initial-state)
                                                           (make-approval-event fixture)))
                        (define overlay (ui-state-active-overlay new-state))
                        (check-not-false overlay)
                        (check-equal? (overlay-state-type overlay) 'approval-prompt))

    (approval-test-case "overlay content is list of styled-lines"
                        (define fixture (register-fixture '(shell-exec) "Run deploy"))
                        (define new-state
                          (handle-spawn-approval-requested (make-initial-state)
                                                           (make-approval-event fixture)))
                        (define content (overlay-state-content (ui-state-active-overlay new-state)))
                        (check-true (pair? content))
                        (for ([line (in-list content)])
                          (check-true (styled-line? line))))

    (approval-test-case "overlay renders the authoritative broker view"
                        (define fixture (register-fixture '(shell-exec) "Run deploy script"))
                        (define new-state
                          (handle-spawn-approval-requested (make-initial-state)
                                                           (make-approval-event fixture)))
                        (define all-text (overlay-text (ui-state-active-overlay new-state)))
                        (check-true (string-contains? all-text "Approval"))
                        (check-true (string-contains? all-text "shell-exec"))
                        (check-true (string-contains? all-text "Run deploy script"))
                        (for ([committed-field (in-list '("approved-model" "approved-project"
                                                                           "approved-provider"
                                                                           "job-1"
                                                                           "call-1"
                                                                           "child-1"
                                                                           "session-1"))])
                          (check-true (string-contains? all-text committed-field))))

    (approval-test-case "forged event preview cannot replace the authoritative broker view"
                        (define fixture (register-fixture '(shell-exec) "broker-owned preview"))
                        (define forged-event
                          (make-event "mas.spawn-approval-requested"
                                      (current-inexact-milliseconds)
                                      "test-session"
                                      #f
                                      (hasheq 'request-id
                                              (approval-fixture-id fixture)
                                              'commitment-digest
                                              (approval-fixture-commitment-digest fixture)
                                              'presentation-digest
                                              (approval-fixture-presentation-digest fixture)
                                              'capabilities
                                              '(git-write)
                                              'task-preview
                                              "FORGED EVENT PREVIEW")))
                        (define new-state
                          (handle-spawn-approval-requested (make-initial-state) forged-event))
                        (define text (overlay-text (ui-state-active-overlay new-state)))
                        (check-true (string-contains? text "broker-owned preview"))
                        (check-true (string-contains? text "shell-exec"))
                        (check-false (string-contains? text "FORGED EVENT PREVIEW"))
                        (check-false (string-contains? text "git-write")))

    (approval-test-case
     "wrong or missing commitment/presentation digest is rejected at the UI boundary"
     (define fixture (register-fixture '(shell-exec) "authoritative"))
     (define invalid-payloads
       (list (hasheq 'request-id
                     (approval-fixture-id fixture)
                     'commitment-digest
                     DIGEST-B
                     'presentation-digest
                     (approval-fixture-presentation-digest fixture))
             (hasheq 'request-id
                     (approval-fixture-id fixture)
                     'commitment-digest
                     (approval-fixture-commitment-digest fixture)
                     'presentation-digest
                     PRESENTATION-B)
             (hasheq 'request-id
                     (approval-fixture-id fixture)
                     'commitment-digest
                     (approval-fixture-commitment-digest fixture))))
     (for ([payload (in-list invalid-payloads)])
       (define forged-event
         (make-event "mas.spawn-approval-requested"
                     (current-inexact-milliseconds)
                     "test-session"
                     #f
                     payload))
       (define state (make-initial-state))
       (define rejected (handle-spawn-approval-requested state forged-event))
       (check-eq? rejected state)
       (check-false (ui-state-active-overlay rejected))))

    (approval-test-case
     "multijob authoritative manifest renders inspectable rows"
     (define fixture
       (register-fixture '(shell-exec git-write)
                         (string-append "Dangerous subagent batch (2 jobs)\n"
                                        "[job-a] task=\"deploy\" caps=shell-exec child=child-a\n"
                                        "[job-b] task=\"commit\" caps=git-write child=child-b")))
     (define new-state
       (handle-spawn-approval-requested (make-initial-state) (make-approval-event fixture)))
     (define row-texts
       (for/list ([line (in-list (overlay-state-content (ui-state-active-overlay new-state)))])
         (apply string-append
                (for/list ([segment (in-list (styled-line-segments line))])
                  (styled-segment-text segment)))))
     (for ([expected (in-list '("job-a" "job-b" "child-a" "child-b"))])
       (check-true (ormap (lambda (text) (string-contains? text expected)) row-texts))))

    (approval-test-case "overlay extra binds authoritative presentation and commitment digests"
                        (define fixture
                          (register-fixture '(shell-exec git-write)
                                            "Push to prod"
                                            #:commitment-digest DIGEST-B
                                            #:presentation-digest PRESENTATION-B))
                        (define new-state
                          (handle-spawn-approval-requested (make-initial-state)
                                                           (make-approval-event fixture)))
                        (define extra (overlay-state-extra (ui-state-active-overlay new-state)))
                        (check-equal? (hash-ref extra 'request-id) (approval-fixture-id fixture))
                        (check-equal? (hash-ref extra 'commitment-digest) DIGEST-B)
                        (check-equal? (hash-ref extra 'presentation-digest) PRESENTATION-B)
                        (check-equal? (hash-ref extra 'capabilities) '(shell-exec git-write))
                        (check-equal? (hash-ref extra 'task-preview) "Push to prod"))

    (approval-test-case
     "approval request replaces a non-approval overlay"
     (define state-with-overlay
       (struct-copy ui-state
                    (make-initial-state)
                    [active-overlay (overlay-state 'command-palette '() "" 'top-left #f #f 0 #f)]))
     (define fixture (register-fixture '(file-write) "Edit config"))
     (define new-state
       (handle-spawn-approval-requested state-with-overlay (make-approval-event fixture)))
     (check-equal? (overlay-state-type (ui-state-active-overlay new-state)) 'approval-prompt))

    (approval-test-case
     "multiple capabilities are formatted with commas"
     (define fixture (register-fixture '(shell-exec git-write file-write) "Complex task"))
     (define new-state
       (handle-spawn-approval-requested (make-initial-state) (make-approval-event fixture)))
     (check-true (string-contains? (overlay-text (ui-state-active-overlay new-state))
                                   "shell-exec, git-write, file-write")))

    (approval-test-case "approval lifecycle reducers are registered"
                        (check-true (event-reducer-registered? "mas.spawn-approval-requested"))
                        (check-true (event-reducer-registered? "mas.spawn-approval-terminal")))

    (approval-test-case
     "apply-event-to-state dispatches to approval handler"
     (define fixture (register-fixture '(shell-exec) "Test task"))
     (define new-state (apply-event-to-state (make-initial-state) (make-approval-event fixture)))
     (check-equal? (overlay-state-type (ui-state-active-overlay new-state)) 'approval-prompt))

    (approval-test-case
     "empty authoritative capabilities and preview are handled"
     (define fixture (register-fixture '() ""))
     (define new-state
       (handle-spawn-approval-requested (make-initial-state) (make-approval-event fixture)))
     (check-equal? (overlay-state-type (ui-state-active-overlay new-state)) 'approval-prompt))

    (approval-test-case
     "missing request-id is rejected at the UI boundary"
     (define evt
       (make-event "mas.spawn-approval-requested"
                   (current-inexact-milliseconds)
                   "test-session"
                   #f
                   (hasheq 'commitment-digest DIGEST-A 'presentation-digest PRESENTATION-A)))
     (define state (make-initial-state))
     (define rejected (handle-spawn-approval-requested state evt))
     (check-eq? rejected state)
     (check-false (ui-state-active-overlay rejected)))

    (approval-test-case "cancelled broker registration is rejected at the UI boundary"
                        (define fixture (register-fixture '(shell-exec) "already terminal"))
                        (check-true (cancel-approval-request! (approval-fixture-id fixture)))
                        (define state (make-initial-state))
                        (define rejected
                          (handle-spawn-approval-requested state (make-approval-event fixture)))
                        (check-eq? rejected state)
                        (check-false (ui-state-active-overlay rejected)))

    (approval-test-case
     "terminal reducer promotes the next digest-bound request"
     (define fixture-a
       (register-fixture '(shell-exec)
                         "A"
                         #:commitment-digest DIGEST-A
                         #:presentation-digest PRESENTATION-A))
     (define fixture-b
       (register-fixture '(file-write)
                         "B"
                         #:commitment-digest DIGEST-B
                         #:presentation-digest PRESENTATION-B))
     (define state-a
       (handle-spawn-approval-requested (make-initial-state) (make-approval-event fixture-a)))
     (define state-ab (handle-spawn-approval-requested state-a (make-approval-event fixture-b)))
     (check-true (approval-decide! (approval-fixture-id fixture-a)
                                   (approval-fixture-commitment-digest fixture-a)
                                   #f))
     (define promoted (handle-spawn-approval-terminal state-ab (make-terminal-event fixture-a)))
     (define extra (overlay-state-extra (ui-state-active-overlay promoted)))
     (check-equal? (hash-ref extra 'request-id) (approval-fixture-id fixture-b))
     (check-equal? (hash-ref extra 'commitment-digest) DIGEST-B)
     (check-equal? (hash-ref extra 'presentation-digest) PRESENTATION-B)
     (check-equal? (hash-ref extra 'approval-queue) '()))))

(run-tests suite)
