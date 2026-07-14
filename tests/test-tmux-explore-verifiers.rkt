#lang racket/base

;; @speed fast
;; @suite default

;; v0.99.50 W3 (TMUX-09): semantic verifier fixtures for every exact
;; registered explorer tag. Positive fixtures carry correlated structured
;; lifecycle evidence; deceptive negatives contain plausible prose or
;; unrelated/mismatched events and must fail.

(require rackunit
         rackunit/text-ui
         racket/list
         "../scripts/tmux-explore/verifiers.rkt")

(define turn "turn-1")
(define call "call-1")
(define approval "approval-1")
(define session "session-1")
(define memory "memory-1")

(define (evt phase #:turn [turn-id turn] #:data [data (hash)])
  (hash 'phase phase 'session-id session 'turn-id turn-id 'data data))

(define completion (evt "turn.completed"))

(define positive-fixtures
  (hash
   "memory"
   (list (evt "memory.retrieval.performed"
              #:data (hash 'scope "session" 'query-snippet "blue-harbor" 'result-count 1))
         completion)
   "gsd"
   (list (evt "gsd.transition.attempted" #:data (hash 'from "planned" 'to "executing"))
         (evt "gsd.transition.succeeded" #:data (hash 'from "planned" 'to "executing"))
         completion)
   "mas"
   (list (evt "mas.spawn-approval-requested"
              #:data (hash 'approval-id approval 'tool-call-id call 'child-id "child-1"))
         (evt "mas.spawn-approval-decided"
              #:data (hash 'approval-id approval 'tool-call-id call 'decision "approved"))
         (evt "tool.execution.correlated-completed"
              #:data (hash 'tool-call-id
                           call
                           'tool-name
                           "spawn_subagent"
                           'result-present?
                           #t
                           'result-summary
                           "completed"))
         completion)
   "tools"
   (list
    (evt "tool.execution.started" #:data (hash 'tool-call-id call 'tool-name "read"))
    (evt "tool.execution.correlated-completed"
         #:data
         (hash 'tool-call-id call 'tool-name "read" 'result-present? #t 'result-summary "completed"))
    completion)
   "release-audit"
   (list (evt "release.authorization.refused" #:data (hash 'reason "missing-live-ci" 'authorized? #f))
         completion)
   "durable-memory"
   (list (evt "memory.item.stored"
              #:turn "turn-store"
              #:data (hash 'memory-id memory 'session-id "session-0"))
         (evt "session.started"
              #:turn #f
              #:data (hash 'reason "resume" 'session-id session 'previous-session-id "session-0"))
         (evt "memory.retrieval.performed"
              #:data (hash 'memory-id memory 'session-id session 'result-present? #t))
         completion)
   "resume"
   (list (evt "session.started"
              #:turn #f
              #:data (hash 'reason "resume" 'session-id session 'previous-session-id "session-0"))
         (evt "session.resumed" #:data (hash 'session-id session 'previous-session-id "session-0"))
         completion)
   "compact"
   (list (evt "session.compact.started"
              #:turn #f
              #:data (hash 'session-id session 'request-id "compact-1" 'persisted? #f))
         (evt "session.compact.completed"
              #:turn #f
              #:data (hash 'session-id
                           session
                           'request-id
                           "compact-1"
                           'before-count
                           42
                           'after-count
                           13
                           'removed-count
                           30
                           'kept-count
                           12
                           'persisted?
                           #t)))
   "interrupt"
   (list
    (evt "interrupt.requested"
         #:turn "prompt-turn-a"
         #:data
         (hash 'request-id "interrupt-1" 'target-session-id session 'target-turn-id "prompt-turn-a"))
    (evt "interrupt.accepted"
         #:turn "prompt-turn-a"
         #:data
         (hash 'request-id "interrupt-1" 'target-session-id session 'target-turn-id "prompt-turn-a"))
    (evt "turn.cancelled"
         #:turn "prompt-turn-a"
         #:data
         (hash 'request-id "interrupt-1" 'target-session-id session 'target-turn-id "prompt-turn-a"))
    (evt "turn.started" #:turn "recovery-turn")
    (evt "turn.completed" #:turn "recovery-turn"))))

(define negative-fixtures
  (hash
   ;; Plausible answer and completion, but no retrieval event.
   "memory"
   (list completion)
   ;; Attempt and success are unrelated transition IDs.
   "gsd"
   (list (evt "gsd.transition.attempted" #:data (hash 'from "planned" 'to "executing"))
         (evt "gsd.transition.succeeded" #:data (hash 'from "planned" 'to "done"))
         completion)
   ;; Approval decision and result target another tool call.
   "mas"
   (list (evt "mas.spawn-approval-requested"
              #:data (hash 'approval-id approval 'tool-call-id call 'child-id "child-1"))
         (evt "mas.spawn-approval-decided"
              #:data (hash 'approval-id approval 'tool-call-id "other" 'decision "approved"))
         (evt "tool.execution.correlated-completed"
              #:data (hash 'tool-call-id
                           "other"
                           'tool-name
                           "spawn_subagent"
                           'result-present?
                           #t
                           'result-summary
                           "completed"))
         completion)
   ;; Tool names and call IDs do not match.
   "tools"
   (list
    (evt "tool.execution.started" #:data (hash 'tool-call-id call 'tool-name "read"))
    (evt
     "tool.execution.correlated-completed"
     #:data
     (hash 'tool-call-id "other" 'tool-name "bash" 'result-present? #t 'result-summary "completed"))
    completion)
   ;; Refusal prose alone must not count as protocol evidence.
   "release-audit"
   (list completion)
   ;; Store/retrieve IDs differ and no resume edge.
   "durable-memory"
   (list (evt "memory.item.stored"
              #:turn "turn-store"
              #:data (hash 'memory-id memory 'session-id "session-0"))
         (evt "memory.retrieval.performed"
              #:data (hash 'memory-id "other" 'session-id session 'result-present? #t))
         completion)
   ;; Fresh session is not resume.
   "resume"
   (list (evt "session.started" #:turn #f #:data (hash 'reason "new" 'session-id session)) completion)
   ;; Accepted/prose without terminal compact completion is not PASS.
   "compact"
   (list (evt "session.compact.requested" #:data (hash 'session-id session)) completion)
   ;; Cancellation acknowledgement belongs to another request.
   "interrupt"
   (list
    (evt "interrupt.requested"
         #:turn "prompt-turn-a"
         #:data
         (hash 'request-id "interrupt-1" 'target-session-id session 'target-turn-id "prompt-turn-a"))
    (evt "interrupt.accepted" #:turn "prompt-turn-a" #:data (hash 'request-id "interrupt-1"))
    (evt "turn.cancelled" #:turn "prompt-turn-a" #:data (hash 'request-id "other-request"))
    (evt "turn.started" #:turn "recovery-turn")
    (evt "turn.completed" #:turn "recovery-turn"))))

(define (observation events
                     #:capture [capture "plausible assistant prose says everything passed"]
                     #:status [status 'completed]
                     #:mock? [mock? #f]
                     #:control? [control? #t]
                     #:timed-out? [timed-out? #f]
                     #:crashed? [crashed? #f])
  (hash 'status
        status
        'trace-events
        events
        'capture
        capture
        'provider-confirmed?
        (not mock?)
        'control-command-confirmed?
        control?
        'mock-provider?
        mock?
        'timed-out?
        timed-out?
        'crashed?
        crashed?))

(define suite
  (test-suite "tmux explorer semantic verifiers"

    (test-case "interrupt is a required semantic scenario"
      (check-not-false (member "interrupt" required-scenario-tags)))

    (test-case "all exact registered tags accept correlated positive fixtures"
      (for ([tag (in-list required-scenario-tags)])
        (define result (verify-scenario-evidence tag (observation (hash-ref positive-fixtures tag))))
        (check-true
         (verification-result-passed? result)
         (format "positive fixture should pass: ~a / ~a" tag (verification-result-reasons result)))))

    (test-case "all exact registered tags reject deceptive negative fixtures"
      (for ([tag (in-list required-scenario-tags)])
        (define result (verify-scenario-evidence tag (observation (hash-ref negative-fixtures tag))))
        (check-false (verification-result-passed? result)
                     (format "deceptive fixture must fail: ~a" tag))))

    (test-case "timeout missing trace mock fallback and crash can never pass"
      (define valid-tools (hash-ref positive-fixtures "tools"))
      (for ([obs (in-list (list (observation valid-tools #:timed-out? #t)
                                (observation '())
                                (observation valid-tools #:mock? #t)
                                (observation valid-tools #:crashed? #t)
                                (observation valid-tools #:status 'failed)))])
        (check-false (verification-result-passed? (verify-scenario-evidence "tools" obs)))))

    (test-case "compact lifecycle requires one matching request ID"
      (define events
        (list (evt "session.compact.started"
                   #:turn #f
                   #:data (hash 'session-id session 'request-id "request-a"))
              (evt "session.compact.completed"
                   #:turn #f
                   #:data (hash 'session-id
                                session
                                'request-id
                                "request-b"
                                'before-count
                                10
                                'after-count
                                4
                                'removed-count
                                7
                                'kept-count
                                3
                                'persisted?
                                #t))))
      (check-false (verification-result-passed? (verify-scenario-evidence "compact"
                                                                          (observation events)))))

    (test-case "interrupt recovery must remain in the interrupted session"
      (define events
        (for/list ([event (in-list (hash-ref positive-fixtures "interrupt"))])
          (if (member (hash-ref event 'turn-id #f) '("recovery-turn"))
              (hash-set event 'session-id "foreign-session")
              event)))
      (check-false (verification-result-passed? (verify-scenario-evidence "interrupt"
                                                                          (observation events)))))

    (test-case "unknown scenario tag fails closed"
      (define result (verify-scenario-evidence "not-registered" (observation (list completion))))
      (check-false (verification-result-passed? result))
      (check-equal? (verification-result-status result) 'unsupported))))

(run-tests suite)
