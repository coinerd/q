#lang racket/base

;; scripts/tmux-explore/verifiers.rkt — Scenario-specific semantic evidence
;;
;; v0.99.50 W3 (TMUX-09): A real explorer result is PASS only when structured
;; lifecycle evidence is correlated. Pane prose, prompt echoes, generic mock
;; responses, stale events, and unrelated IDs are insufficient.

(require racket/list
         racket/string)

(provide required-scenario-tags
         (struct-out verification-result)
         verify-scenario-evidence)

(define required-scenario-tags
  '("memory" "gsd" "mas" "tools" "release-audit" "durable-memory" "resume" "compact"))

(struct verification-result (passed? status reasons evidence) #:transparent)

(define absent (gensym 'absent))

(define (key-candidates key)
  (define s
    (if (symbol? key)
        (symbol->string key)
        key))
  (remove-duplicates (list key
                           s
                           (string->symbol s)
                           (string->symbol (string-replace s "-" "_"))
                           (string-replace s "-" "_")
                           (string->symbol (string-replace s "_" "-"))
                           (string-replace s "_" "-"))))

(define (hash-ref-any h keys [default #f])
  (if (not (hash? h))
      default
      (let loop ([remaining keys])
        (cond
          [(null? remaining) default]
          [else
           (define found
             (for/or ([candidate (in-list (key-candidates (car remaining)))])
               (define value (hash-ref h candidate absent))
               (and (not (eq? value absent)) (cons #t value))))
           (if found
               (cdr found)
               (loop (cdr remaining)))]))))

(define (event-data event)
  (hash-ref-any event '(data payload details) (hash)))

(define (event-field event keys [default #f])
  (define top (hash-ref-any event keys absent))
  (if (eq? top absent)
      (hash-ref-any (event-data event) keys default)
      top))

(define (event-phase event)
  (define phase (event-field event '(phase event type) ""))
  (string-downcase (format "~a" phase)))

(define (event-turn event)
  (event-field event '(turn-id turnId turn sequence-id sequenceId) #f))

(define (event-session event)
  (event-field event '(session-id sessionId) #f))

(define completion-phases '("turn.completed" "stream.turn.completed"))

(define (completion-event? event)
  (and (member (event-phase event) completion-phases)
       (not (member (string-downcase (format "~a" (event-field event '(reason status) "completed")))
                    '("error" "failed" "cancelled" "canceled")))))

(define (phase=? event phase)
  (string=? (event-phase event) phase))

(define (events-with-phase events phase)
  (filter (lambda (event) (phase=? event phase)) events))

(define (same-value? a b)
  (and a b (equal? (format "~a" a) (format "~a" b))))

(define (truthy? value)
  (and (or (eq? value #t)
           (member (string-downcase (format "~a" value)) '("true" "yes" "present" "approved")))
       #t))

(define (correlated-to-completion? event completion)
  (define event-turn-id (event-turn event))
  (and (same-value? (event-session event) (event-session completion))
       (or (not event-turn-id) (same-value? event-turn-id (event-turn completion)))))

(define (index-of-event events target)
  (for/first ([event (in-list events)]
              [index (in-naturals)]
              #:when (eq? event target))
    index))

(define (ordered? events . targets)
  (define indexes (map (lambda (target) (index-of-event events target)) targets))
  (and (andmap exact-nonnegative-integer? indexes)
       (for/and ([a (in-list indexes)]
                 [b (in-list (cdr indexes))])
         (< a b))))

(define (verify-memory events completion)
  (for/or ([event (in-list (events-with-phase events "memory.retrieval.performed"))])
    (define count (event-field event '(result-count resultCount) 0))
    (and (correlated-to-completion? event completion)
         (or (and (exact-nonnegative-integer? count) (positive? count))
             (truthy? (event-field event '(result-present? result-present found?) #f)))
         (ordered? events event completion))))

(define (verify-gsd events completion)
  (for*/or ([attempt (in-list (events-with-phase events "gsd.transition.attempted"))]
            [success (in-list (events-with-phase events "gsd.transition.succeeded"))])
    (define attempt-id (event-field attempt '(transition-id transitionId plan-id)))
    (define success-id (event-field success '(transition-id transitionId plan-id)))
    (define matching-transition?
      (if (and attempt-id success-id)
          (same-value? attempt-id success-id)
          (and (same-value? (event-field attempt '(from)) (event-field success '(from)))
               (same-value? (event-field attempt '(to)) (event-field success '(to))))))
    (and (correlated-to-completion? attempt completion)
         (correlated-to-completion? success completion)
         matching-transition?
         (ordered? events attempt success completion))))

(define (verify-tools events completion [required-tool #f])
  (for*/or ([started (in-list (events-with-phase events "tool.execution.started"))]
            [completed (in-list (events-with-phase events "tool.execution.correlated-completed"))])
    (define started-name (event-field started '(tool-name toolName name)))
    (define completed-name (event-field completed '(tool-name toolName name)))
    (and (correlated-to-completion? started completion)
         (correlated-to-completion? completed completion)
         (same-value? (event-field started '(tool-call-id toolCallId call-id))
                      (event-field completed '(tool-call-id toolCallId call-id)))
         (same-value? started-name completed-name)
         (or (not required-tool)
             (string=? (string-downcase (format "~a" started-name)) required-tool))
         (truthy? (event-field completed '(result-present? result-present has-result?) #f))
         (string=? (string-downcase (format "~a" (event-field completed '(result-summary status))))
                   "completed")
         (ordered? events started completed completion))))

(define (verify-mas events completion-event)
  (for*/or ([request (in-list (events-with-phase events "mas.spawn-approval-requested"))]
            [decision (in-list (events-with-phase events "mas.spawn-approval-decided"))]
            [completed (in-list (events-with-phase events "tool.execution.correlated-completed"))])
    (define approval-id (event-field request '(approval-id request-id approvalId)))
    (define call-id (event-field request '(tool-call-id toolCallId call-id)))
    (and (correlated-to-completion? request completion-event)
         (correlated-to-completion? decision completion-event)
         (correlated-to-completion? completed completion-event)
         (event-field request '(child-id childId))
         (same-value? approval-id (event-field decision '(approval-id request-id approvalId)))
         (same-value? call-id (event-field decision '(tool-call-id toolCallId call-id)))
         (same-value? call-id (event-field completed '(tool-call-id toolCallId call-id)))
         (string=? (string-downcase (format "~a" (event-field decision '(decision status))))
                   "approved")
         (truthy? (event-field completed '(result-present? result-present has-result?) #f))
         (ordered? events request decision completed completion-event))))

(define (verify-release-audit events completion)
  (for/or ([event (in-list (events-with-phase events "release.authorization.refused"))])
    (and (correlated-to-completion? event completion)
         (eq? (event-field event '(authorized? authorized) #t) #f)
         (event-field event '(reason code))
         (ordered? events event completion))))

(define (resume-start? event)
  (and (phase=? event "session.started")
       (string=? (string-downcase (format "~a" (event-field event '(reason mode)))) "resume")
       (event-field event '(session-id sessionId))
       (event-field event '(previous-session-id previousSessionId))))

(define (verify-durable-memory events completion)
  (for*/or ([stored (in-list (events-with-phase events "memory.item.stored"))]
            [started (in-list (filter resume-start? events))]
            [retrieved (in-list (events-with-phase events "memory.retrieval.performed"))])
    (and (correlated-to-completion? retrieved completion)
         (same-value? (event-field stored '(memory-id item-id id))
                      (event-field retrieved '(memory-id item-id id)))
         (same-value? (event-field started '(session-id sessionId))
                      (event-field retrieved '(session-id sessionId)))
         (truthy? (event-field retrieved '(result-present? result-present found?) #f))
         (ordered? events stored started retrieved completion))))

(define (verify-resume events completion)
  (for*/or ([started (in-list (filter resume-start? events))]
            [resumed (in-list (events-with-phase events "session.resumed"))])
    (and (correlated-to-completion? resumed completion)
         (same-value? (event-field started '(session-id sessionId))
                      (event-field resumed '(session-id sessionId)))
         (same-value? (event-field started '(previous-session-id previousSessionId))
                      (event-field resumed '(previous-session-id previousSessionId)))
         (ordered? events started resumed completion))))

(define (verify-compact events terminal)
  (for*/or ([started (in-list (events-with-phase events "session.compact.started"))]
            [completed (in-list (events-with-phase events "session.compact.completed"))])
    (define removed (event-field completed '(removed-count removedCount) #f))
    (define kept (event-field completed '(kept-count keptCount) #f))
    (define before
      (or
       (event-field completed '(before-count beforeCount original-count) #f)
       (and (exact-nonnegative-integer? removed) (exact-nonnegative-integer? kept) (+ removed kept))))
    (define after (or (event-field completed '(after-count afterCount compacted-count) #f) kept))
    (and (eq? completed terminal)
         (same-value? (event-session started) (event-session completed))
         (same-value? (event-field started '(request-id requestId))
                      (event-field completed '(request-id requestId)))
         (exact-nonnegative-integer? removed)
         (positive? removed)
         (exact-nonnegative-integer? kept)
         (exact-nonnegative-integer? before)
         (exact-nonnegative-integer? after)
         (= after (add1 kept))
         (<= after before)
         (truthy? (event-field completed '(persisted? durable?) #f))
         (ordered? events started completed))))

(define (scenario-semantic-pass? tag events completion)
  (case (string->symbol tag)
    [(memory) (verify-memory events completion)]
    [(gsd) (verify-gsd events completion)]
    [(mas) (verify-mas events completion)]
    [(tools) (verify-tools events completion)]
    [(release-audit) (verify-release-audit events completion)]
    [(durable-memory) (verify-durable-memory events completion)]
    [(resume) (verify-resume events completion)]
    [(compact) (verify-compact events completion)]
    [else #f]))

(define (verify-scenario-evidence tag observation)
  (cond
    [(not (member tag required-scenario-tags))
     (verification-result #f 'unsupported (list "unregistered scenario tag") '())]
    [(not (hash? observation))
     (verification-result #f 'fail (list "executor returned no observation") '())]
    [else
     (define status (hash-ref-any observation '(status) 'failed))
     (define events (hash-ref-any observation '(trace-events traceEvents) '()))
     (define compact? (string=? tag "compact"))
     (define blockers
       (filter values
               (list (and (not (eq? status 'completed)) (format "terminal status is ~a" status))
                     (and (truthy? (hash-ref-any observation '(timed-out?) #f)) "execution timed out")
                     (and (truthy? (hash-ref-any observation '(crashed?) #f)) "session crashed")
                     (and (truthy? (hash-ref-any observation '(mock-provider?) #f))
                          "mock-provider fallback observed")
                     (and (not compact?)
                          (not (truthy? (hash-ref-any observation '(provider-confirmed?) #f)))
                          "non-mock provider execution is not positively confirmed")
                     (and compact?
                          (not (truthy? (hash-ref-any observation '(control-command-confirmed?) #f)))
                          "real control-command execution is not positively confirmed")
                     (and (not (and (list? events) (pair? events))) "structured trace is missing"))))
     (define completion
       (and (list? events)
            (findf (if compact?
                       (lambda (event) (phase=? event "session.compact.completed"))
                       completion-event?)
                   (reverse events))))
     (define turn-id (and completion (event-turn completion)))
     (define reasons
       (append blockers
               (if completion
                   '()
                   (list "terminal completion event is missing"))
               (if (or compact? (and completion turn-id))
                   '()
                   (list "completion turn ID is missing"))
               (if (and completion (event-session completion))
                   '()
                   (list "completion session ID is missing"))))
     (cond
       [(pair? reasons)
        (verification-result #f
                             'fail
                             reasons
                             (list (cons 'trace-event-count
                                         (if (list? events)
                                             (length events)
                                             0))))]
       [(scenario-semantic-pass? tag events completion)
        (verification-result #t
                             'pass
                             '()
                             (list (cons 'turn-id turn-id)
                                   (cons 'trace-event-count (length events))))]
       [else
        (verification-result #f
                             'fail
                             (list "scenario-specific correlated lifecycle evidence is absent")
                             (list (cons 'turn-id turn-id)
                                   (cons 'trace-event-count (length events))))])]))
