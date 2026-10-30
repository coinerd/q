#lang racket/base
;; @covers extensions/gsd/delivery-verifier.rkt
;; @covers extensions/gsd/verification-job.rkt
;; @speed slow
;; @suite extensions
;; @boundary e2e
;; @timeout 180
;;
;; R102 / BUG-0102 — terminal event identity contract (producer side).
;;
;; Every test here goes through the PUBLIC route: run-delivery-verification
;; over a real isolated registry, a real short subprocess gate, and an
;; INSTALLED bridge (emit-gsd-event! -> session bus -> TUI reducer).
;;
;; Contract (w0/security-contracts.md §3.2, BUG_PLAN R102):
;;   - started/completed carry additive trusted identity: registry
;;     incarnation, registry job-id, verify identity key, the bound
;;     attempt/fence triple, session attachment generation — never model
;;     text, bound at bridge ENTRY (not arrival time);
;;   - ONE terminal finalization path: exactly one gsd.verification.completed
;;     with the actual registry facts (state/exit/log) on normal completion,
;;     wait exception (after cancel/reap), observer exception, timeout and
;;     bus throw;
;;   - the ORIGINAL exception escapes; secondary cancel/emit failures stay
;;     distinct;
;;   - a cancelled/reaped attribution is published only from observed
;;     registry facts (live record or replacement tombstone);
;;   - a successful exit plus a rejected (raising) observer is NOT workflow
;;     approval: no delivery-verification value crosses at all.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "helpers/private-fixture-templates.rkt" call-with-private-git-environment)
         (only-in "helpers/delivery-fixtures.rkt" make-tmp-git-repo cleanup-tmp load-plan**)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  delivery-verification-approved?
                  current-gsd-delivery-verify-command
                  current-gsd-delivery-verify-timeout-sec
                  current-owned-verify-observer
                  current-gsd-verification-attempt-context
                  current-gsd-verification-attachment-generation
                  delivery-verify-identity
                  checkout-key)
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt"
                  make-verification-registry
                  verification-registry-incarnation
                  verification-job-facts
                  verification-job?
                  verification-job-state
                  verification-terminal-tombstone?
                  verification-terminal-tombstone-state
                  verification-terminal-tombstone-replacement-id
                  start-result-job-id
                  verification-start!
                  verification-cancel!
                  start-result?
                  verification-reconcile!
                  simulate-lost-process!
                  verification-close-registry!)
         (only-in "../extensions/gsd/events.rkt" set-gsd-event-bus!)
         ;; W4: the registry-backed reattach route (consumer side of the R102
         ;; identity contract) — foreign incarnation, unknown job and
         ;; replaced-job tombstone attribution.
         (only-in "../tui/commands.rkt" reattach-verification-state)
         (only-in "../tui/state-events.rkt" apply-event-to-state)
         (only-in "../tui/state-types.rkt"
                  initial-ui-state
                  ui-state-verification-progress
                  ui-state-status-message
                  ui-state-transcript
                  transcript-entry-meta
                  transcript-entry-text)
         (only-in "../util/event/event.rkt" event))

;; Typed pause exception used as a raising "observer rejection" (the shape a
;; review adapter raises when it refuses the observation).
(struct exn:test-pause exn ())

;; Install the REAL production bridge shape: emit-gsd-event! -> bus proc ->
;; wrapped payload -> TUI reducer (apply-event-to-state). The optional
;; on-event hook runs synchronously inside the emitter, at the exact
;; producer point the event is published. Returns the collected
;; (cons event-name wrapped-payload) list.
(define (call-with-verification-bus state-box sid thunk [on-event void])
  (define events-box (box '()))
  (define (bus name wrapped)
    (set-box! events-box (append (unbox events-box) (list (cons name wrapped))))
    (on-event name wrapped)
    (set-box! state-box
              (apply-event-to-state (unbox state-box)
                                    (event 1 name (current-inexact-milliseconds) sid "turn" wrapped))))
  (dynamic-wind
   (lambda () (set-gsd-event-bus! bus))
   thunk
   (lambda () (set-gsd-event-bus! void)))
  (unbox events-box))

(define (events-of-name events name)
  (for/list ([e (in-list events)] #:when (equal? (car e) name)) (cdr e)))

(define (event-data wrapped) (hash-ref wrapped 'data (hash)))

;; Capture log-warning output while thunk runs (secondary-failure
;; distinctness assertions).
(define (call-with-captured-warnings thunk)
  (define lg (make-logger 'r102-test))
  (define msgs (box '()))
  (define recv (make-log-receiver lg 'warning))
  (define pump
    (thread
     (lambda ()
       (let loop ()
         (define v (sync recv))
         (set-box! msgs (cons (vector-ref v 1) (unbox msgs)))
         (loop)))))
  (dynamic-wind
   (lambda () (current-logger lg))
   thunk
   (lambda ()
     (sleep 0.05)
     (kill-thread pump)))
  (reverse (unbox msgs)))

;; The verify command runs from the fixture's git root (two-tier layout:
;; base-dir/q), normalized exactly as the verifier normalizes it.
(define (fixture-run-cwd base) (checkout-key (build-path base "q")))

;; One observer-raise scenario, parameterized by the declared command, with
;; full assertions shared by the failed-job and exit-0 variants.
(define (run-observer-raise-test! command)
  (define base (make-tmp-git-repo))
  (define reg (make-verification-registry))
  (define state-box (box (initial-ui-state #:session-id "test-session")))
  (define pause-obj (exn:test-pause "observer rejects the observation" (current-continuation-marks)))
  (define escaped (box #f))
  (define returned (box 'no-value))
  (define events
    (parameterize ([current-gsd-verification-registry reg]
                   [current-gsd-delivery-verify-command command]
                   [current-gsd-delivery-verify-timeout-sec 60]
                   [current-owned-verify-observer (lambda (_o) (raise pause-obj))])
      (call-with-verification-bus
       state-box
       "test-session"
       (lambda ()
         (with-handlers ([exn:test-pause? (lambda (e) (set-box! escaped e))])
           (set-box! returned (run-delivery-verification base (load-plan** base '("q/ui-core/preferences.rkt")) 0)))))))
  (check-true (exn:test-pause? (unbox escaped)) "the observer exception escapes the verifier")
  (check-true (eq? (unbox escaped) pause-obj) "the ORIGINAL exception object escapes (no re-wrap)")
  (check-equal? (unbox returned) 'no-value "no delivery-verification value crosses an observer rejection")
  (define completed (events-of-name events 'gsd.verification.completed))
  (check-equal? (length completed) 1 "exactly ONE terminal update despite the observer failure")
  (values base
          reg
          state-box
          (and (= (length completed) 1) (event-data (car completed)))
          events))

;; W4: seed an IDENTIFIED foreground verification progress through the REAL
;; reducer (the consumer half of the R102 identity contract) so the reattach
;; route under test sees exactly the production state shape.
(define (seed-foreground-progress! state-box job-id incarnation)
  (set-box!
   state-box
   (apply-event-to-state
    (unbox state-box)
    (event 1
           'gsd.verification.started
           (current-inexact-milliseconds)
           "test-session"
           "turn"
           (hasheq 'wave
                   4
                   'timeout-sec
                   60
                   'start-ms
                   1000.0
                   'job-id
                   job-id
                   'registry-incarnation
                   incarnation
                   'verify-identity-key
                   "idkey-w4")))))

(define (terminal-events-suite)
  (test-suite "R102 terminal event identity contract (producer, public routes)"

    ;; ------------------------------------------------ observer raise: failed
    (test-case "terminal failed job + raising observer (typed pause): one terminal update, original exception escapes"
      (define-values (base reg state-box data events)
        (run-observer-raise-test! "( exit 3 )"))
      (check-equal? (hash-ref data 'state #f) 'failed)
      (check-equal? (hash-ref data 'exit-code #f) 3)
      (check-equal? (hash-ref data 'verdict #f) "reject")
      (check-true (symbol? (hash-ref data 'job-id #f)) "terminal event carries the registry job-id")
      (check-equal? (hash-ref data 'registry-incarnation #f)
                    (verification-registry-incarnation reg)
                    "terminal event carries the registry incarnation")
      (check-true (string? (hash-ref data 'verify-identity-key #f))
                  "terminal event carries the verify identity key")
      ;; the reducer saw exactly the one terminal transition
      (define verdict-entries
        (for/list ([entry (in-list (ui-state-transcript (unbox state-box)))]
                   #:when (hash-ref (transcript-entry-meta entry) 'verification #f))
          entry))
      (check-equal? (length verdict-entries) 1 "exactly one terminal UI transition")
      (check-false (ui-state-verification-progress (unbox state-box))
                   "terminal clears the foreground progress")
      (check-equal? (length (events-of-name events 'gsd.verification.started)) 1)
      (cleanup-tmp base))

    ;; ------------------------------------------ observer raise: exit 0
    (test-case "exit 0 + rejected (raising) observer is NOT workflow approval"
      (define-values (base _reg _state-box data _events)
        (run-observer-raise-test! "( exit 0 )"))
      (check-equal? (hash-ref data 'state #f) 'completed)
      (check-equal? (hash-ref data 'exit-code #f) 0)
      (cleanup-tmp base))

    ;; ---------------------------------------------------------- timeout
    (test-case "timeout publishes exactly one terminal event with the actual timed-out facts"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (define events
        (parameterize ([current-gsd-verification-registry reg]
                       [current-gsd-delivery-verify-command "( sleep 30 )"]
                       [current-gsd-delivery-verify-timeout-sec 1])
          (call-with-verification-bus
           state-box
           "test-session"
           (lambda () (run-delivery-verification base (load-plan** base '("q/ui-core/preferences.rkt")) 0)))))
      (define completed (events-of-name events 'gsd.verification.completed))
      (check-equal? (length completed) 1 "exactly one terminal update on timeout")
      (when (= (length completed) 1)
        (define data (event-data (car completed)))
        (check-equal? (hash-ref data 'state #f) 'timed-out)
        (check-equal? (hash-ref data 'exit-code #f) 124)
        (check-equal? (hash-ref data 'verdict #f) "reject")
        (check-equal? (hash-ref data 'registry-incarnation #f)
                      (verification-registry-incarnation reg)))
      (check-equal? (length (events-of-name events 'gsd.verification.started)) 1)
      (cleanup-tmp base))

    ;; -------------------------------------------------- identity fields
    (test-case "started and completed carry the additive trusted identity fields"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (define events
        (parameterize ([current-gsd-verification-registry reg]
                       [current-gsd-delivery-verify-command "( exit 0 )"]
                       [current-gsd-delivery-verify-timeout-sec 60]
                       [current-gsd-verification-attachment-generation 7]
                       [current-gsd-verification-attempt-context
                        (hasheq 'attempt-id "attempt-42" 'attempt-fence "af-9" 'coordinator-fence "cf-3")])
          (call-with-verification-bus
           state-box
           "test-session"
           (lambda () (run-delivery-verification base (load-plan** base '("q/ui-core/preferences.rkt")) 0)))))
      (define started (events-of-name events 'gsd.verification.started))
      (define completed (events-of-name events 'gsd.verification.completed))
      (check-equal? (length started) 1)
      (check-equal? (length completed) 1)
      (for ([e (in-list (append started completed))])
        (define data (event-data e))
        (check-true (symbol? (hash-ref data 'job-id #f)) "job-id present")
        (check-equal? (hash-ref data 'registry-incarnation #f)
                      (verification-registry-incarnation reg)
                      "registry incarnation present")
        (check-true (string? (hash-ref data 'verify-identity-key #f)) "verify identity key present")
        (check-equal? (hash-ref data 'attempt-id #f) "attempt-42" "bound attempt id present")
        (check-equal? (hash-ref data 'attempt-fence #f) "af-9" "bound attempt fence present")
        (check-equal? (hash-ref data 'coordinator-fence #f) "cf-3" "bound coordinator fence present")
        (check-equal? (hash-ref data 'session-attachment-generation #f)
                      7
                      "session attachment generation present"))
      (check-equal? (hash-ref (event-data (car started)) 'job-id #f)
                    (hash-ref (event-data (car completed)) 'job-id #f)
                    "started/completed name the same registry job")
      (cleanup-tmp base))

    ;; --------------------------------------- entry-bound, not arrival-time
    (test-case "identity facts are bound at bridge ENTRY, not at event-arrival time"
      ;; ONE process-wide bus (the production shape): a second caller ATTACHES
      ;; to the owned job while the first is still waiting. Each caller's
      ;; events carry ITS OWN entry-bound attachment generation / attempt
      ;; context, and each caller's terminal publication keeps its ENTRY
      ;; binding — never a value read at arrival time.
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (define caller1-started (box #f))
      (define command "( sleep 10 )")
      (define events-box (box '()))
      (define (bus name wrapped)
        (set-box! events-box (append (unbox events-box) (list (cons name wrapped))))
        (when (and (equal? name 'gsd.verification.started)
                   (equal? (hash-ref (hash-ref wrapped 'data) 'session-attachment-generation #f) 101))
          (set-box! caller1-started #t))
        (set-box! state-box
                  (apply-event-to-state (unbox state-box)
                                        (event 1 name (current-inexact-milliseconds)
                                               "test-session" "turn" wrapped))))
      (define errors (box '()))
      (define (run-as generation attempt-id)
        (with-handlers ([exn:fail? (lambda (e) (set-box! errors (cons e (unbox errors))))])
          (parameterize ([current-gsd-verification-registry reg]
                         [current-gsd-delivery-verify-command command]
                         [current-gsd-delivery-verify-timeout-sec 60]
                         [current-gsd-verification-attachment-generation generation]
                         [current-gsd-verification-attempt-context
                          (hasheq 'attempt-id attempt-id
                                  'attempt-fence (string-append "f" attempt-id)
                                  'coordinator-fence (string-append "c" attempt-id))])
            (run-delivery-verification base (load-plan** base '("q/ui-core/preferences.rkt")) 0))))
      (dynamic-wind
       (lambda () (set-gsd-event-bus! bus))
       (lambda ()
         (define t1 (thread (lambda () (run-as 101 "A"))))
         (define t2
           (thread
            (lambda ()
              (let wait ([n 500])
                (unless (or (unbox caller1-started) (zero? n))
                  (sleep 0.02)
                  (wait (sub1 n))))
              (run-as 202 "B"))))
         (check-not-false (sync/timeout 30 t1) "caller 1 finished")
         (check-not-false (sync/timeout 30 t2) "caller 2 finished"))
       (lambda () (set-gsd-event-bus! void)))
      (check-equal? (unbox errors) '() "no caller failed")
      (check-true (unbox caller1-started) "caller 1 started the owned job")
      (define all (unbox events-box))
      (define (gen-of e) (hash-ref (event-data (cdr e)) 'session-attachment-generation #f))
      (define (attempt-of e) (hash-ref (event-data (cdr e)) 'attempt-id #f))
      (define ev1-started
        (for/first ([e (in-list all)]
                    #:when (and (equal? (car e) 'gsd.verification.started)
                                (equal? (gen-of e) 101)))
          (cdr e)))
      (define ev1-completed
        (for/first ([e (in-list all)]
                    #:when (and (equal? (car e) 'gsd.verification.completed)
                                (equal? (gen-of e) 101)))
          (cdr e)))
      (define ev2-started
        (for/first ([e (in-list all)]
                    #:when (and (equal? (car e) 'gsd.verification.started)
                                (equal? (gen-of e) 202)))
          (cdr e)))
      (check-true (and ev1-started #t) "caller 1 started observed")
      (check-true (and ev1-completed #t) "caller 1 terminal observed")
      (check-true (and ev2-started #t) "caller 2 attach-started observed")
      (when (and ev1-started ev1-completed ev2-started)
        (check-equal? (hash-ref (event-data ev1-started) 'session-attachment-generation #f)
                      101
                      "caller 1 entry-bound generation")
        (check-equal? (hash-ref (event-data ev1-completed) 'session-attachment-generation #f)
                      101
                      "terminal publication keeps the ENTRY binding, not a later one")
        (check-equal? (hash-ref (event-data ev1-started) 'attempt-id #f) "A")
        (check-equal? (hash-ref (event-data ev1-completed) 'attempt-id #f) "A")
        (check-equal? (hash-ref (event-data ev2-started) 'session-attachment-generation #f)
                      202
                      "caller 2 entry-bound generation")
        (check-equal? (hash-ref (event-data ev2-started) 'attempt-id #f) "B")
        (check-equal? (hash-ref (event-data ev2-started) 'attached? #f)
                      #t
                      "caller 2 attached to the owned singleton")
        (check-equal? (hash-ref (event-data ev1-started) 'job-id #f)
                      (hash-ref (event-data ev2-started) 'job-id #f)
                      "one owned job, two entry bindings"))
      (check-equal? (length (for/list ([e (in-list all)]
                                        #:when (equal? (car e) 'gsd.verification.completed))
                              e))
                    2
                    "each attached caller publishes exactly one terminal")
      (cleanup-tmp base))

    ;; --------------------------------- replacement + failing cancel + tombstone
    (test-case "wait failure with failing cancel: primary exception kept, one terminal from observed tombstone facts"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (define command "( sleep 30 )")
      (define ident (delivery-verify-identity base 0 (fixture-run-cwd base) command))
      (define replacement (box #f))
      (define escaped (box #f))
      (define original-job (box #f))
      (define reconcile-state (box #f))
      (define events-box (box '()))
      (define (bus name wrapped)
        (set-box! events-box (append (unbox events-box) (list (cons name wrapped))))
        (set-box! state-box
                  (apply-event-to-state (unbox state-box)
                                        (event 1 name (current-inexact-milliseconds)
                                               "test-session" "turn" wrapped)))
        ;; runs synchronously at the started emission — after start-or-attach!,
        ;; BEFORE the producer's wait: kill out of band, reconcile
        ;; terminal+reaped, then REPLACE the handle for the same identity
        (when (equal? name 'gsd.verification.started)
          (define job-id (hash-ref (hash-ref wrapped 'data) 'job-id))
          (set-box! original-job job-id)
          (simulate-lost-process! reg job-id)
          (let die ([n 500])
            (define facts (verification-job-facts reg job-id))
            (when (and facts
                       (verification-job? facts)
                       (eq? (verification-job-state facts) 'running)
                       (positive? n))
              (sleep 0.01)
              (die (sub1 n))))
          (define rec (verification-reconcile! reg job-id))
          (set-box! reconcile-state (verification-job-state rec))
          (set-box! replacement (verification-start! reg ident "/bin/sh" (list "-c" command)))))
      (define warnings
        (call-with-captured-warnings
         (lambda ()
           (dynamic-wind
            (lambda () (set-gsd-event-bus! bus))
            (lambda ()
              (parameterize ([current-gsd-verification-registry reg]
                             [current-gsd-delivery-verify-command command]
                             [current-gsd-delivery-verify-timeout-sec 60])
                (with-handlers ([exn:fail? (lambda (e) (set-box! escaped e))])
                  (run-delivery-verification base (load-plan** base '("q/ui-core/preferences.rkt")) 0))))
            (lambda () (set-gsd-event-bus! void))))))
      (define events (unbox events-box))
      (check-equal? (unbox reconcile-state) 'orphan-recovered "replacement precondition held")
      (check-pred start-result? (unbox replacement))
      (check-true (exn:fail? (unbox escaped)) "the primary wait failure escapes")
      (check-true (string-contains? (exn-message (unbox escaped)) "unknown job")
                  "the primary failure is the vanished job id, not the cancel failure")
      (define completed (events-of-name events 'gsd.verification.completed))
      (check-equal? (length completed) 1 "exactly one terminal publication")
      (when (= (length completed) 1)
        (define data (event-data (car completed)))
        (check-equal? (hash-ref data 'job-id #f) (unbox original-job))
        (check-equal? (hash-ref data 'state #f) 'orphan-recovered)
        (check-equal? (hash-ref data 'registry-incarnation #f)
                      (verification-registry-incarnation reg))
        (check-false (hash-ref data 'approved? #t) "an orphan-recovered job never approves"))
      (check-true
       (for/or ([m (in-list warnings)])
         (string-contains? (format "~a" m) "secondary"))
       "the cancel failure is logged as a SECONDARY failure, distinct from the primary")
      (with-handlers ([exn:fail? void])
        (verification-close-registry! reg))
      (cleanup-tmp base))

    ;; ---------------------------------------------- tombstone attribution
    (test-case "replaced jobs stay attributable: tombstone carries the captured terminal record"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define ident (delivery-verify-identity base 0 (fixture-run-cwd base) "( exit 0 )"))
      (define r1 (verification-start! reg ident "/bin/sh" (list "-c" "( sleep 30 )")))
      (check-pred verification-job? (verification-job-facts reg (start-result-job-id r1)))
      (verification-cancel! reg (start-result-job-id r1))
      (define r2 (verification-start! reg ident "/bin/sh" (list "-c" "( exit 0 )")))
      (check-not-equal? (start-result-job-id r2) (start-result-job-id r1) "replacement got a fresh job id")
      (define tomb (verification-job-facts reg (start-result-job-id r1)))
      (check-true (verification-terminal-tombstone? tomb) "the replaced job is retained as a tombstone")
      (when (verification-terminal-tombstone? tomb)
        (check-equal? (verification-terminal-tombstone-state tomb) 'cancelled)
        (check-equal? (verification-terminal-tombstone-replacement-id tomb)
                      (start-result-job-id r2)))
      (check-pred verification-job? (verification-job-facts reg (start-result-job-id r2)))
      (check-false (verification-job-facts reg 'vj-424242) "unknown job stays #f")
      (verification-close-registry! reg)
      (cleanup-tmp base))

    ;; ----------------------------------------------------- incarnation
    (test-case "registry recreation mints a fresh incarnation; foreign facts are refused"
      (define reg1 (make-verification-registry))
      (define reg2 (make-verification-registry))
      (check-true (symbol? (verification-registry-incarnation reg1)))
      (check-true (symbol? (verification-registry-incarnation reg2)))
      (check-not-equal? (verification-registry-incarnation reg1)
                        (verification-registry-incarnation reg2)
                        "each registry carries its own incarnation")
      (check-false (verification-job-facts reg1 'vj-999) "unknown job yields #f, never an error"))

    ;; -------------------------------------------------- W4 reattach route
    ;; The restore path drops cached UI clocks and consults ONLY the ambient
    ;; registry. W4: every outcome names its attribution explicitly — a
    ;; foreign incarnation (process restart) or an unknown job is an explicit
    ;; unknown, never a silent clear, and never a relaunch: this route only
    ;; renders observed registry facts. The shared foreground-progress seeder
    ;; is defined at module level (seed-foreground-progress!) because test-suite
    ;; bodies are expressions, not definition contexts.
    (test-case "reattach: a foreign-incarnation job is attributed explicitly, not silently cleared"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (seed-foreground-progress! state-box 'vj-old 'vjreg-dead)
      (check-true (hash? (ui-state-verification-progress (unbox state-box))))
      (define restored
        (parameterize ([current-gsd-verification-registry reg])
          (reattach-verification-state (unbox state-box))))
      (check-false (ui-state-verification-progress restored) "cached clocks are dropped")
      (define text (ui-state-status-message restored))
      (check-true (string? text) "an explicit attribution replaces the silent clear")
      (check-true (string-contains? text "foreign") text)
      (check-true (string-contains? text "vj-old") "the registry job-id stays correlatable")
      (check-true (string-contains? text "not relaunched") text)
      (cleanup-tmp base))

    (test-case "reattach: an unknown job in the same incarnation is explicit too"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (seed-foreground-progress! state-box 'vj-424242 (verification-registry-incarnation reg))
      (define restored
        (parameterize ([current-gsd-verification-registry reg])
          (reattach-verification-state (unbox state-box))))
      (check-false (ui-state-verification-progress restored))
      (define text (ui-state-status-message restored))
      (check-true (string? text) "no silent clear")
      (check-true (string-contains? text "unknown") text)
      (check-true (string-contains? text "not relaunched") text)
      (cleanup-tmp base))

    (test-case "reattach: a replaced job's tombstone yields a truthful verdict, no crash"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define ident (delivery-verify-identity base 0 (fixture-run-cwd base) "( exit 0 )"))
      (define r1 (verification-start! reg ident "/bin/sh" (list "-c" "( sleep 30 )")))
      (verification-cancel! reg (start-result-job-id r1))
      (verification-start! reg ident "/bin/sh" (list "-c" "( exit 0 )"))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (seed-foreground-progress!
       state-box (start-result-job-id r1) (verification-registry-incarnation reg))
      (define restored
        (parameterize ([current-gsd-verification-registry reg])
          (reattach-verification-state (unbox state-box))))
      (define entries
        (for/list ([entry (in-list (ui-state-transcript restored))]
                   #:when (hash-ref (transcript-entry-meta entry) 'detached #f))
          entry))
      (check-equal? (length entries) 1 "the captured tombstone is attributed truthfully")
      (check-true (string-contains? (transcript-entry-text (car entries)) "cancelled")
                  (transcript-entry-text (car entries)))
      (with-handlers ([exn:fail? void])
        (verification-close-registry! reg))
      (cleanup-tmp base))

    (test-case "reattach keeps the registry-backed detached and terminal routes"
      (define base (make-tmp-git-repo))
      (define reg (make-verification-registry))
      (define ident (delivery-verify-identity base 0 (fixture-run-cwd base) "( exit 0 )"))
      (define r (verification-start! reg ident "/bin/sh" (list "-c" "( sleep 30 )")))
      (define state-box (box (initial-ui-state #:session-id "test-session")))
      (seed-foreground-progress!
       state-box (start-result-job-id r) (verification-registry-incarnation reg))
      (define detached
        (parameterize ([current-gsd-verification-registry reg])
          (reattach-verification-state (unbox state-box))))
      (check-true (string-contains? (ui-state-status-message detached) "running (detached)")
                  (ui-state-status-message detached))
      (verification-cancel! reg (start-result-job-id r))
      (define terminal
        (parameterize ([current-gsd-verification-registry reg])
          (reattach-verification-state (unbox state-box))))
      (check-true
       (for/or ([entry (in-list (ui-state-transcript terminal))]
                #:when (hash-ref (transcript-entry-meta entry) 'detached #f))
         (string-contains? (transcript-entry-text entry) "state cancelled"))
       "the terminal registry record is rendered after the wait")
      (with-handlers ([exn:fail? void])
        (verification-close-registry! reg))
      (cleanup-tmp base))))

(module+ main
  (exit (call-with-private-git-environment (lambda () (run-tests (terminal-events-suite))))))
