#lang racket/base
;; @covers tui/state-events/core-handlers.rkt
;; @covers tui/state-types.rkt
;; @covers tui/commands.rkt
;; @speed fast
;; @suite tui
;; @boundary unit
;;
;; R102 / BUG-0102 — verification foreground lifecycle (consumer side).
;;
;; Public routes only: the real event reducer (apply-event-to-state), a REAL
;; event bus with DUPLICATE subscriptions, and the real campaign stop/error +
;; session-restore lifecycle through commands.rkt's public
;; execute-extension-command route (campaign token, no factory -> stop/error
;; path + restore run synchronously).
;;
;; Matrix (BUG_PLAN R102):
;;   A-start/B-start/late-A-complete, duplicate completion (dedupe =>
;;   exactly-once UI transition), late start after terminal (no timer
;;   resurrection), same-wave new attempt, old-session events, identity-less
;;   legacy events, A->B->A queued events, and stop/error/restore clearing
;;   only matching state.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../../util/event/event.rkt" event)
         (only-in "../../util/event/event-bus.rkt" make-event-bus subscribe! publish!)
         "../../tui/state-types.rkt"
         "../../tui/state-events.rkt"
         (only-in "../../tui/commands.rkt"
                  cmd-ctx
                  execute-extension-command
                  cmd-ctx-state-box
                  cmd-ctx-needs-redraw-box
                  cmd-ctx-session-factory-runner)
         (only-in "../../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../../extensions/gsd/verification-job.rkt"
                  make-verification-registry
                  verification-registry-incarnation
                  verification-start!
                  verification-cancel!
                  verification-status
                  verification-close-registry!
                  verification-identity
                  start-result-job-id
                  verification-job-state))

;; ------------------------------------------------------------- harness

(define INC1 'r102-incarnation-1)

(define (ident-event ev payload #:session [sid "S1"] #:turn [turn "t1"])
  (event 1 ev 1000.0 sid turn payload))

(define (started-payload job-id
                         #:incarnation [inc INC1]
                         #:wave [wave 0]
                         #:gen [gen #f]
                         #:vkey [vkey "vkey-A"])
  (hasheq 'wave
          wave
          'command
          "racket tests/x.rkt"
          'timeout-sec
          60
          'log-path
          "/var/tmp/r102.log"
          'attached?
          #f
          'start-ms
          1000.0
          'job-id
          job-id
          'registry-incarnation
          inc
          'verify-identity-key
          vkey
          'session-attachment-generation
          gen))

(define (completed-payload job-id
                           state
                           exit-code
                           #:incarnation [inc INC1]
                           #:wave [wave 0]
                           #:gen [gen #f]
                           #:vkey [vkey "vkey-A"]
                           #:verdict [verdict "reject"])
  (hasheq 'wave
          wave
          'command
          "racket tests/x.rkt"
          'state
          state
          'exit-code
          exit-code
          'approved?
          (equal? verdict "approve")
          'verdict
          verdict
          'reason
          (if (equal? verdict "approve") #f "verify state=failed exit=3")
          'elapsed-sec
          65.0
          'log-path
          "/var/tmp/r102.log"
          'job-id
          job-id
          'registry-incarnation
          inc
          'verify-identity-key
          vkey
          'session-attachment-generation
          gen))

(define (reduce! state-box evt)
  (set-box! state-box (apply-event-to-state (unbox state-box) evt)))

(define (verification-entries state)
  (for/list ([entry (in-list (ui-state-transcript state))]
             #:when (hash-ref (transcript-entry-meta entry) 'verification #f))
    entry))

(define (progress-id state)
  (define p (ui-state-verification-progress state))
  (and (hash? p) (hash-ref p 'identity-key #f)))

(define (reduce!-state sb evt)
  (apply-event-to-state (unbox sb) evt))

;; start-result job id (short local alias)
(define start-job-id start-result-job-id)

;; ------------------------------------------------------------- suite

(define (lifecycle-suite)
  (test-suite "R102 verification foreground lifecycle (consumer)"

    ;; ---------------------------------------------- identity-aware progress
    (test-case "started stores progress with identity+generation; completed clears only the matching identity"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1)))
      (check-equal? (progress-id (unbox sb)) "r102-incarnation-1|vj-1|vkey-A" "identity stored")
      (check-equal? (hash-ref (ui-state-verification-progress (unbox sb)) 'job-id #f) 'vj-1)
      ;; a DIFFERENT job's terminal must not clear the foreground progress
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-2 'failed 3 #:vkey "vkey-B")))
      (check-equal? (progress-id (unbox sb)) "r102-incarnation-1|vj-1|vkey-A" "mismatched terminal keeps progress")
      (check-true (string-contains? (ui-state-status-message (unbox sb)) "W0 verifying")
                  "status still shows the running verification")
      (check-equal? (length (verification-entries (unbox sb)))
                    1
                    "the mismatched terminal is still recorded truthfully")
      ;; the matching terminal clears
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3)))
      (check-false (ui-state-verification-progress (unbox sb)))
      (check-false (ui-state-status-message (unbox sb))))

    (test-case "late A-complete after B-start does not clear B (A-start/B-start/late-A-complete)"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1 #:vkey "vkey-A")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-2 #:vkey "vkey-B")))
      (check-equal? (progress-id (unbox sb)) "r102-incarnation-1|vj-2|vkey-B" "B owns the foreground")
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3 #:vkey "vkey-A")))
      (check-equal? (progress-id (unbox sb))
                    "r102-incarnation-1|vj-2|vkey-B"
                    "late A terminal cannot clear B")
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-2 'failed 3 #:vkey "vkey-B")))
      (check-false (ui-state-verification-progress (unbox sb)) "B's own terminal clears"))

    (test-case "duplicate terminal delivery is deduped to exactly one UI transition"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1)))
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3)))
      (check-equal? (length (verification-entries (unbox sb))) 1)
      ;; duplicate subscription / re-delivery of the SAME terminal
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3)))
      (check-equal? (length (verification-entries (unbox sb)))
                    1
                    "duplicate terminal: exactly-once UI transition"))

    (test-case "late start after terminal does not resurrect the timer"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1)))
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3)))
      (check-false (ui-state-verification-progress (unbox sb)))
      ;; queued/reordered start of the SAME (already terminal) job
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1)))
      (check-false (ui-state-verification-progress (unbox sb))
                   "late start of a terminal job never resurrects progress"))

    (test-case "same-wave new attempt after replacement runs its own lifecycle"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1)))
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3)))
      (check-false (ui-state-verification-progress (unbox sb)))
      ;; replacement attempt: same wave, NEW job id
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-3)))
      (check-equal? (progress-id (unbox sb)) "r102-incarnation-1|vj-3|vkey-A")
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-3 'failed 3)))
      (check-false (ui-state-verification-progress (unbox sb)))
      (check-equal? (length (verification-entries (unbox sb)))
                    2
                    "two attempts produce two separate transitions"))

    (test-case "old-session events (older attachment generation) can neither set nor clear"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'session.started (hasheq 'session-id "S1")))
      ;; foreground moved to a newer generation (session switched away)
      (set-box! sb (set-attachment-generation (unbox sb) 5))
      ;; queued event from the old session (generation 1)
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1 #:gen 1)))
      (check-false (ui-state-verification-progress (unbox sb)) "stale start cannot resurrect")
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3 #:gen 1)))
      (check-equal? (length (verification-entries (unbox sb)))
                    0
                    "stale terminal from an old session is dropped entirely")
      ;; same-generation events still work
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-9 #:gen 5)))
      (check-true (hash? (ui-state-verification-progress (unbox sb))) "current-generation start applies"))

    (test-case "identity-less legacy terminal never clears identified newer progress"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1)))
      ;; legacy identity-less completed (old producer shape)
      (reduce! sb
               (ident-event 'gsd.verification.completed
                            (hasheq 'wave 0 'verdict "reject" 'reason "legacy" 'elapsed-sec 1.0)))
      (check-equal? (progress-id (unbox sb)) "r102-incarnation-1|vj-1|vkey-A" "identified progress survives")
      (check-true (string-contains? (ui-state-status-message (unbox sb)) "W0 verifying")
                  "running status survives the legacy terminal")
      (check-equal? (length (verification-entries (unbox sb)))
                    1
                    "legacy terminal still recorded truthfully")
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3)))
      (check-false (ui-state-verification-progress (unbox sb)) "real terminal clears"))

    (test-case "legacy identity-less events keep their historical behavior without identified progress"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb
               (ident-event 'gsd.verification.started
                            (hasheq 'wave 0 'timeout-sec 60 'log-path "/tmp/l.log" 'start-ms 1000.0)))
      (check-true (hash? (ui-state-verification-progress (unbox sb))) "legacy start stores progress")
      (reduce! sb
               (ident-event 'gsd.verification.completed
                            (hasheq 'wave 0 'verdict "approve" 'elapsed-sec 2.0)))
      (check-false (ui-state-verification-progress (unbox sb)) "legacy terminal clears legacy progress"))

    (test-case "duplicate subscriptions on a REAL bus yield exactly-once transitions"
      (define bus (make-event-bus))
      (define sb (box (initial-ui-state #:session-id "S1")))
      (define (bridge evt) (reduce! sb evt))
      (subscribe! bus bridge)
      (subscribe! bus bridge) ; the duplicate subscription defect shape
      (publish! bus (ident-event 'gsd.verification.started (started-payload 'vj-1)))
      (publish! bus (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3)))
      (check-equal? (length (verification-entries (unbox sb)))
                    1
                    "two subscriptions deliver twice; dedupe keeps one transition")
      (check-false (ui-state-verification-progress (unbox sb))))

    (test-case "A->B->A with queued events: only matching generation/state survives"
      (define sb (box (initial-ui-state #:session-id "S1")))
      ;; wave A session (generation 1) starts a verification
      (set-box! sb (set-attachment-generation (unbox sb) 1))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1 #:gen 1)))
      (check-true (hash? (ui-state-verification-progress (unbox sb))))
      ;; switch to wave B: fresh foreground generation; A's progress is cleared
      (set-box! sb
                (set-status-message
                 (set-attachment-generation
                  (set-verification-progress
                   (reduce!-state sb (ident-event 'session.started (hasheq 'session-id "S-B"))) #f)
                  2)
                 #f))
      (check-false (ui-state-verification-progress (unbox sb)) "switch cleared A's progress")
      ;; queued events from A (generation 1) arrive late
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-1 'failed 3 #:gen 1)))
      (check-equal? (length (verification-entries (unbox sb))) 0 "queued A terminal dropped")
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1 #:gen 1)))
      (check-false (ui-state-verification-progress (unbox sb)) "queued A start dropped")
      ;; restore back to A with a fresh generation 3: new events flow again
      (set-box! sb (set-attachment-generation (unbox sb) 3))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-4 #:gen 3)))
      (check-equal? (progress-id (unbox sb)) "r102-incarnation-1|vj-4|vkey-A")
      (reduce! sb (ident-event 'gsd.verification.completed (completed-payload 'vj-4 'failed 3 #:gen 3)))
      (check-false (ui-state-verification-progress (unbox sb))))

    (test-case "session switch clears foreign-session verification state in the reducer"
      (define sb (box (initial-ui-state #:session-id "S1")))
      (reduce! sb (ident-event 'gsd.verification.started (started-payload 'vj-1) #:session "S1"))
      (check-true (hash? (ui-state-verification-progress (unbox sb))))
      (reduce! sb (ident-event 'session.started (hasheq 'session-id "S2") #:session "S2"))
      (check-false (ui-state-verification-progress (unbox sb))
                   "a different session attach clears the foreign verification progress")
      ;; same-session resume keeps the progress (reattach does not drop it)
      (define sb2 (box (initial-ui-state #:session-id "S1")))
      (reduce! sb2 (ident-event 'gsd.verification.started (started-payload 'vj-1) #:session "S1"))
      (reduce! sb2 (ident-event 'session.resumed (hasheq 'session-id "S1") #:session "S1"))
      (check-true (hash? (ui-state-verification-progress (unbox sb2)))
                  "same-session resume keeps the foreground progress"))

    ;; ------------------------------------- campaign stop/error/restore route
    (test-case "campaign stop/error path clears matching foreground verification state (real registry)"
      (define reg (make-verification-registry))
      (define ident
        (verification-identity "camp-r102" "W0" "suite" (path->string (current-directory)) "profile"))
      (define r (verification-start! reg ident "/bin/sh" (list "-c" "( sleep 30 )")))
      (define started-rec (verification-status reg (start-job-id r)))
      (check-equal? (verification-job-state started-rec) 'running)
      (define state0
        (set-verification-progress
         (initial-ui-state #:session-id "S1")
         (hasheq 'wave
                 0
                 'started-ms
                 1000.0
                 'log-path
                 "/var/tmp/r102.log"
                 'identity-key
                 (format "~a|~a|vkey" (verification-registry-incarnation reg) (start-job-id r))
                 'job-id
                 (start-job-id r)
                 'registry-incarnation
                 (verification-registry-incarnation reg))))
      (define cctx
        (cmd-ctx (box state0)
                 (box #t)
                 #f
                 #f
                 (box #f)
                 #f
                 (box #f)
                 #f
                 (box "")
                 (box #f)
                 #f ; no session factory -> synchronous stop/error + restore
                 (box #f)
                 (box #f)))
      (parameterize ([current-gsd-verification-registry reg])
        (execute-extension-command cctx state0 (hasheq 'campaign-token 'r102-token 'text "starting")))
      (define st (unbox (cmd-ctx-state-box cctx)))
      (check-false (ui-state-verification-progress st) "stop/error cleared the cached progress")
      (check-true (> (ui-state-attachment-generation st) 0) "restore carried a fresh generation")
      (check-true (string-contains? (ui-state-status-message st) "running (detached)")
                  (format "still-running job stays attributable with an explicit detached status, got ~v"
                          (ui-state-status-message st)))
      (check-false (hash-ref (or (ui-state-verification-progress st) (hash)) 'started-ms #f)
                   "no cached elapsed clock is restored")
      ;; the registry job itself is untouched: clearing is never cancellation
      (check-equal? (verification-job-state (verification-status reg (start-job-id r))) 'running)
      (verification-cancel! reg (start-job-id r))
      (verification-close-registry! reg))

    (test-case "restore over a terminal job appends the observed registry verdict, never a false completion"
      (define reg (make-verification-registry))
      (define ident
        (verification-identity "camp-r102b" "W0" "suite" (path->string (current-directory)) "profile"))
      (define r (verification-start! reg ident "/bin/sh" (list "-c" "( exit 3 )")))
      (verification-cancel! reg (start-job-id r))
      (define state0
        (set-verification-progress
         (initial-ui-state #:session-id "S1")
         (hasheq 'wave
                 0
                 'started-ms
                 1000.0
                 'identity-key
                 (format "~a|~a|vkey" (verification-registry-incarnation reg) (start-job-id r))
                 'job-id
                 (start-job-id r)
                 'registry-incarnation
                 (verification-registry-incarnation reg))))
      (define cctx
        (cmd-ctx (box state0) (box #t) #f #f (box #f) #f (box #f) #f (box "") (box #f) #f (box #f) (box #f)))
      (parameterize ([current-gsd-verification-registry reg])
        (execute-extension-command cctx state0 (hasheq 'campaign-token 'r102-token 'text "starting")))
      (define st (unbox (cmd-ctx-state-box cctx)))
      (check-false (ui-state-verification-progress st))
      (check-true
       (for/or ([entry (in-list (ui-state-transcript st))])
         (and (hash-ref (transcript-entry-meta entry) 'verification #f)
              (string-contains? (transcript-entry-text entry) "cancelled")))
       "the observed terminal record (cancelled) is reported, not a fabricated completion")
      (verification-close-registry! reg))

    (test-case "restore after registry recreation clears without inventing facts"
      (define old-reg (make-verification-registry))
      (define state0
        (set-verification-progress
         (initial-ui-state #:session-id "S1")
         (hasheq 'wave
                 0
                 'started-ms
                 1000.0
                 'identity-key
                 "old-incarnation|vj-1|vkey"
                 'job-id
                 'vj-1
                 'registry-incarnation
                 (verification-registry-incarnation old-reg))))
      (define cctx
        (cmd-ctx (box state0) (box #t) #f #f (box #f) #f (box #f) #f (box "") (box #f) #f (box #f) (box #f)))
      (parameterize ([current-gsd-verification-registry (make-verification-registry)])
        (execute-extension-command cctx state0 (hasheq 'campaign-token 'r102-token 'text "starting")))
      (define st (unbox (cmd-ctx-state-box cctx)))
      (check-false (ui-state-verification-progress st))
      (check-false (ui-state-status-message st)
                   "foreign-incarnation jobs are cleared, never guessed")
      (check-true (> (ui-state-attachment-generation st) 0)))))

;; verification-job / start-result accessors used above are required at the
;; top of this module.

(module+ main
  (exit (run-tests (lifecycle-suite))))
