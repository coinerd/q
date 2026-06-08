#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: pure

;; BOUNDARY: unit

;; tests/test-fsm-property.rkt -- A9-01/02: FSM property tests

(require rackunit
         rackunit/text-ui
         (only-in "../agent/loop-fsm.rkt"
                  TURN-TRANSITIONS
                  turn-state-emit-start
                  turn-state-build-context
                  turn-state-pre-hook
                  turn-state-stream
                  turn-state-post-hook
                  turn-state-complete
                  turn-state-blocked
                  turn-state? turn-state->symbol
                  turn-event-start
                  turn-event-context-built
                  turn-event-hook-pass
                  turn-event-hook-block
                  turn-event-stream-complete
                  turn-event-stream-cancel
                  turn-event-post-hook-done
                  turn-event? turn-event->symbol
                  next-turn-state
                  valid-turn-transition?)
         (only-in "../runtime/iteration/fsm-types.rkt"
                  TRANSITIONS
                  iteration-state? iteration-event?
                  state-idle state-provider-turn state-tool-exec
                  state-decision state-complete state-retrying state-aborted
                  event-start-loop event-model-response event-tool-result
                  event-tool-calls-present event-termination-reason
                  event-hook-block event-error event-retry-requested event-cancel
                  next-iteration-state valid-transition?
                  state->symbol iteration-event->symbol))

(define fsm-suite
  (test-suite "FSM property tests"

    ;; -- Turn FSM (A9-01) --

    (test-case "all TURN-TRANSITIONS have valid states and events"
      (for ([entry (in-list TURN-TRANSITIONS)])
        (match entry
          [`((,s . ,e) . ,next)
           (check-pred symbol? s "state must be symbol")
           (check-pred symbol? e "event must be symbol")
           (check-pred symbol? next "next must be symbol")])))

    (test-case "turn happy path transitions"
      (define s1 (next-turn-state turn-state-emit-start turn-event-start))
      (check-equal? (turn-state->symbol s1) 'build-context)
      (define s2 (next-turn-state s1 turn-event-context-built))
      (check-equal? (turn-state->symbol s2) 'pre-hook)
      (define s3 (next-turn-state s2 turn-event-hook-pass))
      (check-equal? (turn-state->symbol s3) 'stream)
      (define s4 (next-turn-state s3 turn-event-stream-complete))
      (check-equal? (turn-state->symbol s4) 'post-hook)
      (define s5 (next-turn-state s4 turn-event-post-hook-done))
      (check-equal? (turn-state->symbol s5) 'complete))

    (test-case "turn blocked path"
      (define s1 (next-turn-state turn-state-pre-hook turn-event-hook-block))
      (check-equal? (turn-state->symbol s1) 'blocked))

    (test-case "turn cancelled path"
      (define s1 (next-turn-state turn-state-stream turn-event-stream-cancel))
      (check-equal? (turn-state->symbol s1) 'complete))

    (test-case "turn terminal states stay terminal"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-complete turn-event-start))
                    'complete)
      (check-equal? (turn-state->symbol (next-turn-state turn-state-blocked turn-event-start))
                    'blocked))

    (test-case "turn invalid transitions raise errors"
      (check-exn exn:fail? (lambda () (next-turn-state turn-state-complete turn-event-hook-block)))
      (check-exn exn:fail? (lambda () (next-turn-state turn-state-emit-start turn-event-hook-block))))

    (test-case "valid-turn-transition? matches TURN-TRANSITIONS"
      (check-true (valid-turn-transition? turn-state-emit-start turn-event-start))
      (check-true (valid-turn-transition? turn-state-pre-hook turn-event-hook-pass))
      (check-false (valid-turn-transition? turn-state-complete turn-event-hook-block)))

    ;; -- Iteration FSM (A9-02) --

    (test-case "all TRANSITIONS have valid states and events"
      (for ([entry (in-list TRANSITIONS)])
        (match entry
          [`((,s . ,e) . ,next)
           (check-pred symbol? s "state must be symbol")
           (check-pred symbol? e "event must be symbol")
           (check-pred symbol? next "next must be symbol")])))

    (test-case "iteration happy path with tool calls"
      (define s1 (next-iteration-state state-idle event-start-loop))
      (check-equal? (state->symbol s1) 'provider-turn)
      (define s2 (next-iteration-state s1 event-model-response))
      (check-equal? (state->symbol s2) 'decision)
      (define s3 (next-iteration-state s2 event-tool-calls-present))
      (check-equal? (state->symbol s3) 'tool-exec)
      (define s4 (next-iteration-state s3 event-tool-result))
      (check-equal? (state->symbol s4) 'decision)
      (define s5 (next-iteration-state s4 event-termination-reason))
      (check-equal? (state->symbol s5) 'complete))

    (test-case "iteration happy path without tool calls"
      (define s1 (next-iteration-state state-idle event-start-loop))
      (define s2 (next-iteration-state s1 event-model-response))
      (define s3 (next-iteration-state s2 event-termination-reason))
      (check-equal? (state->symbol s3) 'complete))

    (test-case "iteration error recovery"
      (define s1 (next-iteration-state state-idle event-start-loop))
      (define s2 (next-iteration-state s1 event-error))
      (check-equal? (state->symbol s2) 'retrying)
      (define s3 (next-iteration-state s2 event-retry-requested))
      (check-equal? (state->symbol s3) 'provider-turn))

    (test-case "iteration terminal states"
      (check-equal? (state->symbol (next-iteration-state state-complete event-cancel))
                    'complete)
      (check-equal? (state->symbol (next-iteration-state state-aborted event-cancel))
                    'aborted))

    (test-case "iteration invalid transitions raise errors"
      (check-exn exn:fail? (lambda () (next-iteration-state state-idle event-tool-result)))
      (check-exn exn:fail? (lambda () (next-iteration-state state-tool-exec event-cancel))))

    (test-case "valid-transition? matches TRANSITIONS"
      (check-true (valid-transition? state-idle event-start-loop))
      (check-true (valid-transition? state-decision event-tool-calls-present))
      (check-false (valid-transition? state-idle event-tool-result)))

    (test-case "every iteration state has at least one exit transition"
      (define states '(idle provider-turn decision tool-exec complete retrying aborted))
      (for ([st-sym (in-list states)])
        (define has-exit
          (for/or ([entry (in-list TRANSITIONS)])
            (match entry [`((,s . ,_) . ,_) (eq? s st-sym)] [_ #f])))
        (check-true has-exit (format "state ~a has no exit transition" st-sym))))

    (test-case "every iteration state is reachable from idle"
      (define (successors st)
        (for/list ([entry (in-list TRANSITIONS)]
                   #:when (match entry [`((,s . ,_) . ,_) (eq? s st)] [_ #f]))
          (match entry [`((,_ . ,_) . ,n) n])))
      (define reachable
        (let loop ([visited '()] [queue '(idle)])
          (if (null? queue)
              visited
              (let* ([current (car queue)]
                     [next-queue (cdr queue)])
                (if (member current visited)
                    (loop visited next-queue)
                    (loop (cons current visited)
                          (append next-queue (successors current))))))))
      (for ([st '(idle provider-turn decision tool-exec complete retrying aborted)])
        (check-not-false (member st reachable) (format "state ~a is unreachable from idle" st))))
    ;; -- R13: Generative PBT properties --

    (test-case "turn FSM: determinism -- every (state,event) maps to exactly one next state"
      (define pairs
        (for/list ([e (in-list TURN-TRANSITIONS)])
          (match e [`((,s . ,ev) . ,next) (cons (cons s ev) next)])))
      (define grouped (make-hash))
      (for ([p (in-list pairs)])
        (hash-update! grouped (car p) (lambda (v) (cons (cdr p) v)) (list)))
      (for ([(key targets) (in-hash grouped)])
        (check = (length targets) 1
          (format "non-deterministic transition for key ~a -> ~a" key targets))))

    (test-case "turn FSM: event closure -- all events have source states"
      (define all-events
        (remove-duplicates
          (for/list ([e (in-list TURN-TRANSITIONS)])
            (match e [`((,s . ,ev) . ,next) ev]))))
      (for ([ev (in-list all-events)])
        (define sources
          (filter (lambda (e) (match e [`((,s . ,e2) . ,_) (eq? e2 ev)] [_ #f]))
                  TURN-TRANSITIONS))
        (check > (length sources) 0
          (format "event ~a has no source states" ev))))

    (test-case "turn FSM: all non-terminal states reachable from emit-start"
      (define (next-states-from s)
        (for/list ([e (in-list TURN-TRANSITIONS)]
                   #:when (match e [`((,from . ,_) . ,_) (eq? from s)]))
          (match e [`((,_ . ,_) . ,next) next])))
      (define reachable (mutable-set))
      (set-add! reachable 'emit-start)
      (let loop ([queue '(emit-start)])
        (unless (null? queue)
          (define s (car queue))
          (define new-states
            (for/list ([ns (in-list (next-states-from s))]
                       #:unless (set-member? reachable ns))
              (set-add! reachable ns)
              ns))
          (loop (append new-states (cdr queue)))))
      (for ([expected '(build-context pre-hook stream post-hook)])
        (check set-member? reachable expected
          (format "state ~a not reachable from emit-start" expected))))

    (test-case "iteration FSM: determinism -- every (state,event) maps to exactly one next state"
      (define pairs
        (for/list ([e (in-list TRANSITIONS)])
          (match e [`((,s . ,ev) . ,next) (cons (cons s ev) next)])))
      (define grouped (make-hash))
      (for ([p (in-list pairs)])
        (hash-update! grouped (car p) (lambda (v) (cons (cdr p) v)) (list)))
      (for ([(key targets) (in-hash grouped)])
        (check = (length targets) 1
          (format "non-deterministic transition for key ~a -> ~a" key targets))))

    (test-case "iteration FSM: event closure -- all events have source states"
      (define all-events
        (remove-duplicates
          (for/list ([e (in-list TRANSITIONS)])
            (match e [`((,s . ,ev) . ,next) ev]))))
      (for ([ev (in-list all-events)])
        (define sources
          (filter (lambda (e) (match e [`((,s . ,e2) . ,_) (eq? e2 ev)] [_ #f]))
                  TRANSITIONS))
        (check > (length sources) 0
          (format "event ~a has no source states" ev))))

    (test-case "iteration FSM: all non-terminal states reachable from idle"
      (define (next-states-from s)
        (for/list ([e (in-list TRANSITIONS)]
                   #:when (match e [`((,from . ,_) . ,_) (eq? from s)]))
          (match e [`((,_ . ,_) . ,next) next])))
      (define reachable (mutable-set))
      (set-add! reachable 'idle)
      (let loop ([queue '(idle)])
        (unless (null? queue)
          (define s (car queue))
          (define new-states
            (for/list ([ns (in-list (next-states-from s))]
                       #:unless (set-member? reachable ns))
              (set-add! reachable ns)
              ns))
          (loop (append new-states (cdr queue)))))
      (for ([expected '(provider-turn decision tool-exec retrying)])
        (check set-member? reachable expected
          (format "state ~a not reachable from idle" expected))))))

(run-tests fsm-suite 'verbose)
