#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-event-bus-unification.rkt
;; TDD test for W5: Unify GSD event bus under gsd-session-ctx

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/events.rkt"
                  emit-gsd-event!
                  set-gsd-event-bus!
                  ctx-emit-gsd-event!
                  resolve-gsd-event-bus)
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  gsd-ctx-set-event-bus!
                  gsd-ctx-event-bus))

(define event-bus-unification-suite
  (test-suite "gsd-event-bus-unification"

    (test-case "ctx-emit-gsd-event! emits to ctx-specific bus"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      (define events-a (box '()))
      (define events-b (box '()))
      ;; Set per-ctx buses
      (gsd-ctx-set-event-bus! ctx-a
                              (lambda (name data) (set-box! events-a (cons data (unbox events-a)))))
      (gsd-ctx-set-event-bus! ctx-b
                              (lambda (name data) (set-box! events-b (cons data (unbox events-b)))))
      ;; Emit to ctx-a only
      (ctx-emit-gsd-event! ctx-a 'gsd.mode.changed (hasheq 'mode 'exploring))
      ;; ctx-a received the event, ctx-b did not
      (check-equal? (length (unbox events-a)) 1 "ctx-a received event")
      (check-equal? (length (unbox events-b)) 0 "ctx-b did not receive event"))

    (test-case "ctx-emit-gsd-event! falls back to global when ctx has no bus"
      (define ctx (make-gsd-context))
      (define global-events (box '()))
      ;; Set global bus
      (set-gsd-event-bus! (lambda (name data)
                            (set-box! global-events (cons data (unbox global-events)))))
      ;; ctx has no bus → should fall back to global
      (ctx-emit-gsd-event! ctx 'gsd.mode.changed (hasheq 'mode 'idle))
      (check-equal? (length (unbox global-events)) 1 "global received fallback event")
      ;; Cleanup
      (set-gsd-event-bus! void))

    (test-case "legacy set-gsd-event-bus! still works"
      (define events (box '()))
      (set-gsd-event-bus! (lambda (name data) (set-box! events (cons data (unbox events)))))
      (emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'idle))
      (check-equal? (length (unbox events)) 1 "legacy emit works")
      (set-gsd-event-bus! void))

    (test-case "resolve-gsd-event-bus prefers ctx bus over global"
      (define ctx (make-gsd-context))
      (define ctx-events (box '()))
      (define global-events (box '()))
      (gsd-ctx-set-event-bus! ctx
                              (lambda (name data)
                                (set-box! ctx-events (cons data (unbox ctx-events)))))
      (set-gsd-event-bus! (lambda (name data)
                            (set-box! global-events (cons data (unbox global-events)))))
      ;; Resolve should give ctx's bus
      (define bus (resolve-gsd-event-bus ctx))
      (bus 'test (hasheq))
      (check-equal? (length (unbox ctx-events)) 1 "ctx bus called")
      (check-equal? (length (unbox global-events)) 0 "global bus not called")
      ;; Cleanup
      (set-gsd-event-bus! void))

    (test-case "two independent contexts emit to separate buses"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      (define events-a (box '()))
      (define events-b (box '()))
      (gsd-ctx-set-event-bus!
       ctx-a
       (lambda (name data) (set-box! events-a (cons (hash-ref data 'data #f) (unbox events-a)))))
      (gsd-ctx-set-event-bus!
       ctx-b
       (lambda (name data) (set-box! events-b (cons (hash-ref data 'data #f) (unbox events-b)))))
      (ctx-emit-gsd-event! ctx-a 'gsd.wave.started (hasheq 'wave 0))
      (ctx-emit-gsd-event! ctx-a 'gsd.wave.completed (hasheq 'wave 0))
      (ctx-emit-gsd-event! ctx-b 'gsd.wave.started (hasheq 'wave 1))
      (check-equal? (length (unbox events-a)) 2 "ctx-a got 2 events")
      (check-equal? (length (unbox events-b)) 1 "ctx-b got 1 event")
      (check-equal? (second (unbox events-a)) (hasheq 'wave 0) "ctx-a first event correct"))))

(run-tests event-bus-unification-suite)
