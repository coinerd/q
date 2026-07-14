#lang racket/base

;; runtime/session/session-interruption.rkt — per-turn cancellation ownership
;;
;; A cancellation token is one-way. This module binds one token to one active
;; prompt turn, correlates at most one accepted interrupt request, and rotates
;; a cancelled token before the next prompt.

(require racket/contract
         racket/dict
         "session-types.rkt"
         "session-mutation.rkt"
         "../../util/cancellation.rkt"
         "../../util/ids.rkt")

(struct interruption-state (turn-id token request-id) #:transparent)

(define interruption-lock (make-semaphore 1))
(define interruption-states (make-weak-hasheq))

(define (session-token sess)
  (define token (dict-ref (agent-session-config sess) 'cancellation-token #f))
  (if (cancellation-token? token)
      token
      (let ([fresh (make-cancellation-token)])
        (guarded-set-config! sess (dict-set (agent-session-config sess) 'cancellation-token fresh))
        fresh)))

(define (begin-session-turn! sess)
  (define turn-id (generate-id))
  (define state (interruption-state turn-id (session-token sess) #f))
  (call-with-semaphore interruption-lock (lambda () (hash-set! interruption-states sess state)))
  turn-id)

(define (active-session-turn-id sess)
  (call-with-semaphore interruption-lock
                       (lambda ()
                         (define state (hash-ref interruption-states sess #f))
                         (and state (interruption-state-turn-id state)))))

(define (request-session-interrupt! sess target-session-id target-turn-id request-id)
  (call-with-semaphore
   interruption-lock
   (lambda ()
     (define state (hash-ref interruption-states sess #f))
     (cond
       [(not (equal? target-session-id (agent-session-session-id sess))) 'unrelated]
       [(or (not (agent-session-prompt-running? sess)) (not state)) 'no-active]
       [(not (equal? target-turn-id (interruption-state-turn-id state))) 'stale]
       [(interruption-state-request-id state) 'already-requested]
       [else
        ;; Record acceptance atomically. Signalling is separate so observers
        ;; always see interrupt.accepted before cancellation can finish.
        (hash-set! interruption-states
                   sess
                   (struct-copy interruption-state state [request-id request-id]))
        'accepted]))))

(define (signal-session-interrupt! sess request-id)
  (define token
    (call-with-semaphore interruption-lock
                         (lambda ()
                           (define state (hash-ref interruption-states sess #f))
                           (and state
                                (equal? request-id (interruption-state-request-id state))
                                (interruption-state-token state)))))
  (when token
    (cancel-token! token))
  (and token #t))

(define (finish-session-turn! sess)
  (define state
    (call-with-semaphore interruption-lock
                         (lambda ()
                           (begin0 (hash-ref interruption-states sess #f)
                             (hash-remove! interruption-states sess)))))
  (cond
    [state
     (when (or (interruption-state-request-id state)
               (cancellation-token-cancelled? (interruption-state-token state)))
       (guarded-set-config!
        sess
        (dict-set (agent-session-config sess) 'cancellation-token (make-cancellation-token))))
     (values (interruption-state-turn-id state) (interruption-state-request-id state))]
    [else (values #f #f)]))

(provide (contract-out [begin-session-turn! (-> agent-session? string?)]
                       [active-session-turn-id (-> agent-session? (or/c string? #f))]
                       [request-session-interrupt!
                        (-> agent-session?
                            string?
                            string?
                            string?
                            (or/c 'accepted 'unrelated 'no-active 'stale 'already-requested))]
                       [signal-session-interrupt! (-> agent-session? string? boolean?)]
                       [finish-session-turn!
                        (-> agent-session? (values (or/c string? #f) (or/c string? #f)))]))
