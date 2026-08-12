#lang racket/base

;; runtime/session/session-prompt-scope.rkt — session-owned prompt parameter scope
;; STABILITY: internal

(require racket/contract
         (only-in "session-types.rkt"
                  agent-session?
                  agent-session-lifecycle
                  lifecycle-state-rollback-st
                  set-lifecycle-state-rollback-st!)
         (only-in "session-mutation.rkt" current-prompt-operation-session)
         (only-in "../context-assembly/rollback-actions.rkt"
                  current-rollback-state
                  make-default-rollback-state))

(provide (contract-out [call-with-session-prompt-scope (-> agent-session? (-> any) any)]))

(define (call-with-session-prompt-scope sess thunk)
  (define lifecycle (agent-session-lifecycle sess))
  (parameterize ([current-prompt-operation-session sess]
                 [current-rollback-state (or (lifecycle-state-rollback-st lifecycle)
                                             (make-default-rollback-state))])
    (dynamic-wind void
                  thunk
                  (lambda () (set-lifecycle-state-rollback-st! lifecycle (current-rollback-state))))))
