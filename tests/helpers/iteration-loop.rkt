#lang racket/base

;; tests/helpers/iteration-loop.rkt — Test convenience wrapper for
;; run-iteration-loop that supplies concrete runtime implementations.
;;
;; v0.99.85: The deprecated v1 wrapper was removed from
;; agent/iteration/main-loop.rkt to eliminate its direct import of
;; runtime/turn-orchestrator.rkt. This module provides the same
;; positional-argument convenience for tests.

(require (prefix-in ml: "../../agent/iteration/main-loop.rkt")
         (only-in "../../agent/iteration/loop-config.rkt" make-loop-config)
         (only-in "../../runtime/turn-orchestrator.rkt" run-provider-turn build-assembled-context)
         (only-in "../../runtime/session/session-config.rkt" hash->session-config)
         (only-in "../../util/loop-result.rkt" loop-result?))

(provide run-iteration-loop)

(define (run-iteration-loop context
                            prov
                            bus
                            reg
                            ext-reg
                            log-path
                            session-id
                            max-iterations
                            #:cancellation-token [token #f]
                            #:config [config-raw (hash->session-config (hash))]
                            #:queue [steering-queue #f]
                            #:injected-box [injected-box #f]
                            #:shutdown-check [shutdown-check #f]
                            #:force-shutdown-check [force-shutdown-check #f]
                            #:working-set [initial-ws #f]
                            #:session [sess #f])
  (ml:run-iteration-loop/v2 (make-loop-config context
                                              prov
                                              bus
                                              reg
                                              ext-reg
                                              log-path
                                              session-id
                                              max-iterations
                                              #:cancellation-token token
                                              #:config config-raw
                                              #:queue steering-queue
                                              #:injected-box injected-box
                                              #:shutdown-check shutdown-check
                                              #:force-shutdown-check force-shutdown-check
                                              #:working-set initial-ws
                                              #:session sess
                                              #:build-context-fn build-assembled-context
                                              #:run-provider-turn-fn run-provider-turn)))
