#lang racket/base

;; tests/helpers/iteration-loop.rkt — Test convenience wrapper for
;; run-iteration-loop that supplies concrete runtime implementations.
;;
;; This is a test-only composition root. It wraps the real Runtime
;; functions in closures that capture a session-config, exactly like
;; the production composition root (session-lifecycle.rkt).
;;
;; v0.99.87: The Agent loop no longer accepts or imports session-config.
;; The test helper binds config into closures for the injected operations.

(require (prefix-in ml: "../../agent/iteration/main-loop.rkt")
         (only-in "../../agent/iteration/loop-config.rkt" make-loop-config)
         (only-in "../../agent/iteration/loop-state.rkt" iteration-snapshot)
         (only-in "../../runtime/turn-orchestrator.rkt" run-provider-turn build-assembled-context)
         (only-in "../../runtime/iteration/step-executor.rkt" interpret-step)
         (only-in "../../runtime/working-set.rkt" make-working-set compute-working-set-budget)
         (only-in "../../runtime/session/session-config.rkt"
                  hash->session-config
                  config-token-budget-threshold
                  config-max-context-tokens
                  resolve-max-iterations-hard)
         (only-in racket/dict dict-set)
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
                            #:config [cfg-raw (hash->session-config (hash))]
                            #:queue [steering-queue #f]
                            #:injected-box [injected-box #f]
                            #:shutdown-check [shutdown-check #f]
                            #:force-shutdown-check [force-shutdown-check #f]
                            #:working-set [initial-ws #f]
                            #:session [sess #f])
  (define cfg
    (if (hash? cfg-raw)
        (hash->session-config cfg-raw)
        cfg-raw))
  (define ctx-budget (or (config-token-budget-threshold cfg) (config-max-context-tokens cfg)))
  (ml:run-iteration-loop/v2
   (make-loop-config
    context
    prov
    bus
    reg
    ext-reg
    log-path
    session-id
    max-iterations
    #:cancellation-token token
    #:max-iterations-hard (resolve-max-iterations-hard cfg max-iterations)
    #:context-budget ctx-budget
    #:queue steering-queue
    #:injected-box injected-box
    #:shutdown-check shutdown-check
    #:force-shutdown-check force-shutdown-check
    #:working-set initial-ws
    #:session sess
    ;; Closures capture cfg — same pattern as production
    #:build-context-fn (lambda (ctx-to-use ws ext-reg-arg bus-arg sid-arg iter #:session sess-arg)
                         (build-assembled-context ctx-to-use
                                                  (dict-set cfg 'working-set ws)
                                                  ext-reg-arg
                                                  bus-arg
                                                  sid-arg
                                                  iter
                                                  #:session sess-arg))
    #:run-provider-turn-fn
    (lambda (ctx-final prov-arg bus-arg reg-arg ext-reg-arg sid-arg tid-arg tok-arg)
      (run-provider-turn ctx-final prov-arg bus-arg reg-arg ext-reg-arg sid-arg tid-arg tok-arg cfg))
    #:interpret-step-fn (lambda (step-res step-result new-msgs infra snapshot)
                          (interpret-step step-res
                                          step-result
                                          new-msgs
                                          infra
                                          (struct-copy iteration-snapshot snapshot [config cfg])))
    ;; v0.99.9x: same injected working-set construction as production
    #:ensure-working-set-fn
    (lambda (ws-arg)
      (or ws-arg (make-working-set #:max-tokens (compute-working-set-budget ctx-budget)))))))
