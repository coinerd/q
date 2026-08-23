#lang racket/base

;; runtime/session/executor-inheritance.rkt — GSD executor model inheritance
;; STABILITY: internal
;;
;; BUG-0018 W3 (R-B3): /go-spawned executor sessions used to be built fresh
;; from the startup rt-config, so an interactive `/model <name>` switch never
;; reached spawned executor sessions. This module carries the coordinator's
;; switched provider/model into the executor's runtime config unless the user
;; made no explicit switch (then behavior is unchanged).

(require racket/dict
         racket/contract
         (only-in "session-config.rkt" session-config? session-config->hash explicit-model-override?))

(provide (contract-out [inherit-coordinator-runtime-config
                        (-> dict? (or/c hash? #f) (or/c string? #f) any/c dict?)]))

;; inherit-coordinator-runtime-config :
;;   rt-config prior-config prior-model-name prior-provider -> rt-config'
;;
;; Returns an updated runtime config for a spawned executor session. When the
;; coordinator session carries an explicit runtime model override
;; ('model-override marker set by set-model!/switch-model!), the executor
;; inherits both the overridden model name and the live provider instance.
;; Without an explicit override, rt-config is returned untouched (v1.00.13
;; semantics preserved).
(define (inherit-coordinator-runtime-config rt-config prior-config prior-model-name prior-provider)
  (define prior-hash
    (cond
      [(not prior-config) #f]
      [(session-config? prior-config) (session-config->hash prior-config)]
      [else prior-config]))
  (if (explicit-model-override? prior-hash)
      (let* ([with-provider (if prior-provider
                                (dict-set rt-config 'provider prior-provider)
                                rt-config)]
             [with-model (if prior-model-name
                             (dict-set with-provider 'model-name prior-model-name)
                             with-provider)])
        with-model)
      rt-config))
