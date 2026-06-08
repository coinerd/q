#lang racket/base

;; runtime/config/session-config-params.rkt — Session infrastructure config struct
;; STABILITY: internal
;;
;; v0.96.7 (F6): Grouping session-related parameters into a config struct.

(provide (struct-out session-config-params)
         default-session-config-params)

;; Session infrastructure configuration
(struct session-config-params
        (load-session-log ; (or/c #f procedure?) — session log loader
         append-entry! ; (or/c #f procedure?) — entry appender
         archive-entry-fn ; (or/c #f procedure?) — archiver
         crash-log-dir ; (or/c #f path?) — crash log directory
         task-rollout-rate ; real? — task-state-aware rollout rate [0..1]
         assembly-profile ; symbol? — context assembly profile
         goal-loop-enabled ; boolean? — enable goal loop
         gsd-mode-query ; procedure? — query current GSD mode
         )
  #:transparent)

(define (default-session-config-params)
  (session-config-params #f ; load-session-log
                         #f ; append-entry!
                         #f ; archive-entry-fn
                         #f ; crash-log-dir
                         0.0 ; task-rollout-rate
                         'off ; assembly-profile
                         #t ; goal-loop-enabled
                         (lambda () 'idle))) ; gsd-mode-query
