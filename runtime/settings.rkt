#lang racket/base
;; STABILITY: public

;; runtime/settings.rkt — Settings facade (F6a+F6b extraction)
;;
;; Re-exports settings-core (struct, loading, merging) and
;; settings-query (accessor functions) as a single public API.
;; All consumers import from this module — no behavioral change.
;;
;; CONSUMERS: auth-store, model-registry, providers, session-config, etc.

(require "settings-core.rkt"
         "settings-query.rkt"
         (only-in "../util/sandbox-config.rkt"
                  sandbox-enabled?
                  sandbox-timeout
                  sandbox-memory-limit
                  sandbox-max-output
                  sandbox-max-processes))

;; Re-export everything from both sub-modules
(provide (all-from-out "settings-core.rkt")
         (all-from-out "settings-query.rkt")
         sandbox-enabled?
         sandbox-timeout
         sandbox-memory-limit
         sandbox-max-output
         sandbox-max-processes)
