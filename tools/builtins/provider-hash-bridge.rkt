#lang racket/base

;; tools/builtins/provider-hash-bridge.rkt — message→provider hash conversion
;; STABILITY: internal
;;
;; M10 (v0.97.15): Extracted from spawn-subagent.rkt for reuse.
;; Delegates to the neutral transport owner; no tool layer may serialize its
(require (only-in "../../util/message/provider-transport.rkt"
                  [messages->provider-hashes canonical-messages->provider-hashes]))

(provide (rename-out [canonical-messages->provider-hashes messages->provider-hashes]))
