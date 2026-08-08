#lang racket/base

;; runtime/iteration/decision.rkt — COMPATIBILITY RE-EXPORT
;;
;; v0.99.86: Types moved to util/iteration/decision.rkt.
;; Decision functions moved to agent/iteration/decision.rkt.
;; This file re-exports both for backward compatibility.
;; TODO: Remove once all consumers import from the new locations.

(require (only-in "../../util/iteration/decision.rkt"
                  iteration-ctx
                  iteration-ctx?
                  iteration-ctx-iteration
                  iteration-ctx-consecutive-tool-count
                  iteration-ctx-explore-count
                  iteration-ctx-max-iterations
                  iteration-ctx-max-iterations-hard
                  step-result
                  step-result?
                  step-result-action
                  step-result-termination
                  step-result-new-counters
                  step-result-metadata
                  step-action?
                  known-termination-reasons
                  current-max-consecutive-tool-calls)
         (only-in "../../agent/iteration/decision.rkt" decide-next-action compute-step-result))

(provide iteration-ctx
         iteration-ctx?
         iteration-ctx-iteration
         iteration-ctx-consecutive-tool-count
         iteration-ctx-explore-count
         iteration-ctx-max-iterations
         iteration-ctx-max-iterations-hard
         step-result
         step-result?
         step-result-action
         step-result-termination
         step-result-new-counters
         step-result-metadata
         step-action?
         known-termination-reasons
         current-max-consecutive-tool-calls
         decide-next-action
         compute-step-result)
