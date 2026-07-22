#lang racket/base

;;; tools/scheduler.rkt — Thin coordinator for scheduler subsystem
;;;
;;; Re-exports from scheduler-preflight.rkt and scheduler-execution.rkt.
;;; Owns the coordination structs and the main orchestration functions:
;;;   plan-tool-batch, execute-tool-plan, run-tool-batch
;;;
;;; Behavior (from ARCHITECTURE.md section 6.4):
;;;   1. preserve source order
;;;   2. run preflight hooks serially
;;;   3. revalidate arguments after hook mutation
;;;   4. execute approved calls, optionally in parallel
;;;   5. emit final results in source order

(require racket/contract
         racket/match
         (only-in "tool.rkt"
                  tool-call?
                  tool-call-name
                  tool-call-arguments
                  make-tool-call
                  make-exec-context
                  tool-registry?
                  lookup-tool
                  make-error-result
                  make-success-result
                  tool-result-is-error?
                  exec-context?
                  exec-context-event-publisher)
         (only-in "scheduler-strategy.rkt"
                  scheduler-strategy?
                  scheduler-strategy-preflight-filter
                  scheduler-strategy-execution-order
                  default-scheduler-strategy)
         (only-in "scheduler-preflight.rkt"
                  preflight-entry
                  preflight-entry?
                  preflight-entry-status
                  preflight-entry-tool-call
                  preflight-entry-tool
                  preflight-entry-error-message
                  run-preflight)
         (only-in "scheduler-execution.rkt"
                  tool-pre-hook-payload
                  tool-pre-hook-payload?
                  tool-pre-hook-payload-tool-name
                  tool-pre-hook-payload-args
                  tool-pre-hook-payload-entry-id
                  tool-post-hook-payload
                  tool-post-hook-payload?
                  tool-post-hook-payload-tool-name
                  tool-post-hook-payload-result
                  tool-post-hook-payload-entry-id
                  tool-post-hook-payload-arguments
                  ipc-response->tool-result
                  max-parallel-tools
                  run-execution))

;; ── Re-exports from sub-modules ──
(provide preflight-entry
         preflight-entry?
         preflight-entry-status
         preflight-entry-tool-call
         preflight-entry-tool
         preflight-entry-error-message
         run-preflight
         tool-pre-hook-payload
         tool-pre-hook-payload?
         tool-pre-hook-payload-tool-name
         tool-pre-hook-payload-args
         tool-pre-hook-payload-entry-id
         tool-post-hook-payload
         tool-post-hook-payload?
         tool-post-hook-payload-tool-name
         tool-post-hook-payload-result
         tool-post-hook-payload-entry-id
         tool-post-hook-payload-arguments
         ipc-response->tool-result
         max-parallel-tools)

;; ── Own exports ──
(provide scheduler-result
         scheduler-result?
         scheduler-result-results
         scheduler-result-metadata
         scheduler-problem
         scheduler-problem?
         scheduler-problem-calls
         scheduler-problem-registry
         scheduler-problem-strategy
         scheduler-problem-hook-dispatcher
         scheduler-problem-exec-context
         scheduler-problem-parallel?
         scheduler-plan
         scheduler-plan?
         scheduler-plan-entries
         scheduler-plan-ordered-calls
         scheduler-plan-execution-order
         scheduler-plan-metadata
         scheduler-batch-stats
         scheduler-batch-stats?
         scheduler-batch-stats-total
         scheduler-batch-stats-executed
         scheduler-batch-stats-blocked
         scheduler-batch-stats-errors
         scheduler-batch-stats->hash
         (contract-out [run-tool-batch
                        (->* ((listof tool-call?) tool-registry?)
                             (#:hook-dispatcher (or/c procedure? #f)
                                                #:exec-context (or/c exec-context? #f)
                                                #:parallel? (or/c boolean? #f))
                             scheduler-result?)]
                       [plan-tool-batch (-> scheduler-problem? scheduler-plan?)]
                       [execute-tool-plan
                        (-> scheduler-plan?
                            exec-context?
                            (or/c procedure? #f)
                            boolean?
                            (or/c procedure? #f)
                            scheduler-result?)]))

;; ============================================================
;; Scheduler result struct
;; ============================================================

(struct scheduler-result (results metadata) #:transparent)

;; ============================================================
;; Planning-phase structs for scheduler observability
;; ============================================================

(struct scheduler-problem (calls registry strategy hook-dispatcher exec-context parallel?)
  #:transparent)
(struct scheduler-plan (entries ordered-calls execution-order metadata) #:transparent)

;; ============================================================
;; Batch stats struct
;; ============================================================

(struct scheduler-batch-stats (total executed blocked errors) #:transparent)

;; Convert scheduler-batch-stats to a hash for event emission.
;; Preserves struct for internal use while providing serializable hash
;; for event bus consumers.
(define (scheduler-batch-stats->hash s)
  (hasheq 'total
          (scheduler-batch-stats-total s)
          'executed
          (scheduler-batch-stats-executed s)
          'blocked
          (scheduler-batch-stats-blocked s)
          'errors
          (scheduler-batch-stats-errors s)))

;; ============================================================
;; Compute metadata
;; ============================================================

(define (compute-metadata results preflight-entries)
  (define total (length results))
  (define blocked
    (for/sum ([entry (in-list preflight-entries)])
             (if (eq? (preflight-entry-status entry) 'blocked) 1 0)))
  ;; Count errors that are NOT from blocked entries
  (define blocked-indices
    (for/list ([entry (in-list preflight-entries)]
               [idx (in-naturals)]
               #:when (eq? (preflight-entry-status entry) 'blocked))
      idx))
  (define errors
    (for/sum ([r (in-list results)] [idx (in-naturals)]
                                    #:when (and (tool-result-is-error? r)
                                                (not (member idx blocked-indices))))
             1))
  (define executed (- total blocked errors))
  ;; v0.44.4: Return typed struct directly (was dead-unpacking to hasheq)
  (scheduler-batch-stats total executed blocked errors))

;; ============================================================
;; Main entry point
;; ============================================================

;; v0.44.2 (R3): Pure planning phase — constructs scheduler-plan from scheduler-problem
(define (plan-tool-batch problem)
  (define calls (scheduler-problem-calls problem))
  (define registry (scheduler-problem-registry problem))
  (define hook-dispatcher (scheduler-problem-hook-dispatcher problem))
  (define parallel? (scheduler-problem-parallel? problem))
  (define strat (or (scheduler-problem-strategy problem) (default-scheduler-strategy)))
  (define filtered-calls ((scheduler-strategy-preflight-filter strat) calls))
  (define ordered-calls ((scheduler-strategy-execution-order strat) filtered-calls))
  (define entries (run-preflight ordered-calls registry hook-dispatcher))
  (scheduler-plan entries ordered-calls (if parallel? 'parallel 'serial) #f))

;; v0.44.2 (R3): Effectful execution phase — runs a scheduler-plan
(define (execute-tool-plan plan exec-ctx hook-dispatcher parallel? ev-pub)
  (define entries (scheduler-plan-entries plan))
  (when ev-pub
    (ev-pub "tool.batch.preflight.started"
            (hasheq 'toolCount
                    (length (scheduler-plan-ordered-calls plan))
                    'toolNames
                    (map tool-call-name (scheduler-plan-ordered-calls plan)))))
  (define results (run-execution entries exec-ctx parallel? hook-dispatcher))
  (define metadata (compute-metadata results entries))
  (when ev-pub
    (ev-pub "tool.batch.completed" (scheduler-batch-stats->hash metadata)))
  (scheduler-result results metadata))

;; Main entry point — backward compatible
(define (run-tool-batch tool-calls
                        registry
                        #:hook-dispatcher [hook-dispatcher #f]
                        #:exec-context [exec-ctx (make-exec-context)]
                        #:parallel? [parallel? #f]
                        #:strategy [strategy #f])
  (define problem (scheduler-problem tool-calls registry strategy hook-dispatcher exec-ctx parallel?))
  (define plan (plan-tool-batch problem))
  (define ev-pub (and exec-ctx (exec-context-event-publisher exec-ctx)))
  (execute-tool-plan plan exec-ctx hook-dispatcher parallel? ev-pub))
