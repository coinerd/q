#lang racket/base

;; agent/turn-reducer.rkt — Pure turn reducer (R1-P2)
;;
;; Extracted from agent/loop.rkt's 6 effectful branch points.
;; ZERO side effects -- no I/O, no mutation, no event emission.
;; All functions contracted via contract-out.
;;
;; Coexists with loop.rkt; integration deferred to v0.42.x.
;; STABILITY: stable

(require racket/contract
         racket/match
         "turn-model.rkt"
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "loop-messages.rkt" classify-hook-result))

;; ============================================================
;; Decision functions
;; ============================================================

;; After turn start -- always build context
(define (decide-after-start ctx)
  (make-decision-build-context ctx))

;; After context built -- always check pre-hook
(define (decide-after-context ctx)
  (make-decision-check-pre-hook ctx))

;; After pre-hook classification
(define (decide-after-pre-hook hook-result)
  (match (classify-hook-result hook-result)
    [(list 'block reason) (make-decision-blocked reason)]
    [_ (make-decision-check-msg-hook hook-result)]))

;; After message-start hook classification
(define (decide-after-msg-hook hook-result)
  (match (classify-hook-result hook-result)
    [(list 'block reason) (make-decision-blocked reason)]
    [_ (make-decision-begin-stream hook-result)]))

;; After stream completion
(define (decide-after-stream sc)
  (if (stream-completion-cancelled? sc)
      (make-decision-cancelled (or (stream-completion-cancel-reason sc) "user"))
      (make-decision-complete sc)))

;; ============================================================
;; Top-level reducer
;; ============================================================

(define (decide-turn-step cmd)
  (match (turn-command-tag cmd)
    ['start (decide-after-start (turn-command-payload cmd))]
    ['hook-result
     (define hsp (turn-command-payload cmd))
     (define stage-sym (hook-stage-payload-stage hsp))
     (define hook-result (hook-stage-payload-result hsp))
     (case stage-sym
       [(pre) (decide-after-pre-hook hook-result)]
       [(msg) (decide-after-msg-hook hook-result)]
       [else (make-decision-blocked "unknown-stage")])]
    ['stream-complete (decide-after-stream (turn-command-payload cmd))]
    ['cancel (make-decision-cancelled (turn-command-payload cmd))]
    [_ (make-decision-blocked "unknown-command")]))

;; ============================================================
;; Provide
;; ============================================================

(provide classify-hook-result
         (contract-out [decide-after-start (-> any/c turn-decision?)]
                       [decide-after-context (-> any/c turn-decision?)]
                       [decide-after-pre-hook (-> any/c turn-decision?)]
                       [decide-after-msg-hook (-> any/c turn-decision?)]
                       [decide-after-stream (-> stream-completion? turn-decision?)]
                       [decide-turn-step (-> turn-command? turn-decision?)]))
