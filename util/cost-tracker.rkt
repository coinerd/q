#lang racket/base

;; util/cost-tracker.rkt — cost tracking with per-model pricing
;;
;; Tracks input/output tokens and calculates dollar cost using
;; standard model pricing. Thread-safe via a semaphore.

(require racket/match
         racket/string)

(provide (struct-out cost-tracker)
         make-cost-tracker
         calculate-cost
         format-cost
         cost-tracker-update!
         cost-tracker-total
         cost-tracker-reset!
         cost-tracker-input-tokens-total
         cost-tracker-output-tokens-total)

;; ============================================================
;; Struct
;; ============================================================

;; Thread-safe mutable cost accumulator.
;; Uses a semaphore to serialize updates.
(struct cost-tracker
        (input-tokens ; box of exact-nonnegative-integer
         output-tokens ; box of exact-nonnegative-integer
         model-name ; box of (or/c #f string?)
         sem ; semaphore for thread safety
         )
  #:transparent)

;; Constructor
(define (make-cost-tracker [model #f])
  (cost-tracker (box 0) (box 0) (box model) (make-semaphore 1)))

;; ============================================================
;; Pricing table
;; ============================================================

;; Returns (cons input-price output-price) where each price is
;; dollars per 1M tokens.
(define (model-pricing model-name)
  (define m (and model-name (string-downcase model-name)))
  (cond
    [(and m (or (string-contains? m "gpt-4o-mini") (string-contains? m "gpt-4o-mini")))
     (cons 0.15 0.60)]
    [(and m (string-contains? m "gpt-4o")) (cons 2.50 10.0)]
    [(and m (or (string-contains? m "claude-3.5-sonnet") (string-contains? m "claude-3.5-sonnet")))
     (cons 3.0 15.0)]
    ;; Default pricing: $3/1M input, $15/1M output
    [else (cons 3.0 15.0)]))

;; ============================================================
;; Cost calculation
;; ============================================================

;; Calculate dollar cost for given token counts and model.
;; Returns an exact or inexact number.
(define (calculate-cost input-tokens output-tokens model-name)
  (define prices (model-pricing model-name))
  (define input-cost (* (/ (car prices) 1000000.0) input-tokens))
  (define output-cost (* (/ (cdr prices) 1000000.0) output-tokens))
  (+ input-cost output-cost))

;; Format a numeric cost as "$X.XX"
(define (format-cost cost)
  (format "$~a" (real->decimal-string cost 2)))

;; ============================================================
;; Thread-safe mutators
;; ============================================================

;; Add tokens to the tracker. Thread-safe.
(define (cost-tracker-update! tracker input-tokens output-tokens [model-name #f])
  (call-semaphore (cost-tracker-sem tracker)
                  (lambda ()
                    (set-box! (cost-tracker-input-tokens tracker)
                              (+ (unbox (cost-tracker-input-tokens tracker)) input-tokens))
                    (set-box! (cost-tracker-output-tokens tracker)
                              (+ (unbox (cost-tracker-output-tokens tracker)) output-tokens))
                    (when model-name
                      (set-box! (cost-tracker-model-name tracker) model-name)))))

;; Get total cost based on accumulated tokens.
(define (cost-tracker-total tracker)
  (call-semaphore (cost-tracker-sem tracker)
                  (lambda ()
                    (calculate-cost (unbox (cost-tracker-input-tokens tracker))
                                    (unbox (cost-tracker-output-tokens tracker))
                                    (unbox (cost-tracker-model-name tracker))))))

;; Get total input tokens.
(define (cost-tracker-input-tokens-total tracker)
  (unbox (cost-tracker-input-tokens tracker)))

;; Get total output tokens.
(define (cost-tracker-output-tokens-total tracker)
  (unbox (cost-tracker-output-tokens tracker)))

;; Reset tracker to zero.
(define (cost-tracker-reset! tracker)
  (call-semaphore (cost-tracker-sem tracker)
                  (lambda ()
                    (set-box! (cost-tracker-input-tokens tracker) 0)
                    (set-box! (cost-tracker-output-tokens tracker) 0))))

;; Helper: acquire semaphore, call thunk, release.
(define (call-semaphore sem thunk)
  (semaphore-wait sem)
  (begin0 (thunk)
    (semaphore-post sem)))
