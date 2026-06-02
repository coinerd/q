#lang racket/base

;; runtime/session-controls.rkt — session control functions
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05c).
;; Model switching, thinking level, and shutdown controls.
;; Uses session-types.rkt for struct accessors to avoid circular deps.

(require racket/contract
         racket/list
         (only-in "../model-registry.rkt" available-models model-entry-name model-registry?)
         "session-types.rkt")
(require "session-mutation.rkt")

(provide (contract-out [set-model! (-> agent-session? string? void?)]
                       [cycle-model! (-> agent-session? model-registry? (or/c string? #f))]
                       ;; Thinking level control (#1153)
                       [thinking-levels (listof symbol?)]
                       [thinking-level? (-> any/c boolean?)]
                       [thinking-level->budget (-> symbol? exact-nonnegative-integer?)]
                       [set-thinking-level! (-> agent-session? symbol? void?)]
                       ;; Graceful shutdown (#1158)
                       [request-shutdown! (-> agent-session? void?)]
                       [force-shutdown! (-> agent-session? void?)]
                       [shutdown-requested? (-> agent-session? boolean?)]
                       [force-shutdown-requested? (-> agent-session? boolean?)]
                       [reset-shutdown-flags! (-> agent-session? void?)]))

;; ============================================================
;; FEAT-65: Runtime model control
;; ============================================================

;; set-model! : agent-session? string? -> void?
;; Sets the active model name for the session.
(define (set-model! sess model-name)
  (unless (string? model-name)
    (raise-argument-error 'set-model! "string?" model-name))
  (guarded-set-model-name! sess model-name))

;; cycle-model! : agent-session? model-registry? -> (or/c string? #f)
;; Cycles to the next model in the registry's available models list.
(define (cycle-model! sess registry)
  (define models (available-models registry))
  (if (null? models)
      #f
      (let* ([current (or (agent-session-model-name sess) "")]
             [names (map model-entry-name models)]
             [unique-names (remove-duplicates names)]
             [current-idx (for/first ([n (in-list unique-names)]
                                      [i (in-naturals)]
                                      #:when (equal? n current))
                            i)]
             [next-idx (if current-idx
                           (modulo (add1 current-idx) (length unique-names))
                           0)]
             [next-model (list-ref unique-names next-idx)])
        (guarded-set-model-name! sess next-model)
        next-model)))

;; ============================================================
;; Thinking level control (#1153)
;; ============================================================

(define thinking-levels '(off minimal low medium high xhigh))

(define (thinking-level? v)
  (and (symbol? v) (member v thinking-levels) #t))

(define (thinking-level->budget level)
  (case level
    [(off) 0]
    [(minimal) 1024]
    [(low) 4096]
    [(medium) 8192]
    [(high) 16384]
    [(xhigh) 32768]
    [else 0]))

(define (set-thinking-level! sess level)
  (unless (thinking-level? level)
    (raise-argument-error 'set-thinking-level! "thinking level" level))
  (guarded-set-thinking-level! sess level))

;; ============================================================
;; Graceful shutdown (#1158)
;; ============================================================

(define (request-shutdown! sess)
  (guarded-set-shutdown-requested! sess #t))

(define (force-shutdown! sess)
  (guarded-set-force-shutdown! sess #t))

(define (shutdown-requested? sess)
  (agent-session-shutdown-requested? sess))

(define (force-shutdown-requested? sess)
  (agent-session-force-shutdown? sess))

(define (reset-shutdown-flags! sess)
  (guarded-set-shutdown-requested! sess #f)
  (guarded-set-force-shutdown! sess #f))
