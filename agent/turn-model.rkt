#lang racket/base

;; agent/turn-model.rkt — Turn-level model structs (R1-P1)
;;
;; Extracted as pure data layer for turn orchestration.
;; Mirror of runtime/iteration/decision.rkt pattern.
;; All immutable, all transparent, no boxes, no parameters.
;; STABILITY: stable

(require racket/contract)

;; ============================================================
;; Turn context -- 7 immutable fields
;; ============================================================

(struct turn-context
        (session-id iteration messages active-tools model-config budget-ctx extension-registry)
  #:transparent)

;; ============================================================
;; Turn command -- discriminated union (4 tags)
;; ============================================================

(struct turn-command (tag payload) #:transparent)

(define (make-turn-start payload)
  (turn-command 'start payload))
(define (make-turn-hook-result payload)
  (turn-command 'hook-result payload))
(define (make-turn-stream-complete payload)
  (turn-command 'stream-complete payload))
(define (make-turn-cancel payload)
  (turn-command 'cancel payload))

;; Tag predicates
(define (turn-start? cmd)
  (and (turn-command? cmd) (eq? (turn-command-tag cmd) 'start)))
(define (turn-hook-result? cmd)
  (and (turn-command? cmd) (eq? (turn-command-tag cmd) 'hook-result)))
(define (turn-stream-complete? cmd)
  (and (turn-command? cmd) (eq? (turn-command-tag cmd) 'stream-complete)))
(define (turn-cancel? cmd)
  (and (turn-command? cmd) (eq? (turn-command-tag cmd) 'cancel)))

(define turn-command-tag/c (or/c 'start 'hook-result 'stream-complete 'cancel))

;; ============================================================
;; Turn decision -- discriminated union (8 tags)
;; ============================================================

(struct turn-decision (tag payload metadata) #:transparent)

(define (make-decision-emit-start payload)
  (turn-decision 'emit-start payload (hasheq)))
(define (make-decision-build-context payload)
  (turn-decision 'build-context payload (hasheq)))
(define (make-decision-check-pre-hook payload)
  (turn-decision 'check-pre-hook payload (hasheq)))
(define (make-decision-check-msg-hook payload)
  (turn-decision 'check-msg-hook payload (hasheq)))
(define (make-decision-begin-stream payload)
  (turn-decision 'begin-stream payload (hasheq)))
(define (make-decision-blocked reason)
  (turn-decision 'blocked reason (hasheq 'blocked #t)))
(define (make-decision-complete result)
  (turn-decision 'complete result (hasheq 'done #t)))
(define (make-decision-cancelled reason)
  (turn-decision 'cancelled reason (hasheq 'cancelled #t)))

;; Tag predicates
(define (decision-emit-start? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'emit-start)))
(define (decision-build-context? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'build-context)))
(define (decision-check-pre-hook? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'check-pre-hook)))
(define (decision-check-msg-hook? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'check-msg-hook)))
(define (decision-begin-stream? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'begin-stream)))
(define (decision-blocked? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'blocked)))
(define (decision-complete? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'complete)))
(define (decision-cancelled? d)
  (and (turn-decision? d) (eq? (turn-decision-tag d) 'cancelled)))

(define turn-decision-tag/c
  (or/c 'emit-start
        'build-context
        'check-pre-hook
        'check-msg-hook
        'begin-stream
        'blocked
        'complete
        'cancelled))

;; ============================================================
;; Stream completion -- replaces raw hash for decide-after-stream
;; ============================================================

(struct stream-completion (cancelled? cancel-reason text tool-calls usage model all-chunks)
  #:transparent)

(define (make-stream-completion #:cancelled? [cancelled? #f]
                                #:cancel-reason [cancel-reason #f]
                                #:text [text ""]
                                #:tool-calls [tool-calls '()]
                                #:usage [usage (hasheq)]
                                #:model [model ""]
                                #:all-chunks [all-chunks '()])
  (stream-completion cancelled? cancel-reason text tool-calls usage model all-chunks))

;; ============================================================
;; Hook stage payload -- replaces raw hash for decide-turn-step
;; ============================================================

(struct hook-stage-payload (stage result) #:transparent)

;; ============================================================
;; Provide
;; ============================================================

(provide (contract-out (struct turn-context
                               ([session-id string?] [iteration exact-nonnegative-integer?]
                                                     [messages list?]
                                                     [active-tools list?]
                                                     [model-config hash?]
                                                     [budget-ctx hash?]
                                                     [extension-registry any/c])))
         (contract-out (struct turn-command ([tag turn-command-tag/c] [payload any/c])))
         (contract-out (struct turn-decision
                               ([tag turn-decision-tag/c] [payload any/c] [metadata hash?])))
         make-turn-start
         make-turn-hook-result
         make-turn-stream-complete
         make-turn-cancel
         turn-start?
         turn-hook-result?
         turn-stream-complete?
         turn-cancel?
         make-decision-emit-start
         make-decision-build-context
         make-decision-check-pre-hook
         make-decision-check-msg-hook
         make-decision-begin-stream
         make-decision-blocked
         make-decision-complete
         make-decision-cancelled
         decision-emit-start?
         decision-build-context?
         decision-check-pre-hook?
         decision-check-msg-hook?
         decision-begin-stream?
         decision-blocked?
         decision-complete?
         decision-cancelled?
         ;; Stream completion
         (contract-out (struct stream-completion
                               ([cancelled? boolean?] [cancel-reason (or/c string? #f)]
                                                      [text string?]
                                                      [tool-calls list?]
                                                      [usage hash?]
                                                      [model string?]
                                                      [all-chunks list?])))
         make-stream-completion
         ;; Hook stage payload
         (contract-out (struct hook-stage-payload ([stage symbol?] [result any/c]))))
