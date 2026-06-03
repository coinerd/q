#lang racket/base

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/helpers/goal-scenarios.rkt — Goal loop scenario factories
;;
;; Provides mock goal-run! factories and event capture for testing
;; autonomous goal loop behavior without real providers.

(require racket/list
         racket/match
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         "../../runtime/goal/goal-types.rkt"
         "../../util/loop-result.rkt"
         "provider-scenarios.rkt")

(provide make-goal-capture
         goal-capture-events
         goal-capture-statuses
         goal-capture-state
         goal-capture-reset!
         make-on-event
         make-on-status
         
         make-fake-run-prompt
         make-fake-run-prompt-with-error
         make-fake-run-prompt-timeout
         
         make-goal-provider
         make-goal-provider-no-progress
         make-goal-provider-tool-timeout
         make-goal-provider-per-turn-cap
         
         make-fake-shutdown-check
         make-immediate-shutdown)

;; ---------------------------------------------------------------------------
;; Event capture
;; ---------------------------------------------------------------------------

(struct goal-capture (events-box statuses-box state-box) #:transparent)

(define (make-goal-capture)
  (goal-capture (box '()) (box '()) (box #f)))

(define (goal-capture-events cap)
  (reverse (unbox (goal-capture-events-box cap))))

(define (goal-capture-statuses cap)
  (reverse (unbox (goal-capture-statuses-box cap))))

(define (goal-capture-state cap)
  (unbox (goal-capture-state-box cap)))

(define (goal-capture-reset! cap)
  (set-box! (goal-capture-events-box cap) '())
  (set-box! (goal-capture-statuses-box cap) '())
  (set-box! (goal-capture-state-box cap) #f))

(define ((make-on-event cap) evt-type payload)
  (set-box! (goal-capture-events-box cap)
            (cons (list evt-type payload) (unbox (goal-capture-events-box cap)))))

(define ((make-on-status cap) msg)
  (set-box! (goal-capture-statuses-box cap)
            (cons msg (unbox (goal-capture-statuses-box cap)))))

;; ---------------------------------------------------------------------------
;; Fake run-prompt! factories
;; ---------------------------------------------------------------------------

(define ((make-fake-run-prompt responses) sess msg)
  ;; Pop the next response, cycling if needed
  (define idx (modulo (hash-ref (make-hash) 'count 0) (length responses)))
  (define resp (list-ref responses idx))
  ;; Increment via mutation
  (define call-count-box (hash-ref (make-hash) 'box (lambda () 
    (define b (box 0))
    (hash-set! (make-hash) 'box b)
    b)))
  (set-box! call-count-box (add1 (unbox call-count-box)))
  (values sess resp))

(define (make-fake-run-prompt-with-error error-msg)
  (lambda (sess msg)
    (raise (exn:fail error-msg (current-continuation-marks)))))

(define (make-fake-run-prompt-timeout)
  (lambda (sess msg)
    ;; Simulate timeout by returning a stop response
    (values sess (make-loop-result '() 'stop (hash)))))

;; ---------------------------------------------------------------------------
;; Provider factories for goal scenarios
;; ---------------------------------------------------------------------------

(define (make-goal-provider #:responses [responses #f])
  "Create a provider that returns a sequence of text + then 'achieved' evaluation."
  (define prov-responses
    (or responses
        (list
         ;; First call: text response
         (scenario-text "Working on the goal...")
         ;; Second call: text + achieved evaluation
         (scenario-text "Goal achieved!"))))
  (define-values (prov _cap) (make-scenario-provider prov-responses))
  prov)

(define (make-goal-provider-no-progress #:turns [turns 3])
  "Provider that returns the same unhelpful text every turn."
  (define resp (scenario-text "I don't know what to do."))
  (define-values (prov _cap) (make-scenario-provider (make-list turns resp)))
  prov)

(define (make-goal-provider-tool-timeout)
  "Provider that returns a tool call, then times out."
  (define-values (prov _cap)
    (make-scenario-provider
     (list
      (scenario-tool-call "bash")
      (scenario-text "Timed out"))))
  prov)

;; ---------------------------------------------------------------------------
;; Shutdown helpers
;; ---------------------------------------------------------------------------

(define (make-goal-provider-per-turn-cap #:iterations [max-iter 5])
  "Provider that returns incremental progress but never 'achieved'.
   Forces the goal runner to respect max-iterations cap."
  (define resp (scenario-text "Working on it... not done yet."))
  (define-values (prov _cap) (make-scenario-provider (make-list max-iter resp)))
  prov)

(define (make-fake-shutdown-check)
  (let ([called (box 0)])
    (values (lambda () #f)
            (lambda () (unbox called))
            (lambda () (set-box! called (add1 (unbox called)))))))

(define (make-immediate-shutdown)
  (lambda () #t))
