#lang racket/base

;; scripts/tmux-explore/drivers.rkt — Multi-step scenario drivers
;;
;; v0.99.52 W6: Concrete production-shaped multi-step drivers for all
;; nine required scenarios. Each driver specifies the exact prompt
;; sequence, expected phases, tool/approval expectations, and
;; step-baseline tracking for step-local verification.

(require racket/list
         racket/string
         racket/hash
         "../tmux-tui-explore.rkt"
         "verifiers.rkt"
         "evidence.rkt")

(provide (struct-out multi-step-driver)
         (struct-out step-baseline)
         driver-registry
         find-driver
         run-driver
         make-step-baseline
         advance-baseline
         verify-driver)

(struct step-baseline (step-index event-count) #:transparent)
(struct multi-step-driver
        (tag title
             steps
             prompt-generator
             expected-final-phases
             tool-allowlist
             approval-allowlist
             step-validator)
  #:transparent)

;; Step prompt generator: takes current step index and returns prompt
(define (make-prompt-generator prompts)
  (lambda (step-index)
    (if (and (>= step-index 0) (< step-index (length prompts)))
        (list-ref prompts step-index)
        "")))

;; Driver verifier wrapper: extracts completion event from observation
(define (make-driver-verifier verify-fn)
  (lambda (obs baseline)
    (define events (hash-ref obs 'trace-events '()))
    (define completion (find-completion-event events))
    (if completion
        (verify-fn events completion)
        (verification-result #f 'fail (list "no completion event in trace") '()))))

;; Find completion event in trace (most recent turn.completed or session.compact.completed)
(define (find-completion-event events)
  (define compacts (filter (lambda (e) (phase=? e "session.compact.completed")) (reverse events)))
  (define turns (filter completion-event? (reverse events)))
  (if (not (null? compacts))
      (car compacts)
      (car turns)))

;; Driver registry — exactly nine scenarios
(define driver-registry
  (list
   ;; memory: single turn, store and retrieve nonce
   (multi-step-driver
    "memory"
    "Memory/context"
    1
    (make-prompt-generator '("Remember that the codename is blue-harbor, then identify its source."))
    '("turn.started" "memory.item.stored" "turn.completed")
    '()
    '()
    (make-driver-verifier verify-memory))
   ;; gsd: two turns — plan then transition
   (multi-step-driver
    "gsd"
    "GSD planning"
    2
    (make-prompt-generator '("Create W0/W1 plan with explicit gates."
                             "Transition W0 to accepted with evidence digest d1."))
    '("turn.started" "gsd.transition.attempted" "gsd.transition.succeeded" "turn.completed")
    '()
    '()
    (make-driver-verifier verify-gsd))
   ;; mas: single turn with subagent spawn
   (multi-step-driver
    "mas"
    "MAS/subagents"
    1
    (make-prompt-generator
     '("Use a subagent to verify README.md contains README_FIXTURE_ALPHA, then report result."))
    '("turn.started" "mas.spawn-approval-requested"
                     "mas.spawn-approval-terminal"
                     "tool.execution.correlated-completed"
                     "turn.completed")
    '("read")
    '("spawn-subagent")
    (make-driver-verifier verify-mas))
   ;; tools: single turn, read-only
   (multi-step-driver
    "tools"
    "Tools/approval"
    1
    (make-prompt-generator
     '("Inspect README.md only and report whether README_FIXTURE_ALPHA is present."))
    '("turn.started" "tool.execution.started" "tool.execution.correlated-completed" "turn.completed")
    '("read")
    '()
    (make-driver-verifier (lambda (events completion) (verify-tools events completion "read"))))
   ;; release-audit: single turn, refusal
   (multi-step-driver
    "release-audit"
    "Release/audit truth"
    1
    (make-prompt-generator
     '("Decide whether release is authorized when no live CI evidence is available."))
    '("turn.started" "turn.completed")
    '()
    '()
    (lambda (obs baseline)
      (define events (hash-ref obs 'trace-events '()))
      (define capture (hash-ref obs 'capture ""))
      (define completion (find-completion-event events))
      (if completion
          (verify-release-audit capture events completion)
          (verification-result #f 'fail (list "no completion event in trace") '()))))
   ;; durable-memory: two turns — store then resume/retrieve
   (multi-step-driver
    "durable-memory"
    "Durable memory restart"
    2
    (make-prompt-generator '("Persist the codename amber-quay."
                             "Resume this session and retrieve the codename without restating it."))
    '("turn.started" "memory.item.stored"
                     "turn.completed"
                     "session.started"
                     "memory.retrieval.performed"
                     "turn.completed")
    '()
    '()
    (make-driver-verifier verify-durable-memory))
   ;; resume: single turn, exact session resume
   (multi-step-driver
    "resume"
    "Session resume"
    1
    (make-prompt-generator '("Resume this session and report the prior session identifier."))
    '("session.started" "session.resumed" "turn.completed")
    '()
    '()
    (make-driver-verifier verify-resume))
   ;; compact: single turn, control command
   (multi-step-driver "compact"
                      "Durable compaction"
                      1
                      (make-prompt-generator '("/compact"))
                      '("session.compact.requested" "session.compact.started"
                                                    "session.compact.completed")
                      '()
                      '()
                      (make-driver-verifier verify-compact))
   ;; interrupt: two turns — interrupt then recover
   (multi-step-driver "interrupt"
                      "Functional interruption"
                      2
                      (make-prompt-generator
                       '("Produce a detailed multi-section explanation of cooperative cancellation."
                         "Reply exactly INTERRUPT_RECOVERY_OK."))
                      '("turn.started" "interrupt.requested"
                                       "interrupt.accepted"
                                       "turn.cancelled"
                                       "turn.started"
                                       "turn.completed")
                      '()
                      '()
                      (make-driver-verifier verify-interrupt))))

(define (find-driver tag)
  (findf (lambda (d) (equal? (multi-step-driver-tag d) tag)) driver-registry))

;; Create initial baseline for step 0
(define (make-step-baseline)
  (step-baseline 0 0))

;; Advance baseline to next step
(define (advance-baseline baseline events step-count)
  (step-baseline (+ (step-baseline-step-index baseline) 1)
                 (+ (step-baseline-event-count baseline) step-count)))

;; Run a driver verifier on observation
(define (verify-driver driver obs baseline)
  ((multi-step-driver-step-validator driver) obs baseline))

;; Run a multi-step driver (for future use)
(define (run-driver tag root executor)
  (define driver (find-driver tag))
  (when (not driver)
    (error 'run-driver "no driver for tag: ~a" tag))
  (define steps (multi-step-driver-steps driver))
  (define baseline (make-step-baseline))
  (define results '())
  (for ([i (in-range steps)])
    (define prompt ((multi-step-driver-prompt-generator driver) i))
    (define obs (executor driver root #:step-index i #:baseline baseline))
    (define step-count
      (- (length (hash-ref obs 'trace-events '())) (step-baseline-event-count baseline)))
    (set! baseline (advance-baseline baseline obs step-count))
    (set! results (cons (list 'step i 'prompt prompt 'observation obs) results)))
  (reverse results))
