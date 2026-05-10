#lang racket/base

;; tests/test-session-mutation.rkt — Session mutation guard tests (W-04)

(require rackunit
         "../runtime/session-types.rkt"
         "../runtime/session-mutation.rkt"
         "../util/ids.rkt"
         "../agent/queue.rkt")

(define (make-test-session)
  (agent-session
   (generate-id)    ; session-id
   #f               ; session-dir
   #f               ; provider
   #f               ; tool-registry
   #f               ; event-bus
   #f               ; extension-registry
   #f               ; model-name
   '()              ; system-instructions
   #f               ; index
   #f               ; queue
   (make-hash)      ; config
   #t               ; active?
   (current-seconds) ; start-time
   #f               ; compacting?
   #f               ; last-compaction-time
   #f               ; persisted?
   '()              ; pending-entries
   'medium          ; thinking-level
   #f               ; shutdown-requested?
   #f               ; force-shutdown?
   #f))             ; prompt-running?

;; W-04: prompt-running? transition guards

(test-case "guarded-set-prompt-running!: #f->#t succeeds"
  (define sess (make-test-session))
  (check-false (agent-session-prompt-running? sess))
  (guarded-set-prompt-running! sess #t)
  (check-true (agent-session-prompt-running? sess)))

(test-case "guarded-set-prompt-running!: #t->#f succeeds"
  (define sess (make-test-session))
  (guarded-set-prompt-running! sess #t)
  (guarded-set-prompt-running! sess #f)
  (check-false (agent-session-prompt-running? sess)))

(test-case "guarded-set-prompt-running!: #t->#t raises error"
  (define sess (make-test-session))
  (guarded-set-prompt-running! sess #t)
  (check-exn exn:fail?
             (lambda () (guarded-set-prompt-running! sess #t))))

(test-case "guarded-set-compacting!: #f->#t succeeds"
  (define sess (make-test-session))
  (check-false (agent-session-compacting? sess))
  (guarded-set-compacting! sess #t)
  (check-true (agent-session-compacting? sess)))

(test-case "guarded-set-compacting!: #t->#t raises error"
  (define sess (make-test-session))
  (guarded-set-compacting! sess #t)
  (check-exn exn:fail?
             (lambda () (guarded-set-compacting! sess #t))))

;; valid-session-phase? predicate

(test-case "valid-session-phase? accepts known phases"
  (check-not-false (valid-session-phase? 'idle))
  (check-not-false (valid-session-phase? 'running))
  (check-not-false (valid-session-phase? 'compacting))
  (check-not-false (valid-session-phase? 'shutting-down)))

(test-case "valid-session-phase? rejects unknown phases"
  (check-false (valid-session-phase? 'unknown))
  (check-false (valid-session-phase? 'foo)))
