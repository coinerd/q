#lang racket/base

;; BOUNDARY: integration

;; tests/test-session-mutation.rkt — Session mutation guard tests (W-04)

(require rackunit
         "../runtime/session/session-types.rkt"
         "../runtime/session/session-mutation.rkt"
         "../runtime/context-assembly/ws-evolution.rkt"
         (only-in "../runtime/working-set.rkt" make-working-set ws-entry ws-entry?)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-id)
         "../util/ids.rkt"
         "../agent/queue.rkt")

(define (make-test-session)
  (agent-session (generate-id) ; session-id
                 #f ; session-dir
                 #f ; provider
                 #f ; tool-registry
                 #f ; event-bus
                 #f ; extension-registry
                 #f ; model-name
                 '() ; system-instructions
                 #f ; index
                 #f ; queue
                 (make-hash) ; config
                 #t ; active?
                 (current-seconds) ; start-time
                 #f ; compacting?
                 #f ; last-compaction-time
                 #f ; persisted?
                 '() ; pending-entries
                 'medium ; thinking-level
                 #f ; shutdown-requested?
                 #f ; force-shutdown?
                 #f ; prompt-running?
                 #f ; task-fsm-state
                 '() ; task-conclusions
                 '())) ; recent-tool-calls

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
  (check-exn exn:fail? (lambda () (guarded-set-prompt-running! sess #t))))

(test-case "guarded-set-compacting!: #f->#t succeeds"
  (define sess (make-test-session))
  (check-false (agent-session-compacting? sess))
  (guarded-set-compacting! sess #t)
  (check-true (agent-session-compacting? sess)))

(test-case "guarded-set-compacting!: #t->#t raises error"
  (define sess (make-test-session))
  (guarded-set-compacting! sess #t)
  (check-exn exn:fail? (lambda () (guarded-set-compacting! sess #t))))

;; valid-session-phase? predicate

(test-case "valid-session-phase? accepts known phases"
  (check-not-false (valid-session-phase? 'idle))
  (check-not-false (valid-session-phase? 'running))
  (check-not-false (valid-session-phase? 'compacting))
  (check-not-false (valid-session-phase? 'shutting-down)))

(test-case "valid-session-phase? rejects unknown phases"
  (check-false (valid-session-phase? 'unknown))
  (check-false (valid-session-phase? 'foo)))

;; ── G5 Fix: WS Evolution Conclusion Merge ──

(test-case "guarded-set-working-set-evolved! merges conclusions, does not replace"
  ;; Create session with existing conclusions from multiple phases
  (define sess (make-test-session))
  (guarded-set-task-conclusions!
   sess
   (list (task-conclusion "existing-1" "old fact" 'fact 'exploration '() 1000 '() '())
         (task-conclusion "existing-2" "old decision" 'decision 'implementation '() 2000 '() '())))
  (check-equal? (length (agent-session-task-conclusions sess)) 2)
  ;; Simulate evolution result with new conclusions (the "evicted" set)
  (define evo-res
    (evolution-result
     '() ; kept-entries
     '() ; archived-entries
     (list (task-conclusion "new-1" "injected fact" 'fact 'implementation '() 3000 '() '()))))
  ;; Call the function
  (guarded-set-working-set-evolved! sess evo-res)
  ;; Verify: ALL original conclusions survive + new ones added
  (define final-conclusions (agent-session-task-conclusions sess))
  (check-equal? (length final-conclusions) 3)
  (define final-ids (map task-conclusion-id final-conclusions))
  (check-not-false (member "existing-1" final-ids))
  (check-not-false (member "existing-2" final-ids))
  (check-not-false (member "new-1" final-ids)))

(test-case "guarded-set-working-set-evolved! with empty evolution result preserves conclusions"
  (define sess (make-test-session))
  (guarded-set-task-conclusions!
   sess
   (list (task-conclusion "keep-me" "important" 'fact 'exploration '() 1000 '() '())))
  (define evo-res (evolution-result '() '() '()))
  (guarded-set-working-set-evolved! sess evo-res)
  ;; Original conclusion should survive since nothing to merge
  (check-equal? (length (agent-session-task-conclusions sess)) 1)
  (check-equal? (task-conclusion-id (car (agent-session-task-conclusions sess))) "keep-me"))

;; v0.78.6 C3: G3 auto-distill persistence test
;; Verifies that guarded-set-task-conclusions! persists conclusions correctly,
;; simulating the auto-distill path where new conclusions are appended.
(test-case "auto-distill persistence: new conclusions survive through guarded-set"
  (define sess (make-test-session))
  ;; Start with existing conclusions from exploration phase
  (guarded-set-task-conclusions!
   sess
   (list (task-conclusion "exp-1" "found auth module" 'fact 'exploration '() 1000 '() '())
         (task-conclusion "exp-2" "config in env vars" 'fact 'exploration '() 2000 '() '())))
  (check-equal? (length (agent-session-task-conclusions sess)) 2)
  ;; Simulate auto-distill appending a new distilled conclusion
  (define existing (agent-session-task-conclusions sess))
  (define distilled
    (task-conclusion "dist-1"
                     "auth uses OAuth2 + refresh tokens"
                     'decision
                     'implementation
                     '()
                     3000
                     '()
                     '()))
  (guarded-set-task-conclusions! sess (append existing (list distilled)))
  ;; Verify all 3 conclusions survive
  (define final (agent-session-task-conclusions sess))
  (check-equal? (length final) 3)
  (define final-ids (map task-conclusion-id final))
  (check-not-false (member "exp-1" final-ids) "existing exp-1 should survive")
  (check-not-false (member "exp-2" final-ids) "existing exp-2 should survive")
  (check-not-false (member "dist-1" final-ids) "new distilled conclusion should survive"))

;; v0.79.2 GAP-4: Archive entry callback test
(test-case "guarded-set-working-set-evolved! calls archive callback for archived entries"
  (define sess (make-test-session))
  (define archived-entries (box '()))
  ;; Create an evolution result with archived entries
  (define ws (make-working-set #:max-entries 10 #:max-tokens 1000))
  (define e1 (ws-entry "file1.rkt" "m1" 100 1000))
  (define e2 (ws-entry "file2.rkt" "m2" 200 2000))
  (define result (evolution-result '() (list e1 e2) '()))
  (parameterize ([current-archive-entry-fn
                  (lambda (e) (set-box! archived-entries (cons e (unbox archived-entries))))])
    (guarded-set-working-set-evolved! sess result))
  (check-equal? (length (unbox archived-entries)) 2))
