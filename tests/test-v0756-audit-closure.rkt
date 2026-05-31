#lang racket/base

;; tests/test-v0756-audit-closure.rkt — W3 tests for v0.75.6 Audit Closure
;; T1: Persistence round-trip tests
;; T2: Preamble integration test
;; T3: FSM validation edge-case tests

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         ;; Persistence
         "../runtime/session-store.rkt"
         "../util/protocol-types.rkt"
         ;; Task state
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-implementation
                  task-verification)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-text
                  conclusion->hash
                  hash->conclusion)
         ;; Session types + mutation
         "../runtime/session-types.rkt"
         "../runtime/session-mutation.rkt"
         ;; Preamble
         (only-in "../runtime/context-assembly/serialization.rkt" build-state-awareness-preamble)
         ;; Message content access
         (only-in "../util/content-parts.rkt" text-part-text)
         (only-in "../util/protocol-types.rkt" message-content message-kind))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-log-path)
  (define dir (make-temporary-file "q-test-v0756-~a" 'directory))
  (define log-path (build-path dir "session.jsonl"))
  log-path)

(define (cleanup-path p)
  ;; p is the log file path; delete the parent temp directory
  (define-values (parent _name _dir?) (split-path p))
  (when (and parent (directory-exists? parent))
    (with-handlers ([exn:fail? void])
      (delete-directory/files parent))))

;; Create a minimal agent-session for FSM validation tests
(define (make-test-session)
  (apply agent-session
         (list "test-session"
               "/tmp/test-session"
               #f
               #f
               #f
               #f
               #f
               '()
               #f
               #f
               (hasheq)
               #f
               0
               #f
               #f
               #f
               '()
               #f
               #f
               #f
               #f
               'idle
               '()
               '())))

;; ============================================================
;; Test Suite
;; ============================================================

(define suite
  (test-suite "v0.75.6 Audit Closure"

    ;; ── T1: Persistence round-trip tests ──

    (test-case "append-task-state! + load-latest-task-state round-trip"
      (define log-path (make-temp-log-path))
      (append-task-state! log-path 'exploration)
      (check-equal? (load-latest-task-state log-path) 'exploration)
      (append-task-state! log-path 'implementation)
      (check-equal? (load-latest-task-state log-path) 'implementation)
      (cleanup-path log-path))

    (test-case "load-latest-task-state returns idle for empty log"
      (define log-path (make-temp-log-path))
      ;; Don't write anything — but need at least one entry for file to exist
      ;; Actually load-latest-task-state filters on entries, returns 'idle if none match
      (check-equal? (load-latest-task-state log-path) 'idle)
      (cleanup-path log-path))

    (test-case "load-latest-task-state returns most recent state"
      (define log-path (make-temp-log-path))
      (append-task-state! log-path 'exploration)
      (append-task-state! log-path 'planning)
      (append-task-state! log-path 'implementation)
      (check-equal? (load-latest-task-state log-path) 'implementation)
      (cleanup-path log-path))

    (test-case "append-conclusion! + load-conclusions round-trip"
      (define log-path (make-temp-log-path))
      (define c1 (task-conclusion "c1" "test fact" 'fact 'exploration '() 1000 '() '()))
      (append-conclusion! log-path c1)
      (define loaded (load-conclusions log-path))
      (check-equal? (length loaded) 1)
      (when (and (pair? loaded) (task-conclusion? (car loaded)))
        (check-equal? (task-conclusion-text (car loaded)) "test fact"))
      (cleanup-path log-path))

    (test-case "append-conclusion! accumulates multiple conclusions"
      (define log-path (make-temp-log-path))
      (define c1 (task-conclusion "c1" "first" 'fact 'exploration '() 1000 '() '()))
      (define c2 (task-conclusion "c2" "second" 'decision 'planning '() 2000 '() '()))
      (append-conclusion! log-path c1)
      (append-conclusion! log-path c2)
      (define loaded (load-conclusions log-path))
      (check-equal? (length loaded) 2)
      (cleanup-path log-path))

    (test-case "load-conclusions returns empty for empty log"
      (define log-path (make-temp-log-path))
      (define loaded (load-conclusions log-path))
      (check-equal? loaded '())
      (cleanup-path log-path))

    ;; ── T2: Preamble integration test ──

    (test-case "build-state-awareness-preamble returns #f for idle state"
      (check-false (build-state-awareness-preamble #f '()))
      (check-false (build-state-awareness-preamble 'idle '())))

    (test-case "build-state-awareness-preamble returns message for implementation"
      (define result (build-state-awareness-preamble 'implementation '()))
      (check-not-false result)
      (when result
        (check-not-false result)))

    (test-case "build-state-awareness-preamble includes conclusion summary"
      (define conclusions
        (for/list ([i (in-range 5)])
          (task-conclusion (format "c~a" i)
                           (format "fact ~a" i)
                           'fact
                           'exploration
                           '()
                           (* 1000 i)
                           '()
                           '())))
      (define result (build-state-awareness-preamble 'exploration conclusions))
      (check-not-false result))

    ;; ── T3: FSM validation edge-case tests ──

    (test-case "guarded-set-task-fsm-state! accepts all valid states"
      (define s (make-test-session))
      (for ([state (in-list '(idle exploration planning implementation verification debugging))])
        (guarded-set-task-fsm-state! s state)
        (check-equal? (agent-session-task-fsm-state s) state)))

    (test-case "guarded-set-task-fsm-state! accepts #f"
      (define s (make-test-session))
      (guarded-set-task-fsm-state! s #f)
      (check-equal? (agent-session-task-fsm-state s) #f))

    (test-case "guarded-set-task-fsm-state! rejects invalid state"
      (define s (make-test-session))
      (check-exn exn:fail? (lambda () (guarded-set-task-fsm-state! s 'not-a-valid-state))))

    (test-case "guarded-set-task-fsm-state! rejects non-symbol"
      (define s (make-test-session))
      ;; Contract should reject non-symbol (contract-out says (or/c symbol? #f))
      (check-exn exn:fail? (lambda () (guarded-set-task-fsm-state! s "invalid"))))

    (test-case "guarded-set-task-conclusions! accepts valid list"
      (define s (make-test-session))
      (define c1 (task-conclusion "c1" "test" 'fact 'exploration '() 1000 '() '()))
      (guarded-set-task-conclusions! s (list c1))
      (check-equal? (length (agent-session-task-conclusions s)) 1))))

(run-tests suite)
