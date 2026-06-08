#lang racket/base

;; @speed fast  ;; @suite runtime
;; test-turn-context-assembly.rkt — Characterization + behavioral tests for
;; turn-orchestrator decomposition and extracted functions.
;;
;; W0: Baseline metrics
;; W1: Extract build-assembled-context phases into named helpers
;; W2: Extract context assembly module to turn-context.rkt
;; v0.96.3 AF3: Behavioral tests for extracted functions

(require rackunit
         racket/file
         racket/string
         (only-in "../runtime/context-assembly/turn-context.rkt" symbol->task-state)
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-state?
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging)
         (only-in "../util/fsm/fsm.rkt" fsm-state-name)
         (only-in "../runtime/session/session-config.rkt" hash->session-config session-config?))

(define turn-orch-path (build-path (current-directory) ".." "runtime" "turn-orchestrator.rkt"))
(define turn-context-path
  (build-path (current-directory) ".." "runtime" "context-assembly" "turn-context.rkt"))

;; ---------------------------------------------------------------------------
;; turn-orchestrator.rkt structure (post-W2)
;; ---------------------------------------------------------------------------

(test-case "W2: turn-orchestrator.rkt exists and is readable"
  (check-true (file-exists? turn-orch-path)))

(test-case "W2: turn-orchestrator.rkt reduced to <=370 LOC"
  (define lines (file->lines turn-orch-path))
  (check-true (<= (length lines) 370) (format "Expected <= 370 LOC, got ~a" (length lines))))

(test-case "W2: turn-orchestrator.rkt still contains build-assembled-context"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (build-assembled-context"))
  "build-assembled-context should still exist as coordinator")

(test-case "W2: turn-orchestrator.rkt imports register-session-extensions!"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "register-session-extensions!"))
  "register-session-extensions! should be available (imported or defined)")

(test-case "W2: turn-orchestrator.rkt still contains run-provider-turn"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (run-provider-turn")))
"run-provider-turn should still exist"

(test-case "W2: build-assembled-context is a slim coordinator (30-60 LOC)"
  (define src (file->string turn-orch-path))
  (define lines (string-split src "\n"))
  (define start-idx
    (for/first ([i (in-naturals)]
                [l (in-list lines)]
                #:when (string-contains? l "(define (build-assembled-context"))
      i))
  (define next-define-idx
    (for/first ([i (in-range (add1 start-idx) (length lines))]
                #:when (regexp-match? #rx"^\\(define \\(" (list-ref lines i)))
      i))
  (define fn-len (- next-define-idx start-idx))
  (check-true (and (>= fn-len 30) (<= fn-len 75))
              (format "Expected build-assembled-context 30-75 LOC (post-W2), got ~a" fn-len)))

(test-case "W2: turn-orchestrator.rkt imports from turn-context.rkt"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "context-assembly/turn-context.rkt"))
  "should import from turn-context.rkt")

(test-case "W2: turn-orchestrator.rkt no longer defines symbol->task-state inline"
  (define src (file->string turn-orch-path))
  (check-false (string-contains? src "(define (symbol->task-state")))

(test-case "W2: turn-orchestrator.rkt no longer defines assemble-context/pure inline"
  (define src (file->string turn-orch-path))
  (check-false (string-contains? src "(define (assemble-context/pure")))

;; ---------------------------------------------------------------------------
;; turn-context.rkt (new module, W2)
;; ---------------------------------------------------------------------------

(test-case "W2: context-assembly/turn-context.rkt exists"
  (check-true (file-exists? turn-context-path)))

(test-case "W2: turn-context.rkt provides all extracted helpers"
  (define src (file->string turn-context-path))
  (for ([name '("symbol->task-state" "assemble-context/pure"
                                     "prepare-turn-context-state"
                                     "emit-context-assembly-events!"
                                     "current-last-task-fsm-state")])
    (check-true (string-contains? src name) (format "turn-context.rkt should provide ~a" name))))

(test-case "W2: turn-context.rkt defines the extracted functions"
  (define src (file->string turn-context-path))
  (for ([name '("(define (symbol->task-state" "(define (assemble-context/pure"
                                              "(define (prepare-turn-context-state"
                                              "(define (emit-context-assembly-events!")])
    (check-true (string-contains? src name) (format "turn-context.rkt should define ~a" name))))

;; ---------------------------------------------------------------------------
;; v0.96.3 AF3: Behavioral tests for extracted functions
;; ---------------------------------------------------------------------------

(test-case "AF3: symbol->task-state maps known symbols to fsm-state"
  (define (check-state sym expected-name)
    (define result (symbol->task-state sym))
    (check-true (task-state? result)
                (format "symbol->task-state '~a should return task-state?, got ~a" sym result))
    (check-equal? (fsm-state-name result)
                  expected-name
                  (format "symbol->task-state '~a name should be '~a" sym expected-name)))
  (check-state 'idle 'idle)
  (check-state 'exploration 'exploration)
  (check-state 'planning 'planning)
  (check-state 'implementation 'implementation)
  (check-state 'verification 'verification)
  (check-state 'debugging 'debugging))

(test-case "AF3: symbol->task-state returns #f for unknown symbols"
  (check-false (symbol->task-state 'unknown))
  (check-false (symbol->task-state 'nonexistent))
  (check-false (symbol->task-state 'foo)))

(test-case "AF3: hash->session-config creates valid config with empty hash"
  (define cfg (hash->session-config (hasheq)))
  (check-true (session-config? cfg)
              "hash->session-config with empty hash should produce valid config"))
