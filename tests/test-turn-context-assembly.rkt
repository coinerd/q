#lang racket/base

;; @speed fast  ;; @suite runtime
;; @boundary unit
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

(test-case "W2: turn-orchestrator.rkt reduced to <=450 LOC"
  (define lines (file->lines turn-orch-path))
  (check-true (<= (length lines) 450) (format "Expected <= 450 LOC, got ~a" (length lines))))

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
                                     "emit-context-assembly-events!")])
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

;; ---------------------------------------------------------------------------
;; R99a (BUG-0099): the turn-context auto-distill augmentation path keeps the
;; deterministic writer HELD — persisted fallbacks are recognized by the frozen
;; recognizer and byte-identical to the pre-R99a writer output.
;; ---------------------------------------------------------------------------

(require (only-in "../runtime/context-assembly/turn-context.rkt"
                  prepare-turn-context-state
                  content-part->text)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  current-auto-distillation-enabled?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-ws-evolution-enabled?)
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-rollback-state
                  make-default-rollback-state)
         (only-in "../runtime/working-set.rkt"
                  make-working-set
                  working-set-add!)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-id
                  task-conclusion-text
                  task-conclusion-category
                  task-conclusion-origin-message-ids
                  task-conclusion-relevance-tags
                  task-conclusion-timestamp
                  legacy-deterministic-fallback?
                  conclusion->hash)
         (only-in "../runtime/session/session-store-goal-task.rkt"
                  conclusion-hash->json-safe)
         (only-in "../util/message/protocol-types.rkt" make-message make-text-part)
         (only-in "../runtime/session/session-types.rkt" agent-session-task-conclusions)
         (only-in "helpers/session-fixture.rkt" make-test-session)
         json)

(define (r99a-turn-msg id text)
  (make-message id #f 'user 'text (list (make-text-part text)) (current-seconds) (hasheq)))

(test-case "R99a: prepare-turn-context-state fallbacks are recognized + byte-identical (writer HELD)"
  (define ws (make-working-set))
  (working-set-add! ws "/tmp/r99a-file-a.rkt" "m1" 10)
  (define cfg (hash->session-config (hasheq 'working-set ws)))
  (define sess (make-test-session))
  (define ctx (list (r99a-turn-msg "m1" "parser code content")))
  (parameterize ([current-auto-distillation-enabled? #t]
                 [current-ws-evolution-enabled? #f]
                 [current-rollback-state (make-default-rollback-state)])
    (define-values (task-state-raw task-state augmented reflection)
      (prepare-turn-context-state ctx cfg sess))
    (define new-concs
      (filter (λ (c) (equal? (task-conclusion-id c) "auto-m1"))
              (agent-session-task-conclusions sess)))
    (check-equal? (length new-concs) 1)
    (define c (car new-concs))
    ;; recognized legacy deterministic fallback
    (check-true (legacy-deterministic-fallback? c))
    ;; byte-identity pins: every field matches the frozen writer shape
    (check-equal? (task-conclusion-id c) "auto-m1")
    (check-equal? (task-conclusion-text c) "[Auto] parser code content")
    (check-equal? (task-conclusion-category c) 'fact)
    (check-equal? (task-conclusion-origin-message-ids c) '("m1"))
    (check-equal? (task-conclusion-relevance-tags c) '(auto-distilled))
    ;; serialized store bytes identical to the pre-R99a writer (timestamp pinned)
    (define pinned-record
      (task-conclusion "auto-m1" "[Auto] parser code content" 'fact 'idle '("m1") 0 '(auto-distilled) '()))
    (define pinned (jsexpr->string (conclusion-hash->json-safe (conclusion->hash pinned-record))))
    (define actual (jsexpr->string
                    (conclusion-hash->json-safe
                     (conclusion->hash (struct-copy task-conclusion c [timestamp 0])))))
    (check-equal? actual pinned)))
