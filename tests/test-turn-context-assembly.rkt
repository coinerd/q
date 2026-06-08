#lang racket/base
;; test-turn-context-assembly.rkt — Characterization tests for turn-orchestrator decomposition
;;
;; v0.96.0 W0: Baseline characterization tests documenting current structure
;; before extracting build-assembled-context phases into dedicated helpers.

(require rackunit
         racket/file
         racket/string)

(define turn-orch-path (build-path (current-directory) ".." "runtime" "turn-orchestrator.rkt"))

;; ---------------------------------------------------------------------------
;; Source structure metrics (W0 baseline)
;; ---------------------------------------------------------------------------

(test-case "W0 baseline: turn-orchestrator.rkt exists and is readable"
  (check-true (file-exists? turn-orch-path)))

(test-case "W0 baseline: turn-orchestrator.rkt has ~534 LOC"
  (define lines (file->lines turn-orch-path))
  (check-true (>= (length lines) 500) (format "Expected >= 500 LOC, got ~a" (length lines))))

(test-case "W0 baseline: turn-orchestrator.rkt has ~42 import lines"
  (define src (file->string turn-orch-path))
  (define import-lines
    (filter (lambda (l) (or (string-contains? l "only-in") (string-contains? l "require")))
            (string-split src "\n")))
  (check-true (>= (length import-lines) 35)
              (format "Expected >= 35 import lines, got ~a" (length import-lines))))

(test-case "W0 baseline: turn-orchestrator.rkt contains build-assembled-context"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (build-assembled-context")
              "build-assembled-context should exist"))

(test-case "W0 baseline: turn-orchestrator.rkt contains assemble-context/pure"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (assemble-context/pure")
              "assemble-context/pure should exist"))

(test-case "W0 baseline: turn-orchestrator.rkt contains symbol->task-state"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (symbol->task-state") "symbol->task-state should exist"))

(test-case "W0 baseline: turn-orchestrator.rkt contains register-session-extensions!"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (register-session-extensions!")
              "register-session-extensions! should exist"))

(test-case "W0 baseline: turn-orchestrator.rkt contains run-provider-turn"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (run-provider-turn") "run-provider-turn should exist"))

(test-case "W0 baseline: build-assembled-context is ~160-180 LOC"
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
  (check-true (and (>= fn-len 30) (<= fn-len 80))
              (format "Expected build-assembled-context 30-80 LOC (post-W1), got ~a" fn-len)))

(test-case "W0 baseline: turn-orchestrator provides 5 functions"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "run-provider-turn") "provides run-provider-turn")
  (check-true (string-contains? src "build-assembled-context") "provides build-assembled-context")
  (check-true (string-contains? src "register-session-extensions!")
              "provides register-session-extensions!")
  (check-true (string-contains? src "assemble-context/pure") "provides assemble-context/pure")
  (check-true (string-contains? src "current-last-task-fsm-state")
              "provides current-last-task-fsm-state"))

;; ---------------------------------------------------------------------------
;; turn-context.rkt does NOT exist yet (W2 will create it)
;; ---------------------------------------------------------------------------

(test-case "W0 baseline: context-assembly/turn-context.rkt does not exist yet"
  (define path (build-path (current-directory) ".." "runtime" "context-assembly" "turn-context.rkt"))
  (check-false (file-exists? path) "turn-context.rkt should not exist yet"))

;; ---------------------------------------------------------------------------
;; W1: Verify extracted helpers exist
;; ---------------------------------------------------------------------------

(test-case "W1: turn-orchestrator.rkt contains prepare-turn-context-state"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (prepare-turn-context-state")
              "prepare-turn-context-state should exist after W1 extraction"))

(test-case "W1: turn-orchestrator.rkt contains emit-context-assembly-events!"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(define (emit-context-assembly-events!")
              "emit-context-assembly-events! should exist after W1 extraction"))

(test-case "W1: build-assembled-context calls prepare-turn-context-state"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(prepare-turn-context-state ctx-to-use config-raw session)")
              "build-assembled-context should delegate to prepare-turn-context-state"))

(test-case "W1: build-assembled-context calls emit-context-assembly-events!"
  (define src (file->string turn-orch-path))
  (check-true (string-contains? src "(emit-context-assembly-events!")
              "build-assembled-context should delegate to emit-context-assembly-events!"))
