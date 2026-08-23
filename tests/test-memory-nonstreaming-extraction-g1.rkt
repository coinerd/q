#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;;; test-memory-nonstreaming-extraction-g1.rkt — W0→W2: Verify extraction works
;;; through the full main-loop → agent-turn → streaming-phase chain.
;;;
;;; v0.99.84: Extraction moved from agent/loop-stream.rkt to
;;; runtime/turn-orchestrator.rkt. These tests verify the new location.

(require rackunit
         racket/file
         racket/string
         racket/runtime-path)

(define-runtime-path here ".")
(define q-dir (simplify-path (build-path here "..")))

(test-case "v0.99.84: turn-orchestrator owns post-turn extraction"
  (define orch-source (file->string (build-path q-dir "runtime" "turn-orchestrator.rkt")))
  (check-true (string-contains? orch-source "maybe-auto-extract-after-response!")
              "turn-orchestrator should call maybe-auto-extract-after-response!"))

(test-case "v0.99.84: agent loop-stream no longer imports runtime/memory"
  (define stream-source (file->string (build-path q-dir "agent" "loop-stream.rkt")))
  (check-false (string-contains? stream-source "maybe-auto-extract-after-response!")
               "loop-stream should NOT call extraction (moved to runtime)")
  (check-false (string-contains? stream-source "runtime/memory")
               "loop-stream should NOT import runtime/memory"))

(test-case "W2 verification: agent-turn always goes through streaming-phase"
  (define loop-source (file->string (build-path q-dir "agent" "loop.rkt")))
  ;; run-agent-turn always calls run-streaming-phase — no non-streaming bypass
  (check-true (string-contains? loop-source "run-streaming-phase")))

(test-case "v0.99.84: turn-orchestrator wraps run-agent-turn and calls extraction after"
  ;; run-provider-turn calls run-agent-turn (via call-with-provider-retry),
  ;; then calls maybe-auto-extract-after-response! on the result.
  (define orch-source (file->string (build-path q-dir "runtime" "turn-orchestrator.rkt")))
  (check-true (string-contains? orch-source "run-agent-turn")
              "turn-orchestrator calls run-agent-turn")
  (check-true (string-contains? orch-source "maybe-auto-extract-after-response!")
              "turn-orchestrator calls extraction after agent turn")
  (check-true (string-contains? orch-source "maybe-reflect-session-memories!")
              "turn-orchestrator calls reflection after agent turn"))
