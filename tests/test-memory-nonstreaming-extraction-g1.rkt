#lang racket/base

;;; test-memory-nonstreaming-extraction-g1.rkt — W0→W2: Verify extraction works
;;; through the full main-loop → agent-turn → streaming-phase chain.

;;; Original G1b gap was a false alarm: the non-streaming main-loop delegates to
;;; run-agent-turn which ALWAYS goes through run-streaming-phase → build-stream-result
;;; where extraction fires. W1 fix moved extraction before the tool-call-parts cond,
;;; so ALL response types (text-only, tool-call, non-streaming) get extraction.

(require rackunit
         racket/file
         racket/string
         racket/runtime-path)

(define-runtime-path here ".")
(define q-dir (simplify-path (build-path here "..")))

(test-case "W2 verification: streaming loop imports and calls auto-extraction"
  (define stream-source (file->string (build-path q-dir "agent" "loop-stream.rkt")))
  (check-true (string-contains? stream-source "maybe-auto-extract-after-response!")))

(test-case "W2 verification: agent-turn always goes through streaming-phase"
  (define loop-source (file->string (build-path q-dir "agent" "loop.rkt")))
  ;; run-agent-turn always calls run-streaming-phase — no non-streaming bypass
  (check-true (string-contains? loop-source "run-streaming-phase")))

(test-case "W0 F5: non-streaming extraction acceptance needs behavioral evidence"
  ;; Source-shape checks are useful drift alerts, but v0.95.18 requires a behavioral
  ;; public/session/turn test or a stronger reviewed architectural invariant. W3 must
  ;; replace this expected-red marker with executable evidence.
  (check-true #f "Expected red: add behavioral non-streaming extraction evidence in W3"))
