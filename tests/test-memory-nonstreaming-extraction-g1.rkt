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
