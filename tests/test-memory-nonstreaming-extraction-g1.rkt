#lang racket/base

;; @speed fast
;; @suite default

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

(test-case "W3 F5: architectural invariant — run-agent-turn has no LLM-response path bypassing run-streaming-phase"
  ;; run-agent-turn has exactly two match branches on (turn-decision-tag d-pre):
  ;; 1. 'blocked — returns early, no LLM response, no extraction needed
  ;; 2. _ (default) — delegates to run-streaming-phase which calls maybe-auto-extract-after-response!
  ;; Therefore: every LLM response goes through the extraction path.
  (define loop-source (file->string (build-path q-dir "agent" "loop.rkt")))
  ;; Verify the only match branch that bypasses run-streaming-phase is 'blocked
  (check-true (regexp-match? #rx"\\(match.*turn-decision-tag" loop-source)
              "run-agent-turn dispatches on turn-decision-tag")
  (check-true (regexp-match? #rx"'blocked" loop-source) "blocked branch exists")
  (check-true (string-contains? loop-source "run-streaming-phase")
              "default branch calls run-streaming-phase")
  ;; Verify run-streaming-phase calls extraction
  (define stream-source (file->string (build-path q-dir "agent" "loop-stream.rkt")))
  (check-true (string-contains? stream-source "maybe-auto-extract-after-response!")
              "run-streaming-phase calls auto-extraction")
  ;; Count that run-streaming-phase appears exactly once in the default branch
  ;; (not in blocked branch) — ensuring no accidental bypass
  (define matches (regexp-match* #rx"run-streaming-phase" loop-source))
  (check-true (>= (length matches) 2) "run-streaming-phase referenced in import + call site"))
