#lang racket

;; tests/test-streaming-text-preservation.rkt — v0.14.3 Wave 2
;;
;; Verifies that partial streaming text is preserved in the transcript
;; when a runtime.error occurs during streaming.

(require rackunit
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; Streaming text preservation on runtime.error
;; ============================================================

(test-case "runtime.error preserves non-empty streaming text as partial entry"
  (define state (initial-ui-state))
  ;; Simulate streaming: add streaming text
  (define delta-evt
    (make-event "model.stream.delta" 1000 "s1" "t1" (hasheq 'delta "Ich analysiere das Script...")))
  (define s1 (apply-event-to-state state delta-evt))
  (check-equal? (ui-state-streaming-text s1) "Ich analysiere das Script...")
  (check-true (ui-state-busy? s1))

  ;; Add more streaming text
  (define delta-evt2
    (make-event "model.stream.delta"
                1001
                "s1"
                "t1"
                (hasheq 'delta "Ich schreibe das Script komplett neu...")))
  (define s2 (apply-event-to-state s1 delta-evt2))
  (check-equal? (ui-state-streaming-text s2)
                "Ich analysiere das Script...Ich schreibe das Script komplett neu...")

  ;; Simulate runtime.error — should preserve streaming text
  (define err-evt
    (make-event "runtime.error"
                1002
                "s1"
                "t1"
                (hasheq 'error "HTTP read timeout (60 seconds) waiting for SSE chunk")))
  (define s3 (apply-event-to-state s2 err-evt))
  ;; Streaming text should be cleared
  (check-false (ui-state-streaming-text s3))
  (check-false (ui-state-busy? s3))
  ;; But transcript should contain the partial assistant entry + error + system hint
  (define transcript (ui-state-transcript s3))
  (define texts (map transcript-entry-text transcript))
  ;; Check that the streamed text appears in an assistant entry
  (check-true (ormap (lambda (t) (and t (string-contains? t "Ich analysiere"))) texts)
              "Partial streaming text preserved in transcript"))

(test-case "runtime.error skips empty streaming text"
  (define state (initial-ui-state))
  ;; No streaming text set
  (check-false (ui-state-streaming-text state))

  ;; Simulate runtime.error with no streaming text
  (define err-evt (make-event "runtime.error" 1000 "s1" "t1" (hasheq 'error "some error")))
  (define s1 (apply-event-to-state state err-evt))
  ;; Transcript should only have error + system hint (no assistant entry)
  (define transcript (ui-state-transcript s1))
  (define kinds (map transcript-entry-kind transcript))
  (check-false (member 'assistant kinds) "No assistant entry for empty streaming text"))

(test-case "runtime.error skips whitespace-only streaming text"
  (define state (initial-ui-state))
  ;; Set streaming text to whitespace only
  (define delta-evt (make-event "model.stream.delta" 1000 "s1" "t1" (hasheq 'delta "   ")))
  (define s1 (apply-event-to-state state delta-evt))
  (check-equal? (ui-state-streaming-text s1) "   ")

  ;; Simulate runtime.error
  (define err-evt (make-event "runtime.error" 1001 "s1" "t1" (hasheq 'error "timeout")))
  (define s2 (apply-event-to-state s1 err-evt))
  ;; Transcript should NOT have assistant entry for whitespace-only text
  (define transcript (ui-state-transcript s2))
  (define kinds (map transcript-entry-kind transcript))
  (check-false (member 'assistant kinds) "No assistant entry for whitespace-only text"))
