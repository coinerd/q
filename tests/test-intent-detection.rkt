#lang racket

;; test-intent-detection.rkt — Tests for intent-without-action detection (BUG-INTENT-WITHOUT-ACTION)
;;
;; When the model completes with text expressing intent to write/edit but
;; makes no tool call, the iteration loop should inject a steering nudge
;; to prompt immediate action.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt")

;; ============================================================
;; Helpers (mirrors the ones in iteration.rkt)
;; ============================================================

(define INTENT-RX
  #px"(?i:(?:I'll|I will|let me|now I'll|now let me) (?:write|create|rewrite|edit|update|modify|fix|refactor|implement|build|generate))")

(define (detect-intent-pattern text)
  (and (string? text) (regexp-match? INTENT-RX text)))

;; ============================================================
;; Test suite
;; ============================================================

(define intent-tests
  (test-suite
   "Intent-Without-Action Detection (BUG-INTENT-WITHOUT-ACTION)"

   ;; ── Positive cases: intent patterns ──
   (test-case "detect-intent-pattern matches: I'll rewrite the file"
     (check-true (detect-intent-pattern "I'll rewrite the file")))

   (test-case "detect-intent-pattern matches: I will create a new script"
     (check-true (detect-intent-pattern "I will create a new script")))

   (test-case "detect-intent-pattern matches: Let me edit the configuration"
     (check-true (detect-intent-pattern "Let me edit the configuration")))

   (test-case "detect-intent-pattern matches: Now I'll implement the fix"
     (check-true (detect-intent-pattern "Now I'll implement the fix")))

   (test-case "detect-intent-pattern matches: Now let me build the module"
     (check-true (detect-intent-pattern "Now let me build the module")))

   (test-case "detect-intent-pattern matches: I'll update the version"
     (check-true (detect-intent-pattern "I'll update the version")))

   ;; ── Negative cases: no intent ──
   (test-case "detect-intent-pattern rejects: The task is complete"
     (check-false (detect-intent-pattern "The task is complete")))

   (test-case "detect-intent-pattern rejects: I'll think about it"
     (check-false (detect-intent-pattern "I'll think about it")))

   (test-case "detect-intent-pattern rejects: empty string"
     (check-false (detect-intent-pattern "")))

   (test-case "detect-intent-pattern rejects: #f"
     (check-false (detect-intent-pattern #f)))

   ;; ── Case insensitive ──
   (test-case "detect-intent-pattern is case-insensitive"
     (check-true (detect-intent-pattern "I'LL REWRITE THE FILE"))
     (check-true (detect-intent-pattern "LET ME CREATE the module")))

   ;; ── Structural test: verify intent detection code exists in iteration.rkt ──
   (test-case "iteration.rkt contains detect-intent-pattern helper"
     (define source
       (file->string
        (if (file-exists? "runtime/iteration.rkt")
            "runtime/iteration.rkt"
            "../runtime/iteration.rkt")))
     (check-not-false
      (regexp-match? #rx"detect-intent-pattern" source)
      "iteration.rkt should contain detect-intent-pattern"))

   (test-case "iteration.rkt contains intent steering nudge injection"
     (define source
       (file->string
        (if (file-exists? "runtime/iteration.rkt")
            "runtime/iteration.rkt"
            "../runtime/iteration.rkt")))
     (check-not-false
      (regexp-match? #rx"intent-without-action|intent\\.nudge" source)
      "iteration.rkt should contain intent-without-action or intent.nudge event"))

   ))

(run-tests intent-tests)
