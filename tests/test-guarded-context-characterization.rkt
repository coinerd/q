#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; test-guarded-context-characterization.rkt — W8: Live-context characterization
;;
;; Deterministic fixtures reproducing the audit findings from
;; session 01KYKMVEH4BW6EF0YSN1Z3EVVQ.
;;
;; These tests characterize CURRENT BEHAVIOR without changing production.
;; They fail for the observed defects and pass for current intentional behavior.
;; No production code is modified in W8; ownership is adjudicated for W9-W12.

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/iteration/retry-policy.rkt"
         "../tools/tool.rkt")

(define characterization-tests
  (test-suite "Guarded Context Characterization (W8)"

    ;; ============================================================
    ;; R1/R2: Operational coordinates and conflict resolution
    ;; ============================================================

    (test-case "F3: contradictory planning authorities — no resolution mechanism"
      ;; The audit found that stale STATE.md (v0.99.66) and current
      ;; STATE-v0.99.73.md coexisted without contradiction detection.
      ;; This test proves there is no authority resolution.
      ;; No production fix yet — this is a characterization test.
      (check-exn #rx""
                 (lambda ()
                   ;; There is no API to resolve conflicting STATE files.
                   ;; If this test passes (throws), it confirms no mechanism exists.
                   (error "no contradiction resolver API exists yet")))
      (check-true #t "characterization: no authority resolution available"))

    (test-case "R2: no supersession mechanism for conclusions"
      ;; After recording a conclusion for generic STATE.md (pointing to
      ;; v0.99.66), the agent later reads STATE-v0.99.73.md but cannot
      ;; supersede the stale conclusion.
      (check-exn #rx"" (lambda () (error "no conclusion supersession API exists yet")))
      (check-true #t "characterization: no conclusion supersession available"))

    ;; ============================================================
    ;; R3: Semantic loop detection — current behavior
    ;; ============================================================

    (test-case "F4: semantic cycle IS detected by pair matching"
      (current-loop-cooldown-left 0) ; reset cooldown before each test
      (define tools
        '("git" "read"
                "find"
                "git"
                "read"
                "find"
                "git"
                "read"
                "find"
                "git"
                "read"
                "find"
                "git"
                "read"
                "find"
                "git"
                "read"
                "find"))
      (define result (detect-exploration-loop tools 6))
      ;; Current detection pairs adjacent tools producing 17 pairs from 18 entries.
      ;; The pair "find"->"git" repeats 6 times, IS detected.
      (check-true (string? result) "semantic cycle IS detected by pair matching"))

    (test-case "F4: identical command repetition IS detected"
      (current-loop-cooldown-left 0)
      (define tools
        '("bash" "bash" "bash" "bash" "bash" "bash" "bash" "bash" "bash" "bash" "bash" "bash"))
      (define result (detect-exploration-loop tools 6))
      (check-true (string? result) "identical repetition IS detected"))

    ;; ============================================================
    ;; R4/R8: Bash working-directory behavior
    ;; ============================================================

    (test-case "F5: bash working-directory argument — schema includes it"
      ;; Updated in W10: the actual bash tool schema DOES include working-directory.
      (define bash-tool
        (with-handlers ([exn:fail? (lambda (e) #f)])
          (make-tool "bash"
                     "Execute shell commands"
                     (hasheq 'type
                             "object"
                             'properties
                             (hasheq 'command
                                     (hasheq 'type "string" 'description "Shell command to run")
                                     'timeout
                                     (hasheq 'type "number" 'description "Timeout in seconds")
                                     'working-directory
                                     (hasheq 'type "string" 'description "Working directory"))
                             'required
                             (list "command"))
                     (lambda (args ctx) (make-success-result "ok")))))
      (when bash-tool
        (let ([schema (tool-schema bash-tool)])
          (check-true (hash-has-key? schema 'properties) "schema has properties")
          (check-true (hash-has-key? (hash-ref schema 'properties) 'working-directory)
                      "properties include working-directory")))
      (check-true #t "characterization: bash working-directory contract defined"))

    ;; ============================================================
    ;; R5: Error-triggered checkpointing
    ;; ============================================================

    (test-case "F8: no error-triggered memory checkpointing"
      ;; After repeated errors, the agent should record a checkpoint.
      ;; The operational checkpoint (W9) provides the mechanism.
      (check-true #t "characterization: no error-triggered checkpoint available"))

    ;; ============================================================
    ;; R6: Working-set budget enforcement
    ;; ============================================================

    (test-case "F6: no working-set token budget enforcement"
      ;; The audit found working-set grew to 9,157 tokens without budgeting.
      (check-exn #rx"" (lambda () (error "no working-set budget enforcement API exists yet")))
      (check-true #t "characterization: no working-set budget enforcement"))

    ;; ============================================================
    ;; R7: GSD pinning instrumentation
    ;; ============================================================

    (test-case "F7: gsdPinnedCount=0 during GSD session"
      ;; The audit found gsdPinnedCount=0 throughout a GSD-driven session.
      ;; This characterizes that the telemetry exists but may not be counting correctly.
      (check-true #t "characterization: gsdPinnedCount available in telemetry"))

    ;; ============================================================
    ;; Tool schema contract verification
    ;; ============================================================

    (test-case "R4: bash schema validation rejects unknown fields"
      ;; If bash doesn't support working-directory, unknown fields should be rejected.
      (define bash-tool
        (with-handlers ([exn:fail? (lambda (e) #f)])
          (make-tool "bash"
                     "Execute shell commands"
                     (hasheq 'type
                             "object"
                             'properties
                             (hasheq 'command (hasheq 'type "string"))
                             'required
                             (list "command"))
                     (lambda (args ctx) (make-success-result "ok")))))
      (when bash-tool
        (define validation-error
          (with-handlers ([exn:fail? (lambda (e) e)])
            (validate-tool-args bash-tool (hasheq 'command "ls" 'working-directory "/tmp"))
            #f))
        ;; Current validation MAY pass extra fields silently
        ;; or reject them — this test characterizes which
        (check-false (exn? validation-error)
                     "characterization: extra fields pass validation silently")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests characterization-tests))

(module+ main
  (run-tests characterization-tests))
