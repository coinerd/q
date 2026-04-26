#lang racket

;; tests/test-gsd-planning-budget-counter.rkt — /go budget counter tests
;;
;; Tests for v0.20.2 Wave 3: Read budget counter during /go execution.

(require rackunit
         "../extensions/gsd-planning.rkt"
         "../extensions/hooks.rkt")

;; ============================================================
;; Parameter tests
;; ============================================================

(test-case "go-read-budget defaults to #f"
  (set-go-read-budget! #f)
  (check-false (go-read-budget)))

(test-case "reset-go-budget! sets budget to 30"
  (reset-go-budget!)
  (check-equal? (go-read-budget) 30))

(test-case "GO-READ-BUDGET is 30"
  (check-equal? GO-READ-BUDGET 30))

(test-case "READ-ONLY-TOOLS includes read/grep/find"
  (check-not-false (member "read" READ-ONLY-TOOLS))
  (check-not-false (member "grep" READ-ONLY-TOOLS))
  (check-not-false (member "find" READ-ONLY-TOOLS)))

;; ============================================================
;; Budget enforcement in gsd-tool-guard
;; ============================================================

(test-case "tool-guard passes write tools during executing without budget change"
  (set-gsd-mode! 'executing)
  (reset-go-budget!)
  (define res (gsd-tool-guard (hasheq 'tool-name "edit" 'args (hasheq))))
  (check-eq? (hook-result-action res) 'pass)
  ;; Budget should NOT be decremented for write tools
  (check-equal? (go-read-budget) 30)
  (set-gsd-mode! #f))

(test-case "tool-guard decrements budget for read during executing"
  (set-gsd-mode! 'executing)
  (reset-go-budget!)
  (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq)))
  (check-equal? (go-read-budget) 29)
  (set-gsd-mode! #f))

(test-case "tool-guard passes reads with plenty of budget"
  (set-gsd-mode! 'executing)
  (reset-go-budget!)
  (define res (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
  (check-eq? (hook-result-action res) 'pass)
  (set-gsd-mode! #f))

(test-case "tool-guard passes reads at warning threshold (warning now in result)"
  (set-gsd-mode! 'executing)
  (reset-go-budget!)
  ;; Burn through 25 calls
  (for ([_ (in-range 25)])
    (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
  ;; Budget should be 5 now
  (check-equal? (go-read-budget) 5)
  ;; Next read should still pass (warning is now in tool-result-post)
  (define res (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
  (check-eq? (hook-result-action res) 'pass)
  (set-gsd-mode! #f))

(test-case "tool-guard blocks when budget goes below -3"
  (set-gsd-mode! 'executing)
  (reset-go-budget!)
  ;; Burn through 33 calls (30 budget + 3 overage)
  (for ([_ (in-range 33)])
    (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
  ;; Budget should be -3
  (check-equal? (go-read-budget) -3)
  ;; Next read should pass (block threshold is < -3, not ≤ -3)
  (define res-at-neg3 (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
  (check-eq? (hook-result-action res-at-neg3) 'block)
  (set-gsd-mode! #f))

(test-case "tool-guard does not enforce budget when not in executing mode"
  (set-gsd-mode! 'planning)
  (reset-go-budget!)
  ;; Call read many times
  (for ([_ (in-range 40)])
    (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
  ;; Budget should still be 30 — not decremented in planning mode
  (check-equal? (go-read-budget) 30)
  (set-gsd-mode! #f))

(test-case "tool-guard does not enforce budget when budget is #f"
  (set-gsd-mode! 'executing)
  (set-go-read-budget! #f)
  (define res (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
  (check-eq? (hook-result-action res) 'pass)
  ;; Budget should still be #f
  (check-false (go-read-budget))
  (set-gsd-mode! #f))

(test-case "tool-guard blocks edit during plan-written"
  (set-gsd-mode! 'plan-written)
  (define res (gsd-tool-guard (hasheq 'tool-name "edit" 'args (hasheq))))
  (check-eq? (hook-result-action res) 'block)
  (set-gsd-mode! #f))

(test-case "tool-guard blocks planning-write during executing"
  (set-gsd-mode! 'executing)
  (reset-go-budget!)
  (define res (gsd-tool-guard (hasheq 'tool-name "planning-write" 'args (hasheq))))
  (check-eq? (hook-result-action res) 'block)
  (set-gsd-mode! #f))

(test-case "budget is independent of read counter"
  (set-gsd-mode! 'executing)
  (reset-go-budget!)
  (reset-read-counts!)
  ;; Budget counts all read-only tools, not just file reads
  (gsd-tool-guard (hasheq 'tool-name "grep" 'args (hasheq)))
  (gsd-tool-guard (hasheq 'tool-name "find" 'args (hasheq)))
  (gsd-tool-guard (hasheq 'tool-name "ls" 'args (hasheq)))
  (check-equal? (go-read-budget) 27)
  (set-gsd-mode! #f))
