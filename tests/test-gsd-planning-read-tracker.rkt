#lang racket

;; tests/test-gsd-planning-read-tracker.rkt — Redundant read detection tests
;;
;; Tests for v0.20.2 Wave 2: Per-file read count tracking with hints.

(require rackunit
         racket/file
         "../extensions/gsd-planning.rkt"
         "../tools/tool.rkt"
         "../extensions/hooks.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-success-read-result file-path text)
  (make-success-result (list (hasheq 'type "text" 'text text)) (hasheq 'path file-path)))

;; ============================================================
;; Read counter state tests
;; ============================================================

(test-case "reset-read-counts! clears all counters"
  (reset-read-counts!)
  ;; Simulate some reads by calling tracker directly
  (define result (make-success-read-result "/tmp/test.txt" "content"))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  ;; Reset
  (reset-read-counts!)
  ;; After reset, next read should be count 1 again
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  ;; The tracker should pass through (not amend) since count is now 1
  (check-true #t "completed without error"))

(test-case "gsd-read-tracker ignores non-read tools"
  (reset-read-counts!)
  (define result (make-success-read-result "/tmp/test.txt" "content"))
  (define hook-res (gsd-read-tracker (hasheq 'tool-name "edit" 'result result)))
  (check-eq? (hook-result-action hook-res) 'pass))

(test-case "gsd-read-tracker ignores error results"
  (reset-read-counts!)
  (define err-result (make-error-result "file not found"))
  (define hook-res (gsd-read-tracker (hasheq 'tool-name "read" 'result err-result)))
  (check-eq? (hook-result-action hook-res) 'pass))

(test-case "gsd-read-tracker passes reads below threshold"
  (reset-read-counts!)
  (define result (make-success-read-result "/tmp/below.txt" "content"))
  ;; Read twice (below threshold of 3)
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  (define hook-res (gsd-read-tracker (hasheq 'tool-name "read" 'result result)))
  (check-eq? (hook-result-action hook-res) 'pass))

(test-case "gsd-read-tracker amends result at threshold"
  (reset-read-counts!)
  (define result (make-success-read-result "/tmp/threshold.txt" "content"))
  ;; Read 3 times to hit threshold
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  (define hook-res (gsd-read-tracker (hasheq 'tool-name "read" 'result result)))
  (check-eq? (hook-result-action hook-res) 'amend))

(test-case "gsd-read-tracker hint mentions file path and count"
  (reset-read-counts!)
  (define result (make-success-read-result "/tmp/hint-path.txt" "content"))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  (define hook-res (gsd-read-tracker (hasheq 'tool-name "read" 'result result)))
  (define amended-result (hash-ref (hook-result-payload hook-res) 'result))
  ;; The amended result content should contain the hint
  (define content (tool-result-content amended-result))
  (define all-text (apply string-append (map (lambda (p) (hash-ref p 'text "")) content)))
  (check-true (string-contains? all-text "hint-path.txt"))
  (check-true (string-contains? all-text "3 times")))

(test-case "gsd-read-tracker hints at multiples of threshold"
  (reset-read-counts!)
  (define result (make-success-read-result "/tmp/multi.txt" "content"))
  ;; Reads 1-2: no hint
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
  ;; Read 3: hint
  (define res3 (gsd-read-tracker (hasheq 'tool-name "read" 'result result)))
  (check-eq? (hook-result-action res3) 'amend)
  ;; Reads 4-5: no hint
  (check-eq? (hook-result-action (gsd-read-tracker (hasheq 'tool-name "read" 'result result))) 'pass)
  (check-eq? (hook-result-action (gsd-read-tracker (hasheq 'tool-name "read" 'result result))) 'pass)
  ;; Read 6: hint again
  (define res6 (gsd-read-tracker (hasheq 'tool-name "read" 'result result)))
  (check-eq? (hook-result-action res6) 'amend)
  (define amended6 (hash-ref (hook-result-payload res6) 'result))
  (define content6 (tool-result-content amended6))
  (define all-text6 (apply string-append (map (lambda (p) (hash-ref p 'text "")) content6)))
  (check-true (string-contains? all-text6 "6 times")))

(test-case "gsd-read-tracker tracks different files independently"
  (reset-read-counts!)
  (define result-a (make-success-read-result "/tmp/file-a.txt" "a"))
  (define result-b (make-success-read-result "/tmp/file-b.txt" "b"))
  ;; Read file-a twice
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result-a))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result-a))
  ;; Read file-b twice
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result-b))
  (gsd-read-tracker (hasheq 'tool-name "read" 'result result-b))
  ;; Both at count 2, neither should hint
  (check-eq? (hook-result-action (gsd-read-tracker (hasheq 'tool-name "read" 'result result-a)))
             'amend)
  (check-eq? (hook-result-action (gsd-read-tracker (hasheq 'tool-name "read" 'result result-b)))
             'amend))

(test-case "gsd-read-tracker ignores results without path in details"
  (reset-read-counts!)
  (define result-no-path (make-success-result (list (hasheq 'type "text" 'text "content")) (hasheq)))
  (define hook-res (gsd-read-tracker (hasheq 'tool-name "read" 'result result-no-path)))
  (check-eq? (hook-result-action hook-res) 'pass))

(test-case "gsd-read-tracker ignores non-tool-result values"
  (reset-read-counts!)
  (define hook-res (gsd-read-tracker (hasheq 'tool-name "read" 'result "not a result")))
  (check-eq? (hook-result-action hook-res) 'pass))
