#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; test-working-set-budget.rkt — W12: Working-set budgeting and budget-action telemetry (R6/R7)

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/working-set.rkt")

(define budget-tests
  (test-suite "Working Set Budget (W12 R6/R7)"

    ;; ============================================================
    ;; Context-relative default cap
    ;; ============================================================

    (test-case "working-set budget is 30 percent of context capped at 24576"
      (check-equal? (compute-working-set-budget 1) 0)
      (check-equal? (compute-working-set-budget 100) 30)
      ;; Cap boundary: floor(81919 * .30) = 24575; next token reaches the cap.
      (check-equal? (compute-working-set-budget 81919) 24575)
      (check-equal? (compute-working-set-budget 81920) 24576)
      ;; v0.99.78: raised cap — large-context providers keep ~3x more reads
      ;; in the Tier-A working set, preventing eviction-forced re-reads.
      (check-equal? (compute-working-set-budget 128000) 24576)
      (check-equal? (compute-working-set-budget 1000000) 24576))

    (test-case "provider-bound context share evicts to at most 30 percent"
      (define ws (make-working-set #:max-tokens 8192))
      (for ([i (in-range 5)])
        (working-set-add! ws (format "/tmp/~a.rkt" i) (format "m~a" i) 100))
      (define actions (working-set-enforce-context-share! ws 1000))
      (check-equal? (working-set-token-count ws) 300)
      (check-equal? (length actions) 2)
      (check-true (andmap (lambda (entry) (eq? (ws-entry-budget-action entry) 'evicted)) actions)))

    ;; ============================================================
    ;; R6: Default budget enforcement
    ;; ============================================================

    (test-case "default budget is 20 entries / 8192 tokens"
      (define ws (make-working-set))
      ;; Fill past both budgets with 25 entries of 500 tokens each
      (for ([i (in-range 25)])
        (define path (format "/tmp/file~a.rkt" i))
        (working-set-add! ws path (format "msg-~a" i) 500))
      ;; Should be truncated to 20 entries
      (check-equal? (working-set-entry-count ws) 16)
      ;; Tokens: 500 * 16 = 8000 <= 8192
      (check-true (<= (working-set-token-count ws) 8192)))

    (test-case "custom budget via parameters"
      (define ws (make-working-set #:max-entries 5 #:max-tokens 500))
      (for ([i (in-range 10)])
        (define path (format "/tmp/file~a.rkt" i))
        (working-set-add! ws path (format "msg-~a" i) 100))
      (check-equal? (working-set-entry-count ws) 5)
      (check-equal? (working-set-token-count ws) 500))

    (test-case "small entries allow more entries under token budget"
      (define ws (make-working-set #:max-entries 10 #:max-tokens 400))
      (for ([i (in-range 10)])
        (define path (format "/tmp/file~a.rkt" i))
        (working-set-add! ws path (format "msg-~a" i) 30))
      (check-equal? (working-set-entry-count ws) 10)
      (check-equal? (working-set-token-count ws) 300))

    ;; ============================================================
    ;; R7: Budget-action telemetry
    ;; ============================================================

    (test-case "ws-entry-budget-action: new entries are 'kept"
      (define ws (make-working-set))
      (working-set-add! ws "/tmp/test.rkt" "msg-1" 100)
      (define entry (car (working-set-entries ws)))
      (check-equal? (ws-entry-budget-action entry) 'kept))

    (test-case "evicted entries have 'evicted budget-action"
      (define ws (make-working-set #:max-entries 3 #:max-tokens 300))
      ;; Add 5 entries of 100 tokens each — last 2 will be evicted
      (for ([i (in-range 5)])
        (define path (format "/tmp/file~a.rkt" i))
        (working-set-add! ws path (format "msg-~a" i) 100))
      ;; Only 3 should remain
      (check-equal? (working-set-entry-count ws) 3)
      ;; First entry (newest) should be kept
      (check-equal? (ws-entry-budget-action (car (working-set-entries ws))) 'kept))

    (test-case "working-set-enforce-budget! returns evicted entries"
      (define ws (make-working-set #:max-entries 2 #:max-tokens 200))
      ;; Manually add entries beyond budget
      ;; Add 3 entries, each 100 tokens — budget: 2 entries / 200 tokens
      (working-set-add! ws "/a.rkt" "m1" 100)
      (working-set-add! ws "/b.rkt" "m2" 100)
      (working-set-add! ws "/c.rkt" "m3" 100)
      ;; Last entry triggers eviction of oldest
      (check-equal? (working-set-entry-count ws) 2))

    (test-case "ws-entry->text includes budget-action"
      (define now 1000)
      (define entry (ws-entry "test.rkt" "msg-1" 150 now 'kept))
      (define text (ws-entry->text entry))
      (check-true (string-contains? text "test.rkt"))
      (check-true (string-contains? text "150"))
      (check-true (string-contains? text "kept")))

    (test-case "WS-ACTION-KEPT equality"
      (check-eq? WS-ACTION-KEPT 'kept)
      (check-eq? WS-ACTION-EVICTED 'evicted)
      (check-eq? WS-ACTION-SUMMARIZED 'summarized)
      (check-eq? WS-ACTION-SUPERSEDED 'superseded))

    ;; ============================================================
    ;; Supersession detection
    ;; ============================================================

    (test-case "re-reading same path supersedes old entry"
      (define ws (make-working-set))
      (working-set-add! ws "same.rkt" "msg-1" 100)
      (working-set-add! ws "same.rkt" "msg-2" 200)
      (check-equal? (working-set-entry-count ws) 1)
      (check-equal? (ws-entry-token-estimate (car (working-set-entries ws))) 200)
      (check-equal? (ws-entry-budget-action (car (working-set-entries ws))) 'kept))))

(module+ test
  (require rackunit/text-ui)
  (run-tests budget-tests))

(module+ main
  (run-tests budget-tests))
