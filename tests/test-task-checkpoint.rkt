#lang racket/base

;; tests/test-task-checkpoint.rkt
;; W4 (#8941): Bounded active-task checkpoint — renders an
;; active-task-checkpoint into a fixed-budget protocol-safe text record
;; suitable for injection into provider context.

(require rackunit
         rackunit/text-ui
         racket/list
         "../runtime/task-memory/types.rkt"
         "../runtime/task-memory/projection.rkt"
         "../runtime/context-assembly/task-checkpoint.rkt")

(define (h . kvs)
  (for/hasheq ([i (in-range 0 (length kvs) 2)])
    (values (list-ref kvs i) (list-ref kvs (add1 i)))))

(define (make-ev-seq seq kind payload)
  (make-task-ledger-event 1
                          seq
                          (string-append "ev-" (number->string seq))
                          "sess-1"
                          "proj-1"
                          "task-1"
                          #f
                          "branch-1"
                          "turn-1"
                          "req-1"
                          "asm-1"
                          "corr-1"
                          #f
                          'runtime-observed
                          kind
                          payload
                          (* 1000 seq)
                          '()
                          "d"))

(define-test-suite
 task-checkpoint-suite
 ;; ── Budget constant ──
 (test-case "default checkpoint token budget is positive"
   (check-true (integer? DEFAULT-CHECKPOINT-TOKEN-BUDGET))
   (check-true (> DEFAULT-CHECKPOINT-TOKEN-BUDGET 0)))
 ;; ── Render: empty checkpoint ──
 (test-case "render-empty-checkpoint produces minimal text"
   (define cp (project-active-task-checkpoint '()))
   (define rec (render-task-checkpoint cp))
   (check-true (task-checkpoint-record? rec))
   (check-true (string? (task-checkpoint-record-text rec)))
   (check-true (> (string-length (task-checkpoint-record-text rec)) 0))
   (check-false (task-checkpoint-record-overflow? rec)))
 ;; ── Render: full checkpoint ──
 (test-case "render-checkpoint includes objective"
   (define evs (list (make-ev-seq 1 'objective-set (h 'summary "Ship W4 feature"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (check-true (regexp-match? #rx"Ship W4 feature" text)))
 (test-case "render-checkpoint includes owned paths"
   (define evs (list (make-ev-seq 1 'artifact-modified (h 'path "src/foo.rkt"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (check-true (regexp-match? #rx"src/foo.rkt" text)))
 (test-case "render-checkpoint includes verification state"
   (define evs (list (make-ev-seq 1 'verification-passed (h 'path "t.rkt"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (check-true (regexp-match? #rx"passing" text)))
 (test-case "render-checkpoint includes blockers"
   (define evs (list (make-ev-seq 1 'error-occurred (h 'message "OOM crash"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (check-true (regexp-match? #rx"OOM crash" text)))
 (test-case "render-checkpoint includes next action"
   (define evs (list (make-ev-seq 1 'phase-changed (h 'to-state "implementation"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (check-true (regexp-match? #rx"verify" text)))
 (test-case "render-checkpoint includes current phase"
   (define evs (list (make-ev-seq 1 'phase-changed (h 'to-state "verification"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (check-true (regexp-match? #rx"verification" text)))
 ;; ── Budget enforcement ──
 (test-case "budget=0 forces overflow"
   (define evs (list (make-ev-seq 1 'objective-set (h 'summary (make-string 1000 #\x)))))
   (define cp (project-active-task-checkpoint evs))
   (define rec (render-task-checkpoint cp #:token-budget 0))
   (check-true (task-checkpoint-record-overflow? rec)))
 (test-case "large budget avoids overflow"
   (define evs (list (make-ev-seq 1 'objective-set (h 'summary "short goal"))))
   (define cp (project-active-task-checkpoint evs))
   (define rec (render-task-checkpoint cp #:token-budget 10000))
   (check-false (task-checkpoint-record-overflow? rec)))
 ;; ── Estimate tokens ──
 (test-case "estimate-tokens approximates char/4"
   (check-true (>= (estimate-tokens "hello world") 2))
   (check-true (>= (estimate-tokens "") 0)))
 ;; ── Priority ordering: objective first ──
 (test-case "objective appears before owned-paths in text"
   (define evs
     (list (make-ev-seq 1 'objective-set (h 'summary "MYOBJECTIVE"))
           (make-ev-seq 2 'artifact-modified (h 'path "MYPATH.rkt"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (define obj-idx (regexp-match-positions #rx"MYOBJECTIVE" text))
   (define path-idx (regexp-match-positions #rx"MYPATH.rkt" text))
   (check-not-false obj-idx)
   (check-not-false path-idx)
   (check-true (< (car (car obj-idx)) (car (car path-idx)))))
 ;; ── Protocol-safe message ──
 (test-case "task-checkpoint->message wraps text as system message"
   (define cp (project-active-task-checkpoint '()))
   (define msg (task-checkpoint->message cp))
   (check-not-false msg)
   (check-equal? (hash-ref msg 'role) "system"))
 ;; ── Security: no raw content leakage ──
 (test-case "checkpoint text does not contain raw tool output markers"
   ;; The checkpoint should reference paths/shas, not embed file contents.
   (define evs (list (make-ev-seq 1 'artifact-modified (h 'path "src/x.rkt"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   ;; Should contain the path reference, not a content dump.
   (check-true (regexp-match? #rx"src/x.rkt" text))
   ;; Should not contain a fake content marker we didn't put in.
   (check-false (regexp-match? #rx"RAW_FILE_CONTENT" text)))
 ;; ── Truncation under budget ──
 (test-case "text is truncated to fit budget"
   (define evs (list (make-ev-seq 1 'objective-set (h 'summary (make-string 5000 #\z)))))
   (define cp (project-active-task-checkpoint evs))
   (define rec (render-task-checkpoint cp #:token-budget 50))
   ;; The rendered text's token estimate should be <= budget + slack.
   (check-true (<= (estimate-tokens (task-checkpoint-record-text rec)) 80)))
 ;; ── Completed work rendering ──
 (test-case "completed work renders commit sha"
   (define evs (list (make-ev-seq 1 'commit-created (h 'sha "abc1234" 'summary "fix"))))
   (define cp (project-active-task-checkpoint evs))
   (define text (task-checkpoint-record-text (render-task-checkpoint cp)))
   (check-true (regexp-match? #rx"abc1234" text))))

(run-tests task-checkpoint-suite)
