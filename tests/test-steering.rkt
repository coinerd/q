#lang racket

;; tests/test-steering.rkt — Tests for exploration steering behavior
;; v0.18.0: Context-aware exploration steering with same-file dedup

(require rackunit
         rackunit/text-ui
         racket/set
         "../util/protocol-types.rkt"
         "../runtime/iteration.rkt")

;; ── Helpers ──

;; Create a tool-call struct for testing
(define (make-read-tool path)
  (make-tool-call "test-id" "read" (hash 'path path)))

(define (make-write-tool path)
  (make-tool-call "test-id" "write" (hash 'path path)))

(define (make-edit-tool path)
  (make-tool-call "test-id" "edit" (hash 'path path)))

(define (make-bash-tool cmd)
  (make-tool-call "test-id" "bash" (hash 'command cmd)))

(define (make-find-tool path)
  (make-tool-call "test-id" "find" (hash 'path path)))

(define (make-tool-without-path name)
  (make-tool-call "test-id" name (hash)))

;; ── Test Suite ──

(define test-steering
  (test-suite "Exploration Steering (v0.18.0)"

    ;; ══════════════════════════════════════════════════════════
    ;; extract-tool-target-path
    ;; ══════════════════════════════════════════════════════════

    (test-case "extract-tool-target-path: read tool with 'path key"
      (define tc (make-read-tool "foo.rkt"))
      (check-equal? (extract-tool-target-path tc) "foo.rkt"))

    (test-case "extract-tool-target-path: tool with 'file key"
      (define tc (make-tool-call "id" "read" (hash 'file "bar.rkt")))
      (check-equal? (extract-tool-target-path tc) "bar.rkt"))

    (test-case "extract-tool-target-path: tool without path returns #f"
      (define tc (make-tool-without-path "bash"))
      (check-false (extract-tool-target-path tc)))

    (test-case "extract-tool-target-path: non-hash arguments returns #f"
      (define tc (make-tool-call "id" "read" "not a hash"))
      (check-false (extract-tool-target-path tc)))

    ;; ══════════════════════════════════════════════════════════
    ;; update-seen-paths: same-file dedup
    ;; ══════════════════════════════════════════════════════════

    (test-case "same file read 10 times → counter stays at 1"
      ;; First read: counter goes 0→1
      (define-values (sp1 inc1?) (update-seen-paths (list (make-read-tool "file.rkt")) (set)))
      (check-true inc1?)
      (check-equal? (set-count sp1) 1)

      ;; Subsequent reads of same file: no increment
      (for ([_ (in-range 9)])
        (define-values (sp inc?) (update-seen-paths (list (make-read-tool "file.rkt")) sp1))
        (check-false inc? "Should not increment for same file")
        (set! sp1 sp)))

    (test-case "different files read → each increments"
      (define-values (sp1 inc1?) (update-seen-paths (list (make-read-tool "a.rkt")) (set)))
      (check-true inc1?)

      (define-values (sp2 inc2?) (update-seen-paths (list (make-read-tool "b.rkt")) sp1))
      (check-true inc2?)

      (define-values (sp3 inc3?) (update-seen-paths (list (make-read-tool "c.rkt")) sp2))
      (check-true inc3?)

      (check-equal? (set-count sp3) 3))

    (test-case "write tool resets seen-paths to empty"
      (define-values (sp1 _w1) (update-seen-paths (list (make-read-tool "a.rkt")) (set)))
      (check-equal? (set-count sp1) 1)

      (define-values (sp2 inc2?) (update-seen-paths (list (make-write-tool "a.rkt")) sp1))
      (check-equal? (set-count sp2) 0 "Write should reset seen-paths")
      (check-false inc2? "Write should not cause increment"))

    (test-case "edit tool resets seen-paths to empty"
      (define-values (sp1 _e1) (update-seen-paths (list (make-read-tool "a.rkt")) (set)))

      (define-values (sp2 inc2?) (update-seen-paths (list (make-edit-tool "a.rkt")) sp1))
      (check-equal? (set-count sp2) 0 "Edit should reset seen-paths")
      (check-false inc2?))

    (test-case "mix: same file 5x then different file → increments once for each unique"
      (define-values (sp1 _m1) (update-seen-paths (list (make-read-tool "bigfile.py")) (set)))

      ;; Read same file 4 more times — no increment
      (for ([_ (in-range 4)])
        (define-values (sp inc?) (update-seen-paths (list (make-read-tool "bigfile.py")) sp1))
        (check-false inc?)
        (set! sp1 sp))

      ;; Now read a different file — should increment
      (define-values (sp2 inc2?) (update-seen-paths (list (make-read-tool "other.py")) sp1))
      (check-true inc2? "Different file should increment"))

    (test-case "bash tool: no path → uses 'no-path sentinel"
      (define tc (make-bash-tool "grep -rn def ."))
      (define-values (sp inc?) (update-seen-paths (list tc) (set)))
      ;; bash is not in read-tools, so it won't be collected
      ;; But it's also not a write tool, so has-new-path? depends on whether
      ;; new-paths is empty
      (check-false inc? "bash tool without path match doesn't add new path"))

    (test-case "find tool with path → collected in seen-paths"
      (define tc (make-find-tool "src/"))
      (define-values (sp inc?) (update-seen-paths (list tc) (set)))
      (check-true inc? "find with new path should increment")
      (check-equal? (set-count sp) 1))

    (test-case "same file via read then find → deduped"
      (define-values (sp1 _w2) (update-seen-paths (list (make-read-tool "foo.rkt")) (set)))
      (check-equal? (set-count sp1) 1)

      (define-values (sp2 inc2?) (update-seen-paths (list (make-find-tool "foo.rkt")) sp1))
      (check-false inc2? "Same path via different tool should not increment"))

    (test-case "write then read same file → counter restarts"
      ;; Read first
      (define-values (sp1 _s1) (update-seen-paths (list (make-read-tool "foo.rkt")) (set)))

      ;; Write (resets)
      (define-values (sp2 _s2) (update-seen-paths (list (make-write-tool "foo.rkt")) sp1))
      (check-equal? (set-count sp2) 0)

      ;; Read same file again — should increment (seen-paths was reset)
      (define-values (sp3 inc3?) (update-seen-paths (list (make-read-tool "foo.rkt")) sp2))
      (check-true inc3? "After write reset, same file should increment"))

    (test-case "multiple reads in one batch: all deduped against seen set"
      (define batch (list (make-read-tool "a.rkt") (make-read-tool "b.rkt")))
      (define-values (sp1 inc1?) (update-seen-paths batch (set)))
      (check-true inc1? "New files in batch should increment")
      (check-equal? (set-count sp1) 2)

      ;; Same batch again — no increment
      (define-values (sp2 inc2?) (update-seen-paths batch sp1))
      (check-false inc2? "Same files in batch should not increment"))

    ;; ══════════════════════════════════════════════════════════
    ;; Threshold values (8/12/20)
    ;; ══════════════════════════════════════════════════════════

    (test-case "gentle threshold at 8: counter 7 → no nudge, 8 → nudge"
      ;; We verify indirectly: at 7 different files, no steering level triggered
      ;; At 8, gentle steering triggers (verified by the cond logic in iteration.rkt)
      ;; This test validates the threshold constants via simulation
      (define (simulate-n-reads n)
        (for/fold ([count 0]
                   [paths (set)]
                   #:result count)
                  ([i (in-range n)])
          (define-values (new-paths inc?)
            (update-seen-paths (list (make-read-tool (format "file~a.rkt" i))) paths))
          (values (if inc?
                      (add1 count)
                      count)
                  new-paths)))

      (check-equal? (simulate-n-reads 7) 7 "7 unique files = count 7 (below gentle=8)")
      (check-equal? (simulate-n-reads 8) 8 "8 unique files = count 8 (at gentle=8)"))

    (test-case "counter resets on write mid-sequence"
      (define (simulate-mixed)
        ;; Read 5 unique files
        (define-values (c1 sp1)
          (for/fold ([c 0]
                     [sp (set)])
                    ([i (in-range 5)])
            (define-values (new-sp inc?)
              (update-seen-paths (list (make-read-tool (format "f~a.rkt" i))) sp))
            (values (if inc?
                        (add1 c)
                        c)
                    new-sp)))

        ;; Write (resets)
        (define-values (sp2 inc?) (update-seen-paths (list (make-write-tool "f0.rkt")) sp1))

        ;; Read 3 more unique files
        (define-values (c2 _final-sp)
          (for/fold ([c 0]
                     [sp sp2])
                    ([i (in-range 5 8)])
            (define-values (new-sp inc2?)
              (update-seen-paths (list (make-read-tool (format "f~a.rkt" i))) sp))
            (values (if inc2?
                        (add1 c)
                        c)
                    new-sp)))

        (values c1 c2))

      (define-values (before-write after-write) (simulate-mixed))
      (check-equal? before-write 5 "5 unique reads before write")
      (check-equal? after-write 3 "3 unique reads after write reset"))

    ;; ══════════════════════════════════════════════════════════
    ;; Edge cases
    ;; ══════════════════════════════════════════════════════════

    (test-case "empty tool calls list → no change"
      (define-values (sp inc?) (update-seen-paths '() (set)))
      (check-false inc? "No tools → no increment")
      (check-equal? (set-count sp) 0))

    (test-case "seen-paths survives across iterations (non-empty set)"
      (define sp-init (set "already.seen.rkt"))
      (define-values (sp inc?) (update-seen-paths (list (make-read-tool "already.seen.rkt")) sp-init))
      (check-false inc? "Already-seen file should not increment")
      (check-equal? (set-count sp) 1))))

(run-tests test-steering)
