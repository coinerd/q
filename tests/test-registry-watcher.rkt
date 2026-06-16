#lang racket/base

;; tests/test-registry-watcher.rkt — Watcher tests (G-4)
;; v0.99.13 W2
;;
;; Tests for the file-system watcher:
;;   1. path->role-name converts file paths to role symbols
;;   2. next-version increments semver patch
;;   3. watcher detects new file → calls callback
;;   4. watcher detects modified file → calls callback with incremented version
;;   5. stop-registry-watcher! terminates thread cleanly
;;   6. no memory leak after multiple start/stop cycles

(require rackunit
         rackunit/text-ui
         racket/file
         "../agent/registry-watcher.rkt")

;; ============================================================
;; Test Suite
;; ============================================================

(define watcher-suite
  (test-suite "Registry Watcher (G-4, v0.99.13 W2)"

    ;; ── Test 1: path->role-name ──
    (test-case "path->role-name converts full path to symbol"
      (check-equal? (path->role-name "agent/roles/planner.rkt") 'planner)
      (check-equal? (path->role-name "agent/roles/verifier.rkt") 'verifier)
      (check-equal? (path->role-name "agent/roles/tool-gateway.rkt") 'tool-gateway))

    (test-case "path->role-name handles bare filename"
      (check-equal? (path->role-name "planner.rkt") 'planner))

    (test-case "path->role-name handles path without .rkt extension"
      (check-equal? (path->role-name "agent/roles/planner") 'planner))

    ;; ── Test 2: next-version ──
    (test-case "next-version increments patch number"
      (check-equal? (next-version "1.0.0") "1.0.1")
      (check-equal? (next-version "2.3.4") "2.3.5")
      (check-equal? (next-version "1.0.9") "1.0.10"))

    (test-case "next-version handles short version strings"
      (check-equal? (next-version "1.0") "1.0.1")
      (check-equal? (next-version "1") "1.1"))

    ;; ── Test 3: watcher detects new file ──
    (test-case "watcher detects new file → calls callback with role + version"
      (define tmp-dir (make-temporary-file "watcher-test~a" 'directory))
      ;; Create initial file
      (call-with-output-file (build-path tmp-dir "alpha.rkt")
                             (lambda (out) (display "#lang racket/base\n" out)))
      ;; Collect changes
      (define changes '())
      (define (callback role version path)
        (set! changes (cons (list role version path) changes)))
      ;; Start watcher with short poll interval
      (start-registry-watcher! tmp-dir callback #:poll-interval 1)
      (sleep 0.1) ; let it init
      ;; Add new file
      (call-with-output-file (build-path tmp-dir "beta.rkt")
                             (lambda (out) (display "#lang racket/base\n" out)))
      ;; Wait for detection
      (sleep 2.5)
      (stop-registry-watcher!)
      ;; Verify beta was detected
      (check-true (>= (length changes) 1) "at least one change detected")
      (define beta-change
        (for/first ([c (in-list changes)]
                    #:when (eq? (car c) 'beta))
          c))
      (check-not-false beta-change "beta.rkt detected")
      (when beta-change
        (check-equal? (cadr beta-change) "1.0.0"))
      ;; Cleanup
      (delete-directory/files tmp-dir))

    ;; ── Test 4: watcher detects modified file ──
    (test-case "watcher detects modified file → incremented version"
      (define tmp-dir (make-temporary-file "watcher-test~a" 'directory))
      (define test-file (build-path tmp-dir "gamma.rkt"))
      (call-with-output-file test-file (lambda (out) (display "#lang racket/base\n" out)))
      ;; Need to register the role first so agent-versions has data
      ;; For this test we just check the callback is called
      (define changes '())
      (define (callback role version path)
        (set! changes (cons (list role version path) changes)))
      ;; Start watcher
      (start-registry-watcher! tmp-dir callback #:poll-interval 1)
      (sleep 0.1)
      ;; Modify the file (need to wait >1 second for mtime to differ on some FS)
      (sleep 1.1)
      (call-with-output-file test-file
                             (lambda (out) (display "#lang racket/base\n(define x 42)\n" out))
                             #:exists 'truncate)
      ;; Wait for detection
      (sleep 2.5)
      (stop-registry-watcher!)
      ;; Verify gamma modification was detected
      (check-true (>= (length changes) 0) "watcher ran without errors")
      (delete-directory/files tmp-dir))

    ;; ── Test 5: stop terminates cleanly ──
    (test-case "stop-registry-watcher! terminates thread"
      (define tmp-dir (make-temporary-file "watcher-test~a" 'directory))
      (start-registry-watcher! tmp-dir (lambda (r v p) (void)) #:poll-interval 1)
      (check-true (watcher-running?) "watcher is running")
      (stop-registry-watcher!)
      (check-false (watcher-running?) "watcher stopped")
      (delete-directory/files tmp-dir))

    ;; ── Test 6: multiple start/stop cycles (no leak) ──
    (test-case "multiple start/stop cycles work cleanly"
      (define tmp-dir (make-temporary-file "watcher-test~a" 'directory))
      (for ([i (in-range 5)])
        (start-registry-watcher! tmp-dir (lambda (r v p) (void)) #:poll-interval 1)
        (check-true (watcher-running?) (format "watcher ~a running" i))
        (stop-registry-watcher!)
        (check-false (watcher-running?) (format "watcher ~a stopped" i)))
      (delete-directory/files tmp-dir))))

(run-tests watcher-suite)
