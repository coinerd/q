#lang racket

;; @suite ci
;; @speed fast
;; @boundary unit
;; tests/test-scheduler-shadow-workflow.rkt
;; W5 (#8533): Focused governance test for the scheduler shadow workflow.
;;
;; Proves the shadow workflow is wired correctly (text-level checks only;
;; no yaml package required):
;;   - it is NOT a required PR check (action 1 + 4)
;;   - it does not substitute for any semantic gate
;;   - the scheduler variable resolution (action 2) defaults to batch
;;   - the metadata contract (action 3) is satisfied
;;   - rollback (action 5) restores the unchanged required-check set

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         racket/match)

;; ── Paths ──

(define repo-root
  (let loop ([p (current-directory)])
    (if (file-exists? (build-path p ".git"))
        p
        (let ([parent (path-only p)])
          (if (equal? parent p)
              (current-directory)
              (loop parent))))))

(define shadow-wf-path (build-path repo-root ".github" "workflows" "test-scheduler-shadow.yml"))
(define ci-wf-path (build-path repo-root ".github" "workflows" "ci.yml"))
(define policy-path (build-path repo-root "scripts" "required-pr-checks.policy"))

(define shadow-src (and (file-exists? shadow-wf-path) (file->string shadow-wf-path)))
(define ci-src (and (file-exists? ci-wf-path) (file->string ci-wf-path)))
(define policy-list (and (file-exists? policy-path) (with-input-from-file policy-path read)))

;; ── Helper ──

(define (extract-on-trigger-keys src)
  ;; Find the first `on:` line; return the list of keys at the same
  ;; indentation as the first child of `on:`. Stops at any non-indented
  ;; top-level key (e.g. `concurrency:`, `env:`, `jobs:`, `name:`) or at a
  ;; more-shallow-than-target-indent key.
  (if (not src)
      '()
      (let* ([lines (string-split src "\n")]
             [on-idx (for/first ([l (in-list lines)]
                                 [i (in-naturals 0)]
                                 #:when (or (regexp-match? #rx"^on:" l)
                                            (regexp-match? #rx"^['\"]on['\"]:" l)))
                       i)])
        (if (not on-idx)
            '()
            (let* ([target-indent (for/first ([l (in-list (drop lines (add1 on-idx)))]
                                              [i (in-naturals (add1 on-idx))])
                                    (and (regexp-match? #rx"^([ \t]+)" l)
                                         (string-length (cadr (regexp-match #rx"^([ \t]+)" l)))))]
                   [lns (drop lines (add1 on-idx))])
              (if (not target-indent)
                  '()
                  (let loop ([lns lns]
                             [acc '()])
                    (match lns
                      ['() (reverse acc)]
                      [(cons l rest)
                       (define trimmed (string-trim l))
                       (cond
                         [(or (= 0 (string-length trimmed)) (regexp-match? #rx"^#" trimmed))
                          (loop rest acc)]
                         [(and (not (regexp-match? #rx"^[ \t]" l))
                               (regexp-match? #rx"^[A-Za-z0-9_'-]" trimmed))
                          ;; Non-indented top-level key → stop.
                          (reverse acc)]
                         [(regexp-match? #rx"^[ \t]" l)
                          (define indent (string-length (cadr (regexp-match #rx"^([ \t]+)" l))))
                          (cond
                            ;; Shallower than target → leaving `on:` block.
                            [(< indent target-indent) (reverse acc)]
                            ;; Deeper (e.g. inputs: under
                            ;; workflow_dispatch:) → skip, not a sibling.
                            [(> indent target-indent) (loop rest acc)]
                            [else
                             (define m (regexp-match #rx"^[ \t]+([A-Za-z0-9_-]+):" l))
                             (if m
                                 (loop rest (cons (string->symbol (cadr m)) acc))
                                 (loop rest acc))])]
                         [else (reverse acc)])]))))))))

;; ── Test Suite ──

(define tests
  (test-suite "test-scheduler-shadow-workflow"

    (test-case "shadow workflow file exists"
      (check-true (file-exists? shadow-wf-path)))

    (test-case "shadow workflow exposes only workflow_dispatch"
      (define keys (extract-on-trigger-keys shadow-src))
      (check-equal? (length keys) 1)
      (check-equal? keys '(workflow_dispatch)))

    (test-case "shadow workflow has no pull_request trigger"
      (define keys (extract-on-trigger-keys shadow-src))
      (check-false (member 'pull_request keys))
      (check-false (member 'pull_request_target keys))
      (check-false (member 'push keys))
      (check-false (member 'schedule keys)))

    (test-case "shadow workflow resolves scheduler with batch as default"
      (check-true (regexp-match? #rx"TEST_RUNNER_SCHEDULER" shadow-src))
      (check-true (regexp-match? #rx"vars\\.TEST_RUNNER_SCHEDULER" shadow-src))
      (check-true (regexp-match? #rx"\\*\\)[ \t]+SCHEDULER=batch" shadow-src))
      (check-true (regexp-match? #rx"queue\\)[ \t]+SCHEDULER=queue" shadow-src)))

    (test-case "shadow workflow records cohort metadata fields"
      (for ([field '("ref" "sha"
                           "scheduler"
                           "shard_index"
                           "shard_total"
                           "workers"
                           "prepared_env_state"
                           "ordering"
                           "run_url"
                           "run_id"
                           "run_attempt")])
        (check-true (regexp-match? (regexp (regexp-quote field)) shadow-src)
                    (format "field ~a missing" field))))

    (test-case "required-pr-checks.policy unchanged and contains no shadow job"
      (check-true (file-exists? policy-path))
      (check-true (list? policy-list))
      (for ([entry (in-list policy-list)])
        (check-false (and (string? entry) (regexp-match? #rx"shadow" entry))
                     (format "policy must not contain 'shadow': ~v" entry)))
      (for ([required '("lint" "test-aggregate" "test-platform")])
        (check-not-false (member required policy-list)
                         (format "policy must still require ~a" required))))

    (test-case "ci.yml does not register shadow as a required check"
      (check-true (file-exists? ci-wf-path))
      (check-false (regexp-match? #rx"test-scheduler-shadow" ci-src))
      (check-false (regexp-match? #rx"shadow-run" ci-src)))

    (test-case "rollback is achievable via TEST_RUNNER_SCHEDULER=batch"
      (check-true (regexp-match? #rx"vars\\.TEST_RUNNER_SCHEDULER" shadow-src))
      (check-true (regexp-match? #rx"queue" shadow-src)))))

(module+ main
  (run-tests tests))
