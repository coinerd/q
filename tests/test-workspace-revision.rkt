#lang racket/base

;; tests/test-workspace-revision.rkt
;; W3B (#8940): Workspace revision — file mutation generation tracking
;; for evidence invalidation.
;;
;; When a file is mutated, prior evidence (test outputs, git status) that
;; referenced that file at an earlier generation becomes stale. The
;; workspace-revision tracker maintains per-path generation counters so
;; evidence can be marked invalid after mutations.

(require rackunit
         rackunit/text-ui
         "../util/outcome/outcome-types.rkt"
         "../runtime/task-memory/workspace-revision.rkt")

(define-test-suite workspace-revision-suite
                   ;; ── Construction ──
                   (test-case "make-workspace-revision produces empty tracker"
                     (define ws (make-workspace-revision))
                     (check-true (workspace-revision? ws))
                     (check-equal? (workspace-revision-count ws) 0))
                   ;; ── ref returns 0 for unknown paths ──
                   (test-case "workspace-revision-ref returns 0 for unknown path"
                     (define ws (make-workspace-revision))
                     (check-equal? (workspace-revision-ref ws "foo.rkt") 0))
                   ;; ── bump increments generation ──
                   (test-case "workspace-revision-bump returns new revision with gen 1"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     (check-equal? (workspace-revision-ref ws2 "foo.rkt") 1))
                   (test-case "workspace-revision-bump is cumulative"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     (define ws3 (workspace-revision-bump ws2 "foo.rkt"))
                     (check-equal? (workspace-revision-ref ws3 "foo.rkt") 2))
                   (test-case "bump on one path does not affect another"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     (define ws3 (workspace-revision-bump ws2 "bar.rkt"))
                     (check-equal? (workspace-revision-ref ws3 "foo.rkt") 1)
                     (check-equal? (workspace-revision-ref ws3 "bar.rkt") 1))
                   ;; ── Immutability: bump does not mutate original ──
                   (test-case "bump returns a new revision (immutable)"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     ;; Original unchanged
                     (check-equal? (workspace-revision-ref ws "foo.rkt") 0)
                     ;; New has the bump
                     (check-equal? (workspace-revision-ref ws2 "foo.rkt") 1))
                   ;; ── bump-all ──
                   (test-case "workspace-revision-bump-all bumps multiple paths"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump-all ws '("a.rkt" "b.rkt" "c.rkt")))
                     (check-equal? (workspace-revision-ref ws2 "a.rkt") 1)
                     (check-equal? (workspace-revision-ref ws2 "b.rkt") 1)
                     (check-equal? (workspace-revision-ref ws2 "c.rkt") 1)
                     (check-equal? (workspace-revision-count ws2) 3))
                   (test-case "bump-all is idempotent in result count (dedupes)"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump-all ws '("a.rkt" "a.rkt" "b.rkt")))
                     (check-equal? (workspace-revision-ref ws2 "a.rkt") 1)
                     (check-equal? (workspace-revision-count ws2) 2))
                   ;; ── count tracks distinct paths ──
                   (test-case "workspace-revision-count tracks distinct paths"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "a.rkt"))
                     (define ws3 (workspace-revision-bump ws2 "b.rkt"))
                     (define ws4 (workspace-revision-bump ws3 "a.rkt"))
                     (check-equal? (workspace-revision-count ws4) 2))
                   ;; ── Staleness check ──
                   (test-case "evidence at gen 0 is stale after file bumped to gen 1"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     ;; Evidence observed the file at gen 0 (before bump)
                     (check-true (workspace-revision-stale? ws2 "foo.rkt" 0)))
                   (test-case "evidence at current gen is not stale"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     ;; Evidence observed the file at gen 1 (after bump)
                     (check-false (workspace-revision-stale? ws2 "foo.rkt" 1)))
                   (test-case "evidence at gen higher than current is not stale (future)"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     ;; Evidence claims gen 5 but tracker only has gen 1 — treat as not stale
                     ;; (future gens are optimistic, current evidence is still valid)
                     (check-false (workspace-revision-stale? ws2 "foo.rkt" 5)))
                   (test-case "evidence on unknown path is not stale"
                     (define ws (make-workspace-revision))
                     (check-false (workspace-revision-stale? ws "unknown.rkt" 0)))
                   ;; ── Snapshot / restore ──
                   (test-case "workspace-revision-snapshot produces a hash"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     (define ws3 (workspace-revision-bump ws2 "bar.rkt"))
                     (define snap (workspace-revision-snapshot ws3))
                     (check-true (hash? snap))
                     (check-equal? (hash-ref snap "foo.rkt") 1)
                     (check-equal? (hash-ref snap "bar.rkt") 1))
                   (test-case "restore-from-snapshot reproduces the revision"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump ws "foo.rkt"))
                     (define ws3 (workspace-revision-bump ws2 "bar.rkt"))
                     (define snap (workspace-revision-snapshot ws3))
                     (define restored (workspace-revision-from-snapshot snap))
                     (check-equal? (workspace-revision-ref restored "foo.rkt") 1)
                     (check-equal? (workspace-revision-ref restored "bar.rkt") 1)
                     (check-equal? (workspace-revision-count restored) 2))
                   ;; ── Paths listing ──
                   (test-case "workspace-revision-paths lists all tracked paths"
                     (define ws (make-workspace-revision))
                     (define ws2 (workspace-revision-bump-all ws '("a.rkt" "b.rkt")))
                     (define paths (workspace-revision-paths ws2))
                     (check-equal? (sort paths string<?) '("a.rkt" "b.rkt")))
                   ;; ── bump-from-outcome: integrate with typed-tool-outcome ──
                   (test-case "bump-from-artifact-outcome bumps the path in payload"
                     ;; Simulate an artifact outcome with path in payload
                     (define ws (make-workspace-revision))
                     (define o
                       (make-typed-tool-outcome #:kind 'artifact
                                                #:tool-call-id "c1"
                                                #:tool-name "edit"
                                                #:status 'success
                                                #:payload (hash 'path "src/foo.rkt")
                                                #:timestamp #f))
                     (define ws2 (workspace-revision-bump-from-outcome ws o))
                     (check-equal? (workspace-revision-ref ws2 "src/foo.rkt") 1)
                     (check-equal? (workspace-revision-count ws2) 1))
                   (test-case "bump-from-outcome on non-artifact outcome is a no-op"
                     (define ws (make-workspace-revision))
                     (define o
                       (make-typed-tool-outcome #:kind 'test
                                                #:tool-call-id "c1"
                                                #:tool-name "bash"
                                                #:status 'success
                                                #:payload (hash)
                                                #:timestamp #f))
                     (define ws2 (workspace-revision-bump-from-outcome ws o))
                     (check-equal? (workspace-revision-count ws2) 0)))

(run-tests workspace-revision-suite)
