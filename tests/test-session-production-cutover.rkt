#lang racket/base

;; tests/test-session-production-cutover.rkt
;; @speed fast
;; @suite security
;;
;; W3 (#8767): atomic session production cutover — red-first swap tests.
;;
;; Level 1 (W3.1): repository event-ops resist synchronized symlink swaps and
;;   preserve the hash chain.
;; Level 2 (W3.4–W3.6): the production hot paths (buffer-or-append!,
;;   ensure-persisted!, sm-append!) route through the repository, so an
;;   artifact swap cannot escape the session root.

(require rackunit
         racket/file
         racket/path
         "../runtime/session/session-repository.rkt"
         "../runtime/session/session-filesystem.rkt"
         "../runtime/session/session-store-integrity.rkt"
         "../runtime/session/session-types.rkt"
         "../runtime/session/session-manager.rkt"
         "../runtime/agent-session.rkt"
         "../util/message/message.rkt"
         (only-in "../util/content/content-parts.rkt" make-text-part)
         "helpers/session-fs-harness.rkt"
         "helpers/session-fixture.rkt")

(define (mkmsg [id "m1"] [parent #f])
  (make-message id parent 'user 'text (list (make-text-part "hello")) (current-seconds) (hasheq)))

(define (skip-unless-supported)
  (unless (supported-on-this-platform?)
    ((current-test-case-around) (lambda () (check-true #t "skipped on non-POSIX platform")))))

;; ============================================================
;; Level 1 — repository event-ops resist swaps and preserve integrity
;; ============================================================

(test-case "repository-append-event! preserves the hash chain (round-trip)"
  (skip-unless-supported)
  (with-fs-root (lambda ()
                  (define root (fs-root-path (current-fs-root)))
                  (define repo (make-session-repository root))
                  (repository-append-event! repo "sess-1" (mkmsg "a"))
                  (repository-append-event! repo "sess-1" (mkmsg "b" "a"))
                  (define events (repository-load-events repo "sess-1"))
                  (check-equal? (length events) 2)
                  (define chain (verify-hash-chain (build-path root "sess-1" "session.jsonl")))
                  (check-true (hash-ref chain 'valid?) "hash chain must be valid")
                  (check-true (hash-ref chain 'has-hashes?))
                  (close-session-repository! repo))))

(test-case "repository-write-version-header! writes a header iff the log is empty"
  (skip-unless-supported)
  (with-fs-root (lambda ()
                  (define root (fs-root-path (current-fs-root)))
                  (define repo (make-session-repository root))
                  (repository-write-version-header! repo "sess-v")
                  (define e1 (repository-load-events repo "sess-v"))
                  (check-true (and (pair? e1) (equal? (hash-ref (car e1) 'kind #f) "session-info")))
                  ;; Idempotent: a second call must not duplicate or clobber.
                  (repository-write-version-header! repo "sess-v")
                  (check-equal? (length (repository-load-events repo "sess-v")) 1)
                  ;; Non-empty after a real event: header write is a no-op.
                  (repository-append-event! repo "sess-v" (mkmsg "x"))
                  (repository-write-version-header! repo "sess-v")
                  (check-equal? (length (repository-load-events repo "sess-v")) 2)
                  (close-session-repository! repo))))

(test-case "repository pending marker lifecycle is no-follow"
  (skip-unless-supported)
  (with-fs-root (lambda ()
                  (define root (fs-root-path (current-fs-root)))
                  (define repo (make-session-repository root))
                  (check-false (repository-has-pending-marker? repo "sess-p"))
                  (repository-write-pending-marker! repo "sess-p" 1)
                  (check-true (repository-has-pending-marker? repo "sess-p"))
                  (repository-remove-pending-marker! repo "sess-p")
                  (check-false (repository-has-pending-marker? repo "sess-p"))
                  ;; remove when absent is a no-op (no raise).
                  (repository-remove-pending-marker! repo "sess-p")
                  (close-session-repository! repo))))

(test-case "repository-append-event! resists an artifact symlink swap (fail-closed)"
  (skip-unless-supported)
  (with-fs-root
   (lambda ()
     (define root (fs-root-path (current-fs-root)))
     (define repo (make-session-repository root))
     (repository-append-event! repo "sess-s" (mkmsg "a"))
     (define-values (sentinel-dir sentinel-file) (make-outside-sentinel "OUTSIDE-SECRET"))
     ;; Attacker swaps session.jsonl -> symlink to the outside secret.
     (swap-artifact-to-symlink! root "sess-s" "session.jsonl" sentinel-file)
     ;; The no-follow append must NOT follow the symlink: it raises (fail-closed)
     ;; and the outside sentinel is unchanged.
     (check-exn exn:fail? (lambda () (repository-append-event! repo "sess-s" (mkmsg "b" "a"))))
     (check-equal? (outside-sentinel-value sentinel-dir) "OUTSIDE-SECRET")
     (close-session-repository! repo))))

(test-case "repository-fork-event-log! copies the ancestor path without escaping"
  (skip-unless-supported)
  (with-fs-root (lambda ()
                  (define root (fs-root-path (current-fs-root)))
                  (define repo (make-session-repository root))
                  (repository-append-event! repo "src" (mkmsg "root"))
                  (repository-append-event! repo "src" (mkmsg "child" "root"))
                  (repository-append-event! repo "src" (mkmsg "sibling" "root"))
                  ;; Fork at "child": path is root -> child (sibling excluded).
                  (define n (repository-fork-event-log! repo "src" "dst" "child"))
                  (check-equal? n 2)
                  (define dst (repository-load-events repo "dst"))
                  ;; header + 2 path entries = 3 lines
                  (check-equal? (length dst) 3)
                  (close-session-repository! repo))))

;; ============================================================
;; Level 2 — production hot paths route through the repository
;; ============================================================

(test-case "PRODUCTION buffer-or-append! resists an artifact symlink swap"
  (skip-unless-supported)
  (with-fs-root (lambda ()
                  (define root (fs-root-path (current-fs-root)))
                  ;; Built via the production constructor: the session carries a repository.
                  (define sess (make-test-session #:dir root))
                  (define sid (agent-session-session-id sess))
                  (check-pred session-repository?
                              (agent-session-repository sess)
                              "production session must carry a repository")
                  (buffer-or-append! sess (mkmsg "a"))
                  (define-values (sentinel-dir sentinel-file) (make-outside-sentinel "PROD-SECRET"))
                  ;; Attacker swaps the production session.jsonl -> symlink to outside secret.
                  (swap-artifact-to-symlink! root sid "session.jsonl" sentinel-file)
                  ;; Production persistence must NOT follow the swapped link.
                  (check-exn exn:fail? (lambda () (buffer-or-append! sess (mkmsg "b" "a"))))
                  (check-equal? (outside-sentinel-value sentinel-dir) "PROD-SECRET")
                  (close-session-repository! (agent-session-repository sess)))))

(test-case "PRODUCTION sm-append! resists an artifact symlink swap"
  (skip-unless-supported)
  (with-fs-root (lambda ()
                  (define root (fs-root-path (current-fs-root)))
                  (define mgr (make-persistent-session-manager root))
                  (check-pred session-repository?
                              (persistent-session-manager-repository mgr)
                              "persistent manager must carry a repository")
                  (sm-append! mgr "mgr-sess" (mkmsg "a"))
                  (define-values (sentinel-dir sentinel-file) (make-outside-sentinel "MGR-SECRET"))
                  (swap-artifact-to-symlink! root "mgr-sess" "session.jsonl" sentinel-file)
                  (check-exn exn:fail? (lambda () (sm-append! mgr "mgr-sess" (mkmsg "b" "a"))))
                  (check-equal? (outside-sentinel-value sentinel-dir) "MGR-SECRET")
                  (close-session-repository! (persistent-session-manager-repository mgr)))))

(test-case "FALLBACK: buffer-or-append! works via path when repository is #f"
  ;; Backward compatibility: a session without a repository degrades to the
  ;; path-based persistence (e.g. on non-POSIX backends). It must still write a
  ;; valid, readable event log.
  (skip-unless-supported)
  (with-fs-root (lambda ()
                  (define root (fs-root-path (current-fs-root)))
                  (define dir (build-path root "fb-sess"))
                  (define sess (make-session-struct #:id "fb-sess" #:dir dir #:repository #f))
                  (ensure-persisted! sess)
                  (buffer-or-append! sess (mkmsg "a"))
                  ;; Read back via an independent repository to verify the file is valid JSONL.
                  (define verify-repo (make-session-repository root))
                  (define events (repository-load-events verify-repo "fb-sess"))
                  ;; header + one event
                  (check-equal? (length events) 2)
                  (close-session-repository! verify-repo))))
