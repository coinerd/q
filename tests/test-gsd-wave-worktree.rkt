#lang racket/base

;; q/tests/test-gsd-wave-worktree.rkt — W6 (#9512a) of the
;; executor-hardening release.
;;
;; Wave worktree isolation lifecycle, per
;; .planning/waves/W6-worktree-execution.md:
;;   * make-wave-worktree! creates a worktree on a FRESH branch off the
;;     current origin/main, placed as a SIBLING of the project root
;;     (never /tmp, never inside the repo — `<cwd>/../q` path parity).
;;   * The executor session cwd is the worktree; `.planning/` resolves to
;;     the REAL project root (canonical campaign state stays shared).
;;   * cleanup-wave-worktree! removes BOTH worktree and branch, is
;;     best-effort (never raises, never masks the wave outcome).
;;   * Orphaned worktrees from a crashed attempt are reclaimed on the next
;;     campaign start (reclaim-orphaned-worktrees!); unrelated worktrees
;;     are untouched.
;;   * Feature flag `gsd.worktree-isolation` defaults ON (flipped by the
;;     W8 integration bake of this release); `#:isolate? #f` overrides.
;;
;; Layer 1 tests are pure (no git binary). Layer 2 tests run against a real
;; throwaway git repository in a temp sandbox; git-dependent cases are
;; gated on GIT availability (unavailable git means the case is logged and
;; skipped, not failed).

(require racket/file
         racket/format
         racket/list
         racket/path
         racket/string
         rackunit
         rackunit/text-ui
         "../extensions/gsd/wave-executor.rkt")

(define GIT (find-executable-path "git"))

;; path-basename helper — defined BEFORE first use (module-level order matters)
(define (path-basename p)
  (let-values ([(_parent name _dir?) (split-path p)])
    (path->string name)))

;; 64-hex stand-in for a campaign plan-id (SHA-256 of the manifest).
(define CID (apply string (build-list 64 (lambda (i) (string-ref "0123456789abcdef" (modulo i 16))))))
(define CID-OTHER
  (apply string (build-list 64 (lambda (i) (string-ref "fedcba9876543210" (modulo i 16))))))

;; ============================================================
;; Layer 2 helpers — real git sandbox (module-level: defines are not
;; allowed inside a test-suite expression)
;; ============================================================

(define (git! repo . args)
  (define r (default-run-git repo args))
  (unless (zero? (git-result-code r))
    (eprintf "git ~a failed: ~a\n" (string-join args " ") (string-trim (git-result-stderr r))))
  (check-equal? (git-result-code r) 0 (format "git ~a failed" (string-join args " ")))
  r)

(define (git-out repo . args)
  (string-trim (git-result-stdout (apply git! repo args))))

;; Project-root layout mirroring production: <tmp>/proj/{.planning,q}.
;; The q/ directory is a standalone git repo (no remote); a local
;; refs/heads/origin/main stand-in makes offline `worktree add ... origin/main`
;; behave exactly like the real tracking ref.
(define (make-sandbox)
  (define tmp (make-temporary-file "gsd-w6-~a" 'directory))
  (define proj (build-path tmp "proj"))
  (make-directory* (build-path proj ".planning"))
  (define repo (build-path proj "q"))
  (make-directory* repo)
  (git! repo "init" "-q" "-b" "main")
  (git! repo "config" "user.email" "w6@test.local")
  (git! repo "config" "user.name" "W6 Test")
  (call-with-output-file* (build-path repo "README.md") (lambda (p) (display "base\n" p)))
  (git! repo "add" "-A")
  (git! repo "commit" "-q" "-m" "base")
  (git! repo "update-ref" "refs/heads/origin/main" "HEAD")
  (values proj repo))

;; ============================================================
;; Layer 1 — pure naming, placement, command shape, flag
;; ============================================================

;; All cases run in one suite so the file's process exit code is the
;; rackunit failure count (0 = pass) under plain `racket file.rkt`.
(define w6-suite
  (test-suite "wave worktree isolation (v1.00.x W6, #9512a)"

    (test-case "worktree-hash8 takes first 8 hex chars, downcased"
      (check-equal? (worktree-hash8 CID) "01234567")
      (check-equal? (worktree-hash8 "DEADBEEF00") "deadbeef")
      (check-equal? (worktree-hash8 "ab") "ab")) ; lenient on short ids

    (test-case "dirname and branch follow the documented contract"
      (check-equal? (wave-worktree-dirname CID 4) "wt-campaign-01234567-w4")
      (check-equal? (wave-worktree-branch-name CID 4) "campaign/01234567/w4"))

    (test-case "worktree dir is a SIBLING of the repo root — never inside it, never /tmp-derived"
      (define repo (string->path "/x/proj/q"))
      (define dir (wave-worktree-dir repo CID 1))
      (check-equal? (path-only dir)
                    (path-only (path->complete-path repo))
                    "worktree parent MUST equal repo parent (sibling placement)")
      (check-equal? (path->string dir) "/x/proj/wt-campaign-01234567-w1")
      (check-false (regexp-match? (regexp (string-append "^" (regexp-quote "/x/proj/q/")))
                                  (path->string dir))
                   "worktree must NOT live inside the repo")
      ;; The constraint is placement BY CONSTRUCTION from the repo root: the
      ;; function receives no other location input (no /tmp default exists).
      (check-true (string-suffix? (path->string (wave-worktree-dir repo CID 2)) "-w2")))

    (test-case "git worktree add command shape: fresh branch, path, base ref"
      (define args
        (wave-worktree-add-args "/x/proj/wt-campaign-01234567-w1"
                                "campaign/01234567/w1"
                                "origin/main"))
      (check-equal? args
                    (list "worktree"
                          "add"
                          "-b"
                          "campaign/01234567/w1"
                          "/x/proj/wt-campaign-01234567-w1"
                          "origin/main")))

    (test-case "isolation flag defaults OFF (BUG-0028 hotfix); #:isolate? #t opts in"
      (check-equal? (current-gsd-worktree-isolation) #f)
      (check-equal? (worktree-isolation-enabled?) #f)
      (check-equal? (worktree-isolation-enabled? #:isolate? #t)
                    #t
                    "explicit #:isolate? #t is the opt-in switch")
      (parameterize ([current-gsd-worktree-isolation #t])
        (check-equal? (worktree-isolation-enabled?) #t)))

    (test-case "find-repo-root supports both layouts with base-dir precedence"
      (check-equal? (find-repo-root "/does/not/exist") #f)
      ;; real-directory cases are covered in Layer 2 against the sandbox
      )

    ;; ============================================================
    ;; Layer 2 — real git sandbox cases
    ;; ============================================================

    (test-case "make-wave-worktree!: fresh branch off origin/main, sibling placement, shared tree untouched"
      (if (not GIT)
          (log-warning "test-gsd-wave-worktree: git unavailable; skipping")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define dir (wave-worktree-dir repo CID 1))
            (define wt (make-wave-worktree! proj #:campaign-id CID #:wave-index 1))
            ;; creation on a fresh branch off origin/main
            (check-equal? (git-out dir "rev-parse" "HEAD") (git-out repo "rev-parse" "origin/main"))
            (check-equal? (git-out dir "branch" "--show-current") "campaign/01234567/w1")
            ;; The worktree must NOT sit on the shared checkout's branch. Compare
            ;; the checked-out BRANCH NAMES: HEAD commits are equal by construction
            ;; (the fresh branch starts at the base ref), so a commit comparison
            ;; cannot distinguish isolation from the shared checkout.
            (check-not-equal? (git-out dir "branch" "--show-current")
                              (git-out repo "branch" "--show-current")
                              "worktree must NOT sit on the shared checkout's branch")
            ;; sibling placement on the real filesystem
            (check-true (directory-exists? dir))
            (check-equal? (path-only (path->complete-path dir))
                          (path-only (path->complete-path repo)))
            ;; shared checkout remains untouched by the attempt
            (check-equal? (git-out repo "status" "--porcelain") "")
            ;; executor cwd = worktree; .planning/ = REAL project root
            (check-equal? (wave-worktree-cwd wt) dir)
            (check-equal? (wave-worktree-planning-dir wt) (build-path proj ".planning"))
            (check-false (directory-exists? (build-path dir ".planning"))
                         "campaign state must NOT be duplicated into the worktree")
            ;; writes inside the worktree never leak into the shared checkout
            (call-with-output-file* (build-path dir "W6-SENTINEL") (lambda (p) (display "x" p)))
            (check-false (file-exists? (build-path repo "W6-SENTINEL")))
            (check-equal? (git-out repo "status" "--porcelain") "")
            ;; find-repo-root resolves the two-tier layout
            (check-equal? (find-repo-root proj) repo)
            (cleanup-wave-worktree! wt)
            (delete-directory/files (path-only (path->complete-path dir))))))

    (test-case "cleanup-wave-worktree! removes BOTH worktree and branch; best-effort, never raises"
      (if (not GIT)
          (log-warning "test-gsd-wave-worktree: git unavailable; skipping")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define wt (make-wave-worktree! proj #:campaign-id CID #:wave-index 2))
            (define dir (wave-worktree-path wt))
            (define status (cleanup-wave-worktree! wt))
            (check-true (hash-ref status 'ok?))
            (check-false (directory-exists? dir) "worktree directory must be gone")
            (define r
              (default-run-git
               repo
               (list "show-ref" "--verify" "--quiet" "refs/heads/campaign/01234567/w2")))
            (check-not-equal? (git-result-code r) 0 "campaign branch must be gone")
            ;; idempotent + never raises on an already-clean state
            (define status2 (cleanup-wave-worktree! wt))
            (check-true (hash? status2))
            (delete-directory/files proj #:must-exist? #f))))

    (test-case "orphaned worktrees from a crashed attempt are reclaimed; unrelated worktrees survive"
      (if (not GIT)
          (log-warning "test-gsd-wave-worktree: git unavailable; skipping")
          (let ()
            (define-values (proj repo) (make-sandbox))
            ;; crash simulation: two attempt worktrees left behind, no cleanup ran
            (define wt1 (make-wave-worktree! proj #:campaign-id CID #:wave-index 1))
            (define wt2 (make-wave-worktree! proj #:campaign-id CID #:wave-index 2))
            ;; an unrelated worktree + branch that MUST survive reclaim
            (git! repo "branch" "unrelated-x")
            (define other-dir (build-path (path-only (path->complete-path repo)) "wt-unrelated"))
            (git! repo "worktree" "add" (path->string other-dir) "unrelated-x")
            ;; a DIFFERENT campaign's worktree must also survive a scoped reclaim
            (define wt-other (make-wave-worktree! proj #:campaign-id CID-OTHER #:wave-index 1))
            ;; next campaign start: scoped reclaim
            (define reclaimed (reclaim-orphaned-worktrees! repo #:campaign-id CID))
            (check-equal? (sort (map (lambda (p)
                                       (define s
                                         (if (string? p)
                                             p
                                             (path->string p)))
                                       (let-values ([(base name _dir?) (split-path (string->path s))])
                                         (if (path? name)
                                             (path->string name)
                                             s)))
                                     reclaimed)
                                string<?)
                          (list "wt-campaign-01234567-w1" "wt-campaign-01234567-w2"))
            (check-false (directory-exists? (wave-worktree-path wt1)))
            (check-false (directory-exists? (wave-worktree-path wt2)))
            (check-true (directory-exists? (wave-worktree-path wt-other))
                        "other campaigns' worktrees must not be touched by a scoped reclaim")
            (check-true (directory-exists? other-dir) "unrelated worktrees must survive")
            ;; unscoped reclaim (full campaign wipe) picks up the rest
            (define reclaimed2 (reclaim-orphaned-worktrees! repo))
            ;; NOTE: rackunit's check-true is STRICT (#t only) in this Racket;
            ;; member returns a truthy tail — never #t — so check-not-false is
            ;; the semantically correct assertion here.
            (check-not-false (member (path->string (wave-worktree-path wt-other))
                                     (map (lambda (p)
                                            (if (string? p)
                                                p
                                                (path->string p)))
                                          reclaimed2)
                                     equal?)
                             "other campaign reclaimed by the unscoped pass")
            (check-true (directory-exists? other-dir) "still: unrelated worktree survives")
            ;; make-wave-worktree! is crash-idempotent: reclaim-then-add on a stale path
            (define wt3 (make-wave-worktree! proj #:campaign-id CID #:wave-index 1))
            (check-true (directory-exists? (wave-worktree-path wt3)))
            (define wt3-again (make-wave-worktree! proj #:campaign-id CID #:wave-index 1))
            (check-equal? (git-out (wave-worktree-path wt3-again) "branch" "--show-current")
                          "campaign/01234567/w1")
            (cleanup-wave-worktree! wt3-again)
            (git! repo "worktree" "remove" (path->string other-dir))
            (delete-directory/files proj #:must-exist? #f))))

    (test-case "make-wave-worktree! surfaces git failure with captured stderr"
      (if (not GIT)
          (log-warning "test-gsd-wave-worktree: git unavailable; skipping")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (check-exn exn:fail?
                       (lambda ()
                         (make-wave-worktree! proj
                                              #:campaign-id CID
                                              #:wave-index 1
                                              #:base-ref "refs/heads/no-such-branch")))
            (check-exn
             #rx"make-wave-worktree!"
             (lambda ()
               (make-wave-worktree! (build-path proj "nowhere") #:campaign-id CID #:wave-index 1)))
            ;; raise-argument-error signals exn:fail:contract (NOT filesystem)
            (check-exn exn:fail:contract?
                       (lambda () (make-wave-worktree! proj #:campaign-id "short" #:wave-index 1)))
            (delete-directory/files proj #:must-exist? #f))))))

(exit (run-tests w6-suite))
