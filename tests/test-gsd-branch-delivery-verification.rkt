#lang racket/base

;; q/tests/test-gsd-branch-delivery-verification.rkt — v1.00.17 W7 (#9512b)
;;
;; Branch-based delivery verification with worktree isolation ON:
;;   * delivery = COMMITTED diff of the wave branch vs its base commit
;;     (captured at attempt start) — never any working tree;
;;   * dirty-worktree-only changes are REJECTED (dirt cannot fake delivery);
;;   * the "no wave target files changed" failure message is byte-identical
;;     to the legacy path (so #9515 no-change retries keep working);
;;   * legacy path (context unbound = isolation OFF) is regression-proven
;;     identical: same checks, same messages, shared-tree evidence;
;;   * the coordinator commits uncommitted wave-branch changes before
;;     approval with the deterministic message feat(<hash8>/w<N>): …;
;;   * an empty branch diff at completion fires the existing no-change path;
;;   * on approval the branch name + head SHA are recorded in the durable
;;     campaign record (merge/PR stays OUTSIDE — no silent auto-merge);
;;   * the coordinator releases the worktree on every exit path, keeping the
;;     branch only when delivery was approved.
;;
;; Layer 1 cases are pure. Layer 2 cases run against a throwaway git repo
;; in a temp sandbox and are gated on git availability (logged + skipped,
;; never failed, when git is absent).

(require racket/file
         racket/format
         racket/list
         racket/path
         racket/set
         racket/string
         racket/system
         rackunit
         rackunit/text-ui
         "../extensions/gsd/wave-executor.rkt"
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-evidence
                  delivery-verification-message
                  current-gsd-delivery-verify-command
                  current-gsd-delivery-branch-context
                  make-branch-delivery-context
                  branch-delivery-context?
                  branch-delivery-context-ref
                  committed-branch-changed-files)
         (only-in "../extensions/gsd/plan-types.rkt"
                  gsd-plan
                  make-gsd-wave
                  plan-wave-ref
                  gsd-wave-files)
         (only-in "../extensions/gsd/campaign-state.rkt"
                  migrate-campaign!
                  campaign-record-waves
                  campaign-plan-id
                  campaign-wave-status
                  campaign-wave-delivery-branch
                  campaign-wave-delivery-head-sha)
         (only-in "../extensions/gsd/campaign-repository.rkt" load-campaign-record)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign-wave
                  campaign-result-status
                  campaign-result-message
                  wave-delivery-commit-message
                  commit-wave-worktree!))

(define GIT (find-executable-path "git"))

;; ============================================================
;; Layer 2 sandbox — mirrors production layout <tmp>/proj/{.planning,q}
;; ============================================================

(define (git! cwd . args)
  (define exit
    (parameterize ([current-directory cwd])
      (apply system*/exit-code GIT args)))
  (unless (zero? exit)
    (error 'sandbox-git "git ~a failed in ~a" (string-join args " ") cwd))
  exit)

(define (git-out cwd . args)
  (define o (open-output-string))
  (parameterize ([current-directory cwd]
                 [current-output-port o]
                 [current-error-port (open-output-string)])
    (apply system*/exit-code GIT args))
  (string-trim (get-output-string o)))

(define (write-file! path content)
  (make-directory* (path-only path))
  (call-with-output-file* path (lambda (p) (display content p)) #:exists 'truncate))

;; <tmp>/proj with .planning/ + a git repo at proj/q carrying a local
;; refs/heads/origin/main so offline worktree creation behaves like the
;; real tracking ref. Baseline commits the wave target file.
(define (make-sandbox)
  (define tmp (make-temporary-file "w7-~a" 'directory))
  (define proj (build-path tmp "proj"))
  (make-directory* (build-path proj ".planning" "waves"))
  (define repo (build-path proj "q"))
  (make-directory* (build-path repo "ui-core"))
  (git! repo "init" "-q" "-b" "main")
  (git! repo "config" "user.email" "w7@test.local")
  (git! repo "config" "user.name" "W7 Test")
  (write-file! (build-path repo "ui-core" "preferences.rkt")
               "#lang racket/base\n(provide foo)\n(define foo 1)\n")
  (git! repo "add" "-A")
  (git! repo "commit" "-q" "-m" "baseline")
  (git! repo "update-ref" "refs/heads/origin/main" "HEAD")
  (values proj repo))

(define (write-plan! proj idx title slug files)
  (write-file! (build-path proj ".planning" "PLAN.md")
               (string-append "# Plan: W7 test\n\n## Waves\n\n- [Inbox] W"
                              (number->string idx)
                              ": "
                              title
                              " → waves/W"
                              (number->string idx)
                              "-"
                              slug
                              ".md\n"))
  (write-file!
   (build-path proj ".planning" "waves" (string-append "W" (number->string idx) "-" slug ".md"))
   (string-append "# Wave "
                  (number->string idx)
                  "\nStatus: Inbox\n\n## Files\n"
                  (apply string-append
                         (for/list ([f files])
                           (string-append "- File: " f "\n")))
                  "\n## Verify\ntrue\n")))

(define (plan-for files)
  (gsd-plan (list (make-gsd-wave 0 "Wave Zero" "" files '() "verify" '("done"))) "" '() '()))

;; Context exactly as the coordinator builds it at attempt start.
(define (run-verify* proj plan ctx)
  (if ctx
      (parameterize ([current-gsd-delivery-branch-context ctx])
        (run-delivery-verification proj plan 0))
      (run-delivery-verification proj plan 0)))

(define NO-CHANGE-RX #rx"no wave target files changed")

(define (files-check v)
  (for/first ([c (in-list (delivery-verification-evidence v))]
              #:when (string=? (car c) "files"))
    (cdr c)))

(define (cleanup proj)
  (delete-directory/files proj #:must-exist? #f))

(define w7-suite
  (test-suite "branch-based delivery verification (v1.00.17 W7, #9512b)"

    ;; ---------------- Layer 1: pure ----------------

    (test-case "context: constructor/predicate/ref; hash8 commit message is deterministic"
      (define c
        (make-branch-delivery-context #:repo-root "/x/q"
                                      #:branch "campaign/01234567/w4"
                                      #:base-commit "abc123"
                                      #:worktree-path "/x/wt"))
      (check-true (branch-delivery-context? c))
      (check-equal? (branch-delivery-context-ref c 'branch) "campaign/01234567/w4")
      (check-equal? (branch-delivery-context-ref c 'base-commit) "abc123")
      (check-equal? (branch-delivery-context-ref c 'repo-root) "/x/q")
      (check-equal? (branch-delivery-context-ref c 'worktree-path) "/x/wt")
      (check-false (branch-delivery-context? (hasheq 'branch "b")))
      (check-false (branch-delivery-context? "not-a-hash"))
      ;; feat(<hash8>/w<N>): <title> — same inputs → same message, always
      (check-equal? (wave-delivery-commit-message
                     "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                     4
                     "Title X")
                    "feat(01234567/w4): Title X")
      (check-equal? (wave-delivery-commit-message "0123" 7 "T")
                    (wave-delivery-commit-message "0123" 7 "T")))

    (test-case "verify command default OFF: context unbound selects the legacy path"
      (check-false (current-gsd-delivery-branch-context)))

    ;; ---------------- Layer 2: real git sandbox ----------------

    (test-case "isolation ON: committed branch diff APPROVES delivery"
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping committed-diff approval")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define wt (make-wave-worktree! proj #:campaign-id "01234567deadbeef" #:wave-index 0))
            (define dir (wave-worktree-path wt))
            (define branch (wave-worktree-branch wt))
            ;; committed delivery on the wave branch (worktree clean afterwards)
            (write-file! (build-path dir "ui-core" "preferences.rkt")
                         "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n")
            (git! dir "add" "-A")
            (git! dir "commit" "-q" "-m" "wave delivery")
            (write-plan! proj 0 "Wave Zero" "zero" '("q/ui-core/preferences.rkt"))
            (define plan (plan-for '("q/ui-core/preferences.rkt")))
            (define ctx
              (make-branch-delivery-context #:repo-root repo
                                            #:branch branch
                                            #:base-commit (git-out repo "rev-parse" "origin/main")
                                            #:worktree-path dir))
            (define v
              (parameterize ([current-gsd-delivery-branch-context ctx])
                (run-delivery-verification proj plan 0)))
            (check-true (delivery-verification-approved? v) (delivery-verification-message v))
            (check-true (parameterize ([current-gsd-delivery-branch-context ctx])
                          (set-member? (committed-branch-changed-files) "ui-core/preferences.rkt"))
                        "committed diff must list the delivered file")
            (cleanup-wave-worktree! wt)
            (cleanup proj))))

    (test-case "isolation ON: dirty-worktree-only change is REJECTED (dirt cannot fake delivery)"
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping dirty-worktree rejection")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define wt (make-wave-worktree! proj #:campaign-id "01234567deadbeef" #:wave-index 0))
            (define dir (wave-worktree-path wt))
            (define branch (wave-worktree-branch wt))
            ;; UNCOMMITTED mutation only — no commit, branch diff stays empty
            (write-file! (build-path dir "ui-core" "preferences.rkt")
                         "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n")
            (write-plan! proj 0 "Wave Zero" "zero" '("q/ui-core/preferences.rkt"))
            (define plan (plan-for '("q/ui-core/preferences.rkt")))
            (define ctx
              (make-branch-delivery-context #:repo-root repo
                                            #:branch branch
                                            #:base-commit (git-out repo "rev-parse" "origin/main")
                                            #:worktree-path dir))
            (define v
              (parameterize ([current-gsd-delivery-branch-context ctx])
                (run-delivery-verification proj plan 0)))
            (check-false (delivery-verification-approved? v)
                         "uncommitted worktree change must NOT satisfy delivery")
            (check-not-false (regexp-match NO-CHANGE-RX (delivery-verification-message v)))
            ;; the same sandbox with the change COMMITTED flips to approved:
            ;; the rejection was caused by the missing commit, nothing else
            (git! dir "add" "-A")
            (git! dir "commit" "-q" "-m" "now committed")
            (define v2
              (parameterize ([current-gsd-delivery-branch-context ctx])
                (run-delivery-verification proj plan 0)))
            (check-true (delivery-verification-approved? v2) (delivery-verification-message v2))
            (cleanup-wave-worktree! wt)
            (cleanup proj))))

    (test-case "isolation ON: empty branch diff failure message is byte-identical to legacy"
      ;; The #9515 no-change retry keys on this exact message, so the branch
      ;; path must emit it verbatim.
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping message parity")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define wt (make-wave-worktree! proj #:campaign-id "01234567deadbeef" #:wave-index 0))
            (define dir (wave-worktree-path wt))
            (write-plan! proj 0 "Wave Zero" "zero" '("q/ui-core/preferences.rkt"))
            (define plan (plan-for '("q/ui-core/preferences.rkt")))
            (define ctx
              (make-branch-delivery-context #:repo-root repo
                                            #:branch (wave-worktree-branch wt)
                                            #:base-commit (git-out repo "rev-parse" "origin/main")
                                            #:worktree-path dir))
            (define v-branch
              (parameterize ([current-gsd-delivery-branch-context ctx])
                (run-delivery-verification proj plan 0)))
            (check-false (delivery-verification-approved? v-branch))
            (check-equal? (files-check v-branch)
                          (cons #f "no wave target files changed: q/ui-core/preferences.rkt"))
            ;; legacy path on a clean shared tree emits the SAME files detail
            (define v-legacy (run-delivery-verification proj plan 0))
            (check-equal? (files-check v-legacy)
                          (cons #f "no wave target files changed: q/ui-core/preferences.rkt"))
            (cleanup-wave-worktree! wt)
            (cleanup proj))))

    (test-case "isolation ON: branch check resolves the recorded campaign branch"
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping branch check")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define wt (make-wave-worktree! proj #:campaign-id "01234567deadbeef" #:wave-index 0))
            (define ctx
              (make-branch-delivery-context #:repo-root repo
                                            #:branch (wave-worktree-branch wt)
                                            #:base-commit (git-out repo "rev-parse" "origin/main")
                                            #:worktree-path (wave-worktree-path wt)))
            (define branch-check
              (parameterize ([current-gsd-delivery-branch-context ctx])
                (for/first ([c (in-list (delivery-verification-evidence
                                         (run-delivery-verification
                                          proj
                                          (plan-for '("q/ui-core/preferences.rkt"))
                                          0)))]
                            #:when (string=? (car c) "branch"))
                  (cdr c))))
            (check-true (car branch-check) (format "branch check failed: ~a" branch-check))
            (check-not-false (regexp-match #rx"isolated" (cdr branch-check)))
            ;; unknown branch ref → fail closed
            (define bad-ctx
              (make-branch-delivery-context #:repo-root repo
                                            #:branch "campaign/deadbeef/w9"
                                            #:base-commit (git-out repo "rev-parse" "origin/main")))
            (define bad-check
              (parameterize ([current-gsd-delivery-branch-context bad-ctx])
                (for/first ([c (in-list (delivery-verification-evidence
                                         (run-delivery-verification
                                          proj
                                          (plan-for '("q/ui-core/preferences.rkt"))
                                          0)))]
                            #:when (string=? (car c) "branch"))
                  (cdr c))))
            (check-false (car bad-check))
            (cleanup-wave-worktree! wt)
            (cleanup proj))))

    (test-case "isolation OFF (legacy): behavior identical without the branch context"
      ;; Same repo, same committed change, context unbound: the legacy path
      ;; must still approve — proving the default path is untouched.
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping legacy regression")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (git! repo "checkout" "-q" "-b" "feature/issue-42-wave")
            (write-file! (build-path repo "ui-core" "preferences.rkt")
                         "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n")
            (git! repo "add" "-A")
            (git! repo "commit" "-q" "-m" "legacy delivery")
            (write-plan! proj 0 "Wave Zero" "zero" '("q/ui-core/preferences.rkt"))
            (define plan (plan-for '("q/ui-core/preferences.rkt")))
            (check-false (current-gsd-delivery-branch-context))
            (define v (run-delivery-verification proj plan 0))
            (check-true (delivery-verification-approved? v) (delivery-verification-message v))
            (cleanup proj))))

    (test-case "commit-wave-worktree!: deterministic message, clean-tree no-op, hermetic identity"
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping auto-commit")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define wt (make-wave-worktree! proj #:campaign-id "01234567deadbeef" #:wave-index 0))
            (define dir (wave-worktree-path wt))
            ;; 1. clean worktree: nothing to commit is a no-op success
            (check-true (commit-wave-worktree! wt "01234567deadbeef" 0 "Wave Zero"))
            (check-equal? (git-out dir "rev-list" "--count" "HEAD")
                          (git-out repo "rev-list" "--count" "origin/main"))
            ;; 2. dirty worktree: coordinator auto-commits with the
            ;;    deterministic message so delivery becomes committed
            (write-file! (build-path dir "ui-core" "preferences.rkt")
                         "#lang racket/base\n(provide foo)\n(define foo 2)\n")
            (check-true (commit-wave-worktree! wt "01234567deadbeef" 0 "Wave Zero"))
            (check-equal? (git-out dir "log" "-1" "--format=%s") "feat(01234567/w0): Wave Zero")
            (check-equal? (git-out dir "status" "--porcelain") "" "tree clean after auto-commit")
            ;; 3. the auto-commit IS the delivery evidence the verifier reads
            (define ctx
              (make-branch-delivery-context #:repo-root repo
                                            #:branch (wave-worktree-branch wt)
                                            #:base-commit (git-out repo "rev-parse" "origin/main")
                                            #:worktree-path dir))
            (define plan (plan-for '("q/ui-core/preferences.rkt")))
            (define v
              (parameterize ([current-gsd-delivery-branch-context ctx])
                (run-delivery-verification proj plan 0)))
            (check-true (delivery-verification-approved? v) (delivery-verification-message v))
            (cleanup-wave-worktree! wt)
            (cleanup proj))))

    (test-case "isolation ON end-to-end: run-campaign-wave approves, records branch+SHA, keeps branch"
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping end-to-end coordinator case")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (write-plan! proj 0 "Wave Zero" "zero" '("q/ui-core/preferences.rkt"))
            (define rec (migrate-campaign! proj))
            (define plan (plan-for '("q/ui-core/preferences.rkt")))
            (define result
              (parameterize ([current-gsd-delivery-verify-command "true"])
                (run-campaign-wave
                 proj
                 rec
                 0
                 #:runner (lambda (idx)
                            ;; executor edits INSIDE its worktree and
                            ;; leaves the change uncommitted — the
                            ;; coordinator must commit it before approval
                            (define cwd (current-directory))
                            (write-file! (build-path cwd "ui-core" "preferences.rkt")
                                         "#lang racket/base\n(provide foo)\n(define foo 3)\n")
                            'done)
                 #:verifier (lambda (idx)
                              (parameterize ([current-gsd-delivery-verify-command "true"])
                                (run-delivery-verification proj plan idx)))
                 #:isolate? #t)))
            (check-equal? (campaign-result-status result) 'wave-done (campaign-result-message result))
            ;; durable record carries the delivery provenance. NOTE:
            ;; migrate-campaign! RE-SEEDS from PLAN.md and never reads the
            ;; persisted .rktd — reading delivery evidence through it would
            ;; always see #f/#"" (a previous run failed exactly this way).
            ;; The durable record is the source of truth: reload it the way
            ;; every coordinator path does (load-campaign-record).
            (define final (load-campaign-record proj (campaign-plan-id rec)))
            (define wave
              (and (pair? (campaign-record-waves final)) (first (campaign-record-waves final))))
            (check-not-false wave)
            (when wave
              (define expected-branch
                (format "campaign/~a/w0" (worktree-hash8 (campaign-plan-id final))))
              (check-equal? (campaign-wave-status wave) 'done)
              ;; W7 action 3: branch name + head SHA recorded in the
              ;; campaign record; merge/PR stays outside this flow
              (check-equal? (campaign-wave-delivery-branch wave) expected-branch)
              (check-equal? (campaign-wave-delivery-head-sha wave)
                            (git-out repo "rev-parse" expected-branch)))
            ;; shared checkout untouched by the isolated attempt
            (check-equal? (git-out repo "status" "--porcelain") "")
            (check-equal? (git-out repo "rev-parse" "HEAD")
                          (git-out repo "rev-parse" "origin/main")
                          "shared checkout HEAD must not move")
            ;; the delivered branch SURVIVES the release (kept as merge
            ;; evidence) and carries the committed wave change
            (define delivered
              (for/list ([b (string-split (git-out repo
                                                   "for-each-ref"
                                                   "--format=%(refname:short)"
                                                   "refs/heads/campaign/")
                                          "\n")]
                         #:when (non-empty-string? b))
                b))
            (check-true (pair? delivered) "approved delivery branch must be KEPT")
            (define tip (git-out repo "rev-parse" (car delivered)))
            (check-not-false (regexp-match #px"^[0-9a-f]{40}$" tip))
            (check-equal? (git-out repo "log" "-1" "--format=%s" (car delivered))
                          (wave-delivery-commit-message (campaign-plan-id rec) 0 "Wave Zero"))
            (check-not-false
             (regexp-match
              #rx"ui-core/preferences.rkt"
              (git-out repo
                       "diff"
                       "--name-only"
                       (string-append (git-out repo "rev-parse" "origin/main") "..." tip))))
            (cleanup proj))))

    (test-case "isolation ON end-to-end: zero-edit attempt fails with the no-change verdict"
      (if (not GIT)
          (log-warning "w7: git unavailable; skipping end-to-end no-change case")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (write-plan! proj 0 "Wave Zero" "zero" '("q/ui-core/preferences.rkt"))
            (define rec (migrate-campaign! proj))
            (define plan (plan-for '("q/ui-core/preferences.rkt")))
            (define result
              (parameterize ([current-gsd-delivery-verify-command "true"])
                (run-campaign-wave proj
                                   rec
                                   0
                                   #:runner (lambda (idx) 'done) ;; edits nothing
                                   #:verifier
                                   (lambda (idx)
                                     (parameterize ([current-gsd-delivery-verify-command "true"])
                                       (run-delivery-verification proj plan idx)))
                                   #:isolate? #t
                                   #:no-change-retries 0)))
            (check-eq? (campaign-result-status result) 'wave-failed)
            (check-not-false (regexp-match NO-CHANGE-RX (campaign-result-message result))
                             "empty branch diff must surface the honest no-change verdict")
            ;; rejected attempt: branch is cleaned up, none survive
            (check-equal?
             (git-out repo "for-each-ref" "--format=%(refname:short)" "refs/heads/campaign/")
             "")
            (cleanup proj))))))

(module+ main
  (void (run-tests w7-suite)))
(exit (run-tests w7-suite))
