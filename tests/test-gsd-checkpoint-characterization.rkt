#lang racket/base

;; @speed fast
;; @suite gsd
;; @boundary pure

;; q/tests/test-gsd-checkpoint-characterization.rkt
;;
;; BUG-0030 — W4 PIN FLIP (checkpoint commits + dirty-state hand-off)
;;
;; W0 pinned the BROKEN behavior (no checkpoint concept anywhere; an
;; infra-stopped attempt left only a dirty tree). W4 delivers the fix and
;; this file now pins the FIXED behavior:
;;
;;   1. Executor surface: CHECKPOINT-COMMIT-PREFIX / wave-checkpoint-
;;      commit-message / checkpoint-commit-message? / commit-wave-
;;      checkpoint! / checkpoint-contract-lines exist in wave-executor.
;;   2. Orchestrator surface: capture-worktree-dirty-state (dirty-sha /
;;      diff-stat / edited-files), append-dirty-capture-to-context (BUG-
;;      0024 PRIOR ATTEMPT CONTEXT reuse, 2 KB cap), outside-lease-dirty-
;;      warning (coordinator-side loud warning), warn-zero-commit-
;;      delivery-branch! (warn-only verifier tolerance).
;;   3. Schema decision: campaign-wave KEEPS its 9 fields — progress
;;      state rides the existing attempt-context (the BUG-0024 block),
;;      not a new struct slot.
;;   4. Git-level simulation: a mid-wave infra stop after a green step
;;      leaves a `checkpoint: <step summary>` commit on the delivery
;;      branch; a restart FINDS the committed progress; only the
;;      post-checkpoint edit is stranded (and is captured as pure data).
;;   5. Checkpoint commits are normal commits: they must NOT be confused
;;      with delivery commits by the verifier's commit classifier.
;;
;; Pure-level: temp git repos via git CLI + the real exported functions —
;; NO live worker subprocess, NO live campaign.

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         rackunit
         rackunit/text-ui
         "../extensions/gsd/campaign-state.rkt"
         (only-in "../extensions/gsd/wave-executor.rkt"
                  CHECKPOINT-COMMIT-PREFIX
                  wave-checkpoint-commit-message
                  checkpoint-commit-message?
                  commit-wave-checkpoint!
                  checkpoint-contract-lines
                  wave-worktree)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  capture-worktree-dirty-state
                  append-dirty-capture-to-context
                  outside-lease-dirty-warning
                  outside-lease-dirty-rkt-files
                  warn-outside-lease-dirty-state!
                  wave-branch-commit-count
                  warn-zero-commit-delivery-branch!))

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

;; Paths relative to THIS test file (not the invocation cwd).
(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))

(define here (simplify-path (build-path this-file 'up 'up)))

(define (repo-file . parts)
  (apply build-path (cons here parts)))

(define EXECUTOR-SRC (repo-file "extensions" "gsd" "wave-executor.rkt"))
(define ORCHESTRATOR-SRC (repo-file "extensions" "gsd" "go-orchestrator.rkt"))
(define HANDLERS-SRC (repo-file "extensions" "gsd" "command-handlers.rkt"))

(define GIT (find-executable-path "git"))

;; Run git in `dir` capturing stdout; return exit code.
(define (git* dir . args)
  (parameterize ([current-directory dir])
    (with-output-to-string (lambda ()
                             (parameterize ([current-error-port (open-output-nowhere)])
                               (apply system*/exit-code (cons GIT args)))))))

(define (git-lines dir . args)
  (define out (apply git* dir args))
  (filter non-empty-string? (string-split out "\n")))

(define (make-git-repo! base-name)
  (define repo (make-temporary-file (format "~a-~a" base-name "~a") 'directory))
  (git* repo "init" "-q")
  (git* repo "config" "user.email" "pin@localhost")
  (git* repo "config" "user.name" "pin")
  repo)

(define (git-commit-all! repo message)
  (git* repo "add" "-A")
  (git* repo "commit" "-q" "-m" message))

(define (write-file! repo name content)
  (call-with-output-file* (build-path repo name)
                          (lambda (o) (displayln content o))
                          #:exists 'replace))

;; ------------------------------------------------------------
;; Suite 1: surface + pure-function pins (flipped)
;; ------------------------------------------------------------

(define pure-suite
  (test-suite "BUG-0030 fixed behavior (pure): checkpoint surface + capture helpers"

    (test-case "campaign-wave has 10 fields: W5 adds the artifact ledger slot"
      ;; W4 DESIGN DECISION: dirty-state hand-off reuses the existing
      ;; attempt-context (BUG-0024 PRIOR ATTEMPT CONTEXT block) instead of
      ;; growing the struct (slot count stayed 9 through W4).
      ;; W5 (BUG-0029) later added exactly ONE slot: the attempt-
      ;; artifact ledger. The count must now stay 10 — any further growth is a
      ;; schema-review event.
      (define w (make-campaign-wave* 0 "W0" 'pending 0 #f))
      (check-equal? (vector-length (struct->vector w)) 10)
      (check-equal? (campaign-wave-attempt-context w) ""))

    (test-case "wave-executor exports the checkpoint contract surface"
      (define text (file->string EXECUTOR-SRC))
      (for ([sym (in-list '("CHECKPOINT-COMMIT-PREFIX" "wave-checkpoint-commit-message"
                                                       "checkpoint-commit-message?"
                                                       "commit-wave-checkpoint!"
                                                       "checkpoint-contract-lines"))])
        (check-true (and (regexp-match? (regexp-quote sym) text) #t)
                    (format "~a missing from wave-executor.rkt" sym))))

    (test-case "go-orchestrator exports the dirty-capture + warning surface"
      (define text (file->string ORCHESTRATOR-SRC))
      (for ([sym (in-list '("capture-worktree-dirty-state" "append-dirty-capture-to-context"
                                                           "outside-lease-dirty-warning"
                                                           "warn-outside-lease-dirty-state!"
                                                           "warn-zero-commit-delivery-branch!"))])
        (check-true (and (regexp-match? (regexp-quote sym) text) #t)
                    (format "~a missing from go-orchestrator.rkt" sym))))

    (test-case "wave prompt embeds the checkpoint contract (action 1)"
      (define text (file->string HANDLERS-SRC))
      (check-true (and (regexp-match? "checkpoint-contract-lines" text) #t)
                  "command-handlers.rkt must inject checkpoint-contract-lines into the wave prompt"))

    (test-case "checkpoint commit messages use the reserved prefix and classify cleanly"
      (check-equal? CHECKPOINT-COMMIT-PREFIX "checkpoint: ")
      (check-equal? (wave-checkpoint-commit-message "step 1: scaffold")
                    "checkpoint: step 1: scaffold")
      (check-true (checkpoint-commit-message? "checkpoint: step 1: scaffold"))
      (check-true (checkpoint-commit-message? "checkpoint: "))
      ;; Delivery commits (and anything else) are NOT checkpoints — the
      ;; verifier keeps its checkpoint/delivery distinction exact.
      (check-false (checkpoint-commit-message? "feat: step 1"))
      (check-false (checkpoint-commit-message? "checkpoint"))
      (check-false (checkpoint-commit-message? ""))
      (check-false (checkpoint-commit-message? #f)))

    (test-case "checkpoint-contract-lines describe cadence + non-verification semantics"
      (define lines (checkpoint-contract-lines))
      (check-true (and (list? lines) (pair? lines)))
      (for ([l (in-list lines)])
        (check-true (string? l)))
      (define joined (string-append* lines))
      (check-true (and (regexp-match? #rx"checkpoint: " joined) #t)
                  "contract must show the literal commit message prefix")
      (check-true (and (regexp-match? #rx"(?i:do(es)? not trigger delivery verification)" joined) #t)
                  "contract must state checkpoints never trigger delivery verification")
      (check-true (and (regexp-match? #rx"(?i:after each completed implementation step)" joined) #t)
                  "contract must state the cadence"))

    (test-case "append-dirty-capture-to-context: clean capture appends nothing"
      (define clean (hasheq 'dirty-sha #f 'diff-stat "" 'edited-files '()))
      (check-equal? (append-dirty-capture-to-context "PRIOR CONTEXT" clean) "PRIOR CONTEXT")
      (check-equal? (append-dirty-capture-to-context "PRIOR CONTEXT" #f) "PRIOR CONTEXT"))

    (test-case "append-dirty-capture-to-context: dirty capture joins the block, ~2 KB cap"
      (define dirty
        (hasheq 'dirty-sha
                "abc123def456"
                'diff-stat
                "3 files changed, 40 insertions(+)"
                'edited-files
                (list "a.rkt" "b.rkt" "c.rkt")))
      (define out (append-dirty-capture-to-context "PRIOR CONTEXT" dirty))
      (check-true (and (regexp-match? "Dirty state captured at infra-stop" out) #t))
      (check-true (and (regexp-match? "dirty-sha-if-committed: abc123def456" out) #t))
      (check-true (and (regexp-match? "diff-summary-stat: 3 files changed" out) #t))
      (check-true (and (regexp-match? "edited-files: a.rkt, b.rkt, c.rkt" out) #t))
      ;; ~2 KB cap (attempt-context-max-chars = 2048): a huge capture is
      ;; truncated, never blows the context budget.
      (define huge
        (hasheq 'dirty-sha
                (make-string 5000 #\a)
                'diff-stat
                (make-string 5000 #\b)
                'edited-files
                (build-list 200 (lambda (i) (format "file~a.rkt" i)))))
      (define huge-out (append-dirty-cirty-context-safe huge))
      (check-true (<= (string-length huge-out) 2048)
                  (format "capture must be capped at 2048 chars, got ~a" (string-length huge-out))))

    (test-case "untracked-only capture degrades to 'none' dirty-sha, files still named"
      (define untracked-only (hasheq 'dirty-sha #f 'diff-stat "" 'edited-files (list "stray.rkt")))
      (define out (append-dirty-capture-to-context "PRIOR" untracked-only))
      (check-true (and (regexp-match? (regexp-quote "none (clean or untracked-only residue)") out)
                       #t))
      (check-true (and (regexp-match? "stray.rkt" out) #t)))))

;; Guard helper: never blow up the suite if capture shape changes.
(define (append-dirty-cirty-context-safe capture)
  (append-dirty-capture-to-context "PRIOR" capture))

;; ------------------------------------------------------------
;; Suite 2: git-level simulation — mid-wave infra stop → restart
;; ------------------------------------------------------------

(define git-suite
  (test-suite "BUG-0030 fixed behavior (git): checkpoint commits survive the infra stop"

    (test-case "simulated infra stop: restart finds checkpoint commits, only tail edit stranded"

      (unless GIT
        (fail "git executable not found"))

      (define repo (make-temporary-file "bug30flip-~a" 'directory))

      (define (cleanup)
        (delete-directory/files repo #:must-exist? #f))

      (define (attempt-body)
        ;; Same shape the wave executor creates: base commit + delivery branch.
        (git* repo "init" "-q")
        (git* repo "config" "user.email" "pin@localhost")
        (git* repo "config" "user.name" "pin")
        (write-file! repo "base.rkt" "#lang racket/base\n;; wave base state")
        (git-commit-all! repo "wave-base")
        (define base-count (string->number (first (git-lines repo "rev-list" "--count" "HEAD"))))
        (git* repo "checkout" "-q" "-b" "delivery/w0")

        ;; --- Step 1 completes with green tests. The executor contract
        ;; (checkpoint-contract-lines) says: commit a checkpoint NOW, via
        ;; the real exported function.
        (write-file! repo "work.rkt" "#lang racket/base\n;; 40 minutes of in-flight wave work")
        (check-true (commit-wave-checkpoint! repo "step 1: work.rkt scaffold")
                    "checkpoint commit must succeed")

        ;; --- Step 2 is mid-edit when infra dies: NO checkpoint ran.
        (write-file! repo
                     "work2.rkt"
                     "#lang racket/base\n;; second step, in flight when provider dropped")

        ;; --- Infra stop happens HERE. The restart observes the branch:
        (define subjects (git-lines repo "log" "--format=%s"))
        (define tip-subject (first subjects))
        (define tip-count
          (string->number (first (git-lines repo "rev-list" "--count" "delivery/w0"))))
        (define dirty (git-lines repo "status" "--porcelain"))
        (define committed-work (apply git* repo (list "show" "HEAD:work.rkt")))

        ;; Restart FINDS the committed progress (the W4 headline).
        (check-equal? tip-subject
                      "checkpoint: step 1: work.rkt scaffold"
                      "delivery tip is the step-1 checkpoint commit")
        (check-equal? tip-count (add1 base-count) "exactly one checkpoint commit ahead of base")
        (check-true (pair? (filter checkpoint-commit-message? subjects))
                    "checkpoint classifier recognizes the commit on the branch")
        (check-true (non-empty-string? (string-trim committed-work)))
        (check-true (and (regexp-match? "in-flight wave work" committed-work) #t)
                    "step-1 work is recoverable from the branch, not stranded")

        ;; Only the post-checkpoint tail edit is stranded — and it is the
        ;; ONLY residue (one file), exactly what dirty capture records.
        (check-equal? dirty (list "?? work2.rkt") "only the post-checkpoint edit remains as residue")

        ;; wave-branch-commit-count (verifier-tolerance helper) sees N>=1.
        (check-equal? (wave-branch-commit-count repo "HEAD~1" "delivery/w0") 1))

      (dynamic-wind void attempt-body cleanup))

    (test-case "checkpoint on a clean tree is tolerated (idempotent, no failure)"

      (unless GIT
        (fail "git executable not found"))

      (define repo (make-temporary-file "bug30clean-~a" 'directory))
      (define (cleanup)
        (delete-directory/files repo #:must-exist? #f))
      (define (body)
        (git* repo "init" "-q")
        (git* repo "config" "user.email" "pin@localhost")
        (git* repo "config" "user.name" "pin")
        (write-file! repo "base.rkt" "#lang racket/base")
        (git-commit-all! repo "wave-base")
        ;; Nothing to commit: must not raise, must not fail the wave.
        (check-true (commit-wave-checkpoint! repo "step with nothing new")))
      (dynamic-wind void body cleanup))))

;; ------------------------------------------------------------
;; Suite 3: infra-stop dirty capture + outside-lease warning
;; ------------------------------------------------------------

(define dirty-suite
  (test-suite "BUG-0030 fixed behavior (capture): dirty state + coordinator warning"

    (test-case "infra-stopped worktree is captured as pure data"

      (unless GIT
        (fail "git executable not found"))

      (define repo (make-temporary-file "bug30cap-~a" 'directory))
      (define (cleanup)
        (delete-directory/files repo #:must-exist? #f))

      (define (body)
        (git* repo "init" "-q")
        (git* repo "config" "user.email" "pin@localhost")
        (git* repo "config" "user.name" "pin")
        (write-file! repo "tracked.rkt" "#lang racket/base\n(define v 1)")
        (git-commit-all! repo "wave-base")

        ;; Dying attempt: one tracked edit + one untracked stray file.
        (write-file! repo "tracked.rkt" "#lang racket/base\n(define v 2)")
        (write-file! repo "stray.rkt" "#lang racket/base\n;; in flight")
        ;; Non-.rkt noise must NOT appear in the outside-lease warning.
        (write-file! repo "note.txt" "ignore me")

        ;; The same worktree struct shape the orchestrator hands to
        ;; capture-worktree-dirty-state at infra-stop.
        (define wt (wave-worktree repo repo "delivery/w0" #f #f))
        (define capture (capture-worktree-dirty-state wt))

        (check-true (hash? capture))
        (check-true (and (hash-has-key? capture 'dirty-sha)
                         (hash-has-key? capture 'diff-stat)
                         (hash-has-key? capture 'edited-files))
                    "capture carries dirty-sha-if-committed / diff-stat / edited-files")
        ;; Tracked edit exists => `git stash create` yields a REAL sha.
        (check-true (and (string? (hash-ref capture 'dirty-sha))
                         (regexp-match? #px"^[0-9a-f]{40}$" (hash-ref capture 'dirty-sha)))
                    "dirty-sha-if-committed is a recoverable commit sha")
        (check-true (and (regexp-match? #rx"tracked\\.rkt" (hash-ref capture 'diff-stat "")) #t)
                    "diff-stat names the tracked edit")
        (check-true (and (member "tracked.rkt" (hash-ref capture 'edited-files '())) #t))
        (check-true (and (member "stray.rkt" (hash-ref capture 'edited-files '())) #t)
                    "untracked in-flight files are named too")

        ;; The capture joins the PRIOR ATTEMPT CONTEXT block.
        (define ctx (append-dirty-capture-to-context "PRIOR ATTEMPT CONTEXT" capture))
        (check-true (and (regexp-match? "dirty-sha-if-committed:" ctx) #t))
        (check-true (and (regexp-match? "edited-files: " ctx) #t))

        ;; --- Coordinator-side guard (action 4): the loud warning.
        (define warning (outside-lease-dirty-warning repo))
        (check-true (and (string? warning)) "dirty main checkout produces a warning")
        (check-true (and (regexp-match? "tracked.rkt" warning) #t)
                    (format "warning names the dirty .rkt file — warning was: ~s" warning))
        (check-true (and (regexp-match? "stray.rkt" warning) #t))
        (check-false (and (string? warning) (regexp-match? "note\\.txt" warning))
                     "non-.rkt noise is not flagged")
        (check-true (and (regexp-match? "UNCOMMITTED DRIFT" warning) #t)
                    "the warning is LOUD (prefixed)")
        ;; Effectful twin returns the same warning, never raises.
        (check-equal? (warn-outside-lease-dirty-state! repo) warning)

        ;; Exempting the attempt's own files silences exactly those.
        (check-false (and (regexp-match? "tracked\\.rkt"
                                         (outside-lease-dirty-warning repo
                                                                      #:exempt (list "tracked.rkt")))
                          #t)
                     "exempt files disappear from the warning")
        (check-false (outside-lease-dirty-warning repo #:exempt (list "tracked.rkt" "stray.rkt"))
                     "fully-exempted dirty state produces no warning")

        ;; The raw file list helper only reports .rkt paths. Git's porcelain
        ;; order is tracked-changes-first, untracked-last — assert membership,
        ;; not ordering (ordering is a git implementation detail).
        (define listed (outside-lease-dirty-rkt-files repo))
        (check-true (and (= (length listed) 2)
                         (and (member "tracked.rkt" listed) #t)
                         (and (member "stray.rkt" listed) #t))
                    (format "only .rkt paths listed: ~s" listed))
        (check-false (and (member "note.txt" listed) #t))

        ;; Clean tree: no warning at all.
        (git-commit-all! repo "absorb everything")
        (check-false (outside-lease-dirty-warning repo) "clean checkout never warns"))

      (dynamic-wind void body cleanup))))

(module+ main
  (define failures (+ (run-tests pure-suite) (run-tests git-suite) (run-tests dirty-suite)))
  (exit failures))
