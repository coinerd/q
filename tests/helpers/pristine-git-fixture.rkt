#lang racket/base

;; Pristine Git fixture (v1.00.28 W2 — fixture amplification reduction).
;;
;; Builds ONE immutable, fully self-contained baseline repository per process
;; (single commit, repo-local identity, no hooks, no alternates, no shared
;; refs) and hands out cheap isolated instances as physical byte-level copies
;; of that baseline. A copy is an independent object store: no hardlinks, no
;; alternates entries, no shared mutable refs, no inherited hooks.
;;
;; Activation contract: `make-private-git-fixture!` in
;; private-fixture-templates.rkt dispatches here when
;; `current-git-fixture-strategy` is 'pristine-copy; rollback is flipping that
;; parameter default back to 'legacy-clone (one edit).

(require racket/file
         racket/path
         racket/runtime-path
         racket/system
         (only-in "private-fixture-templates.rkt"
                  private-fixture
                  private-fixture-meta
                  allocate-unique-root!
                  git-quiet!
                  hermetic-identity!))

(provide pristine-git-baseline-root!
         make-pristine-fixture
         make-pristine-git-fixture-instance!
         pristine-git-instance!
         pristine-git-instance-dir)

;; Convenience alias + repo-path accessor used by the safety suite.
(define (pristine-git-instance! #:tag [tag "git"] #:branch [branch #f])
  (make-pristine-git-fixture-instance! #:tag tag #:branch branch))
(define (pristine-git-instance-dir fx)
  (hash-ref (private-fixture-meta fx) 'repo))

(define-runtime-path module-dir ".")

;; ---------------------------------------------------------------------------
;; Baseline construction (once per process)
;; ---------------------------------------------------------------------------

(define baseline-root #f)

(define (set-perms! p mode)
  ;; file-or-directory-permissions! is unavailable in the CI Racket; chmod is
  ;; a coreutils given everywhere git is.
  (define chmod (find-executable-path "chmod"))
  (unless (and chmod (zero? (system*/exit-code chmod (format "~a" mode) (path->string p))))
    (error 'pristine-git-fixture "chmod failed: ~a" p)))

(define (freeze-tree! root)
  ;; Read-only enforcement for the pristine baseline (defense in depth;
  ;; instance copies are re-chmodded writable after the byte copy).
  (for ([p (in-directory root)])
    (set-perms! p "a-w"))
  (set-perms! root "a-w"))

(define (build-pristine-baseline!)
  (define root (make-temporary-file "q-pristine-git-baseline-~a" 'directory))
  (define repo (build-path root "repo"))
  (git-quiet! root "init" "-q" (path->string repo))
  ;; Deterministic branch topology: exactly one branch `main`, matching the
  ;; legacy template's HEAD pinning, plus the offline `origin/main` stand-in
  ;; that worktree fixtures expect.
  (git-quiet! repo "symbolic-ref" "HEAD" "refs/heads/main")
  (call-with-output-file (build-path repo "README.md")
                         (lambda (out) (displayln "pristine git fixture baseline (immutable)" out))
                         #:exists 'truncate)
  ;; Strip template sample hooks so the pristine baseline (and every instance
  ;; copied from it) is genuinely hook-free per the W2 safety gate.
  (define hooks-dir (build-path repo ".git" "hooks"))
  (when (directory-exists? hooks-dir)
    (for ([h (directory-list hooks-dir)]
          #:when (path-has-extension? h #".sample"))
      (delete-file (build-path hooks-dir h))))
  (hermetic-identity! repo)
  (git-quiet! repo "add" "README.md")
  (git-quiet! repo "commit" "-q" "-m" "pristine baseline")
  (git-quiet! repo "update-ref" "refs/heads/origin/main" "HEAD")
  (with-output-to-file (build-path root ".pristine-ok")
                       (lambda () (displayln "pristine-git-fixture baseline v1"))
                       #:exists 'truncate)
  (freeze-tree! root)
  root)

(define (pristine-git-baseline-root!)
  (unless baseline-root
    (set! baseline-root (build-pristine-baseline!)))
  baseline-root)

;; ---------------------------------------------------------------------------
;; Isolated instances
;; ---------------------------------------------------------------------------

(define (thaw-tree! root)
  (set-perms! root "u+w")
  (for ([p (in-directory root)])
    (set-perms! p (if (directory-exists? p) "u+w" "u+w")))
  (set-perms! root "u+w"))

(define (make-pristine-git-fixture-instance! #:parent-root [parent-root #f]
                                             #:tag [tag "git"]
                                             #:branch [branch #f])
  (define base (pristine-git-baseline-root!))
  (define parent (or parent-root (make-temporary-file "q-fx-git-host-~a" 'directory)))
  (define root (allocate-unique-root! parent tag))
  (define repo (build-path root "repo"))
  ;; Physical, byte-level copy: every object file is an independent inode.
  (copy-directory/files (build-path base "repo") repo)
  (thaw-tree! repo)
  ;; Repo-local identity per instance (never inherited from the environment).
  (hermetic-identity! repo)
  (git-quiet! repo "update-ref" "refs/heads/origin/main" "HEAD")
  (when branch
    (git-quiet! repo "checkout" "-q" "-b" branch))
  (private-fixture 'git root (hash 'root root 'repo repo)))

(define (make-pristine-fixture #:tag [tag "git"] #:branch [branch #f])
  (make-pristine-git-fixture-instance! #:tag tag #:branch branch))
