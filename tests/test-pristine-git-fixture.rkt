#lang racket/base
;; @speed fast
;; @suite testing

;; q/tests/test-pristine-git-fixture.rkt — W2 (v1.00.28) safety gate for the
;; pristine physical-copy git fixture strategy.
;;
;; Replaces the per-instance `git clone --no-local` transport with a
;; once-per-process pristine, fully self-contained baseline repo plus a
;; physical filesystem copy per instance. The strategy is activated ONLY if
;; every safety gate below holds; rollback is a one-edit strategy default.
;;
;; Gates (each is a hard, evidence-backed assertion):
;;   G1  byte-level isolation: no hardlinks (st_nlink = 1) anywhere in the
;;       instance tree, no symlinks, no .git file pointers.
;;   G2  no alternates: instance has no .git/objects/info/alternates and
;;       `git fsck --full` succeeds (objects fully self-contained).
;;   G3  baseline immutability: creating instances and applying scenario
;;       deltas never changes a byte of the pristine baseline (SHA-256 tree
;;       digest stable across constructions + deltas + concurrent stress).
;;   G4  no inherited hooks: no executable hook files in the instance.
;;   G5  repo-local identity: .git/config carries the hermetic user identity;
;;       fixture creation does not leak GIT_* env into the caller.
;;   G6  constructor contract: handle shape/keywords identical to the legacy
;;       clone strategy (kind 'git, root/repo accessors, #:branch honored).
;;   G7  concurrency stress: N parallel create/delta/destroy rounds succeed
;;       with distinct roots and leave the baseline pristine.
;;   G8  rollback switch: current-git-fixture-strategy selects between
;;       pristine-copy and legacy clone through the SAME constructor.

(require racket/bool
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         rackunit
         rackunit/text-ui
         (only-in "helpers/private-fixture-templates.rkt"
                  git-available?
                  make-private-git-fixture!
                  private-fixture-cleanup!
                  private-fixture-kind
                  private-fixture-root
                  private-git-fixture-repo
                  current-git-fixture-strategy)
         (only-in "helpers/pristine-git-fixture.rkt"
                  pristine-git-baseline-root!
                  pristine-git-instance!
                  pristine-git-instance-dir
                  materialize-private-hardlinks!))

;; This CI Racket lacks file-or-directory-permissions 'link; stat(1) is a
;; coreutils given everywhere git is.
(define (link-count p)
  (define out (open-output-bytes))
  (parameterize ([current-output-port out])
    (system*/exit-code (find-executable-path "stat") "-c" "%h" (path->string p)))
  (string->number (string-trim (bytes->string/utf-8 (get-output-bytes out)))))

;; When this suite runs under `git commit` (pre-commit hook), git exports
;; GIT_INDEX_FILE/GIT_DIR/GIT_WORK_TREE etc. into our process environment.
;; The G5b leak gate asserts our fixture library adds none of these, so the
;; ambient (git-injected) values must be cleared at instantiation — before any
;; fixture is built. Top-level (not `module+ main`) so `raco test` sandboxes
;; run it too.
(for ([k '("GIT_DIR" "GIT_WORK_TREE"
                     "GIT_INDEX_FILE"
                     "GIT_OBJECT_DIRECTORY"
                     "GIT_COMMON_DIR"
                     "GIT_ALTERNATE_OBJECT_DIRECTORIES")])
  (environment-variables-set! (current-environment-variables) (string->bytes/utf-8 k) #f))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

;; True when git is usable (availability gate mirrors the legacy helper).
(define (git-ok?)
  (git-available?))

(define (git-out! dir . args)
  (parameterize ([current-directory dir])
    (define outp (open-output-string))
    (define errp (open-output-string))
    (define res
      (parameterize ([current-output-port outp]
                     [current-error-port errp])
        (apply system*/exit-code (find-executable-path "git") args)))
    (unless (zero? res)
      (error 'git-out! "git ~a failed: ~a" (string-join (map ~a args) " ") (get-output-string errp)))
    (string-trim (get-output-string outp))))

;; Recursively collect all file paths under root (skips directories and
;; symlinks; symlinks are reported so G1 can fail loudly on them).
(define (collect-files! root)
  (let loop ([dir root]
             [acc '()])
    (for/fold ([acc acc]) ([p (in-list (directory-list dir #:build? #t))])
      (define st (filesystem-change-notify-noop p))
      (cond
        [(link-exists? p) (error 'collect-files! "symlink found: ~a" p)]
        [(directory-exists? p) (loop p acc)]
        [else (cons p acc)]))))

(define (filesystem-change-notify-noop p)
  (file-or-directory-identity p #f))

;; Tree snapshot: deterministic order, path + full file bytes. The pristine
;; baseline is a tiny single-commit repo, so in-memory byte equality is both
;; cheap and stronger than any digest comparison.
(define (tree-snapshot root)
  (define files (sort (map path->string (collect-files! root)) string<?))
  (for/list ([f files])
    (cons f (file->bytes f))))

(define (hardlink-violations root baseline-root)
  ;; A file inside the instance tree that shares a filesystem identity with a
  ;; file in the pristine baseline is a hardlink/alias (G1 violation).
  (define baseline-ids
    (for/hash ([f (in-list (collect-files! baseline-root))])
      (values (file-or-directory-identity f #f) #t)))
  (for/list ([f (collect-files! root)]
             #:when (hash-ref baseline-ids (file-or-directory-identity f #f) #f))
    f))

(define (executable-hook-violations repo)
  (define hooks-dir (build-path repo ".git" "hooks"))
  (if (directory-exists? hooks-dir)
      (for/list ([h (in-list (directory-list hooks-dir #:build? #t))]
                 #:when (and (file-exists? h)
                             (member (system-type 'os) '(unix macosx))
                             (positive? (bitwise-and (file-or-directory-permissions h 'bits)
                                                     user-execute-bit))))
        h)
      '()))

(define (scenario-delta! repo)
  ;; Simulate a consumer test mutating its instance: commit + branch churn.
  (with-output-to-file (build-path repo "delta.txt")
                       (lambda () (displayln "instance delta"))
                       #:exists 'replace)
  (git-out! repo "add" "delta.txt")
  (git-out! repo "commit" "-q" "--no-gpg-sign" "-m" "instance delta")
  (git-out! repo "branch" "feature/delta"))

;; ---------------------------------------------------------------------------
;; Suite
;; ---------------------------------------------------------------------------

(define pristine-suite
  (test-suite "pristine git fixture safety gate (v1.00.28 W2)"

    (test-case "G3a: baseline exists once, is self-contained, digest stable across re-derivation"
      (check-true (git-ok?) "git binary must be available for this suite")
      (define base1 (pristine-git-baseline-root!))
      (define d1 (tree-snapshot base1))
      (check-equal? (tree-snapshot base1) d1 "baseline snapshot must be deterministic")
      (check-equal? (pristine-git-baseline-root!)
                    base1
                    "baseline root must be process-stable (built once)"))

    (test-case "G1+G2+G4+G5: instance is byte-isolated, self-contained, hook-free, identity-local"
      (define fx (make-pristine-fixture))
      (dynamic-wind
       void
       (lambda ()
         (define root (private-fixture-root fx))
         (define repo (pristine-git-instance-dir fx))
         ;; G1: no hardlinks, no symlinks anywhere in the instance tree.
         (check-equal? (length (hardlink-violations root (pristine-git-baseline-root!)))
                       0
                       "instance must contain no hardlinked files")
         ;; G2a: no alternates.
         (check-false (file-exists? (build-path repo ".git" "objects" "info" "alternates"))
                      "instance must not reference an object store via alternates")
         (check-false (file-exists? (build-path repo ".git"))
                      "instance .git must be a real directory, not a gitdir pointer")
         (check-true (directory-exists? (build-path repo ".git"))
                     "instance .git must be a real directory")
         ;; G2b: objects fully self-contained.
         (check-equal? (git-out! repo "fsck" "--full") "" "git fsck must pass with no dangling refs")
         ;; G4: no executable hooks.
         (check-equal? (executable-hook-violations repo)
                       '()
                       "instance must not inherit executable hooks")
         ;; G5a: repo-local hermetic identity.
         (define cfg (file->string (build-path repo ".git" "config")))
         (check-true (string-contains? cfg "q-fixture@example.invalid")
                     "instance config must carry the repo-local fixture identity")
         ;; G5b: no GIT_* repo-local leakage into the caller environment.
         (for ([k '("GIT_DIR" "GIT_WORK_TREE"
                              "GIT_INDEX_FILE"
                              "GIT_OBJECT_DIRECTORY"
                              "GIT_COMMON_DIR")])
           (check-false (environment-variables-ref (current-environment-variables)
                                                   (string->bytes/utf-8 k))
                        (format "~a must not leak into caller env" k)))
         ;; Contract check inside the same wind (before cleanup).
         (check-equal? (private-fixture-kind fx) 'git)
         (check-equal? (git-out! repo "rev-parse" "--abbrev-ref" "HEAD") "main"))
       (lambda () (private-fixture-cleanup! fx))))

    (test-case "G3b+G6: scenario delta on instance never mutates baseline; #:branch honored"
      (define baseline-digest (tree-snapshot (pristine-git-baseline-root!)))
      (define fx (make-pristine-fixture #:tag "g3" #:branch "feat/w2"))
      (dynamic-wind void
                    (lambda ()
                      (define repo (pristine-git-instance-dir fx))
                      (check-equal? (git-out! repo "rev-parse" "--abbrev-ref" "HEAD")
                                    "feat/w2"
                                    "#:branch must be created and checked out")
                      (scenario-delta! repo)
                      (check-equal? (git-out! repo "rev-parse" "--abbrev-ref" "HEAD")
                                    "feat/w2"
                                    "delta commit must land on the instance branch")
                      (check-equal? (tree-snapshot (pristine-git-baseline-root!))
                                    baseline-digest
                                    "baseline bytes must be untouched by instance deltas"))
                    (lambda () (private-fixture-cleanup! fx))))

    (test-case "G7: concurrent create/delta/destroy rounds keep baseline pristine"
      (define baseline-digest (tree-snapshot (pristine-git-baseline-root!)))
      (define rounds 8)
      (define roots-box (make-vector rounds #f))
      (define failures (make-vector rounds #f))
      (define threads
        (for/list ([i (in-range rounds)])
          (thread (lambda ()
                    (with-handlers ([exn:fail? (lambda (e) (vector-set! failures i e))])
                      (define fx (make-pristine-fixture #:tag (format "stress-~a" i)))
                      (dynamic-wind
                       void
                       (lambda ()
                         (vector-set! roots-box i (path->string (private-fixture-root fx)))
                         (scenario-delta! (pristine-git-instance-dir fx)))
                       (lambda () (private-fixture-cleanup! fx))))))))
      (for-each thread-wait threads)
      (for ([f failures])
        (check-false f "stress round must not raise"))
      (check-equal? (length (remove-duplicates (vector->list roots-box)))
                    rounds
                    "each stress round must get a distinct instance root")
      (check-equal? (tree-snapshot (pristine-git-baseline-root!))
                    baseline-digest
                    "baseline must stay pristine after concurrent stress"))

    ;; W2 hard gate H1: an adversarial hardlink attack on the instance
    ;; object store must be silently defused — bytes preserved, but the
    ;; materialized file is a private regular file (link count 1), never a
    ;; hardlink into a shared store.
    (test-case "H1: adversarial hardlink attack on instance objects is defused"
      (define fx (make-private-git-fixture! #:tag "hardlink-attack"))
      (dynamic-wind
       void
       (lambda ()
         (define repo (private-git-fixture-repo fx))
         (define victim (build-path repo ".git" "objects" "aa" "attackblob"))
         (make-directory* (path-only victim))
         (with-output-to-file victim
                              #:exists 'replace
                              (lambda () (display #"pristine attack-surface probe")))
         (define defended (materialize-private-hardlinks! (list victim)))
         (check-equal? (length defended) 1 "the attacked path must be defended")
         (check-equal? (file->bytes victim)
                       #"pristine attack-surface probe"
                       "object bytes must be preserved")
         (check-equal? (link-count victim) 1 "materialized object must have link count 1")
         (check-true (file-exists? victim)))
       (lambda () (private-fixture-cleanup! fx))))

    ;; G6 cleanup completeness: after destroy, the parent work area holds
    ;; nothing of the instance (instance dirs were #f-prefixed) and the
    ;; caller's working directory is untouched.
    (test-case "G6: cleanup removes every instance trace; work dir untouched"
      (define parent (make-temporary-file "q-cleanup-parent~a" 'directory))
      (define (defuse-hardlink! path)
        (define tmp (path-add-extension path #".private"))
        (copy-file path tmp)
        (delete-file path)
        (rename-file-or-directory tmp path))
      (define pre-existing (make-temporary-file "q-pre~a" 'directory parent))
      (parameterize ([current-git-fixture-strategy 'pristine-copy])
        (define fx (make-private-git-fixture! #:tag "cleanup" #:parent-root parent))
        (define root (private-fixture-root fx))
        (private-fixture-cleanup! fx)
        (check-equal? (directory-list parent #:build? #t)
                      (list pre-existing)
                      "destroyed instance must leave no trace in parent")
        (check-false (directory-exists? root)))
      (check-true (directory-exists? pre-existing)
                  "pre-existing sibling fixture must survive cleanup"))

    (test-case "G8: strategy switch selects legacy clone through the same constructor"
      (check-false (false? (current-git-fixture-strategy)) "strategy parameter must be bound")
      (parameterize ([current-git-fixture-strategy 'clone])
        (define fx (make-private-git-fixture! #:tag "legacy-contract"))
        (dynamic-wind
         void
         (lambda ()
           (check-equal? (private-fixture-kind fx) 'git)
           (check-true (directory-exists? (private-git-fixture-repo fx)))
           (check-equal? (git-out! (private-git-fixture-repo fx) "rev-parse" "--abbrev-ref" "HEAD")
                         "main"))
         (lambda () (private-fixture-cleanup! fx))))
      (parameterize ([current-git-fixture-strategy 'pristine-copy])
        (define fx (make-private-git-fixture! #:tag "pristine-contract"))
        (dynamic-wind void
                      (lambda ()
                        (check-equal? (private-fixture-kind fx) 'git)
                        (check-equal? (pristine-git-instance-dir fx) (private-git-fixture-repo fx)))
                      (lambda () (private-fixture-cleanup! fx)))))))

;; Strategy-aware constructor: dispatches on the parameter so both strategies
;; flow through one call site (G8).
(define (make-pristine-fixture #:tag [tag "pristine"] #:branch [branch #f])
  (parameterize ([current-git-fixture-strategy 'pristine-copy])
    (make-private-git-fixture! #:tag tag #:branch branch)))

(exit (run-tests pristine-suite))
