#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/helpers/private-fixture-templates.rkt — Private copy-on-test fixture
;; templates for session and Git fixture families.
;;
;; Design contract (W6):
;; - Immutable templates live under tests/fixtures/ and are never mutated.
;; - Every fixture instance copies/clones into its own unique private root
;;   (atomic mkdtemp-style allocation), so two concurrent instances never
;;   share a canonical root, a mutable file, refs, index, or metadata.
;; - Git baseline repo is built lazily ONCE per process; every instance gets
;;   a real `git clone --no-local` (no hardlinks, no alternates, no shared
;;   objects) with an empty template dir (no inherited hooks) and repo-local
;;   hermetic identity (no global env mutation).
;; - Offline `refs/heads/origin/main` stand-in is recreated inside each
;;   clone's own .git, preserving GSD worktree semantics.
;; - Cleanup is idempotent and order-independent.
;; - When the git binary is unavailable, callers skip explicitly.

(require racket/bytes
         racket/file
         racket/format
         racket/path
         racket/runtime-path
         racket/string
         racket/system)

(provide private-fixture?
         private-fixture-root
         private-fixture-kind
         private-fixture-cleanup!
         make-private-session-fixture!
         make-private-git-fixture!
         private-git-fixture-repo
         private-session-fixture-session-dir
         private-session-fixture-session-id
         git-available?
         with-private-git-repo
         private-session-template-dir
         private-git-template-root!)

;; ---------------------------------------------------------------------------
;; Immutable template locations
;; ---------------------------------------------------------------------------

(define-runtime-path session-template-dir "../fixtures/session-template/")

(define (private-session-template-dir)
  session-template-dir)

;; Process-lazy git baseline template root (built on first git fixture).
(define git-template-root-box (box #f))

(define (private-git-template-root!)
  (unbox git-template-root-box))

;; ---------------------------------------------------------------------------
;; Unique private roots (atomic allocation)
;; ---------------------------------------------------------------------------

;; Allocate a unique directory under parent using filesystem atomicity:
;; make-temporary-file with 'directory uses an internal retry loop so two
;; concurrent allocations never collide or share a root.
(define (allocate-unique-root! parent tag)
  (make-temporary-file (string-append "q-fx-" tag "-~a") 'directory parent))

;; ---------------------------------------------------------------------------
;; Fixture handle
;; ---------------------------------------------------------------------------

(struct private-fixture (kind root meta) #:transparent)

(define (private-fixture-cleanup! fx)
  (define root (private-fixture-root fx))
  ;; Idempotent: #:must-exist? #f tolerates repeated/ordered cleanup.
  (when (path? root)
    (delete-directory/files root #:must-exist? #f))
  (void))

;; ---------------------------------------------------------------------------
;; Git availability
;; ---------------------------------------------------------------------------

(define (git-available?)
  (and (find-executable-path "git") #t))

;; Run git in dir, error on failure. Returns first line of stdout.
(define (git! dir . args)
  (define outp (open-output-string))
  (define errp (open-output-string))
  (define res
    (parameterize ([current-directory dir]
                   [current-output-port outp]
                   [current-error-port errp])
      (apply system*/exit-code (find-executable-path "git") args)))
  (unless (zero? res)
    (error 'git!
           "git ~a failed (~a): ~a"
           (string-join (map ~a args) " ")
           res
           (get-output-string errp)))
  (string-trim (get-output-string outp)))

(define (git-quiet! dir . args)
  (parameterize ([current-directory dir])
    (apply system*/exit-code (find-executable-path "git") args))
  (void))

;; ---------------------------------------------------------------------------
;; Lazy per-process baseline template repo
;; ---------------------------------------------------------------------------

(define (ensure-git-template!)
  (unless (unbox git-template-root-box)
    (define root (make-temporary-file "q-git-tmpl-~a" 'directory))
    (define repo (build-path root "baseline"))
    (git-quiet! root "init" "-q" (path->string repo))
    (hermetic-identity! repo)
    (with-output-to-file (build-path repo "baseline.txt")
                         (lambda () (displayln "template baseline"))
                         #:exists 'replace)
    (git-quiet! repo "add" "baseline.txt")
    (git-quiet! repo "commit" "-q" "--no-gpg-sign" "-m" "template baseline")
    ;; Offline stand-in: a branch literally named origin/main pointing at HEAD.
    (git-quiet! repo "update-ref" "refs/heads/origin/main" "HEAD")
    (set-box! git-template-root-box root))
  (unbox git-template-root-box))

;; Repo-local hermetic identity: never touches global env or ~/.gitconfig.
(define (hermetic-identity! repo)
  (git-quiet! repo "config" "user.name" "Q Fixture Bot")
  (git-quiet! repo "config" "user.email" "q-fixture@example.invalid")
  (git-quiet! repo "config" "commit.gpgsign" "false"))

;; ---------------------------------------------------------------------------
;; Private git fixture (clone of lazy baseline)
;; ---------------------------------------------------------------------------

;; Create a private clone of the per-process baseline template.
;; - `git clone --no-local` forces real transport: no hardlinks, no
;;   alternates, no shared object store.
;; - `--template=` (empty) prevents inheriting system hooks/info from
;;   /usr/share/git-core/templates.
;; - `refs/heads/origin/main` stand-in is recreated inside the clone's own
;;   .git so offline `worktree add <p> origin/main` keeps working.
;; Keyword `#:branch` creates and checks out an initial feature branch.
(define (make-private-git-fixture! #:parent-root [parent-root #f]
                                   #:tag [tag "git"]
                                   #:branch [branch #f])
  (unless (git-available?)
    (error 'make-private-git-fixture! "git unavailable"))
  (define tmpl (ensure-git-template!))
  (define parent (or parent-root (make-temporary-file "q-fx-git-host-~a" 'directory)))
  (define root (allocate-unique-root! parent tag))
  (define repo (build-path root "repo"))
  (git-quiet! root
              "clone"
              "-q"
              "--no-local"
              "--template="
              (path->string (build-path tmpl "baseline"))
              (path->string repo))
  (hermetic-identity! repo)
  (git-quiet! repo "update-ref" "refs/heads/origin/main" "HEAD")
  (when branch
    (git-quiet! repo "checkout" "-q" "-b" branch))
  (private-fixture 'git root (hash 'root root 'repo repo)))

(define (private-git-fixture-repo fx)
  (hash-ref (private-fixture-meta fx) 'repo))

;; Convenience: run thunk with a private git fixture, cleanup on exit
;; (normal or exceptional). Cleanup is idempotent.
(define (with-private-git-repo thunk
                               #:parent-root [parent-root #f]
                               #:tag [tag "git"]
                               #:branch [branch #f])
  (define fx (make-private-git-fixture! #:parent-root parent-root #:tag tag #:branch branch))
  (dynamic-wind (lambda () (void)) (lambda () (thunk fx)) (lambda () (private-fixture-cleanup! fx))))

;; ---------------------------------------------------------------------------
;; Private session fixture (copy of immutable session template)
;; ---------------------------------------------------------------------------

(define tmpl-session-id "tmpl-seed-0001")

(define session-counter (box 0))

(define (fresh-session-id!)
  (set-box! session-counter (add1 (unbox session-counter)))
  (format "sess-priv-~a-~a" (current-milliseconds) (unbox session-counter)))

;; Copy the immutable session template into a private root, allocate a fresh
;; session id, and rewrite the copied JSONL meta so no two instances share an
;; id or file bytes. The template itself is never written.
(define (make-private-session-fixture! #:parent-root [parent-root #f] #:tag [tag "session"])
  (define parent (or parent-root (make-temporary-file "q-fx-sess-host-~a" 'directory)))
  (define root (allocate-unique-root! parent tag))
  (define new-id (fresh-session-id!))
  (define src (build-path session-template-dir "session" tmpl-session-id))
  (define dst-dir (build-path root "session" new-id))
  (make-directory* (build-path root "session"))
  (copy-directory/files src dst-dir)
  (define jsonl (build-path dst-dir "session.jsonl"))
  (define text (file->string jsonl))
  (define rewritten (string-replace text tmpl-session-id new-id))
  (with-output-to-file jsonl (lambda () (write-string rewritten)) #:exists 'replace)
  (private-fixture 'session
                   root
                   (hash 'root root 'session-id new-id 'session-dir dst-dir 'jsonl jsonl)))

(define (private-session-fixture-session-dir fx)
  (hash-ref (private-fixture-meta fx) 'session-dir))

(define (private-session-fixture-session-id fx)
  (hash-ref (private-fixture-meta fx) 'session-id))
