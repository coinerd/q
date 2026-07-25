#lang racket/base

;; runtime/memory/project-identity.rkt
;; STABILITY: evolving
;;
;; W5A (#8942): Canonical project identity and durable project memory
;; scoping. Derives a stable, normalized project identity from a
;; repository/project root so that memory (facts, conclusions) can be
;; physically isolated per project in a user-data location.
;;
;; CONTRACT (per W5 issue):
;;   - Canonical identity derives from normalized repository/project root.
;;   - Handles: symlink, Git worktree, nested repo, submodule, relocated
;;     checkout, non-Git directory, Windows drive/case/UNC.
;;   - FAIL CLOSED: if identity is unavailable, returns #f; NEVER falls
;;     back to "." or a session log path.
;;   - Physical project memory lives in a project-keyed USER DATA location,
;;     not the repository or session directory.
;;
;; DESIGN: Pure logic (canonicalization, keying, aliasing, namespace safety)
;; is separated from the I/O layer (filesystem walk + git subprocess) so the
;; core invariants are exhaustively testable without side effects.
;;
;; Layering: RUNTIME module. Self-contained: only stdlib + file/sha1.

(require racket/contract
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         file/sha1)

;; Struct
(provide project-identity?
         project-identity-key
         project-identity-root
         project-identity-kind
         project-identity-origin-url
         project-identity-common-dir
         make-project-identity
         ;; Pure canonicalization
         canonicalize-origin-url
         ;; Pure gitdir logic
         parse-gitdir-entry
         classify-gitdir-path
         worktree-common-dir-from-gitdir
         ;; Pure keying
         compute-project-key
         ;; Pure aliasing
         same-project?
         ;; Pure storage path
         project-memory-subdir
         ;; Namespace safety
         namespace-safe?
         quarantine-reason
         ;; I/O entrypoint (fail-closed)
         derive-project-identity)

;; ============================================================
;; Struct
;; ============================================================

;; kind ∈ {git-main, git-worktree, git-submodule, plain-directory}
(struct project-identity
        (key ; string — stable canonical key (never "." or session path)
         root ; string — normalized absolute project root path
         kind ; symbol — repo kind
         origin-url ; (or/c string? #f) — raw git origin URL
         common-dir) ; (or/c string? #f) — shared .git common dir (for worktree aliasing)
  #:transparent)

;; Validated constructor. key must be a non-empty safe string.
(define (make-project-identity #:key key
                               #:root root
                               #:kind kind
                               #:origin-url origin-url
                               #:common-dir common-dir)
  (unless (and (string? key) (> (string-length key) 0))
    (error 'make-project-identity "key must be non-empty string, given: ~e" key))
  (unless (memq kind '(git-main git-worktree git-submodule plain-directory))
    (error 'make-project-identity "invalid kind: ~e" kind))
  (project-identity key root kind origin-url common-dir))

;; ============================================================
;; Origin URL canonicalization (pure)
;; ============================================================

;; Normalizes a git remote URL to a canonical project coordinate so that
;; aliases (ssh vs https, trailing .git, trailing /, host case) collapse.
;;   "git@github.com:u/r.git"     → "github.com/u/r"
;;   "https://github.com/u/r.git" → "github.com/u/r"
;;   "https://GitHub.COM/U/R/"    → "github.com/u/r"
;; Returns #f for empty/whitespace input.
(define (canonicalize-origin-url url)
  (cond
    [(not (string? url)) #f]
    [(string=? (string-trim url) "") #f]
    [else
     (define s (string-trim url))
     (define ssh-parsed (parse-ssh-url s))
     (define host+path (or ssh-parsed (parse-https-url s) (parse-protocol-url s)))
     (cond
       [host+path
        ;; Identity keying collapses casing variants of the full coordinate
        ;; (host + org/repo) so aliases like GitHub.COM/U/R and github.com/u/r
        ;; resolve to the same project memory.
        (define full (string-append (car host+path) "/" (cdr host+path)))
        (string-downcase (normalize-origin-path full))]
       ;; unparseable: return trimmed original (still usable as identity)
       [else s])]))

;; "git@host:path" → (host . path)
(define (parse-ssh-url s)
  (define m (regexp-match #rx"^([^@]+)@([^:]+):(.*)$" s))
  (and m
       ;; ignore the user part; use host:path
       (cons (caddr m) (cadddr m))))

;; "https://host/path" → (host . path)
(define (parse-https-url s)
  (define m (regexp-match #rx"^https?://([^/]+)/(.*)$" s))
  (and m (cons (cadr m) (caddr m))))

;; "git://host/path" or "ssh://host/path" → (host . path)
(define (parse-protocol-url s)
  (define m (regexp-match #rx"^[a-z]+://([^/]+)/(.*)$" s))
  (and m (cons (cadr m) (caddr m))))

;; Strip trailing .git and trailing slashes from the path component.
(define (normalize-origin-path p)
  (define no-git
    (if (string-suffix? p ".git")
        (substring p 0 (- (string-length p) 4))
        p))
  (string-trim no-git "/"))

;; ============================================================
;; Gitdir file parsing & classification (pure)
;; ============================================================

;; Parses the content of a `.git` file (worktree/submodule pointer).
;; Returns the referenced path string, or #f.
(define (parse-gitdir-entry content)
  (cond
    [(not (string? content)) #f]
    [else
     (define m (regexp-match #px"^gitdir:\\s*(\\S+)" content))
     (and m (cadr m))]))

;; Classify a gitdir path + whether it's a directory or file.
;;   "/proj/.git"            directory → git-main
;;   "/proj/.git/worktrees/x" directory → git-worktree
;;   "/proj/.git/modules/x"  directory → git-submodule
;; If it's a FILE, it's a pointer (caller should follow it).
(define (classify-gitdir-path path-str file-type)
  (cond
    [(eq? file-type 'file) 'git-pointer]
    [(or (string-contains? path-str "/worktrees/")
         (regexp-match? #rx"[/\\\\]worktrees[/\\\\]" path-str))
     'git-worktree]
    [(or (string-contains? path-str "/modules/") (regexp-match? #rx"[/\\\\]modules[/\\\\]" path-str))
     'git-submodule]
    [else 'git-main]))

;; Given a worktree's gitdir path "/main/.git/worktrees/wt", return the
;; common dir "/main/.git". Returns #f if not a worktree path.
(define (worktree-common-dir-from-gitdir gitdir-str)
  (define parts (regexp-split #rx"[/\\\\]" gitdir-str))
  (define wt-idx
    (for/first ([p (in-list parts)]
                [i (in-naturals)]
                #:when (equal? p "worktrees"))
      i))
  (cond
    [(and wt-idx (> wt-idx 0))
     (define common-parts (take parts wt-idx))
     (string-join common-parts "/")]
    [else #f]))

;; ============================================================
;; Key computation (pure, deterministic)
;; ============================================================

;; Computes the canonical project key. Priority:
;;   1. canonicalized origin URL (stable across relocation/worktree)
;;   2. common git dir (stable within a repo, incl. worktrees)
;;   3. normalized root path (plain directory; relocation-fragile)
;; The result is a hex sha1 digest; never "." or a session path.
(define (compute-project-key #:origin-url origin-url #:root root #:common-dir common-dir)
  (define canon (and origin-url (canonicalize-origin-url origin-url)))
  (define raw
    (cond
      [canon (string-append "origin:" canon)]
      [common-dir (string-append "gitdir:" common-dir)]
      [(and (string? root) (not (string=? root "")))
       (string-append "dir:" (normalize-path-string root))]
      [else (error 'compute-project-key "no identity signal available")]))
  (sha1 (open-input-string raw)))

;; Normalize a path string for keying: collapse repeated separators,
;; strip trailing separator, but keep case (cross-platform roots vary).
(define (normalize-path-string s)
  (define no-backslash (string-replace s "\\" "/"))
  (define collapsed (regexp-replace* #rx"/+" no-backslash "/"))
  (string-trim collapsed "/"))

;; ============================================================
;; Worktree aliasing (pure)
;; ============================================================

;; Two identities refer to the same project iff:
;;   - they share a canonical origin URL, OR
;;   - they share a common git dir (main repo + its linked worktrees), OR
;;   - their keys are equal.
;; Submodules have a distinct origin/common-dir and do NOT alias to parent.
(define (same-project? id1 id2)
  (unless (and (project-identity? id1) (project-identity? id2))
    (error 'same-project? "expected two project-identity structs"))
  (define o1
    (and (project-identity-origin-url id1)
         (canonicalize-origin-url (project-identity-origin-url id1))))
  (define o2
    (and (project-identity-origin-url id2)
         (canonicalize-origin-url (project-identity-origin-url id2))))
  (define c1 (project-identity-common-dir id1))
  (define c2 (project-identity-common-dir id2))
  (cond
    [(and o1 o2 (string=? o1 o2)) #t]
    [(and c1 c2 (string=? c1 c2)) #t]
    [(string=? (project-identity-key id1) (project-identity-key id2)) #t]
    [else #f]))

;; ============================================================
;; Storage path derivation (pure)
;; ============================================================

;; Derives the project-keyed memory subdirectory under a user-data base dir.
;; Sharded by the first 2 chars of the key to avoid giant flat directories:
;;   <base>/projects/<key0><key1>/project-<full-key>
(define (project-memory-subdir id base-data-dir)
  (unless (project-identity? id)
    (error 'project-memory-subdir "expected project-identity"))
  (define key (project-identity-key id))
  (define shard
    (if (>= (string-length key) 2)
        (substring key 0 2)
        "00"))
  (string-append base-data-dir "/projects/" shard "/project-" key))

;; ============================================================
;; Namespace safety (fail-closed)
;; ============================================================

;; A namespace is "safe" iff it is not "." (cwd) and not a session-log path.
;; This enforces the W5 contract that identity never falls back to the
;; current directory or a session storage location.
(define (namespace-safe? ns)
  (not (quarantine-reason ns)))

;; Returns the quarantine reason, or #f if safe.
(define (quarantine-reason ns)
  (cond
    [(not ns) 'missing]
    [(not (string? ns)) 'invalid-type]
    [(string=? ns "") 'empty]
    [(string=? ns ".") 'dot-namespace]
    [(string=? (string-trim ns) ".") 'dot-namespace]
    [else
     (define lower (string-downcase ns))
     (cond
       ;; Session log path: ends in .jsonl AND mentions a sessions dir.
       [(and (string-suffix? ns ".jsonl") (regexp-match? #rx"sessions?[/\\\\]" lower))
        'session-log-path]
       ;; A path that IS under a known sessions storage tree.
       [(regexp-match? #rx"[/\\\\]sessions?[/\\\\].+\\.jsonl$" lower) 'session-log-path]
       [else #f])]))

;; ============================================================
;; I/O entrypoint (fail-closed)
;; ============================================================

;; Derives the canonical project identity for a start directory.
;; Walks up to find a .git marker, resolves symlinks, queries origin.
;; Returns #f if no stable identity can be derived (fail-closed).
;;
;; NOTE: For a plain (non-Git) directory, we DO produce an identity keyed
;; by the resolved realpath — but ONLY after resolving symlinks. We never
;; use "." as a key. If the realpath itself is "." (i.e., caller passed
;; "." and it could not be resolved to an absolute path), we fail closed.
(define (derive-project-identity #:start-dir [start-dir #f])
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define start (or start-dir (current-directory)))
    (define resolved (resolve-path-safe start))
    (cond
      [(not resolved) #f]
      [else
       (define git-info (find-git-root resolved))
       (cond
         [git-info (build-git-identity resolved git-info)]
         [else (build-plain-identity resolved)])])))

;; Resolve a path to an absolute, symlink-resolved path. Returns #f on failure.
(define (resolve-path-safe p)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define abs (path->complete-path (string->path p)))
    (define expanded (simple-form-path abs))
    (and expanded (path->string expanded))))

;; Walk up from `dir` to find a directory containing a `.git` marker.
;; Returns (gitdir-path kind common-dir origin-url) or #f.
(define (find-git-root dir)
  (define dir-path (string->path dir))
  (let loop ([d dir-path])
    (define git-marker (build-path d ".git"))
    (cond
      [(file-exists? git-marker)
       ;; .git is a FILE → worktree or submodule pointer
       (define content (file->string git-marker))
       (define target (parse-gitdir-entry content))
       (cond
         [target
          (define target-path (path->string (path->complete-path (string->path target) d)))
          (define kind (classify-gitdir-path target-path 'directory))
          (define common
            (if (eq? kind 'git-worktree)
                (worktree-common-dir-from-gitdir target-path)
                target-path))
          (list target-path kind common (git-origin-url d))]
         [else #f])]
      [(directory-exists? git-marker)
       ;; .git is a DIRECTORY → main repo (or nested repo)
       (define gd (path->string git-marker))
       (list gd 'git-main gd (git-origin-url d))]
      [else
       (define parent (path-only d))
       (cond
         [(and parent (not (equal? parent d))) (loop parent)]
         [else #f])])))

;; Read `git config --get remote.origin.url` for a directory. Returns #f on any error.
(define (git-origin-url dir)
  (define r
    (parameterize ([current-directory dir])
      (do-git-capture "config" "--get" "remote.origin.url")))
  (and r (> (string-length r) 0) r))

;; Portable git stdout capture (no external dep). Returns trimmed stdout or #f.
(define (do-git-capture . args)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define git-bin (find-executable-path "git" #f))
    (and git-bin
         (let-values ([(sp out in err) (apply subprocess #f 'out 'in 'err git-bin args)])
           (define stdout-str (port->string out))
           (close-input-port out)
           (close-output-port in)
           (close-input-port err)
           (subprocess-wait sp)
           (define code (subprocess-status sp))
           (if (= code 0)
               (string-trim stdout-str)
               #f)))))

;; Build a git-backed identity.
(define (build-git-identity resolved git-info)
  (match-define (list gitdir kind common origin) git-info)
  (define root (git-toplevel resolved))
  (define real-root (or root resolved))
  (define key (compute-project-key #:origin-url origin #:root real-root #:common-dir common))
  (make-project-identity #:key key
                         #:root real-root
                         #:kind (if (eq? kind 'git-pointer) 'git-worktree kind)
                         #:origin-url origin
                         #:common-dir common))

;; `git rev-parse --show-toplevel` for the canonical repo root.
(define (git-toplevel dir)
  (define r
    (parameterize ([current-directory dir])
      (do-git-capture "rev-parse" "--show-toplevel")))
  (and r (> (string-length r) 0) r))

;; Build a plain-directory identity (no git). Keyed by normalized realpath.
;; Fail closed if the realpath resolves to ".".
(define (build-plain-identity resolved)
  (define norm (normalize-path-string resolved))
  (cond
    [(or (string=? norm "") (string=? norm ".") (not norm)) #f]
    [else
     (define key (compute-project-key #:origin-url #f #:root resolved #:common-dir #f))
     (make-project-identity #:key key
                            #:root resolved
                            #:kind 'plain-directory
                            #:origin-url #f
                            #:common-dir #f)]))
