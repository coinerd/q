#lang racket/base

;; tools/scratch.rkt — trusted PER-SESSION scratch allocation (W2)
;;
;; Every scratch consumer resolves the SAME absolute root for ONE session:
;;   <platform-temp>/q-agent-scratch/<opaque session token>/
;;
;; Consumers:
;;   - runtime/agent-session.rkt (allocates once per created/resumed/forked
;;     agent session; releases the binding on close)
;;   - runtime/tool-coordinator.rkt (stamps the session's root onto the
;;     per-batch exec-context)
;;   - extensions/gsd/prompts.rkt (the /go executor prompt advertises the
;;     root the runtime allocated for that executor session)
;;   - tools/builtins/write.rkt / edit.rkt (structured write + edit)
;;   - tools/builtins/bash.rkt (redirection policy, via bash-safety)
;;   - sandbox/worker-tools.rkt / worker-dispatch.rkt (per-request trusted
;;     scratch root on the trusted IPC channel)
;;   - tools/scheduler-execution.rkt (trusted propagation to the worker)
;;
;; Properties:
;;   - Isolation is PER SESSION, not per process. Allocations live in a
;;     session-keyed registry: two agent sessions in the SAME q process get
;;     two distinct random roots and neither plane (structured write/edit,
;;     bash redirection, sandbox allowed roots, prompt guidance) can widen one
;;     session to the other's root. There is deliberately NO process-wide
;;     allocation and NO fallback that sanctions the scratch BASE directory:
;;     a session without an allocated (or explicitly adopted) root has no
;;     trusted scratch at all, so arbitrary /tmp writes stay denied.
;;   - The session component is a crypto-random 16-hex token, never a model
;;     value and never a session id: registry keys (agent session ids, /go
;;     scratch tokens) are bookkeeping only and never become path components.
;;   - Both the base directory and the session directory are created with
;;     mode 0700 (owner-only), and that mode is RE-READ afterwards: a chmod
;;     that cannot be enforced (foreign owner, non-POSIX filesystem) fails
;;     closed instead of leaving an unenforced trust boundary. A symlink in
;;     place of the container or the session directory is refused, never
;;     followed.
;;   - One explicit adoption path exists for a session whose root was decided
;;     BEFORE the session object existed (the /go executor prompt has to name
;;     the path before the fresh wave session is created):
;;     `call-with-adopted-session-scratch-root` binds exactly one already
;;     allocated root for exactly one session creation, and
;;     `bind-session-scratch-root!` records it under the real session id.
;;     Adoption is single-shot and shape-checked; it never invents a root and
;;     never reuses a foreign session's allocation.
;;   - Fail-closed surface: the trusted IPC channel accepts ONLY the exact
;;     "<base>/<opaque token>" shape (validated in the IPC decoder and again in
;;     the worker), so a model or a compromised coordinator value can never
;;     widen the worker's roots to an arbitrary directory. The decoder accepts
;;     the producer's spelling through a symlinked temporary ANCESTOR (a
;;     TMPDIR alias) but not a link at the allocation itself; the worker
;;     additionally requires the container and the session directory to ALREADY
;;     exist as real (non-symlink) owner-only directories and refuses a
;;     missing or lax allocation for the whole request instead of repairing it.
;;   - Containment (bash redirection and structured-tool scratch access) is
;;     decided on CANONICAL spellings, not raw strings: the longest existing
;;     prefix of both sides is resolved through the filesystem, so a symlink
;;     below the root (or at the target itself) does not escape the root, while
;;     a spelling that reaches the same root through a symlinked temp ancestor
;;     still matches. A link cycle fails closed.
;;   - Honest remote-executor limitation: the trusted scratch root travels on the
;;     trusted channel to the LOCAL (subprocess) worker only. A remote (mTLS
;;     broker / executor-server) request is built without either trust channel —
;;     sandbox/gateway-bridge.rkt `execute-via-remote` constructs the 7-argument
;;     request — so a remote worker receives NO scratch root at all and
;;     sanctions no redirection target: remote sandboxed writes into a scratch
;;     path fail closed by omission. That is deliberate (a coordinator-local
;;     path is meaningless on another host's filesystem), and the executor node
;;     enforces it server-side: `process-secure-request` refuses any
;;     wire-supplied trusted working directory OR trusted scratch root with
;;     the `remote-trusted-root-refused` class before dispatch, so the
;;     limitation is a refusal, not merely an omission by the client. It means
;;     remote execution does not get a per-session scratch destination in this
;;     work. Granting one would require the remote worker to allocate and
;;     return its own root (and the coordinator to accept that returned root);
;;     until then the honest behavior is "no root, no sanction".
;;
;; This module is deliberately dependency-free (racket only) so tools, the
;; runtime and the worker can share it without pulling the tool registry or
;; runtime layer.

(require racket/file
         racket/path
         racket/random
         racket/string)

(provide SCRATCH-BASE-DIRNAME
         default-scratch-base-dir
         current-scratch-base-dir
         fresh-scratch-token
         valid-scratch-token?
         scratch-root-for-token
         trusted-session-scratch-path?
         existing-trusted-session-scratch-root?
         canonical-scratch-path
         ensure-scratch-root!
         scratch-path-within?
         allocate-session-scratch-root!
         allocated-session-scratch-root
         bind-session-scratch-root!
         release-session-scratch-root!
         take-adopted-session-scratch-root!
         call-with-adopted-session-scratch-root
         reset-session-scratch-registry!)

;; Base directory name below the platform temporary directory. The platform
;; temp dir is honored (TMPDIR on POSIX, the known folder on Windows); docs
;; call the POSIX spelling /tmp/q-agent-scratch.
(define SCRATCH-BASE-DIRNAME "q-agent-scratch")

(define (default-scratch-base-dir)
  (simplify-path (build-path (find-system-path 'temp-dir) SCRATCH-BASE-DIRNAME) #f))

;; Test/trusted-caller override for the platform base directory.
(define current-scratch-base-dir (make-parameter (default-scratch-base-dir)))

;; A session component is an opaque lowercase-hex token. The /go runtime
;; generates one per executor session; every allocation uses the same shape.
(define scratch-token-rx #px"^[0-9a-f]{16,64}$")

(define (valid-scratch-token? token)
  (and (string? token) (regexp-match? scratch-token-rx token)))

(define (bytes->hex-string bs)
  (apply string-append
         (for/list ([v (in-bytes bs)])
           (define s (number->string v 16))
           (if (= (string-length s) 1)
               (string-append "0" s)
               s))))

;; 8 crypto-random bytes -> 16 opaque hex characters.
(define (fresh-scratch-token)
  (bytes->hex-string (crypto-random-bytes 8)))

;; Pure: token -> absolute root below the CURRENT base directory, or #f for an
;; invalid token (separators, `..`, absolute paths, NUL, wrong alphabet).
(define (scratch-root-for-token token)
  (and (valid-scratch-token? token) (simplify-path (build-path (current-scratch-base-dir) token) #f)))

(define (trim-trailing-separator path-str)
  (if (and (> (string-length path-str) 1) (string-suffix? path-str "/"))
      (substring path-str 0 (sub1 (string-length path-str)))
      path-str))

(define (directory-string p)
  (trim-trailing-separator (path->string (simplify-path (path->complete-path (if (path? p)
                                                                                 p
                                                                                 (string->path p)))
                                                        #f))))

;; ── Canonical resolution ──────────────────────────────────────
;; Resolve the LONGEST EXISTING PREFIX of P through the filesystem (following
;; every symlink) and append the remaining components lexically. Returns an
;; absolute simplified path, or #f when resolution fails (a link cycle, an
;; unreadable directory). A dangling link is followed to the lexical target the
;; OS would create.
;;
;; Why a prefix walk: `simplify-path` is lexical (a symlinked ancestor stays
;; spelled as written) and `resolve-path` only resolves the FINAL component.
;; Both miss the case that matters for authorization — an existing ancestor or
;; final name that is a symlink out of the authorized tree, followed by a name
;; that does not exist yet (exactly a bash redirect target). Restarting the
;; walk at every link target also normalizes platform aliases such as
;; /var -> /private/var, so the same directory always compares equal.
;;
;; Links are resolved with `resolve-path` one candidate at a time, and every
;; resolved link is remembered: a self-referential or cyclic link (where
;; `resolve-path` makes no progress and returns the link itself) fails the
;; whole resolution closed instead of being re-walked forever. A dangling link
;; is followed to its lexical target (where the OS would create the file), and
;; an unreadable component raises and fails closed, so a link planted inside a
;; scratch root can neither escape containment nor hang the classifier.
(define (canonical-scratch-path p)
  (and p
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (let ([complete (path->complete-path (if (path? p)
                                                  p
                                                  (string->path p)))])
           (let loop ([remaining (explode-path complete)]
                      [prefix #f]
                      [visited (hash)])
             (cond
               [(null? remaining) (or prefix (simplify-path complete #f))]
               [else
                (define candidate
                  (if prefix
                      (build-path prefix (car remaining))
                      (car remaining)))
                (cond
                  [(link-exists? candidate)
                   (define key (path->string (simplify-path candidate #f)))
                   (and (not (hash-ref visited key #f))
                        (let* ([resolved (resolve-path candidate)]
                               [resolved-complete (if (complete-path? resolved)
                                                      resolved
                                                      (path->complete-path resolved
                                                                           (or (path-only candidate)
                                                                               (current-directory))))]
                               [resolved-simplified (simplify-path resolved-complete #f)])
                          (and (not (equal? (path->string resolved-simplified) key))
                               (loop (append (explode-path resolved-simplified) (cdr remaining))
                                     #f
                                     (hash-set visited key #t)))))]
                  [(or (file-exists? candidate) (directory-exists? candidate))
                   (loop (cdr remaining) (simplify-path candidate #f) visited)]
                  [else
                   ;; Non-existent tail: lexical from here on.
                   (simplify-path (for/fold ([acc candidate]) ([part (in-list (cdr remaining))])
                                    (build-path acc part))
                                  #f)])]))))))

;; #t iff P is exactly one opaque session component directly below the base
;; directory. This is the only shape accepted from a trusted worker channel:
;; it can never name the base itself, a sibling path, or an escape.
(define (trusted-session-scratch-path? p)
  (and p
       (let* ([path (if (path? p)
                        p
                        (string->path p))]
              [simplified (simplify-path path #f)])
         (and (complete-path? simplified)
              (equal? (directory-string (path-only simplified))
                      (directory-string (current-scratch-base-dir)))
              (valid-scratch-token? (path->string (file-name-from-path simplified)))))))

;; Containment for already-authorized scratch roots. Both operands are resolved
;; through the FILESYSTEM first (see `canonical-scratch-path`), so a symlink
;; below the root — or a spelling that reaches the same directory through a
;; symlinked ancestor — cannot change the verdict. The separator boundary keeps
;; /…/<token>-escape out of /…/<token>.
(define (scratch-path-within? p root)
  (and p
       root
       (let ([path (canonical-scratch-path p)]
             [canonical-root (canonical-scratch-path root)])
         (and path
              canonical-root
              (string-path-within? (path->string path) (path->string canonical-root))))))

;; Pure string form of the component-boundary containment rule.
(define (string-path-within? path-str root-str)
  (let ([prefix (if (string-suffix? root-str "/")
                    root-str
                    (string-append root-str "/"))])
    (or (string=? path-str root-str) (string-prefix? path-str prefix))))

(define (scratch-location-error what path message)
  (exn:fail (format "scratch: refusing ~a ~a: ~a" what (path->string path) message)
            (current-continuation-marks)))

(define (owner-only-bits? bits)
  ;; No group/other read, write or execute.
  (zero? (bitwise-and bits #o077)))

;; Enforce owner-only permissions, then VERIFY them. This is a trust boundary,
;; not a best-effort cosmetic chmod: an allocation container or session
;; directory that another principal can read, write or traverse would let a
;; foreign user observe or redirect the model's disposable writes. A chmod
;; that fails (typically because the directory is owned by someone else) or a
;; re-read that still shows group/other access fails closed with an explicit
;; refusal instead of continuing with an unenforced boundary.
(define (private-permissions! what path)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise (scratch-location-error
                                       what
                                       path
                                       (format "cannot enforce owner-only permissions (~a)"
                                               (exn-message e)))))])
    (file-or-directory-permissions path #o700))
  (with-handlers ([exn:fail? (lambda (e)
                               (raise (scratch-location-error
                                       what
                                       path
                                       (format "cannot verify owner-only permissions (~a)"
                                               (exn-message e)))))])
    (define bits (file-or-directory-permissions path 'bits))
    (unless (owner-only-bits? bits)
      (raise (scratch-location-error what path (format "permissions ~o are not owner-only" bits))))))

;; A scratch location must be a real directory, never a symlink. A symlinked
;; container or session directory could point anywhere, so the "one opaque
;; component below the base" shape would no longer describe the bytes the
;; model actually writes. Ancestors of the base are the platform's business;
;; the allocation itself must be an honest directory.
(define (require-real-directory! what path)
  (when (link-exists? path)
    (raise (scratch-location-error what path "it is a symbolic link")))
  (unless (directory-exists? path)
    (raise (scratch-location-error what path "it is not a directory"))))

;; An EXISTING owner-only real directory: no symlink in the final component and
;; no group/other access. Permissions are read, never repaired (a reader of
;; someone else's allocation must not widen or tighten it).
(define (real-private-directory? path)
  (and (directory-exists? path)
       (not (link-exists? path))
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (owner-only-bits? (file-or-directory-permissions path 'bits)))))

;; The opaque session component of P, or #f when P has no file name.
(define (session-component-name p)
  (define name (file-name-from-path p))
  (and name (path->string name)))

;; #t iff ROOT names an ALREADY EXISTING session allocation that may be
;; sanctioned on this filesystem:
;;   - exactly one opaque token component below the CURRENT base directory
;;     (compared canonically, so a spelling that reaches the same directory
;;     through a symlinked temp alias is accepted and a foreign directory is
;;     not), and
;;   - the base and the session directory both exist as real (non-symlink)
;;     directories with owner-only permissions, and
;;   - the CANONICAL session directory still sits below the CANONICAL base.
;;
;; Read-only by construction: unlike `ensure-scratch-root!` nothing is created
;; and no mode is repaired. The sandboxed worker uses this to refuse a missing
;; or lax allocation for the WHOLE request instead of papering over it — an
;; allocation it did not create is not an allocation it can trust.
(define (existing-trusted-session-scratch-root? root)
  (and root
       (let* ([path (if (path? root)
                        root
                        (string->path root))]
              [absolute (with-handlers ([exn:fail? (lambda (_) #f)])
                          (simplify-path (path->complete-path path) #f))])
         (and absolute
              (complete-path? absolute)
              (valid-scratch-token? (session-component-name absolute))
              (let* ([base (current-scratch-base-dir)]
                     [canonical-base (canonical-scratch-path base)]
                     [canonical-parent (canonical-scratch-path (path-only absolute))]
                     [canonical-absolute (canonical-scratch-path absolute)])
                (and canonical-base
                     canonical-parent
                     canonical-absolute
                     ;; The delivered parent IS the container this process
                     ;; trusts (same directory, spelling may differ).
                     (equal? (path->string canonical-parent) (path->string canonical-base))
                     (real-private-directory? base)
                     (real-private-directory? absolute)
                     (string-path-within? (path->string canonical-absolute)
                                          (path->string canonical-base))))))))

;; Create (if needed) and return ROOT. Refuses any path that is not exactly
;; one session component below the current base directory.
(define (ensure-scratch-root! root)
  (unless (trusted-session-scratch-path? root)
    (raise-argument-error 'ensure-scratch-root! "trusted session scratch path" root))
  (define base (current-scratch-base-dir))
  ;; Simulate a pre-created container: a symlink or a file in its place is a
  ;; foreign object, never this session's allocation.
  (when (and (link-exists? base) (not (directory-exists? base)))
    (raise (scratch-location-error "scratch base" base "it is a symbolic link")))
  (unless (directory-exists? base)
    (make-directory* base))
  (require-real-directory! "scratch base" base)
  (when (link-exists? root)
    (raise (scratch-location-error "session scratch root" root "it is a symbolic link")))
  (unless (directory-exists? root)
    (make-directory* root))
  (require-real-directory! "session scratch root" root)
  ;; Owner-only for the allocation container and the session directory. The
  ;; mode is (re)applied because a pre-existing base directory may have been
  ;; created with a laxer umask, by an older installation, or by another
  ;; principal trying to widen this session's scratch.
  (private-permissions! "scratch base" base)
  (private-permissions! "session scratch root" root)
  root)

;; ============================================================
;; Session-keyed allocation registry
;; ============================================================
;;
;; The registry maps an opaque session key (an agent session id, or a /go
;; scratch token that is about to become a session's root) to the ONE root
;; that session owns. It replaces the former process-wide box: sharing one
;; root across every session in a process is exactly the W2 defect. Keys never
;; appear in a path.

(define session-roots (make-hash))
(define registry-lock (make-semaphore 1))

(define (valid-session-key? key)
  (and (string? key) (positive? (string-length key))))

(define (check-session-key who key)
  (unless (valid-session-key? key)
    (raise-argument-error who "non-empty-string? session key" key)))

(define (coerce-scratch-path who v)
  (define path
    (cond
      [(path? v) v]
      [(string? v) (string->path v)]
      [else #f]))
  (unless (and path (absolute-path? path) (trusted-session-scratch-path? path))
    (raise-argument-error who "allocated trusted session scratch path" v))
  (ensure-scratch-root! (simplify-path (path->complete-path path) #f)))

;; Lookup only. Never allocates and never falls back to a broader directory: a
;; session without an allocation simply has no trusted scratch.
(define (allocated-session-scratch-root key)
  (check-session-key 'allocated-session-scratch-root key)
  (call-with-semaphore registry-lock (lambda () (hash-ref session-roots key #f))))

;; Allocate exactly once per session key. PREFERRED-TOKEN (a scratch token that
;; was already advertised) becomes the session component of the very first
;; allocation; later calls for the same key return the same root and never
;; move it.
(define (allocate-session-scratch-root! key [preferred-token #f])
  (check-session-key 'allocate-session-scratch-root! key)
  (when (and preferred-token (not (valid-scratch-token? preferred-token)))
    (raise-argument-error 'allocate-session-scratch-root!
                          "valid scratch token or #f"
                          preferred-token))
  (call-with-semaphore registry-lock
                       (lambda ()
                         (or (hash-ref session-roots key #f)
                             (let* ([token (or preferred-token (fresh-scratch-token))]
                                    [root (ensure-scratch-root! (scratch-root-for-token token))])
                               (hash-set! session-roots key root)
                               root)))))

;; Adopt an ALREADY allocated root (shape-checked, never a broader path) under
;; a session key. A key that already owns a different root is a fail-closed
;; conflict: one session never silently inherits a second root.
(define (bind-session-scratch-root! key root)
  (check-session-key 'bind-session-scratch-root! key)
  (define path (coerce-scratch-path 'bind-session-scratch-root! root))
  (call-with-semaphore
   registry-lock
   (lambda ()
     (define existing (hash-ref session-roots key #f))
     (cond
       [(not existing)
        (hash-set! session-roots key path)
        path]
       [(equal? (path->string existing) (path->string path)) existing]
       [else
        (raise
         (exn:fail
          (format
           "scratch: refusing to rebind session scratch root (session key ~a already owns a different root)"
           key)
          (current-continuation-marks)))]))))

;; Forget a session's binding on session close. Files are not deleted: scratch
;; is disposable and nothing here cleans up foreign sessions.
(define (release-session-scratch-root! key)
  (check-session-key 'release-session-scratch-root! key)
  (call-with-semaphore registry-lock
                       (lambda ()
                         (hash-remove! session-roots key)
                         (void))))

;; ============================================================
;; Explicit, single-shot adoption of a pre-allocated root
;; ============================================================
;;
;; The /go executor prompt must name the scratch path BEFORE the executor
;; session object exists (the prompt text is submitted first). The trusted
;; producer therefore allocates the root, advertises exactly that root, and
;; binds it for exactly one session creation in the same dynamic extent. The
;; box is consumed by the first `take-adopted-session-scratch-root!`, so a
;; second session created in that extent (a subagent, a sibling) never
;; inherits the first session's root.

(define current-adopted-session-scratch-root (make-parameter #f))

(define (call-with-adopted-session-scratch-root root thunk)
  (define path (coerce-scratch-path 'call-with-adopted-session-scratch-root root))
  (parameterize ([current-adopted-session-scratch-root (box path)])
    (thunk)))

;; Returns the adopted root once, then #f for every later session creation in
;; the same extent.
(define (take-adopted-session-scratch-root!)
  (define b (current-adopted-session-scratch-root))
  (cond
    [(not b) #f]
    [else
     (define root (unbox b))
     (when root
       (set-box! b #f))
     root]))

;; Test isolation: forget every binding and any pending adoption. Never
;; deletes files.
(define (reset-session-scratch-registry!)
  (call-with-semaphore registry-lock
                       (lambda ()
                         (hash-clear! session-roots)
                         (current-adopted-session-scratch-root #f)
                         (void))))
