#lang racket/base
;; STABILITY: internal
;; BUG-0088 A2: inert proposals and broker-authenticated owner approvals.
;;
;; TRUSTED-PROCESS boundary, NOT arbitrary same-uid code isolation. The embedding
;; selects a private root outside project/control source and restricts author and
;; reviewer tools: they cannot invoke this process's broker or write private owner
;; files. A different process's fabricated broker grant is not transferable. Same-
;; uid arbitrary code/private-root writes or module-namespace access defeat this
;; boundary; file modes, PTYs, booleans, policy auto-approval and hashes do NOT
;; authenticate operators. The trusted frontend owns request presentation/decision;
;; owner-frontend only CONSUMES its real opaque interactive grant for exact digest.
;; No CLI, request auto-approval, grant constructor or live authorization here.
;;
;; Main API (no root discovery): propose-retain! stores only inert proposals;
;; load-supplemental-record reads deeply immutable canonical records;
;; validate-supplemental-approval ref [expected-proposal-ref] returns an immutable
;; owner record or #f, rechecking root origin, proposal bytes and revocation each
;; time. Validation is a snapshot, not a lease: the journal must revalidate at
;; protected transitions. References are public identities, not standalone
;; authorization. Issuance does not consume task launch permission (the journal does).
;; owner-frontend: approve-supplemental-proposal! proposal-ref interactive-grant;
;; revoke! revocation-proposal-ref interactive-grant. Both consume once through
;; the existing broker. A revocation proposal has exactly one refs entry: the
;; approval to revoke. Public retention of that proposal remains inert.
;;
;; BUG-0088 blockers fixed here:
;; - Duplicate approval dedup: re-approving an already-approved proposal
;;   consumes the second authentic grant but returns the EXISTING approval
;;   identity, computed under the one shared root-private lock by scanning for
;;   a retained approval of the exact same proposal sha. A revoked proposal can
;;   never be re-approved: any revoked approval for that proposal fails closed
;;   instead of minting a fresh nonce approval.
;; - ONE root-private OS-global admission/revocation critical section (flock
;;   on <root>/.authority.lock) plus an in-process semaphore for threads.
;;   Reentrancy is explicit per-thread depth counting; a different root while
;;   held is refused rather than deadlocked. Revocation is fsync-durable (file
;;   and directory) BEFORE success is reported, and validate-supplemental-
;;   approval rechecks origin/proposal bytes/revocation under the same lock,
;;   so the journal can serialize its writes against revocation.
;; - call-with-supplemental-authority-lock proc and
;;   call-with-supplemental-admission refs proc expose brief critical-section
;;   admission for the parent controller's external effects. Admission
;;   revalidates every ref live under the lock; callers must never wait for
;;   long-running jobs or perform further waits while holding it.
(require json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         ffi/unsafe
         ffi/unsafe/port
         (only-in racket/random crypto-random-bytes)
         (only-in file/sha1 bytes->hex-string)
         (only-in "../../runtime/approval/broker.rkt" approval-grant? call-with-approval-grant)
         "../../util/json/checksum.rkt"
         (only-in "evidence-repair-format.rkt" canonical-evidence-json)
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide current-supplemental-authority-root
         supplemental-reference?
         propose-retain!
         load-supplemental-record
         validate-supplemental-approval
         supplemental-authority-status
         call-with-supplemental-authority-lock
         call-with-supplemental-admission)

(define current-supplemental-authority-root (make-parameter #f))
(define-runtime-path control-source-root "../..")
(define max-record-bytes (* 1024 1024))
(define approval-kinds '("task-approval" "candidate-approval" "supersession-approval"))
(define approvable-kinds '("task-proposal" "candidate-proposal" "supersession-proposal"))
(define proposal-kinds (append approvable-kinds '("revocation-proposal")))
(define (refuse message)
  (error 'supplemental-evidence-authority "~a" message))
(define (kind? x)
  (and (string? x) (regexp-match? #px"^[a-z][a-z0-9-]{0,63}$" x)))
(define (digest? x)
  (and (string? x) (regexp-match? #px"^[0-9a-f]{64}$" x)))
(define (supplemental-reference? x)
  (and (hash? x)
       (immutable? x)
       (= 2 (hash-count x))
       (kind? (hash-ref x 'kind #f))
       (digest? (hash-ref x 'sha256 #f))
       (immutable? (hash-ref x 'kind))
       (immutable? (hash-ref x 'sha256))))

;; Copy before computing identities: mutation of caller-owned strings/hashes must
;; not change retained content or the returned descriptor. No readers/macros run.
(define (freeze v [depth 0])
  (when (> depth 64)
    (refuse "JSON nesting exceeds 64 levels or contains a cycle"))
  (cond
    [(hash? v)
     (unless (andmap (lambda (k) (and (symbol? k) (symbol-interned? k))) (hash-keys v))
       (refuse "JSON keys must be interned symbols"))
     (for/hasheq ([(k value) (in-hash v)])
       (values k (freeze value (add1 depth))))]
    [(list? v) (map (lambda (value) (freeze value (add1 depth))) v)]
    [(string? v) (string->immutable-string v)]
    [(or (boolean? v) (exact-integer? v) (eq? v 'null)) v]
    [else (refuse "unsupported canonical JSON data")]))
(define (canonical-bytes record)
  (define bytes (string->bytes/utf-8 (canonical-evidence-json record)))
  (when (> (bytes-length bytes) max-record-bytes)
    (refuse "record exceeds 1 MiB"))
  bytes)
(define (identity record)
  (string->immutable-string (sha256-string (bytes->string/utf-8 (canonical-bytes record)))))
(define (within? path base)
  (define p (explode-path (simplify-path path #f)))
  (define b (explode-path (simplify-path base #f)))
  (and (<= (length b) (length p)) (equal? b (take p (length b)))))

;; Ancestors must be actual directories, never symlinks. Only the final private
;; root needs 0700; ordinary /home and /tmp ancestors need not be private. This is
;; validation within the trusted-process model, not race isolation from same uid.
(define (check-directory-chain! raw)
  ;; A trailing slash can make link-exists? follow the link on Unix.
  (define spelling
    (path->string (if (path? raw)
                      raw
                      (string->path raw))))
  (define path
    (if (regexp-match? #px"^/+$" spelling)
        (string->path "/")
        (string->path (regexp-replace #px"/+$" spelling ""))))
  (when (link-exists? path)
    (refuse "symlink directory component"))
  (unless (eq? (file-or-directory-type path #f) 'directory)
    (refuse "non-directory path component"))
  (define-values (base _name _dir?) (split-path path))
  (when (and (path? base) (not (equal? base path)))
    (check-directory-chain! base)))
(define (private-root!)
  (define raw (current-supplemental-authority-root))
  (unless (and (path-string? raw) (complete-path? raw))
    (refuse "trusted embedding must provide an absolute authority root"))
  (when (member 'up (explode-path raw))
    (refuse "root traversal is forbidden"))
  ;; Check the unsimplified spelling too, so a symlink followed by '.' cannot
  ;; disappear during lexical normalization.
  (check-directory-chain! raw)
  (define root (simplify-path raw #f))
  (when (or (within? root (current-directory)) (within? root control-source-root))
    (refuse "authority root is inside project/execution/control source"))
  (unless (= #o700 (bitwise-and #o7777 (file-or-directory-permissions root 'bits)))
    (refuse "authority directory must have exactly mode 0700"))
  root)
(define (record-path root ref)
  (build-path root (string-append (hash-ref ref 'sha256) ".json")))
(define (check-record-file! path)
  (when (link-exists? path)
    (refuse "symlink record"))
  (unless (eq? (file-or-directory-type path #f) 'file)
    (refuse "record is not a regular file"))
  (define stat (file-or-directory-stat path #t))
  (unless (= #o100000 (bitwise-and #o170000 (hash-ref stat 'mode)))
    (refuse "record is not a regular file"))
  (unless (= 1 (hash-ref stat 'hardlink-count))
    (refuse "hardlinked record"))
  (unless (= #o600 (bitwise-and #o7777 (file-or-directory-permissions path 'bits)))
    (refuse "record must have exactly mode 0600"))
  (when (> (file-size path) max-record-bytes)
    (refuse "record exceeds 1 MiB")))
(define (read-canonical-file path)
  (check-record-file! path)
  (define bytes (call-with-input-file path (lambda (in) (read-bytes (add1 max-record-bytes) in))))
  (when (or (eof-object? bytes) (> (bytes-length bytes) max-record-bytes))
    (refuse "empty or oversized record"))
  (define in (open-input-bytes bytes))
  (define record (freeze (read-json in)))
  (unless (and (eof-object? (peek-byte in)) (hash? record) (bytes=? bytes (canonical-bytes record)))
    (refuse "noncanonical record"))
  (check-record-file! path)
  record)
(define (proposal-record? record)
  (and (= 4 (hash-count record))
       (equal? (hash-ref record 'schema #f) "supplemental-proposal-store/v1")
       (member (hash-ref record 'kind #f) proposal-kinds)
       (hash-has-key? record 'payload)
       (list? (hash-ref record 'refs #f))
       (andmap supplemental-reference? (hash-ref record 'refs))))
(define (owner-record? record)
  (define kind (hash-ref record 'kind #f))
  (define proposal (hash-ref record 'proposal #f))
  (and (equal? (hash-ref record 'schema #f) "supplemental-owner-store/v1")
       (digest? (hash-ref record 'owner-origin #f))
       (supplemental-reference? proposal)
       (cond
         [(member kind approval-kinds)
          (and (= 5 (hash-count record))
               (digest? (hash-ref record 'nonce #f))
               (equal? (hash-ref proposal 'kind)
                       (list-ref approvable-kinds (index-of approval-kinds kind))))]
         [(equal? kind "revocation")
          (and (= 5 (hash-count record))
               (equal? (hash-ref proposal 'kind) "revocation-proposal")
               (supplemental-reference? (hash-ref record 'approval #f))
               (member (hash-ref (hash-ref record 'approval) 'kind) approval-kinds))]
         [else #f])))
(define (load-at root ref)
  (unless (supplemental-reference? ref)
    (refuse "invalid kind+sha256 reference"))
  (define record (read-canonical-file (record-path root ref)))
  (unless (and (or (proposal-record? record) (owner-record? record))
               (equal? (hash-ref record 'kind) (hash-ref ref 'kind))
               (equal? (identity record) (hash-ref ref 'sha256)))
    (refuse "altered or mismatched record"))
  record)
(define (load-supplemental-record ref)
  (load-at (private-root!) ref))

;; ---- ONE root-private admission/revocation critical section (BUG-0088) ----
;; A single OS-level lock file plus an in-process semaphore serialize owner
;; approvals, revocation, validation rechecks and (from the journal module)
;; protected operation admission. The kernel owns the lock lifetime; the file
;; is only its named handle. Reentrancy is explicit per-thread depth counting:
;; nested acquisition in the same thread for the SAME root is supported and
;; holds the section; a different root while held is refused, never deadlocked.
;; Durability: fsync file data and directory entries before success is
;; reported. Non-Unix platforms are refused rather than silently weaker.
(define libc (and (eq? (system-type 'os) 'unix) (ffi-lib #f)))
(define c-fsync (and libc (get-ffi-obj "fsync" libc (_fun _int -> _int))))
(define c-open (and libc (get-ffi-obj "open" libc (_fun _path _int -> _int))))
(define c-close (and libc (get-ffi-obj "close" libc (_fun _int -> _int))))
(define (sync-port! port)
  (unless c-fsync
    (refuse "durable authority unsupported on this platform"))
  (flush-output port)
  (unless (zero? (c-fsync (unsafe-port->file-descriptor port)))
    (refuse "file fsync failed")))
(define (sync-directory! dir)
  (unless c-open
    (refuse "durable authority unsupported on this platform"))
  (define fd (c-open dir 0))
  (when (< fd 0)
    (refuse "cannot open authority directory for fsync"))
  (dynamic-wind void
                (lambda ()
                  (unless (zero? (c-fsync fd))
                    (refuse "directory fsync failed")))
                (lambda () (c-close fd))))
(define authority-lock-semaphore (make-semaphore 1))
(define authority-lock-cell (make-thread-cell #f))
(define (authority-lock-path root)
  (build-path root ".authority.lock"))
(define (ensure-authority-lock-file! root)
  (define path (authority-lock-path root))
  (unless (file-or-directory-type path #f)
    (with-handlers ([exn:fail:filesystem:exists? void])
      (call-with-output-file path void #:exists 'error #:permissions #o600)))
  (check-record-file! path))
(define (run-with-os-authority-lock! root proc)
  ;; Blocking retry loop: Racket-level sleeps keep every thread schedulable;
  ;; no foreign call ever blocks the place while another thread holds the OS
  ;; lock. Content is rechecked inside the section, failing closed on drift.
  (ensure-authority-lock-file! root)
  (let loop ()
    (call-with-file-lock/timeout #f
                                 'exclusive
                                 (lambda ()
                                   (check-record-file! (authority-lock-path root))
                                   (proc))
                                 (lambda ()
                                   (sleep 0.01)
                                   (loop))
                                 #:lock-file (authority-lock-path root)
                                 #:delay 0.01
                                 #:max-delay 0.05)))
(define (call-with-supplemental-authority-lock/at root proc)
  (define held (thread-cell-ref authority-lock-cell))
  (cond
    [(and held (equal? (car held) root))
     ;; Explicitly supported same-thread reentrancy for this exact root.
     (dynamic-wind (lambda () (thread-cell-set! authority-lock-cell (cons root (add1 (cdr held)))))
                   proc
                   (lambda () (thread-cell-set! authority-lock-cell held)))]
    [held (refuse "authority lock is already held for a different private root")]
    [else
     (semaphore-wait authority-lock-semaphore)
     (dynamic-wind (lambda () (thread-cell-set! authority-lock-cell (cons root 1)))
                   (lambda () (run-with-os-authority-lock! root proc))
                   (lambda ()
                     (thread-cell-set! authority-lock-cell #f)
                     (semaphore-post authority-lock-semaphore)))]))
(define (call-with-supplemental-authority-lock proc)
  (define root (private-root!))
  (call-with-supplemental-authority-lock/at root proc))
(define (call-with-supplemental-admission refs proc)
  ;; Brief external-effect admission for the parent controller: every ref is
  ;; revalidated live under the SAME critical section that revocation takes,
  ;; so a concurrent revocation can never interleave with the admitted effect.
  ;; Never wait for long-running jobs inside proc.
  (unless (and (list? refs) (andmap supplemental-reference? refs))
    (refuse "admission refs must be immutable kind+sha256 approval references"))
  (define root (private-root!))
  (call-with-supplemental-authority-lock/at
   root
   (lambda ()
     (for ([ref (in-list refs)])
       (unless (and (member (hash-ref ref 'kind) approval-kinds)
                    (approval-at root ref)
                    (not (revocation-at root ref)))
         (refuse "admission requires a live unrevoked private approval")))
     (proc))))

(define (propose-retain! kind payload [refs '()])
  ;; No model-author tool can use this API as a file-write escape hatch.
  (when (current-gsd-author-phase)
    (refuse "author phase cannot retain proposals"))
  (unless (member kind proposal-kinds)
    (refuse "only inert proposal kinds may be retained"))
  (unless (and (list? refs) (andmap supplemental-reference? refs))
    (refuse "refs must be immutable kind+sha256 descriptors"))
  (define root (private-root!))
  (define record
    (freeze (hasheq 'schema "supplemental-proposal-store/v1" 'kind kind 'payload payload 'refs refs)))
  (retain-at! root record))

;; Only this module's trusted owner path can retain owner record kinds. The public
;; proposal writer never accepts arbitrary schema/kind or a caller write guard.
(define (retain-at! root raw-record)
  (define record (freeze raw-record))
  (define ref (hasheq 'kind (hash-ref record 'kind) 'sha256 (identity record)))
  (define target (record-path root ref))
  (cond
    [(file-or-directory-type target #f)
     (unless (equal? record (load-at root ref))
       (refuse "immutable record collision"))]
    [else
     (define temp (make-temporary-file ".proposal-~a.tmp" #f root))
     (dynamic-wind
      void
      (lambda ()
        (file-or-directory-permissions temp #o600)
        (call-with-output-file temp
                               (lambda (out) (write-bytes (canonical-bytes record) out))
                               #:exists 'truncate)
        ;; Commit the complete bytes in the same filesystem, never overwrite an
        ;; existing identity. Concurrent identical retention is an idempotent read.
        (with-handlers ([exn:fail:filesystem:exists? (lambda (_)
                                                       (unless (equal? record (load-at root ref))
                                                         (refuse "immutable record collision")))])
          (rename-file-or-directory temp target #f)))
      (lambda ()
        (when (file-exists? temp)
          (delete-file temp))))])
  (load-at root ref)
  ref)

;; The root's origin is an identity, NOT a secret/authenticator. Protected owner
;; writes after broker consumption are the trust anchor, not a matching string or
;; file mode. The stored origin rejects foreign-store records; it is retained once,
;; never regenerated on validation. Atomic create
;; handles concurrent owners; immutable per-approval revocation markers avoid a
;; lost-update-prone shared approval/revocation list. No mutable launch ledger here.
(define (random-identity)
  (string->immutable-string (bytes->hex-string (crypto-random-bytes 32))))
(define (origin-path root)
  (build-path root ".owner-origin.json"))
(define (revocation-path root ref)
  (build-path root (string-append "revoked-" (hash-ref ref 'sha256) ".json")))
(define (create-private-file! root target record)
  ;; Durable private create: fsync the complete bytes before the atomic rename
  ;; and fsync the directory entry before returning. Revocation success is
  ;; never reported before it survives a crash.
  (define temp (make-temporary-file ".owner-~a.tmp" #f root))
  (define dir (path-only target))
  (dynamic-wind void
                (lambda ()
                  (file-or-directory-permissions temp #o600)
                  (call-with-output-file temp
                                         (lambda (out)
                                           (write-bytes (canonical-bytes record) out)
                                           (sync-port! out))
                                         #:exists 'truncate)
                  (with-handlers ([exn:fail:filesystem:exists? void])
                    (rename-file-or-directory temp target #f))
                  (sync-directory! dir))
                (lambda ()
                  (when (file-exists? temp)
                    (delete-file temp)))))
(define (owner-origin-at root [create? #f])
  (define path (origin-path root))
  (when (and create? (not (file-or-directory-type path #f)))
    (create-private-file!
     root
     path
     (hasheq 'schema "supplemental-owner-origin/v1" 'identity (random-identity))))
  (define record (read-canonical-file path))
  (unless (and (= 2 (hash-count record))
               (equal? (hash-ref record 'schema #f) "supplemental-owner-origin/v1")
               (digest? (hash-ref record 'identity #f)))
    (refuse "invalid private owner origin"))
  (hash-ref record 'identity))
(define (approval-at root ref [expected #f])
  (unless (and (supplemental-reference? ref) (member (hash-ref ref 'kind) approval-kinds))
    (refuse "not an approval reference"))
  (define record (load-at root ref))
  (unless (and (equal? (hash-ref record 'owner-origin) (owner-origin-at root))
               (or (not expected)
                   (and (supplemental-reference? expected)
                        (equal? (freeze expected) (hash-ref record 'proposal)))))
    (refuse "foreign owner origin or wrong proposal"))
  (load-at root (hash-ref record 'proposal))
  record)
(define (revocation-at root ref)
  (define marker (revocation-path root ref))
  (and (file-or-directory-type marker #f)
       (let* ([revocation-ref (read-canonical-file marker)]
              [record (load-at root revocation-ref)])
         (unless (and (equal? (hash-ref record 'kind) "revocation")
                      (equal? (hash-ref record 'approval) (freeze ref))
                      (equal? (hash-ref record 'owner-origin) (owner-origin-at root)))
           (refuse "invalid revocation marker"))
         (define proposal (load-at root (hash-ref record 'proposal)))
         (unless (equal? (hash-ref proposal 'refs) (list (freeze ref)))
           (refuse "mismatched revocation proposal"))
         revocation-ref)))
(define (validate-supplemental-approval ref [expected-proposal-ref #f])
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define root (private-root!))
    ;; The snapshot is taken inside the one admission/revocation section, so a
    ;; concurrent revocation can never split origin/bytes/revocation checks.
    (call-with-supplemental-authority-lock/at root
                                              (lambda ()
                                                (define record
                                                  (approval-at root ref expected-proposal-ref))
                                                (and (not (revocation-at root ref)) record)))))
(define (supplemental-authority-status)
  (hasheq 'status
          "interactive-broker-required"
          'reason
          "trusted frontend must supply an exact-digest, active, one-use interactive grant"))
(define (check-owner-phase!)
  (when (current-gsd-author-phase)
    (refuse "author phase cannot authorize or revoke")))
;; Duplicate-approval dedup (BUG-0088): the journal keys operations by
;; approval digest, so two approvals of one proposal would admit two
;; operations. Retention is therefore deduplicated by the stable proposal sha
;; under the shared lock: an existing approval of the exact proposal is
;; returned unchanged; if any approval of that proposal was revoked, the
;; proposal is permanently dead and re-approval fails closed.
(define (approvals-for-proposal root origin proposal-ref)
  (define found '())
  (for ([name (in-list (sort (map path->string (directory-list root)) string<?))]
        #:when (regexp-match? #px"^[0-9a-f]{64}\\.json$" name))
    (define record (read-canonical-file (build-path root name)))
    (when (and (member (hash-ref record 'kind #f) approval-kinds)
               (equal? (hash-ref record 'owner-origin) origin)
               (equal? (hash-ref record 'proposal) (freeze proposal-ref)))
      (set! found
            (cons (hasheq 'kind
                          (hash-ref record 'kind)
                          'sha256
                          (string->immutable-string (substring name 0 64)))
                  found))))
  (reverse found))
(define (approve-proposal! ref grant)
  (check-owner-phase!)
  ;; Policy grants are intentionally accepted by the general broker but NEVER
  ;; here. approval-grant? is opaque and distinct from policy-approval-grant?.
  (unless (approval-grant? grant)
    (refuse "interactive broker grant required"))
  (define root (private-root!))
  (define proposal (load-at root ref))
  (define index (index-of approvable-kinds (hash-ref proposal 'kind)))
  (unless index
    (refuse "not an approvable proposal"))
  (call-with-approval-grant grant
                            (hash-ref ref 'sha256)
                            (lambda ()
                              (define origin (owner-origin-at root #t))
                              (call-with-supplemental-authority-lock/at
                               root
                               (lambda ()
                                 (define existing (approvals-for-proposal root origin ref))
                                 (cond
                                   [(pair? existing)
                                    ;; The existing approval is the only
                                    ;; admissible identity for this proposal.
                                    (for ([approval-ref (in-list existing)])
                                      (when (revocation-at root approval-ref)
                                        (refuse "revoked proposal can never be re-approved")))
                                    (define approval-ref (car existing))
                                    (approval-at root approval-ref ref)
                                    approval-ref]
                                   [else
                                    (retain-at! root
                                                (hasheq 'schema
                                                        "supplemental-owner-store/v1"
                                                        'kind
                                                        (list-ref approval-kinds index)
                                                        'proposal
                                                        ref
                                                        'owner-origin
                                                        origin
                                                        'nonce
                                                        (random-identity)))]))))))
(define (revoke-proposal! ref grant)
  (check-owner-phase!)
  (unless (approval-grant? grant)
    (refuse "interactive broker grant required"))
  (define root (private-root!))
  (define proposal (load-at root ref))
  (unless (and (equal? (hash-ref proposal 'kind) "revocation-proposal")
               (= 1 (length (hash-ref proposal 'refs))))
    (refuse "revocation requires an exact one-approval revocation proposal"))
  (define approval-ref (car (hash-ref proposal 'refs)))
  (approval-at root approval-ref)
  (call-with-approval-grant
   grant
   (hash-ref ref 'sha256)
   (lambda ()
     ;; Revocation shares the ONE admission/revocation critical section with
     ;; journal writes and validation rechecks, and the marker is fsync-durable
     ;; before this succeeds.
     (call-with-supplemental-authority-lock/at
      root
      (lambda ()
        (define revocation-ref
          (retain-at! root
                      (hasheq 'schema
                              "supplemental-owner-store/v1"
                              'kind
                              "revocation"
                              'proposal
                              ref
                              'approval
                              approval-ref
                              'owner-origin
                              (owner-origin-at root))))
        (create-private-file! root (revocation-path root approval-ref) revocation-ref)
        (unless (equal? revocation-ref (revocation-at root approval-ref))
          (refuse "approval already revoked by a different proposal"))
        revocation-ref)))))
(module+ owner-frontend
  ;; Loaded only by the trusted embedding/frontend, not exposed as model tools.
  ;; The frontend registers/presents the exact proposal sha256 with the existing
  ;; broker and obtains a grant after an explicit operator decision. No boolean,
  ;; TTY/stdin, arbitrary supplied guard or auto-approval alternative is accepted.
  (provide approve-supplemental-proposal!
           revoke!)
  (define approve-supplemental-proposal! approve-proposal!)
  (define revoke! revoke-proposal!))
