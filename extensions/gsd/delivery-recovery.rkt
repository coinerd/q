#lang racket/base

;; Generic same-attempt repair-tail recovery entry.  This module is deliberately
;; independent from go-orchestrator.rkt but repeats its advisory lock protocol
;; byte-for-byte at the filesystem boundary: stable .lock path, can-update open,
;; exclusive port lock, truncate+owner diagnostics, dynamic-wind release, and no
;; fence mutation.

(require json
         racket/file
         racket/list
         racket/os
         racket/path
         racket/port
         racket/string
         racket/system
         (only-in "attempt-artifacts.rkt" record-wave-delivery!)
         (only-in "campaign-repository.rkt" load-campaign-record)
         (only-in "campaign-state.rkt"
                  campaign-attempt-fence-token
                  campaign-attempt-id
                  campaign-fence-token
                  campaign-plan-id
                  campaign-record-cancellation
                  campaign-record-waves
                  campaign-wave-current-attempt
                  campaign-wave-index
                  campaign-wave-status)
         (only-in "delivery-handoff.rkt" persist-delivery-handoff!)
         (only-in "delivery-journal.rkt"
                  delivery-journal-path
                  load-delivery-journal
                  reconcile-repair-tail-receipt!
                  repair-tail-allowed-stages)
         (only-in "delivery-receipt.rkt"
                  committed-delivery-snapshot
                  default-remote-published?
                  verify-campaign-delivery)
         (only-in "delivery-verifier.rkt"
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-message
                  make-branch-delivery-context
                  run-delivery-verification)
         (only-in "wave-executor.rkt" load-plan-from-index))

(provide (struct-out recovery-result)
         current-gsd-recovery-github-origin?
         recover-delivery!)

(struct recovery-result (status reason actions details) #:transparent)

(define sha-rx #px"^[0-9a-f]{40}$")
(define plan-rx #px"^[0-9a-f]{64}$")

(define (sha? v)
  (and (string? v) (regexp-match? sha-rx v)))

(define (plan? v)
  (and (string? v) (regexp-match? plan-rx v)))

(define (refused reason . details)
  (recovery-result 'refused reason '() details))

(define (github-origin? origin)
  (and (string? origin)
       (regexp-match?
        #px"^(?:https://github\\.com/|git@github\\.com:)[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:\\.git)?$"
        origin)
       #t))

;; Test-only injection point for the candidate resolver's origin policy.  The
;; production default accepts only github.com remotes.  No verifier, snapshot or
;; publication predicate is user supplied by the CLI.
(define current-gsd-recovery-github-origin?
  (make-parameter
   github-origin?
   (lambda (v)
     (unless (and (procedure? v) (procedure-arity-includes? v 1))
       (raise-argument-error 'current-gsd-recovery-github-origin? "(procedure-arity-includes/c 1)" v))
     v)))

(struct campaign-lease (path port owner-pid owner-session) #:mutable)

(define (lease-path base-dir plan-id)
  (build-path base-dir ".planning" "campaigns" (string-append plan-id ".lock")))

(define (acquire-lease base-dir plan-id #:session-id [session-id "gsd-recover-delivery"])
  (define p (lease-path base-dir plan-id))
  (define-values (dir _ __) (split-path p))
  (make-directory* dir)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    (define port (open-output-file p #:exists 'can-update))
    (if (port-try-file-lock? port 'exclusive)
        (begin
          (let ([owner
                 (if (and (string? session-id) (not (string=? session-id ""))) session-id "unknown")])
            (file-truncate port 0)
            (file-position port 0)
            (write (hasheq 'owner owner 'pid (getpid) 'acquired (current-seconds)) port)
            (flush-output port)
            (campaign-lease p port (current-seconds) owner)))
        (begin
          (close-output-port port)
          #f))))

(define (release-lease! lease)
  (when (and lease (campaign-lease? lease))
    (with-handlers ([exn:fail? void])
      (port-file-unlock (campaign-lease-port lease))
      (close-output-port (campaign-lease-port lease)))))

(define (with-recovery-lease root plan apply? thunk)
  ;; Dry-runs are advisory and intentionally side-effect free: they do not
  ;; acquire the campaign OS lock because acquiring it truncates/writes owner
  ;; diagnostics.  The full lock serialization guarantee applies to --apply.
  (cond
    [(not apply?) (thunk)]
    [else
     (define lease (acquire-lease root plan))
     (cond
       [(not lease) (refused 'lock-busy)]
       [else (dynamic-wind void (lambda () (thunk)) (lambda () (release-lease! lease)))])]))

(define (symlink-path? p)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (eq? (file-or-directory-type p #f) 'link)))

(define (refuse-symlinked-root root)
  (define full (path->complete-path root))
  (for/or ([part (in-list (explode-path full))])
    (define p
      (if (eq? part (car (explode-path full)))
          part
          (build-path (car (explode-path full)))))
    #f)
  (symlink-path? full))

(define (find-wave rec wave-idx)
  (for/first ([w (in-list (campaign-record-waves rec))]
              #:when (= (campaign-wave-index w) wave-idx))
    w))

(define (attempt-ok? attempt attempt-id fence)
  (and attempt
       (equal? (campaign-attempt-id attempt) attempt-id)
       (equal? (campaign-attempt-fence-token attempt) fence)))

(define (receipt-ref journal key [default #f])
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (and (hash? receipt) (hash-ref receipt key default)))

(define (copy-directory/files* src dest)
  (make-directory* dest)
  (for ([p (in-list (directory-list src))])
    (define from (build-path src p))
    (define to (build-path dest p))
    (cond
      [(directory-exists? from) (copy-directory/files* from to)]
      [(file-exists? from)
       (make-parent-directory* to)
       (copy-file from to #t)])))

(define (validated-superseded-journal root plan wave src live-attempt-id live-fence)
  (and (file-exists? src)
       (let ([tmp (make-temporary-file "delivery-recovery-journal-~a" 'directory)])
         (dynamic-wind void
                       (lambda ()
                         (define dst (delivery-journal-path tmp plan wave))
                         (make-parent-directory* dst)
                         (copy-file src dst #t)
                         (define journal (load-delivery-journal tmp plan wave))
                         (cond
                           [(not (equal? (receipt-ref journal 'attempt-id) live-attempt-id)) #f]
                           [(not (equal? (receipt-ref journal 'attempt-fence) live-fence)) #f]
                           [else journal]))
                       (lambda () (delete-directory/files tmp #:must-exist? #f))))))

(define (repair-tail-stage? journal)
  (member (hash-ref journal 'stage #f) repair-tail-allowed-stages))

(define (atomic-write-bytes! bs dst)
  (make-parent-directory* dst)
  (call-with-atomic-output-file dst (lambda (out _) (write-bytes bs out))))

(define (ensure-journal root plan wave rec attempt-id fence superseded apply?)
  (define active (delivery-journal-path root plan wave))
  (define w (find-wave rec wave))
  (define attempt (and w (campaign-wave-current-attempt w)))
  (cond
    [(file-exists? active) (values (load-delivery-journal root plan wave) #f)]
    [else
     (define src
       (or superseded
           (build-path root
                       ".planning"
                       "campaigns"
                       plan
                       (format "coordinator-w~a.json.reconciled-superseded" wave))))
     (define restored (validated-superseded-journal root plan wave src attempt-id fence))
     (cond
       [(not restored) (values #f #f)]
       [else
        (define restored-bytes (file->bytes src))
        (values restored restored-bytes)])]))

(define (run-git repo . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code
             (find-executable-path "git")
             "-C"
             (path->string (path->complete-path repo))
             args)))
  (values code (string-trim (get-output-string out)) (string-trim (get-output-string err))))

(define (git-out repo . args)
  (define-values (code out _err) (apply run-git repo args))
  (and (zero? code) out))

(define (git-ok? repo . args)
  (define-values (code _out _err) (apply run-git repo args))
  (zero? code))

(define (clone-candidate! origin branch)
  (define tmp (make-temporary-file "delivery-recovery-clone-~a" 'directory))
  (define checkout (build-path tmp "candidate"))
  (define code
    (parameterize ([current-output-port (open-output-string)]
                   [current-error-port (open-output-string)])
      (system*/exit-code (find-executable-path "git")
                         "clone"
                         "--branch"
                         branch
                         "--single-branch"
                         origin
                         (path->string checkout))))
  (if (zero? code)
      (values checkout tmp)
      (values #f tmp)))

(define (recovery-snapshot cwd)
  (committed-delivery-snapshot cwd))

(define (resolve-candidate root journal expected-head)
  (define branch (receipt-ref journal 'branch))
  (define origin (receipt-ref journal 'origin))
  (cond
    [(not (and (string? branch) (not (member branch '("main" "master")))))
     (values #f #f 'protected-branch #f)]
    [(not ((current-gsd-recovery-github-origin?) origin)) (values #f #f 'non-github-origin #f)]
    [else
     (define-values (candidate cleanup-root) (clone-candidate! origin branch))
     (cond
       [(not candidate) (values #f cleanup-root 'clone-failed #f)]
       [(symlink-path? candidate) (values #f cleanup-root 'symlinked-candidate #f)]
       [else
        (define snapshot (recovery-snapshot candidate))
        (define origin-head (git-out candidate "rev-parse" (string-append "origin/" branch)))
        (cond
          [(not snapshot) (values #f cleanup-root 'candidate-invalid #f)]
          [(not (equal? (hash-ref snapshot 'branch #f) branch))
           (values #f cleanup-root 'candidate-branch-mismatch snapshot)]
          [(not (equal? (hash-ref snapshot 'head #f) expected-head))
           (values #f cleanup-root 'candidate-head-mismatch snapshot)]
          [(not (equal? origin-head expected-head))
           (values #f cleanup-root 'origin-tip-mismatch snapshot)]
          [(not (default-remote-published? candidate branch expected-head))
           (values #f cleanup-root 'unpublished-head snapshot)]
          [else (values candidate cleanup-root #f snapshot)])])]))

(define (verification-approved? v)
  (if (delivery-verification? v)
      (delivery-verification-approved? v)
      (and v #t)))

(define (verification-message v)
  (if (delivery-verification? v)
      (delivery-verification-message v)
      (format "~a" v)))

(define (verify-on-copy root plan wave candidate expected-base old-head restored-journal-bytes)
  (define tmp (make-temporary-file "delivery-recovery-root-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (copy-directory/files* root tmp)
     (when restored-journal-bytes
       (define restored-active (delivery-journal-path tmp plan wave))
       (make-parent-directory* restored-active)
       (call-with-atomic-output-file restored-active
                                     (lambda (out _) (write-bytes restored-journal-bytes out))))
     (define plan-struct (load-plan-from-index tmp))
     (define journal (load-delivery-journal tmp plan wave))
     (define branch (receipt-ref journal 'branch))
     (define ctx
       (make-branch-delivery-context #:repo-root candidate
                                     #:branch branch
                                     #:base-commit expected-base
                                     #:worktree-path candidate
                                     #:repair-tail-head old-head))
     (define verifier (lambda (idx) (run-delivery-verification candidate plan-struct idx)))
     (define result
       (verify-campaign-delivery tmp
                                 plan
                                 wave
                                 candidate
                                 ctx
                                 verifier
                                 (lambda () (load-campaign-record tmp plan))))
     (values (verification-approved? result) (verification-message result)))
   (lambda () (delete-directory/files tmp #:must-exist? #f))))

(define (ancestor? repo old-head new-head)
  (git-ok? repo "merge-base" "--is-ancestor" old-head new-head))

(define (recovery-record-attempt-delivery-provenance! root plan wave attempt-id fence)
  (define journal (load-delivery-journal root plan wave))
  (define receipt (and journal (hash-ref journal 'receipt #f)))
  (and (hash? receipt)
       (equal? (hash-ref receipt 'attempt-id #f) attempt-id)
       (equal? (hash-ref receipt 'attempt-fence #f) fence)
       (let ([branch (hash-ref receipt 'branch #f)]
             [head (hash-ref receipt 'head #f)])
         (and (string? branch)
              (not (member branch '("main" "master")))
              (sha? head)
              (record-wave-delivery! root plan wave branch head)
              (cons branch head)))))

(define (recover-delivery! #:root root
                           #:plan plan
                           #:wave wave
                           #:attempt-id attempt-id
                           #:fence fence
                           #:expected-head expected-head
                           #:expected-base expected-base
                           #:old-receipt-head [old-receipt-head #f]
                           #:superseded-journal [superseded-journal #f]
                           #:apply? [apply? #f])
  (let/ec return
    (define (stop reason . details)
      (return (recovery-result 'refused reason '() details)))
    (unless (plan? plan)
      (stop 'invalid-plan))
    (unless (exact-nonnegative-integer? wave)
      (stop 'invalid-wave))
    (unless (and (string? attempt-id) (positive? (string-length attempt-id)))
      (stop 'invalid-attempt))
    (unless (exact-nonnegative-integer? fence)
      (stop 'invalid-fence))
    (unless (sha? expected-head)
      (stop 'invalid-expected-head))
    (unless (sha? expected-base)
      (stop 'invalid-expected-base))
    (when (refuse-symlinked-root root)
      (stop 'symlinked-root))
    (with-recovery-lease
     root
     plan
     apply?
     (lambda ()
       (define rec (load-campaign-record root plan))
       (unless rec
         (stop 'missing-campaign))
       (unless (equal? (campaign-plan-id rec) plan)
         (stop 'plan-mismatch))
       (when (campaign-record-cancellation rec)
         (stop 'cancelled))
       (define w (find-wave rec wave))
       (unless w
         (stop 'missing-wave))
       (unless (memq (campaign-wave-status w) '(awaiting-delivery verifying))
         (stop 'invalid-wave-status (campaign-wave-status w)))
       (unless (equal? (campaign-fence-token rec) fence)
         (stop 'stale-fence))
       (define attempt (campaign-wave-current-attempt w))
       (unless (attempt-ok? attempt attempt-id fence)
         (stop 'stale-attempt))
       (define-values (journal restored-journal-bytes)
         (ensure-journal root plan wave rec attempt-id fence superseded-journal apply?))
       (unless journal
         (stop 'missing-or-invalid-journal))
       (unless (repair-tail-stage? journal)
         (stop 'repair-tail-stage-refused (hash-ref journal 'stage #f)))
       (unless (equal? (receipt-ref journal 'attempt-id) attempt-id)
         (stop 'journal-attempt-mismatch))
       (unless (equal? (receipt-ref journal 'attempt-fence) fence)
         (stop 'journal-fence-mismatch))
       (unless (string? (receipt-ref journal 'branch))
         (stop 'journal-branch-mismatch))
       (when (member (receipt-ref journal 'branch) '("main" "master"))
         (stop 'protected-branch))
       (define old-head (or old-receipt-head (receipt-ref journal 'head)))
       (unless (sha? old-head)
         (stop 'invalid-old-receipt-head))
       (when (and old-receipt-head (not (equal? old-receipt-head (receipt-ref journal 'head))))
         (stop 'old-receipt-head-mismatch))
       (define-values (candidate cleanup-root candidate-reason snapshot)
         (resolve-candidate root journal expected-head))
       (dynamic-wind
        void
        (lambda ()
          (when candidate-reason
            (stop candidate-reason))
          (unless (ancestor? candidate expected-base old-head)
            (stop 'stale-base))
          ;; Verification always runs against a COPY that carries exactly the
          ;; journal state apply would produce; the root tree stays untouched
          ;; until verification approves. A verify failure therefore leaves
          ;; NO active journal behind (no effect on refusal).
          (define-values (ok? message)
            (verify-on-copy root plan wave candidate expected-base old-head restored-journal-bytes))
          (unless ok?
            (stop 'verify-failed message))
          (cond
            [(not apply?)
             (recovery-result 'dry-run
                              #f
                              '(strict-checks journal resolver verify)
                              (list 'would-reconcile old-head expected-head))]
            [else
             (define new-receipt
               (hash-set* snapshot
                          'verified-at
                          (current-seconds)
                          'evidence
                          "Recovered by gsd-recover-delivery repair-tail verification"
                          'attempt-id
                          attempt-id
                          'attempt-fence
                          fence))
             ;; Only now — verification approved — does the superseded journal
             ;; become the active one (byte-identical), immediately before the
             ;; reconciliation that reads it. Refused runs wrote nothing.
             (when restored-journal-bytes
               (atomic-write-bytes! restored-journal-bytes (delivery-journal-path root plan wave)))
             (reconcile-repair-tail-receipt! root
                                             plan
                                             wave
                                             new-receipt
                                             #:expected-attempt-id attempt-id
                                             #:expected-fence fence
                                             #:head-ancestor?
                                             (lambda (old new) (ancestor? candidate old new)))
             ;; Equivalent narrow provenance effect to delivery-finalize's
             ;; record-attempt-delivery-provenance!: bind the same attempt's
             ;; receipt branch/head into the durable wave record.  Recovery
             ;; intentionally does not record verification-context provenance:
             ;; no verify pipeline context exists at recovery time, and the
             ;; receipt-branch provenance equivalence is the durable contract.
             ;; Kept local to avoid importing finalization/tracker/outbox code.
             (recovery-record-attempt-delivery-provenance! root plan wave attempt-id fence)
             (define after (load-campaign-record root plan))
             (define after-wave (find-wave after wave))
             (persist-delivery-handoff! root
                                        plan
                                        after-wave
                                        "repair-tail recovered; protected delivery remains pending")
             (recovery-result 'applied
                              #f
                              '(reconciled-receipt recorded-provenance persisted-handoff)
                              (list 'old-head old-head 'new-head expected-head))]))
        (lambda ()
          (when cleanup-root
            (delete-directory/files cleanup-root #:must-exist? #f))))))))
