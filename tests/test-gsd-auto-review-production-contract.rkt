#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 300
;; @covers extensions/gsd/delivery-coordinator.rkt
;; B2 fast contract owner for the default production coordinator (wave B2 of
;; milestone #899). The deep two-generation owned-Verify world stays owned by
;; tests/test-gsd-auto-review-production.rkt (now a slow owner in full
;; regression); THIS file is its required-PR counterpart and keeps a genuine
;; production boundary slice under 60 s:
;;
;;   * ONE genuine Verify-command execution through the REAL production
;;     command/registry path (`run-owned-red-verification`: the singleton
;;     verification registry, setsid process ownership, raw-log retention,
;;     behavioral-failure validation) with honest success/failure/timeout
;;     exit-truth — a focused stage of a frozen declaration, never a replay
;;     of the whole Verify campaign;
;;   * the REAL native review controller (default-review-python ->
;;     auto-review-seam-run!) driven through the coordinator-owned
;;     `auto-review-dispatch!` wiring over a real Git world: the typed
;;     prepare -> fresh reviewer -> finalize flow, the exact
;;     head/branch/attempt-id/attempt-fence/generation request wiring, and a
;;     genuine REQUEST_CHANGES whose findings are committed to the REAL
;;     remote and recorded by the REAL journal writer through
;;     `resolve-review-checkpoint!` (receipt-history immutability, rejection
;;     count/findings, wave-failure stamp);
;;   * the recovery replay: a second dispatch reads the committed rejection
;;     from the receipt-bound evidence tail and NEVER re-invokes the
;;     reviewer (the reviewer runner fails the test if consulted);
;;   * prohibited-publication refusal: the UNMODIFIED production (owned
;;     mode) dispatch without an authenticated owned proof is a typed
;;     review-infra stop — the reviewer is never consulted and nothing is
;;     published;
;;   * the approval side of the wiring: a genuine fresh-review approval
;;     flows through the controller's strict gates and PUBLISHES the trio to
;;     the real remote at the pinned head.
;;
;; Honesty boundary: the controller runs in its documented
;; manual-compatibility mode for the rejection/approval wiring cases (the
;; owned-proof end-to-end path stays in the slow production owner); `gh` is
;; a PATH shim (the network boundary the native controller suite also
;; fixtures) and never carries a security decision. No controller status is
;; invented: every typed status below is produced by the real controller.
(require rackunit
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         json
         openssl
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/plan-snapshot.rkt" make-plan-snapshot!)
         (only-in "../extensions/gsd/delivery-journal.rkt"
                  record-delivery-receipt!
                  load-delivery-journal
                  journal-review-rejection-count)
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/review-dispatch.rkt"
         (only-in "../extensions/gsd/review-checkpoint.rkt"
                  resolve-review-checkpoint!
                  pending-review-rejection-data)
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/review-proof-format.rkt"
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt"
                  make-verification-registry
                  verification-job-state
                  verification-job-exit-code
                  verification-job-reaped?
                  verification-close-registry!)
         "helpers/b2-review-contract-fixture.rkt")

(module+ test
  ;; The four cases own independent real Git worlds, campaign roots and
  ;; durable journal identities. The controller round trips are subprocess-
  ;; bound (not CPU-bound), so they are started together and joined by their
  ;; own test-case. Every check still executes in its original case; a case
  ;; failure is re-raised inside the matching test-case, keeping rackunit's
  ;; exit-truth per guarantee instead of hiding it in a background thread.
  (define case-slots (make-hash))
  (define red-case-finished (make-semaphore))
  (define (start-case! name thunk #:wait [before void])
    (define slot (vector #f #f))
    (hash-set! case-slots name slot)
    (vector-set! slot
                 0
                 (thread (lambda ()
                           (with-handlers ([(lambda (_e) #t) (lambda (e) (vector-set! slot 1 e))])
                             (before)
                             ;; Rackunit counts a check run outside a test-case
                             ;; as its own test. Run the whole case under
                             ;; test-begin with a re-raising around so the one
                             ;; matching test-case owns the verdict.
                             (parameterize ([current-test-case-around (lambda (t) (t))])
                               (test-begin
                                 (thunk)))
                             (vector-set! slot 1 'done))))))
  (define (finish-case! name)
    (define slot (hash-ref case-slots name))
    (thread-wait (vector-ref slot 0))
    (define outcome (vector-ref slot 1))
    (unless (eq? outcome 'done)
      (raise outcome)))

  ;; ============================================================
  ;; 1. Genuine Verify-command execution: honest exit-truth at the real
  ;;    production command/registry path.
  ;; ============================================================
  (define (case-red-verification!)
    (define base (make-temporary-file "q-b2-contract-red-~a" 'directory))
    (define plan (b2-plan-id "red-probe"))
    (dynamic-wind
     void
     (lambda ()
       (define world (build-red-probe-world! base plan))
       (define repo (hash-ref world 'repo))
       (define root (hash-ref world 'campaign))
       (define proof-root (hash-ref world 'proof-root))
       (define guard-consulted (make-hasheq))
       (define (fresh-guard!)
         (define consulted (box #f))
         (hash-set! guard-consulted (gensym "g") consulted)
         (b2-probe-guard consulted))
       (parameterize ([current-owned-proof-root proof-root]
                      [current-gsd-verification-registry (make-verification-registry)])
         ;; -- behavioral RED: the focused stage genuinely fails and the
         ;;    negative proof is retained with its raw log.
         (define-values (red-job red-ref)
           (run-owned-red-verification root
                                       plan
                                       0
                                       "attempt-5"
                                       2
                                       repo
                                       (hash-ref world 'red-focused)
                                       #:guard (fresh-guard!)
                                       #:timeout-sec 120))
         (check-eq? (verification-job-state red-job) 'failed)
         (check-equal? (verification-job-exit-code red-job) 1)
         (check-true (verification-job-reaped? red-job))
         (check-true (hash? red-ref) "an authentic assertion failure must retain a red proof")
         ;; The retained manifest is valid and bound to the exact identity.
         (define manifest
           (call-with-input-file
            (build-path proof-root plan (string-append (hash-ref red-ref 'sha256) ".json"))
            read-json))
         (check-true (valid-owned-red-manifest? manifest))
         (define binding (hash-ref manifest 'binding))
         (check-equal? (hash-ref binding 'head) (hash-ref world 'head))
         (check-equal? (hash-ref binding 'branch) b2-red-probe-branch)
         (check-equal? (hash-ref binding 'attempt-id) "attempt-5")
         (check-equal? (hash-ref binding 'attempt-fence) 2)
         (check-equal? (hash-ref manifest 'focused-normalized-command) (hash-ref world 'red-focused))
         (define job-facts (hash-ref manifest 'job))
         (check-equal? (hash-ref job-facts 'state) "failed")
         (check-equal? (hash-ref job-facts 'exit-code) 1)
         (check-equal? (hash-ref job-facts 'fresh) #t)
         ;; The retained raw log really carries the runner's failure marker.
         (define raw-log
           (file->string (build-path proof-root
                                     plan
                                     (string-append (hash-ref (hash-ref manifest 'log) 'sha256)
                                                    ".log"))))
         (check-regexp-match #rx"[1-9][0-9]*/[1-9][0-9]* test failures" raw-log)
         (check-regexp-match #rx"b2 red probe: deliberate behavioral failure" raw-log)
         ;; The offline loader (production read path) reloads the same proof
         ;; from the managed plan store.
         (define reloaded
           (load-owned-red-proof (build-path proof-root plan)
                                 red-ref
                                 (hash-ref red-ref 'binding)
                                 #:command (hash-ref manifest 'command)
                                 #:focused-normalized (hash-ref world 'red-focused)))
         (check-equal? (hash-ref reloaded 'binding) (hash-ref binding 'binding binding))
         ;; The live guard was really consulted.
         (check-true (for/or ([v (in-hash-values guard-consulted)])
                       (unbox v)))
         ;; -- honest success: a passing focused stage retains NO proof.
         (define store-before
           (length (filter (lambda (p) (equal? (filename-extension p) #"json"))
                           (directory-list (build-path proof-root plan)))))
         (define-values (green-job green-ref)
           (run-owned-red-verification root
                                       plan
                                       0
                                       "attempt-5"
                                       2
                                       repo
                                       (hash-ref world 'green-focused)
                                       #:guard (fresh-guard!)
                                       #:timeout-sec 120))
         (check-eq? (verification-job-state green-job) 'completed)
         (check-equal? (verification-job-exit-code green-job) 0)
         (check-false green-ref "a green exit must never mint a red proof")
         ;; -- honest timeout: a probe sleeping past the deadline is a typed
         ;;    timed-out attribution (exit 124), never a red proof.
         (define-values (slow-job slow-ref)
           (run-owned-red-verification root
                                       plan
                                       0
                                       "attempt-5"
                                       2
                                       repo
                                       (hash-ref world 'slow-focused)
                                       #:guard (fresh-guard!)
                                       #:timeout-sec 2))
         (check-eq? (verification-job-state slow-job) 'timed-out)
         (check-equal? (verification-job-exit-code slow-job) 124)
         (check-false slow-ref "a timed-out execution must never mint a red proof")
         (define store-after
           (length (filter (lambda (p) (equal? (filename-extension p) #"json"))
                           (directory-list (build-path proof-root plan)))))
         (check-equal? store-after store-before)
         (check-equal? store-after 1)))
     (lambda () (delete-directory/files base #:must-exist? #f))))

  ;; ============================================================
  ;; Shared review-world scaffolding: campaign + REAL receipt + record.
  ;; ============================================================
  (define (open-review-world kind)
    (define w (b2-copy-review-world! kind))
    ;; The campaign identity is derived from the manifest hash (the same
    ;; discipline as the production fixture: plan-id == manifest-hash is the
    ;; durable identity backstop `persist-campaign!` enforces).
    (define manifest (make-campaign-manifest 1 "w0" '() '() (symbol->string kind)))
    (define plan (campaign-manifest-hash manifest))
    (define root (b2-repo-link! w (b2-campaign-root w plan 0)))
    (define prior-receipt
      (hash-set (hasheq 'repo
                        (path->string (hash-ref w 'subject))
                        'origin
                        (format "https://github.com/~a.git" b2-slug)
                        'head
                        (make-string 40 #\3)
                        'tree
                        (make-string 40 #\d)
                        'verified-at
                        1
                        'evidence
                        b2-evidence-rel
                        'branch
                        b2-wave-branch
                        'attempt-id
                        "attempt-5"
                        'attempt-fence
                        2)
                'head
                (make-string 40 #\3)))
    (define receipt
      (b2-write-receipt! record-delivery-receipt! root plan 0 w #:history (list prior-receipt)))
    ;; The durable wave record the rejection checkpoint authorizes against:
    ;; awaiting delivery, live attempt attempt-5 at fence 2 (the same shape
    ;; the production fixture persists).
    (define rec
      (make-campaign-record
       plan
       manifest
       (list (make-campaign-wave 0 "w0" 'awaiting-delivery 1 (campaign-attempt "attempt-5" 2 0)))
       #f
       2
       'test
       0
       0))
    (persist-campaign! root rec)
    (define bin (b2-install-gh-shim! w))
    (hash 'w
          w
          'plan
          plan
          'root
          root
          'receipt
          receipt
          'head
          (hash-ref w 'head)
          'env
          (b2-controller-environment w bin)
          'bin
          bin))

  (define (with-review-world kind proc)
    (define world (open-review-world kind))
    (dynamic-wind void (lambda () (proc world)) (lambda () (b2-cleanup-world! (hash-ref world 'w)))))

  ;; A runner that fails the test when consulted — the recovery and refusal
  ;; cases prove the reviewer is never re-invoked.
  (define (no-reviewer label)
    (lambda (request) (error 'contract "~a must not invoke the reviewer" label)))

  (define (dispatch world runner seam)
    (define w (hash-ref world 'w))
    (define plan (hash-ref world 'plan))
    (define root (hash-ref world 'root))
    (parameterize ([current-environment-variables (hash-ref world 'env)])
      (auto-review-dispatch! root
                             plan
                             0
                             "implementation-review"
                             #:generation (binding-generation root plan 0)
                             #:runner runner
                             #:run-python (seam root plan 0))))

  ;; ============================================================
  ;; 2. Rejection wiring at the real Git boundary + journal canary + the
  ;;    recovery that never re-invokes the reviewer.
  ;; ============================================================
  (define (case-rejection-and-replay!)
    (with-review-world
     'reject
     (lambda (world)
       (define w (hash-ref world 'w))
       (define plan (hash-ref world 'plan))
       (define root (hash-ref world 'root))
       (define head (hash-ref world 'head))
       (define calls (box 0))
       (define (wired-runner request)
         (set-box! calls (add1 (unbox calls)))
         ;; Exact wiring: the typed request carries the durable receipt
         ;; identity and the dispatch generation, verbatim.
         (check-equal? (hash-ref request 'head) head)
         (check-equal? (hash-ref request 'branch) b2-wave-branch)
         (check-equal? (hash-ref request 'attempt-id) "attempt-5")
         (check-equal? (hash-ref request 'attempt-fence) 2)
         (check-equal? (hash-ref request 'generation) 1)
         (check-true (hash-ref request 'fresh-session-required))
         (check-true (hash-ref request 'non-author-review-required))
         (check-equal? (hash-ref request 'kind) "implementation-review")
         (check-equal? (hash-ref request 'staging) (review-dispatch-staging-path root plan 0 1))
         (check-true (non-empty-string? (hash-ref request 'prompt)))
         (check-regexp-match (regexp (regexp-quote head)) (hash-ref request 'prompt))
         (b2-rejected-runner-result))
       (define result (dispatch world wired-runner b2-manual-controller-seam))
       ;; -- the typed rejection outcome, bound to the pinned identity.
       (check-eq? (delivery-effect-result-kind result)
                  'review-rejected
                  (format "dispatch outcome: ~s" (delivery-effect-result-data result)))
       (define data (delivery-effect-result-data result))
       (check-equal? (hash-ref data 'head) head)
       (check-equal? (hash-ref data 'branch) b2-wave-branch)
       (check-equal? (hash-ref data 'attempt-id) "attempt-5")
       (check-equal? (hash-ref data 'attempt-fence) 2)
       (check-equal? (hash-ref data 'generation) 1)
       (check-equal? (hash-ref data 'postmerge) #f)
       (check-equal? (hash-ref data 'findings) b2-gate-red-findings)
       (check-equal? (unbox calls) 1)
       ;; -- the dispatch alone does not touch the durable journal.
       (define before (load-delivery-journal root plan 0))
       (check-equal? (hash-ref before 'stage) "context-ready")
       (check-false (hash-ref before 'review-rejection #f))
       (define history (hash-ref before 'receipt-history '()))
       (check = (length history) 1)
       ;; -- Git-boundary canary: the rejection artifact reached the REAL
       ;;    remote as a receipt-bound evidence tail at the binding
       ;;    generation's derived output path.
       (define tip (b2-remote-tip w))
       (check-not-false tip)
       (check-not-equal? tip head "the controller must have published the rejection tail")
       (define review-artifact (b2-generation-rel b2-review-rel 1))
       (define shown (b2-git! (hash-ref w 'origin) "show" (format "~a:~a" tip review-artifact)))
       (define datum (read (open-input-string shown)))
       (check-equal? (hash-ref datum 'verdict) "REQUEST_CHANGES")
       (check-equal? (hash-ref datum 'reviewed-sha) head)
       (check-equal? (hash-ref datum 'head) head)
       (check-equal? (hash-ref datum 'branch) b2-wave-branch)
       (check-equal? (hash-ref datum 'attempt-id) "attempt-5")
       (check-equal? (hash-ref datum 'attempt-fence) 2)
       (check-equal? (hash-ref datum 'findings) b2-gate-red-findings)
       ;; -- the REAL typed rejection checkpoint records the marker.
       (check-eq? (resolve-review-checkpoint! root plan 0 data) #t)
       (define after (load-delivery-journal root plan 0))
       (check-equal? (journal-review-rejection-count after) 1)
       (define marker (hash-ref after 'review-rejection #f))
       (check-true (hash? marker))
       (check-equal? (hash-ref marker 'findings) b2-gate-red-findings)
       (check-equal? (hash-ref marker 'attempt-id) "attempt-5")
       (check-equal? (hash-ref marker 'attempt-fence) 2)
       (check-equal? (hash-ref marker 'head) head)
       (check-equal? (hash-ref marker 'branch) b2-wave-branch)
       ;; -- receipt-history immutability across the rejection.
       (check-equal? (hash-ref after 'receipt-history '()) history)
       ;; -- the wave is durably failed by the same checkpoint.
       (define reloaded-record (load-campaign-record root plan))
       (check-eq? (campaign-wave-status (car (campaign-record-waves reloaded-record))) 'failed)
       ;; -- recovery replay: the durable marker authorizes the lane replay
       ;;    input; a second dispatch replays the COMMITTED rejection and
       ;;    never consults a reviewer.
       (define replay (pending-review-rejection-data after))
       (check-true (hash? replay))
       (check-equal? (hash-ref replay 'findings) b2-gate-red-findings)
       (check-equal? (hash-ref replay 'postmerge) #f)
       (define replayed
         (parameterize ([current-environment-variables (hash-ref world 'env)])
           (auto-review-dispatch! root
                                  plan
                                  0
                                  "implementation-review"
                                  #:generation (binding-generation root plan 0)
                                  #:runner (no-reviewer "rejection replay")
                                  #:run-python (b2-manual-controller-seam root plan 0))))
       (check-eq? (delivery-effect-result-kind replayed) 'review-rejected)
       (check-equal? (delivery-effect-result-data replayed) data)
       (check-equal? (unbox calls) 1 "the replay must never re-invoke the reviewer"))))

  ;; ============================================================
  ;; 3. Prohibited publication: the production (owned mode) dispatch
  ;;    refuses without an authenticated owned proof.
  ;; ============================================================
  (define (case-prohibited-publication!)
    (with-review-world
     'refuse
     (lambda (world)
       (define w (hash-ref world 'w))
       (define plan (hash-ref world 'plan))
       (define root (hash-ref world 'root))
       (define tip-before (b2-remote-tip w))
       (define result
         (dispatch world (no-reviewer "prohibited publication") b2-owned-controller-seam))
       (check-eq? (delivery-effect-result-kind result)
                  'review-infra
                  (format "refusal outcome: ~s" (delivery-effect-result-data result)))
       (define reason (hash-ref (delivery-effect-result-data result) 'reason ""))
       (check-true (string-contains? reason "missing or invalid authenticated owned proof")
                   (format "unexpected refusal reason: ~a" reason))
       ;; Nothing published, nothing journaled, the reviewer never consulted.
       (check-equal? (b2-remote-tip w) tip-before)
       (define journal (load-delivery-journal root plan 0))
       (check-equal? (hash-ref journal 'stage) "context-ready")
       (check-false (hash-ref journal 'review-rejection #f))
       (check-equal? (length (hash-ref journal 'receipt-history '())) 1)
       (define staging (review-dispatch-staging-path root plan 0 1))
       (check-false (file-exists? (build-path staging "reviewer-output.json"))))))

  ;; ============================================================
  ;; 4. Approval wiring: the strict gates publish a genuine approval to the
  ;;    real remote at the pinned head.
  ;; ============================================================
  (define (case-approval-publication!)
    (with-review-world
     'approve
     (lambda (world)
       (define w (hash-ref world 'w))
       (define plan (hash-ref world 'plan))
       (define root (hash-ref world 'root))
       (define head (hash-ref world 'head))
       (define result
         (dispatch world
                   (lambda (request)
                     (hasheq 'kind
                             "approved"
                             'reviewer
                             "independent-contract-reviewer-session"
                             'scope
                             (make-string 70 #\s)
                             'report
                             (make-string 70 #\r)
                             'validation
                             (b2-approved-validation)))
                   b2-manual-controller-seam))
       (check-eq? (delivery-effect-result-kind result)
                  'ok
                  (format "approval outcome: ~s" (delivery-effect-result-data result)))
       (check-equal? (hash-ref (delivery-effect-result-data result) 'status) "reviewed")
       ;; -- Git-boundary canary: the APPROVED trio reached the REAL remote
       ;;    at the binding generation's derived output paths.
       (define tip (b2-remote-tip w))
       (check-not-equal? tip head "the strict gate must have published the reviewed trio")
       (define evidence
         (read (open-input-string
                (b2-git! (hash-ref w 'origin)
                         "show"
                         (format "~a:~a" tip (b2-generation-rel b2-evidence-rel 1))))))
       (check-equal? (hash-ref evidence 'status) "ready-for-merge")
       (check-equal? (hash-ref evidence 'implementation-sha) head)
       (check-equal? (hash-ref evidence 'branch) b2-wave-branch)
       (define review
         (read (open-input-string
                (b2-git! (hash-ref w 'origin)
                         "show"
                         (format "~a:~a" tip (b2-generation-rel b2-review-rel 1))))))
       (check-equal? (hash-ref review 'verdict) "APPROVED")
       (check-equal? (hash-ref review 'reviewed-sha) head)
       ;; -- the dispatch alone leaves the journal stage untouched (the
       ;;    coordinator owns stage advancement).
       (define journal (load-delivery-journal root plan 0))
       (check-equal? (hash-ref journal 'stage) "context-ready")
       (check-equal? (length (hash-ref journal 'receipt-history '())) 1))))

  ;; Warm the immutable Git template once before any case thread copies it.
  (b2-warm-review-template!)
  (start-case!
   'red-verification
   (lambda ()
     (dynamic-wind void case-red-verification! (lambda () (semaphore-post red-case-finished)))))
  (start-case! 'rejection case-rejection-and-replay!)
  ;; The refusal is a single controller phase: start it only after the
  ;; Verify probe finishes, keeping three (not four) controller worlds
  ;; contending for the runner's CPUs at any moment.
  (start-case! 'refusal
               case-prohibited-publication!
               #:wait (lambda () (semaphore-wait red-case-finished)))
  (start-case! 'approval case-approval-publication!)

  (test-case "one genuine owned Verify-command execution keeps honest success/failure/timeout exit-truth"
    (finish-case! 'red-verification))
  (test-case "production dispatch wiring rejects, journals and replays at the real Git boundary"
    (finish-case! 'rejection))
  (test-case "production owned-mode dispatch refuses without an authenticated owned proof"
    (finish-case! 'refusal))
  (test-case "an approved fresh review publishes through the strict gates at the real Git boundary"
    (finish-case! 'approval)))
