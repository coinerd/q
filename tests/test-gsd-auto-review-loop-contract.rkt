#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 300
;; @covers extensions/gsd/go-orchestrator.rkt
;; @covers extensions/gsd/review-session.rkt
;; @covers extensions/gsd/review-dispatch.rkt
;; B1 fast contract owner for the auto-review /go loop (wave B1 of milestone
;; #899). The deep two-full-Verify-chain world stays owned by
;; tests/test-gsd-auto-review-loop.rkt (slow/full); THIS file is its required-PR
;; counterpart and keeps the real ownership claims on a focused boundary slice
;; (measured ~43 s with `racket scripts/run-tests.rkt` on this file):
;;
;;   * real /go request wiring through `make-campaign-request` into
;;     `auto-review-dispatch!`, with a REAL `make-auto-review-runner` review
;;     session (only its agent-loop transport is deterministic) that proves
;;     the fresh, non-author reviewer requirement and the exact
;;     head/branch/attempt-id/fence/generation pin at the dispatch boundary;
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
;;   * rejection-checkpoint recovery: the REAL production recovery reader
;;     accepts the committed marker against the still-live receipt and
;;     returns the typed retry input bound to the Git-published rejection;
;;     the reviewer is consulted exactly once,
;;   * prohibited-publication refusal: the UNMODIFIED production (owned
;;     mode) dispatch without an authenticated owned proof is a typed
;;     review-infra stop — the reviewer is never consulted and nothing is
;;     published;
;;
;; Honesty boundary: the controller runs in its documented
;; manual-compatibility mode for the rejection wiring case (the two full
;; owned-Verify chains, the full model-transport review session and the
;; owned-proof end-to-end path stay in the slow owner); `gh` is
;; a PATH shim (the network boundary the native controller suite also
;; fixtures) and never carries a security decision. No controller status is
;; invented: every typed status below is produced by the real controller.
(require rackunit
         racket/file
         racket/format
         racket/list
         racket/path
         racket/string
         json
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
         (only-in "../extensions/gsd/campaign-request.rkt"
                  make-campaign-request
                  campaign-request-auto-review-runner)
         "../extensions/gsd/review-session.rkt"
         (only-in "../extensions/gsd-planning/execution-policy.rkt" current-gsd-author-phase)
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../util/loop-result.rkt" make-loop-result)
         "helpers/b1-auto-review-loop-contract-fixture.rkt")

(module+ test
  ;; The three cases own independent real Git worlds, campaign roots and
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
    (define base (make-temporary-file "q-b1-contract-red-~a" 'directory))
    (define plan (b1-plan-id "red-probe"))
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
         (b1-probe-guard consulted))
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
         (check-equal? (hash-ref binding 'branch) b1-red-probe-branch)
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
         (check-regexp-match #rx"b1 red probe: deliberate behavioral failure" raw-log)
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
    (define w (b1-copy-review-world! kind))
    ;; The campaign identity is derived from the manifest hash (the same
    ;; discipline as the production fixture: plan-id == manifest-hash is the
    ;; durable identity backstop `persist-campaign!` enforces).
    (define manifest (make-campaign-manifest 1 "w0" '() '() (symbol->string kind)))
    (define plan (campaign-manifest-hash manifest))
    (define root (b1-repo-link! w (b1-campaign-root w plan 0)))
    (define prior-receipt
      (hash-set (hasheq 'repo
                        (path->string (hash-ref w 'subject))
                        'origin
                        (format "https://github.com/~a.git" b1-slug)
                        'head
                        (make-string 40 #\3)
                        'tree
                        (make-string 40 #\d)
                        'verified-at
                        1
                        'evidence
                        b1-evidence-rel
                        'branch
                        b1-wave-branch
                        'attempt-id
                        "attempt-5"
                        'attempt-fence
                        2)
                'head
                (make-string 40 #\3)))
    (define receipt
      (b1-write-receipt! record-delivery-receipt! root plan 0 w #:history (list prior-receipt)))
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
    (define bin (b1-install-gh-shim! w))
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
          (b1-controller-environment w bin)
          'bin
          bin
          'rec
          rec))

  (define (with-review-world kind proc)
    (define world (open-review-world kind))
    (dynamic-wind void (lambda () (proc world)) (lambda () (b1-cleanup-world! (hash-ref world 'w)))))

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

  ;; The /go request owns the reviewer runner (go-orchestrator wiring).
  ;; The REAL review-session module still resolves the clean exact-receipt
  ;; worktree, builds its credential-scrubbed fresh session, applies its
  ;; liveness/pin gates, parses and validates the final JSON and accounts
  ;; usage; ONLY its agent-loop transport is deterministic here (the deep
  ;; model-transport path stays in the slow owner). Returns (values
  ;; session-count request-bound-runner).
  (define (build-go-review-session world)
    (define root (hash-ref world 'root))
    (define plan (hash-ref world 'plan))
    (define sessions (box 0))
    (define (session-run session prompt)
      (set-box! sessions (add1 (unbox sessions)))
      ;; BUG-0084: the review thread must never inherit an author phase.
      (check-false (current-gsd-author-phase) "reviewer ran inside an author phase")
      (check-true (string-contains? prompt "FRESH independent non-author reviewer")
                  "the reviewer did not receive the fresh non-author mandate")
      (check-true (file-exists? (build-path (current-directory) "src/file.txt"))
                  "reviewer did not open the committed receipt checkout")
      (check-equal? (environment-variables-ref (current-environment-variables) #"GIT_TERMINAL_PROMPT")
                    #"0"
                    "reviewer environment was not scrubbed")
      (define final
        (make-message "b1-review-final"
                      #f
                      'assistant
                      'message
                      (list (make-text-part (jsexpr->string (b1-rejected-runner-result))))
                      (current-seconds)
                      (hasheq)))
      (make-loop-result (list final)
                        'completed
                        (hasheq 'usage
                                (hasheq 'prompt_tokens 3 'completion_tokens 4 'total_tokens 7))))
    (define real-reviewer (make-auto-review-runner root plan #:session-run session-run))
    (define go-request
      (make-campaign-request root (hash-ref world 'rec) void void #:auto-review-runner real-reviewer))
    (check-eq? (campaign-request-auto-review-runner go-request) real-reviewer)
    (values sessions (lambda (request) ((campaign-request-auto-review-runner go-request) request))))

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
       ;; The /go request owns the reviewer runner (go-orchestrator wiring),
       ;; backed by the real review-session gates with a deterministic agent
       ;; loop transport; its typed REQUEST_CHANGES then crosses the real
       ;; controller/finalize/Git boundary in this one case.
       (define-values (sessions session-runner) (build-go-review-session world))
       (define calls (box 0))
       (define (wired-runner request)
         (set-box! calls (add1 (unbox calls)))
         ;; Exact wiring: the typed request carries the durable receipt
         ;; identity and the dispatch generation, verbatim.
         (check-equal? (hash-ref request 'head) head)
         (check-equal? (hash-ref request 'branch) b1-wave-branch)
         (check-equal? (hash-ref request 'attempt-id) "attempt-5")
         (check-equal? (hash-ref request 'attempt-fence) 2)
         (check-equal? (hash-ref request 'generation) 1)
         (check-true (hash-ref request 'fresh-session-required))
         (check-true (hash-ref request 'non-author-review-required))
         (check-equal? (hash-ref request 'kind) "implementation-review")
         (check-equal? (hash-ref request 'staging) (review-dispatch-staging-path root plan 0 1))
         (check-true (non-empty-string? (hash-ref request 'prompt)))
         (check-regexp-match (regexp (regexp-quote head)) (hash-ref request 'prompt))
         (define verdict (session-runner request))
         (check-equal? (hash-ref verdict 'kind) "request-changes")
         (check-regexp-match #px"^q-session:" (hash-ref verdict 'reviewer ""))
         verdict)
       (define result (dispatch world wired-runner b1-manual-controller-seam))
       (check-equal? (unbox sessions) 1 "exactly one fresh reviewer session must open")
       ;; -- the typed rejection outcome, bound to the pinned identity.
       (check-eq? (delivery-effect-result-kind result)
                  'review-rejected
                  (format "dispatch outcome: ~s" (delivery-effect-result-data result)))
       (define data (delivery-effect-result-data result))
       (check-equal? (hash-ref data 'head) head)
       (check-equal? (hash-ref data 'branch) b1-wave-branch)
       (check-equal? (hash-ref data 'attempt-id) "attempt-5")
       (check-equal? (hash-ref data 'attempt-fence) 2)
       (check-equal? (hash-ref data 'generation) 1)
       (check-equal? (hash-ref data 'postmerge) #f)
       (check-equal? (hash-ref data 'findings) b1-gate-red-findings)
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
       (define tip (b1-remote-tip w))
       (check-not-false tip)
       (check-not-equal? tip head "the controller must have published the rejection tail")
       (define review-artifact (b1-generation-rel b1-review-rel 1))
       (define shown (b1-git! (hash-ref w 'origin) "show" (format "~a:~a" tip review-artifact)))
       (define datum (read (open-input-string shown)))
       (check-equal? (hash-ref datum 'verdict) "REQUEST_CHANGES")
       (check-equal? (hash-ref datum 'reviewed-sha) head)
       (check-equal? (hash-ref datum 'head) head)
       (check-equal? (hash-ref datum 'branch) b1-wave-branch)
       (check-equal? (hash-ref datum 'attempt-id) "attempt-5")
       (check-equal? (hash-ref datum 'attempt-fence) 2)
       (check-equal? (hash-ref datum 'findings) b1-gate-red-findings)
       ;; -- the REAL typed rejection checkpoint records the marker.
       (check-eq? (resolve-review-checkpoint! root plan 0 data) #t)
       (define after (load-delivery-journal root plan 0))
       (check-equal? (journal-review-rejection-count after) 1)
       (define marker (hash-ref after 'review-rejection #f))
       (check-true (hash? marker))
       (check-equal? (hash-ref marker 'findings) b1-gate-red-findings)
       (check-equal? (hash-ref marker 'attempt-id) "attempt-5")
       (check-equal? (hash-ref marker 'attempt-fence) 2)
       (check-equal? (hash-ref marker 'head) head)
       (check-equal? (hash-ref marker 'branch) b1-wave-branch)
       ;; -- receipt-history immutability across the rejection.
       (check-equal? (hash-ref after 'receipt-history '()) history)
       ;; -- the wave is durably failed by the same checkpoint.
       (define reloaded-record (load-campaign-record root plan))
       (check-eq? (campaign-wave-status (car (campaign-record-waves reloaded-record))) 'failed)
       ;; -- rejection-checkpoint recovery: the REAL production recovery
       ;;    reader accepts the durable marker against the still-live receipt
       ;;    and returns the typed retry input; its identity is the exact
       ;;    Git-published rejection checked above.
       (define recovery (pending-review-rejection-data after))
       (check-true (hash? recovery))
       (check-equal? (hash-ref recovery 'findings) b1-gate-red-findings)
       (check-equal? (hash-ref recovery 'postmerge) #f)
       (check-equal? (hash-ref recovery 'head) head)
       (check-equal? (hash-ref recovery 'branch) b1-wave-branch)
       (check-equal? (hash-ref recovery 'attempt-id) "attempt-5")
       (check-equal? (hash-ref recovery 'attempt-fence) 2)
       (check-equal? (unbox calls) 1 "only the fresh reviewer may be consulted"))))

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
       (define tip-before (b1-remote-tip w))
       (define result
         (dispatch world (no-reviewer "prohibited publication") b1-owned-controller-seam))
       (check-eq? (delivery-effect-result-kind result)
                  'review-infra
                  (format "refusal outcome: ~s" (delivery-effect-result-data result)))
       (define reason (hash-ref (delivery-effect-result-data result) 'reason ""))
       (check-true (string-contains? reason "missing or invalid authenticated owned proof")
                   (format "unexpected refusal reason: ~a" reason))
       ;; Nothing published, nothing journaled, the reviewer never consulted.
       (check-equal? (b1-remote-tip w) tip-before)
       (define journal (load-delivery-journal root plan 0))
       (check-equal? (hash-ref journal 'stage) "context-ready")
       (check-false (hash-ref journal 'review-rejection #f))
       (check-equal? (length (hash-ref journal 'receipt-history '())) 1)
       (define staging (review-dispatch-staging-path root plan 0 1))
       (check-false (file-exists? (build-path staging "reviewer-output.json"))))))

  ;; Warm the immutable Git template once before any case thread copies it.
  (b1-warm-review-template!)
  (start-case!
   'red-verification
   (lambda ()
     (dynamic-wind void case-red-verification! (lambda () (semaphore-post red-case-finished)))))
  (start-case! 'rejection case-rejection-and-replay!)
  ;; Start the refusal only after the Verify probes finish: it is a single
  ;; controller phase and must not contend with the real rejection finalize.
  (start-case! 'refusal
               case-prohibited-publication!
               #:wait (lambda () (semaphore-wait red-case-finished)))

  (test-case "/go request opens one fresh review, rejects and journals at the real Git boundary"
    (finish-case! 'rejection))
  (test-case "one genuine owned Verify-command execution keeps honest success/failure/timeout exit-truth"
    (finish-case! 'red-verification))
  (test-case "production owned-mode dispatch refuses without an authenticated owned proof"
    (finish-case! 'refusal)))
