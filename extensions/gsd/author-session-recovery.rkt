#lang racket/base
;; Owner-only completion of a lost evidence-author launch. Called at the /go
;; checkpoint under its campaign lease, never from a model phase. The local
;; semaphore also serializes direct checkpoint callers. No launch permission is
;; created here: only real frozen Verify and existing receipt certification.
(require racket/list
         racket/path
         racket/string
         "author-scope.rkt"
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         "delivery-receipt.rkt"
         "delivery-verifier.rkt"
         "delivery-publication.rkt"
         "evidence-author-journal.rkt"
         "review-proof.rkt"
         "wave-runner-port.rkt"
         (only-in "system-adapters.rkt" run-wave-with-timeout)
         (only-in "composition-root.rkt" current-gsd-verification-registry)
         (only-in "verification-job.rkt" make-verification-registry verification-close-registry!)
         (only-in "wave-executor.rkt" load-plan-from-index)
         (only-in "attempt-artifacts.rkt" record-wave-delivery!)
         (only-in "../../sandbox/subprocess.rkt"
                  run-subprocess subprocess-result-exit-code subprocess-result-stdout
                  subprocess-result-timed-out? subprocess-result-truncated?))
(provide recover-lost-author-session!
         author-publication-recovery
         current-gsd-author-recovery-timeout-sec)
(define recovery-lock (make-semaphore 1))
;; Operational deadline only, never Verify authority. Leave the full four-hour
;; default Verify window plus an hour for bounded history/proof/publication.
;; Tests may shorten the OUTER window without shortening real declared gates.
(define current-gsd-author-recovery-timeout-sec
  (make-parameter #f
                  (lambda (v)
                    (unless (or (not v) (and (rational? v) (positive? v)))
                      (raise-argument-error 'current-gsd-author-recovery-timeout-sec
                                            "(or/c #f positive-finite-real?)" v))
                    v)))
(define (call-with-recovery-deadline thunk)
  (define seconds (or (current-gsd-author-recovery-timeout-sec)
                      (+ 3600 (current-gsd-delivery-verify-timeout-sec))))
  (need (and (rational? seconds) (positive? seconds)) "non-finite owner recovery deadline")
  (define cust (make-custodian))
  ;; Never close an ambient registry: only this recovery's children belong here.
  (define registry (make-verification-registry))
  (define worker #f)
  (define result #f)
  (define (stop!)
    ;; Close admission atomically with spawn BEFORE interrupting the worker.
    ;; This prevents a half-registered verifier or a launch after cancellation.
    (verification-close-registry! registry)
    (when (and worker (not (thread-dead? worker))) (break-thread worker))
    (void))
  (dynamic-wind
   void
   (lambda ()
     (define outcome
       (parameterize ([current-custodian cust]
                      [current-gsd-verification-registry registry])
         (run-wave-with-timeout
          (make-wave-runner-port
           (lambda (_)
             (set! worker (current-thread))
             (with-handlers ([exn:break? (lambda (_) (wave-execution-outcome 'interrupted "owner recovery stopped"))])
               (set! result (thunk))
               (wave-execution-outcome 'done "owner recovery finished")))
           #:cancel! stop!)
          seconds 0)))
     (need (eq? (wave-execution-outcome-kind outcome) 'done)
           "owner recovery deadline exceeded; running author retained; resume /go after inspection")
     result)
   (lambda ()
     (parameterize-break #f
       (stop!)
       ;; Also honor cancellation of the CALLER, not just the timeout path:
       ;; let the worker unwind its sandbox process-group cleanup first.
       (when worker (sync/timeout 1.5 worker))
       (custodian-shutdown-all cust)))))
(define (author-publication-recovery dir plan wave)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define replay
      (call-with-recovery-deadline
       (lambda () (author-publication-recovery/unbounded dir plan wave))))
    (and replay (lambda () (call-with-recovery-deadline replay)))))
(define (recover-lost-author-session! root plan wave #:session-live? session-live?)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (call-with-recovery-deadline
     (lambda ()
       (recover-lost-author-session/unbounded! root plan wave #:session-live? session-live?)))))
;; Unlike scalar Git facts, NUL-delimited filenames must NEVER be trimmed:
;; leading whitespace is part of a pathname, not presentation decoration.
;; Strict decoding is equally mandatory: replacement decoding aliases an
;; invalid-byte filename to a DIFFERENT, literal U+FFFD frozen pathname.
(define (commit-paths cwd commit)
  (define r
    (run-subprocess "git"
                    #:args (list "-C" cwd "diff-tree" "--root" "--no-commit-id" "--name-only"
                                 "--no-renames" "-r" "-m" "-z" commit)
                    #:directory cwd #:timeout 15 #:process-group? #t
                    #:encoding 'utf-8/strict))
  (and (not (subprocess-result-timed-out? r))
       (not (subprocess-result-truncated? r))
       (equal? (subprocess-result-exit-code r) 0)
       (subprocess-result-stdout r)))
(define (need v message)
  (unless v (error 'author-session-recovery "~a" message))
  v)
;; Existing confirmed-publication crash replay. No new Verify/model dispatch.
(define (author-publication-recovery/unbounded dir plan wave)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define journal (load-delivery-journal dir plan wave))
    (define marker (journal-evidence-author-repair journal))
    (define pub (and marker (load-approved-publication dir plan wave)))
    (define candidate (and pub (hash-ref pub 'receipt-candidate #f)))
    (define old (and marker (hash-ref marker 'old-receipt #f)))
    (and marker pub candidate old
         (evidence-author-repair-pending? journal)
         (equal? (hash-ref marker 'state #f) "running")
         (equal? (hash-ref pub 'status #f) "confirmed")
         (load-receipt-owned-proof candidate dir plan wave)
         (equal? (hash-ref pub 'attempt-id #f) (hash-ref marker 'successor-attempt #f))
         (equal? (hash-ref pub 'attempt-fence #f) (hash-ref marker 'successor-fence #f))
         (equal? (hash-ref pub 'snapshot-digest #f)
                 (hash-ref (hash-ref marker 'context #f) 'snapshot-sha256 #f))
         (for/and ([k '(repo origin branch)])
           (equal? (hash-ref pub k #f) (hash-ref old k #f)))
         (strict-descendant? (hash-ref pub 'repo) (hash-ref old 'head) (hash-ref pub 'head))
         (lambda ()
           (call-with-semaphore
            recovery-lock
            (lambda ()
              (define pub-fence (hash-ref pub 'coordinator-fence #f))
              (define (current!)
                (define rec (load-campaign-record dir plan))
                (define w
                  (and rec (current-wave-for-attempt rec wave
                                                    (hash-ref candidate 'attempt-fence)
                                                    (hash-ref candidate 'attempt-id))))
                (need (and rec w
                           (not (campaign-record-cancellation rec))
                           (not (campaign-record-budget-pause rec))
                           (equal? (campaign-fence-token rec) pub-fence)
                           (memq (campaign-wave-status w) '(done awaiting-delivery))
                           (equal? (campaign-record-plan-snapshot-digest rec) (hash-ref pub 'snapshot-digest))
                           (member (campaign-wave-delivery-branch w) (list "" (hash-ref candidate 'branch)))
                           (member (campaign-wave-delivery-head-sha w)
                                   (list "" (hash-ref old 'head) (hash-ref candidate 'head))))
                      "stale confirmed author publication")
                rec)
              (current!)
              ;; Bind the genuine approved head BEFORE certification so a crash
              ;; after receipt supersession cannot strand stale wave provenance.
              (record-wave-delivery! dir plan wave (hash-ref candidate 'branch) (hash-ref candidate 'head))
              (replay-approved-publication! dir plan wave current!)
              (hasheq 'status "recovered"
                      'message "author successor publication recovered; receipt supersession replayed")))))))
(define (recover-lost-author-session/unbounded! root plan wave #:session-live? session-live?)
  (call-with-semaphore
   recovery-lock
   (lambda ()
     (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
       (define j (load-delivery-journal root plan wave))
       (define m (journal-evidence-author-repair j))
       (cond
         [(not (and (evidence-author-repair-pending? j)
                    (equal? (hash-ref m 'state #f) "running"))) #f]
         [(session-live?) #f] ; conservative default preserves ambiguous-live pause
         [else
          (define cwd (hash-ref (hash-ref m 'context) 'repo))
          (define source
            (need (committed-delivery-snapshot cwd)
                  (string-append
                   "execution checkout is dirty or unavailable; preserve all files, "
                   "explicitly account for staged, unstaged and untracked changes, "
                   "then resume /go (no automatic discard or copy)")))
          (define old (hash-ref m 'old-receipt))
          ;; Receipt repo is the common Git repository, NOT execution CWD.
          ;; committed-delivery-snapshot resolves --git-common-dir, so q/ and
          ;; its retained worktree must agree without aliasing arbitrary clones.
          (need (for/and ([k '(repo branch origin)])
                  (equal? (hash-ref source k) (hash-ref old k)))
                "execution checkout repository/branch/origin differs from retained receipt")
          (cond
            [(equal? (hash-ref source 'tree) (hash-ref old 'tree))
             ;; BUG-0085 unchanged-tree disposition remains truthful.
             (abandon-evidence-author-session!
              root plan wave (hash-ref source 'tree)
              #:guard (lambda ()
                        (and (not (session-live?))
                             (equal? source (committed-delivery-snapshot cwd)))))
             #t]
            [else (finish-committed-author! root plan wave cwd source j m session-live?)])])))))
(define (finish-committed-author! root plan wave cwd source j m session-live?)
  (define old (hash-ref m 'old-receipt))
  (define record (load-campaign-record root plan))
  (define attempt-id (hash-ref m 'successor-attempt))
  (define attempt-fence (hash-ref m 'successor-fence))
  (define active-fence (and record (campaign-fence-token record)))
  (define frozen (frozen-verify-binding root plan wave))
  (define old-proof
    (need (load-receipt-owned-proof old root plan wave)
          "retained receipt lacks authentic owned Verify; owner finish refused"))
  (need (equal? (simplify-path (path->directory-path cwd))
                (simplify-path (path->directory-path (hash-ref old-proof 'execution-cwd))))
        "author context differs from authenticated old execution checkout")
  (define files (frozen-author-files root plan wave))
  (define head (hash-ref source 'head))
  (define candidate #f)
  (define (current!)
    (need (not (session-live?)) "author session is live; owner finish refused")
    (define rec (load-campaign-record root plan))
    (define w
      (and rec (for/first ([w (in-list (campaign-record-waves rec))]
                          #:when (= wave (campaign-wave-index w))) w)))
    (define a (and w (campaign-wave-current-attempt w)))
    (need (and rec w a
               (equal? (campaign-plan-id rec) plan)
               (not (campaign-record-cancellation rec))
               (not (campaign-record-budget-pause rec))
               (memq (campaign-wave-status w) '(done awaiting-delivery))
               (equal? active-fence (campaign-fence-token rec))
               ;; Do not advance/rebind a stale implementation attempt.
               (equal? active-fence attempt-fence)
               (equal? (campaign-attempt-id a) attempt-id)
               (equal? (campaign-attempt-fence-token a) attempt-fence)
               (member (campaign-wave-delivery-branch w) (list "" (hash-ref source 'branch)))
               (member (campaign-wave-delivery-head-sha w) (list "" (hash-ref old 'head) head))
               (equal? (campaign-record-plan-snapshot-digest rec) (hash-ref frozen 'snapshot-sha256))
               (equal? frozen (frozen-verify-binding root plan wave))
               (equal? files (hash-ref m 'frozen-files))
               (equal? files (frozen-author-files root plan wave))
               (equal? (hash-ref (hash-ref m 'context) 'snapshot-sha256)
                       (hash-ref frozen 'snapshot-sha256))
               (equal? source (committed-delivery-snapshot cwd)))
          "stale successor/fence, paused campaign, frozen scope or clean snapshot drift; owner finish refused")
    (define now (load-delivery-journal root plan wave))
    ;; Certification calls its guard again after reconciliation. Accept ONLY
    ;; that exact candidate's completed transition, never an unrelated write.
    (need (or (equal? now j)
              (and candidate
                   (equal? (hash-ref now 'receipt #f) candidate)
                   (equal? (hash-ref (journal-evidence-author-repair now) 'state #f) "completed")
                   (equal? (hash-ref now 'receipt-history '())
                           (append (hash-ref j 'receipt-history '()) (list old)))))
          "author journal raced; owner finish refused")
    rec)
  (current!)
  (need (strict-descendant? (hash-ref source 'repo) (hash-ref old 'head) head)
        "committed successor is not a strict descendant of retained receipt")
  ;; Check ALL intervening changes, not just net diff (a reverted forbidden
  ;; edit is still outside the frozen author permission). -m checks merges
  ;; against every parent; --no-renames includes deletion/source paths.
  (define commits
    (need (delivery-git cwd "rev-list" (string-append (hash-ref old 'head) ".." head))
          "cannot enumerate successor ancestry"))
  (define history (string-split commits "\n"))
  (need (<= (length history) 128)
        "successor history exceeds bounded owner-finish scan; manual reconciliation required")
  (for ([commit (in-list history)])
    (define paths
      (need (commit-paths cwd commit) "cannot inspect committed author scope"))
    (for ([path (in-list (string-split paths "\u0000"))])
      (need (member path files) (format "committed author path outside frozen FILES: ~s" path))))
  (current!)
  (define prior (load-approved-publication root plan wave))
  (cond
    [(and prior (equal? (hash-ref prior 'attempt-id) attempt-id))
     ;; Publication may have been interrupted after its intent or confirmed
     ;; readback. Replay the SAME owned proof; do not regenerate a different
     ;; approval identity or launch any model phase.
     (set! candidate (hash-ref prior 'receipt-candidate))
     (need (and (for/and ([k '(repo branch origin head tree)])
                  (equal? (hash-ref candidate k) (hash-ref source k)))
                (equal? (hash-ref candidate 'attempt-fence) attempt-fence)
                (load-receipt-owned-proof candidate root plan wave))
           "existing successor publication differs from exact owned candidate")
     (need (equal? (hash-ref prior 'coordinator-fence) active-fence)
           "existing publication coordinator fence differs; owner finish refused")
     (publish-approved-head! root plan wave source attempt-id attempt-fence active-fence
                             (hash-ref candidate 'evidence) current!
                             #:owned-proof (hash-ref candidate 'owned-proof))]
    [else
     (define ctx
       (make-branch-delivery-context #:repo-root (hash-ref source 'repo)
                                     #:branch (hash-ref source 'branch)
                                     #:base-commit (hash-ref old 'head)
                                     #:worktree-path (string->path cwd)))
     (define plan-data (need (load-plan-from-index root) "missing declared plan"))
     (define-values (verdict capture)
       (parameterize ([current-gsd-delivery-branch-context ctx])
         (call-with-owned-proof-capture
          root plan wave source attempt-id attempt-fence
          (lambda () (run-delivery-verification root plan-data wave)) #:cwd cwd)))
     (current!)
     (need (delivery-verification-approved? verdict)
           (format "full frozen Verify failed; running author retained: ~a; ~s"
                   (delivery-verification-message verdict) (delivery-verification-evidence verdict)))
     (define proof (and capture (owned-proof-capture-reference capture)))
     (need proof "full frozen Verify lacks authentic owned proof; running author retained")
     (publish-approved-head! root plan wave source attempt-id attempt-fence active-fence
                             (format "~s" (delivery-verification-evidence verdict))
                             current! #:owned-proof proof)
     (set! candidate (hash-ref (load-approved-publication root plan wave) 'receipt-candidate))])
  ;; Durable publication precedes provenance; provenance precedes receipt
  ;; supersession. Every crash cut is resumable using the same owned proof.
  (current!)
  (record-wave-delivery! root plan wave (hash-ref source 'branch) head)
  (current!)
  (certify-publication-receipt! root plan wave candidate
                                #:expected-attempt-id attempt-id #:expected-fence attempt-fence
                                #:guard (lambda () (current!) #t))
  (need (and candidate
             (equal? (hash-ref (load-delivery-journal root plan wave) 'receipt) candidate))
        "owner Verify completed but successor receipt was not reconciled")
  #t)
