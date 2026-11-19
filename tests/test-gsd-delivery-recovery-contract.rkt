#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 300
;; @covers extensions/gsd/delivery-recovery.rkt
;; E-wave required-PR contract for lost-delivery recovery (milestone #899).
;; The deep recovery world stays in tests/test-gsd-delivery-recovery.rkt
;; (slow/full regression); THIS file is its required-PR counterpart and keeps
;; the family's @covers claim on a focused real-boundary slice:
;;
;;   * the REAL clone resolver with a byte-for-byte honest dry-run (every
;;     root byte unchanged, campaign lock never acquired);
;;   * REAL apply-mode reconciliation through `recover-delivery!`: the
;;     genuine repair-tail verify (green exit-truth) reconciles receipt +
;;     provenance + handoff without finalizing or bumping the fence;
;;   * stale-authority refusal truth (fence/attempt/head/base/cancellation)
;;     before any clone or verify side effect;
;;   * genuine RED Verify exit-truth: an undeclared declared-target drift
;;     after the recorded receipt must refuse with `verify-failed`, leaving
;;     every root byte untouched except the sanctioned lease file;
;;   * BUG-0082 binding truth: the candidate verify executes the DECLARED
;;     carrier doc's gate, never the stale synthesized twin's (real command,
;;     real exit truth, named in the refusal details).
;;
;; No mock-only security decision: every verdict above is produced by the
;; real recovery pipeline over real Git state.
(require rackunit
         racket/file
         racket/path
         racket/string
         (only-in "helpers/private-fixture-templates.rkt" git-quiet!)
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-recovery.rkt"
         "helpers/e-required-retier-fixture.rkt")

(define plan recovery-plan)
(define attempt-id recovery-attempt-id)
(define fence recovery-fence)
(define branch recovery-branch)

(define (with-lock-bytes root proc)
  (define lock (campaign-lock-path root))
  (make-parent-directory* lock)
  (display-to-file "preexisting-lock-bytes" lock #:exists 'truncate)
  (define before (file->bytes lock))
  (dynamic-wind void
                (lambda () (proc before))
                (lambda ()
                  (when (file-exists? lock)
                    (delete-file lock)))))

(module+ test
  (test-case "dry-run keeps every root byte and the lock untouched; apply reconciles through a real green verify"
    (with-fixture
     (lambda (root bare repo base old-head head)
       ;; Dry-run truth: real clone resolver, zero effects, no lock rewrite.
       (with-lock-bytes root
                        (lambda (lock-before)
                          (define before (tree-bytes root))
                          (define result (call-recover root bare base old-head head))
                          (check-eq? (recovery-result-status result) 'dry-run (format "~s" result))
                          (check-equal? (file->bytes (campaign-lock-path root))
                                        lock-before
                                        "dry-run must not acquire or rewrite the campaign lock")
                          (check-equal? (tree-bytes root) before)))
       ;; Apply truth: real repair-tail verify passes on the honest candidate.
       (define before-fence (campaign-fence-token (load-campaign-record root plan)))
       (define applied (call-recover root bare base old-head head #:apply? #t))
       (check-eq? (recovery-result-status applied) 'applied (format "~s" applied))
       (define journal (load-delivery-journal root plan 0))
       (check-equal? (hash-ref (hash-ref journal 'receipt) 'head) head)
       (check-equal? (hash-ref (car (hash-ref journal 'receipt-history)) 'head) old-head)
       (define rec (load-campaign-record root plan))
       (define w (car (campaign-record-waves rec)))
       (check-equal? (campaign-fence-token rec) before-fence "fence unchanged")
       (check-eq? (campaign-wave-status w) 'awaiting-delivery "not finalized")
       (check-equal? (campaign-wave-delivery-head-sha w) head)
       (check-eq? (delivery-handoff-status root plan 0) 'delivery-pending)
       (check-false (directory-exists? (build-path root ".planning" "outbox"))))))

  (test-case "stale authority refuses before any clone or verify side effect"
    (with-fixture
     (lambda (root bare repo base old-head head)
       (check-eq? (recovery-result-reason (call-recover root bare base old-head head #:fence* 99))
                  'stale-fence)
       (check-eq?
        (recovery-result-reason (call-recover root bare base old-head head #:attempt* "other"))
        'stale-attempt)
       (check-eq?
        (recovery-result-reason (call-recover root bare base old-head head #:expected-head* old-head))
        'candidate-head-mismatch)
       (check-eq?
        (recovery-result-reason (call-recover root bare base old-head head #:expected-base* head))
        'stale-base)
       (define rec (load-campaign-record root plan))
       (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))
       (persist-campaign! root rec)
       (check-eq? (recovery-result-reason (call-recover root bare base old-head head)) 'cancelled))))

  (test-case "apply-mode RED verify leaves no restored journal and no root effects beyond the lease"
    (with-fixture
     (lambda (root bare repo base old-head head)
       ;; Declared-target regression on the candidate branch: every strict
       ;; pre-check passes, but the production repair-tail verify must
       ;; refuse it ("target file changed after the recorded receipt").
       (write-rkt (build-path repo "ui-core" "preferences.rkt") 3)
       (git-quiet! repo "add" "-A")
       (git-quiet! repo "commit" "-q" "-m" "undeclared drift")
       (define drifted (git-out repo "rev-parse" "HEAD"))
       (git-quiet! repo "push" "-q" "origin" branch)
       (git-quiet! repo "fetch" "-q" "origin" (string-append branch ":refs/remotes/origin/" branch))
       (define-values (active _superseded _bytes) (park-active-as-superseded! root))
       (define before (tree-bytes root))
       (define result
         (call-recover root bare base old-head drifted #:expected-head* drifted #:apply? #t))
       (check-eq? (recovery-result-status result) 'refused (format "~s" result))
       (check-eq? (recovery-result-reason result) 'verify-failed)
       (check-false (file-exists? active) "refusal must not restore the journal")
       ;; The campaign lock file is the single sanctioned apply-mode write:
       ;; the lease's owner-diagnostics path (same protocol as /go). Every
       ;; other root byte — records, journals, handoffs — must be untouched.
       (define lock-path (campaign-lock-path root))
       (define (without-lock bytes)
         (filter (lambda (entry) (not (equal? (car entry) (find-relative-path root lock-path))))
                 bytes))
       (check-equal? (without-lock (tree-bytes root))
                     (without-lock before)
                     "refusal must not touch any root byte except the lease file"))))

  (test-case "BUG-0082: the copied plan's DECLARED identity drives the candidate verify"
    ;; The plan declares W0 through a carrier doc; the original synthesized
    ;; waves/W0-zero.md stays behind as a STALE twin with a different (weak)
    ;; verify. Both travel into the byte-faithful verification copy. The
    ;; candidate verify must execute the DECLARED carrier's gate (its
    ;; distinctive failure names it) — never the stale twin's.
    (with-fixture
     (lambda (root bare repo base old-head head)
       (define (put! rel text)
         (define p (build-path root ".planning" rel))
         (make-parent-directory* p)
         (call-with-output-file p (lambda (out) (display text out)) #:exists 'truncate))
       (put! "PLAN.md" "# Plan\n\n- [Inbox] W0: Recovery → waves/W0-recovery-carrier.md\n")
       (put! "waves/W0-recovery-carrier.md"
             (string-append
              "# Wave 0\nStatus: Inbox\n\n"
              "## Files\n- File: q/ui-core/preferences.rkt\n\n"
              "## Verify\n- `bash -c 'test -f recovery-carrier-marker-8e2.txt'` green.\n\n"
              "## Done\n- carrier done\n"))
       (put! "waves/W0-zero.md"
             (string-append "# Wave 0\nStatus: Inbox\n\n"
                            "## Files\n- File: q/ui-core/preferences.rkt\n\n"
                            "## Verify\n- `bash -c 'test -f stale-weak-2b1d.txt'` green.\n\n"
                            "## Done\n- stale done\n"))
       (define before (tree-bytes root))
       (define result (call-recover root bare base old-head head))
       (check-eq? (recovery-result-status result) 'refused (format "~s" result))
       (check-eq? (recovery-result-reason result) 'verify-failed)
       (define message (format "~a" (car (recovery-result-details result))))
       (check-true (string-contains? message "recovery-carrier-marker-8e2")
                   "the copied plan's DECLARED doc drove the candidate verify")
       (check-false (string-contains? message "stale-weak-2b1d")
                    "the stale synthesized twin never drives the verify")
       (check-equal? (tree-bytes root) before "dry-run refusal leaves the root untouched")))))
