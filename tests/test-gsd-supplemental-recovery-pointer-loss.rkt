#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 1200
(require rackunit
         racket/file
         "helpers/supplemental-go-fixture.rkt"
         "helpers/honest-delivery-fixture.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd-planning/execution-policy.rkt")

;; Snapshot every existing private/receipt/proof artifact, not just the claim.
(define (protected-bytes root private)
  (for/hash ([path (in-directory root)]
             #:when (and (file-exists? path)
                         (or (path-has-prefix? path private)
                             (regexp-match? #rx"/owned-proofs/|/coordinator-w[0-9]+[.]json$" (path->string path)))))
    (values path (file->bytes path))))
(define (path-has-prefix? path prefix)
  (define parts (explode-path path))
  (define base (explode-path prefix))
  (and (<= (length base) (length parts)) (equal? base (take parts (length base)))))

(define (check-storage-policy)
  (test-case "readonly root policy: absent is inert; invalid or ambiguous stops"
    (define outer (make-temporary-file "pointer-claim-policy-~a" 'directory))
    (define root (build-path outer "private"))
    (define pointer (build-path outer "pointer.json"))
    (define (claimed?) (supplemental-recovery-pointer-claimed? pointer))
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([current-supplemental-authority-root #f]) (check-false (claimed?)))
        (parameterize ([current-supplemental-authority-root root])
          (check-false (claimed?))
          (check-false (directory-exists? root))
          (make-directory root #o700)
          (check-false (claimed?))
          (check-equal? (directory-list root) '() "lookup creates no locks or files")
          (file-or-directory-permissions root #o755)
          (check-exn exn:fail? claimed?)
          (file-or-directory-permissions root #o700)
          (define invalid (build-path root "supplemental-recovery-invalid.json"))
          (display-to-file "{}" invalid)
          (file-or-directory-permissions invalid #o600)
          (check-exn exn:fail? claimed?)
          (delete-file invalid)
          (make-file-or-directory-link pointer invalid)
          (check-exn exn:fail? claimed?)
          (delete-file invalid))
        (define linked (build-path outer "linked"))
        (make-file-or-directory-link root linked)
        (parameterize ([current-supplemental-authority-root linked])
          (check-exn exn:fail? claimed?))
        (parameterize ([current-supplemental-authority-root (build-path outer "missing" "nested")])
          (check-false (claimed?)))
        (parameterize ([current-supplemental-authority-root "relative"])
          (check-exn exn:fail? claimed?))
        ;; A malformed entry (including a dangling link/directory) is block-only.
        (define public (supplemental-pointer-path outer "plan" 1))
        (make-directory* (path-only public))
        (parameterize ([current-supplemental-authority-root #f])
          (make-directory public)
          (check-true (supplemental-pointer-retained? outer "plan" 1))
          (check-false (supplemental-author-pending? outer "plan" 1))
          (delete-directory public)
          (make-file-or-directory-link pointer public)
          (check-true (supplemental-pointer-retained? outer "plan" 1))
          (check-false (supplemental-author-pending? outer "plan" 1))))
      (lambda () (delete-directory/files outer)))))
(module+ storage-policy (check-storage-policy))
(module+ test
  (check-storage-policy)
  (for* ([policy '(#f owned-red-v1)] [loss '(deleted malformed)])
    (test-case (format "durable claim denies pointer ~a, policy ~a" loss policy)
      (call-with-supplemental-fixture
       (lambda (fx)
         (define root (supplemental-fixture-root fx))
         (define plan (supplemental-fixture-plan fx))
         (define private (supplemental-fixture-private fx))
         (fixture-finalize-predecessor! fx)
         (define calls 0)
         (define ordinary 0)
         (define coordinator 0)
         (define verifier 0)
         (define (go)
           (run-campaign!
            root (load-campaign-record root plan)
            #:isolate? #f #:timeout-sec 30
            #:delivery-reader (test-delivered-reader)
            #:runner (lambda (_)
                       (if (current-gsd-author-phase)
                           (set! calls (add1 calls))
                           (set! ordinary (add1 ordinary)))
                       'ok)
            #:delivery-coordinator (lambda _ (set! coordinator (add1 coordinator))
                                     (error 'test "ordinary coordinator forbidden"))
            #:verifier (lambda _ (set! verifier (add1 verifier)) #f)))
         (void (go))
         (check-equal? calls 1 "genuine spent parent launch")
         (define path (supplemental-pointer-path root plan 1))
         (define pointer (load-supplemental-pointer root plan 1))
         (define pointer-bytes (file->bytes path))
         (define-values (child task)
           (propose-supplemental-recovery!
            root plan 1 (hash-ref (hash-ref pointer 'task-proposal) 'sha256)
            (+ (current-seconds) 3600)))
         (define approval (fixture-approve! child))
         (parameterize ([current-supplemental-journal-crash-cut
                         (lambda (cut) (when (eq? cut 'recovery-claim-written) (error 'cut "claim")))])
           (check-exn exn:fail? (lambda () (install-supplemental-recovery! root plan 1 child approval))))
         ;; Exercise both campaign routing policies against the same genuinely
         ;; consumed parent. Policy is not an ownership or approval shortcut.
         (define rec (load-campaign-record root plan))
         (set-campaign-record-execution-policy! rec policy)
         (persist-campaign! root rec)
         (define protected (protected-bytes root private))
         (check-true (supplemental-recovery-pointer-claimed? path))
         (check-false (supplemental-recovery-pointer-claimed? (build-path root "unrelated.json")))
         (check-false (supplemental-author-pending? root plan 1))
         (for ([damage (list 'intact loss)])
           (case damage
             [(deleted) (delete-file path)]
             [(malformed) (display-to-file "not-json" path #:exists 'truncate)])
           (define result (go))
           (check-equal? (campaign-result-status result) 'wave-blocked (format "~a" damage))
           (check-equal? calls 1 "no supplemental redispatch")
           (check-equal? ordinary 0 "no ordinary runner")
           (check-equal? coordinator 0 "no ordinary coordinator")
           (check-equal? verifier 0 "no ordinary verifier")
           (for ([(path bytes) (in-hash protected)])
             (check-equal? (file->bytes path) bytes (path->string path)))
           ;; Restore only the public pointer: replay still uses production CAS.
           (display-to-file pointer-bytes path #:exists 'truncate)
           (file-or-directory-permissions path #o600))
         (check-not-exn (lambda () (install-supplemental-recovery! root plan 1 child approval)))
         ;; Revocation denies authorization, never erases durable ownership.
         (define rev (propose-retain! "revocation-proposal" (hasheq 'reason "stop") (list approval)))
         (revoke! rev (fixture-interactive-grant rev))
         (delete-file path)
         (check-true (supplemental-recovery-pointer-claimed? path))
         (check-true (supplemental-pointer-retained? root plan 1))
         (check-false (supplemental-author-pending? root plan 1))
         (check-equal? (campaign-result-status (go)) 'wave-blocked)
         (check-equal? calls 1)
         (check-equal? (list ordinary coordinator verifier) '(0 0 0))
         (for ([(path bytes) (in-hash protected)])
           (check-equal? (file->bytes path) bytes (path->string path))))
       #:completed-author? #t
       #:physical-worktree? #t))))
