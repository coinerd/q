#lang racket/base
;; B3-owned real-boundary fixture for
;; tests/test-gsd-generated-metadata-correction-contract.rkt (milestone #899).
;; It deliberately shares NO mutable surface with the slow owner's
;; tests/helpers/lost-author-fixture.rkt. Everything security-relevant is the
;; real production path:
;;   * real Git repositories (bare origin + private execution checkout whose
;;     logical GitHub origin is re-pointed at the local bare remote);
;;   * real frozen plan snapshot and real campaign record/fence/attempt;
;;   * ONE focused Verify stage through `check-verify-command`, the real
;;     verification registry and the real owned-proof producer — the frozen
;;     declaration contains only that one command, so no Verify campaign is
;;     replayed to mint the old receipt;
;;   * the real production receipt writer, evidence-author reservation and
;;     owner-correction propose/authorize/apply/publication APIs.
;;
;; The only deterministic substitution is the tiny probe program's exit mode;
;; its exit code is produced by rackunit and observed by the real registry.
(require racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         json
         file/sha1
         openssl
         "delivery-fixtures.rkt"
         "private-fixture-templates.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/campaign-repository.rkt"
         "../../extensions/gsd/delivery-journal.rkt"
         "../../extensions/gsd/delivery-receipt.rkt"
         "../../extensions/gsd/delivery-publication.rkt"
         "../../extensions/gsd/delivery-verifier.rkt"
         "../../extensions/gsd/evidence-author-journal.rkt"
         "../../extensions/gsd/evidence-readiness.rkt"
         "../../extensions/gsd/plan-snapshot.rkt"
         (only-in "../../extensions/gsd/generated-readme.rkt" generated-metrics)
         "../../extensions/gsd/review-proof.rkt"
         (only-in "../../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../../extensions/gsd/verification-job.rkt"
                  make-verification-registry
                  verification-close-registry!)
         (only-in "../../extensions/gsd/wave-executor.rkt" load-plan-from-index))
(provide b3-metadata-world!
         b3-metadata-cleanup!
         b3-put!
         b3-git!
         b3-git-output!
         b3-check!
         b3-owned-verify-command
         b3-red-verify-command
         b3-slow-verify-command)

(define git-executable (find-executable-path "git"))
(define-runtime-path metrics-script "../../scripts/metrics.rkt")
(define-runtime-path metrics-helpers "../../scripts/metrics-helpers.rkt")

(define b3-owned-verify-command "racket tests/b3-owned-probe.rkt green")
(define b3-red-verify-command "racket tests/b3-owned-probe.rkt red")
(define b3-slow-verify-command "racket tests/b3-owned-probe.rkt slow")

(define (b3-put! path text)
  (make-parent-directory* path)
  (display-to-file text path #:exists 'truncate))

(define (b3-git! repo . args)
  (define (run)
    (apply system*/exit-code git-executable args))
  (define exit
    (if repo
        (parameterize ([current-directory repo])
          (run))
        (run)))
  (unless (zero? exit)
    (error 'b3-metadata-fixture "git failed: ~s" args)))

(define (b3-git-output! repo . args)
  (string-trim (with-output-to-string (lambda () (apply b3-git! repo args)))))

(define (b3-plan-id seed)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 (format "b3-metadata-contract-~a-~a"
                                                                seed
                                                                (current-inexact-milliseconds))))))

(define probe-program
  (string-append "#lang racket/base\n"
                 "(require rackunit rackunit/text-ui)\n"
                 "(define mode (car (vector->list (current-command-line-arguments))))\n"
                 "(define (report suite) (exit (run-tests suite)))\n"
                 "(cond [(equal? mode \"green\") (report (test-suite \"b3 focused\"\n"
                 "                                     (test-case \"focused\"\n"
                 "                                       (check-equal? 21 21))))]\n"
                 "      [(equal? mode \"red\") (report (test-suite \"b3 focused\"\n"
                 "                                   (test-case \"focused\"\n"
                 "                                     (check-equal? 21 22))))]\n"
                 "      [(equal? mode \"slow\") (sync/timeout 30 (make-semaphore))])\n"))

(define (b3-check! root)
  (define check (check-verify-command root 0 #f))
  (delivery-verification (cadr check) (list check) (format "~a" check)))

;; Build one complete, genuinely owned world. Returns a hash of paths and the
;; REAL old receipt (whose owned proof came from the single focused stage).
(define (b3-metadata-world! #:seed [seed "world"])
  (define root (make-temporary-file "b3-metadata-~a" 'directory))
  (define repo (build-path root "q"))
  (define remote (build-path root "origin.git"))
  ;; Private owner authority MUST live outside both the campaign root and
  ;; the execution repository; the real store! guard enforces that boundary.
  (define store (make-temporary-file "b3-owner-authority-~a" 'directory))
  (define proofs (build-path root "owned-proofs"))
  (make-directory repo)
  (file-or-directory-permissions store #o700)
  (b3-git! #f "init" "-q" "--bare" (path->string remote))
  (b3-git! #f "init" "-q" "-b" "campaign/b3-seed" (path->string repo))
  (hermetic-identity! repo)
  (b3-git! repo "config" "gc.auto" "0")
  (b3-git! repo "remote" "add" "origin" "https://github.com/example/q.git")
  (b3-git! repo
           "config"
           (string-append "url.file://" (path->string remote) ".insteadOf")
           "https://github.com/example/q.git")
  (b3-put! (build-path repo ".gitignore") "compiled/\n")
  (b3-put! (build-path repo "source.rkt")
           "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
  (b3-put! (build-path repo "tests/probe.rkt")
           "#lang racket/base\n(require rackunit)\n(module+ test (check-true #t))\n")
  (b3-put! (build-path repo "tests/second.rkt")
           "#lang racket/base\n(require rackunit)\n(module+ test (check-true #t))\n")
  (b3-put! (build-path repo "tests/b3-owned-probe.rkt") probe-program)
  (make-directory* (build-path repo "scripts"))
  (copy-file metrics-script (build-path repo "scripts/metrics.rkt"))
  (copy-file metrics-helpers (build-path repo "scripts/metrics-helpers.rkt"))
  ;; Pin the NON-correction metric cells to the real committed tree using
  ;; the same pinned interpreter the owner uses. Only the two correction-owned
  ;; cells (test lines/assertions) are intentionally stale at baseline.
  (define rkt-paths
    (for/list ([p (in-directory repo)]
               #:when (and (regexp-match? #px"[.]rkt$" (path->string p))
                           (not (string-contains? (path->string p) "/.git/"))
                           (not (string-contains? (path->string p) "/compiled/"))))
      (cons (path->string (find-relative-path repo p)) (file->string p))))
  (define metrics (generated-metrics rkt-paths))
  (define (metric name)
    (cdr (assoc name metrics)))
  (b3-put! (build-path repo "README.md")
           (format (string-append "Prose stays byte-identical.\n## Test Suite\n"
                                  "| Source modules | ~a |\n| Test files | ~a |\n"
                                  "| Source lines | ~a |\n| Test lines | 0 |\n"
                                  "| Test assertions | 0 |\n")
                   (metric "Source modules")
                   (metric "Test files")
                   (metric "Source lines")))
  (b3-git! repo "add" ".")
  (b3-git! repo "commit" "-qm" "b3 focused baseline")
  (make-directory* (build-path root ".planning/waves"))
  (write-plan! root 0 "B3 metadata" "b3-metadata")
  (write-wave-doc! root
                   0
                   "b3-metadata"
                   '("q/source.rkt" "q/tests/probe.rkt" "q/tests/second.rkt")
                   b3-owned-verify-command)
  ;; Migration is the production seeding boundary: it creates the immutable
  ;; snapshot and binds the durable plan identity (SHA-256 manifest hash).
  (define campaign (migrate-campaign! root))
  (define plan (campaign-plan-id campaign))
  ;; The real publisher binds campaign/<plan8>/w0; use the durable plan id.
  (define branch (format "campaign/~a/w0" (substring plan 0 8)))
  (b3-git! repo "branch" "-m" branch)
  (b3-git! repo "push" "-q" "origin" (string-append "HEAD:refs/heads/" branch))
  (b3-git! repo "update-ref" (string-append "refs/remotes/origin/" branch) "HEAD")
  (set-campaign-record-execution-policy! campaign #f)
  (persist-campaign! root campaign)
  (define registry (make-verification-registry))
  (define before (committed-delivery-snapshot repo))
  (define-values (result capture)
    (parameterize ([current-owned-proof-root proofs]
                   [current-gsd-verification-registry registry]
                   [current-gsd-delivery-verify-timeout-sec 30]
                   [current-gsd-delivery-branch-context
                    (make-branch-delivery-context #:repo-root repo
                                                  #:branch branch
                                                  #:base-commit
                                                  (b3-git-output! repo "rev-parse" "HEAD")
                                                  #:worktree-path repo)])
      (call-with-owned-proof-capture root
                                     plan
                                     0
                                     before
                                     "b3-old-attempt"
                                     1
                                     (lambda () (b3-check! root))
                                     #:cwd repo)))
  (unless (and (delivery-verification-approved? result) capture)
    (error 'b3-metadata-fixture
           "focused owned Verify did not yield green proof: ~a"
           (delivery-verification-message result)))
  (define old-receipt
    (hash-set* before
               'attempt-id
               "b3-old-attempt"
               'attempt-fence
               1
               'verified-at
               (current-seconds)
               'evidence
               "actual focused owned Verify GREEN"
               'owned-proof
               (owned-proof-capture-reference capture)))
  (parameterize ([current-owned-proof-root proofs])
    (record-delivery-receipt! root plan 0 old-receipt))
  ;; A real successor attempt/fence under the same frozen snapshot.
  (define rec (load-campaign-record root plan))
  (set-campaign-fence-token! rec 2)
  (begin-attempt! rec 0 2)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'awaiting-delivery)
  (persist-campaign! root rec)
  (hash 'root
        root
        'repo
        repo
        'remote
        remote
        'store
        store
        'proofs
        proofs
        'plan
        plan
        'branch
        branch
        'registry
        registry
        'old-receipt
        old-receipt
        'green-result
        result
        'wave
        (car (campaign-record-waves (load-campaign-record root plan)))
        'cleanup
        (lambda ()
          (with-handlers ([exn:fail? void])
            (verification-close-registry! registry))
          (delete-directory/files root #:must-exist? #f)
          (delete-directory/files store #:must-exist? #f))))

(define (b3-metadata-cleanup! w)
  ((hash-ref w 'cleanup)))
