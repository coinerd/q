#lang racket/base
(require json
         racket/list
         racket/path
         racket/string
         racket/system
         (only-in "delivery-journal.rkt"
                  load-delivery-journal
                  load-supplemental-promotion-intent)
         "review-proof.rkt"
         "evidence-repair-format.rkt"
         "evidence-repair-ledger.rkt"
         "evidence-probe.rkt"
         "evidence-discovery.rkt")
(provide current-evidence-context
         evidence-readiness)
(define (git repo . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") "-C" repo args)))
  (unless (zero? code)
    (error 'evidence-readiness "git authentication failed: ~a" (get-output-string err)))
  (string-trim (get-output-string out)))
(define (authenticated base plan wave)
  (define journal (load-delivery-journal base plan wave))
  (define receipt (and journal (hash-ref journal 'receipt #f)))
  (unless (and (hash? receipt) (hash-ref receipt 'owned-proof #f))
    (error 'evidence-readiness "missing-owned-proof"))
  (validate-receipt-owned-proof! receipt base plan wave)
  (define ref (hash-ref receipt 'owned-proof))
  (define binding (hash-ref ref 'binding))
  (define frozen (frozen-verify-binding base plan wave))
  (define root (build-path (current-owned-proof-root) plan))
  (define manifest (call-with-input-file (build-path root (hash-ref ref 'path)) read-json))
  ;; The receipt's logical repository is digest-bound, not an execution port.
  (define repo
    (path->string (simplify-path (path->complete-path (hash-ref manifest 'execution-cwd)))))
  (unless (and (equal? (git repo "rev-parse" "HEAD") (hash-ref receipt 'head))
               (equal? (git repo "rev-parse" "HEAD^{tree}") (hash-ref receipt 'tree))
               (equal? (git repo "symbolic-ref" "--short" "HEAD") (hash-ref receipt 'branch))
               (string=? (git repo "status" "--porcelain" "--untracked-files=all") ""))
    (error 'evidence-readiness "stale-or-dirty-receipt"))
  (define context
    (for/fold ([c (hasheq 'plan-id
                          plan
                          'wave
                          wave
                          'receipt-generation
                          (length (hash-ref journal 'receipt-history '()))
                          'receipt-sha256
                          (canonical-evidence-digest receipt)
                          'evidence-revision
                          0
                          'snapshot-sha256
                          (hash-ref frozen 'snapshot-sha256)
                          'green-proof-sha256
                          (hash-ref ref 'sha256))])
              ([key '(repo branch origin head tree attempt-id attempt-fence)])
      (hash-set c
                key
                (if (eq? key 'repo)
                    repo
                    (hash-ref (if (memq key '(attempt-id attempt-fence)) binding receipt) key)))))
  (unless (valid-evidence-context? context)
    (error 'evidence-readiness "invalid-current-context"))
  (values context root manifest))
(define (current-evidence-context base plan wave)
  (define-values (context root manifest) (authenticated base plan wave))
  context)
(define (evidence-readiness base plan wave)
  (with-handlers ([exn:fail? (lambda (e)
                               (hasheq 'status
                                       "blocked"
                                       'code
                                       "invalid-current-evidence"
                                       'message
                                       (exn-message e)
                                       'context
                                       'null
                                       'spec
                                       'null))])
    (define-values (context root manifest) (authenticated base plan wave))
    ;; Receipt generations have SEPARATE bounded evidence budgets: an
    ;; authorized supersession (rejection retry or the permanent one-per-wave
    ;; author allowance) consumed the prior generation's slot; its durable
    ;; operation and registration remain auditable at their own ledger path.
    ;; Only the authenticated journal's exact generation opens the current
    ;; slot — a same-generation foreign context is still stale, and an older
    ;; non-terminal operation never rotates.
    (define generation (hash-ref context 'receipt-generation))
    (for ([g (in-range 0 generation)])
      (define prior (load-evidence-repair base plan wave g))
      (when (and (hash? (hash-ref prior 'operation #f))
                 (not (equal? (hash-ref (hash-ref prior 'operation) 'state #f) "registered")))
        (error 'evidence-readiness
               "prior evidence operation is not terminal; refusing rotation")))
    (define ledger (load-evidence-repair base plan wave generation))
    (define registrations (hash-ref ledger 'registrations))
    (define registration (and (pair? registrations) (last registrations)))
    (when registration
      (unless (and (equal? context (hash-ref registration 'context))
                   (equal? (hash-ref registration 'retained-green)
                           (hash-ref context 'green-proof-sha256)))
        (error 'evidence-readiness "stale-registration-context"))
      (validate-baseline-probe! (hash-ref context 'repo) (hash-ref registration 'probe) context)
      ;; BUG-0088 (e): a PROMOTED registration is ready only when the
      ;; journal's durable promotion intent attests its provenance against
      ;; the CURRENT certified context/generation/receipt. A registration
      ;; whose P provenance cannot be re-anchored now never reads ready.
      (define promoted
        (and (hash? (hash-ref ledger 'operation #f))
             (hash-ref (hash-ref ledger 'operation) 'promoted #f)))
      (when promoted
        (define intent (load-supplemental-promotion-intent base plan wave))
        (unless (and (hash? intent)
                     (hash? (hash-ref intent 'promotion #f))
                     (equal? (hash-ref promoted 'promotion #f)
                             (hash-ref (hash-ref intent 'promotion) 'sha256 #f))
                     (equal? (hash-ref promoted 'generation #f)
                             (hash-ref intent 'generation #f))
                     (equal? (hash-ref intent 'generation #f)
                             (hash-ref context 'receipt-generation))
                     (equal? (hash-ref intent 'receipt-sha256 #f)
                             (hash-ref context 'receipt-sha256))
                     (equal? (hash-ref intent 'context-digest #f)
                             (evidence-context-digest context))
                     (equal? (hash-ref intent 'operation-id #f)
                             (hash-ref (hash-ref ledger 'operation) 'id #f)))
          (error 'evidence-readiness "unattested-promoted-registration"))))
    (define linked
      (for/first ([pair (in-list (find-owned-red-proofs root manifest))]
                  #:when (string=? (git (hash-ref context 'repo)
                                        "merge-base"
                                        (hash-ref (hash-ref (second pair) 'binding) 'head)
                                        (hash-ref context 'head))
                                   (hash-ref (hash-ref (second pair) 'binding) 'head)))
        pair))
    (cond
      [registration
       (hasheq 'status
               "ready"
               'code
               "registered-probe-ready"
               'context
               context
               'spec
               'null
               'registration
               registration
               'evidence-revision
               (hash-ref registration 'revision))]
      [else
       (if linked
           (hasheq 'status
                   "ready"
                   'code
                   "legacy-red-ready"
                   'context
                   context
                   'spec
                   'null
                   'red-reference
                   (first linked))
           (let ([spec (discover-evidence-probe-spec (hash-ref context 'repo)
                                                     context
                                                     (hash-ref (frozen-verify-binding base plan wave)
                                                               'normalized-command))])
             (hasheq
              'status
              "repair-required"
              'code
              (if spec "missing-meaningful-red-proof" "unsupported-harness")
              'context
              context
              'spec
              (or spec 'null)
              'message
              (if spec
                  "Authenticated regression probe ready for bounded execution"
                  "No compatible frozen regression suite: author repair requires a separate source/test attempt and full Verify."))))])))
