#lang racket/base
;; Offline audit validator. AUTHORITY must be supplied by the trusted caller / CI,
;; not selected from model output or a self-authored evidence sidecar.
(require json
         racket/file
         racket/list
         racket/path
         racket/string
         "../../extensions/gsd/review-proof.rkt"
         "../../extensions/gsd/evidence-probe.rkt"
         "../../extensions/gsd/evidence-repair-format.rkt"
         "../../extensions/gsd/review-audit-format.rkt")
(provide validate-owned-audit
         validate-owned-authority
         baseline-probe-red-facts)
;; Called only after engine validation; all facts come from addressed bytes.
(define (baseline-probe-red-facts bundle ref manifest)
  (define (load ref)
    (call-with-input-file (build-path bundle (string-append (hash-ref ref 'sha256) ".json")) read-json))
  (define spec (load (hash-ref manifest 'spec)))
  (define baseline (car (hash-ref manifest 'results)))
  (define result (load (hash-ref baseline 'structured)))
  (define target (hash-ref spec 'target))
  (define failed
    (for/first ([c (in-list (hash-ref result 'cases))]
                #:when (and (equal? (hash-ref c 'test-case-name) (hash-ref target 'test-case-name))
                            (equal? (hash-ref c 'suite-path) (hash-ref target 'suite-path)))) c))
  (define paths (hash-ref manifest 'execution-paths))
  (hasheq 'kind "baseline-probe" 'reference ref
          'command (string-join
                    (list (hash-ref paths 'racket) (hash-ref paths 'runner)
                          (path->string (build-path (hash-ref paths 'work-root) "baseline"
                                                    (car (hash-ref spec 'test-paths))))) " ")
          'adapter-argv (list (hash-ref paths 'racket) (hash-ref paths 'runner)
                              (path->string (build-path (hash-ref paths 'work-root) "baseline"
                                                        (car (hash-ref spec 'test-paths)))))
          'focused-selection (hash-ref spec 'test-paths)
          'failure-summary (canonical-evidence-json failed)
          'owner-job-id (hash-ref (hash-ref baseline 'job) 'id)
          'origin "posthoc-reproduction" 'relation "baseline-with-test-overlay-v1"))
(define (validate-owned-authority root authority)
  (unless (and (hash? authority) (equal? (hash-ref authority 'mode #f) owned-audit-mode))
    (error 'owned-audit "unknown-proof-authority"))
  (define relative (hash-ref authority 'bundle #f))
  (define ref (hash-ref authority 'reference #f))
  (unless (and (valid-owned-proof-reference? ref)
               (equal? relative
                       (string-append "docs/reports/gsd-wave-validation/owned-proofs/"
                                      (hash-ref ref 'sha256))))
    (error 'owned-audit "unsafe-owned-proof-bundle"))
  (define bundle-root (build-path root relative))
  (define manifest
    (load-owned-review-proof bundle-root
                             ref
                             (hash-ref authority 'expected)
                             #:command (hash-ref authority 'command)
                             #:normalized-command (hash-ref authority 'normalized-command)))
  (define receipt (hash-ref authority 'receipt #f))
  (unless (and (proof-reference-matches-receipt? ref receipt)
               (equal? ref (hash-ref receipt 'owned-proof #f)))
    (error 'owned-audit "receipt-owned-proof-mismatch"))
  (define gates (owned-green-gates manifest))
  ;; A linked RED is reloaded from the SAME portable bundle and revalidated
  ;; against the actual retained raw bytes; the authority's declared red
  ;; facts must then match what the bundle proves, byte for byte.
  (define red (hash-ref authority 'red #f))
  (unless (or (and (hash? red) (equal? (hash-ref red 'relation #f) "baseline-with-test-overlay-v1"))
              (and (equal? (hash-ref authority 'evidence-revision 0) 0)
                   (not (hash-ref authority 'evidence-registration-sha256 #f))))
    (error 'owned-audit "registration-without-baseline-probe"))
  (define red-manifest
    (and
     (hash? red)
     (if (equal? (hash-ref red 'relation #f) "baseline-with-test-overlay-v1")
         (let* ([context (hash-ref authority 'evidence-context)]
                [registration (hash-ref authority 'evidence-registration)]
                [probe (hash-ref registration 'probe)]
                [probe-root (build-path bundle-root "probes" (hash-ref probe 'sha256))]
                [loaded (validate-exported-baseline-probe! root probe-root probe context)]
                [spec (call-with-input-file
                       (build-path probe-root (string-append (hash-ref (hash-ref loaded 'spec) 'sha256) ".json"))
                       read-json)])
           (unless (and (equal? (hash-ref registration 'schema #f) "evidence-registration/v1")
                        (equal? (hash-ref registration 'sha256 #f) (canonical-evidence-digest registration))
                        (equal? (hash-ref authority 'evidence-registration-sha256 #f) (hash-ref registration 'sha256))
                        (equal? context (hash-ref registration 'context))
                        (equal? (hash-ref context 'receipt-sha256) (canonical-evidence-digest receipt))
                        (equal? (hash-ref context 'green-proof-sha256) (hash-ref ref 'sha256))
                        (equal? (hash-ref registration 'retained-green) (hash-ref ref 'sha256))
                        (equal? (hash-ref context 'snapshot-sha256) (hash-ref (hash-ref ref 'binding) 'snapshot-sha256))
                        (equal? (hash-ref context 'receipt-generation) (hash-ref authority 'generation))
                        (equal? (hash-ref registration 'operation-id) (hash-ref loaded 'operation-id))
                        (equal? (hash-ref loaded 'spec-sha256 (lambda () (hash-ref (hash-ref loaded 'spec) 'sha256)))
                                (canonical-evidence-digest spec))
                        (member (hash-ref spec 'focused-normalized-command)
                                (map string-trim (string-split (hash-ref authority 'normalized-command) "&&")))
                        (equal? (hash-ref registration 'previous-revision) 0)
                        (equal? (hash-ref registration 'prior-registration-sha256) 'null)
                        (equal? (hash-ref authority 'evidence-revision) (hash-ref registration 'revision))
                        (equal? (hash-ref registration 'revision) 1)
                        (for/and ([key '(plan-id wave head tree branch attempt-id attempt-fence)])
                          (equal? (hash-ref context key) (hash-ref (hash-ref authority 'expected) key)))
                        (equal? red (baseline-probe-red-facts probe-root probe loaded)))
             (error 'owned-audit "baseline-probe-authority-mismatch"))
           loaded)
     (let* ([red-ref (hash-ref red 'reference #f)]
            [loaded (begin
                      (unless (valid-owned-red-reference? red-ref)
                        (error 'owned-audit "invalid-owned-red-reference"))
                      (load-owned-red-proof bundle-root
                                            red-ref
                                            (hash-ref red-ref 'binding)
                                            #:command (hash-ref red 'command #f)
                                            #:focused-normalized
                                            (hash-ref red 'focused-normalized-command #f)))]
            [origin (hash-ref loaded 'red-origin)]
            [expected-relation (if (equal? origin "preimplementation")
                                   "preimplementation-ancestor"
                                   "reproduction-ancestor")])
       (unless (red-links-green-proof? loaded manifest)
         (error 'owned-audit "red-proof-not-linked-to-green-execution"))
       (unless (equal? (hash-ref red 'relation #f) expected-relation)
         (error 'owned-audit "dishonest-red-origin-relation"))
       (unless (and (equal? (hash-ref red 'red-head #f) (hash-ref (hash-ref loaded 'binding) 'head))
                    (equal? (hash-ref red 'red-tree #f) (hash-ref (hash-ref loaded 'binding) 'tree))
                    (equal? (hash-ref red 'failure-summary #f)
                            (owned-red-failure-summary bundle-root loaded))
                    (equal? (hash-ref red 'command #f) (hash-ref loaded 'command)))
         (error 'owned-audit "authority-red-facts-do-not-match-retained-red"))
       loaded))))
  (hasheq 'green-gates gates 'red-manifest (or red-manifest #f) 'red-facts (and (hash? red) red)))
(define (validate-owned-audit root evidence review validation authority)
  (with-handlers ([exn:fail? (lambda (e) (list (exn-message e)))])
    (define audited (validate-owned-authority root authority))
    (define gates (hash-ref audited 'green-gates))
    (define red-manifest (hash-ref audited 'red-manifest))
    (define red-facts (hash-ref audited 'red-facts))
    (define binding (hash-ref authority 'audit-binding #f))
    (unless (hash? binding)
      (error 'owned-audit "missing-audit-binding"))
    (define expected (hash-ref authority 'expected))
    (unless (and (equal? (hash-ref binding 'evidence-revision 0)
                         (hash-ref authority 'evidence-revision 0))
                 (equal? (hash-ref binding 'evidence-registration-sha256 #f)
                         (hash-ref authority 'evidence-registration-sha256 #f)))
      (error 'owned-audit "stale-evidence-registration-binding"))
    (unless (and (equal? (hash-ref binding 'generation #f) (hash-ref authority 'generation #f))
                 (equal? (hash-ref binding 'proof-sha256 #f)
                         (hash-ref (hash-ref authority 'reference) 'sha256))
                 (for/and ([key '(plan-id wave head tree branch attempt-id attempt-fence)])
                   (equal? (hash-ref binding key #f) (hash-ref expected key #f))))
      (error 'owned-audit "stale-audit-binding"))
    (for ([datum (in-list (list evidence review validation))])
      (unless (and (hash? datum)
                   (equal? (hash-ref datum 'review-mode #f) owned-audit-mode)
                   (equal? (hash-ref datum 'audit-binding #f) binding)
                   (equal? (hash-ref datum 'owned-proof #f) (hash-ref authority 'reference)))
        (error 'owned-audit "missing-or-mismatched-audit-provenance"))
      (when red-manifest
        (unless (equal? (hash-ref datum 'owned-red #f) (hash-ref red-facts 'reference))
          (error 'owned-audit "missing-or-mismatched-owned-red-provenance")))
      (unless (or red-manifest (not (hash-has-key? datum 'owned-red)))
        (error 'owned-audit "unlinked-red-cannot-be-claimed")))
    ;; A postmerge binding trio (draft marker `merge-sha`) publishes on the
    ;; BINDING branch while the audited execution identity stays the wave
    ;; branch: check `wave-branch` there instead of the publication branch.
    ;; The FF law (implementation-sha == execution head) is unchanged.
    (define evidence-branch
      (if (hash-has-key? evidence 'merge-sha)
          (hash-ref evidence 'wave-branch #f)
          (hash-ref evidence 'branch #f)))
    (unless (and (equal? (hash-ref evidence 'implementation-sha #f) (hash-ref expected 'head))
                 (equal? (hash-ref evidence 'content-digest #f) (hash-ref binding 'content-digest #f))
                 (equal? (hash-ref evidence 'plan-id #f) (hash-ref expected 'plan-id))
                 (equal? (hash-ref evidence 'wave #f) (format "W~a" (hash-ref expected 'wave)))
                 (equal? evidence-branch (hash-ref expected 'branch))
                 (valid-audit-ack? (hash-ref review 'audit-ack #f) binding))
      (error 'owned-audit "invalid-audit-ack-or-source-binding"))
    ;; The peer must meaningfully acknowledge the linked behavioral RED —
    ;; exact retained red identity plus its actual raw failure output — and
    ;; may never acknowledge a red that is not linked.
    (define ack (hash-ref review 'audit-ack))
    (cond
      [red-manifest
       (unless (valid-audit-red-ack? (hash-ref ack 'red-ack #f)
                                     (hash-ref (hash-ref red-facts 'reference) 'sha256))
         (error 'owned-audit "missing-or-stale-owned-red-acknowledgement"))]
      [(hash-has-key? ack 'red-ack) (error 'owned-audit "red-acknowledged-without-linked-red-proof")])
    (for ([(key fact) (in-hash gates)])
      (unless (equal? fact (hash-ref validation key #f))
        (error 'owned-audit "model-authored-or-mismatched-owned-gate-facts")))
    (cond
      [red-manifest
       (unless (equal? (hash-ref validation 'red-first #f)
                       (hasheq 'command
                               (hash-ref red-facts 'command)
                               'failure
                               (hash-ref red-facts 'failure-summary)
                               'result
                               "passed"
                               'verification-origin
                               "audited-owned"
                               'owner-job-id
                               (if (equal? (hash-ref red-facts 'relation) "baseline-with-test-overlay-v1")
                                   (hash-ref (hash-ref (car (hash-ref red-manifest 'results)) 'job) 'id)
                                   (hash-ref (hash-ref red-manifest 'job) 'id))))
         (error 'owned-audit "model-authored-or-mismatched-owned-red-facts"))]
      [else
       ;; Green capture has no meaningful-RED authority: no terminal
       ;; diagnostic, compile error, fixture prose or independent approval
       ;; can be converted to RED. This typed gap stops the audit.
       (error 'owned-audit "missing-meaningful-red-proof: no authentic behavioral RED is linked")])
    ;; Strict approval never manufactures the separate operator merge
    ;; authorization (F11) or a canary: those stay separate gates.
    (for ([datum (in-list (list evidence review validation))])
      (when (hash-has-key? datum 'merge-authorization)
        (error 'owned-audit "audit-approval-cannot-manufacture-merge-authorization")))
    '()))
(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (unless (= 2 (length args))
    (error 'owned-audit "expected <authority.json> <bundle-root>"))
  (write-json (validate-owned-authority (second args) (call-with-input-file (first args) read-json)))
  (newline))
