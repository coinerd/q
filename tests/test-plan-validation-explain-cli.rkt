#lang racket

;; @covers scripts/validate-plan.rkt
;; @covers extensions/gsd/planning-artifact-validation.rkt
;; @speed fast  ;; @suite extensions
;; @boundary integration

;; tests/test-plan-validation-explain-cli.rkt — W1 (GSD-SIMPLIFICATION-PLAN):
;; the standalone plan validator's READ-ONLY --explain campaign inspection.
;;
;; The CLI delegates to the same shared kernel /go uses
;; (validate-plan-artifacts in extensions/gsd/planning-artifact-validation.rkt).
;; --explain must name, without changing anything:
;;   - the plan path and declared wave paths,
;;   - the stored plan-id and the manifest hash recomputed from that record,
;;   - the frozen bound plan-body hash vs the live one,
;;   - per-wave status projections (index / doc / durable),
;;   - snapshot drift classes.
;; Exit codes are unchanged: 0 ready, 1 rejected, 2 usage.

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/string
         racket/system
         (only-in "../extensions/gsd/campaign-state.rkt"
                  campaign-plan-id
                  campaign-record-manifest
                  campaign-record-waves
                  campaign-manifest-hash
                  campaign-manifest-schema-version
                  campaign-manifest-title
                  campaign-manifest-dependencies
                  campaign-manifest-waves
                  campaign-manifest-constraints-hash
                  campaign-wave-descriptor-index
                  campaign-wave-descriptor-title
                  campaign-wave-descriptor-doc-path
                  campaign-wave-descriptor-content-hash
                  campaign-wave-index
                  campaign-wave-status
                  make-campaign-manifest
                  migrate-campaign!
                  set-campaign-record-plan-snapshot-path!
                  set-campaign-record-plan-snapshot-digest!)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign!))

(define repo-root
  (simplify-path
   (build-path (simplify-path (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
               'up
               'up)))

(define cli (build-path repo-root "scripts" "validate-plan.rkt"))
(define racket-bin (find-executable-path "racket"))

(define (path->command-part a)
  (if (path? a)
      (path->string a)
      a))

(define (run-cli dir . args)
  (define out (open-output-string))
  (define argv (map path->command-part (append (list racket-bin cli) args)))
  (define code
    (parameterize ([current-directory dir]
                   [current-output-port out]
                   [current-error-port out])
      (system/exit-code (string-join argv " "))))
  (values code (get-output-string out)))

(define (make-fixture)
  (define dir (make-temporary-file "explain-cli-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Fixture\n\n## Waves\n\n" out)
                           (display "- [Inbox] W0: Fixture wave → waves/W0-fixture-wave.md\n" out))
                         #:exists 'truncate)
  (call-with-output-file (build-path dir ".planning" "waves" "W0-fixture-wave.md")
                         (lambda (out)
                           (display (string-append "# Wave 0\nStatus: Inbox\n\n"
                                                   "## Files\n\n- File: existing-file.rkt\n\n"
                                                   "## Verify\n\nraco test tests/x.rkt\n\n"
                                                   "## Done\n\nDone means closed.\n")
                                    out))
                         #:exists 'truncate)
  (display-to-file "" (build-path dir "existing-file.rkt") #:exists 'replace)
  (simplify-path dir))

(define (drop-fixture dir)
  (delete-directory/files dir #:must-exist? #f))

;; --- no campaign: the explanation invents no identity ------------------

(test-case "explain: no durable campaign means no invented identity"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (define-values (code out) (run-cli dir "--explain"))
                  (check-equal? code 0 out)
                  (check-true (string-contains? out "== plan inspection (read-only) =="))
                  (check-true (string-contains? out "declared wave paths (1)"))
                  (check-true (string-contains? out ".planning/waves/W0-fixture-wave.md"))
                  (check-true (string-contains? out "status projections (1 waves)"))
                  (check-true (string-contains? out "index=Inbox"))
                  (check-true (string-contains? out "doc=Inbox"))
                  (check-true (string-contains? out "durable=(no campaign)"))
                  (check-true (string-contains? out "campaign: (none)"))
                  (check-true (string-contains? out "identity matches: n/a (no durable record)"))
                  ;; read-only: nothing was created
                  (check-false (directory-exists? (build-path dir ".planning" "campaigns"))))
                (lambda () (drop-fixture dir))))

;; --- with a durable campaign: identity namespaces shown side by side ---

(test-case "explain: stored plan-id, recomputed hash, bound/live body hashes, projections"
  (define dir (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (define rec (migrate-campaign! dir))
                  (persist-campaign! dir rec)
                  (define plan-id (campaign-plan-id rec))
                  (define plan-before (file->string (build-path dir ".planning" "PLAN.md")))
                  (define-values (code out) (run-cli dir "--explain"))
                  (check-equal? code 0 out)
                  (check-true (string-contains? out (format "campaign: ~a" plan-id)))
                  (check-true (string-contains? out (format "stored plan-id: ~a" plan-id)))
                  (check-true (string-contains? out "recomputed manifest hash:"))
                  (check-true (string-contains? out "identity matches: yes"))
                  (check-true (string-contains? out "bound plan-body hash:"))
                  (check-true (string-contains? out "live plan-body hash:"))
                  (check-true (string-contains? out "bound body matches live: yes"))
                  (check-true (string-contains? out "snapshot PLAN.md sha256:"))
                  ;; Declared (live) and bound wave paths are both named.
                  (check-true (string-contains? out "bound wave paths (1)"))
                  ;; Durable status projection reaches the report.
                  (check-true (or (string-contains? out "durable=pending")
                                  (string-contains? out "durable=inbox")
                                  (string-contains? out "durable=interrupted")))
                  ;; READ-ONLY: the authored plan is byte-identical afterwards.
                  (check-equal? (file->string (build-path dir ".planning" "PLAN.md")) plan-before))
                (lambda () (drop-fixture dir))))

;; --- --campaign feeds the same structured drift findings ----------------

(test-case "explain --campaign: drift class is reported for a mutated authored body"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     ;; Mutate the AUTHORED plan body after the snapshot froze it, without
     ;; changing the index structure (structural errors would take the
     ;; kernel's earlier refusal branch).
     (display-to-file (string-append "# Plan: Fixture\n\n## Waves\n\n"
                                     "- [Inbox] W0: Fixture wave → waves/W0-fixture-wave.md\n\n"
                                     "## Notes\n\nAmended after the campaign froze.\n")
                      (build-path dir ".planning" "PLAN.md")
                      #:exists 'replace)
     (define-values (code out) (run-cli dir "--explain" "--campaign" plan-id))
     ;; Snapshot drift is surfaced by class: an amended PLAN.md body is the
     ;; frozen-contract-stale class (named, with a recovery action).
     (check-equal? code 1 out)
     (check-true (string-contains? out "FAILED"))
     (check-true (string-contains? out "bound body matches live: NO (drift)"))
     (check-true (string-contains? out "snapshot drift (1)"))
     (check-true (string-contains? out "frozen-contract-stale"))
     (check-true (string-contains? out "(snapshot-drift-frozen-contract-stale)"))
     (check-true (string-contains? out "-> next:")))
   (lambda () (drop-fixture dir))))

;; --- absent and corrupt identities stay named and advisory ---------------

(test-case "explain: invalid and absent campaigns never become internal contract failures"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define-values (invalid-code invalid-out) (run-cli dir "--explain" "--campaign" "not-an-id"))
     (check-equal? invalid-code 1 invalid-out)
     (check-true (string-contains? invalid-out "campaign-unknown"))
     (check-false (string-contains? invalid-out "contract violation"))
     (define missing-id (make-string 64 (integer->char 98)))
     (define-values (missing-code missing-out) (run-cli dir "--explain" "--campaign" missing-id))
     (check-equal? missing-code 0 missing-out)
     (check-true (string-contains? missing-out "snapshot-absent"))
     (check-true (string-contains? missing-out "identity matches: n/a (no durable record)"))
     (check-true (string-contains? missing-out "bound body matches live: n/a (no durable record)"))
     (check-false (string-contains? missing-out "FAILED")))
   (lambda () (drop-fixture dir))))

(test-case "explain: implicit newest selection names a corrupt candidate"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     (display-to-file "not-a-campaign-record"
                      (build-path dir ".planning" "campaigns" (string-append plan-id ".rktd"))
                      #:exists 'replace)
     (define-values (code out) (run-cli dir "--explain"))
     (check-equal? code 1 out)
     (check-true (string-contains? out "campaign: (unreadable)"))
     (check-true (string-contains? out "campaign source: unreadable"))
     (check-true (string-contains? out "campaign-candidate-unreadable"))
     (check-true (string-contains? out "not inspected: campaign record unreadable"))
     (check-false (string-contains? out "campaign: (none)")))
   (lambda () (drop-fixture dir))))

(test-case "explain: corrupt durable record and snapshot are named, not rendered as absent"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     (define record-path (build-path dir ".planning" "campaigns" (string-append plan-id ".rktd")))
     (display-to-file "not-a-campaign-record" record-path #:exists 'replace)
     (define-values (record-code record-out) (run-cli dir "--explain" "--campaign" plan-id))
     (check-equal? record-code 1 record-out)
     (check-true (string-contains? record-out "campaign-unreadable"))
     (check-true (string-contains? record-out "not inspected: campaign record unreadable"))
     (check-false (string-contains? record-out "campaign: (none)"))
     ;; Restore a readable record and UNBIND its snapshot. A bound record would
     ;; fail closed during record verification (campaign-unreadable), never
     ;; reaching the independent snapshot renderer under test.
     (set-campaign-record-plan-snapshot-path! rec #f)
     (set-campaign-record-plan-snapshot-digest! rec #f)
     (persist-campaign! dir rec)
     (define snapshot-path
       (build-path dir ".planning" "campaigns" plan-id "plan-snapshot" "snapshot-manifest.rktd"))
     (display-to-file "not-a-snapshot" snapshot-path #:exists 'replace)
     (define stale (build-path dir ".planning" "campaigns" plan-id "plan-snapshot.tmp-stale"))
     (make-directory* stale)
     (define-values (snapshot-code snapshot-out) (run-cli dir "--explain" "--campaign" plan-id))
     (check-equal? snapshot-code 1 snapshot-out)
     (check-true (string-contains? snapshot-out "snapshot-unreadable"))
     (check-true (string-contains? snapshot-out "snapshot manifest digest: (unreadable)"))
     (check-true (string-contains? snapshot-out "FAILED"))
     (check-true (directory-exists? stale)
                 "read-only record loading must not sweep staging evidence"))
   (lambda () (drop-fixture dir))))

(test-case "explain: legacy unbound body hash is unknown, not drift"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     ;; Unbind the snapshot so the legacy record identity is verified on its
     ;; own; the legacy plan-id below has no matching snapshot directory.
     (set-campaign-record-plan-snapshot-path! rec #f)
     (set-campaign-record-plan-snapshot-digest! rec #f)
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     (define record-path (build-path dir ".planning" "campaigns" (string-append plan-id ".rktd")))
     (define m (campaign-record-manifest rec))
     ;; A valid legacy manifest carries NO plan-body hash; identity must be
     ;; recomputed from that manifest rather than reusing the bound plan-id.
     (define legacy-manifest
       (make-campaign-manifest (campaign-manifest-schema-version m)
                               (campaign-manifest-title m)
                               (campaign-manifest-dependencies m)
                               (campaign-manifest-waves m)
                               (campaign-manifest-constraints-hash m)))
     (define legacy-plan-id (campaign-manifest-hash legacy-manifest))
     (define legacy-manifest-datum
       (list 'manifest
             (campaign-manifest-schema-version legacy-manifest)
             (campaign-manifest-title legacy-manifest)
             (campaign-manifest-dependencies legacy-manifest)
             (for/list ([w (in-list (campaign-manifest-waves legacy-manifest))])
               (list (campaign-wave-descriptor-index w)
                     (campaign-wave-descriptor-title w)
                     (campaign-wave-descriptor-doc-path w)
                     (campaign-wave-descriptor-content-hash w)))
             (campaign-manifest-constraints-hash legacy-manifest)))
     (define datum (file->value record-path))
     (define legacy-datum
       (list* (car datum) legacy-plan-id (cons legacy-manifest-datum (cdddr datum))))
     (define legacy-record-path
       (build-path dir ".planning" "campaigns" (string-append legacy-plan-id ".rktd")))
     (call-with-output-file legacy-record-path
       (lambda (out) (write legacy-datum out))
       #:exists 'replace)
     (delete-file record-path)
     (define-values (code out) (run-cli dir "--explain" "--campaign" legacy-plan-id))
     (check-equal? code 0 out)
     (check-true (string-contains? out "bound plan-body hash: (none)"))
     (check-true (string-contains? out "bound body matches live: n/a (no bound plan-body hash)"))
     (check-false (string-contains? out "bound body matches live: NO (drift)")))
   (lambda () (drop-fixture dir))))

(test-case "explain: absent snapshot inspection preserves interrupted staging"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     (set-campaign-record-plan-snapshot-path! rec #f)
     (set-campaign-record-plan-snapshot-digest! rec #f)
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     (define stale (build-path dir ".planning" "campaigns" plan-id "plan-snapshot.tmp-stale"))
     (define snapshot-dir (build-path dir ".planning" "campaigns" plan-id "plan-snapshot"))
     (when (directory-exists? snapshot-dir)
       (delete-directory/files snapshot-dir))
     (make-directory* stale)
     (check-false
      (file-exists?
       (build-path dir ".planning" "campaigns" plan-id "plan-snapshot" "snapshot-manifest.rktd")))
     (define-values (code out) (run-cli dir "--explain" "--campaign" plan-id))
     (check-equal? code 0 out)
     (check-true (string-contains? out "snapshot-absent"))
     (check-true (directory-exists? stale)
                 "read-only record loading must not sweep absent-manifest staging evidence"))
   (lambda () (drop-fixture dir))))

;; --- exit codes and ordinary validation are unchanged ------------------

(test-case "cli: ordinary validation exit codes are unchanged"
  (define good (make-fixture))
  (define bad (make-fixture))
  (dynamic-wind void
                (lambda ()
                  (define-values (good-code good-out) (run-cli good))
                  (check-equal? good-code 0 good-out)
                  (check-true (string-contains? good-out "OK: plan is /go-ready"))
                  ;; bad: delete the declared wave document
                  (delete-file (build-path bad ".planning" "waves" "W0-fixture-wave.md"))
                  (define-values (bad-code bad-out) (run-cli bad))
                  (check-equal? bad-code 1 bad-out)
                  (check-true (string-contains? bad-out "FAILED"))
                  (check-true (string-contains? bad-out "(wave-doc-missing)"))
                  (check-true (string-contains? bad-out "-> next:"))
                  (define-values (usage-code _usage-out) (run-cli good "--bogus"))
                  (check-equal? usage-code 2))
                (lambda ()
                  (drop-fixture good)
                  (drop-fixture bad))))

(displayln "PASS test-plan-validation-explain-cli (W1: read-only plan/campaign inspection)")
