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
;;   - snapshot drift classes,
;;   - the authoritative record/snapshot, attempts/fence, lease facts, the
;;     legal operator actions, and unknown user backup artifacts (read-only),
;;   - the exact `## Last Failure` exclusion span normalization uses.
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
                  begin-attempt!
                  set-campaign-fence-token!
                  make-campaign-manifest
                  migrate-campaign!
                  set-campaign-record-plan-snapshot-path!
                  set-campaign-record-plan-snapshot-digest!)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign!)
         (only-in "../extensions/gsd/planning-artifact-validation.rkt"
                  plan-artifact-explanation
                  format-plan-artifact-explanation)
         (only-in "../extensions/gsd/planning-paths.rkt" campaign-lock-file)
         (only-in "../extensions/gsd/campaign-lease.rkt" lease-path)
         (only-in "../extensions/gsd/plan-snapshot.rkt"
                  normalize-wave-doc-content
                  plan-snapshot-staging-marker-filename))

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

;; --- W4 doctor: authoritative record, attempt/fence, lease, legal action ---

(test-case "W4 doctor: names record/snapshot/attempts/lease and the legal action, read-only"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     (begin-attempt! rec 0 11)
     (set-campaign-fence-token! rec 11)
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     (define planning (build-path dir ".planning"))
     (define record-path (build-path planning "campaigns" (string-append plan-id ".rktd")))
     ;; A leftover lock FILE with a recorded holder. The OS lock is NOT held,
     ;; so this is diagnostic only: the doctor must say so and offer no release.
     (define lock-path (build-path planning "campaigns" (string-append plan-id ".lock")))
     (call-with-output-file lock-path
       (lambda (out) (write (hasheq 'owner "session-A" 'pid 4242 'acquired 1700000000) out))
       #:exists 'truncate)
     (define record-before (file->bytes record-path))
     (define lock-before (file->bytes lock-path))
     ;; In-process inspection: same function the CLI prints from, without
     ;; paying a subprocess spawn per assertion.
     (define explanation (plan-artifact-explanation dir #:campaign-id plan-id))
     (define out (format-plan-artifact-explanation explanation))
     (check-equal? (hash-ref explanation 'errors) '() out)
     (check-true (string-contains? out "what is authoritative:"))
     (check-true (string-contains? out (format "durable record: ~a" (path->string record-path))))
     (check-true (string-contains? out "bound snapshot:"))
     (check-true (string-contains? out "authority used by /go: durable-campaign-record"))
     (check-true (string-contains? out "campaign fence token: 11"))
     (check-true (string-contains? out "attempt-count=1"))
     (check-true (string-contains? out "current-attempt=attempt-1"))
     (check-true (string-contains? out "fence=11"))
     (check-true (string-contains? out "lease lock file:"))
     (check-true (string-contains? out "lease file exists: yes"))
     (check-true (string-contains? out "session-A (pid 4242"))
     (check-true (string-contains? out "lease assessment: undetermined-read-only"))
     (check-true (string-contains? out "no force-release command exists"))
     (check-true (string-contains? out "legal operator action(s)"))
     (check-true (string-contains? out "never delete campaign state"))
     ;; READ-ONLY: record and lock bytes are untouched; nothing swept.
     (check-equal? (file->bytes record-path) record-before)
     (check-equal? (file->bytes lock-path) lock-before))
   (lambda () (drop-fixture dir))))

(test-case "W4 doctor: drift names an overridable class and a legal correction"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     (display-to-file (string-append "# Plan: Fixture\n\n## Waves\n\n"
                                     "- [Inbox] W0: Fixture wave → waves/W0-fixture-wave.md\n\n"
                                     "## Notes\n\nAmended after the freeze.\n")
                      (build-path dir ".planning" "PLAN.md")
                      #:exists 'replace)
     (define explanation (plan-artifact-explanation dir #:campaign-id plan-id))
     (define out (format-plan-artifact-explanation explanation))
     (check-true (string-contains? out "resolve frozen-contract-stale for .planning/PLAN.md"))
     (check-true (string-contains? out "re-freeze under a new campaign identity"))
     ;; The doctor offers no release/reset action of any kind.
     (check-false (string-contains? out "release the lease"))
     (check-false (string-contains? out "delete the campaign")))
   (lambda () (drop-fixture dir))))

;; --- W4 doctor: one shared lease path -------------------------------

(test-case "W4 doctor: the lease lock path is ONE shared helper"
  (define base (make-temporary-file "lease-path-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define plan-id (make-string 64 #\c))
     (check-equal? (lease-path base plan-id)
                   (campaign-lock-file base plan-id)
                   "the doctor and the lease can never name two lock files"))
   (lambda () (drop-fixture base))))

;; --- W4 doctor: unknown user artifacts are reported, never removed ---

(test-case "W4 doctor: unknown .bak/temp/backup artifacts are reported and preserved"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define rec (migrate-campaign! dir))
     (persist-campaign! dir rec)
     (define plan-id (campaign-plan-id rec))
     (define planning (build-path dir ".planning"))
     (define campaign (build-path planning "campaigns" plan-id))
     ;; Real user state in the planning root, the campaigns root and the
     ;; selected campaign directory. None of it is ours.
     (define plan-bak (build-path planning "PLAN.md.bak"))
     (display-to-file "operator backup; keep it" plan-bak #:exists 'replace)
     (define temp-file (build-path planning "notes.temp"))
     (display-to-file "temp; keep it" temp-file #:exists 'replace)
     (define backup-dir (build-path planning "waves-pre-plan-backup"))
     (make-directory* backup-dir)
     (define campaign-bak (build-path campaign "review-notes.bak"))
     (display-to-file "campaign backup; keep it" campaign-bak #:exists 'replace)
     (define stale-dir (build-path campaign "plan-snapshot.tmp-user"))
     (make-directory* stale-dir)
     ;; A lock FILE is not a user backup and must never be reported or probed
     ;; as one.
     (define lock-path (build-path planning "campaigns" (string-append plan-id ".lock")))
     (call-with-output-file lock-path
       (lambda (out) (write (hasheq 'owner "session-A" 'pid 4242) out))
       #:exists 'truncate)
     (define lock-before (file->bytes lock-path))
     (define explanation (plan-artifact-explanation dir #:campaign-id plan-id))
     (define out (format-plan-artifact-explanation explanation))
     (define unknown (hash-ref explanation 'unknown-artifacts))
     (check-equal? (hash-ref explanation 'errors) '() out)
     (check-true (>= (length unknown) 5))
     (for ([rel (in-list (list ".planning/PLAN.md.bak"
                               ".planning/notes.temp"
                               ".planning/waves-pre-plan-backup"
                               (format ".planning/campaigns/~a/review-notes.bak" plan-id)
                               (format ".planning/campaigns/~a/plan-snapshot.tmp-user" plan-id)))])
       (check-true (for/or ([row (in-list unknown)])
                     (equal? (hash-ref row 'path) rel))
                   (format "missing unknown artifact ~a in ~a" rel out)))
     (for ([row (in-list unknown)])
       (check-eq? (hash-ref row 'ownership) 'unowned)
       (check-false (string-suffix? (hash-ref row 'path) ".lock")))
     (check-true (string-contains?
                  out
                  (format "unknown planning artifacts (~a, informational; nothing is removed):"
                          (length unknown))))
     (check-true (string-contains? out "[unowned] .planning/PLAN.md.bak"))
     (check-false (string-contains? out "FAILED"))
     ;; The CLI renders the same additive section and stays exit 0.
     (define-values (code cli-out) (run-cli dir "--explain" "--campaign" plan-id))
     (check-equal? code 0 cli-out)
     (check-true (string-contains? cli-out "[unowned] .planning/PLAN.md.bak"))
     ;; READ-ONLY: every user artifact survives byte-identically.
     (check-true (file-exists? plan-bak))
     (check-true (file-exists? temp-file))
     (check-true (directory-exists? backup-dir))
     (check-true (file-exists? campaign-bak))
     (check-true (directory-exists? stale-dir))
     (check-equal? (file->bytes plan-bak) (string->bytes/utf-8 "operator backup; keep it"))
     (check-equal? (file->bytes lock-path) lock-before))
   (lambda () (drop-fixture dir))))

(test-case "W4 doctor: only an authentic ownership marker excludes a known owned artifact"
  (define dir (make-fixture))
  (dynamic-wind
   void
   (lambda ()
     (define campaigns (build-path dir ".planning" "campaigns"))
     (make-directory* campaigns)
     (define owned (build-path campaigns "plan-snapshot.tmp-owned"))
     (make-directory* owned)
     (call-with-output-file (build-path owned plan-snapshot-staging-marker-filename)
       (lambda (out)
         (write (hasheq 'schema-version 1
                        'kind 'plan-snapshot-staging
                        'pid 1
                        'created-at 0)
                out))
       #:exists 'truncate)
     ;; The OTHER authentic marker q writes: /plan supersede rotation.
     (define waves-backup (build-path dir ".planning" "waves-pre-plan-backup"))
     (make-directory* waves-backup)
     (call-with-output-file (build-path waves-backup ".q-gsd-plan-supersede-owner")
       (lambda (out)
         (write (hasheq 'schema-version 1
                        'kind 'waves-pre-plan-backup
                        'pid 1
                        'created-at 0)
                out))
       #:exists 'truncate)
     ;; A lookalike name with a non-authentic marker is still UNKNOWN: only the
     ;; exact kind/schema proves ownership.
     (define impostor (build-path campaigns "plan-snapshot.tmp-impostor"))
     (make-directory* impostor)
     (call-with-output-file (build-path impostor plan-snapshot-staging-marker-filename)
       (lambda (out) (write (hasheq 'schema-version 1 'kind 'not-ours) out))
       #:exists 'truncate)
     (define explanation (plan-artifact-explanation dir))
     (define out (format-plan-artifact-explanation explanation))
     (define unknown (hash-ref explanation 'unknown-artifacts))
     (define known (hash-ref explanation 'owned-planning-artifacts))
     (check-equal? (hash-ref explanation 'errors) '() out)
     (check-equal? (length known) 2 out)
     (for ([rel (in-list (list ".planning/waves-pre-plan-backup"
                               ".planning/campaigns/plan-snapshot.tmp-owned"))])
       (define row (for/first ([row (in-list known)]
                               #:when (equal? (hash-ref row 'path) rel))
                    row))
       (check-not-false row (format "missing owned artifact ~a in ~a" rel out))
       (check-eq? (hash-ref row 'ownership) 'owned))
     (check-true (for/or ([row (in-list unknown)])
                   (equal? (hash-ref row 'path)
                           ".planning/campaigns/plan-snapshot.tmp-impostor")))
     (check-false (for/or ([row (in-list unknown)])
                    (equal? (hash-ref row 'path)
                            ".planning/campaigns/plan-snapshot.tmp-owned")))
     (check-false (for/or ([row (in-list unknown)])
                    (equal? (hash-ref row 'path)
                            ".planning/waves-pre-plan-backup")))
     (check-true (string-contains? out
                                  "known owned planning artifacts (2, excluded from unknown):"))
     (check-true (string-contains? out "[owned] .planning/waves-pre-plan-backup"))
     (check-true (string-contains? out "[owned] .planning/campaigns/plan-snapshot.tmp-owned"))
     (check-true (string-contains? out
                                  "[unowned] .planning/campaigns/plan-snapshot.tmp-impostor"))
     ;; READ-ONLY: both markers and both directories survive.
     (check-true (file-exists? (build-path owned plan-snapshot-staging-marker-filename)))
     (check-true (file-exists? (build-path waves-backup ".q-gsd-plan-supersede-owner")))
     (check-true (file-exists? (build-path impostor plan-snapshot-staging-marker-filename)))
     (check-true (directory-exists? owned))
     (check-true (directory-exists? waves-backup))
     (check-true (directory-exists? impostor)))
   (lambda () (drop-fixture dir))))

;; --- W4 docs: the `## Last Failure` exclusion is span-exact -----------

(test-case "W4 docs: Last Failure exclusion ends at the next top-level #/## heading, not ###"
  (define base
    (string-append "# Wave 0\nStatus: Inbox\n\n## Files\n\n- File: a.rkt\n\n"
                   "## Last Failure\nreason one\n### Generated detail\nmore\n\n"
                   "## Done\n\nDone means closed.\n"))
  (define rewritten
    (string-append "# Wave 0\nStatus: Inbox\n\n## Files\n\n- File: a.rkt\n\n"
                   "## Last Failure\nreason two\n### Generated detail\nchanged\n\n"
                   "## Done\n\nDone means closed.\n"))
  (check-equal? (normalize-wave-doc-content base)
                (normalize-wave-doc-content rewritten)
                "the whole failure span, including ### lines, is excluded")
  ;; The next `##` heading ends the excluded span, so authored edits after it
  ;; are still compared.
  (define done-edited (string-replace base "Done means closed." "Done means open."))
  (check-false (equal? (normalize-wave-doc-content base)
                       (normalize-wave-doc-content done-edited)))
  ;; So does a top-level `#` heading.
  (define top-base
    (string-append "# Wave 0\nStatus: Inbox\n\n## Last Failure\nreason\n\n"
                   "# Top\nauthored after the span\n"))
  (define top-edited (string-replace top-base "authored after" "changed after"))
  (check-false (equal? (normalize-wave-doc-content top-base)
                       (normalize-wave-doc-content top-edited))))

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
