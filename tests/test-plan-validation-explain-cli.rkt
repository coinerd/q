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
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  migrate-campaign!)
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
                  (check-true (string-contains? out "identity matches: n/a (no bound campaign)"))
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
     (check-equal? code 0 out)
     (check-true (string-contains? out "bound body matches live: NO (drift)"))
     (check-true (string-contains? out "snapshot drift (1)"))
     (check-true (string-contains? out "frozen-contract-stale"))
     (check-true (string-contains? out "(snapshot-drift-frozen-contract-stale)"))
     (check-true (string-contains? out "-> next:")))
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
