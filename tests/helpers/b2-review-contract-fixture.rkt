#lang racket/base
;; B2-owned fixture for tests/test-gsd-auto-review-production-contract.rkt
;; (milestone #899 wave B2), consolidated onto the shared stateless
;; construction library `review-contract-common.rkt` (milestone #900 W4a).
;;
;; This module is the B2 BINDING, not a second construction: it pins the
;; frozen B2 identity and re-provides the historical `b2-*` names over the
;; shared library, so the consumer's public surface is unchanged. The
;; shared module owns the real Git/receipt/Verify/environment construction
;; (same honesty boundaries as before the consolidation: real Git worlds,
;; real plan snapshots, the production dispatch seam and journal writer
;; inputs; `gh` is a PATH shim and the receipt-history seed is the
;; documented simulated prior generation).
;;
;; B2-specific facts bound here:
;;   * slug `owner/repo`, wave branch `campaign/v9.9.9-w1`, label
;;     `v9.9.9-w1`, issue #9686, milestone #895;
;;   * with-review-worktree? #f — B2 has no registered review checkout
;;     step (that registration is B1-only);
;;   * the `b2` namespace for plan-id seeds, temp-dir prefixes, error tags,
;;     probe file names and reviewer probe commands;
;;   * the owned-proof end-to-end path stays owned by the slow production
;;     suite.
(require (prefix-in common: "review-contract-common.rkt"))

(define b2-cfg
  (common:review-fixture-config "b2" "owner/repo" "campaign/v9.9.9-w1" "v9.9.9-w1" 9686 895 #f))

(provide b2-slug
         b2-wave-branch
         b2-fixture-label
         b2-issue
         b2-milestone
         b2-plan-id
         b2-put-text!
         b2-git!
         b2-git-raw!
         b2-copy-review-world!
         b2-cleanup-world!
         b2-generation-rel
         b2-campaign-root
         b2-repo-link!
         b2-write-receipt!
         b2-journal-path
         b2-evidence-rel
         b2-review-rel
         b2-validation-rel
         b2-install-gh-shim!
         b2-controller-environment
         b2-manual-controller-seam
         b2-owned-controller-seam
         b2-gate-red-findings
         b2-approved-validation
         b2-rejected-runner-result
         b2-remote-tip
         b2-red-probe-branch
         build-red-probe-world!
         b2-probe-guard
         b2-warm-review-template!)

(define b2-slug (common:review-fixture-config-slug b2-cfg))
(define b2-wave-branch (common:review-fixture-config-wave-branch b2-cfg))
(define b2-fixture-label (common:review-fixture-config-fixture-label b2-cfg))
(define b2-issue (common:review-fixture-config-issue b2-cfg))
(define b2-milestone (common:review-fixture-config-milestone b2-cfg))
(define b2-evidence-rel (common:fixture-evidence-rel b2-cfg))
(define b2-review-rel (common:fixture-review-rel b2-cfg))
(define b2-validation-rel (common:fixture-validation-rel b2-cfg))
(define b2-red-probe-branch (common:fixture-red-probe-branch b2-cfg))

(define (b2-plan-id seed)
  (common:fixture-plan-id b2-cfg seed))
(define (b2-put-text! path text)
  (common:put-text! path text))
(define (b2-git! dir . args)
  (apply common:git! b2-cfg dir args))
(define (b2-git-raw! dir . args)
  (apply common:git-raw! b2-cfg dir args))
(define (b2-copy-review-world! kind)
  (common:copy-review-world! b2-cfg kind))
(define (b2-cleanup-world! w)
  (common:cleanup-world! w))
(define b2-generation-rel common:generation-rel)
(define (b2-campaign-root w plan wave-index)
  (common:campaign-root b2-cfg w plan wave-index))
(define b2-repo-link! common:repo-link!)
(define (b2-write-receipt! record-receipt!
                           root
                           plan
                           wave-index
                           w
                           #:attempt-id [attempt-id "attempt-5"]
                           #:fence [fence 2]
                           #:history [history '()])
  (common:write-receipt! b2-cfg
                         record-receipt!
                         root
                         plan
                         wave-index
                         w
                         #:attempt-id attempt-id
                         #:fence fence
                         #:history history))
(define b2-journal-path common:journal-path)
(define (b2-install-gh-shim! w)
  (common:install-gh-shim! b2-cfg w))
(define b2-controller-environment common:controller-environment)
(define b2-manual-controller-seam common:manual-controller-seam)
(define b2-owned-controller-seam common:owned-controller-seam)
(define b2-gate-red-findings common:gate-red-findings)
(define (b2-approved-validation)
  (common:approved-validation b2-cfg))
(define b2-rejected-runner-result common:rejected-runner-result)
(define (b2-remote-tip w)
  (common:remote-tip b2-cfg w))
(define (build-red-probe-world! base plan)
  (common:build-red-probe-world! b2-cfg base plan))
(define b2-probe-guard common:probe-guard)
(define (b2-warm-review-template!)
  (common:warm-review-template! b2-cfg))
