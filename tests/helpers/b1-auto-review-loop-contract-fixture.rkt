#lang racket/base
;; B1-owned fixture for tests/test-gsd-auto-review-loop-contract.rkt
;; (milestone #899 wave B1), consolidated onto the shared stateless
;; construction library `review-contract-common.rkt` (milestone #900 W4a).
;;
;; This module is the B1 BINDING, not a second construction: it pins the
;; frozen B1 identity and re-provides the historical `b1-*` names over the
;; shared library, so the consumer's public surface is unchanged. The
;; shared module owns the real Git/receipt/Verify/environment construction
;; (same honesty boundaries as before the consolidation: real Git worlds,
;; real plan snapshots, the production dispatch seam and journal writer
;; inputs; `gh` is a PATH shim and the receipt-history seed is the
;; documented simulated prior generation).
;;
;; B1-specific facts bound here:
;;   * slug `owner/b1repo`, wave branch `campaign/v9.9.9-b1`, label
;;     `v9.9.9-b1`, issue #9686, milestone #895;
;;   * with-review-worktree? #t — the reviewer opens an EXISTING clean
;;     checkout on the exact receipt branch (resolve-review-checkout), a
;;     step B2 does not have;
;;   * the `b1` namespace for plan-id seeds, temp-dir prefixes, error tags,
;;     probe file names and reviewer probe commands;
;;   * the deep two-full-Verify-chain end-to-end path stays owned by
;;     tests/test-gsd-auto-review-loop.rkt (slow/full).
(require (prefix-in common: "review-contract-common.rkt"))

(define b1-cfg
  (common:review-fixture-config "b1" "owner/b1repo" "campaign/v9.9.9-b1" "v9.9.9-b1" 9686 895 #t))

(provide b1-slug
         b1-wave-branch
         b1-fixture-label
         b1-issue
         b1-milestone
         b1-plan-id
         b1-put-text!
         b1-git!
         b1-git-raw!
         b1-copy-review-world!
         b1-cleanup-world!
         b1-generation-rel
         b1-campaign-root
         b1-repo-link!
         b1-write-receipt!
         b1-journal-path
         b1-evidence-rel
         b1-review-rel
         b1-validation-rel
         b1-install-gh-shim!
         b1-controller-environment
         b1-manual-controller-seam
         b1-owned-controller-seam
         b1-gate-red-findings
         b1-approved-validation
         b1-rejected-runner-result
         b1-remote-tip
         b1-red-probe-branch
         build-red-probe-world!
         b1-probe-guard
         b1-warm-review-template!)

(define b1-slug (common:review-fixture-config-slug b1-cfg))
(define b1-wave-branch (common:review-fixture-config-wave-branch b1-cfg))
(define b1-fixture-label (common:review-fixture-config-fixture-label b1-cfg))
(define b1-issue (common:review-fixture-config-issue b1-cfg))
(define b1-milestone (common:review-fixture-config-milestone b1-cfg))
(define b1-evidence-rel (common:fixture-evidence-rel b1-cfg))
(define b1-review-rel (common:fixture-review-rel b1-cfg))
(define b1-validation-rel (common:fixture-validation-rel b1-cfg))
(define b1-red-probe-branch (common:fixture-red-probe-branch b1-cfg))

(define (b1-plan-id seed)
  (common:fixture-plan-id b1-cfg seed))
(define (b1-put-text! path text)
  (common:put-text! path text))
(define (b1-git! dir . args)
  (apply common:git! b1-cfg dir args))
(define (b1-git-raw! dir . args)
  (apply common:git-raw! b1-cfg dir args))
(define (b1-copy-review-world! kind)
  (common:copy-review-world! b1-cfg kind))
(define (b1-cleanup-world! w)
  (common:cleanup-world! w))
(define b1-generation-rel common:generation-rel)
(define (b1-campaign-root w plan wave-index)
  (common:campaign-root b1-cfg w plan wave-index))
(define b1-repo-link! common:repo-link!)
(define (b1-write-receipt! record-receipt!
                           root
                           plan
                           wave-index
                           w
                           #:attempt-id [attempt-id "attempt-5"]
                           #:fence [fence 2]
                           #:history [history '()])
  (common:write-receipt! b1-cfg
                         record-receipt!
                         root
                         plan
                         wave-index
                         w
                         #:attempt-id attempt-id
                         #:fence fence
                         #:history history))
(define b1-journal-path common:journal-path)
(define (b1-install-gh-shim! w)
  (common:install-gh-shim! b1-cfg w))
(define b1-controller-environment common:controller-environment)
(define b1-manual-controller-seam common:manual-controller-seam)
(define b1-owned-controller-seam common:owned-controller-seam)
(define b1-gate-red-findings common:gate-red-findings)
(define (b1-approved-validation)
  (common:approved-validation b1-cfg))
(define b1-rejected-runner-result common:rejected-runner-result)
(define (b1-remote-tip w)
  (common:remote-tip b1-cfg w))
(define (build-red-probe-world! base plan)
  (common:build-red-probe-world! b1-cfg base plan))
(define b1-probe-guard common:probe-guard)
(define (b1-warm-review-template!)
  (common:warm-review-template! b1-cfg))
