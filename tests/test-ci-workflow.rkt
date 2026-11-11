#lang racket

;; @suite ci
;; @speed fast
;; @boundary integration
;; tests/test-ci-workflow.rkt
;;
;; Workflow action-version policy (#9566 #9567 #9770).
;;
;; The release lanes (release.yml, release-core.yml, release-repair.yml)
;; pin actions to immutable commit SHAs; those pins are the repository's
;; canonical version for each action. A lane pinned to a SHA must pin
;; the SAME SHA as the release lanes, and a superseded ref (checkout
;; v5, download-artifact v7, upload-artifact v4) must not survive
;; anywhere. Test-facing mutable tags (@v7, @v8) are out of scope here.
;; Composite actions (.github/actions/*) are held to the same
;; one-canonical-SHA policy (#9883: dependabot bumped every workflow lane
;; to download-artifact v8.0.2 while a composite action kept v8.0.1).

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string)

(define checkout-v701-sha "3d3c42e5aac5ba805825da76410c181273ba90b1")
;; Canonical download-artifact pin = upstream tag v8.0.2 (git ls-remote
;; https://github.com/actions/download-artifact.git refs/tags/v8.0.2).
(define download-artifact-v8-sha "9000827ccba6bdab643e8b6fd33ac0654aef8333")
;; The superseded v8.0.1 pin (3e5f45b2…, the #9883 drift) — never canonical.
(define download-artifact-v801-sha "3e5f45b2cfb9172054b4087a40e8e0b5a5461e7c")
(define upload-artifact-v7-sha "043fb46d1a93c77aae656e7c1c64a875d1fc6a0a")

(define-runtime-path ci-yml-path "../.github/workflows/ci.yml")
(define-runtime-path release-yml-path "../.github/workflows/release.yml")
(define-runtime-path release-core-yml-path "../.github/workflows/release-core.yml")
(define-runtime-path workflows-dir "../.github/workflows")
(define-runtime-path pilot-yml-path "../.github/workflows/prepared-environment-pilot.yml")
(define-runtime-path full-regression-yml-path "../.github/workflows/full-regression.yml")
(define-runtime-path pr-latency-guard-yml-path "../.github/workflows/pr-latency-guard.yml")
(define-runtime-path ci-exit-truth-canary-yml-path "../.github/workflows/ci-exit-truth-canary.yml")
(define-runtime-path actions-dir "../.github/actions")

(define (read-pilot-yml)
  (file->string pilot-yml-path))
(define (read-full-regression-yml)
  (file->string full-regression-yml-path))
(define (read-pr-latency-guard-yml)
  (file->string pr-latency-guard-yml-path))
(define (read-ci-exit-truth-canary-yml)
  (file->string ci-exit-truth-canary-yml-path))

;; Every `uses: <action>@<ref>` ref of the named action in a workflow text.
;; pregexp: the rx dialect does not support [[:space:]] character classes.
(define (action-refs content action)
  (regexp-match* (pregexp (string-append "uses: " (regexp-quote action) "@([^[:space:]]+)"))
                 content
                 #:match-select cadr))

(test-case "pilot checkout pins the repository checkout v7.0.1 SHA (#9566)"
  (check-equal? (action-refs (read-pilot-yml) "actions/checkout")
                (list checkout-v701-sha)
                "the pilot's only checkout must pin the checkout v7.0.1 SHA")
  (check-true (string-contains? (file->string release-core-yml-path) checkout-v701-sha)
              "sanity: release lanes must keep pinning the same checkout SHA"))

(test-case "download-artifact in ci/full-regression/canary pins the v8 SHA (#9567)"
  (for ([path (in-list (list ci-yml-path full-regression-yml-path ci-exit-truth-canary-yml-path))])
    (define refs (action-refs (file->string path) "actions/download-artifact"))
    (check-true (pair? refs) "each audited lane must still consume download-artifact")
    (check-false (member "v7" refs) "superseded download-artifact@v7 must not survive")
    (check-true (andmap (lambda (ref) (string=? ref download-artifact-v8-sha)) refs)
                "every download-artifact ref must be the release-lane v8 SHA")))

(test-case "composite actions pin the same download-artifact SHA (#9883)"
  ;; Fail-closed over EVERY composite action: the #9883 drift lived exactly
  ;; here (restore-racket-environment kept v8.0.1 while all lanes moved to
  ;; v8.0.2), so the policy scan must cover .github/actions too.
  (define action-yml-paths
    (for/list ([name (in-list (sort (map path->string (directory-list actions-dir)) string<?))]
               #:when (file-exists? (build-path actions-dir name "action.yml")))
      (build-path actions-dir name "action.yml")))
  (define refs
    (apply append
           (for/list ([path (in-list action-yml-paths)])
             (action-refs (file->string path) "actions/download-artifact"))))
  (check-true (pair? refs) "composite-action coverage must not go vacuous")
  (check-true (andmap (lambda (ref) (string=? ref download-artifact-v8-sha)) refs)
              "every composite-action download-artifact ref must be the canonical v8 SHA")
  (check-false (member download-artifact-v801-sha refs)
               "the superseded download-artifact v8.0.1 pin must not survive (#9883)"))

(test-case "pr-latency-guard upload-artifact pins the repository v7 SHA (#9770)"
  (check-equal? (action-refs (file->string pr-latency-guard-yml-path) "actions/upload-artifact")
                (list upload-artifact-v7-sha)
                "pr-latency-guard must pin the upload-artifact v7 SHA")
  (check-true (string-contains? (file->string release-yml-path) upload-artifact-v7-sha)
              "sanity: release lanes must keep pinning the same upload-artifact SHA"))

(test-case "use policy: no superseded checkout/artifact refs remain anywhere"
  ;; Fail-closed scan over EVERY workflow file (including future lanes).
  (define content
    (apply string-append
           (for/list ([name (in-list (sort (map path->string (directory-list workflows-dir))
                                           string<?))])
             (file->string (build-path workflows-dir name)))))
  (check-false (string-contains? content "actions/checkout@08c6903cd8c0fde910a37f88322edcfb5dd907a8")
               "the surviving checkout v5 pin must be upgraded")
  (check-false (string-contains? content "actions/download-artifact@v7")
               "superseded download-artifact@v7 must be upgraded to the v8 SHA")
  (check-false
   (string-contains? content (string-append "actions/download-artifact@" download-artifact-v801-sha))
   "superseded download-artifact v8.0.1 must be upgraded to the canonical v8 SHA")
  (check-false (string-contains? content "actions/upload-artifact@v4")
               "superseded upload-artifact@v4 must be upgraded to the v7 SHA"))
