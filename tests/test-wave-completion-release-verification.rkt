#lang racket

;;; test-wave-completion-release-verification.rkt — W0 characterization
;;; pin for BUG-0051: wave completion for a release wave checks only
;;; GIT-LOCAL facts (tag exists, main pushed). No GitHub Release-object
;;; check exists anywhere in the completion path — a wave can be marked
;;; DONE (and a release "complete") while no GitHub Release was ever
;;; published, exactly the v1.00.21 false-completion incident.
;;;
;;; Absent-seam marker (v1.00.19 freshness-pin precedent): we pin the
;;; source-level absence of any Release-object verification in the
;;; three completion-path modules.
;;;
;;; Flip owner: W6 (release verification gate). When W6 adds a GitHub
;;; Release-object check to the completion path, this pin must be
;;; flipped to assert the check exists (and fails completion when the
;;; Release object is missing).

(require rackunit
         racket/file
         racket/path)

;; Module-path repo-root: robust under `raco test -t` (run-tests.rkt
;; invocation), where find-system-path 'run-file names the raco
;; executable rather than this test file.
(define repo-root
  (simplify-path
   (build-path
    (simplify-path
     (resolved-module-path-name
      (variable-reference->resolved-module-path (#%variable-reference))))
    'up 'up)))

;; The completion path: delivery verification + wave completion +
;;; orchestration.
(define completion-modules
  (list "extensions/gsd/delivery-verifier.rkt"
        "extensions/gsd/wave-completion.rkt"
        "extensions/gsd/go-orchestrator.rkt"))

;; A GitHub Release-object check would look like a gh CLI release
;;; query, a REST releases endpoint, or a release-fetch call.
(define github-release-rx
  #px"(?i:gh\\s+(release|run)|api\\.github\\.com|/releases|releases/latest|release-view|list-releases)")

(for ([module (in-list completion-modules)])
  (define src (file->string (build-path repo-root module)))
  (check-false
   (regexp-match? github-release-rx src)
   (format "~a contains no GitHub Release-object check (absent seam)" module)))

;; And the completion path DOES contain the git-local facts it relies
;;; on instead: comparing against origin/main.
(check-not-false
 (regexp-match? #rx"origin/main"
                (file->string (build-path repo-root "extensions/gsd/delivery-verifier.rkt")))
 "completion path relies on git-local facts (origin/main) — no remote Release verification")

(displayln "PASS test-wave-completion-release-verification (BUG-0051 pin: no GitHub Release-object check in completion path)")
