#lang racket

;;; test-wave-completion-release-verification.rkt — W6 characterization
;;; pin for BUG-0051: wave completion for a release wave MUST verify the
;;; GitHub Release object before persisting DONE.
;;;
;;; Flipped by W6 (release verification gate): the v1.00.2x false-completion
;;; incident (wave marked DONE while no GitHub Release ever existed) is
;;; closed. The pin now asserts the check EXISTS in the completion path and
;;; that a release wave whose Release object is missing/draft FAILS
;;; completion with a named "release not verified: …" reason.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         (only-in "../extensions/gsd/campaign-state.rkt"
                  campaign-plan-id
                  campaign-record-waves
                  campaign-wave-status
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  set-campaign-fence-token!
                  begin-attempt!
                  set-campaign-wave-status!
                  migrate-campaign!)
         (only-in "../extensions/gsd/campaign-repository.rkt" load-campaign-record persist-campaign!)
         (only-in "../extensions/gsd/wave-completion.rkt"
                  try-complete-wave!
                  completion-result-status))

;; Module-path repo-root: robust under `raco test -t` (run-tests.rkt
;; invocation), where find-system-path 'run-file names the raco
;; executable rather than this test file.
(define repo-root
  (simplify-path
   (build-path (simplify-path (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
               'up
               'up)))

(define (src-of rel)
  (file->string (build-path repo-root rel)))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-release-campaign-dir)
  (define dir (make-temporary-file "w6-release-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display
      "# Plan: W6 release verification\n\n## Waves\n\n- [Verifying] W0: Release wave → waves/W0-release.md\n"
      out))
   #:exists 'truncate)
  (call-with-output-file (build-path dir ".planning" "waves" "W0-release.md")
                         (lambda (out)
                           (display "# Wave 0\nStatus: Verifying\n\n# Wave 0: Release wave\n" out))
                         #:exists 'truncate)
  dir)

(define (release-campaign-in-verifying dir)
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
  (persist-campaign! dir rec)
  rec)

(define (release-attempt rec)
  (campaign-wave-current-attempt (car (campaign-record-waves rec))))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

;; ============================================================
;; Tests
;; ============================================================

(define all-tests
  (test-suite "wave-completion-release-verification (W6)"

    (test-case "completion path contains the release-check seam"
      ;; wave-completion.rkt: the #:release-check gate + named failure reason.
      (define wave-completion-src (src-of "extensions/gsd/wave-completion.rkt"))
      (check-true (regexp-match? #rx"release-check" wave-completion-src)
                  "wave-completion.rkt exposes a #:release-check gate")
      (check-true (regexp-match? #rx"release not verified:" wave-completion-src)
                  "wave-completion.rkt names the failure reason 'release not verified:'")

      ;; github-port.rkt: a release-view command (read-only Release-object
      ;; query) plus a make-release-check builder wired to find-release-by-tag.
      (define github-port-src (src-of "extensions/gsd/github-port.rkt"))
      (check-true (regexp-match? #rx"release-view" github-port-src)
                  "github-port.rkt supports a release-view (Release-object query) command")
      (check-true (regexp-match? #rx"make-release-check" github-port-src)
                  "github-port.rkt provides make-release-check")
      (check-true (regexp-match? #rx"find-release-by-tag" github-port-src)
                  "github-port.rkt queries the Release object by tag")

      ;; go-orchestrator.rkt: the campaign completion path threads the
      ;; configured release check into try-complete-wave!.
      (define orchestrator-src (src-of "extensions/gsd/go-orchestrator.rkt"))
      (check-true (regexp-match? #rx"current-gsd-release-check" orchestrator-src)
                  "go-orchestrator.rkt reads current-gsd-release-check (policy parameter)")
      (check-true (regexp-match? #rx"#:release-check" orchestrator-src)
                  "go-orchestrator.rkt passes #:release-check into try-complete-wave!")

      ;; policy.rkt: the release-check policy parameter exists.
      (define policy-src (src-of "extensions/gsd/policy.rkt"))
      (check-true (regexp-match? #rx"current-gsd-release-check" policy-src)
                  "policy.rkt defines current-gsd-release-check"))

    (test-case "release wave with missing Release object fails completion with named reason"
      (define dir (make-release-campaign-dir))
      (define rec (release-campaign-in-verifying dir))
      (define attempt (release-attempt rec))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:verifier-message "delivery verified"
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)
                            #:release-check (lambda () "no GitHub Release object for tag v1.00.2x")))
      (check-eq? (completion-result-status result)
                 'failed
                 "completion FAILS when the Release object is absent")
      (check-eq? (campaign-wave-status (car (campaign-record-waves
                                             (load-campaign-record dir (campaign-plan-id rec)))))
                 'failed
                 "durable wave status is failed")
      (define doc-text
        (call-with-input-file (build-path dir ".planning" "waves" "W0-release.md") port->string))
      (check-true (regexp-match? #rx"release not verified:" doc-text)
                  "wave doc records the named release-not-verified reason")
      (cleanup-tmp dir))

    (test-case "release wave with verified Release object completes"
      (define dir (make-release-campaign-dir))
      (define rec (release-campaign-in-verifying dir))
      (define attempt (release-attempt rec))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:verifier-message "delivery verified"
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)
                            #:release-check (lambda () #f)))
      (check-eq? (completion-result-status result)
                 'done
                 "completion succeeds when the Release object is verified")
      (cleanup-tmp dir))

    (test-case "non-release wave without a release check is unaffected"
      (define dir (make-release-campaign-dir))
      (define rec (release-campaign-in-verifying dir))
      (define attempt (release-attempt rec))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:verifier-message "delivery verified"
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result)
                 'done
                 "no release gate when #:release-check is not provided")
      (cleanup-tmp dir))))

(void (run-tests all-tests))
