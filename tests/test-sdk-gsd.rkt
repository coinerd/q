#lang racket

;; @speed fast
;; @suite default
;; @boundary integration

;; BOUNDARY: integration

;; tests/test-sdk-gsd.rkt — tests for v0.20.5 W2: GSD Convenience API
;;
;; Covers:
;;   - q:plan dispatches /plan through extension registry
;;   - q:go dispatches /go through extension registry
;;   - q:gsd-status returns snapshot or 'no-active-session
;;   - q:reset-gsd! resets all GSD state
;;   - Edge cases: no extension registry, no submit text, no session

(require rackunit
         racket/file
         racket/system
         "../interfaces/sdk.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../extensions/gsd-planning.rkt"
         "../util/event/event-bus.rkt"
         "helpers/mock-provider.rkt"
         "helpers/temp-fs.rkt"
         (only-in "../util/event/event.rkt" event-session-id)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  campaign-result-status
                  campaign-result-completed-waves
                  campaign-result-message)
         (only-in "../agent/verification/verifier-core.rkt" current-verifier-enabled))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-gsd-runtime #:with-ext-reg? [with-ext? #f] #:with-session? [with-sess? #f])
  (define tmp (make-temporary-file "/tmp/sdk-gsd-test-~a" 'directory))
  (define git (find-executable-path "git"))
  (when git
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (unless (zero? (system*/exit-code git "-C" tmp "init" "--quiet"))
        (error 'make-gsd-runtime "could not initialize test repository"))))
  (define prov (make-simple-mock-provider "done" "done" "done"))
  (define ext-reg (and with-ext? (make-extension-registry)))
  (when with-ext?
    (register-extension! ext-reg the-extension))
  (define rt
    (make-runtime #:provider prov
                  #:session-dir tmp
                  #:extension-registry ext-reg
                  #:register-default-tools? #f))
  (define opened
    (if with-sess?
        (open-session rt)
        rt))
  (values opened tmp))

(define (cleanup-gsd! tmp)
  (reset-all-gsd-state!)
  (delete-directory/files tmp #:must-exist? #f))

;; ============================================================
;; Tests
;; ============================================================

(test-case "W2: q:gsd-status returns 'no-active-session when inactive"
  (reset-all-gsd-state!)
  (check-equal? (q:gsd-status) 'no-active-session))

(test-case "W2: q:reset-gsd! clears all state"
  (reset-all-gsd-state!)
  ;; Set some state
  (set-gsd-mode! 'planning)
  (check-equal? (gsd-mode) 'planning)
  ;; Reset via SDK
  (q:reset-gsd!)
  (check-false (gsd-mode)))

(test-case "W2: q:plan returns 'no-extension-registry without ext-reg"
  (define-values (rt tmp) (make-gsd-runtime))
  (define-values (rt2 result) (q:plan rt "test task"))
  (check-equal? result 'no-extension-registry)
  (cleanup-gsd! tmp))

(test-case "W2: q:go returns 'no-extension-registry without ext-reg"
  (define-values (rt tmp) (make-gsd-runtime))
  (define-values (rt2 result) (q:go rt))
  (check-equal? result 'no-extension-registry)
  (cleanup-gsd! tmp))

(test-case "W2: q:plan dispatches through extension and returns submit text (no session)"
  (reset-all-gsd-state!)
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t))
  (define-values (rt2 result) (q:plan rt "build a foo"))
  ;; The GSD extension should return a hook-amend with submit text
  ;; Since no session, we get back the submit text string
  (check-pred string? result "q:plan should return submit text string when no session")
  (check-true (string-contains? result "build a foo") "submit text should contain the task")
  (cleanup-gsd! tmp))

;; Campaign-layer fixtures must be INDEX-format (BUG-0023 W2): the campaign
;; record seeds from parse-plan-index rows, so bare inline plans yield zero
;; actionable waves. Write PLAN.md index + per-wave docs on disk.
(define (write-index-plan! tmp waves)
  (define plan-dir (build-path tmp ".planning"))
  (make-directory* plan-dir)
  (define waves-dir (build-path plan-dir "waves"))
  (make-directory* waves-dir)
  (call-with-output-file (build-path plan-dir "PLAN.md")
                         (lambda (out)
                           (for ([w waves])
                             (fprintf out
                                      "- [Inbox] W~a: ~a → waves/W~a-~a.md\n"
                                      (list-ref w 0)
                                      (list-ref w 1)
                                      (list-ref w 0)
                                      (list-ref w 2))))
                         #:exists 'truncate)
  (for ([w waves])
    (call-with-output-file (build-path waves-dir (format "W~a-~a.md" (list-ref w 0) (list-ref w 2)))
                           (lambda (out)
                             (fprintf out
                                      "## Wave ~a: ~a\n- File: q/test~a.rkt\n- Verify: raco test\n"
                                      (list-ref w 0)
                                      (list-ref w 1)
                                      (list-ref w 0)))
                           #:exists 'truncate)))

(test-case "W2: q:go dispatches through extension and returns submit text (no session)"
  (reset-all-gsd-state!)
  ;; Need a PLAN.md for /go to work — write one via planning-write
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t))
  ;; First write a plan via planning-write (directly)
  (define plan-dir (build-path tmp ".planning"))
  (make-directory* plan-dir)
  (write-index-plan! tmp '((0 "Test" "test")))
  ;; Pin the planning dir so the extension finds it
  (set-pinned-planning-dir! tmp)
  (define-values (rt2 result) (q:go rt))
  ;; /go should find the plan and return submit text
  (check-pred string? result "q:go should return submit text when no session")
  (check-true (string-contains? result "IMPLEMENT")
              "submit text should contain implementation instructions")
  (cleanup-gsd! tmp))

(test-case "W2: q:plan with session runs prompt through provider"
  (reset-all-gsd-state!)
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t #:with-session? #t))
  (define-values (rt2 result) (q:plan rt "build a bar"))
  ;; Should have run the prompt through the mock provider
  (check-not-equal? result 'no-extension-registry)
  (check-not-equal? result 'no-active-session)
  (cleanup-gsd! tmp))

(test-case "q:go uses a fresh SDK session and stops without delivery evidence"
  (reset-all-gsd-state!)
  (define-values (rt tmp) (make-gsd-runtime #:with-ext-reg? #t #:with-session? #t))
  (define initiating-id (hash-ref (session-info rt) 'session-id))
  (define observed-session-ids (box '()))
  (subscribe-events! rt
                     (lambda (evt)
                       (define sid (event-session-id evt))
                       (when (and sid (not (equal? sid initiating-id)))
                         (set-box! observed-session-ids (cons sid (unbox observed-session-ids))))))
  (define plan-dir (build-path tmp ".planning"))
  (make-directory* plan-dir)
  (write-index-plan! tmp '((0 "Test" "test") (1 "Test two" "test-two")))
  (set-pinned-planning-dir! tmp)
  (define-values (rt2 result)
    (parameterize ([current-verifier-enabled #f])
      (q:go rt)))
  (check-eq? (campaign-result-status result)
             'wave-failed
             (format "~a completed=~a"
                     (campaign-result-message result)
                     (campaign-result-completed-waves result)))
  (check-equal? (campaign-result-completed-waves result) '())
  (check-equal? (hash-ref (session-info rt2) 'session-id)
                initiating-id
                "q:go restores SDK ownership to the initiating session")
  (check-equal? (gsd-mode)
                'executing
                "advisory rejection must not publish an authoritative idle/done transition")
  (check >=
         (length (remove-duplicates (unbox observed-session-ids)))
         1
         "the attempted wave should publish from a fresh session")
  (cleanup-gsd! tmp))
