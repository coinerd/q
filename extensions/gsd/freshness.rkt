#lang racket/base

;; freshness.rkt — GSD /go freshness guard + build identity (BUG-0042,
;; v1.00.22 W7).
;;
;; Extracted VERBATIM from go-orchestrator.rkt (behavior-preserving
;; decomposition), where this logic previously lived inline: the
;; campaign-freshness record, checkout/origin build-version resolution,
;; staleness classification, refusal/offline-warning message
;; construction, and build-identity stamping. The guard parameter
;; (current-gsd-freshness-check) remains injectable for tests.
;; go-orchestrator re-provides these names for compatibility with
;; existing importers; new code should import this module directly.

(require racket/string
         (only-in racket/file file->string)
         (only-in "../../util/version.rkt" q-version)
         (only-in "wave-executor.rkt"
                  find-repo-root
                  default-run-git
                  git-result-code
                  git-result-stdout)
         (only-in "campaign-state.rkt"
                  set-campaign-record-build-version!
                  set-campaign-record-main-head-sha!))

(provide campaign-freshness
         campaign-freshness?
         campaign-freshness-running-version
         campaign-freshness-checkout-version
         campaign-freshness-origin-head
         campaign-freshness-behind-origin?
         campaign-freshness-offline?
         freshness-running-version
         freshness-checkout-version
         freshness-origin-head
         freshness-behind-origin?
         freshness-offline?
         FRESHNESS-VERSION-RX
         read-checkout-build-version
         resolve-origin-main-head
         checkout-behind-origin-main?
         check-campaign-freshness
         freshness-stale?
         freshness-refusal-message
         freshness-offline-warning
         stamp-campaign-build-identity!
         current-gsd-freshness-check)

;; Pure status value. running-version  : the RUNNING process's (q-version)
;; checkout-version    : freshly-read util/version.rkt version, or #f when
;;                       no repo checkout is resolvable (then nothing can
;;                       diverge and the run proceeds — legacy behavior)
;; origin-head         : origin/main HEAD SHA at /go time, or #f (offline /
;;                       no such ref / outside a work tree — never fatal)
;; behind-origin?      : #t when the checkout HEAD is a strict ancestor of
;;                       origin/main (checkout is out of date)
;; offline?            : #t when origin/main could not be resolved → the
;;                       operator is warned but NEVER blocked
(struct campaign-freshness (running-version checkout-version origin-head behind-origin? offline?)
  #:transparent)

;; Short aliases used by the guard, the /go entry path, and the tests.
(define freshness-running-version campaign-freshness-running-version)

(define freshness-checkout-version campaign-freshness-checkout-version)

(define freshness-origin-head campaign-freshness-origin-head)

(define freshness-behind-origin? campaign-freshness-behind-origin?)

(define freshness-offline? campaign-freshness-offline?)

;; util/version.rkt is `(define q-version "1.00.19")`. Read FRESH from disk —
;; the module binding in this process is exactly the thing that may be stale.
(define FRESHNESS-VERSION-RX #rx"q-version[ \t]*\"([^\"]+)\"")

(define (read-checkout-build-version repo-root)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and repo-root
         (let ([version-file (build-path repo-root "util" "version.rkt")])
           (and (file-exists? version-file)
                (let ([m (regexp-match FRESHNESS-VERSION-RX
                                       (file->string version-file #:mode 'text))])
                  (and m (cadr m))))))))

(define (resolve-origin-main-head repo-root)
  (and repo-root
       (let ([r (default-run-git repo-root '("rev-parse" "origin/main"))])
         (and (zero? (git-result-code r))
              (non-empty-string? (git-result-stdout r))
              (string-trim (git-result-stdout r))))))

;; Strictly-behind = HEAD is an ancestor of origin/main AND differs from it.
;; A campaign branch with its own commits, or a checkout equal to main, is
;; NOT behind — the guard never blocks legitimate forward work.
(define (checkout-behind-origin-main? repo-root origin-head)
  (and repo-root
       origin-head
       (let* ([head-r (default-run-git repo-root '("rev-parse" "HEAD"))]
              [head (and (zero? (git-result-code head-r))
                         (non-empty-string? (git-result-stdout head-r))
                         (string-trim (git-result-stdout head-r)))])
         (and head
              (not (string=? head origin-head))
              (zero? (git-result-code
                      (default-run-git repo-root
                                       (list "merge-base" "--is-ancestor" head origin-head))))))))

;; The authoritative /go-time check. Pure w.r.t. injected ingredients so
;; tests can simulate a stale build without mutating the real checkout.
(define (check-campaign-freshness base-dir
                                  #:running-version [running-version q-version]
                                  #:repo-root [repo-root (find-repo-root base-dir)])
  (define checkout-version (read-checkout-build-version repo-root))
  (define origin-head (resolve-origin-main-head repo-root))
  (campaign-freshness running-version
                      checkout-version
                      origin-head
                      (checkout-behind-origin-main? repo-root origin-head)
                      (not origin-head)))

;; Stale = running version ≠ checkout version (authoritative), or the
;; checkout itself is behind origin/main. Unknown checkout (#f) or offline
;; origin NEVER counts as stale — the guard fails open there.
(define (freshness-stale? f)
  (and (campaign-freshness? f)
       (or (and (freshness-checkout-version f)
                (not (string=? (freshness-running-version f) (freshness-checkout-version f))))
           (freshness-behind-origin? f))))

(define (freshness-refusal-message f)
  (cond
    [(and (freshness-checkout-version f)
          (not (string=? (freshness-running-version f) (freshness-checkout-version f))))
     (format
      (string-append
       "/go refused — restart required (running ~a, checkout ~a): the running q "
       "process predates the checked-out build. Exit and restart q, then /go "
       "again. To override anyway: /go <plan> allow-stale (records stale-override: true in the campaign record).")
      (freshness-running-version f)
      (freshness-checkout-version f))]
    [(freshness-behind-origin? f)
     (format
      (string-append "/go refused — update required: checkout HEAD is behind origin/main (~a). "
                     "Run git pull and restart q, then /go again. To override anyway: "
                     "/go <plan> allow-stale (records stale-override: true in the campaign record).")
      (freshness-origin-head f))]
    [else "/go refused — running build is stale."]))

;; Offline operators are warned, never blocked (BUG-0031 action 4).
(define (freshness-offline-warning f)
  (and
   (freshness-offline? f)
   (format
    "gsd freshness: origin/main unreachable — continuing with checkout-only version comparison (running ~a)."
    (freshness-running-version f))))

;; Stamp build identity onto a campaign record (idempotent re-stamp is fine —
;; the running build IS the identity). stale-override is owned by the guard
;; decision and is never cleared here.
(define (stamp-campaign-build-identity! rec base-dir)
  (define repo-root (find-repo-root base-dir))
  (set-campaign-record-build-version! rec q-version)
  (set-campaign-record-main-head-sha! rec (resolve-origin-main-head repo-root))
  rec)

;; Injection point for tests: replace the /go-entry check without touching
;; the real checkout or network.
(define current-gsd-freshness-check (make-parameter check-campaign-freshness))
