#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; tests/test-gsd-red-checkpoint-instance-key.rkt — #9899 evidence suite
;; (v1.00.32 W3).
;;
;; The RED checkpoint location was deterministic in (base attempt fence)
;; alone. A campaign attempt that failed mid-phase and was superseded or
;; recreated kept the same plan-id (it hashes the plan manifest) and the
;; same attempt fence, so the recreated incarnation re-derived the SAME
;; checkpoint root — and the poisoned state there ("no completed RED
;; checkpoint; do not relaunch") refused every relaunch forever, forcing
;; plan-bytes churn to recover.
;;
;; The flip under test: location-key mixes a per-campaign-instance
;; component into the hash input. The instance is resolved from the
;; durable campaign record (its created-at) through the existing campaign
;; repository seam, so a recreated record (new created-at) lands on a
;; fresh checkpoint namespace while retries inside one instance keep
;; their stable location. Records that do not resolve to an instance
;; (no durable record) keep the legacy hash input byte-for-byte, so
;; old-format checkpoint locations (no instance component) remain
;; loadable — nothing migrates and no old checkpoint is invalidated.
;;
;; RED baseline (pre-fix): the instance bindings do not exist. The new
;; bindings are therefore resolved DYNAMICALLY so the RED baseline fails
;; at runtime with clean rackunit failures instead of a module-load
;; error (a failing compile would read as a broken tree).

(require rackunit
         racket/path
         racket/runtime-path
         "../extensions/gsd/red-checkpoint.rkt"
         (only-in "../extensions/gsd/campaign-state.rkt"
                  campaign-manifest-hash
                  campaign-attempt
                  make-campaign-manifest
                  make-campaign-record
                  make-campaign-wave)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign!)
         (only-in "../extensions/gsd/review-proof.rkt" current-owned-proof-root)
         (only-in "../util/json/checksum.rkt" sha256-string)
         (only-in "helpers/temp-fs.rkt" with-temp-dir))

(define-runtime-path red-checkpoint-src "../extensions/gsd/red-checkpoint.rkt")

;; ── The #9899 bindings under test (dynamic: #f before the flip) ──────

(define (red-binding name)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require red-checkpoint-src name)))

(define location-key (red-binding 'location-key))
(define campaign-instance (red-binding 'campaign-instance))

(define (require-flip)
  (unless (and location-key campaign-instance)
    (fail
     "#9899 flip not implemented: red-checkpoint.rkt does not export the
campaign-instance/location-key helpers")))

;; ── Campaign-record fixture (same on-disk seam as the coordinator) ───
;; The plan manifest is byte-identical across incarnations, so the
;; plan-id — the location's first path component — never changes; only
;; the record's created-at distinguishes a recreated campaign.

(define fixture-manifest (make-campaign-manifest 1 "instance-key" '() '() "constraints"))
(define fixture-plan (campaign-manifest-hash fixture-manifest))

(define (fixture-record created-at)
  (make-campaign-record
   fixture-plan
   fixture-manifest
   (list (make-campaign-wave 1 "Instance key" 'in-progress 1 (campaign-attempt "attempt-1" 1 0)))
   #f
   8
   'test
   created-at
   created-at))

;; The checkpoint identity under test: identical across incarnations.
(define wave 3)
(define attempt "attempt-1")
(define fence 1)

(module+ test
  (test-case "red-checkpoint: the #9899 flip exports the instance-key helpers"
    (check-true (and location-key #t))
    (check-true (and campaign-instance #t)))

  (test-case "red-checkpoint: recreated campaign identities land on different checkpoint roots"
    (require-flip)
    (with-temp-dir
     (base)
     ;; Incarnation 1 of the campaign record (same plan-id, created-at 111).
     (persist-campaign! base (fixture-record 111))
     (define instance-a (campaign-instance base fixture-plan))
     (check-true (and (string? instance-a) (positive? (string-length instance-a))))
     (define root-a (location-key base fixture-plan wave attempt fence instance-a))
     ;; Incarnation 2: the record is superseded/recreated — same plan-id,
     ;; same attempt identity, NEW instance identity (created-at 222).
     (persist-campaign! base (fixture-record 222))
     (define instance-b (campaign-instance base fixture-plan))
     (check-false (equal? instance-a instance-b))
     (define root-b (location-key base fixture-plan wave attempt fence instance-b))
     (check-not-equal? root-a root-b)
     ;; The namespace really moved: instance b is not instance a's root.
     (check-false (equal? root-b (location-key base fixture-plan wave attempt fence instance-a)))))

  (test-case "red-checkpoint: one instance keeps one stable checkpoint root across calls"
    (require-flip)
    (with-temp-dir
     (base)
     (persist-campaign! base (fixture-record 111))
     (define instance (campaign-instance base fixture-plan))
     ;; Re-deriving the instance is stable (the record's created-at is
     ;; immutable within one incarnation).
     (check-equal? (campaign-instance base fixture-plan) instance)
     ;; And the location derived from it is stable across calls — retries
     ;; within one instance must keep their checkpoint reachable.
     (check-equal? (location-key base fixture-plan wave attempt fence instance)
                   (location-key base fixture-plan wave attempt fence instance))
     (check-equal?
      (location-key base fixture-plan wave attempt fence (campaign-instance base fixture-plan))
      (location-key base fixture-plan wave attempt fence instance))))

  (test-case "red-checkpoint: old-format locations (no instance component) remain loadable"
    (require-flip)
    (with-temp-dir
     (proof-store)
     (parameterize ([current-owned-proof-root (build-path proof-store "owned-review-proofs")])
       (with-temp-dir
        (base)
        ;; A record-less base resolves no instance — the checkpoint
        ;; derivation must stay the legacy one, byte-for-byte, so
        ;; checkpoints written there before this flip remain reachable
        ;; and loadable.
        (check-false (campaign-instance base fixture-plan))
        (define legacy-root (location-key base fixture-plan wave attempt fence #f))
        (check-equal?
         legacy-root
         (build-path (simplify-path (build-path (current-owned-proof-root) 'up))
                     "red-checkpoints"
                     fixture-plan
                     (format "W~a" wave)
                     (sha256-string
                      (format "~a:~a:~a" (path->string (path->complete-path base)) attempt fence))))
        ;; And the instance component is what moves NEW roots away from
        ;; the legacy namespace — it never perturbs the legacy root.
        (persist-campaign! base (fixture-record 111))
        (check-not-equal?
         legacy-root
         (location-key base fixture-plan wave attempt fence (campaign-instance base fixture-plan)))
        (check-equal? (location-key base fixture-plan wave attempt fence #f) legacy-root)))))

  (test-case "red-checkpoint: a blank instance identity is refused, not silently mixed"
    (require-flip)
    (with-temp-dir
     (base)
     (check-exn exn:fail? (lambda () (location-key base fixture-plan wave attempt fence " ")))
     (check-exn exn:fail? (lambda () (location-key base fixture-plan wave attempt fence ""))))))
