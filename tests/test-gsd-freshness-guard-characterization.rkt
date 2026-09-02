#lang racket/base

;; @speed fast
;; @suite gsd
;; @boundary pure

;; q/tests/test-gsd-freshness-guard-characterization.rkt
;;
;; W3 POST-CHANGE PIN — BUG-0031 (flipped from the W0 characterization)
;;
;; W0 pinned the DEFECT: /go campaign records carried no build identity and
;; no staleness-refusal existed on the /go entry path. W3 added:
;;   1. build-version / main-head-sha / stale-override fields on the
;;      campaign-record struct, persisted in every record write;
;;   2. a version-freshness guard at /go entry ("restart required
;;      (running X, checkout Y)") with an explicit `allow-stale` escape
;;      hatch through the command parser.
;;
;; This file now pins the FIXED behavior:
;;   - a record with identity set persists it (datum scan finds the keys);
;;   - the struct schema carries the three new fields (12 slots);
;;   - legacy pre-campaign records (fields absent) deserialize with #f,
;;     never failing the load (record-schema evolution requirement);
;;   - the go-orchestrator source surface contains the guard concepts.
;; Pure-level pin: temp dirs + structs + source surface only; no live
;; TUI / worker subprocess.

(require racket/file
         racket/format
         racket/list
         rackunit
         rackunit/text-ui
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt")

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

;; Resolve paths relative to THIS test file (not the invocation cwd), so
;; the pin works from any working directory.
(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
(define here (simplify-path (build-path this-file 'up 'up)))
(define (repo-file . parts)
  (apply build-path (cons here parts)))

(define ORCHESTRATOR-SRC (repo-file "extensions" "gsd" "go-orchestrator.rkt"))

;; Deep-collect every SYMBOL appearing anywhere in a persisted .rktd datum
;; (keys, values, nested lists alike).
(define (datum-symbols v)
  (cond
    [(symbol? v) (list v)]
    [(pair? v) (append (datum-symbols (car v)) (datum-symbols (cdr v)))]
    [(vector? v) (append-map datum-symbols (vector->list v))]
    [(box? v) (datum-symbols (unbox v))]
    [else '()]))

;; A minimal, fully-valid campaign record as /go would persist it.
;; plan-id MUST equal the computed manifest hash (the repository fails
;; closed otherwise), so it is derived, never hardcoded.
(define (make-pin-manifest)
  (make-campaign-manifest 1 "freshness-pin-campaign" '() '() "pin-constraints"))

(define (make-pin-record)
  (define manifest (make-pin-manifest))
  (define wave (make-campaign-wave* 0 "W0" 'failed 1 #f))
  (make-campaign-record (campaign-manifest-hash manifest) manifest (list wave) #f #f 'pin 1 2))

;; Locate the .rktd the repository persisted under `base` (layout-agnostic).
(define (find-rktd base)
  (define hits
    (filter (lambda (p) (regexp-match? #rx"[.]rktd$" p))
            (find-files (lambda (p) (file-exists? p)) base)))
  (and (pair? hits) (car hits)))

;; ------------------------------------------------------------
;; Suite
;; ------------------------------------------------------------

(define suite
  (test-suite "BUG-0031 post-W3: /go records build identity; freshness guard on the /go path"

    (test-case "persisted campaign record carries build identity (datum scan)"
      (define tmp (make-temporary-file "bug31-pin-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define rec (make-pin-record))
                      ;; W3 stamps these at campaign start; simulate the stamp here.
                      (set-campaign-record-build-version! rec "9.9.99-pin")
                      (set-campaign-record-main-head-sha! rec "abcdef0123456789")
                      (set-campaign-record-stale-override! rec #t)
                      (persist-campaign! tmp rec)
                      (define rktd (find-rktd tmp))
                      (unless rktd
                        (fail "repository persisted no .rktd — pin cannot observe the schema"))
                      (define d (with-input-from-file rktd read))
                      ;; W3's schema is positional: (campaign-record pid manifest waves
                      ;; cancellation fence prov created updated build-version
                      ;; main-head-sha stale-override cumulative-usage) — 13 list
                      ;; elements (BUG-0039 W5 appended the trailing usage slot). The
                      ;; three identity values ride in slots 9-11.
                      (check-true (and (list? d) (= (length d) 13))
                                  (format "campaign datum is not the 13-element W5 form: ~s"
                                          (and (list? d) (length d))))
                      (check-equal? (list-ref d 9)
                                    "9.9.99-pin"
                                    "build-version value missing from persisted record")
                      (check-equal? (list-ref d 10)
                                    "abcdef0123456789"
                                    "main-head-sha value missing from persisted record")
                      (check-true (eq? (list-ref d 11) #t)
                                  "stale-override value missing from persisted record")
                      (define body (file->string rktd))
                      ;; Sanity: the scan actually read THIS campaign's record.
                      (check-true (and (regexp-match? #rx"freshness-pin-campaign" body) #t)
                                  "expected the pinned campaign's title in the persisted record"))
                    (lambda () (delete-directory/files tmp))))

    (test-case "record schema struct carries the three build-identity fields"
      ;; struct->vector length pins the exact field set:
      ;; #(struct:campaign-record plan-id manifest waves cancellation
      ;;   fence-token provenance created-at updated-at build-version
      ;;   main-head-sha stale-override budget-pause snapshot-path
      ;;   snapshot-digest) => 15 slots. BUG-0052 added the two immutable
      ;;   plan-snapshot binding fields.
      (define rec (make-pin-record))
      (check-equal? (vector-length (struct->vector rec)) 15)
      ;; #:auto fields default to #f on the legacy 8-arg constructor.
      (check-false (campaign-record-build-version rec))
      (check-false (campaign-record-main-head-sha rec))
      (check-false (campaign-record-stale-override rec))
      ;; Round-trip through the fail-closed repository proves the durable
      ;; schema both preserves the fields when set AND tolerates their
      ;; absence (an in-flight record from before this change must never fail to load).
      (define tmp (make-temporary-file "bug31-rt-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      ;; legacy-shaped record: fields unset (legacy-shaped)
                      (persist-campaign! tmp rec)
                      (define back (load-campaign-record tmp (campaign-plan-id rec)))
                      (check-true (and (campaign-record? back) #t)
                                  "legacy record without identity fields failed to load")
                      (check-equal? (vector-length (struct->vector back)) 13)
                      (check-false (campaign-record-build-version back)
                                   "legacy record must deserialize missing build-version as #f")
                      (check-false (campaign-record-main-head-sha back)
                                   "legacy record must deserialize missing main-head-sha as #f")
                      (check-false (campaign-record-stale-override back)
                                   "legacy record must deserialize missing stale-override as #f")
                      ;; identity-bearing record: values survive the round trip
                      (set-campaign-record-build-version! rec "1.0.1")
                      (set-campaign-record-main-head-sha! rec "deadbeef")
                      (set-campaign-record-stale-override! rec #t)
                      (persist-campaign! tmp rec)
                      (define back2 (load-campaign-record tmp (campaign-plan-id rec)))
                      (check-equal? (campaign-record-build-version back2) "1.0.1")
                      (check-equal? (campaign-record-main-head-sha back2) "deadbeef")
                      (check-true (eq? (campaign-record-stale-override back2) #t)))
                    (lambda () (delete-directory/files tmp))))

    (test-case "staleness-refusal / freshness-guard exists on the /go path"
      ;; Flipped from the W0 absence-scan: the go-orchestrator (the /go
      ;; entry) must now contain the freshness guard concepts — the guard
      ;; itself and the allow-stale escape hatch.
      (define src (file->string ORCHESTRATOR-SRC))
      (for ([pat (in-list (list #rx"freshness" #rx"build-version" #rx"allow-stale"))])
        (check-true (and (regexp-match? pat src) #t)
                    (format "~a missing from go-orchestrator.rkt — W3 regression" pat))))))

(module+ main
  (exit (run-tests suite)))
