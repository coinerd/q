#lang racket/base

;; @speed fast
;; @suite gsd
;; @boundary pure

;; q/tests/test-gsd-freshness-guard-characterization.rkt
;;
;; WAVE W0 CHARACTERIZATION PIN — BUG-0031
;; (campaign: executor-infrastructure defects; flipping wave: W3)
;;
;; BUG: A long-running q TUI keeps executing the modules it loaded at
;; startup. After a release, /go campaigns run on OLD code until the
;; operator restarts — nothing warns, blocks, or even RECORDS which build
;; produced the evidence. Tonight (v1.00.18 W1) this produced recorded
;; campaign failure messages emitted by pre-v1.00.17 pool code, which
;; downstream attempts then treated as ground truth.
;;
;; Live evidence: see the BUG-0031 report Evidence index (tmux q-go
;; 2026-08-25: executor proved the coordinator had recorded evidence from
;; stale modules — "the recorded failure message had no mapping lines,
;; meaning the coordinator ran old/stale code").
;;
;; THIS FILE PINS TODAY'S (BROKEN) BEHAVIOR. It PASSES against the defect:
;;   1. /go campaign records carry NO build identity of any kind
;;      (persisted .rktd datum contains no build-version / main-head /
;;      freshness key anywhere in the tree).
;;   2. No staleness-refusal / freshness-guard exists on the /go entry
;;      path (no such concept in the go-orchestrator module surface).
;; Wave W3 flips these pins into presence-tests when it adds
;; {build-version, main-head-sha-at-start} recording and the stale-build
;; refusal. Pure-level pin: temp dirs + structs + source surface only;
;; no live TUI / worker subprocess.

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
;; (keys, values, nested lists alike). A "key position" distinction is not
;; needed for characterization: today NO build identity exists ANYWHERE in
;; the record, so any occurrence flips the pin.
(define (datum-symbols v)
  (cond
    [(symbol? v) (list v)]
    [(pair? v) (append (datum-symbols (car v)) (datum-symbols (cdr v)))]
    [(vector? v) (append-map datum-symbols (vector->list v))]
    [(box? v) (datum-symbols (unbox v))]
    [else '()]))

;; A minimal, fully-valid campaign record as /go would persist it after an
;; infra-failed attempt (the exact scenario from the bug report: evidence
;; recorded by an executor session that then died on a provider failure).
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
  (test-suite "BUG-0031 characterization: /go records no build identity; no freshness guard"

    (test-case "persisted campaign record carries NO build identity (datum scan)"
      (define tmp (make-temporary-file "bug31-pin-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (persist-campaign! tmp (make-pin-record))
         (define rktd (find-rktd tmp))
         (unless rktd
           (fail "repository persisted no .rktd — pin cannot observe the schema"))
         (define syms (datum-symbols (with-input-from-file rktd read)))
         ;; W3 adds {build-version, main-head-sha-at-start} (and plausibly a
         ;; freshness/staleness marker) to every campaign record. Today: zero.
         (define offenders
           (filter (lambda (s)
                     (regexp-match? #rx"build|freshness|main-head|stale" (symbol->string s)))
                   syms))
         (check-equal?
          offenders
          '()
          (format "campaign record already carries build identity ~s — W3 landed; flip this pin"
                  offenders))
         ;; Sanity: the scan actually read THIS campaign's record (the
         ;; repository persists positional/list data, not keyed fields —
         ;; which is itself part of today's characterization).
         (check-true (and (regexp-match? #rx"freshness-pin-campaign" (file->string rktd)) #t)
                     "expected the pinned campaign's title in the persisted record"))
       (lambda () (delete-directory/files tmp))))

    (test-case "record schema struct itself has no build-identity field"
      ;; The transparent campaign-record struct is the write-side schema;
      ;; W3 must extend it before anything can be recorded. struct->vector
      ;; length pins today's exact field set:
      ;; #(struct:campaign-record plan-id manifest waves cancellation
      ;;   fence-token provenance created-at updated-at) => 9 slots.
      (define rec (make-pin-record))
      (check-equal? (vector-length (struct->vector rec)) 9)
      ;; Round-trip through the fail-closed repository proves the durable
      ;; schema accepts (and preserves absence of) this shape.
      (define tmp (make-temporary-file "bug31-rt-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (persist-campaign! tmp rec)
                      (define back (load-campaign-record tmp (campaign-plan-id rec)))
                      (check-true (and (campaign-record? back) #t))
                      (check-equal? (vector-length (struct->vector back)) 9))
                    (lambda () (delete-directory/files tmp))))

    (test-case "no staleness-refusal / freshness-guard exists on the /go path"
      ;; Characterization of the module surface: the go-orchestrator (the
      ;; /go entry) contains no freshness/staleness/build-version concept —
      ;; in code, prompts, or comments. W3's guard flips this scan.
      (define src (file->string ORCHESTRATOR-SRC))
      (for ([pat (in-list (list #rx"freshness"
                                #rx"stale-build"
                                #rx"build-version"
                                #rx"main-head"
                                #rx"version-guard"
                                #rx"allow-stale"))])
        (check-false (and (regexp-match? pat src) #t)
                     (format "~a already present in go-orchestrator.rkt — W3 landed; flip this pin"
                             pat))))))

(module+ main
  (exit (run-tests suite)))
