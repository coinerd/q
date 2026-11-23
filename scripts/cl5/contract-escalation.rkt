#lang racket/base

;; scripts/cl5/contract-escalation.rkt — ISOLATED prototype of contract-aware
;; escalation for shared behavioural contracts (issue #9997).
;;
;; Design-stage tooling. NOT wired into `./qt`, CI, admission or scheduling, and
;; it executes no tests. It answers: when a *shared contract* module changes, is
;; there a declared consumer/boundary verification set, and is that declaration
;; still true of the dependency graph?
;;
;; Why a contract layer at all: file-level ownership (the `@covers` manifest) is
;; per-test, so it cannot express "this vocabulary is consumed by these
;; consumers, whose behaviour must be re-verified". The coverage-gap audit shows
;; the consequence: 17 high-fan-in modules have an audited detector yet no
;; consumer escalation, and a vocabulary change (D5) selected none of its
;; consumers' detectors.
;;
;; Model: a declared contract is DATA.
;;   module     the shared contract module (repo-relative)
;;   kind       vocabulary | policy | protocol | shared-state
;;   consumers  modules whose behaviour depends on the contract
;;   tests      tests that must run for a contract change (owner/boundary tests)
;;
;; Fail-closed rules (each is a named escalation, never a silent narrowing):
;;   unmapped-contract  a changed module is high fan-in but has no declaration
;;   stale-consumer     a declared consumer does not actually require the module
;;   unknown-test       a declared test path does not exist
;;   no-coverage        a declaration exists but yields no runnable test
;;
;; `contract-decision` returns the required verification set; it does not run
;; it. A future lane may consume that set; this module makes no such change.

(require racket/file
         racket/list
         racket/path
         racket/set
         racket/string
         "selection-guard.rkt")

(provide default-fan-in-threshold
         repo-inputs
         evaluate-contract-changes
         validate-contract
         contract-decision
         contract-decision->jsexpr
         default-registry)

;; A module with at least this many direct dependents is a shared contract in
;; the sense this prototype uses. Chosen from the audit's fan-in distribution;
;; the threshold is a design parameter, not a policy.
(define default-fan-in-threshold 3)

(define (escalation code module detail tests)
  (hasheq 'code code 'module module 'detail detail 'tests tests))

;; validate-contract : hash (hash module -> (listof module)) (hash module -> boolean)
;;   -> (listof escalation)
;; `requires` is the forward graph; `known-tests` maps a test path to existence.
(define (validate-contract entry requires known-tests)
  (define module (hash-ref entry 'module))
  (define consumers (hash-ref entry 'consumers '()))
  (define tests (hash-ref entry 'tests '()))
  (append
   (for/list ([consumer (in-list consumers)]
              #:unless (member module (transitive-requires requires consumer)))
     (escalation 'stale-consumer
                 module
                 (format "declared consumer ~a does not require the contract module" consumer)
                 (list consumer)))
   (for/list ([test (in-list tests)]
              #:unless (hash-ref known-tests test #f))
     (escalation 'unknown-test module (format "declared test ~a does not exist" test) (list test)))))

;; Forward requires closure, excluding the module itself.
(define (transitive-requires requires module)
  (let loop ([frontier (hash-ref requires module '())]
             [seen '()])
    (cond
      [(null? frontier) seen]
      [else
       (define f (car frontier))
       (if (or (equal? f module) (member f seen))
           (loop (cdr frontier) seen)
           (loop (append (cdr frontier) (hash-ref requires f '())) (cons f seen)))])))

;; contract-decision : (listof string) changed
;;                     (listof hash) registry
;;                     (hash module -> (listof test)) declared
;;                     (hash module -> (listof module)) dependents
;;                     (hash module -> (listof module)) requires
;;                     (hash module -> boolean) known-tests
;;   -> hash 'required 'escalations 'contracts
(define (contract-decision changed
                           registry
                           declared
                           dependents
                           requires
                           known-tests
                           #:fan-in-threshold [fan-in-threshold default-fan-in-threshold])
  (define by-module
    (for/hash ([entry (in-list registry)])
      (values (hash-ref entry 'module) entry)))
  (define (declared-of m)
    (hash-ref declared m '()))
  (define (fan-in m)
    (length (hash-ref dependents m '())))

  (define matched
    (for/list ([m (in-list changed)]
               #:when (hash-has-key? by-module m))
      (hash-ref by-module m)))
  (define unmatched-high-fan-in
    (for/list ([m (in-list changed)]
               #:when (and (not (hash-has-key? by-module m)) (>= (fan-in m) fan-in-threshold)))
      m))

  (define validation
    (append* (for/list ([entry (in-list matched)])
               (validate-contract entry requires known-tests))))

  ;; Required verification set: the contract's own tests, plus the declared
  ;; detectors of its consumers (a consumer change needs its own detector).
  (define required
    (sort
     (remove-duplicates
      (append* (for/list ([entry (in-list matched)])
                 (append (hash-ref entry 'tests '())
                         (append* (for/list ([consumer (in-list (hash-ref entry 'consumers '()))])
                                    (declared-of consumer)))))))
     string<?))

  (define unmapped
    (for/list ([m (in-list unmatched-high-fan-in)])
      (escalation 'unmapped-contract
                  m
                  (format "shared contract module with ~a dependents has no declared consumer set"
                          (fan-in m))
                  '())))
  (define empty-coverage
    (for/list ([entry (in-list matched)]
               #:when (and (null? (hash-ref entry 'tests '()))
                           (null? (append* (for/list ([c (in-list (hash-ref entry 'consumers '()))])
                                             (declared-of c))))))
      (escalation 'no-coverage
                  (hash-ref entry 'module)
                  "declaration yields no runnable test (no contract tests, no consumer detectors)"
                  '())))

  (define escalations (append unmapped validation empty-coverage))
  (hasheq 'required
          required
          'matched
          (sort (map (lambda (e) (hash-ref e 'module)) matched) string<?)
          'unmatched-high-fan-in
          (sort unmatched-high-fan-in string<?)
          'escalations
          escalations
          'verdict
          (if (pair? escalations) 'escalate 'covered)))

;; ---- a small, explicit seed registry (design example, not production data) ---------

;; Two entries only, both justified by the audit + corpus:
;;  * wave-status.rkt — the D5 vocabulary contract. Consumers are its direct
;;    dependents from the impact graph; tests are the corpus-documented owner
;;    test (tests/test-go-orchestrator-wave-status.rkt does not exist and is
;;    deliberately not listed; validate-contract would flag it).
;;  * util/error/errors.rkt — the highest fan-in module in the audit (44).
(define default-registry
  (list (hasheq 'module
                "extensions/gsd/wave-status.rkt"
                'kind
                'vocabulary
                'consumers
                (list "extensions/gsd/archive.rkt"
                      "extensions/gsd/campaign-state.rkt"
                      "extensions/gsd/go-orchestrator.rkt"
                      "extensions/gsd/projection-kernel.rkt"
                      "extensions/gsd/wave-completion.rkt")
                'tests
                (list "tests/test-gsd-wave-status.rkt"))
        (hasheq 'module
                "util/error/errors.rkt"
                'kind
                'shared-state
                'consumers
                (list "agent/loop.rkt" "runtime/agent-session.rkt")
                'tests
                (list "tests/test-browser-errors.rkt"))))

;; ---- repository adapter (read-only; no execution) ------------------------------------

(define (repo-inputs root)
  (define projection (audit-projection root))
  ;; known-tests must cover the declared detectors (values of module -> tests)
  ;; AND the registry's own declared tests, otherwise every registry test would
  ;; look unknown.
  (define declared (hash-ref projection 'declared))
  (define tests
    (remove-duplicates
     (append (append* (for/list ([ts (in-hash-values declared)]) ts))
             (append* (for/list ([entry (in-list default-registry)])
                        (hash-ref entry 'tests '()))))))
  (define known-tests
    (for/hash ([test (in-list tests)])
      (values test (file-exists? (build-path root test)))))
  (hash 'declared (hash-ref projection 'declared)
        'dependents (hash-ref projection 'dependents)
        'requires (hash-ref projection 'requires)
        'known-tests known-tests))

;; Evaluate contract-aware escalation for a real changed-file set on this tree.
(define (evaluate-contract-changes root changed)
  (define inputs (repo-inputs root))
  (contract-decision changed
                     default-registry
                     (hash-ref inputs 'declared)
                     (hash-ref inputs 'dependents)
                     (hash-ref inputs 'requires)
                     (hash-ref inputs 'known-tests)))

(define (contract-decision->jsexpr result)
  (hash 'verdict
        (symbol->string (hash-ref result 'verdict))
        'required
        (hash-ref result 'required)
        'matched
        (hash-ref result 'matched)
        'unmatched-high-fan-in
        (hash-ref result 'unmatched-high-fan-in)
        'escalations
        (for/list ([e (in-list (hash-ref result 'escalations))])
          (hash 'code
                (symbol->string (hash-ref e 'code))
                'module
                (hash-ref e 'module)
                'detail
                (hash-ref e 'detail)
                'tests
                (hash-ref e 'tests)))))
