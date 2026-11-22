#lang racket/base

;; scripts/cl5/selection-guard.rkt — ISOLATED, offline prototype of a
;; fail-closed selection guard (CL5 W2 remediation prototype, #9982).
;;
;; This module is NOT wired into `./qt`, CI, admission or any workflow. It is a
;; pure decision function plus a data adapter, used by the W2 follow-up tests to
;; demonstrate — deterministically, without GitHub Actions — how a conservative
;; selector would treat the frozen D5 corpus case and its clean control.
;;
;; The guard answers one question: given a changed-file set and the repository's
;; DECLARED coverage (@covers) and ADMITTED ownership (audited quick owners), can
;; the selector honestly claim a meaningful green?
;;
;; It escalates (refuses to claim) when:
;;   unadmitted-cover  a changed module declares covering tests but NONE of them
;;                     is selectable — `./qt quick` could run nothing for it.
;;                     The unadmitted tests are named in the evidence.
;;   uncovered-module  a changed module has no declared cover at all: nothing is
;;                     known about who detects it.
;;   zero-selection    the computed selected set is empty (no zero-test green).
;;   admission-stale   declared coverage exists but the audited owner table is
;;                     empty/unreadable, so no ownership evidence can be trusted.
;;
;; Only when every changed module has at least one selectable detector does the
;; guard return 'selected.

(require racket/list
         racket/path
         racket/string
         "../testing/changed-selection.rkt"
         "../testing/quick-audit.rkt")

(provide guard-decision
         guard-decide-repo
         guard->jsexpr
         audit-projection)

(define (escalation code module detail tests)
  (hasheq 'code code 'module module 'detail detail 'tests tests))

(define (audited-member? rel owners)
  (and (member rel owners) #t))

;; guard-decision : (listof string) changed
;;                  (hash module -> (listof test)) declared
;;                  (listof string) owners
;;                  (hash module -> (listof module)) dependents
;;   -> hash 'verdict 'selected 'escalations 'lane 'changed
(define (guard-decision changed declared owners dependents)
  (define (declared-of m)
    (hash-ref declared m '()))
  (define (audited-tests-of m)
    (filter (lambda (t) (audited-member? t owners)) (declared-of m)))
  (define unadmitted
    (for/list ([m (in-list changed)]
               #:when (let ([d (declared-of m)]) (and (pair? d) (null? (audited-tests-of m)))))
      (escalation 'unadmitted-cover
                  m
                  "declared detectors exist but none is in the audited owner table"
                  (declared-of m))))
  (define uncovered
    (for/list ([m (in-list changed)]
               #:when (null? (declared-of m)))
      (escalation 'uncovered-module m "no @covers attribution reaches this module" '())))
  (define admission-stale
    (if (and (positive? (hash-count declared)) (null? owners))
        (list (escalation 'admission-stale
                          #f
                          "audited owner table is empty; ownership evidence cannot be verified"
                          '()))
        '()))
  (define required
    (sort (remove-duplicates
           (append* (for/list ([m (in-list changed)])
                      (append (audited-tests-of m)
                              (append* (for/list ([d (in-list (hash-ref dependents m '()))])
                                         (audited-tests-of d)))))))
          string<?))
  (define zero
    (if (null? required)
        (list (escalation 'zero-selection
                          #f
                          "selection is empty; a zero-test green must never be reported"
                          '()))
        '()))
  (define escalations
    (if (pair? (append admission-stale unadmitted uncovered))
        (append admission-stale unadmitted uncovered)
        (append admission-stale zero)))
  (hasheq 'verdict
          (if (pair? escalations) 'escalate 'selected)
          'selected
          required
          'changed
          (sort (remove-duplicates changed) string<?)
          'escalations
          escalations
          'lane
          (if (pair? escalations) 'comprehensive 'quick)))

;; ---- repository adapter (read-only) --------------------------------------------------

(define (repo-relative-files root)
  (sort (for/list ([f (in-directory root)]
                   #:when (and (file-exists? f)
                               (let ([rel (path->string (find-relative-path (simplify-path root)
                                                                            (simplify-path f)))])
                                 (and (member (path-get-extension f) (list #".rkt" #".ss" #".rktl"))
                                      (not (string-prefix? rel "tests/"))
                                      (not (string-prefix? rel "scripts/"))
                                      (not (string-prefix? rel "docs/"))
                                      (not (string-prefix? rel "artifacts/"))
                                      (not (string-contains? rel "/compiled/"))))))
          (path->string (find-relative-path (simplify-path root) (simplify-path f))))
        string<?))

(define (audit-projection root)
  (define owners (append (current-admitted-owners) '()))
  (define-values (covers _sources status) (load-coverage-manifest root))
  (define declared (make-hash))
  (for ([(test modules) (in-hash covers)])
    (for ([m (in-list modules)])
      (hash-update! declared m (lambda (l) (sort (cons test l) string<?)) '())))
  (define-values (_g rgraph _f) (build-dependency-graph root (repo-relative-files root)))
  (hash 'owners owners 'manifest-status status 'declared declared 'dependents rgraph))

;; D5-shaped decision for a real changed-file set, from repository data only.
(define (guard-decide-repo root changed)
  (define projection (audit-projection root))
  (hash 'decision
        (guard-decision changed
                        (hash-ref projection 'declared)
                        (hash-ref projection 'owners)
                        (hash-ref projection 'dependents))
        'owners
        (length (hash-ref projection 'owners))
        'manifest-status
        (hash-ref projection 'manifest-status)
        'changed
        changed))

(define (guard->jsexpr result)
  (define d (hash-ref result 'decision))
  (hash 'verdict
        (symbol->string (hash-ref d 'verdict))
        'lane
        (symbol->string (hash-ref d 'lane))
        'changed
        (hash-ref d 'changed)
        'selected
        (hash-ref d 'selected)
        'escalations
        (for/list ([e (in-list (hash-ref d 'escalations))])
          (hash 'code
                (symbol->string (hash-ref e 'code))
                'module
                (or (hash-ref e 'module) "")
                'detail
                (hash-ref e 'detail)
                'tests
                (hash-ref e 'tests)))
        'owners
        (hash-ref result 'owners)))
