#lang racket/base

;; scripts/cl5/selection-guard.rkt — ISOLATED, offline prototype of a
;; fail-closed selection guard (CL5 W2 remediation prototype, #9982).
;;
;; NOT wired into `./qt`, CI, admission or any workflow. Pure decision + a
;; read-only adapter, used by the W2 follow-up tests.
;;
;; FAIL-CLOSED CONTRACT (review #7088040257): 'selected is returned only when
;;   * the coverage manifest loaded (`'loaded`, not missing/corrupt);
;;   * the dependency graph parsed every module in the changed modules' requires
;;     cone (a parse failure makes the edge set unknown);
;;   * every changed module has at least one declared detector that is BOTH in
;;     the audited owner table AND freshly admitted (record current, scan clean);
;;   * the shipped selector COMPLETED (a failure or exception is a named
;;     `selection-unavailable` escalation, never a skipped check);
;;   * the shipped selector did NOT escalate (a conservative fallback is
;;     preserved, never downgraded to a quick verdict);
;;   * the shipped selector actually selected those detectors (attribution that
;;     never reaches the selected set is not evidence);
;;   * the resulting selection is non-empty.
;; Anything unknown, stale, missing, corrupt or unparseable escalates with a
;; machine-readable code and names the evidence that was missing.
;;
;; Codes: manifest-unavailable, graph-parse-failure, unadmitted-cover,
;; uncovered-module, selection-unavailable, shipped-escalation-preserved,
;; unselected-declared-cover, zero-selection.
;;
;; This proves attribution + executability + admission freshness at most. It does
;; NOT prove that a detector detects the mutation, and a shared-contract edge can
;; still need conservative escalation even when one direct test is selectable.

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

(define (fresh? admission owner)
  (eq? (hash-ref admission owner 'missing) 'admitted))

;; guard-decision : (listof string) changed, hash evidence -> hash
;;   evidence keys:
;;     declared              hash module -> (listof test)
;;     owners                (listof string) audited owner table
;;     admission             hash owner -> 'admitted/'review-required/'rejected/'missing
;;     dependents            hash module -> (listof module)
;;     requires              hash module -> (listof module)
;;     manifest-status       'loaded/'missing/'corrupt
;;     graph-parse-failures  (listof string)
;;     shipped-selected      (listof string) or #f when not measured
(define (guard-decision changed evidence)
  (define declared (hash-ref evidence 'declared (hash)))
  (define owners (hash-ref evidence 'owners '()))
  (define admission (hash-ref evidence 'admission (hash)))
  (define dependents (hash-ref evidence 'dependents (hash)))
  (define requires (hash-ref evidence 'requires (hash)))
  (define manifest-status (hash-ref evidence 'manifest-status 'missing))
  (define parse-failures (hash-ref evidence 'graph-parse-failures '()))
  (define shipped-selected (hash-ref evidence 'shipped-selected #f))
  ;; Absent evidence is NOT "nothing to check": the shipped selector must have
  ;; completed. 'ok must be supplied explicitly.
  (define shipped-status (hash-ref evidence 'shipped-status 'failed))
  (define shipped-escalations (hash-ref evidence 'shipped-escalations '()))
  (define shipped-error (hash-ref evidence 'shipped-error #f))

  (define (declared-of m)
    (hash-ref declared m '()))
  (define (selectable-detectors m)
    (filter (lambda (t) (and (member t owners) (fresh? admission t))) (declared-of m)))

  ;; Any module in a changed module's requires cone (itself included) that the
  ;; graph could not parse makes the selection untrustworthy.
  (define (cone m)
    (let loop ([frontier (list m)]
               [seen '()])
      (cond
        [(null? frontier) seen]
        [else
         (define f (car frontier))
         (if (member f seen)
             (loop (cdr frontier) seen)
             (loop (append (cdr frontier) (hash-ref requires f '())) (cons f seen)))])))
  (define unparseable
    (sort (remove-duplicates (for*/list ([m (in-list changed)]
                                         [c (in-list (cone m))]
                                         #:when (member c parse-failures))
                                c))
          string<?))

  (define manifest-bad
    (if (eq? manifest-status 'loaded)
        '()
        (list (escalation 'manifest-unavailable
                          #f
                          (format "coverage manifest status is ~a" manifest-status)
                          '()))))
  (define graph-bad
    (if (null? unparseable)
        '()
        (list (escalation 'graph-parse-failure
                          #f
                          "dependency graph could not parse a module in the changed cone"
                          unparseable))))
  (define uncovered
    (for/list ([m (in-list changed)]
               #:when (null? (declared-of m)))
      (escalation 'uncovered-module m "no @covers attribution reaches this module" '())))
  ;; A declared detector that is audited but not fresh is stale evidence; one
  ;; that is not audited is unadmitted. Both block a green.
  (define unadmitted
    (for/list ([m (in-list changed)]
               #:when (and (pair? (declared-of m)) (null? (selectable-detectors m))))
      (escalation 'unadmitted-cover
                  m
                  "declared detectors exist but none is both audited and freshly admitted"
                  (for/list ([t (in-list (declared-of m))])
                    (hash 'test
                          t
                          'admission
                          (hash-ref admission t 'missing)
                          'audited
                          (and (member t owners) #t))))))
  ;; A missing/failed shipped computation can never yield a guard green: the
  ;; selection check would otherwise be silently skipped (review #7088615109).
  (define shipped-bad
    (cond
      [(not (eq? shipped-status 'ok))
       (list (escalation 'selection-unavailable
                         #f
                         (format "shipped selection unavailable: ~a"
                                 (or shipped-error shipped-status))
                         '()))]
      [(pair? shipped-escalations)
       ;; Never downgrade a conservative fallback to a quick verdict.
       (list (escalation 'shipped-escalation-preserved
                         #f
                         "the shipped selector escalated; the guard preserves that decision"
                         (map (lambda (e)
                                (if (hash? e) (hash-ref e 'code e) e))
                              shipped-escalations)))]
      [else '()]))
  ;; Attribution that the shipped selector did not actually run is not evidence.
  (define unshipped
    (if shipped-selected
        (for/list ([m (in-list changed)]
                   #:when (let ([fresh-detectors (selectable-detectors m)])
                            (and (pair? fresh-detectors)
                                 (for/or ([t (in-list fresh-detectors)])
                                   (not (member t shipped-selected))))))
          (escalation 'unselected-declared-cover
                      m
                      "a fresh audited detector exists but the shipped selection did not include it"
                      (selectable-detectors m)))
        '()))
  ;; The shipped rule walks transitive dependents, so the guard must too.
  (define (dependents-closure m)
    (let loop ([frontier (list m)] [seen '()])
      (cond
        [(null? frontier) seen]
        [else
         (define f (car frontier))
         (define next
           (filter (lambda (d) (and (not (equal? d m)) (not (member d seen))))
                   (hash-ref dependents f '())))
         (loop (append (cdr frontier) next) (append next seen))])))
  (define required
    (sort (remove-duplicates
           (append* (for/list ([m (in-list changed)])
                      (append (selectable-detectors m)
                              (append* (for/list ([d (in-list (dependents-closure m))])
                                         (selectable-detectors d)))))))
          string<?))
  (define zero
    (if (null? required)
        (list (escalation 'zero-selection
                          #f
                          "selection is empty; a zero-test green must never be reported"
                          '()))
        '()))
  (define escalations
    (append manifest-bad graph-bad shipped-bad uncovered unadmitted unshipped zero))
  (hasheq 'verdict
          (if (pair? escalations) 'escalate 'selected)
          'selected
          required
          'changed
          (sort (remove-duplicates changed) string<?)
          'escalations
          escalations
          'evidence
          (hasheq 'manifest-status
                  manifest-status
                  'owners
                  (length owners)
                  'admission-checked
                  (hash-count admission)
                  'graph-parse-failures
                  (length parse-failures)
                  'shipped-selected
                  (and shipped-selected (length shipped-selected))
                  'shipped-status
                  shipped-status
                  'shipped-escalations
                  (length shipped-escalations))
          'lane
          (if (pair? escalations) 'comprehensive 'quick)
          'claims
          'attribution-and-admission-only))

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
  (define admission
    (for/hash ([o (in-list owners)])
      (values o (admission-state-status (owner-admission-state root o)))))
  (define prod (repo-relative-files root))
  (define-values (graph rgraph parse-failures) (build-dependency-graph root prod))
  (hash 'owners
        owners
        'admission
        admission
        'manifest-status
        status
        'covers
        covers
        'declared
        declared
        'dependents
        rgraph
        'requires
        graph
        'graph-parse-failures
        (sort (hash-keys parse-failures) string<?)))

(define (guard-decide-repo root changed)
  (define projection (audit-projection root))
  ;; Keep BOTH the selected set and the shipped selector's own escalations, and
  ;; keep the failure diagnostic instead of discarding it.
  (define computed
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (hash 'status 'failed 'error (exn-message e) 'selected #f 'escalations '()))])
      (let ([sel (compute-impact-selection root
                                           changed
                                           (hash-ref projection 'covers)
                                           (hash)
                                           (hash-ref projection 'owners))])
        (hash 'status
              'ok
              'error
              #f
              'selected
              (selection-selected sel)
              'escalations
              (selection-escalations sel)))))
  (define evidence
    (hash 'declared
          (hash-ref projection 'declared)
          'owners
          (hash-ref projection 'owners)
          'admission
          (hash-ref projection 'admission)
          'dependents
          (hash-ref projection 'dependents)
          'requires
          (hash-ref projection 'requires)
          'manifest-status
          (hash-ref projection 'manifest-status)
          'graph-parse-failures
          (hash-ref projection 'graph-parse-failures)
          'shipped-status
          (hash-ref computed 'status)
          'shipped-error
          (hash-ref computed 'error)
          'shipped-escalations
          (hash-ref computed 'escalations)
          'shipped-selected
          (hash-ref computed 'selected)))
  (hash 'decision
        (guard-decision changed evidence)
        'owners
        (length (hash-ref projection 'owners))
        'manifest-status
        (hash-ref projection 'manifest-status)
        'admission
        (hash-ref projection 'admission)
        'changed
        changed))

(define (guard->jsexpr result)
  (define d (hash-ref result 'decision))
  (hash 'verdict
        (symbol->string (hash-ref d 'verdict))
        'lane
        (symbol->string (hash-ref d 'lane))
        'claims
        (symbol->string (hash-ref d 'claims))
        'changed
        (hash-ref d 'changed)
        'selected
        (hash-ref d 'selected)
        'evidence
        (for/hash ([(k v) (in-hash (hash-ref d 'evidence))])
          (values k v))
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
