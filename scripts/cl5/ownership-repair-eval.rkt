#lang racket/base

;; scripts/cl5/ownership-repair-eval.rkt — offline eligibility evaluation for
;; the risk-prioritized ownership repair (issue #9995, CL5 W2 follow-up).
;;
;; READ-ONLY. This tool does not admit owners, does not regenerate the admission
;; record, and is not wired into `./qt`, CI or admission. It answers one
;; question per confirmed gap from `ownership-audit.rkt`: is the declared
;; detector eligible for the existing pure-unit admission contract, or does it
;; need manual review?
;;
;; Eligibility is the contract the owner table already applies, not a new one:
;;   * `@boundary unit` and `@speed fast` metadata;
;;   * the reviewed closure scans clean against the versioned capability
;;     denylist (no filesystem/process/network/eval in file or closure);
;;   * a small closure (the reviewed surface stays reviewable).
;; Anything else is `needs-review` (capability hit, integration boundary, slow
;; tier, oversized closure) or `ineligible` (not usable as a quick owner).
;;
;; A `'eligible` verdict is an ADMISSION PREREQUISITE, not proof that the test
;; detects the module's behaviour: the reviewer must still confirm the test
;; exercises the decision surface named by the gap.

(require json
         racket/file
         racket/list
         racket/path
         (only-in "../run-tests/classify.rkt" base-dir get-file-metadata)
         "../testing/quick-audit.rkt"
         "ownership-audit.rkt")

(provide classify-candidate
         evaluate-candidates
         candidate->jsexpr
         run-evaluation
         max-reviewed-closure)

;; Keep the reviewed closure reviewable; a larger closure is a review burden,
;; not an automatic rejection.
(define max-reviewed-closure 12)

;; classify-candidate : hash-or-#f (listof any) exact-nonnegative-integer symbol -> symbol
;; Metadata values are mixed: `@boundary` parses to a string, `@speed` to a
;; symbol. Compare through a string view so both shapes are handled.
(define (meta-text meta key)
  (define v (hash-ref meta key #f))
  (and v (format "~a" v)))

(define (classify-candidate meta scan-hits closure-size admission)
  (cond
    [(not meta) 'ineligible]
    [(eq? admission 'admitted) 'already-admitted]
    [(not (equal? (meta-text meta 'boundary) "unit")) 'needs-review]
    [(not (equal? (meta-text meta 'speed) "fast")) 'needs-review]
    [(pair? scan-hits) 'needs-review]
    [(> closure-size max-reviewed-closure) 'needs-review]
    [else 'eligible]))

;; get-file-metadata resolves relative paths against the runner's own repo
;; root, so pass an ABSOLUTE path for an arbitrary audit root.
(define (safe-metadata root rel)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define meta (get-file-metadata (path->complete-path (build-path root rel))))
    (and (hash? meta) (positive? (hash-count meta)) meta)))

(define (safe-closure root rel)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (closure-of root rel)))

(define (safe-scan root closure)
  (for/list ([f (in-list closure)]
             #:do [(define-values (flagged? capability _line)
                     (with-handlers ([exn:fail? (lambda (_) (values #t 'unreadable 0))])
                       (scan-file (build-path root f))))]
             #:when flagged?)
    (hasheq 'file f 'capability capability)))

;; One candidate test file for one confirmed gap.
(define (evaluate-candidate root rel)
  (define closure (safe-closure root rel))
  (define scan (safe-scan root closure))
  (define admission
    (with-handlers ([exn:fail? (lambda (_) 'unknown)])
      (admission-state-status (owner-admission-state root rel))))
  (define meta (safe-metadata root rel))
  (hash 'test
        rel
        'verdict
        (classify-candidate meta scan (length closure) admission)
        'admission
        admission
        'boundary
        (and meta (meta-text meta 'boundary))
        'speed
        (and meta (meta-text meta 'speed))
        'closure-size
        (length closure)
        'scan-hits
        scan))

(define (module-verdict candidates)
  (define verdicts (map (lambda (c) (hash-ref c 'verdict)) candidates))
  (cond
    [(member 'eligible verdicts) 'eligible]
    [(member 'needs-review verdicts) 'needs-review]
    [(member 'already-admitted verdicts) 'already-admitted]
    [else 'ineligible]))

;; All confirmed gaps at or above `min-risk`, each with its declared detectors
;; evaluated. Deterministic order: risk desc, then module path.
(define (evaluate-candidates root #:min-risk [min-risk 5])
  (define snapshot (audit-snapshot root))
  (define rows
    (filter (lambda (row) (>= (hash-ref row 'risk-rank) min-risk))
            (hash-ref snapshot 'confirmed-gaps)))
  (for/list ([row (in-list rows)])
    (define candidates
      (for/list ([t (in-list (hash-ref row 'unadmitted-tests))])
        (evaluate-candidate root t)))
    (hash 'module
          (hash-ref row 'module)
          'risk
          (hash-ref row 'risk)
          'risk-rank
          (hash-ref row 'risk-rank)
          'dependents
          (hash-ref row 'dependents-count)
          'declared-tests
          (hash-ref row 'declared-tests)
          'candidates
          candidates
          'verdict
          (module-verdict candidates))))

(define (candidate->jsexpr candidate)
  (hash 'test
        (hash-ref candidate 'test)
        'verdict
        (symbol->string (hash-ref candidate 'verdict))
        'admission
        (symbol->string (hash-ref candidate 'admission))
        'boundary
        (or (hash-ref candidate 'boundary) "")
        'speed
        (or (hash-ref candidate 'speed) "")
        'closure-size
        (hash-ref candidate 'closure-size)
        'scan-hits
        (for/list ([h (in-list (hash-ref candidate 'scan-hits))])
          (hash 'file (hash-ref h 'file) 'capability (hash-ref h 'capability)))))

(define (run-evaluation root #:min-risk [min-risk 5] #:json-out [json-out #f])
  (define rows (evaluate-candidates root #:min-risk min-risk))
  (define summary
    (for/fold ([acc (hash)]) ([row (in-list rows)])
      (hash-update acc (hash-ref row 'verdict) add1 0)))
  (printf "confirmed gaps at risk>=~a: ~a\n" min-risk (length rows))
  (printf "module verdicts: ~a\n"
          (for/list ([(k v) (in-hash summary)])
            (format "~a=~a" k v)))
  (newline)
  (for ([row (in-list rows)])
    (printf "P~a ~a  ~a  => ~a\n"
            (hash-ref row 'risk-rank)
            (or (hash-ref row 'risk) 'any)
            (hash-ref row 'module)
            (hash-ref row 'verdict))
    (for ([c (in-list (hash-ref row 'candidates))])
      (printf "    ~a: ~a (boundary=~a speed=~a closure=~a scan-hits=~a)\n"
              (hash-ref c 'test)
              (hash-ref c 'verdict)
              (or (hash-ref c 'boundary) "-")
              (or (hash-ref c 'speed) "-")
              (hash-ref c 'closure-size)
              (length (hash-ref c 'scan-hits)))))
  (when json-out
    (call-with-output-file json-out
                           (lambda (out)
                             (write-json (hash 'min-risk
                                               min-risk
                                               'gaps
                                               (for/list ([row (in-list rows)])
                                                 (hash 'module
                                                       (hash-ref row 'module)
                                                       'risk
                                                       (or (hash-ref row 'risk) 'none)
                                                       'risk-rank
                                                       (hash-ref row 'risk-rank)
                                                       'dependents
                                                       (hash-ref row 'dependents)
                                                       'verdict
                                                       (symbol->string (hash-ref row 'verdict))
                                                       'candidates
                                                       (map candidate->jsexpr
                                                            (hash-ref row 'candidates)))))
                                         out)
                             (newline out))
                           #:exists 'truncate/replace)
    (printf "\nwrote ~a\n" json-out))
  rows)
