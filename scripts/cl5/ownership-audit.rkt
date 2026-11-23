#lang racket/base

;; scripts/cl5/ownership-audit.rkt — offline, read-only coverage-gap audit for
;; the `./qt quick` changed-file selector (CL5 W2 follow-up, #9982).
;;
;; WHY THIS EXISTS
;; The frozen D5/F-VS-1 case showed the shipped selector returning a false
;; green: `extensions/gsd/wave-status.rkt` changed, the selector ran three
;; files and passed, while comprehensive CI failed real GSD behavioural tests.
;; The detecting owner (`tests/test-gsd-wave-status.rkt`, `@covers
;; extensions/gsd/wave-status.rkt`) was never selectable. This audit finds that
;; class of gap across the repository — deterministically, from the repository's
;; own parsers, without running any tests and without touching selection policy.
;;
;; WHAT IT READS (no competing source of truth)
;;   * `tests/.coverage-manifest.json` via changed-selection.rkt's
;;     `load-coverage-manifest` (declared `@covers` attribution);
;;   * the committed audited-owner table via quick-audit.rkt's
;;     `current-admitted-owners` (the SELECTABLE universe of `./qt quick`);
;;   * the impact graph via changed-selection.rkt's `build-dependency-graph`
;;     (the same graph the selector walks for transitive dependents);
;;   * for drift: `generate-covers-manifest-entries`, the committed manifest's
;;     own generator, recomputed in memory.
;;
;; Classification (what the tool may and may not claim). Rows cover the WHOLE
;; production-module universe, not only modules that appear in the manifest.
;;   confirmed-gap  the module declares at least one covering test, and NONE of
;;                  those tests is in the audited owner table. There is **no
;;                  directly selectable declared detector**: a change to the
;;                  module selects nothing for it directly. A transitive
;;                  consumer test may still run; that is not detection of this
;;                  module's own behaviour.
;;   potential-gap  at least one covering test IS audited, but the module has
;;                  >= 3 production dependents AND no consumer-contract
;;                  escalation exists in the selector. Regression risk is real
;;                  but not proven by this data.
;;   covered        audited cover present, low fan-in.
;;   unverified     NO declared `@covers` at all. The tool cannot say whether a
;;                  detector exists, so it must not claim coverage. This is the
;;                  broadest risk category, and it is counted over the full
;;                  production universe (it is not structurally zero).
;;
;; The audit is a REPORT. It never selects, never executes, never writes, and is
;; not wired into `./qt`, CI or admission.

(require json
         racket/file
         racket/list
         racket/path
         racket/set
         racket/string
         "../testing/changed-selection.rkt"
         "../testing/quick-audit.rkt")

(provide audit-snapshot
         classify-module
         risk-rank
         risk-category
         production-files
         invert-covers
         manifest-drift
         summary-lines
         findings-lines
         audit->jsexpr
         run-audit)

;; ---- inputs -----------------------------------------------------------------------

;; The same production-file set the impact selector builds its graph from.
(define (production-files root)
  (sort (for/list ([f (in-directory root)]
                   #:when (and (file-exists? f)
                               (let ([rel (simple-form-path-relative root f)])
                                 (and (member (path-get-extension f) '(#".rkt" #".ss" #".rktl"))
                                      (not (string-prefix? rel "tests/"))
                                      (not (string-prefix? rel "scripts/"))
                                      (not (string-prefix? rel ".github/"))
                                      (not (string-prefix? rel "docs/"))
                                      (not (string-prefix? rel "artifacts/"))
                                      (not (string-contains? rel "/compiled/"))))))
          (simple-form-path-relative root f))
        string<?))

(define (simple-form-path-relative root f)
  (path->string (find-relative-path (simplify-path root) (simplify-path f))))

;; covers: test -> covered modules  =>  module -> declared tests
(define (invert-covers covers)
  (define inverted (make-hash))
  (for ([(test modules) (in-hash covers)])
    (for ([m (in-list modules)])
      (hash-update! inverted m (lambda (tests) (sort (cons test tests) string<?)) '())))
  (for/hash ([(m tests) (in-hash inverted)])
    (values m tests)))

;; ---- classification ---------------------------------------------------------------

;; Risk categories: the areas where a missed detector is a safety problem, not
;; a slowdown. Ordered — the first matching category wins.
(define risk-rules
  (list (cons 'authorization
              '("permission" "auth" "approval" "credential" "policy" "capability" "access"))
        (cons 'security '("sandbox" "quarantine" "checksum" "secret" "redact" "audit"))
        (cons 'persistence '("session-store" "store" "jsonl" "persist" "storage" "snapshot"))
        (cons 'recovery '("recover" "replay" "retry" "compactor" "resume" "restore"))
        (cons 'provider '("provider" "llm" "stream" "sse" "anthropic" "openai" "model"))
        (cons 'ordering '("event-bus" "event" "queue" "scheduler" "ordering" "serial"))
        (cons 'shared-state '("blackboard" "state" "registry" "shared" "cache"))))

(define (risk-category module)
  (define lower (string-downcase module))
  (for/or ([rule (in-list risk-rules)])
    (and (for/or ([needle (in-list (cdr rule))])
           (string-contains? lower needle))
         (car rule))))

;; 6 = safety-critical shared surface, 1 = none of the risk rules matched.
(define (risk-rank module)
  (case (risk-category module)
    [(authorization) 6]
    [(security) 6]
    [(persistence) 5]
    [(recovery) 5]
    [(provider) 4]
    [(ordering) 4]
    [(shared-state) 3]
    [else 1]))

;; classify-module : string (listof string) (listof string) exact-nonnegative-integer
;;   -> symbol
;; `declared` are the @covers tests for the module, `owners` the audited table.
(define (classify-module _module declared owners dependents-count)
  (define audited (filter (lambda (t) (and (member t owners) #t)) declared))
  (cond
    [(null? declared) 'unverified]
    [(null? audited) 'confirmed-gap]
    [(>= dependents-count 3) 'potential-gap]
    [else 'covered]))

(define (module-audit-row module declared owners dependents)
  (define audited (filter (lambda (t) (and (member t owners) #t)) declared))
  (hash 'module
        module
        'class
        (classify-module module declared owners (length dependents))
        'declared-tests
        declared
        'audited-tests
        audited
        'unadmitted-tests
        (filter-not (lambda (t) (and (member t owners) #t)) declared)
        'dependents
        dependents
        'dependents-count
        (length dependents)
        'risk
        (risk-category module)
        'risk-rank
        (risk-rank module)))

;; ---- manifest drift ---------------------------------------------------------------

;; Recomputed entries vs the committed manifest: a test whose declared `@covers`
;; set differs from the recorded one means selection works from stale metadata.
(define (manifest-drift root)
  (define committed
    (with-handlers ([exn:fail? (lambda (_) (hash))])
      (define j (with-input-from-file (build-path root "tests" ".coverage-manifest.json") read-json))
      (for/hash ([e (in-list (hash-ref j 'entries '()))])
        (values (hash-ref e 'test) (sort (hash-ref e 'covers '()) string<?)))))
  (define regenerated
    (for/hash ([e (in-list (generate-covers-manifest-entries root))])
      (values (hash-ref e 'test) (sort (hash-ref e 'covers '()) string<?))))
  (define missing
    (sort (for/list ([t (in-hash-keys regenerated)]
                     #:unless (hash-has-key? committed t))
            t)
          string<?))
  (define stale
    (sort (for/list ([t (in-hash-keys regenerated)]
                     #:when (and (hash-has-key? committed t)
                                 (not (equal? (hash-ref committed t) (hash-ref regenerated t)))))
            t)
          string<?))
  (define extra
    (sort (for/list ([t (in-hash-keys committed)]
                     #:unless (hash-has-key? regenerated t))
            t)
          string<?))
  (hash 'missing
        missing
        'stale
        stale
        'extra
        extra
        'committed-count
        (hash-count committed)
        'regenerated-count
        (hash-count regenerated)))

;; ---- snapshot ---------------------------------------------------------------------

;; How many rows per risk category (a prioritisation heuristic, not an audit).
(define (risk-counts rows)
  (for/fold ([acc (hash)]) ([row (in-list rows)])
    (hash-update acc (or (hash-ref row 'risk) 'none) add1 0)))

(define (audit-snapshot root)
  (define owners (append (current-admitted-owners) '()))
  (define-values (covers _sources status) (load-coverage-manifest root))
  (define inverted (invert-covers covers))
  (define prod (production-files root))
  (define-values (_graph rgraph parse-failures) (build-dependency-graph root prod))
  ;; Rows are the COMPLETE production universe: a module with no declared cover
  ;; must appear as 'unverified, otherwise `unverified = 0` would be a tautology
  ;; of the input rather than a finding (review #7088040257).
  (define rows
    (for/list ([module (in-list prod)])
      (module-audit-row module (hash-ref inverted module '()) owners (hash-ref rgraph module '()))))
  ;; Declared `@covers` targets outside the production universe are reported,
  ;; not silently dropped.
  (define declared-outside-production
    (sort (for/list ([m (in-hash-keys inverted)]
                     #:unless (member m prod))
            m)
          string<?))
  (define by-class
    (for/fold ([acc (hash)]) ([row (in-list rows)])
      (hash-update acc (hash-ref row 'class) (lambda (l) (cons row l)) '())))
  (define owners-without-covers
    (sort (for/list ([o (in-list owners)]
                     #:unless (hash-has-key? covers o))
            o)
          string<?))
  (hash 'root
        (path->string (simplify-path root))
        'owners
        owners
        'owner-count
        (length owners)
        'manifest-tests
        (hash-count covers)
        'manifest-status
        status
        'production-modules
        (length prod)
        'declared-covered-modules
        (length (filter (lambda (r) (pair? (hash-ref r 'declared-tests)))
                        rows))
        'declared-outside-production
        declared-outside-production
        'classes
        (for/hash ([(k v) (in-hash by-class)])
          (values k (length v)))
        'confirmed-gaps
        (sort (hash-ref by-class 'confirmed-gap '())
              (lambda (a b)
                (or (> (hash-ref a 'risk-rank) (hash-ref b 'risk-rank))
                    (and (= (hash-ref a 'risk-rank) (hash-ref b 'risk-rank))
                         (string<? (hash-ref a 'module) (hash-ref b 'module))))))
        'potential-gaps
        (sort (hash-ref by-class 'potential-gap '())
              (lambda (a b)
                (or (> (hash-ref a 'dependents-count) (hash-ref b 'dependents-count))
                    (and (= (hash-ref a 'dependents-count) (hash-ref b 'dependents-count))
                         (string<? (hash-ref a 'module) (hash-ref b 'module))))))
        'unverified
        (sort (hash-ref by-class 'unverified '())
              (lambda (a b) (string<? (hash-ref a 'module) (hash-ref b 'module))))
        'owners-without-covers
        owners-without-covers
        'graph-parse-failures
        parse-failures
        'drift
        (manifest-drift root)
        'rows
        rows))

;; ---- presentation -----------------------------------------------------------------

(define (rows->jsexpr rows events)
  (for/list ([row (in-list rows)])
    (for/hash ([(k v) (in-hash row)])
      (values k
              (cond
                [(and (eq? k 'dependents) (list? v) (>= (length v) 40))
                 (append (take v 40) (list (format "... ~a more" (- (length v) 40))))]
                [events (->jsexpr-simple v)]
                [else (->jsexpr-simple v)])))))

(define (->jsexpr-simple v)
  (cond
    [(hash? v)
     (for/hash ([(k x) (in-hash v)])
       (values (if (symbol? k) k (string->symbol (format "~a" k)))
               (->jsexpr-simple x)))]
    [(symbol? v) (symbol->string v)]
    [(list? v) (map ->jsexpr-simple v)]
    [else v]))

(define (audit->jsexpr snapshot)
  (hash 'tool
        "scripts/cl5/ownership-audit.rkt"
        'kind
        "offline read-only coverage-gap audit (CL5 W2 follow-up #9982)"
        'root
        (hash-ref snapshot 'root)
        'owner-count
        (hash-ref snapshot 'owner-count)
        'manifest-tests
        (hash-ref snapshot 'manifest-tests)
        'manifest-status
        (symbol->string (hash-ref snapshot 'manifest-status))
        'production-modules
        (hash-ref snapshot 'production-modules)
        'declared-covered-modules
        (hash-ref snapshot 'declared-covered-modules)
        'declared-outside-production
        (hash-ref snapshot 'declared-outside-production)
        'classes
        (for/hash ([(k v) (in-hash (hash-ref snapshot 'classes))]) (values k v))
        'confirmed-gaps
        (rows->jsexpr (hash-ref snapshot 'confirmed-gaps) #f)
        'unverified-by-risk
        (for/hash ([(k v) (in-hash (risk-counts (hash-ref snapshot 'unverified '())))])
          (values k v))
        'potential-gaps
        (rows->jsexpr (hash-ref snapshot 'potential-gaps) #f)
        'unverified-count
        (length (hash-ref snapshot 'unverified '()))
        'unverified
        (rows->jsexpr (take (sort (hash-ref snapshot 'unverified '())
                                  (lambda (a b)
                                    (or (> (hash-ref a 'risk-rank) (hash-ref b 'risk-rank))
                                        (and (= (hash-ref a 'risk-rank) (hash-ref b 'risk-rank))
                                             (string<? (hash-ref a 'module)
                                                       (hash-ref b 'module))))))
                              (min 50 (length (hash-ref snapshot 'unverified '()))))
                        #f)
        'owners-without-covers
        (hash-ref snapshot 'owners-without-covers)
        'graph-parse-failures
        (hash-ref snapshot 'graph-parse-failures)
        'manifest-drift
        (->jsexpr-simple (hash-ref snapshot 'drift))))

(define (summary-lines snapshot)
  (define classes (hash-ref snapshot 'classes))
  (append (list (format "owners (selectable universe): ~a" (hash-ref snapshot 'owner-count))
                (format "declared @covers tests: ~a (manifest ~a); production modules ~a, of which ~a declared-covered"
                        (hash-ref snapshot 'manifest-tests)
                        (symbol->string (hash-ref snapshot 'manifest-status))
                        (hash-ref snapshot 'production-modules)
                        (hash-ref snapshot 'declared-covered-modules))
                (format "declared @covers targets outside the production universe: ~a"
                        (length (hash-ref snapshot 'declared-outside-production)))
                (format "confirmed gaps: ~a | potential gaps: ~a | covered: ~a | unverified: ~a"
                        (hash-ref classes 'confirmed-gap 0)
                        (hash-ref classes 'potential-gap 0)
                        (hash-ref classes 'covered 0)
                        (hash-ref classes 'unverified 0))
                (format "audited owners with no @covers (always included fail-open): ~a"
                        (length (hash-ref snapshot 'owners-without-covers)))
                (format "graph parse failures: ~a | manifest drift: missing ~a, stale ~a, extra ~a"
                        (hash-count (hash-ref snapshot 'graph-parse-failures))
                        (length (hash-ref (hash-ref snapshot 'drift) 'missing))
                        (length (hash-ref (hash-ref snapshot 'drift) 'stale))
                        (length (hash-ref (hash-ref snapshot 'drift) 'extra))))))

(define (findings-lines snapshot)
  (apply append
         (list (list "confirmed gaps (declared cover, no audited owner -> quick selects nothing):")
               (for/list ([row (in-list (hash-ref snapshot 'confirmed-gaps))])
                 (format "  P~a ~a  ~a  (covering tests: ~a)"
                         (hash-ref row 'risk-rank)
                         (or (hash-ref row 'risk) 'any)
                         (hash-ref row 'module)
                         (string-join (hash-ref row 'unadmitted-tests) ", ")))
               (list "potential gaps (audited cover, high fan-in, no consumer escalation):")
               (for/list ([row (in-list (take (hash-ref snapshot 'potential-gaps)
                                              (min 25
                                                   (length (hash-ref snapshot 'potential-gaps)))))])
                 (format "  P~a ~a  ~a  (~a dependents; audited: ~a)"
                         (hash-ref row 'risk-rank)
                         (or (hash-ref row 'risk) 'any)
                         (hash-ref row 'module)
                         (hash-ref row 'dependents-count)
                         (string-join (hash-ref row 'audited-tests) ", ")))
               (list "unverified (no declared detector at all - broadest risk category, top 25 by risk):")
    (for/list ([row (in-list (take (sort (hash-ref snapshot 'unverified '())
                                        (lambda (a b)
                                          (or (> (hash-ref a 'risk-rank) (hash-ref b 'risk-rank))
                                              (and (= (hash-ref a 'risk-rank) (hash-ref b 'risk-rank))
                                                   (string<? (hash-ref a 'module)
                                                             (hash-ref b 'module))))))
                                   (min 25 (length (hash-ref snapshot 'unverified '())))))])
      (format "  P~a ~a  ~a" (hash-ref row 'risk-rank) (or (hash-ref row 'risk) 'any)
              (hash-ref row 'module)))
    (list "audited owners with no @covers (included on every change):")
               (for/list ([o (in-list (hash-ref snapshot 'owners-without-covers))])
                 (format "  ~a" o))
               (list "manifest drift (regenerated entries differing from the committed manifest):")
               (for/list ([t (in-list (append (hash-ref (hash-ref snapshot 'drift) 'missing)
                                              (hash-ref (hash-ref snapshot 'drift) 'stale)))])
                 (format "  ~a" t))
               (list "graph parse failures (dependency metadata unusable for these modules):")
               (for/list ([m (in-list (sort (hash-keys (hash-ref snapshot 'graph-parse-failures))
                                            string<?))])
                 (format "  ~a" m)))))

(define (run-audit root #:json-out [json-out #f])
  (define snapshot (audit-snapshot root))
  (for ([line (in-list (summary-lines snapshot))])
    (displayln line))
  (newline)
  (for ([line (in-list (findings-lines snapshot))])
    (displayln line))
  (when json-out
    (call-with-output-file json-out
                           (lambda (out)
                             (write-json (audit->jsexpr snapshot) out)
                             (newline out))
                           #:exists 'truncate/replace)
    (displayln (format "\nwrote ~a" json-out)))
  snapshot)
