#lang racket

;; @suite default
;; @speed fast
;; @boundary unit

;; Tests for change-impact selection and deterministic prioritization
;; (scripts/run-tests/impact.rkt) — TDD plan W4 (selection core, escape
;; hatches, @covers manifest) and W6 (prioritization, failure history).
;;
;; All cases run against a synthetic fixture repo tree in a temp directory:
;; the selection core is pure and root-parameterized, so no real repository
;; state is touched and no git invocation is needed.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         json
         (file "../scripts/run-tests/impact.rkt"))

;; ------------------------------------------------------------------
;; Fixture repo tree
;; ------------------------------------------------------------------

(define (write-file! root rel content)
  (define p (build-path root rel))
  (make-directory* (path-only p))
  (with-output-to-file p (lambda () (display content)))
  p)

(define (make-fixture-tree!)
  (define root (make-temporary-file "impact-fx-~a" 'directory))
  ;; production modules with a real require chain: app/top → core/mid → core/base
  (write-file! root "core/base.rkt" "#lang racket/base\n(provide x)\n(define x 1)\n")
  (write-file! root "core/mid.rkt"  "#lang racket/base\n(require \"base.rkt\")\n(provide y)\n(define y 2)\n")
  (write-file! root "app/top.rkt"   "#lang racket/base\n(require \"../core/mid.rkt\")\n(provide z)\n(define z 3)\n")
  ;; escape-hatch sources
  (write-file! root "dyn/dyn.rkt"   "#lang racket/base\n(define (g m) (dynamic-require m 'go))\n(provide g)\n")
  (write-file! root "mac/m.rkt"     "#lang racket/base\n(define-syntax-rule (sw a b) (swap a b))\n(provide sw)\n")
  (write-file! root "gen/generated/g.rkt" "#lang racket/base\n(provide gg)\n(define gg 'generated)\n")
  (write-file! root "broken/broken.rkt" "#lang racket/base\n(require \"../core/base.rkt\"\n") ; unbalanced → parse error
  (write-file! root "orphan.rkt"    "#lang racket/base\n(provide o)\n(define o 'orphan)\n")
  ;; non-production categories
  (write-file! root "scripts/run-tests/runner.rkt" "#lang racket/base\n;; stub\n")
  (write-file! root ".github/workflows/ci.yml" "name: ci\n")
  (write-file! root "tests/fixtures/data.json" "{\"k\": 1}\n")
  (write-file! root "README.md" "# fixture\n")
  ;; tests; headers carry @covers for manifest round-trip
  (write-file! root "tests/t-base.rkt"
               "#lang racket/base\n;; @covers core/base.rkt\n;; @suite default\n")
  (write-file! root "tests/t-mid.rkt"
               "#lang racket/base\n;; @covers core/mid.rkt\n;; @suite default\n")
  (write-file! root "tests/t-top.rkt"
               "#lang racket/base\n;; @covers app/top.rkt\n;; @suite default\n")
  (write-file! root "tests/t-dyn.rkt"
               "#lang racket/base\n;; @covers dyn/dyn.rkt\n;; @suite default\n")
  (write-file! root "tests/t-broken.rkt"
               "#lang racket/base\n;; @covers broken/broken.rkt\n;; @suite default\n")
  (write-file! root "tests/t-other.rkt" "#lang racket/base\n;; @suite default\n")
  root)

;; Selection inputs mirroring the fixture @covers mappings.
(define (fixture-covers)
  (hash "tests/t-base.rkt"   '("core/base.rkt")
        "tests/t-mid.rkt"    '("core/mid.rkt")
        "tests/t-top.rkt"    '("app/top.rkt")
        "tests/t-dyn.rkt"    '("dyn/dyn.rkt")
        "tests/t-broken.rkt" '("broken/broken.rkt")))

(define (fixture-sources)
  (for/hash ([(t _) (in-hash (fixture-covers))])
    (values t "@covers manifest")))

(define (fixture-universe)
  '("tests/t-base.rkt" "tests/t-mid.rkt" "tests/t-top.rkt"
    "tests/t-dyn.rkt" "tests/t-broken.rkt" "tests/t-other.rkt"))

(define (select root changed)
  (compute-impact-selection root changed
                            (fixture-covers) (fixture-sources)
                            (fixture-universe)))

(define (escalation-codes sel)
  (map (lambda (e) (hash-ref e 'code)) (selection-escalations sel)))

(define (entry-for sel file)
  (findf (lambda (e) (equal? (hash-ref e 'file) file)) (hash-ref sel 'selected)))

;; ------------------------------------------------------------------
;; Suites
;; ------------------------------------------------------------------

(define selection-suite
  (test-suite
   "impact selection core"

   (test-case "direct hit: @covers of the changed module is selected with a complete reason"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define sel (select root '("core/base.rkt")))
         (check-equal? (selection-selected sel)
                       '("tests/t-base.rkt" "tests/t-mid.rkt" "tests/t-top.rkt"))
         (define e (entry-for sel "tests/t-base.rkt"))
         (check-equal? (hash-ref e 'reason-code) 'direct-cover)
         (check-equal? (hash-ref e 'changed-file) "core/base.rkt")
         (check-equal? (hash-ref e 'mapping-source) "@covers manifest")
         (check-false (hash-ref e 'dependency-path))
         (check-false (selection-escalated? sel)))
       (lambda () (delete-directory/files root))))

   (test-case "transitive hit: dependents' tests selected with dependency paths"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define sel (select root '("core/mid.rkt")))
         (check-equal? (selection-selected sel) '("tests/t-mid.rkt" "tests/t-top.rkt"))
         (check-equal? (hash-ref (entry-for sel "tests/t-top.rkt") 'reason-code)
                       'transitive-dependent)
         (check-equal? (hash-ref (entry-for sel "tests/t-top.rkt") 'dependency-path)
                       "core/mid.rkt → app/top.rkt")
         ;; the two-hop case through mid
         (define sel2 (select root '("core/base.rkt")))
         (check-equal? (hash-ref (entry-for sel2 "tests/t-top.rkt") 'dependency-path)
                       "core/base.rkt → core/mid.rkt → app/top.rkt")
         ;; modules not on a changed path stay unselected
         (check-false (entry-for sel "tests/t-base.rkt")))
       (lambda () (delete-directory/files root))))

   (test-case "changed test file selects itself (L0 loop)"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define sel (select root '("tests/t-base.rkt")))
         (check-equal? (selection-selected sel) '("tests/t-base.rkt"))
         (check-equal? (hash-ref (entry-for sel "tests/t-base.rkt") 'reason-code)
                       'changed-test-file)
         (check-equal? (hash-ref (entry-for sel "tests/t-base.rkt") 'mapping-source) "self")
         (check-false (selection-escalated? sel)))
       (lambda () (delete-directory/files root))))

   (test-case "unmapped source escalates; empty selection is never silent"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define sel (select root '("orphan.rkt")))
         (check-equal? (selection-selected sel) '())
         (check-true (selection-escalated? sel))
         (check-not-false (member 'unmapped-source (escalation-codes sel)))
         (check-not-false (member "fast" (selection-fallback-suites sel)))
         (check-false (selection-doc-only? sel)))
       (lambda () (delete-directory/files root))))

   (test-case "escape hatches expand to declared broad suites with explicit reasons"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define cases
           '(("dyn/dyn.rkt" dynamic-require)
             ("mac/m.rkt" macro-change)
             ("scripts/run-tests/runner.rkt" runner-helper-change)
             (".github/workflows/ci.yml" config-change)
             ("tests/fixtures/data.json" fixture-change)
             ("gen/generated/g.rkt" generated-code)))
         (for ([case (in-list cases)])
           (define sel (select root (list (car case))))
           (check-not-false (member (cadr case) (escalation-codes sel))
                       (format "~a should escalate ~a" (car case) (cadr case)))
           (check-not-false (member "fast" (selection-fallback-suites sel))
                       (format "~a must fall back to a broad suite" (car case)))))
       (lambda () (delete-directory/files root))))

   (test-case "dynamic-require change still selects direct covers alongside the escalation"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define sel (select root '("dyn/dyn.rkt")))
         (check-equal? (selection-selected sel) '("tests/t-dyn.rkt"))
         (check-equal? (hash-ref (entry-for sel "tests/t-dyn.rkt") 'reason-code) 'direct-cover))
       (lambda () (delete-directory/files root))))

   (test-case "graph parse failure fails open"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define sel (select root '("broken/broken.rkt")))
         (check-not-false (member 'graph-parse-failure (escalation-codes sel)))
         ;; the direct cover is still selected; the escalation guarantees breadth
         (check-equal? (selection-selected sel) '("tests/t-broken.rkt")))
       (lambda () (delete-directory/files root))))

   (test-case "doc-only change is reported as such, no escalation"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define sel (select root '("README.md")))
         (check-true (selection-doc-only? sel))
         (check-false (selection-escalated? sel))
         (check-equal? (selection-selected sel) '()))
       (lambda () (delete-directory/files root))))

   (test-case "reasons are stable across identical runs"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define a (select root '("core/base.rkt" "dyn/dyn.rkt")))
         (define b (select root '("core/base.rkt" "dyn/dyn.rkt")))
         (check-equal? a b))
       (lambda () (delete-directory/files root))))))

(define manifest-suite
  (test-suite
   "@covers manifest"

   (test-case "covers-of-file parses @covers header lines"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (check-equal? (covers-of-file root "tests/t-base.rkt") '("core/base.rkt")))
       (lambda () (delete-directory/files root))))

   (test-case "manifest write/load round-trip preserves mappings and provenance"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (write-covers-manifest! root "test-runner")
         (define-values (covers sources status) (load-coverage-manifest root))
         (check-equal? status 'loaded)
         (check-equal? (hash-ref covers "tests/t-base.rkt") '("core/base.rkt"))
         (check-equal? (hash-ref sources "tests/t-base.rkt") "metadata"))
       (lambda () (delete-directory/files root))))

   (test-case "missing manifest degrades to empty and reports missing"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (define-values (covers sources status) (load-coverage-manifest root))
         (check-equal? status 'missing)
         (check-equal? (hash-count covers) 0))
       (lambda () (delete-directory/files root))))

   (test-case "corrupt manifest reports corrupt, never a guess"
     (define root (make-fixture-tree!))
     (dynamic-wind
       void
       (lambda ()
         (write-file! root "tests/.coverage-manifest.json" "{ not json ]")
         (define-values (covers sources status) (load-coverage-manifest root))
         (check-equal? status 'corrupt)
         (check-equal? (hash-count covers) 0))
       (lambda () (delete-directory/files root))))))


(define prioritization-suite
  (test-suite
   "deterministic prioritization (W6)"

   (let ()
     ;; A selected set covering every tier, deliberately fed in non-emission order.
     (define files '("tests/t-rest.rkt"     ; remaining
                     "tests/t-fail-a.rkt"   ; recent failure, weight 1/2
                     "tests/t-fail-b.rkt"   ; recent failure, weight 3/4
                     "tests/t-boundary.rkt" ; changed-boundary contract test
                     "tests/t-mid.rkt"      ; transitive dependent
                     "tests/t-base.rkt"     ; direct @covers
                     "tests/t-exp.rkt"))    ; explicitly named

     (define entries
       (list (hasheq 'file "tests/t-base.rkt" 'reason-code 'direct-cover
                     'changed-file "core/base.rkt" 'mapping-source "@covers manifest")
             (hasheq 'file "tests/t-mid.rkt" 'reason-code 'transitive-dependent
                     'changed-file "core/base.rkt" 'mapping-source "@covers manifest"
                     'dependency-path "core/base.rkt → core/mid.rkt")
             (hasheq 'file "tests/t-boundary.rkt" 'reason-code 'fallback-suite
                     'changed-file ".github/workflows/ci.yml" 'mapping-source "escalation")))

     (define ctx
       (make-prioritize-ctx '("tests/t-exp.rkt")
                            entries
                            (hash "tests/t-fail-a.rkt" 1/2 "tests/t-fail-b.rkt" 3/4)
                            (hash "tests/t-boundary.rkt" "integration")))

     (test-case "tier order: explicit → direct → transitive → boundary → recent-failure → remaining"
       (define-values (ordered emitted) (prioritize-partition files ctx))
       (check-equal? ordered
                    '("tests/t-exp.rkt"
                      "tests/t-base.rkt"
                      "tests/t-mid.rkt"
                      "tests/t-boundary.rkt"
                      "tests/t-fail-b.rkt"   ; weight 3/4 before 1/2 within tier 4
                      "tests/t-fail-a.rkt"
                      "tests/t-rest.rkt"))
       (check-equal? (map (lambda (e) (hash-ref e 'tier)) emitted)
                    '("explicit" "direct" "transitive" "boundary"
                      "recent-failure" "recent-failure" "remaining")))

     (test-case "every emitted test carries a priority reason"
       (define-values (_ emitted) (prioritize-partition files ctx))
       (for ([e (in-list emitted)])
         (check-pred non-empty-string? (hash-ref e 'priority-reason))
         (check-pred exact-nonnegative-integer? (hash-ref e 'tier-rank))))

     (test-case "ordering NEVER changes the selected set"
       (define-values (ordered _) (prioritize-partition files ctx))
       (check-equal? (sort files string<?) (sort ordered string<?)))

     (test-case "byte-stable across identical runs"
       (define-values (o1 e1) (prioritize-partition files ctx))
       (define-values (o2 e2) (prioritize-partition files ctx))
       (check-equal? o1 o2)
       (check-equal? e1 e2)

       ;; Same inputs through the JSON renderer must be byte-identical.
       (check-equal? (render-order-json o1 e1) (render-order-json o2 e2)))

     (test-case "ties break stably by repository path"
       (define ctx2 (make-prioritize-ctx '() '() (hash) (hash)))
       (define-values (ordered _) (prioritize-partition files ctx2))
       (check-equal? ordered (sort files string<?))))))

(define (write-history! p entries)
  (with-output-to-file p
    (lambda ()
      (write-json (hasheq 'files
                          (for/list ([e (in-list entries)])
                            (hasheq 'path (car e) 'category (cdr e))))))))

(define history-suite
  (test-suite
   "failure history (retained CI JSON, decaying weights)"

   (test-case "disabled when no path is given"
     (define-values (w s) (load-failure-history #f))
     (check-equal? s 'disabled)
     (check-equal? (hash-count w) 0))

   (test-case "missing path yields neutral history with a reason"
     (define-values (w s) (load-failure-history "/nonexistent/nowhere.json"))
     (check-equal? s 'missing)
     (check-equal? (hash-count w) 0))

   (test-case "corrupt history yields neutral history, never an error"
     (define root (make-temporary-file "hist-~a" 'directory))
     (dynamic-wind
       void
       (lambda ()
         (write-file! root "bad.json" "{ definitely not json")
         (define-values (w s) (load-failure-history (build-path root "bad.json")))
         (check-equal? s 'corrupt)
         (check-equal? (hash-count w) 0))
       (lambda () (delete-directory/files root))))

   (test-case "only failing/timeout files accumulate weight; passes are ignored"
     (define root (make-temporary-file "hist-~a" 'directory))
     (dynamic-wind
       void
       (lambda ()
         (write-history! (build-path root "r1.json")
                         '(("tests/a.rkt" . "fail")
                           ("tests/b.rkt" . "timeout")
                           ("tests/c.rkt" . "pass")))
         (define-values (w s) (load-failure-history (build-path root "r1.json")))
         (check-equal? s 'loaded)
         (check-equal? (hash-ref w "tests/a.rkt") 1)
         (check-equal? (hash-ref w "tests/b.rkt") 1)
         (check-false (hash-ref w "tests/c.rkt" #f)))
       (lambda () (delete-directory/files root))))

   (test-case "directory input: decay by recency, bounded by recency limit"
     (define root (make-temporary-file "hist-~a" 'directory))
     (dynamic-wind
       void
       (lambda ()
         ;; three artifacts with forced mtimes: newest r3, then r2, then r1
         (write-history! (build-path root "r1.json") '(("tests/old.rkt" . "fail")))
         (write-history! (build-path root "r2.json") '(("tests/mid.rkt" . "fail")))
         (write-history! (build-path root "r3.json") '(("tests/new.rkt" . "fail")))
         (define base (current-seconds))
         (file-or-directory-modify-seconds (build-path root "r1.json") (- base 100))
         (file-or-directory-modify-seconds (build-path root "r2.json") (- base 50))
         (file-or-directory-modify-seconds (build-path root "r3.json") base)
         ;; limit 2 → only r3 (k=0, weight 1) and r2 (k=1, weight 1/2) are read
         (define-values (w s) (load-failure-history root 2 1/2))
         (check-equal? s 'loaded)
         (check-equal? (hash-ref w "tests/new.rkt") 1)
         (check-equal? (hash-ref w "tests/mid.rkt") 1/2)
         (check-false (hash-ref w "tests/old.rkt" #f)))
       (lambda () (delete-directory/files root))))

   (test-case "one file failing across artifacts accumulates decayed weight"
     (define root (make-temporary-file "hist-~a" 'directory))
     (dynamic-wind
       void
       (lambda ()
         (write-history! (build-path root "r1.json") '(("tests/flaky.rkt" . "fail")))
         (write-history! (build-path root "r2.json") '(("tests/flaky.rkt" . "fail")))
         (define base (current-seconds))
         (file-or-directory-modify-seconds (build-path root "r1.json") (- base 100))
         (file-or-directory-modify-seconds (build-path root "r2.json") base)
         (define-values (w s) (load-failure-history root))
         (check-equal? s 'loaded)
         (check-equal? (hash-ref w "tests/flaky.rkt") 3/2)) ; 1 + 1/2
       (lambda () (delete-directory/files root))))))

(module+ test
  (run-tests
   (test-suite
    "test-run-tests-impact"
    selection-suite
    manifest-suite
    prioritization-suite
    history-suite)))
