#lang racket

;; @covers scripts/impact-selector/evaluate.rkt

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-impact-selector-evaluate.rkt — the campaign's W7 wave: the §10 selector
;; threat model exercised against the static, explanation-only evaluator
;; (scripts/impact-selector/evaluate.rkt).
;;
;; EVERY threat-model case must yield broaden/fallback — never a narrower
;; selection. The eighteen §10 classes covered here:
;;
;;   1.  changed test helper          -> fallback:broad helper-change
;;   2.  changed fixture helper       -> fallback:broad fixture-change
;;   3.  changed runner               -> fallback:broad runner-change
;;   4.  changed workflow             -> fallback:broad workflow-change
;;   5.  changed package/dep metadata -> fallback:broad
;;                                       config-or-dependency-metadata
;;   6.  dynamic require              -> fallback:broad dynamic-require
;;   7.  macro-generated dependency   -> fallback:broad
;;                                       macro-generated-dependency
;;   8.  reader/generated source      -> fallback:broad generated-source
;;   9.  missing @covers              -> fallback:broad
;;                                       missing-mapping-metadata
;;   10. malformed dependency graph   -> fallback:broad graph-parse-failure
;;   11. deleted source               -> fallback:broad
;;                                       deleted-or-renamed-source
;;   12. renamed source               -> fallback:broad
;;                                       deleted-or-renamed-source
;;   13. multi-area change            -> fallback:broad multi-area-change
;;   14. security-sensitive path      -> fallback:broad
;;                                       security-sensitive-path
;;   15. platform-sensitive path      -> fallback:broad
;;                                       platform-sensitive-path
;;   16. unknown path                 -> fallback:broad unknown-path
;;   17. empty diff                   -> broad empty-diff (the broad tier,
;;                                       never a narrower selection)
;;   18. huge diff                    -> fallback:broad huge-diff
;;   +   timeout/resource exceedance  -> fallback:broad budget-exceeded
;;                                       (budget-ok = false)
;;
;; Also pinned: zero test execution (tests-run = 0 and
;; zero-test-execution = true on EVERY record, plus a static source scan
;; proving the evaluator has no test-executing code path), determinism
;; (same input twice -> identical canonical output modulo the measured
;; elapsed-ms field), budget fields present on every record, workflow
;; claim-set attribution, SHA-256 known-answer vectors, and canonical JSON
;; key-order invariance.
;;
;; Tests run against a SMALL INLINE FIXTURE GRAPH in a temp directory —
;; never against the huge committed W0 graph (that one is exercised by the
;; offline replay artifact instead). No sleeps, no threads, no network.

(require rackunit
         rackunit/text-ui
         racket/base
         racket/file
         json
         (only-in "../util/version.rkt" q-version)
         (only-in "../scripts/impact-selector/evaluate.rkt"
                  broadening-reasons
                  canonical-json
                  consumer-placeholder
                  decision-schema
                  default-budget-ms
                  evaluate-impact
                  hard-timeout-ms
                  huge-diff-file-limit
                  sha256-hex))

;; ---------------------------------------------------------------------------
;; Repo layout (cwd-independent)
;; ---------------------------------------------------------------------------

(define-values (this-file-dir _tf-name _tf-dir?)
  (split-path (resolved-module-path-name (variable-reference->resolved-module-path
                                          (#%variable-reference)))))

(define repo-root (simplify-path (build-path this-file-dir "..")))

;; ---------------------------------------------------------------------------
;; Fixture graph (temp dir; W0 graph shape + claims + selector mapping)
;; ---------------------------------------------------------------------------

(define tmp-root (make-temporary-file "w7-selector-fixture~a" 'directory))

(define fixture-dir (build-path tmp-root "graphdir"))
(define corrupt-dir (build-path tmp-root "corruptdir"))
(define no-claims-dir (build-path tmp-root "noclaimsdir"))
(define out-dir (build-path tmp-root "out"))
(make-directory* fixture-dir)
(make-directory* corrupt-dir)
(make-directory* no-claims-dir)
(make-directory* out-dir)

(define (write-json-file! path j)
  (call-with-output-file path
                         #:exists 'truncate/replace
                         (lambda (o)
                           (write-json j o)
                           (newline o))))

(define fixture-graph
  (hasheq 'artifact
          "graph"
          'schema_version
          "1.0.0"
          'wave
          (format "v~a-w7-fixture" q-version)
          'base_commit
          "000000000000"
          'node_types
          (list "workflow" "job" "claim")
          'nodes
          (hasheq 'workflows
                  (list (hasheq 'id "wf:ci" 'file ".github/workflows/ci.yml" 'gate_role "required"))
                  'jobs
                  (list (hasheq 'id "job:ci:fast" 'workflow "wf:ci" 'status "required")
                        (hasheq 'id "job:ci:test" 'workflow "wf:ci" 'status "required")
                        (hasheq 'id "job:ci:lint" 'workflow "wf:ci" 'status "required"))
                  'claims_ref
                  "claims.json")
          'edges
          (hasheq 'enables '())))

(define fixture-claims
  (hasheq 'artifact
          "claims"
          'schema_version
          "1.0.0"
          'claims
          (list (hasheq 'claim_id
                        "claim:pr-ci:linux-fast-suite"
                        'proof_type
                        "suite-fast"
                        'produced_by
                        (list "job:ci:fast" "job:ci:test"))
                (hasheq 'claim_id
                        "claim:pr-ci:linux-lint-static"
                        'proof_type
                        "lint/static"
                        'produced_by
                        (list "job:ci:lint"))
                (hasheq 'claim_id
                        "claim:platform:macos-fast-suite"
                        'proof_type
                        "suite-platform"
                        'produced_by
                        (list "job:ci:test")))))

(define fixture-mapping
  (hasheq
   'schema_version
   "1"
   'area_rules
   (list (hasheq 'area "runtime" 'prefix "runtime/")
         (hasheq 'area "llm" 'prefix "llm/")
         (hasheq 'area "gui" 'prefix "gui/")
         (hasheq 'area "security" 'prefix "security/")
         (hasheq 'area "tests" 'prefix "tests/")
         (hasheq 'area "docs" 'prefix "docs/")
         (hasheq 'area "scripts" 'prefix "scripts/"))
   'coverage
   (list
    (hasheq 'test "tests/test-runtime-foo.rkt" 'covers (list "runtime/foo.rkt"))
    (hasheq 'test "tests/test-runtime-bar.rkt" 'covers (list "runtime/foo.rkt" "runtime/bar.rkt"))
    (hasheq 'test "tests/test-llm-baz.rkt" 'covers (list "llm/baz.rkt")))
   'uncertain_sources
   (hasheq 'runtime/dyn.rkt
           "dynamic-require"
           'runtime/macro-gen.rkt
           "macro-generated"
           'runtime/gen-out.rkt
           "generated-source")))

(define graph-path (build-path fixture-dir "graph.json"))
(define claims-path (build-path fixture-dir "claims.json"))
(define mapping-path (build-path fixture-dir "selector-mapping.json"))
(write-json-file! graph-path fixture-graph)
(write-json-file! claims-path fixture-claims)
(write-json-file! mapping-path fixture-mapping)

;; Corrupt graph: unparseable bytes in place of JSON.
(define corrupt-graph-path (build-path corrupt-dir "graph.json"))
(call-with-output-file corrupt-graph-path
                       #:exists 'truncate/replace
                       (lambda (o) (display "not json {{{" o)))

;; Graph directory with NO claims.json companion: the §11.3 affected claim
;; set would be unknowable, so the evaluator must broaden.
(define no-claims-graph-path (build-path no-claims-dir "graph.json"))
(write-json-file! no-claims-graph-path fixture-graph)

(define (make-diff! name changes)
  (define p (build-path out-dir name))
  (write-json-file! p (hasheq 'schema "q.impact-diff/1" 'source_sha "feedface0000" 'changes changes))
  p)

(define (eval-against diff-path [graph graph-path] #:budget-ms [budget default-budget-ms])
  (evaluate-impact diff-path graph "feedface0000" #f #:budget-ms budget))

(define-syntax-rule (diff-of [p c] ...)
  (list (hasheq 'path p 'change c) ...))

;; ---------------------------------------------------------------------------
;; The eighteen §10 threat-model cases (all broaden/fallback, never narrower)
;; ---------------------------------------------------------------------------

(define (mk name changes [graph graph-path])
  (cons name (eval-against (make-diff! name changes) graph)))

(define threat-cases
  (list
   ;; 1. changed test helper
   (mk "case-helper.json" (diff-of ["tests/helpers/db-util.rkt" "modified"]))
   ;; 2. changed fixture helper
   (mk "case-fixture.json" (diff-of ["tests/fixtures/app/seed.rkt" "modified"]))
   ;; 3. changed runner
   (mk "case-runner.json" (diff-of ["scripts/run-tests.rkt" "modified"]))
   ;; 4. changed workflow
   (mk "case-workflow.json" (diff-of [".github/workflows/ci.yml" "modified"]))
   ;; 5. changed package/dependency metadata
   (mk "case-metadata.json" (diff-of ["info.rkt" "modified"]))
   ;; 6. dynamic require
   (mk "case-dynamic-require.json" (diff-of ["runtime/dyn.rkt" "modified"]))
   ;; 7. macro-generated dependency
   (mk "case-macro-generated.json" (diff-of ["runtime/macro-gen.rkt" "modified"]))
   ;; 8. reader/generated source
   (mk "case-generated.json" (diff-of ["runtime/generated/expand.rkt" "modified"]))
   ;; 9. missing @covers (mapped area, no coverage entry)
   (mk "case-missing-covers.json" (diff-of ["runtime/unmapped.rkt" "modified"]))
   ;; 10. malformed dependency graph
   (mk "case-bad-graph.json" (diff-of ["runtime/foo.rkt" "modified"]) corrupt-graph-path)
   ;; 11. deleted source
   (mk "case-deleted.json" (diff-of ["runtime/foo.rkt" "deleted"]))
   ;; 12. renamed source
   (mk "case-renamed.json"
       (list (hasheq 'path "runtime/foo.rkt" 'change "renamed" 'old_path "runtime/old-foo.rkt")))
   ;; 13. multi-area change (both files individually mapped)
   (mk "case-multi-area.json" (diff-of ["runtime/foo.rkt" "modified"] ["llm/baz.rkt" "modified"]))
   ;; 14. security-sensitive path
   (mk "case-security.json" (diff-of ["security/credential-policy.rkt" "modified"]))
   ;; 15. platform-sensitive path
   (mk "case-platform.json" (diff-of ["gui/window.rkt" "modified"]))
   ;; 16. unknown path
   (mk "case-unknown.json" (diff-of ["third_party/mystery/lib.xyz" "modified"]))
   ;; 17. empty diff
   (mk "case-empty.json" '())
   ;; 18. huge diff (mapped files, but over the file-count budget)
   (mk
    "case-huge.json"
    (for/list ([i (in-range (+ huge-diff-file-limit 10))])
      (hasheq 'path (string-append "runtime/huge-" (number->string i) ".rkt") 'change "modified")))))

(define (find-case name)
  (cdr (assoc name threat-cases string=?)))

(define expected-threat-reasons
  (list "helper-change"
        "fixture-change"
        "runner-change"
        "workflow-change"
        "config-or-dependency-metadata"
        "dynamic-require"
        "macro-generated-dependency"
        "generated-source"
        "missing-mapping-metadata"
        "graph-parse-failure"
        "deleted-or-renamed-source"
        "multi-area-change"
        "security-sensitive-path"
        "platform-sensitive-path"
        "unknown-path"
        "empty-diff"
        "huge-diff"))

;; ---------------------------------------------------------------------------
;; Suites
;; ---------------------------------------------------------------------------

(define sha-suite
  (test-suite "sha256 known-answer vectors"
    (check-equal? (sha256-hex #"")
                  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                  "empty-string vector (FIPS 180-4)")
    (check-equal? (sha256-hex #"abc")
                  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                  "abc vector (FIPS 180-4)")
    (check-equal? (sha256-hex #"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                  "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
                  "56-byte two-block-boundary vector (FIPS 180-4)")))

(define canonical-suite
  (test-suite "canonical JSON"
    (check-equal? (canonical-json (hasheq 'b 1 'a 2)) "{\"a\":2,\"b\":1}" "keys sorted")
    (check-equal? (canonical-json (hasheq 'x (list 1 "two" #t '())))
                  "{\"x\":[1,\"two\",true,[]]}"
                  "composite encoding")
    (check-equal? (canonical-json (hasheq 'k "quote\"back\\slash"))
                  "{\"k\":\"quote\\\"back\\\\slash\"}"
                  "string escaping is round-trippable")
    (check-equal? (canonical-json (hasheq 'z (hasheq 'a (list) 'm (list (hasheq 'n 1)))))
                  "{\"z\":{\"a\":[],\"m\":[{\"n\":1}]}}"
                  "nested containers")))

(define produced-reasons
  (for/list ([case-pair threat-cases])
    (hash-ref (cdr case-pair) 'reason #f)))

(define selected-record
  (eval-against (make-diff! "case-selected.json" (diff-of ["runtime/foo.rkt" "modified"]))))

(define changed-test-record
  (eval-against (make-diff! "case-changed-test.json"
                            (diff-of ["tests/test-runtime-foo.rkt" "modified"]))))

(define docs-record
  (eval-against (make-diff! "case-docs.json"
                            (diff-of ["docs/README.md" "modified"] ["docs/guide.md" "added"]))))

(define no-claims-record
  (eval-against (make-diff! "case-no-claims.json" (diff-of ["runtime/foo.rkt" "modified"]))
                no-claims-graph-path))

(define over-budget-record
  (eval-against (make-diff! "case-budget.json" (diff-of ["runtime/foo.rkt" "modified"]))
                #:budget-ms 0))

(define clamped-record
  (eval-against (make-diff! "case-clamp.json" (diff-of ["runtime/foo.rkt" "modified"]))
                #:budget-ms 999999))

(define determinism-diff
  (make-diff! "case-determinism.json"
              (diff-of ["runtime/foo.rkt" "modified"] ["docs/notes.md" "modified"])))

(define r1 (eval-against determinism-diff))
(define r2 (eval-against determinism-diff))
(define r3 (eval-against determinism-diff))

(define evaluator-source
  (file->string (build-path repo-root "scripts" "impact-selector" "evaluate.rkt")))

;; Executable source text only (comment lines stripped): the evaluator's
;; header documents what it does NOT do, so the scan must not read comments.
(define evaluator-source-code
  (string-join (for/list ([l (string-split evaluator-source "\n")]
                          #:unless (let ([t (string-trim l)])
                                     (or (equal? t "") (string-prefix? t ";"))))
                 l)
               "\n"))

(define threat-suite
  (test-suite "s10 selector threat model (all broaden/fallback, never narrower)"
    (for ([case-pair threat-cases])
      (define name (car case-pair))
      (define record (cdr case-pair))
      (test-case (format "~a: ~a" name (hash-ref record 'reason "unparsed"))
        ;; Every case must produce a decision record with the pinned schema.
        (check-equal? (hash-ref record 'schema #f) decision-schema)
        ;; Zero test execution on every record, by construction.
        (check-equal? (hash-ref record 'tests-run #f) 0)
        (check-true (hash-ref record 'zero-test-execution #f))
        ;; Budget fields present on every record.
        (check-true (hash-has-key? record 'budget-ms))
        (check-true (hash-has-key? record 'budget-hard-timeout-ms))
        (check-equal? (hash-ref record 'budget-hard-timeout-ms #f) hard-timeout-ms)
        (check-equal? (hash-ref record 'budget-ok #f) #t)
        (check-true (hash-has-key? record 'elapsed-ms))
        ;; §11.3 explanation fields present on every record.
        (check-equal? (hash-ref record 'consumer #f) consumer-placeholder)
        (check-equal? (hash-ref record 'source-sha #f) "feedface0000")
        (check-equal? (string-length (hash-ref record 'graph-sha256 "")) 64)
        (check-true (list? (hash-ref record 'affected-claim-set)))
        ;; Decision direction: nothing may narrow the proof.
        (check-not-false (member (hash-ref record 'decision #f) (list "fallback:broad" "broad"))
                         (format "~a must broaden, never select" name))
        (check-not-equal? (hash-ref record 'decision #f) "selected")
        ;; The reason is a declared broadening reason.
        (check-not-false (member (hash-ref record 'reason #f) broadening-reasons)
                         "reason must be declared in broadening-reasons")))
    ;; Empty diff: decision is the plain broad tier (the broad gate simply
    ;; remains the proof; still never narrower than broad).
    (check-equal? (hash-ref (find-case "case-empty.json") 'decision #f) "broad")
    ;; Class coverage: every §10 class has a case with its expected named
    ;; reason.
    (for ([expected expected-threat-reasons])
      (check-not-false (member expected produced-reasons)
                       (format "missing §10 case for ~a" expected)))
    ;; Workflow attribution: the affected claim set names the claims the
    ;; changed workflow produces (§11.3 claim-set evidence).
    (check-equal? (hash-ref (find-case "case-workflow.json") 'affected-claim-set #f)
                  (list "claim:platform:macos-fast-suite"
                        "claim:pr-ci:linux-fast-suite"
                        "claim:pr-ci:linux-lint-static"))
    ;; Deleted and renamed each hit the same conservative class.
    (check-equal? (hash-ref (find-case "case-deleted.json") 'reason #f) "deleted-or-renamed-source")
    (check-equal? (hash-ref (find-case "case-renamed.json") 'reason #f) "deleted-or-renamed-source")))

(define selection-suite
  (test-suite "selection happy path + never-narrower guards"
    ;; Mapped single-area source change: the ONLY case that selects, and it
    ;; selects the union of the covering test files.
    (check-equal? (hash-ref selected-record 'decision #f) "selected")
    (check-equal? (hash-ref selected-record 'reason #f) "mapped-single-area")
    (check-equal? (hash-ref (hash-ref selected-record 'selected #f) 'tests #f)
                  (list "tests/test-runtime-bar.rkt" "tests/test-runtime-foo.rkt"))
    (check-equal? (hash-ref selected-record 'tests-run #f) 0)
    (check-equal? (hash-ref selected-record 'affected-claim-set #f)
                  (list "claim:platform:macos-fast-suite" "claim:pr-ci:linux-fast-suite"))
    ;; A changed test file selects itself (minimal correct obligation).
    (check-equal? (hash-ref changed-test-record 'decision #f) "selected")
    (check-equal? (hash-ref (hash-ref changed-test-record 'selected #f) 'tests #f)
                  (list "tests/test-runtime-foo.rkt"))
    ;; Docs-only change: decision is the broad tier, never narrower.
    (check-equal? (hash-ref docs-record 'decision #f) "broad")
    (check-equal? (hash-ref docs-record 'reason #f) "docs-only-change")
    ;; Graph without claims inventory: claim set unknowable -> broaden.
    (check-equal? (hash-ref no-claims-record 'decision #f) "fallback:broad")
    (check-equal? (hash-ref no-claims-record 'reason #f) "missing-claims-metadata")
    ;; Budget exceedance: fallback:broad, budget-ok = false, never
    ;; "select less".
    (check-equal? (hash-ref over-budget-record 'decision #f) "fallback:broad")
    (check-equal? (hash-ref over-budget-record 'reason #f) "budget-exceeded")
    (check-false (hash-ref over-budget-record 'budget-ok #t))
    ;; Hard-timeout clamp: a budget above the hard timeout is clamped down.
    (check-equal? (hash-ref clamped-record 'budget-ms #f) hard-timeout-ms)
    (check-equal? (hash-ref clamped-record 'decision #f) "selected")))

(define determinism-suite
  (test-suite "determinism (same input twice -> identical output)"
    ;; The only measured field is elapsed-ms; every other field must be
    ;; byte-identical under canonical JSON.
    (check-equal? (canonical-json (hash-remove r1 'elapsed-ms))
                  (canonical-json (hash-remove r2 'elapsed-ms))
                  "decision records identical modulo measured elapsed-ms")
    (check-equal? (canonical-json (hash-remove r2 'elapsed-ms))
                  (canonical-json (hash-remove r3 'elapsed-ms))
                  "third run identical too")
    (check-true (>= (hash-ref r1 'elapsed-ms -1) 0))
    (check-equal? (hash-ref r1 'graph-sha256 "") (hash-ref r2 'graph-sha256 "x"))
    (check-equal? (hash-ref r1 'reason "") (hash-ref r2 'reason "y"))
    (check-equal? (hash-ref r1 'decision "") (hash-ref r2 'decision "z"))))

(define zero-exec-suite
  (test-suite "zero test execution (static source scan)"
    ;; Strip comment lines first: the header documents what the tool does
    ;; NOT do; only executable source text is scanned below.
    (for ([banned (list "subprocess"
                        "racket/system"
                        "system*"
                        "(thread"
                        "sleep"
                        "tcp-connect"
                        "udp-"
                        "process*"
                        "racket/cmdline")])
      (check-false (string-contains? evaluator-source-code banned)
                   (format "evaluator source must not reference ~a" banned)))
    ;; The dependency require block is exactly the pinned set.
    (check-true (string-contains? evaluator-source "(require json") "json is a pinned dependency")
    (check-true (string-contains? evaluator-source "racket/contract")
                "racket/contract is a pinned dependency")))

(define all-suites
  (test-suite (format "q.impact-selector evaluator (v~a W7)" q-version)
    sha-suite
    canonical-suite
    threat-suite
    selection-suite
    determinism-suite
    zero-exec-suite))

;; ---------------------------------------------------------------------------
;; Runner wiring
;; ---------------------------------------------------------------------------

(define (cleanup!)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files tmp-root)))

(module+ test
  (run-tests all-suites)
  (cleanup!))

(module+ main
  (run-tests all-suites)
  (cleanup!))
