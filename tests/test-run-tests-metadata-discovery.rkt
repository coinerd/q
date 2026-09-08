#lang racket

;; @suite default
;; @speed fast
;; @boundary integration
;; @isolation process

(require rackunit
         rackunit/text-ui
         (only-in "../util/version.rkt" q-version)
         racket/file
         json
         racket/path
         racket/runtime-path
         racket/string
         (only-in "../scripts/run-tests/inventory.rkt"
                  tier-matrix-columns
                  tier-ownership-errors
                  tier-ownership-drift-errors
                  tier-ownership-rows
                  gate-ownership-rows)
         "../scripts/run-tests.rkt")

(define-runtime-path here ".")

(define (write-lines path lines)
  (call-with-output-file path
                         #:exists 'replace
                         (lambda (out)
                           (for ([line (in-list lines)])
                             (displayln line out)))))

(define metadata-discovery-tests
  (test-suite "run-tests metadata discovery"

    (test-case "metadata parser recognizes v0.99.30 schema tags"
      (define tmp (make-temporary-file "q-run-tests-metadata-~a.rkt"))
      (dynamic-wind void
                    (lambda ()
                      (write-lines tmp
                                   '("#lang racket" ";; @suite smoke tui"
                                                    ";; @speed fast"
                                                    ";; @requires network browser"
                                                    ";; @isolation subprocess"
                                                    ";; @not-test false"
                                                    "(require rackunit)"
                                                    "(check-true #t)"))
                      (clear-metadata-cache!)
                      (define meta (get-file-metadata tmp))
                      (check-equal? (hash-ref meta 'speed #f) 'fast)
                      (check-equal? (hash-ref meta 'suite #f) "smoke")
                      (check-equal? (hash-ref meta 'suites #f) '("smoke" "tui"))
                      (check-equal? (hash-ref meta 'requires #f) '("network" "browser"))
                      (check-equal? (hash-ref meta 'isolation #f) "process")
                      ;; `subprocess` is a deprecated alias (schema-isolation-deprecated-aliases):
                      ;; the parser normalizes it to the canonical "process".
                      (check-false (hash-ref meta 'not-test? #t)))
                    (lambda ()
                      (when (file-exists? tmp)
                        (delete-file tmp))
                      (clear-metadata-cache!))))

    (test-case "bare @not-test metadata excludes helper files from discovery"
      (define fixture (build-path here "test-w1-not-test-fixture.rkt"))
      (dynamic-wind (lambda ()
                      (write-lines fixture
                                   '("#lang racket" ";; @not-test"
                                                    ";; helper module intentionally named test-*"
                                                    "(provide helper-value)"
                                                    "(define helper-value 42)"))
                      (clear-metadata-cache!))
                    (lambda ()
                      (define rel "tests/test-w1-not-test-fixture.rkt")
                      (check-true (hash-ref (get-file-metadata rel) 'not-test? #f))
                      (check-false (member rel (collect-test-files 'all)))
                      (check-equal? (classify-exclusion-reason rel) 'metadata-not-test))
                    (lambda ()
                      (when (file-exists? fixture)
                        (delete-file fixture))
                      (clear-metadata-cache!)))

      ;; ── W0: tier-ownership matrix (eight ownership columns) ──
      ;;
      ;; Red-first contract: every test family declares test, behavior,
      ;; boundary, side effects, required gates, L4 destination, overlap
      ;; rationale, owner. Generation fails closed on a missing column, a
      ;; stale L4 destination or an undeclared overlap; matrix-vs-reality
      ;; drift (family on disk absent from the matrix or vice versa) is
      ;; reported by the governance drift check.

      (test-case "tier-ownership rows: every reality row carries all eight non-empty columns"
        (check-equal? (length tier-matrix-columns) 8)
        (define rows (tier-ownership-rows))
        (check-true (pair? rows) "expected at least one test family on disk")
        (for ([row (in-list rows)])
          (define test (hash-ref row 'test "?"))
          (for ([col (in-list tier-matrix-columns)])
            (check-true (hash-has-key? row col) (format "~a: missing column ~a" test col))
            (check-true (let ([v (hash-ref row col #f)])
                          (and v
                               (not (equal? v ""))
                               (not (null? (if (list? v)
                                               v
                                               '(0))))))
                        (format "~a: empty column ~a" test col)))))

      (test-case "tier-ownership errors: a family with a missing column fails generation"
        (define complete
          (hasheq 'test
                  "tests/example-family.rkt"
                  'behavior
                  "b"
                  'boundary
                  "unit"
                  'side_effects
                  "none-declared"
                  'required_gates
                  '("fast")
                  'l4_destination
                  "in-place fast"
                  'overlap_rationale
                  "none"
                  'owner
                  "core"))
        (check-equal? (tier-ownership-errors (list complete)) '())
        (define missing-owner (hash-remove complete 'owner))
        (define errs (tier-ownership-errors (list missing-owner)))
        (check-true (pair? errs) "a row missing the owner column must fail generation")
        (check-true (ormap (lambda (e) (string-contains? e "missing column: owner")) errs)))

      (test-case "tier-ownership errors: a stale L4 destination fails generation"
        (define row
          (hasheq 'test
                  "tests/example-family.rkt"
                  'behavior
                  "b"
                  'boundary
                  "unit"
                  'side_effects
                  "none-declared"
                  'required_gates
                  '("slow/L4")
                  'l4_destination
                  "in-place fast"
                  'overlap_rationale
                  "none"
                  'owner
                  "core"))
        (define errs (tier-ownership-errors (list row)))
        (check-true (pair? errs) "required gates slow/L4 with an in-place-fast destination is stale")
        (check-true (ormap (lambda (e) (string-contains? e "stale L4 destination")) errs)))

      (test-case "tier-ownership errors: an undeclared overlap fails generation"
        (define behaviors
          (list (hasheq 'behavior-id "B1" 'members '("tests/shared-family.rkt"))
                (hasheq 'behavior-id "B2" 'members '("tests/shared-family.rkt"))))
        (define row
          (hasheq 'test
                  "tests/shared-family.rkt"
                  'behavior
                  "b"
                  'boundary
                  "unit"
                  'side_effects
                  "none-declared"
                  'required_gates
                  '("fast")
                  'l4_destination
                  "in-place fast"
                  'overlap_rationale
                  "none"
                  'owner
                  "core"))
        (define errs (tier-ownership-errors (list row) behaviors))
        (check-true (pair? errs) "a family shared by 2+ behavior rows must declare the overlap")
        (check-true (ormap (lambda (e) (string-contains? e "undeclared overlap")) errs))
        (define declared
          (hash-set
           row
           'overlap_rationale
           "shared member of 2 v1.00.24 behavior rows (B1,B2): edits require every row owner's review"))
        (check-equal? (tier-ownership-errors (list declared) behaviors) '()))

      (test-case "tier-ownership drift: disk-absent-from-matrix and matrix-absent-from-disk are drift"
        (define reality
          (list (hasheq 'test
                        "tests/example-family.rkt"
                        'behavior
                        "b"
                        'boundary
                        "unit"
                        'side_effects
                        "none-declared"
                        'required_gates
                        '("fast")
                        'l4_destination
                        "in-place fast"
                        'overlap_rationale
                        "none"
                        'owner
                        "core")))
        (check-equal? (tier-ownership-drift-errors reality reality) '())
        (define missing-from-matrix (tier-ownership-drift-errors '() reality))
        (check-true (pair? missing-from-matrix))
        (check-true (ormap (lambda (e)
                             (string-contains? e "present on disk but absent from the matrix"))
                           missing-from-matrix))
        (check-true (pair? (tier-ownership-drift-errors reality '()))
                    "a matrix row with no file on disk is drift")
        (define stale-owner (hash-set (car reality) 'owner "someone-else"))
        (check-true (pair? (tier-ownership-drift-errors (list stale-owner) reality))
                    "a column mismatch between matrix and reality is drift"))

      (test-case (string-append "checksummed v"
                                q-version
                                "-w0 matrix is complete and drift-free against the tree")
        (define repo-root (simplify-path (build-path here "..")))
        (define artifact
          (build-path repo-root
                      "artifacts"
                      "tier-ownership"
                      (string-append "v" q-version "-w0")
                      "ownership-matrix.json"))
        (check-true (file-exists? artifact) "the checksummed -w0 ownership matrix must exist")
        (define jsexpr (call-with-input-file artifact read-json))
        (define raw-rows
          (if (list? jsexpr)
              jsexpr
              (hash-ref jsexpr 'rows)))
        (define matrix
          (for/list ([jrow (in-list raw-rows)])
            (for/hash ([(k v) (in-hash jrow)])
              (values (if (symbol? k)
                          k
                          (string->symbol (format "~a" k)))
                      v))))
        (define reality (tier-ownership-rows))
        (check-equal? (tier-ownership-errors reality)
                      '()
                      "reality rows must pass generation-time validation")
        (check-equal? (tier-ownership-drift-errors matrix reality)
                      '()
                      "the checksummed matrix must match reality: no drift")))))

(run-tests metadata-discovery-tests)
