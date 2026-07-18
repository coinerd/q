#lang racket/base

;; @suite all
;; @speed fast
;; Restricted integer-only JCS milestone truth, evidence, and CLI integration.

(require rackunit
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../scripts/gsd-milestone-truth.rkt"
         openssl
         (only-in file/sha1 bytes->hex-string))

(define-runtime-path current-fixture "../docs/reports/gsd-milestones/v0.99.52.json")
(define-runtime-path historical-fixture "../docs/reports/gsd-milestones/v0.99.51-historical.json")
(define historical-fixture-digest "f6e409f9a9757ddc68d442667e86f58673f824d60dbc9be13ca0e86def6a2ba3")
(define-runtime-path schema-fixture "../docs/reports/gsd-milestones/v0.99.52.schema.json")
(define-runtime-path historical-evidence-dir "../docs/reports/gsd-milestones/v0.99.51-evidence")
(define-runtime-path historical-validation-dir "../docs/reports/gsd-wave-validation")
(define-runtime-path truth-script "../scripts/gsd-milestone-truth.rkt")

(define evidence-digest (make-string 64 #\a))
(define review-digest (make-string 64 #\b))
(define validation-digest (make-string 64 #\c))
(define stage-names '("build" "exact-artifact-smoke" "draft-verification" "publication"))

(define (sha256-file path)
  (call-with-input-file path (lambda (in) (bytes->hex-string (sha256-bytes in))) #:mode 'binary))

(define (make-criterion id met?)
  (hash "id" id "met" met? "evidence-digest" (if met? evidence-digest 'null)))

(define (make-work-item i #:status [status "complete"] #:met? [met? #t])
  (hash "id"
        (format "W~a" i)
        "status"
        status
        "criteria"
        (list (make-criterion (format "W~a-C1" i) met?))))

(define (make-release-evidence count)
  (for/list ([stage (in-list (take stage-names count))]
             [digit (in-range 1 (add1 count))])
    (hash "stage" stage "evidence-digest" (make-string 64 (integer->char (+ 48 digit))))))

(define (make-document #:release [release "published"]
                       #:release-count [release-count 4]
                       #:acceptance [acceptance "accepted"]
                       #:work-items [work-items
                                     (for/list ([i (in-range 11)])
                                       (make-work-item i))]
                       #:remaining [remaining '()]
                       #:review-status [review-status "approved"]
                       #:independent? [independent? #t]
                       #:review-evidence [review-evidence review-digest]
                       #:validation-status [validation-status "passed"]
                       #:validation-evidence [validation-evidence validation-digest])
  (hash "schema-version"
        1
        "milestone"
        "v1.2.3"
        "release-mechanics"
        release
        "release-evidence"
        (make-release-evidence release-count)
        "substantive-acceptance"
        acceptance
        "work-items"
        work-items
        "acceptance-critical-remaining-items"
        remaining
        "review"
        (hash "status" review-status "independent" independent? "evidence-digest" review-evidence)
        "validation"
        (hash "status" validation-status "evidence-digest" validation-evidence)))

;; Positive pure-evaluator tests may derive their digest locally. Integration
;; tests below instead derive it from a separately modeled expected projection.
(define (evaluate-current document)
  (evaluate-milestone-truth document (canonical-json-sha256 document)))

(define (replace-work-item document index replacement)
  (hash-set document
            "work-items"
            (for/list ([item (in-list (hash-ref document "work-items"))]
                       [i (in-naturals)])
              (if (= i index) replacement item))))

;; Strict JSON and restricted canonicalization -------------------------------

(test-case "strict JSON accepts the restricted integer-only data model"
  (define value
    (strict-json-read "{\"z\":null,\"a\":[true,false,\"λ\",-9007199254740991,9007199254740991]}"))
  (check-true (hash? value))
  (check-equal? (hash-ref value "z") 'null)
  (check-equal? (last (hash-ref value "a")) 9007199254740991))

(test-case "restricted profile rejects duplicate keys, floats, and unsafe integers"
  (for ([text (in-list '("{\"a\":1,\"a\":2}" "1.0" "1e2" "9007199254740992" "-9007199254740992"))])
    (check-exn exn:fail? (lambda () (strict-json-read text)) text)))

(test-case "strict JSON rejects malformed and trailing input"
  (for ([text (in-list '("01" "true false" "{\"a\":1"))])
    (check-exn exn:fail? (lambda () (strict-json-read text)) text)))

(test-case "strict JSON handles surrogate pairs but rejects unpaired surrogates"
  (check-equal? (strict-json-read "\"\\uD83D\\uDE00\"") "😀")
  (check-exn exn:fail? (lambda () (strict-json-read "\"\\uD83D\"")))
  (check-exn exn:fail? (lambda () (strict-json-read "\"\\uDE00\""))))

(test-case "restricted JCS profile sorts UTF-16 keys and emits canonical UTF-8"
  (check-equal? (bytes->string/utf-8 (canonical-json-bytes (hash "" 2 "😀" 1))) "{\"😀\":1,\"\":2}")
  (check-exn exn:fail? (lambda () (canonical-json-bytes 1.5))))

(test-case "pinned known canonical fixture digest detects a mismatch"
  ;; This published known-answer value is pinned, not calculated from a fixture file.
  (define known-fixture (hash "b" 2 "a" 1))
  (define pinned-digest "43258cff783fe7036d8a43033f830adfc60ec037382473548ac742b888292777")
  (check-equal? (canonical-json-sha256 known-fixture) pinned-digest)
  (check-not-equal? (canonical-json-sha256 (hash-set known-fixture "b" 3)) pinned-digest))

;; Schema parity and authoritative runtime invariants ------------------------

(test-case "schema names the restricted profile and documents runtime-only checks"
  (define schema (strict-json-read-file schema-fixture))
  (check-true (string-contains? (hash-ref schema "title") "restricted integer-only JCS"))
  (check-true (string-contains? (hash-ref schema "description")
                                "does not claim unrestricted RFC 8785"))
  (check-true (string-contains? (hash-ref schema "description") "Runtime-only checks"))
  (check-false (hash-ref schema "additionalProperties"))
  (check-false (hash-has-key? (hash-ref schema "properties") "projection-digest")))

(test-case "schema represents ordered release and W0-W10 prefixes"
  (define schema (strict-json-read-file schema-fixture))
  (define properties (hash-ref schema "properties"))
  (check-equal? (length (hash-ref (hash-ref properties "release-evidence") "prefixItems")) 4)
  (check-equal? (length (hash-ref (hash-ref properties "work-items") "prefixItems")) 11)
  (check-equal? (hash-ref (hash-ref properties "release-mechanics") "enum") release-mechanics-values)
  (define defs (hash-ref schema "$defs"))
  (define criterion-id-schema (hash-ref (hash-ref (hash-ref defs "criterion") "properties") "id"))
  (check-equal? (hash-ref criterion-id-schema "pattern") ".*\\S.*")
  (check-true (hash-has-key? (hash-ref defs "criterion") "if"))
  (check-equal? (length (hash-ref (hash-ref defs "work-item") "allOf")) 2)
  (check-true (hash-has-key? (hash-ref defs "review") "else"))
  (check-true (hash-has-key? (hash-ref defs "validation") "else")))

(test-case "release evidence must be an ordered gap-free evidenced prefix"
  (for ([count (in-range 5)]
        [release (in-list release-mechanics-values)])
    (define result (evaluate-current (make-document #:release release #:release-count count)))
    (check-equal? (milestone-truth-derived-release-mechanics result) release))
  (define gap
    (hash-set (make-document #:release "built" #:release-count 1)
              "release-evidence"
              (list (hash "stage" "exact-artifact-smoke" "evidence-digest" evidence-digest))))
  (check-exn exn:fail? (lambda () (validate-milestone-document gap)))
  (define no-digest
    (hash-set (make-document #:release "built" #:release-count 1)
              "release-evidence"
              (list (hash "stage" "build" "evidence-digest" 'null))))
  (check-exn exn:fail? (lambda () (validate-milestone-document no-digest))))

(test-case "criteria and wave status invariants fail closed in validation"
  (define base (make-document))
  (define met-without-digest
    (hash "id"
          "W0"
          "status"
          "in-progress"
          "criteria"
          (list (hash "id" "C" "met" #t "evidence-digest" 'null))))
  (define complete-unmet
    (hash "id" "W0" "status" "complete" "criteria" (list (make-criterion "C" #f))))
  (define planned-met (hash "id" "W0" "status" "planned" "criteria" (list (make-criterion "C" #t))))
  (define whitespace-id
    (hash "id" "W0" "status" "in-progress" "criteria" (list (make-criterion "   " #f))))
  (for ([bad (in-list (list met-without-digest complete-unmet planned-met whitespace-id))])
    (check-exn exn:fail? (lambda () (validate-milestone-document (replace-work-item base 0 bad))))))

(test-case "approved review and passed validation require evidence digests"
  (check-exn exn:fail?
             (lambda () (validate-milestone-document (make-document #:review-evidence 'null))))
  (check-exn exn:fail?
             (lambda () (validate-milestone-document (make-document #:validation-evidence 'null)))))

(test-case "closed runtime schema rejects unknown, unordered, and duplicate identifiers"
  (define base (make-document))
  (check-exn exn:fail?
             (lambda () (validate-milestone-document (hash-set base "projection-digest" "x"))))
  (check-exn exn:fail?
             (lambda ()
               (validate-milestone-document
                (hash-set base
                          "work-items"
                          (list-set (list-set (hash-ref base "work-items") 0 (make-work-item 1))
                                    1
                                    (make-work-item 0))))))
  (check-exn exn:fail?
             (lambda ()
               (validate-milestone-document
                (hash-set base "acceptance-critical-remaining-items" '("same" "same")))))
  (define duplicate-criteria
    (hash "id"
          "W0"
          "status"
          "complete"
          "criteria"
          (list (make-criterion "same" #t) (make-criterion "same" #t))))
  (check-exn exn:fail?
             (lambda () (validate-milestone-document (replace-work-item base 0 duplicate-criteria)))))

;; Truth derivation ----------------------------------------------------------

(test-case "acceptance requires every gate but is independent of release stage"
  (for ([count (in-range 5)]
        [release (in-list release-mechanics-values)])
    (define result (evaluate-current (make-document #:release release #:release-count count)))
    (check-equal? (milestone-truth-derived-substantive-acceptance result) "accepted")
    (check-equal? (milestone-truth-effective-substantive-acceptance result) "accepted")
    (check-true (milestone-truth-accepted? result))))

(test-case "planned and partial work derive planned and in-progress"
  (define planned-items
    (for/list ([i (in-range 11)])
      (make-work-item i #:status "planned" #:met? #f)))
  (define planned
    (evaluate-current (make-document #:acceptance "planned"
                                     #:work-items planned-items
                                     #:review-status "pending"
                                     #:independent? #f
                                     #:review-evidence 'null
                                     #:validation-status "pending"
                                     #:validation-evidence 'null)))
  (check-equal? (milestone-truth-derived-substantive-acceptance planned) "planned")
  (define partial-items
    (cons (make-work-item 0 #:status "in-progress" #:met? #f) (cdr planned-items)))
  (define partial
    (evaluate-current (make-document #:acceptance "in-progress"
                                     #:work-items partial-items
                                     #:remaining '("work remains")
                                     #:review-status "pending"
                                     #:independent? #f
                                     #:review-evidence 'null
                                     #:validation-status "pending"
                                     #:validation-evidence 'null)))
  (check-equal? (milestone-truth-derived-substantive-acceptance partial) "in-progress"))

(test-case "each acceptance gate is required"
  (define incomplete-items
    (cons (make-work-item 0 #:status "in-progress" #:met? #t)
          (for/list ([i (in-range 1 11)])
            (make-work-item i))))
  (define cases
    (list (make-document #:acceptance "rejected" #:work-items incomplete-items)
          (make-document #:acceptance "rejected" #:remaining '("acceptance blocker"))
          (make-document #:acceptance "rejected" #:independent? #f)
          (make-document #:acceptance "rejected" #:review-status "pending" #:review-evidence 'null)
          (make-document #:acceptance "rejected"
                         #:validation-status "pending"
                         #:validation-evidence 'null)))
  (for ([document (in-list cases)])
    (define result (evaluate-current document))
    (check-not-equal? (milestone-truth-derived-substantive-acceptance result) "accepted")
    (check-equal? (milestone-truth-effective-substantive-acceptance result) "rejected")
    (check-false (milestone-truth-accepted? result))))

(test-case "declared and derived truth remain separately observable and fail closed"
  (define release-mismatch (evaluate-current (make-document #:release "planned" #:release-count 4)))
  (check-equal? (milestone-truth-declared-release-mechanics release-mismatch) "planned")
  (check-equal? (milestone-truth-derived-release-mechanics release-mismatch) "published")
  (check-equal? (milestone-truth-effective-release-mechanics release-mismatch) "inconsistent")
  (check-equal? (milestone-truth-effective-substantive-acceptance release-mismatch) "rejected")
  (check-true (ormap (lambda (reason) (string-contains? reason "declared release-mechanics"))
                     (milestone-truth-reasons release-mismatch)))
  (define partial-items
    (cons (make-work-item 0 #:status "in-progress" #:met? #f)
          (for/list ([i (in-range 1 11)])
            (make-work-item i #:status "planned" #:met? #f))))
  (define substantive-mismatch
    (evaluate-current (make-document #:acceptance "accepted"
                                     #:work-items partial-items
                                     #:review-status "pending"
                                     #:independent? #f
                                     #:review-evidence 'null
                                     #:validation-status "pending"
                                     #:validation-evidence 'null)))
  (check-equal? (milestone-truth-derived-substantive-acceptance substantive-mismatch) "in-progress")
  (check-equal? (milestone-truth-effective-substantive-acceptance substantive-mismatch) "rejected"))

(test-case "missing, malformed, or mismatched external digest prevents acceptance"
  (for ([digest (in-list (list #f "not-a-digest" (make-string 64 #\0)))])
    (define result (evaluate-milestone-truth (make-document) digest))
    (check-equal? (milestone-truth-derived-substantive-acceptance result) "rejected")
    (check-equal? (milestone-truth-effective-substantive-acceptance result) "rejected")
    (check-false (milestone-truth-digest-matches? result))
    (check-not-false (member "projection digest does not match canonical projection"
                             (milestone-truth-reasons result)))))

(test-case "truth result carries the validated milestone identity"
  (define result (evaluate-current (make-document)))
  (check-equal? (milestone-truth-milestone result) "v1.2.3")
  (check-equal? (hash-ref (milestone-truth->jsexpr result) "milestone") "v1.2.3"))

(test-case "standalone gate predicate requires digest, accepted effective truth, and consistency"
  (check-true (milestone-truth-gate-passed? (evaluate-current (make-document))))
  (check-false (milestone-truth-gate-passed?
                (evaluate-current (make-document #:acceptance "rejected" #:remaining '("blocked")))))
  (check-false (milestone-truth-gate-passed? (evaluate-current (make-document #:release "planned"
                                                                              #:release-count 4))))
  (check-false (milestone-truth-gate-passed? (evaluate-milestone-truth (make-document)
                                                                       (make-string 64 #\0)))))

;; Fixtures and integration --------------------------------------------------

(define current-criterion-ids
  '("W0-milestone-truth-remediation" "W1-session-cancellation"
                                     "W2-credentials"
                                     "W3-context"
                                     "W4-release-truth"
                                     "W5-compaction"
                                     "W6-distributed-execution"
                                     "W7-validation"
                                     "W8-review"
                                     "W9-regression"
                                     "W10-post-release-acceptance"))

(define independently-modeled-current
  (hash "schema-version"
        1
        "milestone"
        "v0.99.52"
        "release-mechanics"
        "planned"
        "release-evidence"
        '()
        "substantive-acceptance"
        "in-progress"
        "work-items"
        (for/list ([id (in-list current-criterion-ids)]
                   [i (in-range 11)])
          (hash "id"
                (format "W~a" i)
                "status"
                (if (zero? i) "in-progress" "planned")
                "criteria"
                (list (make-criterion id #f))))
        "acceptance-critical-remaining-items"
        '("Complete W0 milestone-truth remediation and independently validate its evidence"
          "Execute planned work W1-W10 before acceptance")
        "review"
        (hash "status" "pending" "independent" #f "evidence-digest" 'null)
        "validation"
        (hash "status" "pending" "evidence-digest" 'null)))

(define independently-supplied-current-digest
  ;; This digest is produced from the separately modeled expected projection,
  ;; never by reading current-fixture (the integration subject).
  (canonical-json-sha256 independently-modeled-current))

(test-case "v0.99.52 is the truthful current in-progress fixture"
  (define document (strict-json-read-file current-fixture))
  (check-equal? document independently-modeled-current)
  (define result (evaluate-milestone-file current-fixture independently-supplied-current-digest))
  (check-equal? (hash-ref document "milestone") "v0.99.52")
  (check-equal? (map (lambda (item) (hash-ref item "status")) (hash-ref document "work-items"))
                (cons "in-progress" (make-list 10 "planned")))
  (check-equal? (milestone-truth-derived-release-mechanics result) "planned")
  (check-equal? (milestone-truth-derived-substantive-acceptance result) "in-progress")
  (check-true (milestone-truth-digest-matches? result))
  (check-true (pair? (hash-ref document "acceptance-critical-remaining-items"))))

(test-case "historical v0.99.51 derives published and rejected without impossible records"
  (define document (strict-json-read-file historical-fixture))
  (check-equal? (canonical-json-sha256 document) historical-fixture-digest)
  (define result (evaluate-milestone-file historical-fixture historical-fixture-digest))
  (check-equal? (hash-ref document "milestone") "v0.99.51")
  (check-equal? (milestone-truth-declared-release-mechanics result) "published")
  (check-equal? (milestone-truth-derived-release-mechanics result) "published")
  (check-equal? (milestone-truth-declared-substantive-acceptance result) "rejected")
  (check-equal? (milestone-truth-derived-substantive-acceptance result) "rejected")
  (define w6 (list-ref (hash-ref document "work-items") 6))
  (define w6-criterion (car (hash-ref w6 "criteria")))
  (check-equal? (hash-ref w6 "status") "in-progress")
  (check-false (hash-ref w6-criterion "met"))
  (check-equal? (hash-ref w6-criterion "evidence-digest") 'null)
  (for ([item (in-list (hash-ref document "work-items"))])
    (when (string=? (hash-ref item "status") "complete")
      (check-true (andmap (lambda (criterion) (hash-ref criterion "met"))
                          (hash-ref item "criteria"))))))

(test-case "historical evidence digests match immutable retained bytes"
  (define document (strict-json-read-file historical-fixture))
  (define release-digest
    (sha256-file (build-path historical-evidence-dir "release-asset-verification.log")))
  (for ([stage (in-list (hash-ref document "release-evidence"))])
    (check-equal? (hash-ref stage "evidence-digest") release-digest))
  (for ([item (in-list (take (hash-ref document "work-items") 9))]
        [index (in-naturals)])
    (define path (build-path historical-validation-dir (format "v0.99.51-w~a.rktd" index)))
    (for ([criterion (in-list (hash-ref item "criteria"))])
      (when (or (hash-ref criterion "met") (string=? (hash-ref item "status") "complete"))
        (check-equal? (hash-ref criterion "evidence-digest") (sha256-file path)))))
  ;; W6's retained validation is contradictory mechanical-era evidence, not
  ;; accepted criterion evidence: retain and authenticate its bytes separately.
  (check-equal? (sha256-file (build-path historical-validation-dir "v0.99.51-w6.rktd"))
                "bc9d1c3e5265171829aeeccfdf02fac5c7b5a1276f21640d3b8ac9bd0e8ff30f")
  (define deterministic-digest
    (sha256-file (build-path historical-evidence-dir "current-deterministic-gates.log")))
  (define w9-criterion (car (hash-ref (list-ref (hash-ref document "work-items") 9) "criteria")))
  (check-equal? (hash-ref w9-criterion "evidence-digest") deterministic-digest)
  (check-equal? (hash-ref (hash-ref document "validation") "evidence-digest") deterministic-digest)
  (check-equal? (hash-ref (hash-ref document "review") "evidence-digest")
                (sha256-file (build-path historical-evidence-dir
                                         "AUDIT-v0.99.51-POST-RELEASE-IN-DEPTH.md"))))

(define (run-truth-cli . arguments)
  (define-values (process stdout stdin stderr)
    (apply subprocess #f #f #f (find-executable-path "racket") (path->string truth-script) arguments))
  (close-output-port stdin)
  (define output (port->string stdout))
  (define errors (port->string stderr))
  (subprocess-wait process)
  (values (subprocess-status process) output errors))

(test-case "CLI defaults to gate semantics and rejects an in-progress milestone"
  (define-values (status output errors)
    (run-truth-cli "--digest" independently-supplied-current-digest (path->string current-fixture)))
  (check-not-equal? status 0 errors)
  (define result-json (strict-json-read output))
  (check-true (hash-ref result-json "digest-matches"))
  (check-equal? (hash-ref (hash-ref result-json "release-mechanics") "derived") "planned")
  (check-equal? (hash-ref (hash-ref result-json "substantive-acceptance") "effective") "in-progress"))

(test-case "CLI report-only explicitly permits inspection of pinned rejected history"
  (define-values (status output errors)
    (run-truth-cli "--report-only"
                   "--digest"
                   historical-fixture-digest
                   (path->string historical-fixture)))
  (check-equal? status 0 errors)
  (check-equal? (hash-ref (hash-ref (strict-json-read output) "substantive-acceptance") "effective")
                "rejected"))

(test-case "CLI gate accepts consistent accepted truth and rejects digest mismatch"
  (define accepted-document (make-document))
  (define accepted-digest (canonical-json-sha256 accepted-document))
  (define temporary-file (make-temporary-file "milestone-truth-~a.json"))
  (dynamic-wind void
                (lambda ()
                  (call-with-output-file
                   temporary-file
                   #:exists 'truncate
                   (lambda (output) (write-bytes (canonical-json-bytes accepted-document) output)))
                  (define-values (accepted-status _accepted-output accepted-errors)
                    (run-truth-cli "--digest" accepted-digest (path->string temporary-file)))
                  (check-equal? accepted-status 0 accepted-errors)
                  (define-values (mismatch-status _mismatch-output mismatch-errors)
                    (run-truth-cli "--digest" (make-string 64 #\0) (path->string temporary-file)))
                  (check-not-equal? mismatch-status 0 mismatch-errors))
                (lambda () (delete-file temporary-file))))

(test-case "CLI rejects missing digest and documents report-only in usage"
  (define-values (missing-status _missing-output missing-errors)
    (run-truth-cli (path->string current-fixture)))
  (check-not-equal? missing-status 0)
  (define-values (help-status help-output help-errors) (run-truth-cli "--help"))
  (check-equal? help-status 0 help-errors)
  (check-true (string-contains? (string-append help-output help-errors) "--report-only"))
  (check-true (string-contains? missing-errors "--digest")))
