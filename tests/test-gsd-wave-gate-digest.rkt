#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; Tests for the digest side of the W2 evidence-integrity contract: the gate's
;; actual-content-digest comparison (fail-closed), and the excluded-evidence
;; digest computation of scripts/gsd-evidence-bind.rkt — determinism, the
;; excluded-dirs contract, the EMPTY_SHA publication property, and the
;; verify/bind verdicts (register F2, both directions).

(require racket/file
         racket/list
         racket/port
         rackunit)

(require (prefix-in gate: (file "../scripts/gsd-wave-gate.rkt"))
         (file "../scripts/gsd-evidence-bind.rkt")
         (file "../tests/helpers/w2-mini-git-repo.rkt"))

(define (hex-of n char)
  (make-string n char))

(test-case "empty digest constant matches the empty input hash"
  (check-equal? empty-digest "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  (check-true (and (regexp-match? #px"^[0-9a-f]{64}$" empty-digest) #t)))

(define-values (repo base head)
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define r (make-mini-repo! "digest"))
    (define b (mini-commit-file! r "src/one.rkt" "#lang racket/base\n" "base"))
    (define h (mini-commit-file! r "src/two.rkt" "#lang racket/base\n(define x 1)\n" "wave"))
    (values r b h)))

(test-case "F2 digest computation: deterministic and source-sensitive"
  (define d1 (excluded-evidence-digest repo base head))
  (check-true (and (regexp-match? #px"^[0-9a-f]{64}$" d1) #t))
  (check-equal? d1 (excluded-evidence-digest repo base head))
  (check-not-equal? d1 empty-digest))

(test-case "F2 excluded-dirs contract: evidence-only changes digest to EMPTY_SHA"
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define evidence-only
      (mini-commit-file! repo
                         "docs/reports/gsd-wave-evidence/v9.9.9-w2.rktd"
                         "#hasheq((schema-version . 2) (status . \"ready-for-merge\"))\n"
                         "evidence"))
    ;; the range whose only change is the excluded evidence file digests to
    ;; the empty-input hash — the publication property
    (check-equal? (excluded-evidence-digest repo head evidence-only) empty-digest)))

(test-case "F2 verify: computed digest accepted; authored digest refused"
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define root (make-temporary-file "q-w2-verify-~a" 'directory))
    (define evidence-path (build-path root "evidence.rktd"))
    (display-to-file (format "#hasheq((schema-version . 2) (content-digest . ~s))\n"
                             (excluded-evidence-digest repo base head))
                     evidence-path
                     #:exists 'replace)
    (check-true (string-prefix? (verify-records repo base head (list evidence-path)) "digest-ok")
                "control: the computed digest verifies")
    (define authored "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
    (define bad (build-path root "bad.rktd"))
    (display-to-file (format "#hasheq((schema-version . 2) (content-digest . ~s))\n" authored)
                     bad
                     #:exists 'replace)
    (define verdict (verify-records repo base head (list bad)))
    (check-true (string-prefix? verdict "digest-mismatch") "defect: authored digest refused")
    (check-true (string-contains? verdict authored) "the refusal names the authored value")
    (check-true (string-contains? verdict (excluded-evidence-digest repo base head))
                "the refusal names the computed value")))

(test-case "F2 verify: fail-closed on missing and malformed digests"
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define root (make-temporary-file "q-w2-malformed-~a" 'directory))
    (define no-digest (build-path root "no-digest.rktd"))
    (display-to-file "#hasheq((schema-version . 2) (status . \"ready-for-merge\"))\n"
                     no-digest
                     #:exists 'replace)
    (check-true (string-prefix? (verify-records repo base head (list no-digest)) "malformed-digest")
                "a record without a content-digest is refused")
    (define bad-shape (build-path root "bad-shape.rktd"))
    (display-to-file "#hasheq((schema-version . 2) (content-digest . \"PENDING\"))\n"
                     bad-shape
                     #:exists 'replace)
    (check-true (string-prefix? (verify-records repo base head (list bad-shape)) "malformed-digest")
                "a sentinel digest is malformed, fail-closed")))

(test-case "F2 bind: the tool writes the computed digest (the sanctioned author)"
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define root (make-temporary-file "q-w2-bind-~a" 'directory))
    (define target (build-path root "evidence.rktd"))
    (display-to-file "#hasheq((schema-version . 2) (status . \"ready-for-merge\"))\n"
                     target
                     #:exists 'replace)
    (define computed (bind-records! repo base head (list target)))
    (check-equal? computed (excluded-evidence-digest repo base head))
    (define datum (call-with-input-file target read))
    (check-equal? (hash-ref datum 'content-digest) computed)
    (check-true (string-prefix? (verify-records repo base head (list target)) "digest-ok")
                "the bound record verifies")))

(test-case "F4 record-commit: mixed record commit refused with paths; pure passes"
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define mixed (make-mini-repo! "mixed"))
    (define b (mini-commit-file! mixed "src/one.rkt" "one\n" "base"))
    ;; the W4 incident shape: the record authored TOGETHER with source/README
    ;; changes in one commit
    (make-directory* (build-path mixed "docs/reports/gsd-wave-evidence"))
    (display-to-file "#hasheq((schema-version . 2))\n"
                     (build-path mixed "docs/reports/gsd-wave-evidence/v9.9.9-w2.rktd")
                     #:exists 'replace)
    (display-to-file "touched\n" (build-path mixed "README.md") #:exists 'replace)
    (mini-git! mixed "add" "-A" ".")
    (mini-git! mixed "commit" "-q" "-m" "mixed record commit")
    (define h (mini-git! mixed "rev-parse" "HEAD"))
    (define verdict (record-commit-purity mixed b h))
    (check-true (string-prefix? verdict "impure-record-commit") "the mixed commit is refused")
    (check-true (string-contains? verdict "README.md") "the offending paths are named")
    (check-true (string-contains? verdict (substring h 0 12)) "the offending commit is named"))
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define pure (make-mini-repo! "pure"))
    (define b (mini-commit-file! pure "src/one.rkt" "one\n" "base"))
    (mini-commit-file! pure
                       "docs/reports/gsd-wave-evidence/v9.9.9-w2.rktd"
                       "#hasheq((schema-version . 2))\n"
                       "evidence-only record commit")
    (check-equal? (record-commit-purity pure b (mini-git! pure "rev-parse" "HEAD")) "pure")))

(test-case "gate: the caller-supplied digest is compared, fail-closed"
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define root (make-temporary-file "q-w2-gatedigest-~a" 'directory))
    (define evidence-path (build-path root "evidence.rktd"))
    (define digest (excluded-evidence-digest repo base head))
    (display-to-file (format "#hasheq((schema-version . 2) (content-digest . ~s))\n" digest)
                     evidence-path
                     #:exists 'replace)
    ;; an incorrect caller-supplied digest is refused even on an otherwise
    ;; minimal record; the full pass direction is covered in
    ;; tests/test-gsd-wave-gate.rkt with a complete trio
    (define bad
      (gate:validate-wave-evidence (call-with-input-file evidence-path read)
                                   #:root root
                                   #:actual-content-digest empty-digest))
    (check-false (gate:wave-evidence-result-passed? bad))
    (check-true (and (findf (lambda (r) (string-contains? r "actual changed-content digest"))
                            (gate:wave-evidence-result-reasons bad))
                     #t)
                (string-join (gate:wave-evidence-result-reasons bad) "; "))))
