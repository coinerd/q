#lang racket/base

;; @speed slow  ;; @suite default
;; @boundary integration
;; Focused tests for schema-v1 metadata validation (W1).
;; Covers: valid tags pass, each invalid-value class rejected,
;; subprocess alias normalizes to process with a warning,
;; heuristic classification is labeled.

(require rackunit
         racket/file
         racket/list
         racket/string
         (prefix-in meta: (file "../scripts/run-tests/classify-metadata.rkt")))

;; ------------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------------

(define (finding-kinds result)
  (map (lambda (f) (hash-ref f 'kind)) (hash-ref result 'findings)))

(define (finding-codes result)
  (map (lambda (f) (hash-ref f 'code)) (hash-ref result 'findings)))

(define (codes-of-kind result kind)
  (filter values
          (map (lambda (f) (and (eq? (hash-ref f 'kind) kind) (hash-ref f 'code)))
               (hash-ref result 'findings))))

(define scratch-dir (make-temporary-file "classify-meta-test-~a" 'directory))

(define (write-sample name content)
  (define p (build-path scratch-dir name))
  (display-to-file content p #:exists 'replace)
  p)

(define (with-header . tag-lines)
  (string-append "#lang racket/base\n"
                 ";; "
                 (string-join tag-lines "\n;; ")
                 "\n"
                 "(module test racket/base\n  (require rackunit)\n"
                 "  (check-equal? 1 1))\n"))

(define (cleanup!)
  (with-handlers ([exn:fail? void])
    (delete-directory/files scratch-dir)))

;; ------------------------------------------------------------------
;; Schema vocabulary
;; ------------------------------------------------------------------

(test-equal? "schema v1: canonical isolation vocabulary"
             meta:schema-isolation-values
             '("process" "temp-dir" "subprocess"))

(test-equal? "canonical-isolation: subprocess -> process"
             (meta:canonical-isolation "subprocess")
             "process")

(test-equal? "canonical-isolation: process unchanged" (meta:canonical-isolation "process") "process")

(test-not-false "deprecated-isolation-alias?: subprocess"
                (meta:deprecated-isolation-alias? "subprocess"))

(test-false "deprecated-isolation-alias?: process" (meta:deprecated-isolation-alias? "process"))

;; ------------------------------------------------------------------
;; Valid file: explicit classification, no findings
;; ------------------------------------------------------------------

(define valid-file
  (write-sample "valid.rkt"
                (with-header "@suite unit"
                             "@speed fast"
                             "@boundary unit"
                             "@mutates none"
                             "@isolation process"
                             "@timeout 30"
                             "@requires fs")))

(define valid-result (meta:validate-file valid-file))

(test-equal? "valid file: explicit classification" (hash-ref valid-result 'classification) 'explicit)

(test-equal? "valid file: no error findings"
             (filter (lambda (f) (eq? f 'error)) (finding-kinds valid-result))
             '())

(test-equal? "valid file: no warning findings"
             (filter (lambda (f) (eq? f 'warning)) (finding-kinds valid-result))
             '())

(test-equal? "valid file: normalized isolation is canonical"
             (hash-ref (hash-ref valid-result 'normalized) 'isolation)
             "process")

;; ------------------------------------------------------------------
;; Unknown tag rejected
;; ------------------------------------------------------------------

(define unknown-tag-file
  (write-sample "unknown-tag.rkt" (with-header "@suite unit" "@speed fast" "@covres unit")))

(test-not-false "unknown tag is an error"
                (member 'unknown-tag (codes-of-kind (meta:validate-file unknown-tag-file) 'error)))

;; ------------------------------------------------------------------
;; Invalid enum values rejected
;; ------------------------------------------------------------------

(define bad-suite-file (write-sample "bad-suite.rkt" (with-header "@suite nonsense" "@speed fast")))
(test-not-false "invalid suite value is an error"
                (member 'invalid-suite (codes-of-kind (meta:validate-file bad-suite-file) 'error)))

(define bad-speed-file (write-sample "bad-speed.rkt" (with-header "@suite unit" "@speed turbo")))
(test-not-false "invalid speed value is an error"
                (member 'invalid-speed (codes-of-kind (meta:validate-file bad-speed-file) 'error)))

(define bad-boundary-file
  (write-sample "bad-boundary.rkt" (with-header "@suite unit" "@speed fast" "@boundary outer-space")))
(test-not-false "invalid boundary value is an error"
                (member 'invalid-boundary
                        (codes-of-kind (meta:validate-file bad-boundary-file) 'error)))

(define bad-mutates-file
  (write-sample "bad-mutates.rkt" (with-header "@suite unit" "@speed fast" "@mutates everything")))
(test-not-false "invalid mutates value is an error"
                (member 'invalid-mutates
                        (codes-of-kind (meta:validate-file bad-mutates-file) 'error)))

(define bad-isolation-file
  (write-sample "bad-isolation.rkt" (with-header "@suite unit" "@speed fast" "@isolation container")))
(test-not-false "invalid isolation value is an error"
                (member 'invalid-isolation
                        (codes-of-kind (meta:validate-file bad-isolation-file) 'error)))

(define bad-requires-file
  (write-sample "bad-requires.rkt"
                (with-header "@suite unit" "@speed fast" "@requires espresso-machine")))
(test-not-false "unknown requires token is an error"
                (member 'unknown-requires-token
                        (codes-of-kind (meta:validate-file bad-requires-file) 'error)))

;; ------------------------------------------------------------------
;; Malformed timeout rejected
;; ------------------------------------------------------------------

(define bad-timeout-file
  (write-sample "bad-timeout.rkt" (with-header "@suite unit" "@speed fast" "@timeout soon")))
(test-not-false "malformed timeout is an error"
                (member 'malformed-timeout
                        (codes-of-kind (meta:validate-file bad-timeout-file) 'error)))

;; ------------------------------------------------------------------
;; Deprecated alias: subprocess -> process with warning
;; ------------------------------------------------------------------

(define alias-file
  (write-sample "alias.rkt" (with-header "@suite unit" "@speed fast" "@isolation subprocess")))

(define alias-result (meta:validate-file alias-file))

(test-not-false "subprocess alias produces a deprecation warning"
                (member 'deprecated-isolation-alias (codes-of-kind alias-result 'warning)))

(test-not-false "subprocess alias produces no error"
                (not (member 'invalid-isolation (codes-of-kind alias-result 'error))))

(test-equal? "subprocess alias normalizes to process"
             (hash-ref (hash-ref alias-result 'normalized) 'isolation)
             "process")

;; ------------------------------------------------------------------
;; Missing required tags reported
;; ------------------------------------------------------------------

(define no-tags-file
  (write-sample
   "no-tags.rkt"
   "#lang racket/base\n;; no metadata here\n(module test racket/base\n  (require rackunit)\n  (check-equal? 1 1))\n"))

(define no-tags-result (meta:validate-file no-tags-file))

(test-not-false "missing @suite reported"
                (member "suite"
                        (map (lambda (f) (hash-ref f 'tag))
                             (filter (lambda (f) (eq? (hash-ref f 'code) 'missing-required))
                                     (hash-ref no-tags-result 'findings)))))

(test-not-false "missing @speed reported"
                (member "speed"
                        (map (lambda (f) (hash-ref f 'tag))
                             (filter (lambda (f) (eq? (hash-ref f 'code) 'missing-required))
                                     (hash-ref no-tags-result 'findings)))))

;; ------------------------------------------------------------------
;; Heuristic classification is labeled
;; ------------------------------------------------------------------

(test-equal? "no-header file: heuristic classification"
             (hash-ref no-tags-result 'classification)
             'heuristic)

(test-equal? "header file: explicit classification" (hash-ref valid-result 'classification) 'explicit)

;; ------------------------------------------------------------------
;; Reserved tags are not unknown
;; ------------------------------------------------------------------

(define reserved-file
  (write-sample "reserved.rkt" (with-header "@suite unit" "@speed fast" "@covers issue-1234")))

(test-equal? "reserved @covers tag is not an unknown-tag error"
             (filter (lambda (c) (eq? c 'unknown-tag))
                     (finding-codes (meta:validate-file reserved-file)))
             '())

;; ------------------------------------------------------------------
;; Summary aggregation
;; ------------------------------------------------------------------

(define summary
  (meta:summarize-findings (list (meta:validate-file alias-file)
                                 (meta:validate-file bad-suite-file)
                                 (meta:validate-file no-tags-file))))

(test-not-false "summary counts invalid findings" (> (hash-ref summary 'invalid_count) 0))

(test-not-false "summary counts deprecated-alias findings"
                (> (hash-ref summary 'deprecated_alias_count) 0))

(test-not-false "summary counts missing-required findings"
                (> (hash-ref summary 'missing_required_count) 0))

(test-not-false "summary tracks per-area breakdown" (> (hash-count (hash-ref summary 'per_area)) 0))

;; ------------------------------------------------------------------

(cleanup!)
(displayln "classify-metadata tests: all checks passed")
