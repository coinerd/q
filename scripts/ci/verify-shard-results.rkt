#!/usr/bin/env racket
#lang racket/base

;; scripts/ci/verify-shard-results.rkt — Phase D shard-aggregate verifier.
;;
;; Replaces the former shell/jq aggregation inside ci.yml#test-aggregate
;; (a `find` + per-shard `jq -c` projection into artifacts-run/shards.jsonl
;; plus a `jq -s` reduce) with one reviewed Racket verifier that both
;; CHECKS the shard evidence and PRODUCES the aggregate document.
;;
;; Inputs: a directory of downloaded shard result documents (each one a
;; runner `<shard>/test-results.json`, one per-artifact subdirectory), the
;; matrix-derived expected shard count, the run head SHA, and the expected
;; required-check names.
;;
;; Verdict logic (all violations are reported; the exit code is the
;; highest-precedence class present):
;;
;;   (a) shard presence — every expected shard present exactly once, no
;;       extras: total count, artifact-name uniqueness, shard-index
;;       uniqueness, index coverage of 0..n-1, and the declared shard total.
;;   (b) document schema — every document validates with
;;       scripts/run-tests/reporting.rkt's validate-run-summary-json (the
;;       runner's own run-summary projection validator).
;;   (c) run-SHA binding — every document carries run_summary.run_sha and it
;;       equals the expected head SHA (an unattested shard can never prove
;;       it ran at the claimed commit).
;;   (d) per-file completeness — the files array has exactly
;;       run_summary.file_count records, every path appears at most once
;;       inside a document AND at most once across the whole aggregate (the
;;       runner partitions the inventory, so the aggregate union must be a
;;       set), and the per-file statuses reconcile with the declared
;;       pass/fail/timeout/skip counts.
;;   (e) count self-consistency — per-shard pass+fail+timeout+skip ==
;;       file_count, the aggregate equals the sum of the shards, and the
;;       independently re-derived per-file status totals agree with the
;;       declared shard sums.
;;   (f) zero total tests is a failure, and a non-clean aggregate (zero
;;       files, any fail, timeout or skip) refuses to produce evidence at
;;       all.
;;   (g) required-check accounting — every expected required-check name
;;       (--required-check NAME, repeatable; --require-shard-checks PREFIX
;;       derives PREFIX (0..n-1) from the expected shard count) is
;;       accounted for by a shard result, and every shard maps to an
;;       expected required check. A shard's check name is
;;       run_summary.check_name when present, else
;;       --shard-check-prefix (default "test") + " (index)".
;;
;; Exit codes (fail closed):
;;   0  verified: clean, complete, bound — outputs were written
;;   1  non-clean aggregate (failures, timeouts, skips, zero files/tests)
;;   2  structural violation (missing/extra/duplicate shard, invalid
;;      document, incomplete or duplicated per-file results, inconsistent
;;      counts)
;;   3  run-SHA binding mismatch
;;   4  required-check accounting gap
;;
;; Outputs (written only on exit 0):
;;   stdout                     the aggregate JSON (machine readable)
;;   --aggregate-out PATH       the same aggregate JSON, byte-identical
;;   --summary-out PATH         the markdown step summary
;;   --counts-out PATH          shell-sourceable KEY=value claim counts
;;   --selected-paths-out PATH  the executed-selection manifest (one JSON
;;                              string per line, sorted) the claim digests
;;
;; The aggregate document keeps EXACTLY the field names, nesting and values
;; the jq reduce produced (run_sha, suite, sharded, shards[], file_count,
;; pass, fail, timeout, skip, wall_clock_seconds, elapsed_ms_basis), so
;; scripts/ci/verify-result-truth.rkt, the claim's result_summary_digest and
;; the proof-bundle writer keep consuming the same shape. Keys are emitted
;; in that same order and each per-shard `shard` object is emitted as
;; {index, total}.
;;
;; Offline and deterministic: reads only the files under --shard-root and the
;; CLI flags; no network, threads, sleeps or clocks.

(require json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/string
         "../run-tests/reporting.rkt")

;; ---------------------------------------------------------------------------
;; Deterministic ordered JSON writer
;; ---------------------------------------------------------------------------
;; Racket's write-json serializes hash tables in unspecified key order, which
;; would make the aggregate document (and therefore the claim's
;; result_summary_digest) differ between runs over identical inputs. Ordered
;; pairs plus a jq-compatible pretty printer keep the evidence stable and
;; keep the field order the jq reduce produced.

(struct jsobj (pairs) #:transparent)

(define (jq-number->string v)
  ;; jq re-serializes numbers minimally: an integral float prints without the
  ;; trailing ".0". Match that so 12.0 and 12 are indistinguishable in output.
  (if (and (inexact? v) (integer? v))
      (number->string (inexact->exact v))
      (number->string v)))

(define (json-write-ordered v out [indent 0])
  (define (pad n) (display (make-string n #\space) out))
  (define (join items render)
    (define last-index (sub1 (length items)))
    (for ([item (in-list items)] [i (in-naturals)])
      (render item)
      (display (if (= i last-index) "\n" ",\n") out)))
  (cond
    [(jsobj? v)
     (define pairs (jsobj-pairs v))
     (cond
       [(null? pairs) (display "{}" out)]
       [else
        (display "{\n" out)
        (join
         pairs
         (lambda (p)
           (pad (+ indent 2))
           (define key (car p))
           (write-json (if (symbol? key) (symbol->string key) key) out)
           (display ": " out)
           (json-write-ordered (cdr p) out (+ indent 2))))
        (pad indent)
        (display "}" out)])]
    [(list? v)
     (cond
       [(null? v) (display "[]" out)]
       [else
        (display "[\n" out)
        (join v (lambda (item) (pad (+ indent 2)) (json-write-ordered item out (+ indent 2))))
        (pad indent)
        (display "]" out)])]
    [(eq? v (json-null)) (display "null" out)]
    [(boolean? v) (display (if v "true" "false") out)]
    [(number? v) (display (jq-number->string v) out)]
    [(string? v) (write-json v out)]
    [else (error 'verify-shard-results "cannot serialize ~s as JSON" v)]))

(define (jsobj->string v)
  (define out (open-output-string))
  (json-write-ordered v out)
  (newline out)
  (get-output-string out))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (path-parameter)
  (make-parameter #f (lambda (v) (if (string? v) (string->path v) v))))

(define shard-root (path-parameter))
(define expect-shards (make-parameter #f))
(define expect-sha (make-parameter #f))
(define suite-name (make-parameter "fast"))
(define required-checks (make-parameter '()))
(define require-shard-checks (make-parameter #f))
(define shard-check-prefix (make-parameter "test"))
(define aggregate-out (path-parameter))
(define summary-out (path-parameter))
(define counts-out (path-parameter))
(define selected-paths-out (path-parameter))

(command-line
 #:program "verify-shard-results"
 #:once-each [("--shard-root")
              p
              "Directory holding the downloaded per-shard result documents (required)"
              (shard-root p)]
 [("--expect-shards")
  n
  "Matrix-derived number of shard results that must be present (required)"
  (expect-shards (string->number n))]
 [("--expect-sha") sha "Run head SHA every shard result must be bound to (required)" (expect-sha sha)]
 [("--suite") s "Suite name recorded in the aggregate (default fast)" (suite-name s)]
 #:multi [("--required-check")
          name
          "A required check name that must be accounted for by the shard results (repeatable)"
          (required-checks (append (required-checks) (list name)))]
 #:once-each [("--require-shard-checks")
              prefix
              "Derive the required check names '<prefix> (<index>)' for every expected shard"
              (require-shard-checks prefix)]
 #:once-each [("--shard-check-prefix")
              prefix
              "Check-name prefix a shard's index maps to (default test)"
              (shard-check-prefix prefix)]
 [("--aggregate-out") p "Write the aggregate JSON to this path" (aggregate-out p)]
 [("--summary-out") p "Write the markdown step summary to this path" (summary-out p)]
 [("--counts-out") p "Write shell-sourceable claim counts to this path" (counts-out p)]
 [("--selected-paths-out")
  p
  "Write the executed-selection path manifest (one JSON string per line) to this path"
  (selected-paths-out p)]
 #:args ()
 (void))

(unless (shard-root)
  (raise-user-error 'verify-shard-results "--shard-root is required"))
(unless (expect-shards)
  (raise-user-error 'verify-shard-results "--expect-shards is required"))
(unless (expect-sha)
  (raise-user-error 'verify-shard-results "--expect-sha is required"))
(unless (exact-positive-integer? (expect-shards))
  (raise-user-error 'verify-shard-results
                    (format "--expect-shards must be a positive integer, got ~s"
                            (expect-shards))))
(unless (directory-exists? (shard-root))
  (raise-user-error 'verify-shard-results
                    (format "--shard-root is not a directory: ~a" (path->string (shard-root)))))

;; ---------------------------------------------------------------------------
;; Violations
;; ---------------------------------------------------------------------------

(struct violation (kind message) #:transparent)

;; Precedence: a structural problem outranks a SHA-binding problem, which
;; outranks a required-check gap, which outranks a non-clean verdict. Every
;; violation is printed regardless; the exit code is the highest class.
(define kind-precedence '(structural sha-binding required-check non-clean))
(define kind-exit-code (hasheq 'structural 2 'sha-binding 3 'required-check 4 'non-clean 1))

(define violations '())

(define (violation! kind message)
  (set! violations (cons (violation kind message) violations)))

(define (violations-in-order) (reverse violations))

(define (verdict-exit-code)
  (for/first ([kind (in-list kind-precedence)]
              #:when (for/or ([v (in-list violations)]) (eq? (violation-kind v) kind)))
    (hash-ref kind-exit-code kind)))

;; ---------------------------------------------------------------------------
;; Small helpers
;; ---------------------------------------------------------------------------

(define (nonneg-integer? v)
  (and (integer? v) (exact? v) (>= v 0)))

(define (artifact-label abs)
  ;; The artifact name is the enclosing per-artifact directory (the same
  ;; `basename $(dirname ...)` the shell producer used). explode-path is
  ;; separator-safe: path-only/file-name-from-path keep the trailing
  ;; directory separator and then return #f for the name.
  (define parts (explode-path (simplify-path abs)))
  (cond
    [(>= (length parts) 2)
     (define name (list-ref (reverse parts) 1))
     (cond
       [(symbol? name) (symbol->string name)]
       [(path? name) (path->string name)]
       [(string? name) name]
       [else (format "~a" name)])]
    [else (path->string abs)]))

(define (count-into! table key)
  (hash-update! table key add1 0))

;; Every key that occurs more than once, with its multiplicity, sorted.
;;
;; This is the same multiset rule the shard planner applies to inventory
;; preservation (`inventory-preserved?`: same length, same membership, no
;; duplicates): the runner partitions the test inventory across shards, so a
;; path may appear exactly once inside a document and once across the whole
;; aggregate.
(define (duplicates keys)
  (define counts (make-hash))
  (for ([k (in-list keys)]) (count-into! counts k))
  (for/list ([k (in-list (sort (hash-keys counts) string<?))] #:when (> (hash-ref counts k) 1))
    (cons k (hash-ref counts k))))

(define (join-or-none items)
  (if (null? items) "none" (string-join items ", ")))

;; ---------------------------------------------------------------------------
;; Document discovery and loading
;; ---------------------------------------------------------------------------

(define shard-dir (simplify-path (shard-root)))

(define (discover-documents root)
  ;; Own deterministic walk (directory-list order is unspecified, so the
  ;; caller sorts; find-files' filter API differs across Racket versions).
  (define (walk dir)
    (append*
     (for/list ([p (in-list (directory-list dir #:build? #t))])
       (cond
         [(directory-exists? p) (walk p)]
         [(regexp-match? #rx"(^|/)test-results[.]json$" (path->string p)) (list p)]
         [else '()]))))
  (sort (for/list ([p (in-list (walk root))]) (simplify-path p)) string<? #:key path->string))

(struct doc (rel abs artifact js ok?) #:transparent)

;; Root-relative display name for messages/artifacts (string-based: the
;; downloaded layout is always a prefix of the walked path).
(define (relative-document-name root abs)
  (define root-str (path->string (simplify-path root)))
  (define abs-str (path->string (simplify-path abs)))
  (define prefix (if (string-suffix? root-str "/") root-str (string-append root-str "/")))
  (if (string-prefix? abs-str prefix)
      (substring abs-str (string-length prefix))
      abs-str))

(define (load-document root abs)
  (define rel (relative-document-name root abs))
  (define raw
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (violation! 'structural
                                   (format "~a: not readable JSON: ~a" rel (exn-message e)))
                       #f)])
      (read-json (open-input-string (file->string abs)))))
  (cond
    [(not (hash? raw))
     (when raw
       (violation! 'structural (format "~a: document is not a JSON object" rel)))
     (doc rel abs (artifact-label abs) raw #f)]
    [else
     ;; (b) the runner's own run-summary validator decides whether the
     ;; document is a well-formed projection at all.
     (define problems (validate-run-summary-json raw))
     (for ([problem (in-list problems)])
       (violation! 'structural
                   (format "~a: invalid run-summary document: ~a" rel problem)))
     (define summary (hash-ref raw 'run_summary #f))
     (unless (hash? summary)
       (violation! 'structural
                   (format "~a: document has no run_summary object (producer envelope required)"
                           rel)))
     (when (hash? summary)
       (for ([field (in-list '(file_count pass fail timeout skip))])
         (define v (hash-ref summary field #f))
         (unless (nonneg-integer? v)
           (violation! 'structural
                       (format "~a: run_summary.~a is not a nonnegative integer (got ~s)"
                               rel
                               field
                               v))))
       (define wall (hash-ref summary 'wall_clock_seconds #f))
       (unless (and (real? wall) (>= wall 0))
         (violation! 'structural
                     (format "~a: run_summary.wall_clock_seconds is not a nonnegative number (got ~s)"
                             rel
                             wall))))
     (define ok? (and (null? problems) (hash? summary)))
     (doc rel abs (artifact-label abs) raw ok?)]))
(define documents
  (for/list ([abs (in-list (discover-documents shard-dir))])
    (load-document shard-dir abs)))

(unless (pair? documents)
  (violation! 'structural (format "no shard results found under ~a" (path->string shard-dir))))

(define expected-shard-count (expect-shards))
(define found-shard-count (length documents))

(define (doc-summary d)
  (and (doc-ok? d) (hash-ref (doc-js d) 'run_summary)))

(define (summary-ref d key [default #f])
  (define summary (doc-summary d))
  (if (hash? summary) (hash-ref summary key default) default))

;; A document's shard identity: run_summary.shard is {index, total} for a
;; sharded run (or JSON null for an unsharded one).
(define (doc-shard-index d)
  (define sh (summary-ref d 'shard #f))
  (and (hash? sh) (hash-ref sh 'index #f)))

;; ---------------------------------------------------------------------------
;; (a) shard presence: exactly the expected shards, once each, no extras
;; ---------------------------------------------------------------------------

(cond
  [(< found-shard-count expected-shard-count)
   (violation! 'structural
               (format "missing shard results: expected ~a shards, found ~a"
                       expected-shard-count
                       found-shard-count))]
  [(> found-shard-count expected-shard-count)
   (violation! 'structural
               (format "unexpected shard results: expected ~a shards, found ~a"
                       expected-shard-count
                       found-shard-count))])

(for ([dupe (in-list (duplicates (map doc-artifact documents)))])
  (violation! 'structural
              (format "duplicate shard result: artifact \"~a\" appears ~a times"
                      (car dupe)
                      (cdr dupe))))

(for ([d (in-list documents)])
  (define sh (summary-ref d 'shard #f))
  (when (doc-summary d)
    (cond
      [(not (hash? sh))
       (violation!
        'structural
        (format
         "~a: shard ~a has no shard identity (run_summary.shard is not an object); a sharded aggregate cannot account for it"
         (doc-rel d)
         (doc-artifact d)))]
      [else
       (define total (hash-ref sh 'total #f))
       (when (and (nonneg-integer? total) (not (= total expected-shard-count)))
         (violation! 'structural
                     (format
                      "~a: shard ~a declares shard total ~a but the expected shard count is ~a"
                      (doc-rel d)
                      (doc-artifact d)
                      total
                      expected-shard-count)))])))

(define indexed-docs
  (filter (lambda (d) (and (doc-summary d) (nonneg-integer? (doc-shard-index d)))) documents))

(define (doc-shard-index-string d)
  (number->string (doc-shard-index d)))

(for ([dupe (in-list (duplicates (map doc-shard-index-string indexed-docs)))])
  (define index (string->number (car dupe)))
  (violation! 'structural
              (format "duplicate shard index ~a: reported by ~a artifacts (~a)"
                      index
                      (cdr dupe)
                      (join-or-none
                       (for/list ([d (in-list indexed-docs)] #:when (= (doc-shard-index d) index))
                         (doc-artifact d))))))

(for ([index (in-range 0 expected-shard-count)]
      #:unless (for/or ([d (in-list indexed-docs)]) (= (doc-shard-index d) index)))
  (violation!
   'structural
   (format "missing shard index ~a: expected a shard result for shard ~a (found indexes ~a)"
           index
           index
           (join-or-none (map doc-shard-index-string indexed-docs)))))

(for ([d (in-list indexed-docs)] #:when (>= (doc-shard-index d) expected-shard-count))
  (violation! 'structural
              (format "shard index ~a reported by ~a is outside the expected range 0..~a"
                      (doc-shard-index d)
                      (doc-artifact d)
                      (sub1 expected-shard-count))))

;; ---------------------------------------------------------------------------
;; (b/c/d/e) per-document checks
;; ---------------------------------------------------------------------------

;; Same derivation reporting.rkt uses for a per-file record: a profile skip is
;; `skip`, exit code 2 is `timeout`, any other nonzero exit is `fail`, and a
;; clean exit is `pass`. Zero parsed tests stay `pass` at file level; the
;; aggregate-level zero-test rule below is what fails the run.
(define (derived-file-status f)
  (define category (hash-ref f 'category #f))
  (define exit-code (hash-ref f 'exit_code #f))
  (cond
    [(equal? category "SKIPPED_BY_PROFILE") "skip"]
    [(and (integer? exit-code) (= exit-code 2)) "timeout"]
    [(and (integer? exit-code) (not (zero? exit-code))) "fail"]
    [else "pass"]))

(define (doc-file-records d)
  (define js (doc-js d))
  (define files (and (hash? js) (hash-ref js 'files '())))
  (if (list? files) files '()))

(define (check-document! d)
  (define rel (doc-rel d))
  (define artifact (doc-artifact d))
  (define summary (doc-summary d))
  (when summary
    ;; (c) run-SHA binding.
    (define head (hash-ref summary 'run_sha #f))
    (cond
      [(not (and (string? head) (positive? (string-length head))))
       (violation! 'sha-binding
                   (format
                    "~a: shard ~a lacks its shard-side run-SHA attestation (run_summary.run_sha)"
                    rel
                    artifact))]
      [(not (equal? head (expect-sha)))
       (violation! 'sha-binding
                   (format "~a: shard ~a ran at ~a but this run is at ~a"
                           rel
                           artifact
                           head
                           (expect-sha)))])
    ;; (e) per-shard totals must be self-consistent.
    (define file-count (hash-ref summary 'file_count #f))
    (define pass (hash-ref summary 'pass #f))
    (define fail (hash-ref summary 'fail #f))
    (define timeout (hash-ref summary 'timeout #f))
    (define skip (hash-ref summary 'skip #f))
    (when (andmap nonneg-integer? (list file-count pass fail timeout skip))
      (define total (+ pass fail timeout skip))
      (unless (= total file-count)
        (violation!
         'structural
         (format "~a: shard ~a totals are invalid: pass+fail+timeout+skip=~a but file_count=~a"
                 rel
                 artifact
                 total
                 file-count))))
    ;; (d) per-file completeness and non-duplication.
    (define files (doc-file-records d))
    (when (nonneg-integer? file-count)
      (unless (= (length files) file-count)
        (violation!
         'structural
         (format "~a: shard ~a files array has ~a entries but run_summary.file_count=~a"
                 rel
                 artifact
                 (length files)
                 file-count))))
    (define paths
      (for/list ([f (in-list files)] #:when (and (hash? f) (string? (hash-ref f 'path #f))))
        (hash-ref f 'path)))
    (for ([dupe (in-list (duplicates paths))])
      (violation! 'structural
                  (format "~a: shard ~a reports duplicate file path \"~a\" (~a entries)"
                          rel
                          artifact
                          (car dupe)
                          (cdr dupe))))
    ;; Per-file statuses must reconcile with the declared shard counts.
    (define statuses
      (for/list ([f (in-list files)] #:when (and (hash? f) (integer? (hash-ref f 'exit_code #f))))
        (define derived (derived-file-status f))
        (define declared (hash-ref f 'status #f))
        (when (and (string? declared) (not (equal? declared derived)))
          (violation!
           'structural
           (format "~a: file ~a declares status ~a but its exit_code/category classify as ~a"
                   rel
                   (hash-ref f 'path "?")
                   declared
                   derived)))
        derived))
    (when (and (andmap nonneg-integer? (list pass fail timeout skip)) (pair? statuses))
      (define derived-pass (count (lambda (s) (equal? s "pass")) statuses))
      (define derived-fail (count (lambda (s) (equal? s "fail")) statuses))
      (define derived-timeout (count (lambda (s) (equal? s "timeout")) statuses))
      (define derived-skip (count (lambda (s) (equal? s "skip")) statuses))
      (unless (and (= derived-pass pass)
                   (= derived-fail fail)
                   (= derived-timeout timeout)
                   (= derived-skip skip))
        (violation!
         'structural
         (format
          "~a: shard ~a per-file status counts (pass=~a fail=~a timeout=~a skip=~a) disagree with run_summary (pass=~a fail=~a timeout=~a skip=~a)"
          rel
          artifact
          derived-pass
          derived-fail
          derived-timeout
          derived-skip
          pass
          fail
          timeout
          skip))))))

(for ([d (in-list documents)]) (check-document! d))

;; (d) cross-shard duplication: the runner partitions the inventory, so a file
;; must be reported by exactly one shard across the whole aggregate.
(define path-owners (make-hash))
(for ([d (in-list documents)])
  (when (doc-summary d)
    (for ([f (in-list (doc-file-records d))] #:when (hash? f))
      (define path (hash-ref f 'path #f))
      (when (string? path)
        (hash-update! path-owners path (lambda (old) (cons (doc-artifact d) old)) '())))))

(for ([path (in-list (sort (hash-keys path-owners) string<?))]
      #:when (> (length (hash-ref path-owners path)) 1))
  (violation! 'structural
              (format "file \"~a\" reported by more than one shard (~a)"
                      path
                      (string-join (remove-duplicates (reverse (hash-ref path-owners path)))
                                   ", "))))

;; ---------------------------------------------------------------------------
;; (g) required-check accounting
;; ---------------------------------------------------------------------------

(define (observed-check-name d)
  (define declared (summary-ref d 'check_name #f))
  (define index (doc-shard-index d))
  (cond
    [(string? declared) declared]
    [(nonneg-integer? index) (format "~a (~a)" (shard-check-prefix) index)]
    [else #f]))

(define derived-required-checks
  (if (require-shard-checks)
      (for/list ([index (in-range 0 expected-shard-count)])
        (format "~a (~a)" (require-shard-checks) index))
      '()))

(define expected-check-names
  (remove-duplicates (append derived-required-checks (required-checks))))

(when (pair? expected-check-names)
  (define observed
    (filter string?
            (for/list ([d (in-list documents)])
              (and (doc-summary d) (observed-check-name d)))))
  (for ([name (in-list expected-check-names)] #:unless (member name observed))
    (violation! 'required-check
                (format "required check \"~a\" is not accounted for by any shard result (observed: ~a)"
                        name
                        (join-or-none observed))))
  ;; The symmetric direction: a shard that maps to a check nobody requires
  ;; would report green without gating anything.
  (for ([d (in-list documents)])
    (define name (and (doc-summary d) (observed-check-name d)))
    (when (and name (not (member name expected-check-names)))
      (violation! 'required-check
                  (format
                   "shard ~a maps to check \"~a\" which is not in the expected required-check list (~a)"
                   (doc-artifact d)
                   name
                   (string-join expected-check-names ", "))))))

;; ---------------------------------------------------------------------------
;; (e/f) aggregate derivation and cleanliness
;; ---------------------------------------------------------------------------

(define (sum-of field)
  (for/sum ([d (in-list documents)])
           (define v (summary-ref d field 0))
           (if (nonneg-integer? v) v 0)))

(define aggregate-file-count (sum-of 'file_count))
(define aggregate-pass (sum-of 'pass))
(define aggregate-fail (sum-of 'fail))
(define aggregate-timeout (sum-of 'timeout))
(define aggregate-skip (sum-of 'skip))
(define aggregate-wall-clock
  (for/fold ([max-so-far 0]) ([d (in-list documents)])
    (define v (summary-ref d 'wall_clock_seconds 0))
    (if (and (real? v) (> v max-so-far)) v max-so-far)))

(define (all-file-records)
  (for/list ([d (in-list documents)]
             #:when (doc-summary d)
             [f (in-list (doc-file-records d))])
    f))

(define (sum-file-field field)
  (for/sum ([f (in-list (all-file-records))])
           (define v (and (hash? f) (hash-ref f field #f)))
           (if (nonneg-integer? v) v 0)))

(define aggregate-tests-total (sum-file-field 'total))
(define aggregate-tests-passed (sum-file-field 'passed))
(define aggregate-tests-failed (sum-file-field 'failed))

;; (e) the aggregate must equal the sum of the shards and be internally
;; self-consistent. The first is true by construction, so it is checked
;; independently: the per-file status totals across every document must
;; reproduce the same four numbers as the declared per-shard sums. These
;; aggregate-level checks only add information when every document already
;; passed its own structural checks (otherwise they would restate them).
(when (null? violations)
  (define derived-status (make-hash))
  (for ([f (in-list (all-file-records))]
        #:when (and (hash? f) (integer? (hash-ref f 'exit_code #f))))
    (count-into! derived-status (derived-file-status f)))
  (define derived-totals
    (for/list ([status (in-list '("pass" "fail" "timeout" "skip"))])
      (hash-ref derived-status status 0)))
  (define declared-totals
    (list aggregate-pass aggregate-fail aggregate-timeout aggregate-skip))
  (unless (equal? derived-totals declared-totals)
    (violation!
     'structural
     (format
      "aggregate counts (pass=~a fail=~a timeout=~a skip=~a) disagree with the per-file status totals (pass=~a fail=~a timeout=~a skip=~a)"
      aggregate-pass
      aggregate-fail
      aggregate-timeout
      aggregate-skip
      (list-ref derived-totals 0)
      (list-ref derived-totals 1)
      (list-ref derived-totals 2)
      (list-ref derived-totals 3)))))

(define aggregate-status-sum (+ aggregate-pass aggregate-fail aggregate-timeout aggregate-skip))

(when (null? violations)
  (unless (= aggregate-status-sum aggregate-file-count)
    (violation! 'structural
                (format "aggregate totals are inconsistent: pass+fail+timeout+skip=~a but file_count=~a"
                        aggregate-status-sum
                        aggregate-file-count)))

  ;; (f) a clean aggregate: real files, zero failures, zero timeouts, zero
  ;; skips (exactly the refusal the shell producer enforced) and tests that ran.
  (when (or (zero? aggregate-file-count)
            (positive? aggregate-fail)
            (positive? aggregate-timeout)
            (positive? aggregate-skip))
    (violation! 'non-clean
                (format
                 "refusing to write a proof bundle over a non-clean aggregate (files=~a fail=~a timeout=~a skip=~a)"
                 aggregate-file-count
                 aggregate-fail
                 aggregate-timeout
                 aggregate-skip)))

  (when (zero? aggregate-tests-total)
    (violation! 'non-clean
                (format
                 "aggregate reports zero total tests (files=~a); a suite that executed no tests cannot be a clean result"
                 aggregate-file-count))))

;; ---------------------------------------------------------------------------
;; Aggregate document (same field names, nesting and values as the jq reduce)
;; ---------------------------------------------------------------------------

(define elapsed-ms-basis
  "max shard wall_clock_seconds; shards execute in parallel so the suite completed when the slowest shard completed")

(define (shard-identity->jsobj d)
  (define sh (summary-ref d 'shard #f))
  (if (hash? sh)
      (jsobj (list (cons 'index (hash-ref sh 'index json-null))
                   (cons 'total (hash-ref sh 'total json-null))))
      (json-null)))

(define (shard-entry->jsobj d)
  (define summary (doc-summary d))
  (jsobj (list (cons 'artifact (doc-artifact d))
               (cons 'shard (shard-identity->jsobj d))
               (cons 'head_sha (hash-ref summary 'run_sha json-null))
               (cons 'file_count (hash-ref summary 'file_count 0))
               (cons 'pass (hash-ref summary 'pass 0))
               (cons 'fail (hash-ref summary 'fail 0))
               (cons 'timeout (hash-ref summary 'timeout 0))
               (cons 'skip (hash-ref summary 'skip 0))
               (cons 'wall_clock_seconds (hash-ref summary 'wall_clock_seconds 0)))))

(define (sorted-documents)
  (sort documents string<? #:key doc-artifact))

(define (aggregate-jsobj)
  (jsobj (list (cons 'run_sha (expect-sha))
               (cons 'suite (suite-name))
               (cons 'sharded #t)
               (cons 'shards (for/list ([d (in-list (sorted-documents))]) (shard-entry->jsobj d)))
               (cons 'file_count aggregate-file-count)
               (cons 'pass aggregate-pass)
               (cons 'fail aggregate-fail)
               (cons 'timeout aggregate-timeout)
               (cons 'skip aggregate-skip)
               (cons 'wall_clock_seconds aggregate-wall-clock)
               (cons 'elapsed_ms_basis elapsed-ms-basis))))

(define (aggregate-elapsed-ms)
  (inexact->exact (floor (* aggregate-wall-clock 1000))))

;; ---------------------------------------------------------------------------
;; Markdown step summary
;; ---------------------------------------------------------------------------

(define (markdown-summary)
  (string-append (format "## Fast-suite aggregate — `test-aggregate` (~a)~n~n" (suite-name))
                 "- Verdict: PASS (verified)\n"
                 (format "- Run SHA: `~a`~n" (expect-sha))
                 (format "- Shards: ~a/~a (~a)~n"
                         found-shard-count
                         expected-shard-count
                         (join-or-none (map doc-artifact (sorted-documents))))
                 (format "- Files: ~a  pass=~a fail=~a timeout=~a skip=~a~n"
                         aggregate-file-count
                         aggregate-pass
                         aggregate-fail
                         aggregate-timeout
                         aggregate-skip)
                 (format "- Tests: ~a total, ~a passed, ~a failed~n"
                         aggregate-tests-total
                         aggregate-tests-passed
                         aggregate-tests-failed)
                 (format "- Wall clock: ~as (max shard)~n"
                         (jq-number->string aggregate-wall-clock))
                 (format "- Required checks accounted for: ~a~n"
                         (join-or-none expected-check-names))
                 (format "- Elapsed basis: ~a~n" elapsed-ms-basis)))

;; ---------------------------------------------------------------------------
;; Outputs
;; ---------------------------------------------------------------------------

(define (write-text! path text)
  (make-parent-directory* path)
  (call-with-output-file path #:exists 'truncate/replace (lambda (out) (display text out))))

(define (counts-text)
  (string-append (format "FILE_TOTAL=~a~n" aggregate-file-count)
                 (format "PASS_TOTAL=~a~n" aggregate-pass)
                 (format "FAIL_TOTAL=~a~n" aggregate-fail)
                 (format "TIMEOUT_TOTAL=~a~n" aggregate-timeout)
                 (format "SKIP_TOTAL=~a~n" aggregate-skip)
                 (format "WALL_CLOCK_SECONDS=~a~n" (jq-number->string aggregate-wall-clock))
                 (format "ELAPSED_MS=~a~n" (aggregate-elapsed-ms))
                 (format "SHARD_COUNT=~a~n" found-shard-count)))

;; The executed-selection manifest the claim digests: the sorted, unique union
;; of every per-file path across all shards, one JSON string per line — the
;; exact bytes the previous `jq -s '[.[].files[].path] | unique | .[]'`
;; projection emitted.
(define (selected-paths-text)
  (define unique
    (sort (remove-duplicates
           (for/list ([f (in-list (all-file-records))]
                      #:when (and (hash? f) (string? (hash-ref f 'path #f))))
             (hash-ref f 'path)))
          string<?))
  (if (null? unique)
      ""
      (string-append (string-join (for/list ([p (in-list unique)])
                                    (with-output-to-string (lambda () (write-json p))))
                                  "\n")
                     "\n")))

;; ---------------------------------------------------------------------------
;; Verdict
;; ---------------------------------------------------------------------------

(define exit-code (or (verdict-exit-code) 0))

(unless (zero? exit-code)
  (for ([v (in-list (violations-in-order))])
    (eprintf "verify-shard-results: ~a: ~a\n" (violation-kind v) (violation-message v)))
  (eprintf "verify-shard-results: verdict rejected (exit ~a)\n" exit-code)
  (exit exit-code))

(define aggregate-text (jsobj->string (aggregate-jsobj)))
(define markdown-text (markdown-summary))

(when (aggregate-out) (write-text! (aggregate-out) aggregate-text))
(when (summary-out) (write-text! (summary-out) markdown-text))
(when (counts-out) (write-text! (counts-out) (counts-text)))
(when (selected-paths-out) (write-text! (selected-paths-out) (selected-paths-text)))

(display aggregate-text)
(unless (summary-out)
  (display markdown-text))

(eprintf
 "verify-shard-results: verdict verified: shards=~a files=~a pass=~a fail=~a timeout=~a skip=~a tests=~a sha=~a\n"
 found-shard-count
 aggregate-file-count
 aggregate-pass
 aggregate-fail
 aggregate-timeout
 aggregate-skip
 aggregate-tests-total
 (expect-sha))
(exit 0)
