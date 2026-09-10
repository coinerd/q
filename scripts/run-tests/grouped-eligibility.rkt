#lang racket/base

;; run-tests/grouped-eligibility.rkt — v1.00.28 W4 production grouped eligibility
;;
;; Owns the production-side grouped-execution evidence layer:
;;   1. a strict parser for tests/metadata/classification (GROUP-SAFE vs ISOLATED
;;      rows for the first production cohort),
;;   2. parity-cell structs and completeness/passedness predicates over
;;      artifacts/test-runtime/v1.00.28-w4/parity-matrix.json
;;      (per-family isolated/grouped/grouped-random/grouped-repeat/grouped-concurrent
;;      cells + leak checks),
;;   3. the fail-closed decide-eligibility decision: any unclassified file,
;;      any classified-ISOLATED file, and any file whose parity evidence is
;;      missing or failed executes SUBPROCESS with a named reason,
;;   4. the Q_GROUPED_ROLLBACK switch: when set (non-empty), grouped eligibility
;;      is revoked entirely and every file runs subprocess ('rollback-switch),
;;   5. group-safe share reporting: file percentage AND work-mass percentage.
;;
;; Defaults are fail-closed in both directions: with no classification file or
;; no parity matrix on disk the decision layer hands everything to subprocess
;; (the pre-W4 behavior), so this module can never silently activate grouped
;; execution for an unproven file.
;;
;; STABILITY: internal — consumed by runner.rkt, the W4 tests, and the
;; evidence scripts; not part of any public API.

(require racket/string
         racket/list
         racket/file
         racket/path
         json
         "sha256.rkt")

(provide (struct-out classification-row)
         (struct-out parity-cell)
         parse-classification
         parse-parity-matrix
         parity-cell-passing?
         parity-cells-complete?
         decide-eligibility
         classification-total-mass-ms
         group-safe-file-count
         group-safe-work-mass-ms
         group-safe-report
         write-sha256sums!
         sha256-hex
         parity-modes
         %2)

;; ── classification table ────────────────────────────────────────────────
;; Fixed pipe-separated columns; strict parser so a mangled row fails loudly
;; instead of degrading to a silent default:
;;   path|suite|speed|boundary|isolation|mutates|side-effect|decision|mass-ms|reason
;; decision is exactly GROUP-SAFE or ISOLATED (uppercase, no whitespace).
(struct classification-row
        (path suite speed boundary isolation mutates side-effect decision mass-ms reason)
  #:transparent)

(define CLASSIFICATION-HEADER
  "path|suite|speed|boundary|isolation|mutates|side-effect|decision|mass-ms|reason")

(define (valid-decision? s)
  (member s '("GROUP-SAFE" "ISOLATED")))

;; Strict parser. Contract: unknown decision tokens and wrong column counts
;; raise exn:fail — the runner must never guess past a malformed row.
(define (parse-classification path)
  (define lines (file->lines path))
  (unless (and (pair? lines) (string=? (string-trim (car lines)) CLASSIFICATION-HEADER))
    (error 'parse-classification "expected header ~a in ~a" CLASSIFICATION-HEADER path))
  (for/list ([line (in-list (cdr lines))]
             #:unless (string=? (string-trim line) "")
             #:unless (string-prefix? (string-trim line) ";;"))
    (define cols (map string-trim (string-split line "|")))
    (unless (= (length cols) 10)
      (error 'parse-classification "expected 10 columns in row: ~a" line))
    (apply classification-row cols)
    ;; decision validated explicitly (member on structs above is by field
    ;; values; keep the check textual so the error names the token)
    (let ([dec (list-ref cols 7)])
      (unless (valid-decision? dec)
        (error 'parse-classification "unknown decision token ~a in ~a" dec path)))
    (let ([m (string->number (list-ref cols 8))])
      (unless (and m (>= m 0))
        (error 'parse-classification "non-numeric or negative mass-ms in ~a" path))
      (classification-row (list-ref cols 0)
                          (list-ref cols 1)
                          (list-ref cols 2)
                          (list-ref cols 3)
                          (list-ref cols 4)
                          (list-ref cols 5)
                          (list-ref cols 6)
                          (list-ref cols 7)
                          m
                          (list-ref cols 9)))))

;; ── parity matrix ───────────────────────────────────────────────────────
;; One cell per (file, mode). Modes are the five characterization
;; configurations from the wave doc. `repeated-stable?` and
;; `order-independent?` are only meaningful for the grouped modes; they are
;; #t for isolated by convention.
(struct parity-cell
        (path mode passed? exit-code mass-ms wall-ms repeated-stable? order-independent? leaks)
  #:transparent)

(define (parity-modes)
  '(isolated grouped grouped-random grouped-repeat grouped-concurrent))

(define MODE-TOKENS '("isolated" "grouped" "grouped-random" "grouped-repeat" "grouped-concurrent"))

;; Parse the JSON matrix into cells. A cell whose mode is missing or failed
;; is parsed as-is — completeness is decided by parity-cells-complete?.
;; Contract: malformed mode tokens raise.
(define (parse-parity-matrix path)
  (define doc (with-input-from-file path read-json))
  (unless (hash? doc)
    (error 'parse-parity-matrix "not a JSON object: ~a" path))
  (define cells (hash-ref doc 'cells #f))
  (unless (list? cells)
    (error 'parse-parity-matrix "missing cells array: ~a" path))
  (for/list ([c (in-list cells)])
    (define mode (hash-ref c 'mode #f))
    (unless (member mode MODE-TOKENS)
      (error 'parse-parity-matrix "unknown parity mode ~a in ~a" mode path))
    (parity-cell (hash-ref c 'path)
                 mode
                 (equal? (hash-ref c 'pass #f) #t)
                 (hash-ref c 'exit-code 1)
                 (hash-ref c 'mass-ms 0)
                 (hash-ref c 'wall-ms 0)
                 (equal? (hash-ref c 'repeated-stable #f) #t)
                 (equal? (hash-ref c 'order-independent #f) #t)
                 (hash-ref c 'leaks #f))))

;; A cell is passing when the run passed, exited 0, shows no leak-check
;; finding, and (for grouped modes) is repeat- and order-stable.
(define (parity-cell-passing? cell)
  (and (parity-cell-passed? cell)
       (equal? (parity-cell-exit-code cell) 0)
       (not (parity-cell-leaks cell))
       (or (string=? (parity-cell-mode cell) "isolated")
           (and (parity-cell-repeated-stable? cell) (parity-cell-order-independent? cell)))))

;; Complete = every one of the five characterization modes has a passing cell
;; for this file. Anything less is incomplete.
(define (parity-cells-complete? cells path)
  (define mine
    (for/list ([c (in-list cells)]
               #:when (string=? (parity-cell-path c) path))
      c))
  (for/and ([mode (in-list (parity-modes))])
    (define tok (symbol->string mode))
    (define cell (findf (lambda (c) (string=? (parity-cell-mode c) tok)) mine))
    (and cell (parity-cell-passing? cell))))

;; ── the decision (fail-closed) ──────────────────────────────────────────
;; (decide-eligibility rows cells rel-path #:rollback?) → (values mode reason)
;;   mode ∈ {'grouped 'subprocess}, reason is a stable symbol or #f.
;;   Order of checks (documented in the wave doc):
;;     1. rollback switch        → subprocess / rollback-switch
;;     2. no classification row  → subprocess / unclassified-file
;;     3. classified ISOLATED    → subprocess / classified-isolated
;;     4. parity cells missing   → subprocess / parity-missing
;;     5. parity cell failed     → subprocess / parity-failed
;;     6. otherwise              → grouped / #f
(define (decide-eligibility rows cells rel-path #:rollback? [rollback? #f])
  (cond
    [rollback? (values 'subprocess 'rollback-switch)]
    [(not (findf (lambda (r) (string=? (classification-row-path r) rel-path)) rows))
     (values 'subprocess 'unclassified-file)]
    [(string=? (classification-row-decision
                (findf (lambda (r) (string=? (classification-row-path r) rel-path)) rows))
               "ISOLATED")
     (values 'subprocess 'classified-isolated)]
    [else
     (define mine
       (for/list ([c (in-list cells)]
                  #:when (string=? (parity-cell-path c) rel-path))
         c))
     (define complete?
       (for/and ([mode (in-list (parity-modes))])
         (define tok (symbol->string mode))
         (define cell (findf (lambda (c) (string=? (parity-cell-mode c) tok)) mine))
         (and cell (parity-cell-passing? cell))))
     (cond
       [complete? (values 'grouped #f)]
       [(null? mine) (values 'subprocess 'parity-missing)]
       [else
        ;; Missing = some required mode has no cell at all (evidence not
        ;; gathered); Failed = every mode covered but at least one cell fails.
        (define covered?
          (for/and ([mode (in-list (parity-modes))])
            (define tok (symbol->string mode))
            (findf (lambda (c) (string=? (parity-cell-mode c) tok)) mine)))
        (if covered?
            (values 'subprocess 'parity-failed)
            (values 'subprocess 'parity-missing))])]))

;; ── group-safe share reporting ──────────────────────────────────────────
;; mass-ms column of GROUP-SAFE rows. Declared masses come from the W0
;; census (median_ms) so file-% and work-mass-% can diverge honestly.
(define (classification-total-mass-ms rows)
  (for/sum ([r (in-list rows)]) (classification-row-mass-ms r)))

(define (group-safe-file-count rows total-files)
  (values (count (lambda (r) (string=? (classification-row-decision r) "GROUP-SAFE")) rows)
          total-files))

(define (group-safe-work-mass-ms rows total-mass)
  (values (for/sum ([r (in-list rows)] #:when (string=? (classification-row-decision r) "GROUP-SAFE"))
                   (classification-row-mass-ms r))
          total-mass))

(define (%2 num den)
  (if (or (not den) (zero? den))
      0.0
      (exact->inexact (* 100.0 (/ num den)))))

;; Report shape written to group-safe-report.json.
(define (group-safe-report rows all-rows)
  (define total-files (length all-rows))
  (define total-mass (classification-total-mass-ms all-rows))
  (define-values (gs-files _) (group-safe-file-count rows total-files))
  (define-values (gs-mass gs-mass-total) (group-safe-work-mass-ms rows total-mass))
  (hasheq 'schema
          "group-safe-report/v1"
          'group_safe_files
          gs-files
          'total_files
          total-files
          'file_percent
          (%2 gs-files total-files)
          'group_safe_mass_ms
          gs-mass
          'total_mass_ms
          total-mass
          'mass_percent
          (%2 gs-mass total-mass)))

;; ── checksums ───────────────────────────────────────────────────────────
;; sha256-hex comes from the repo-local sha256.rkt (runtime ships no
;; file/sha256 collect); it hashes bytes/ports, so read the file.

;; sha256sum -c compatible listing of every file directly under dir
;; (names are cwd-relative as written, matching the GNU verify contract).
(define (write-sha256sums! dir out-path)
  (define files (sort (directory-list dir #:build? #t) path<?))
  (with-output-to-file out-path
                       #:exists 'truncate/replace
                       (lambda ()
                         (for ([f (in-list files)])
                           (when (file-exists? f)
                             (printf "~a  ~a~n"
                                     (sha256-hex (file->bytes f))
                                     (path->string (find-relative-path dir f))))))))
