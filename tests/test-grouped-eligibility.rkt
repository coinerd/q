#lang racket/base

;; tests/test-grouped-eligibility.rkt — v1.00.28 W4 contract tests
;;
;; Covers the production grouped-execution eligibility layer:
;;   1. strict classification parser (tests/metadata/classification format),
;;   2. parity-matrix parsing + passing/completeness predicates,
;;   3. fail-closed decide-eligibility: unclassified → subprocess,
;;      classified-ISOLATED → subprocess, parity missing/failed → subprocess,
;;   4. the rollback switch: any rollback revokes grouped entirely,
;;   5. group-safe share reporting (file % AND work-mass %).
;;
;; Run: racket tests/test-grouped-eligibility.rkt

(require racket/base
         racket/file
         racket/string
         json
         rackunit
         (prefix-in ge: "../scripts/run-tests/grouped-eligibility.rkt"))

;; ── fixtures ────────────────────────────────────────────────────────────

(define CLASSIFICATION-HEADER
  "path|suite|speed|boundary|isolation|mutates|side-effect|decision|mass-ms|reason")

;; No make-temp-directory/make-temp-file in this runtime — build one under
;; the system temp dir with a time+random suffix.
(define tmpdir
  (let loop ()
    (define d
      (build-path (find-system-path 'temp-dir)
                  (format "w4-eligibility-~a-~a" (current-milliseconds) (random 4294967087))))
    (with-handlers ([exn:fail:filesystem? (lambda _ (loop))])
      (make-directory* d)
      d)))

(define (row->line r)
  (string-join (map (lambda (x)
                      (if (number? x)
                          (number->string x)
                          x))
                    r)
               "|"))

(define (write-lines! path lines)
  (with-output-to-file path
                       (lambda ()
                         (for ([l (in-list lines)])
                           (displayln l)))
                       #:exists 'truncate/replace))

(define (write-json! path h)
  (with-output-to-file path (lambda () (write-json h)) #:exists 'truncate/replace))

(define (write-classification! lines)
  (define p (build-path tmpdir (format "classification-~a.rkt" (random 1000000))))
  (write-lines! p (cons CLASSIFICATION-HEADER lines))
  p)

;; One GROUP-SAFE candidate with complete, passing parity evidence across all
;; five characterization modes.
(define (complete-parity-cells path)
  (for/list ([mode (ge:parity-modes)])
    (ge:parity-cell path (symbol->string mode) #t 0 100 100 #t #t #f)))

;; ── 1. classification parser ────────────────────────────────────────────

(test-case "parse-classification: happy path"
  (define p
    (write-classification! (list (row->line '("tests/test-alpha.rkt" "fast"
                                                                     "fast"
                                                                     "process"
                                                                     "@isolation"
                                                                     "no"
                                                                     "no"
                                                                     "GROUP-SAFE"
                                                                     "1500"
                                                                     "pure unit test"))
                                 ";; a comment line stays out"
                                 ""
                                 (row->line '("tests/test-beta.rkt" "fast"
                                                                    "fast"
                                                                    "process"
                                                                    "@mutates"
                                                                    "yes"
                                                                    "no"
                                                                    "ISOLATED"
                                                                    "700"
                                                                    "process-global mutation")))))
  (define rows (ge:parse-classification p))
  (check-equal? (length rows) 2)
  (check-equal? (ge:classification-row-path (car rows)) "tests/test-alpha.rkt")
  (check-equal? (ge:classification-row-decision (car rows)) "GROUP-SAFE")
  (check-equal? (ge:classification-row-mass-ms (car rows)) 1500)
  (check-equal? (ge:classification-row-mass-ms (cadr rows)) 700))

(test-case "parse-classification: missing header fails loudly"
  (define p (build-path tmpdir "no-header.rkt"))
  (write-lines! p (list "path|suite|decision|mass-ms" "a|fast|GROUP-SAFE|1"))
  (check-exn exn:fail? (lambda () (ge:parse-classification p))))

(test-case "parse-classification: wrong column count fails loudly"
  (define p (write-classification! (list "tests/test-gamma.rkt|fast|GROUP-SAFE|100")))
  (check-exn exn:fail? (lambda () (ge:parse-classification p))))

(test-case "parse-classification: unknown decision token fails loudly"
  (define p
    (write-classification! (list (row->line '("tests/test-delta.rkt" "fast"
                                                                     "fast"
                                                                     "process"
                                                                     "@isolation"
                                                                     "no"
                                                                     "no"
                                                                     "MAYBE"
                                                                     "100"
                                                                     "hedged row")))))
  (check-exn exn:fail? (lambda () (ge:parse-classification p))))

(test-case "parse-classification: negative mass fails loudly"
  (define p
    (write-classification! (list (row->line '("tests/test-eps.rkt" "fast"
                                                                   "fast"
                                                                   "process"
                                                                   "@isolation"
                                                                   "no"
                                                                   "no"
                                                                   "GROUP-SAFE"
                                                                   "-5"
                                                                   "bad census")))))
  (check-exn exn:fail? (lambda () (ge:parse-classification p))))

;; ── 2. parity matrix ────────────────────────────────────────────────────

(test-case "parity-cell-passing?: isolated cell"
  (check-true (ge:parity-cell-passing? (ge:parity-cell "f.rkt" "isolated" #t 0 10 10 #t #t #f)))
  (check-false (ge:parity-cell-passing? (ge:parity-cell "f.rkt" "isolated" #f 1 10 10 #t #t #f)))
  (check-false (ge:parity-cell-passing? (ge:parity-cell "f.rkt" "isolated" #t 1 10 10 #t #t #f)))
  (check-false (ge:parity-cell-passing?
                (ge:parity-cell "f.rkt" "isolated" #t 0 10 10 #t #t (hasheq 'threads 1)))))

(test-case "parity-cell-passing?: grouped cells need stability and order-independence"
  (check-true (ge:parity-cell-passing? (ge:parity-cell "f.rkt" "grouped" #t 0 10 10 #t #t #f)))
  (check-false (ge:parity-cell-passing? (ge:parity-cell "f.rkt" "grouped" #t 0 10 10 #f #t #f))
               "repeat-unstable grouped cell must fail")
  (check-false (ge:parity-cell-passing? (ge:parity-cell "f.rkt" "grouped" #t 0 10 10 #t #f #f))
               "order-dependent grouped cell must fail"))

(test-case "parse-parity-matrix: round trip and unknown mode"
  (define p (build-path tmpdir "parity-ok.json"))
  (write-json! p
               (hasheq 'schema
                       "parity-matrix/v1"
                       'cells
                       (list (hasheq 'path
                                     "tests/test-alpha.rkt"
                                     'mode
                                     "isolated"
                                     'pass
                                     #t
                                     'exit-code
                                     0
                                     'mass-ms
                                     1500
                                     'wall-ms
                                     1200
                                     'repeated-stable
                                     #t
                                     'order-independent
                                     #t
                                     'leaks
                                     #f))))
  (define cells (ge:parse-parity-matrix p))
  (check-equal? (length cells) 1)
  (check-equal? (ge:parity-cell-mode (car cells)) "isolated")
  (check-true (ge:parity-cell-passed? (car cells)))

  (define bad (build-path tmpdir "parity-bad.json"))
  (write-json! bad (hasheq 'cells (list (hasheq 'path "x" 'mode "grouped-teleport"))))
  (check-exn exn:fail? (lambda () (ge:parse-parity-matrix bad))))

(test-case "parity-cells-complete?: complete vs missing vs failed"
  (define path "tests/test-alpha.rkt")
  (check-true (ge:parity-cells-complete? (complete-parity-cells path) path))
  (check-false (ge:parity-cells-complete? (cdr (complete-parity-cells path)) path)
               "missing one of the five modes is incomplete")
  (define broken
    (cons (ge:parity-cell path "grouped-concurrent" #f 1 100 100 #t #t #f)
          (cdr (complete-parity-cells path))))
  (check-false (ge:parity-cells-complete? broken path) "a failed cell is incomplete"))

;; ── 3. fail-closed decisions ────────────────────────────────────────────

(define SAFEROW
  (ge:classification-row "tests/test-alpha.rkt"
                         "fast"
                         "fast"
                         "process"
                         "@isolation"
                         "no"
                         "no"
                         "GROUP-SAFE"
                         1500
                         "pure"))

(define ISOROW
  (ge:classification-row "tests/test-beta.rkt"
                         "fast"
                         "fast"
                         "process"
                         "@mutates"
                         "yes"
                         "no"
                         "ISOLATED"
                         700
                         "process-global mutation"))

(test-case "decide-eligibility: fail-closed defaults"
  (let-values ([(mode reason) (ge:decide-eligibility '() '() "tests/test-alpha.rkt")])
    (check-eq? mode 'subprocess)
    (check-eq? reason 'unclassified-file)
    "an unclassified file defaults to isolated execution")

  (let-values ([(mode reason)
                (ge:decide-eligibility (list SAFEROW ISOROW) '() "tests/test-unknown.rkt")])
    (check-eq? mode 'subprocess)
    (check-eq? reason 'unclassified-file))

  (let-values ([(mode reason)
                (ge:decide-eligibility (list SAFEROW ISOROW) '() "tests/test-beta.rkt")])
    (check-eq? mode 'subprocess)
    (check-eq? reason 'classified-isolated)
    "a classified-ISOLATED family never runs grouped")

  (let-values ([(mode reason) (ge:decide-eligibility (list SAFEROW) '() "tests/test-alpha.rkt")])
    (check-eq? mode 'subprocess)
    (check-eq? reason 'parity-missing)
    "no parity evidence → subprocess (parity-missing)")

  (let-values ([(mode reason) (ge:decide-eligibility
                               (list SAFEROW)
                               (cdr (complete-parity-cells "tests/test-alpha.rkt"))
                               "tests/test-alpha.rkt")])
    (check-eq? mode 'subprocess)
    (check-eq? reason 'parity-missing))

  (let-values ([(mode reason)
                (ge:decide-eligibility
                 (list SAFEROW)
                 (cons (ge:parity-cell "tests/test-alpha.rkt" "grouped-random" #f 1 100 100 #t #f #f)
                       (complete-parity-cells "tests/test-alpha.rkt"))
                 "tests/test-alpha.rkt")])
    (check-eq? mode 'subprocess)
    (check-eq? reason 'parity-failed)
    "any failing parity cell → subprocess (parity-failed)"))

(test-case "decide-eligibility: complete passing evidence grants grouped"
  (let-values ([(mode reason) (ge:decide-eligibility (list SAFEROW)
                                                     (complete-parity-cells "tests/test-alpha.rkt")
                                                     "tests/test-alpha.rkt")])
    (check-eq? mode 'grouped)
    (check-false reason)))

;; ── 4. rollback switch ──────────────────────────────────────────────────

(test-case "rollback switch revokes grouped even with complete evidence"
  (let-values ([(mode reason) (ge:decide-eligibility (list SAFEROW)
                                                     (complete-parity-cells "tests/test-alpha.rkt")
                                                     "tests/test-alpha.rkt"
                                                     #:rollback? #t)])
    (check-eq? mode 'subprocess)
    (check-eq? reason 'rollback-switch)))

;; ── 5. reporting ────────────────────────────────────────────────────────

(test-case "group-safe share: file % and work-mass % are reported separately"
  (define all-rows
    (list SAFEROW
          ISOROW
          (ge:classification-row "tests/test-zeta.rkt"
                                 "fast"
                                 "fast"
                                 "process"
                                 "@isolation"
                                 "no"
                                 "no"
                                 "GROUP-SAFE"
                                 100
                                 "tiny")))
  (define rep (ge:group-safe-report all-rows all-rows))
  (check-equal? (hash-ref rep 'group_safe_files) 2)
  (check-equal? (hash-ref rep 'total_files) 3)
  (check-equal? (hash-ref rep 'group_safe_mass_ms) 1600)
  (check-equal? (hash-ref rep 'total_mass_ms) 2300)
  ;; 2/3 files = 66.67%, but 1600/2300 mass = 69.57% — the numbers diverge.
  (check-equal? (hash-ref rep 'file_percent) (ge:%2 2 3))
  (check-equal? (hash-ref rep 'mass_percent) (ge:%2 1600 2300))
  (check-not-equal? (hash-ref rep 'file_percent) (hash-ref rep 'mass_percent)))

(test-case "%2 guards zero denominator"
  (check-equal? (ge:%2 1 0) 0.0)
  (check-equal? (ge:%2 1 #f) 0.0))

;; Runner-level rollback/fail-closed cases (Q_GROUPED_ROLLBACK, --mode
;; subprocess) live in tests/test-runner-work-queue.rkt (W4 extension).
