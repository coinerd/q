#lang racket

;; scripts/tier-ownership-matrix.rkt
;; W3 (#9591, v1.00.28): Tier-ownership matrix generator + drift check.
;;
;; Scans the tests/ tree for @suite/@speed/@boundary metadata (via
;; scripts/test-metadata.rkt — the same parser the runner uses) and either:
;;   generate — writes tests/tier-ownership-matrix.json from current reality
;;   check    — recomputes reality and diffs it against the committed matrix;
;;              any stale/missing/changed row exits 1 (drift fails closed)
;;
;; The matrix is the drift baseline for tier changes: every retier must land
;; with a regenerated matrix in the same commit, keeping this check green.

(require json
         racket/runtime-path
         "test-metadata.rkt")

(provide matrix-schema-id
         build-matrix-rows
         row->jsexpr
         rows->matrix-doc
         matrix-doc->rows
         matrix-drift
         generate-matrix!
         check-matrix
         default-tests-dir
         matrix-output-path)

(define matrix-schema-id "q.tier-ownership.matrix/1")

(define-runtime-path repo-root "..")

(define default-tests-dir (build-path repo-root "tests"))
(define matrix-output-path (build-path repo-root "tests" "tier-ownership-matrix.json"))

;; ── Scanning ────────────────────────────────────────────────────────────────

(define (all-test-files root)
  ;; Deterministic, sorted list of .rkt files under root (skips compiled/).
  (sort (for*/list ([p (in-directory root)]
                    [s (in-value (path->string (simple-form-path p)))]
                    #:when (and (file-exists? p)
                                (equal? (path-get-extension p) #".rkt")
                                (not (regexp-match? #rx"/compiled(/|$/)" s))))
          (path->string (find-relative-path (simple-form-path repo-root) (simple-form-path p))))
        string<?))

(define (build-matrix-rows [root default-tests-dir])
  ;; One row per test file carrying @suite metadata. Rows without a suite are
  ;; helper modules, not tier-ownership subjects, and are skipped.
  (define metas (scan-files-metadata (all-test-files root)))
  (for/list ([m (in-list metas)]
             #:when (metadata-suite m))
    (row->jsexpr m)))

(define (row->jsexpr m)
  (hasheq 'path
          (metadata-file m)
          'suite
          (metadata-suite m)
          'boundary
          (metadata-boundary m)
          'speed
          (metadata-speed m)))

;; ── Matrix document ─────────────────────────────────────────────────────────

(define (rows->matrix-doc rows)
  (hasheq 'schema
          matrix-schema-id
          ;; No timestamps: the matrix must diff cleanly against itself.
          'rows
          rows))

(define (matrix-doc->rows doc)
  (hash-ref doc 'rows '()))

;; ── Drift ───────────────────────────────────────────────────────────────────

(define (row->key row)
  (list (hash-ref row 'path) (hash-ref row 'suite) (hash-ref row 'boundary) (hash-ref row 'speed)))

(define (matrix-drift computed committed-doc)
  ;; → list of drift strings; '() means the committed matrix matches reality.
  (cond
    [(not (equal? (hash-ref committed-doc 'schema #f) matrix-schema-id))
     (list (format "schema: expected ~a in committed matrix" matrix-schema-id))]
    [else
     (define committed
       (for/hash ([r (in-list (matrix-doc->rows committed-doc))])
         (values (hash-ref r 'path) (row->key r))))
     (define computed-map
       (for/hash ([r (in-list computed)])
         (values (hash-ref r 'path) (row->key r))))
     ;; Stale rows: committed paths no longer matching reality.
     (append (for*/list ([r (in-list (matrix-doc->rows committed-doc))]
                         [path (in-value (hash-ref r 'path))]
                         #:unless (hash-has-key? computed-map path))
               (format "stale row: ~a in committed matrix, absent from reality" path))
             ;; Changed rows.
             (for*/list ([r (in-list (matrix-doc->rows committed-doc))]
                         [path (in-value (hash-ref r 'path))]
                         #:when (hash-has-key? computed-map path)
                         #:unless (equal? (hash-ref committed path) (hash-ref computed-map path)))
               (format "changed row: ~a committed ~a != reality ~a"
                       path
                       (hash-ref committed path)
                       (hash-ref computed-map path)))
             ;; New rows: real test files missing from the committed matrix.
             (for*/list ([r (in-list computed)]
                         [path (in-value (hash-ref r 'path))]
                         #:unless (hash-has-key? committed path))
               (format "missing row: ~a exists in reality but not in committed matrix" path)))]))

;; ── Modes ───────────────────────────────────────────────────────────────────

(define (generate-matrix! [out matrix-output-path])
  (define out-path (simple-form-path out))
  (define doc (rows->matrix-doc (build-matrix-rows)))
  (call-with-output-file out-path (lambda (p) (write-json doc p)) #:exists 'replace)
  (newline)
  (printf "tier-ownership-matrix: wrote ~a rows to ~a~n"
          (length (matrix-doc->rows doc))
          (find-relative-path (simple-form-path (current-directory)) out-path)))

(define (check-matrix [matrix-path matrix-output-path])
  (define committed
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (call-with-input-file matrix-path read-json)))
  (cond
    [(not committed)
     (printf "tier-ownership-matrix: DRIFT — ~a missing or unreadable~n" matrix-path)
     1]
    [else
     (define drift (matrix-drift (build-matrix-rows) committed))
     (cond
       [(null? drift)
        (printf "tier-ownership-matrix: check green (~a rows)~n"
                (length (matrix-doc->rows committed)))
        0]
       [else
        (printf "tier-ownership-matrix: DRIFT (~a)~n" (length drift))
        (for ([d (in-list drift)])
          (printf "  ~a~n" d))
        1])]))

(module+ main
  (define argv (current-command-line-arguments))
  (define mode
    (if (>= (vector-length argv) 1)
        (vector-ref argv 0)
        "check"))
  (case mode
    [("generate") (generate-matrix!)]
    [("check") (exit (check-matrix))]
    [else
     (printf "Usage: racket scripts/tier-ownership-matrix.rkt [generate|check]~n")
     (exit 2)]))
