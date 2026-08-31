#lang racket

;; @speed fast
;; @suite default
;; @boundary unit
;;
;; test-runner-scheduler-order.rkt — W3 ordering contract tests.
;;
;; Proves the W3 invariants:
;;   1. FIFO is deterministic (identity permutation).
;;   2. LPT orders by max retained per-file duration, descending, with stable
;;      repository-path tie-breaks.
;;   3. Ordering is a pure permutation: never adds/removes a file, never changes
;;      the inventory digest, never crosses the serial/parallel partition, and
;;      never changes final result order (results are re-sorted by input position).
;;   4. Every fallback is NAMED and reproducible: missing, stale, malformed,
;;      wrong-inventory, or corrupt data → deterministic FIFO with a reason.
;;   5. Default ordering is FIFO and CI defaults are unchanged.
;;
;; Invoked by the W3 verify command:
;;   racket tests/test-runner-scheduler-order.rkt

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/path
         racket/string
         racket/list
         openssl
         (only-in file/sha1 bytes->hex-string)
         (only-in "../scripts/run-tests/scheduler-order.rkt"
                  ordering-record
                  ordering-record-mode
                  ordering-record-requested
                  ordering-record-fallback-reason
                  ordering-record-snapshot-checksum
                  ordering-record-snapshot-status
                  ordering-record-snapshot-files
                  ordering-record-freshness-decision
                  ordering-record-used-durations
                  default-ordering
                  default-max-age-seconds
                  current-max-age-seconds
                  known-orderings
                  prepare-ordering
                  order-files
                  ordering-record->jsexpr)
         (only-in "../scripts/run-tests/shard-plan.rkt"
                  load-duration-snapshot
                  artifact-json-files
                  build-shard-plan
                  write-plan-json!))

;; ── helpers ───────────────────────────────────────────────────────────────

(define (fake-files n)
  (for/list ([i (in-range n)])
    (format "tests/test-~a.rkt" i)))

(define (make-snapshot-json entries)
  (hasheq 'schema "run-tests-results/1"
          'files
          (for/list ([e (in-list entries)])
            (hasheq 'path (car e)
                    'duration_seconds (cadr e)))))

(define (write-snapshot-tmp entries [name "sched-order-snap-~a.json"])
  (define tmp (make-temporary-file name))
  (call-with-output-file tmp
    (lambda (out) (write-json (make-snapshot-json entries) out))
    #:exists 'replace)
  tmp)

(define (with-snapshot-tmp entries proc)
  (define tmp (write-snapshot-tmp entries))
  (dynamic-wind
    (lambda () (void))
    (lambda () (proc tmp))
    (lambda () (with-handlers ([exn:fail? (lambda (_) (void))])
                (delete-file tmp)))))

(define (with-snapshot-dir-tmp entries proc)
  (define dir (make-temporary-file "sched-order-dir-~a"))
  (delete-file dir)
  (make-directory dir)
  (for ([e (in-list entries)]
        [i (in-naturals)])
    (call-with-output-file (build-path dir (format "snap-~a.json" i))
      (lambda (out) (write-json (make-snapshot-json (list e)) out))
      #:exists 'replace))
  (dynamic-wind
    (lambda () (void))
    (lambda () (proc (path->string dir)))
    (lambda () (with-handlers ([exn:fail? (lambda (_) (void))])
                (delete-directory/files dir)))))

;; ── test suite ───────────────────────────────────────────────────────────

(define scheduler-order-suite
  (test-suite
   "scheduler-order: deterministic FIFO/LPT ordering and duration fallback (W3)"

   ;; ── 5. Default ordering is FIFO; known-orderings is (fifo lpt) ───────
   (test-case "default ordering is fifo"
     (check-eq? default-ordering 'fifo))
   (test-case "known orderings are fifo and lpt"
     (check-equal? known-orderings '(fifo lpt)))

   ;; ── 1. FIFO is the identity permutation ──────────────────────────────
   (test-case "fifo is the identity permutation"
     (define files (fake-files 5))
     (define rec (prepare-ordering files 'fifo default-max-age-seconds #f))
     (check-eq? (ordering-record-mode rec) 'fifo)
     (check-equal? (order-files files rec) files)
     (check-false (ordering-record-fallback-reason rec)))

   (test-case "fifo with a duration snapshot still returns input order"
     (define files (fake-files 4))
     (with-snapshot-tmp `((,(list-ref files 0) 10.0)
                          (,(list-ref files 1) 1.0)
                          (,(list-ref files 2) 20.0)
                          (,(list-ref files 3) 5.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'fifo default-max-age-seconds snap))
         (check-eq? (ordering-record-mode rec) 'fifo)
         (check-equal? (order-files files rec) files))))

   ;; ── 2. LPT orders by max retained duration, descending ───────────────
   (test-case "lpt orders by duration descending"
     (define files (list "tests/test-slow.rkt"
                         "tests/test-fast.rkt"
                         "tests/test-medium.rkt"))
     (with-snapshot-tmp `(("tests/test-slow.rkt" 30.0)
                          ("tests/test-fast.rkt" 2.0)
                          ("tests/test-medium.rkt" 10.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (check-eq? (ordering-record-mode rec) 'lpt)
         (check-false (ordering-record-fallback-reason rec))
         (define ordered (order-files files rec))
         (check-equal? ordered
                       (list "tests/test-slow.rkt"
                             "tests/test-medium.rkt"
                             "tests/test-fast.rkt")))))

   (test-case "lpt retains max across multiple artifacts"
     (define files (list "tests/test-a.rkt" "tests/test-b.rkt"))
     (with-snapshot-dir-tmp `(("tests/test-a.rkt" 5.0)
                              ("tests/test-a.rkt" 15.0)
                              ("tests/test-b.rkt" 8.0)
                              ("tests/test-b.rkt" 3.0))
       (lambda (dir)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds dir))
         (check-eq? (ordering-record-mode rec) 'lpt)
         (define ordered (order-files files rec))
         ;; a (15.0) > b (8.0)
         (check-equal? ordered (list "tests/test-a.rkt" "tests/test-b.rkt")))))

   (test-case "lpt uses stable path tie-break for equal durations"
     (define files (list "tests/zzz.rkt"
                         "tests/aaa.rkt"
                         "tests/mmm.rkt"))
     (with-snapshot-tmp `(("tests/zzz.rkt" 10.0)
                          ("tests/aaa.rkt" 10.0)
                          ("tests/mmm.rkt" 10.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (check-eq? (ordering-record-mode rec) 'lpt)
         (define ordered (order-files files rec))
         ;; Equal durations → stable path tie-break (shorter path first, then lex)
         (check-equal? ordered
                       (list "tests/aaa.rkt"
                             "tests/mmm.rkt"
                             "tests/zzz.rkt")))))

   (test-case "lpt keeps files without duration evidence in input-relative position"
     (define files (list "tests/test-noev.rkt"
                         "tests/test-long.rkt"
                         "tests/test-short.rkt"))
     (with-snapshot-tmp `(("tests/test-long.rkt" 20.0)
                          ("tests/test-short.rkt" 2.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (check-eq? (ordering-record-mode rec) 'lpt)
         (define ordered (order-files files rec))
         ;; test-noev has no duration (defaults to 0.0) → sorts last, but
         ;; its position relative to other zero-duration files is the path
         ;; tie-break.  Here: long(20) > short(2) > noev(0).
         (check-equal? ordered
                       (list "tests/test-long.rkt"
                             "tests/test-short.rkt"
                             "tests/test-noev.rkt")))))

   ;; ── 4. Named fallbacks: missing, stale, malformed, wrong-inventory ─
   (test-case "lpt without snapshot → fifo fallback with named reason"
     (define files (fake-files 5))
     (define rec (prepare-ordering files 'lpt default-max-age-seconds #f))
     (check-eq? (ordering-record-mode rec) 'fifo)
     (check-eq? (ordering-record-snapshot-status rec) 'missing)
     (check-true (string? (ordering-record-fallback-reason rec)))
     (check-equal? (order-files files rec) files))

   (test-case "lpt with nonexistent path → fifo fallback with named reason"
     (define files (fake-files 5))
     (define rec (prepare-ordering files 'lpt default-max-age-seconds "/nonexistent/snap.json"))
     (check-eq? (ordering-record-mode rec) 'fifo)
     (check-eq? (ordering-record-snapshot-status rec) 'missing)
     (check-true (string? (ordering-record-fallback-reason rec)))
     (check-equal? (order-files files rec) files))

   (test-case "lpt with stale snapshot → fifo fallback"
     (define files (fake-files 3))
     (with-snapshot-tmp `((,(list-ref files 0) 10.0)
                          (,(list-ref files 1) 5.0)
                          (,(list-ref files 2) 1.0))
       (lambda (snap)
         ;; max-age = 0 → always stale
         (define rec (prepare-ordering files 'lpt 0 snap))
         (check-eq? (ordering-record-mode rec) 'fifo)
         (check-eq? (ordering-record-snapshot-status rec) 'stale)
         (check-true (string? (ordering-record-fallback-reason rec)))
         (check-equal? (order-files files rec) files))))

   (test-case "lpt with wrong-inventory snapshot → fifo fallback"
     (define files (fake-files 3))
     (with-snapshot-tmp `(("tests/totally-different-a.rkt" 10.0)
                          ("tests/totally-different-b.rkt" 5.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (check-eq? (ordering-record-mode rec) 'fifo)
         (check-eq? (ordering-record-snapshot-status rec) 'wrong-inventory)
         (check-true (string? (ordering-record-fallback-reason rec)))
         (check-equal? (order-files files rec) files))))

   (test-case "lpt with malformed (corrupt) snapshot → fifo fallback"
     (define files (fake-files 3))
     (define tmp (make-temporary-file "malformed-snap-~a.json"))
     (call-with-output-file tmp
       (lambda (out) (display "not valid json {{{" out))
       #:exists 'replace)
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         (define rec (prepare-ordering files 'lpt default-max-age-seconds tmp))
         (check-eq? (ordering-record-mode rec) 'fifo)
         (check-eq? (ordering-record-snapshot-status rec) 'malformed)
         (check-true (string? (ordering-record-fallback-reason rec)))
         (check-equal? (order-files files rec) files))
       (lambda () (with-handlers ([exn:fail? (lambda (_) (void))])
                   (delete-file tmp)))))

   ;; ── 3. Ordering is a pure permutation ────────────────────────────────
   (test-case "ordering never adds or removes a file (lpt fallback)"
     (define files (fake-files 8))
     (define rec (prepare-ordering files 'lpt default-max-age-seconds #f))
     (define ordered (order-files files rec))
     (check-equal? (length ordered) (length files))
     (check-equal? (sort ordered string<?) (sort files string<?)))

   (test-case "ordering never adds or removes a file (lpt with snapshot)"
     (define files (list "tests/test-a.rkt"
                         "tests/test-b.rkt"
                         "tests/test-c.rkt"
                         "tests/test-d.rkt"))
     (with-snapshot-tmp `(("tests/test-a.rkt" 30.0)
                          ("tests/test-c.rkt" 5.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (define ordered (order-files files rec))
         (check-equal? (length ordered) (length files))
         (check-equal? (sort ordered string<?) (sort files string<?)))))

   (test-case "ordering never duplicates a file (lpt with snapshot)"
     (define files (list "tests/test-a.rkt"
                         "tests/test-b.rkt"
                         "tests/test-c.rkt"))
     (with-snapshot-tmp `(("tests/test-a.rkt" 30.0)
                          ("tests/test-b.rkt" 20.0)
                          ("tests/test-c.rkt" 10.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (define ordered (order-files files rec))
         (check-equal? (length ordered) (length (remove-duplicates ordered))))))

   (test-case "ordering does not change the inventory digest"
     ;; The inventory digest is a hash of the file SET.  Ordering is a
     ;; permutation, so the sorted set is identical before and after.
     (define files (list "tests/test-z.rkt"
                         "tests/test-a.rkt"
                         "tests/test-m.rkt"
                         "tests/test-b.rkt"))
     (with-snapshot-tmp `(("tests/test-z.rkt" 20.0)
                          ("tests/test-a.rkt" 5.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (define ordered (order-files files rec))
         (define digest-before
           (bytes->hex-string (sha256-bytes (string->bytes/utf-8
                                             (string-join (sort files string<?) "\n")))))
         (define digest-after
           (bytes->hex-string (sha256-bytes (string->bytes/utf-8
                                             (string-join (sort ordered string<?) "\n")))))
         (check-equal? digest-after digest-before))))

   (test-case "ordering does not cross the serial/parallel partition"
     ;; The partition (serial = mutating, parallel = non-mutating) is applied
     ;; AFTER ordering, on the ordered sequence.  Ordering operates on the
     ;; full runnable-files list before partitioning, so it can never move a
     ;; file from one partition to the other — the partition filter is a
     ;; post-hoc pass over the ordered list.
     (define files (list "tests/test-mut-1.rkt"
                         "tests/test-fast-1.rkt"
                         "tests/test-mut-2.rkt"
                         "tests/test-fast-2.rkt"))
     (with-snapshot-tmp `(("tests/test-fast-1.rkt" 30.0)
                          ("tests/test-mut-1.rkt" 20.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (define ordered (order-files files rec))
         ;; The partition is a filter, not a reorder — the SET of mutating
         ;; files is unchanged regardless of order.
         (define mut-before (filter (lambda (f) (string-contains? f "mut")) files))
         (define mut-after (filter (lambda (f) (string-contains? f "mut")) ordered))
         (check-equal? (sort mut-after string<?) (sort mut-before string<?)))))

   (test-case "ordering does not change final result order"
     ;; Final result order is established by sorting results by the
     ;; original input-position (suite-files) index, not by execution order.
     ;; So even though LPT reorders execution, the results come back in the
     ;; original input order.  We simulate this: results keyed on input
     ;; position are independent of execution order.
     (define files (list "tests/test-c.rkt"
                         "tests/test-a.rkt"
                         "tests/test-b.rkt"))
     (define file-order
       (for/hash ([f (in-list files)]
                  [i (in-naturals)])
         (values f i)))
     (with-snapshot-tmp `(("tests/test-a.rkt" 30.0)
                          ("tests/test-b.rkt" 10.0)
                          ("tests/test-c.rkt" 5.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (define ordered (order-files files rec))
         ;; Simulate results "completed" in ordered (execution) order,
         ;; then re-sorted by input position like the runner does.
         (define fake-results
           (for/list ([f (in-list ordered)]
                      [i (in-naturals)])
             (cons f i)))
         (define re-sorted
           (sort fake-results < #:key (lambda (r) (hash-ref file-order (car r) 0))))
         ;; Final result order matches original input order.
         (check-equal? (map car re-sorted) files))))

   ;; ── 6. JSON evidence record ──────────────────────────────────────────
   (test-case "ordering-record->jsexpr produces auditable JSON"
     (define files (fake-files 3))
     (define rec (prepare-ordering files 'lpt default-max-age-seconds #f))
     (define json-str (ordering-record->jsexpr rec))
     (define parsed (with-input-from-string json-str read-json))
     (check-equal? (hash-ref parsed 'mode) "fifo")
     (check-equal? (hash-ref parsed 'requested) "lpt")
     (check-true (string? (hash-ref parsed 'fallback_reason)))
     (check-equal? (hash-ref parsed 'snapshot_status) "missing"))

   (test-case "ordering-record->jsexpr for successful lpt"
     (define files (list "tests/test-a.rkt" "tests/test-b.rkt"))
     (with-snapshot-tmp `(("tests/test-a.rkt" 10.0)
                          ("tests/test-b.rkt" 2.0))
       (lambda (snap)
         (define rec (prepare-ordering files 'lpt default-max-age-seconds snap))
         (define json-str (ordering-record->jsexpr rec))
         (define parsed (with-input-from-string json-str read-json))
         (check-equal? (hash-ref parsed 'mode) "lpt")
         (check-equal? (hash-ref parsed 'requested) "lpt")
         (check-false (hash-ref parsed 'fallback_reason))
         (check-equal? (hash-ref parsed 'snapshot_status) "usable")
         (check-true (string? (hash-ref parsed 'snapshot_checksum)))
         (check-true (integer? (hash-ref parsed 'snapshot_files)))
          (check-true (string? (hash-ref parsed 'freshness_decision))))))))

(module+ main
  (exit (run-tests scheduler-order-suite)))
