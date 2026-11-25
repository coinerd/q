#lang racket/base

;; cost-record.rkt — summarise and verify a comprehensive-cost measurement (#9982).
;;
;; What this module does NOT do: it does not decide what "comprehensive" means,
;; does not schedule, and does not execute tests. Those are design decisions and
;; live in docs/reports/CL5-P2-COST-DESIGN.md, which must be independently
;; reviewed before the definition changes. This module only turns the production
;; runner's own artifacts (`--json-out` results, exactly as CI writes them) into a
;; record, and re-derives every summary from the recorded per-file evidence so a
;; record cannot claim a cost its own data does not support.
;;
;; Fail-closed rules (each one is a refusal, never a warning):
;;   - a producer whose declared counts disagree with its recorded file list;
;;   - a declared serial/max/timeout aggregate that does not re-derive;
;;   - a producer claiming a pass while it recorded a timeout;
;;   - a comprehensive total that is not the exact sum of its producer walls;
;;   - duplicate file entries inside one producer run;
;;   - a missing/unknown schema, revision, tree or host descriptor.
;;
;; Evaluation-only.

(require json
         racket/file
         racket/format
         racket/list
         racket/match
         racket/string)

(provide build-record
         verify-record
         record-producer
         record-producer?
         record-comprehensive-wall
         record->jsexpr)

(define record-schema "cl5-cost/1")

;; ---- producer entries -------------------------------------------------------------

(struct record-producer
        (name ; string, e.g. "fast-0"
         adapter ; 'runner | 'workflow-command
         lane ; string or #f (workflow commands have no lane)
         shard ; "0/3" or "none"
         jobs ; exact positive integer
         command ; exact command line, for reproduction
         files ; list of (hash 'path 'elapsed_ms 'status)
         wall-s ; inexact, the measured invocation wall clock
         prep-ms ; exact or #f when the adapter has no preparation phase
         file-count ; declared by the run summary
         pass
         fail
         timeout
         skip
         verdict) ; string as reported
  #:transparent)

;; ---- derivation (single source for a producer's aggregates) -----------------------

(define (elapsed-ms f)
  (hash-ref f 'elapsed_ms))
(define (file-status f)
  (hash-ref f 'status "unknown"))

(define (serial-ms files)
  (for/sum ([f (in-list files)]) (elapsed-ms f)))
(define (max-ms files)
  (if (null? files)
      0
      (apply max (map elapsed-ms files))))
(define (timeout-count files)
  (for/sum ([f (in-list files)]) (if (equal? (file-status f) "timeout") 1 0)))

(define (require-exact-nonneg who v)
  (unless (and (exact-integer? v) (>= v 0))
    (error 'cost-record "~a must be a non-negative integer, got ~s" who v))
  v)

;; ---- building ---------------------------------------------------------------------

;; run-json: the parsed `--json-out` file of one production runner invocation.
;; wall-s: the wall clock of that invocation as measured by the caller.
(define (build-record #:revision rev
                      #:tree tree
                      #:host host
                      #:cache cache
                      #:producers producers
                      #:notes [notes '()])
  (unless (regexp-match? #px"^[0-9a-f]{40}$" rev)
    (error 'build-record "revision must be a 40-hex sha, got ~s" rev))
  (unless (regexp-match? #px"^[0-9a-f]{40}$" tree)
    (error 'build-record "tree must be a 40-hex sha, got ~s" tree))
  (unless (member cache '("warm" "cold") equal?)
    (error 'build-record "cache must be \"warm\" or \"cold\", got ~s" cache))
  (unless (and (hash? host) (hash-ref host 'nproc #f))
    (error 'build-record "host descriptor needs at least nproc"))
  (when (null? producers)
    (error 'build-record "a record needs at least one producer"))
  (define names (map record-producer-name producers))
  (unless (= (length names) (length (remove-duplicates names)))
    (error 'build-record "duplicate producer names: ~s" names))
  (hash 'schema
        record-schema
        'revision
        rev
        'tree
        tree
        'host
        host
        'cache
        cache
        'producers
        (for/list ([p (in-list producers)])
          (producer->jsexpr p))
        'comprehensive
        (hash 'wall_s
              (record-comprehensive-wall producers)
              'producer_count
              (length producers)
              'slot_count
              (for/sum ([p (in-list producers)]) (length (record-producer-files p)))
              'unique_files
              (length (remove-duplicates
                       (append* (map (lambda (p)
                                       (map (lambda (f) (hash-ref f 'path))
                                            (record-producer-files p)))
                                     producers))))
              'status
              (comprehensive-status producers)
              'status_reasons
              (comprehensive-status-reasons producers))
        'notes
        (for/list ([n (in-list notes)])
          (format "~a" n))))

(define (record-comprehensive-wall producers)
  (for/sum ([p (in-list producers)]) (record-producer-wall-s p)))

;; A comprehensive run is only `ok` when every producer finished green with no
;; timeout. The status is derived, never declared: a record cannot call itself
;; ok while its own evidence shows a timeout or a non-pass producer.
(define (comprehensive-status-reasons producers)
  (append*
   (for/list ([p (in-list producers)])
     (define fs (record-producer-files p))
     (append (if (positive? (timeout-count fs))
                 (list (format "~a: ~a timeout(s)" (record-producer-name p) (timeout-count fs)))
                 '())
             (if (equal? (record-producer-verdict p) "pass")
                 '()
                 (list (format "~a: verdict ~a" (record-producer-name p) (record-producer-verdict p))))))))

(define (comprehensive-status producers)
  (if (null? (comprehensive-status-reasons producers)) "ok" "degraded"))

(define (producer->jsexpr p)
  (define files (record-producer-files p))
  (hash 'name
        (record-producer-name p)
        'adapter
        (symbol->string (record-producer-adapter p))
        'lane
        (record-producer-lane p)
        'shard
        (record-producer-shard p)
        'jobs
        (record-producer-jobs p)
        'command
        (record-producer-command p)
        'wall_s
        (record-producer-wall-s p)
        'prep_ms
        (record-producer-prep-ms p)
        'file_count
        (record-producer-file-count p)
        'pass
        (record-producer-pass p)
        'fail
        (record-producer-fail p)
        'timeout
        (record-producer-timeout p)
        'skip
        (record-producer-skip p)
        'verdict
        (record-producer-verdict p)
        'serial_ms
        (serial-ms files)
        'max_ms
        (max-ms files)
        'files
        (for/list ([f (in-list files)])
          (hash 'path (hash-ref f 'path) 'elapsed_ms (elapsed-ms f) 'status (file-status f)))))

;; ---- verification ------------------------------------------------------------------

(define (refuse fmt . args)
  (error 'verify-record (string-append "record refused: " (apply format fmt args))))

(define (verify-record rec)
  (define schema (hash-ref rec 'schema #f))
  (unless (equal? schema record-schema)
    (refuse "unknown schema ~s" schema))
  (define rev (hash-ref rec 'revision #f))
  (unless (and (string? rev) (regexp-match? #px"^[0-9a-f]{40}$" rev))
    (refuse "revision is not a 40-hex sha: ~s" rev))
  (define tree (hash-ref rec 'tree #f))
  (unless (and (string? tree) (regexp-match? #px"^[0-9a-f]{40}$" tree))
    (refuse "tree is not a 40-hex sha: ~s" tree))
  (unless (member (hash-ref rec 'cache #f) '("warm" "cold") equal?)
    (refuse "cache label is ~s (expected \"warm\" or \"cold\")" (hash-ref rec 'cache #f)))
  (define host (hash-ref rec 'host #f))
  (unless (and (hash? host)
               (exact-integer? (hash-ref host 'nproc #f))
               (positive? (hash-ref host 'nproc)))
    (refuse "host descriptor missing a positive integer nproc"))
  (define producers (hash-ref rec 'producers '()))
  (when (null? producers)
    (refuse "no producers recorded"))
  (define names
    (for/list ([p (in-list producers)])
      (hash-ref p 'name #f)))
  (unless (= (length names) (length (remove-duplicates names)))
    (refuse "duplicate producer names: ~s" names))
  (define derived-wall
    (for/sum ([p (in-list producers)])
             (define files (producer-files p))
             (check-producer p files)
             (hash-ref p 'wall_s)))
  (define declared-wall (hash-ref (hash-ref rec 'comprehensive (hash)) 'wall_s #f))
  (unless (and (real? declared-wall) (real? derived-wall) (= declared-wall derived-wall))
    (refuse "comprehensive wall ~s != sum of producer walls ~s" declared-wall derived-wall))
  (define comprehensive (hash-ref rec 'comprehensive))
  (define slot-count (for/sum ([p (in-list producers)]) (hash-ref p 'file_count)))
  (define unique-files
    (length (remove-duplicates (append* (map (lambda (p)
                                               (map (lambda (f) (hash-ref f 'path))
                                                    (producer-files p)))
                                             producers)))))
  (define reasons
    (append*
     (for/list ([p (in-list producers)])
       (define files (producer-files p))
       (append (if (positive? (timeout-count files))
                   (list (format "~a: ~a timeout(s)" (hash-ref p 'name) (timeout-count files)))
                   '())
               (if (equal? (hash-ref p 'verdict #f) "pass")
                   '()
                   (list (format "~a: verdict ~a" (hash-ref p 'name) (hash-ref p 'verdict #f))))))))
  (define status (if (null? reasons) "ok" "degraded"))
  (unless (equal? (hash-ref comprehensive 'slot_count #f) slot-count)
    (refuse "comprehensive slot_count ~s != ~s" (hash-ref comprehensive 'slot_count #f) slot-count))
  (unless (equal? (hash-ref comprehensive 'unique_files #f) unique-files)
    (refuse "comprehensive unique_files ~s != ~s" (hash-ref comprehensive 'unique_files #f) unique-files))
  (unless (equal? (hash-ref comprehensive 'status #f) status)
    (refuse "comprehensive status ~s != re-derived ~s" (hash-ref comprehensive 'status #f) status))
  (unless (equal? (hash-ref comprehensive 'status_reasons '()) reasons)
    (refuse "comprehensive status_reasons ~s != re-derived ~s"
            (hash-ref comprehensive 'status_reasons '()) reasons))
  (define count-declared (hash-ref comprehensive 'producer_count #f))
  (unless (equal? count-declared (length producers))
    (refuse "comprehensive producer_count ~s != ~s recorded" count-declared (length producers)))
  (hash 'schema
        schema
        'revision
        rev
        'producers
        (length producers)
        'comprehensive-wall-s
        derived-wall
        'status
        status
        'files
        (for/sum ([p (in-list producers)]) (hash-ref p 'file_count))
        'serial-ms
        (for/sum ([p (in-list producers)]) (serial-ms (producer-files p)))
        'timeouts
        (for/sum ([p (in-list producers)]) (timeout-count (producer-files p)))))

(define (producer-files p)
  (define fs (hash-ref p 'files '()))
  (for/list ([f (in-list fs)])
    (unless (hash-ref f 'path #f)
      (refuse "producer ~s has a file entry without a path" (hash-ref p 'name)))
    (unless (exact-integer? (hash-ref f 'elapsed_ms #f))
      (refuse "producer ~s file ~s has no integer elapsed_ms" (hash-ref p 'name) (hash-ref f 'path)))
    f))

(define (check-producer p files)
  (define name (hash-ref p 'name))
  (define paths (map (lambda (f) (hash-ref f 'path)) files))
  (unless (= (length paths) (length (remove-duplicates paths)))
    (refuse "producer ~s records a duplicate file entry" name))
  (unless (equal? (length files) (hash-ref p 'file_count #f))
    (refuse "producer ~s declares file_count ~s but records ~s files"
            name
            (hash-ref p 'file_count #f)
            (length files)))
  (unless (equal? (serial-ms files) (hash-ref p 'serial_ms #f))
    (refuse "producer ~s declared serial_ms ~s != re-derived ~s"
            name
            (hash-ref p 'serial_ms #f)
            (serial-ms files)))
  (unless (equal? (max-ms files) (hash-ref p 'max_ms #f))
    (refuse "producer ~s declared max_ms ~s != re-derived ~s"
            name
            (hash-ref p 'max_ms #f)
            (max-ms files)))
  (unless (equal? (timeout-count files) (hash-ref p 'timeout #f))
    (refuse "producer ~s declared ~s timeouts but records ~s"
            name
            (hash-ref p 'timeout #f)
            (timeout-count files)))
  (unless (exact-integer? (hash-ref p 'jobs #f))
    (refuse "producer ~s has no integer jobs" name))
  (unless (positive? (hash-ref p 'jobs))
    (refuse "producer ~s has non-positive jobs" name))
  (unless (real? (hash-ref p 'wall_s #f))
    (refuse "producer ~s has no numeric wall_s" name))
  (when (and (equal? (hash-ref p 'verdict #f) "pass") (positive? (timeout-count files)))
    (refuse "producer ~s claims a pass but records ~s timeout(s)" name (timeout-count files)))
  #t)

(define (record->jsexpr rec)
  rec)
