#lang racket

;; scheduler-order.rkt — deterministic file ordering (v1.00.23 W3)
;;
;; Ordering is a SEPARATE contract from scheduler selection: selection decides
;; WHICH files run (and whether they run serial/parallel); ordering decides
;; the START order of the selected files.  Two modes are supported:
;;
;;   fifo  — deterministic input order (the default; preserves today's
;;           behavior exactly, including the serial-mutating-first partition
;;           which happens AFTER ordering, on the ordered sequence).
;;   lpt   — longest-processing-time first: order by the maximum retained
;;           per-file duration, descending, with stable repository-path
;;           tie-breaks.
;;
;; LPT reuses the same duration-snapshot parser/planner that shard-plan.rkt
;; already uses (load-duration-snapshot), so the artifact format is
;; byte-compatible with the planner — no new/incompatible formats are
;; introduced.
;;
;; Fail-safe contract: whenever duration evidence is unusable (missing,
;; stale, malformed, wrong-inventory, or corrupt), ordering falls back to
;; FIFO and records a NAMED reason.  Every fallback is deterministic and
;; reproducible: given the same files, the same snapshot bytes, and the same
;; current time, the same decision is produced.

(provide ordering-record
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

(require racket/string
         racket/match
         racket/list
         racket/file
         openssl
         (only-in file/sha1 bytes->hex-string)
         json)

(require (only-in "shard-plan.rkt" load-duration-snapshot artifact-json-files))

(define default-ordering 'fifo)
(define known-orderings '(fifo lpt))

;; Maximum accepted snapshot age (seconds).  Evidence older than this is
;; "stale" and falls back to FIFO with a named reason.  Config compatibility:
;; Q_TEST_DURATIONS_MAX_AGE_SECONDS overrides the default per environment.
(define default-max-age-seconds 3600)
(define (current-max-age-seconds)
  (define raw (getenv "Q_TEST_DURATIONS_MAX_AGE_SECONDS"))
  (define n (and raw (string->number raw)))
  (if (and n (real? n) (> n 0))
      (exact-floor n)
      default-max-age-seconds))

;; ── ordering record ─────────────────────────────────────────────────────
;; A plain (immutable) struct; the runner embeds the decision in the results
;; JSON via ordering-record->jsexpr so ordering is auditable per run.
(struct ordering-record
  (mode            ; 'fifo | 'lpt — the mode ACTUALLY applied
   requested       ; 'fifo | 'lpt — what the user asked for
   fallback-reason ; #f | string — named reason when mode != requested
   snapshot-checksum    ; string | #f — hex sha256 of the artifact bytes read
   snapshot-status      ; 'usable | 'missing | 'stale | 'malformed | 'wrong-inventory | 'corrupt
   snapshot-files       ; fixnum | #f — artifacts examined (not duration entries)
   freshness-decision   ; string — human-readable decision summary
   used-durations)      ; hash: path-string -> max retained seconds | #f
  #:transparent)

(define (ordering-record->jsexpr rec)
  ;; Deterministic key order.  used-durations is intentionally NOT embedded
  ;; (it can be large); the decision inputs are covered by the checksum +
  ;; status + freshness fields.
  (jsexpr->string
   (hasheq 'mode (symbol->string (ordering-record-mode rec))
           'requested (symbol->string (ordering-record-requested rec))
           'fallback_reason (ordering-record-fallback-reason rec)
           'snapshot_checksum (ordering-record-snapshot-checksum rec)
           'snapshot_status
           (if (ordering-record-snapshot-status rec)
               (symbol->string (ordering-record-snapshot-status rec))
               #f)
           'snapshot_files (ordering-record-snapshot-files rec)
           'freshness_decision (ordering-record-freshness-decision rec))))

;; ── stable repository-path representation (portable across platforms) ───
(define (path->string-repr p)
  (let ([s (if (string? p) p (path->string p))])
    (if (eq? (system-type) 'windows)
        (string-replace s "\\" "/")
        s)))

;; Normalize a path key so evidence and selection spellings agree even when
;; one side is relative and the other absolute.
(define (normalize-path-key p)
  (define s (if (string? p) p (path->string p)))
  (path->string-repr
   (simplify-path (if (absolute-path? s)
                      (string->path s)
                      (path->complete-path (string->path s))))))

;; ── snapshot probing ────────────────────────────────────────────────────
;; Named statuses: 'missing, 'malformed, 'corrupt (unreadable bytes),
;; 'stale (beyond max accepted age), 'wrong-inventory (zero overlap with
;; the current file inventory), 'usable.  Each unusable status carries a
;; deterministic fallback reason.

;; Probe 1: does the source exist and what artifacts does it provide?
;; Reuses shard-plan.rkt's artifact-json-files (identical enumeration order
;; to load-duration-snapshot), so probe and parser always agree.
;; → (values readable? artifact-paths age-seconds reason)
(define (probe-snapshot-readable! source max-age-seconds)
  (define artifacts (artifact-json-files source))
  (cond
    [(null? artifacts)
     (values #f '() #f
             (if source
                 (format "duration snapshot has no readable *.json artifacts: ~a" source)
                 "no duration snapshot (--durations not provided)"))]
    [else
     (define paths (map (lambda (a) (if (path? a) a (string->path a))) artifacts))
     (define ages
       (map (lambda (p)
              (exact-floor (- (current-seconds) (file-or-directory-modify-seconds p))))
            paths))
     (values #t paths (apply min ages) #f)]))

;; Probe 2: checksum of the artifact bytes the parser actually reads.
;; Single file → that file's bytes; directory → the concatenation of its
;; *.json artifacts in sorted order (same enumeration order as the parser).
;; → (values checksum read-ok?)
(define (artifact-checksum artifact-files)
  (define (read-bytes-safe p)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (file->bytes p)))
  (define parts (filter values (map read-bytes-safe artifact-files)))
  (cond
    [(< (length parts) (length artifact-files))
     (values #f #f)]
    [else
     (values (bytes->hex-string (sha256-bytes (apply bytes-append parts))) #t)]))

;; Probe 3: freshness (max accepted age) + inventory compatibility.
(define (probe-snapshot-freshness! durations files age-seconds max-age-seconds)
  (cond
    [(not age-seconds)
     (values 'stale "snapshot modification time unavailable; cannot prove freshness")]
     [(>= age-seconds max-age-seconds)
      (values 'stale (format "snapshot age ~a s exceeds max accepted age ~a s"
                             age-seconds max-age-seconds))]
    [else
     (define selected
       (for/hash ([f (in-list files)])
         (values (normalize-path-key f) #t)))
     (define overlapping
       (for/sum ([(k _) (in-hash durations)])
         (if (hash-ref selected (normalize-path-key k) #f) 1 0)))
     (cond
       [(and (> (hash-count durations) 0) (= overlapping 0))
        (values 'wrong-inventory
                "snapshot duration entries do not overlap the current file inventory")]
       [else
        (values 'usable #f)])]))

;; ── prepare-ordering ────────────────────────────────────────────────────
;; Pure (side effects limited to reading the snapshot artifacts): builds the
;; ordering decision for `files`.  `max-age-seconds` is the maximum accepted
;; snapshot age; `duration-source` is a path (string) or #f.
(define (prepare-ordering files requested max-age-seconds duration-source)
  (define-values (readable? artifact-files age-seconds read-reason)
    (probe-snapshot-readable! duration-source max-age-seconds))
  (define-values (durations parser-status)
    (if readable?
        (load-duration-snapshot duration-source)
        (values #f #f)))
  (define-values (checksum checksum-ok?)
    (if readable?
        (artifact-checksum artifact-files)
        (values #f #f)))
  (define snapshot-status
    (cond
      [(not readable?) 'missing]
      [(eq? parser-status 'corrupt) 'malformed]
      [(not checksum-ok?) 'corrupt]
      [(eq? parser-status 'missing) 'missing]
      [else
       (define-values (freshness-status _reason)
         (probe-snapshot-freshness! durations files age-seconds max-age-seconds))
       freshness-status]))
  (define reason
    (if (eq? requested 'lpt)
        (cond
          [(not readable?) read-reason]
          [(eq? snapshot-status 'malformed) "snapshot artifact is unreadable or has an invalid shape"]
          [(eq? snapshot-status 'corrupt) "snapshot artifact bytes could not be read (I/O error)"]
          [(eq? snapshot-status 'stale)
           (format "snapshot age ~a s exceeds max accepted age ~a s" age-seconds max-age-seconds)]
          [(eq? snapshot-status 'wrong-inventory)
           "snapshot duration entries do not overlap the current file inventory"]
          [else #f])
        #f))
  (define usable? (eq? snapshot-status 'usable))
  (define used-durations
    (if usable?
        (retained-max-durations durations files)
        #f))
  (define mode (if (and (eq? requested 'lpt) usable?) 'lpt 'fifo))
  (define n-artifacts (and readable? (length artifact-files)))
  (define decision
    (cond
      [(not (eq? requested 'lpt)) "ordering fifo requested"]
      [(eq? mode 'lpt)
       (format "lpt applied from ~a artifacts (checksum ~a, max age ~a s, ~a duration entr~a)"
               n-artifacts
               checksum
               max-age-seconds
               (hash-count durations)
               (if (= (hash-count durations) 1) "y" "ies"))]
      [else
       (format "fifo fallback (~a)" reason)]))
  (ordering-record mode requested reason checksum snapshot-status n-artifacts
                   decision used-durations))

;; Retain the MAX duration per file (not the latest, not the mean): queue
;; workers start historically-long files first, so the retained value is the
;; worst-case evidence for each file.  Only entries whose repository path is
;; among the selected `files` are retained; absent entries are simply not
;; used (LPT still applies to the files that HAVE evidence; the rest keep
;; their input-relative position via the stable path tie-break).
(define (retained-max-durations durations files)
  (define selected
    (for/hash ([f (in-list files)])
      (values (normalize-path-key f) #t)))
  (define result (make-hash))
  (hash-for-each durations
                 (lambda (k dur-seconds)
                   (define key (normalize-path-key k))
                   (when (hash-ref selected key #f)
                     (hash-update! result key
                                   (lambda (old) (max old dur-seconds))
                                   dur-seconds))))
  result)

;; ── order-files ─────────────────────────────────────────────────────────
;; Pure permutation: `order-files` returns a permutation of `files` — same
;; length, same membership, no duplicates.  The serial/parallel partition
;; happens AFTER ordering (in the runner), so ordering can never move a file
;; across the partition.  FIFO is the identity permutation.
(define (order-files files rec)
  (define mode (ordering-record-mode rec))
  (cond
    [(eq? mode 'fifo) files]
    [(eq? mode 'lpt)
     (define used (ordering-record-used-durations rec))
     (define indexed
       (for/list ([f (in-list files)]
                  [i (in-naturals)])
         (list f i (hash-ref used (normalize-path-key f) 0.0))))
     (define sorted
       (sort indexed
             (lambda (a b)
               (define dur-a (caddr a))
               (define dur-b (caddr b))
               (define path-a (path->string-repr (car a)))
               (define path-b (path->string-repr (car b)))
               (cond
                 [(> dur-a dur-b) #t]
                 [(< dur-a dur-b) #f]
                 [else
                  ;; Stable repository-path tie-break: shorter path first,
                  ;; then lexicographic, then original input index (total
                  ;; determinism for every input, including duplicates).
                  (cond
                    [(< (string-length path-a) (string-length path-b)) #t]
                    [(> (string-length path-a) (string-length path-b)) #f]
                    [(string<? path-a path-b) #t]
                    [(string>? path-a path-b) #f]
                    [else (< (cadr a) (cadr b))])]))
             #:key identity))
     (map car sorted)]
    [else files]))

(module+ test
  ;; Self-tests for the pure permutation + fallback contract (the dedicated
  ;; integration tests live in tests/test-runner-scheduler-order.rkt).
  (require rackunit)
  (define (fake-files n)
    (for/list ([i (in-range n)])
      (format "tests/test-~a.rkt" i)))
  (test-case "fifo is the identity permutation"
    (define files (fake-files 5))
    (define rec (prepare-ordering files 'fifo 7 #f))
    (check-eq? (ordering-record-mode rec) 'fifo)
    (check-equal? (order-files files rec) files)
    (check-equal? (ordering-record-fallback-reason rec) #f))
  (test-case "lpt without snapshot falls back to fifo with named reason"
    (define files (fake-files 5))
    (define rec (prepare-ordering files 'lpt 7 #f))
    (check-eq? (ordering-record-mode rec) 'fifo)
    (check-equal? (order-files files rec) files)
    (check-true (string? (ordering-record-fallback-reason rec)))
    (check-eq? (ordering-record-snapshot-status rec) 'missing))
  (test-case "ordering is a pure permutation"
    (define files (fake-files 8))
    (define rec (prepare-ordering files 'lpt 7 "/nonexistent/snapshot.json"))
    (define ordered (order-files files rec))
    (check-equal? (length ordered) (length files))
    (check-equal? (sort ordered string<?)
                  (sort files string<?))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (snapshot mode . files)
   (define rec (prepare-ordering files (string->symbol mode) 7 snapshot))
   (for ([f (in-list (order-files files rec))])
     (displayln f))))
