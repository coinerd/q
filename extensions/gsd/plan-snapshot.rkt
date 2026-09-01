#lang racket/base

;; plan-snapshot.rkt — v1.00.24 W3 (BUG-0052): immutable campaign plan snapshots.
;;
;; At campaign start the orchestrator captures PLAN.md and every referenced
;; wave document into .planning/campaigns/<campaign-id>/plan-snapshot/ via a
;; temporary sibling directory and an atomic rename, guarded by a manifest
;; (schema-version, campaign id, created-at, per-file path/size/sha256, plan-id).
;;
;; Invariants:
;;  - absence of a referenced wave document is a typed failure, never the SHA
;;    of the empty string;
;;  - a partial snapshot (interrupted write) is never accepted: readers require
;;    the manifest and verify every file hash; stale tmp directories are swept;
;;  - resume may restore from the snapshot when live .planning drifts or
;;    disappears; drift is always explicit (snapshot-drift? never silent).

(require racket/file
         racket/format
         racket/match
         racket/path
         racket/port
         racket/string
         (only-in racket/list first)
         (only-in "../../util/json/checksum.rkt" sha256-string sha256-file))

(provide plan-snapshot-manifest?
         plan-snapshot-manifest-schema-version
         plan-snapshot-manifest-campaign
         plan-snapshot-manifest-created-at
         plan-snapshot-manifest-plan-id
         plan-snapshot-manifest-files
         snapshot-file-path
         snapshot-file-size
         snapshot-file-sha256
         snapshot-dir
         make-plan-snapshot!
         seed-and-bind-plan-snapshot!
         load-snapshot-manifest
         snapshot-drift?
         restore-plan-from-snapshot!
         plan-references->wave-doc-paths
         exn:fail:gsd-missing-wave-doc
         exn:fail:gsd-missing-wave-doc-path)

(struct exn:fail:gsd-missing-wave-doc exn:fail (path)
  #:transparent
  #:extra-constructor-name make-exn:fail:gsd-missing-wave-doc)

(struct snapshot-file (path size sha256) #:transparent)
(struct plan-snapshot-manifest
  (schema-version campaign created-at plan-id files) #:transparent)

;; ---------------------------------------------------------------------------
;; Wave document reference extraction (decoupled from campaign-state to avoid
;; a require cycle; campaign-state requires this module).
;; ---------------------------------------------------------------------------

(define plan-wave-ref-rx
  #rx"waves/W([0-9]+)-([a-z0-9][a-z0-9-]*)\\.md")

;; Extract canonical relative wave doc paths ("waves/W<idx>-<slug>.md")
;; referenced by a plan text, in first-reference order, deduplicated.
(define (plan-references->wave-doc-paths plan-text)
  (for/fold ([seen (hash)] [acc '()]
             #:result (reverse acc))
            ([m (in-list (regexp-match* plan-wave-ref-rx plan-text
                                        #:match-select values))])
    (define rel (first (regexp-match plan-wave-ref-rx (first m))))
    (if (hash-has-key? seen rel)
        (values seen acc)
        (values (hash-set seen rel #t) (cons rel acc)))))

;; ---------------------------------------------------------------------------
;; Snapshot creation — atomic via tmp sibling + rename
;; ---------------------------------------------------------------------------

(define (snapshot-dir base-dir campaign-id)
  (build-path base-dir ".planning" "campaigns" campaign-id "plan-snapshot"))

(define (snapshot-manifest-path base-dir campaign-id)
  (build-path (snapshot-dir base-dir campaign-id) "snapshot-manifest.rktd"))

(define wave-doc-status-header-rx #rx"^# Wave [0-9]+\nStatus: [^\n]+\n\n")

;; v0.99.90 W5 semantics: the "Status:" header of a wave document is mutable
;; campaign projection state. Snapshot hashes and drift checks therefore hash
;; the stripped body, matching wave-doc-content-hash in campaign-state.rkt;
;; raw bytes are still captured and restored verbatim.
(define (strip-wave-doc-status text)
  (define m (regexp-match wave-doc-status-header-rx text))
  (if m
      (substring text (string-length (car m)))
      text))

(define (normalized-file-hash rel-path raw-text)
  (string-sha256-hex
   (if (string-prefix? rel-path "waves/")
       (strip-wave-doc-status raw-text)
       raw-text)))

(define (file-sha256-hex path) (sha256-file path))

(define (string-sha256-hex s) (sha256-string s))

(define (sweep-stale-tmp-dirs! snapshot-parent)
  (for ([p (in-list (directory-list snapshot-parent #:build? #t))]
        #:when (and (directory-exists? p)
                    (regexp-match? #rx"plan-snapshot\\.tmp-" (path->string p))))
    (delete-directory/files p)))

;; Create the immutable snapshot for a campaign. Fails with
;; exn:fail:gsd-missing-wave-doc if any referenced wave document is absent,
;; before any durable snapshot state is written. Returns the manifest.
(define (make-plan-snapshot! base-dir campaign-id plan-text
                             #:plan-id [plan-id ""] #:now-ms [now-ms #f])
  (define wave-paths (plan-references->wave-doc-paths plan-text))
  ;; 1. Read every referenced doc BEFORE touching the filesystem target.
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (unless (file-exists? plan-path)
    (raise (make-exn:fail:gsd-missing-wave-doc
            (format "plan-snapshot: PLAN.md missing under ~a" base-dir)
            (current-continuation-marks)
            "PLAN.md")))
  (define plan-bytes (file->bytes plan-path))
  (define wave-bytes
    (for/list ([rel (in-list wave-paths)])
      (define p (build-path base-dir ".planning" rel))
      (unless (file-exists? p)
        (raise (make-exn:fail:gsd-missing-wave-doc
                (format "plan-snapshot: referenced wave document missing: ~a" rel)
                (current-continuation-marks)
                rel)))
      (cons rel (file->bytes p))))
  (define files
    (append
     (list (snapshot-file "PLAN.md"
                          (bytes-length plan-bytes)
                          (normalized-file-hash
                           "PLAN.md" (bytes->string/utf-8 plan-bytes))))
     (for/list ([wb (in-list wave-bytes)])
       (snapshot-file (car wb)
                      (bytes-length (cdr wb))
                      (normalized-file-hash
                       (car wb) (bytes->string/utf-8 (cdr wb)))))))
  (define manifest
    (plan-snapshot-manifest 1 campaign-id
                            (or now-ms (current-inexact-milliseconds))
                            plan-id files))
  ;; 2. Write into a temporary sibling, then atomically rename.
  (define campaigns-dir (build-path base-dir ".planning" "campaigns" campaign-id))
  (make-directory* campaigns-dir)
  (define tmp (make-temporary-file "plan-snapshot.tmp-~a" #f campaigns-dir))
  ;; make-temporary-file yields a file; the staging area must be a directory.
  (delete-file tmp)
  (make-directory tmp)
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (for ([wb (in-list (cons (cons "PLAN.md" plan-bytes) wave-bytes))])
        (define target (build-path tmp (car wb)))
        (make-directory* (path-only target))
        (call-with-output-file target
          (lambda (out) (write-bytes (cdr wb) out)) #:exists 'truncate))
      (call-with-output-file (build-path tmp "snapshot-manifest.rktd")
        (lambda (out) (write (manifest->datum manifest) out))
        #:exists 'truncate)
      ;; 3. Atomic swap: any prior snapshot is replaced whole or not at all.
      (define dest (snapshot-dir base-dir campaign-id))
      (when (directory-exists? dest)
        (delete-directory/files dest))
      (rename-file-or-directory tmp dest))
    (lambda ()
      (when (directory-exists? tmp)
        (delete-directory/files tmp))))
  manifest)

;; Seed-and-bind: create the immutable snapshot for a freshly seeded campaign
;; and return (values snapshot-dir-path manifest-sha256). Raises
;; exn:fail:gsd-missing-wave-doc before any durable state is written when a
;; referenced document is absent (BUG-0052 hard-failure semantics).
(define (seed-and-bind-plan-snapshot! base-dir campaign-id)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (define plan-text (file->string plan-path))
  (define m (make-plan-snapshot! base-dir campaign-id plan-text
                                 #:plan-id campaign-id))
  (values (path->string (snapshot-dir base-dir campaign-id))
          (string-sha256-hex (format "~s" (manifest->datum m)))))

(define (manifest->datum m)  `#hasheq((schema-version . ,(plan-snapshot-manifest-schema-version m))
           (campaign . ,(plan-snapshot-manifest-campaign m))
           (created-at . ,(plan-snapshot-manifest-created-at m))
           (plan-id . ,(plan-snapshot-manifest-plan-id m))
           (files .
                  ,(for/list ([f (in-list (plan-snapshot-manifest-files m))])
                     `#hasheq((path . ,(snapshot-file-path f))
                              (size . ,(snapshot-file-size f))
                              (sha256 . ,(snapshot-file-sha256 f)))))))

(define (datum->manifest d)
  (plan-snapshot-manifest
   (hash-ref d 'schema-version)
   (hash-ref d 'campaign)
   (hash-ref d 'created-at)
   (hash-ref d 'plan-id)
   (for/list ([f (in-list (hash-ref d 'files))])
     (snapshot-file (hash-ref f 'path) (hash-ref f 'size) (hash-ref f 'sha256)))))

;; Load and fully verify the on-disk manifest: the manifest must exist and
;; every listed file must be present with the exact recorded size and hash.
;; Returns #f when no snapshot exists (and sweeps stale tmp directories);
;; raises on a corrupt/partial snapshot, which is never accepted.
(define (load-snapshot-manifest base-dir campaign-id)
  (define mp (snapshot-manifest-path base-dir campaign-id))
  (unless (file-exists? mp)
    (sweep-stale-tmp-dirs! (build-path base-dir ".planning" "campaigns" campaign-id)))
  (and (file-exists? mp)
       (let* ([m (datum->manifest (call-with-input-file mp read))])
         ;; Integrity check: every file present, size+normalized-hash exact.
         (for ([f (in-list (plan-snapshot-manifest-files m))])
           (define p (build-path (snapshot-dir base-dir campaign-id)
                                 (snapshot-file-path f)))
           (define broken?
             (or (not (file-exists? p))
                 (let ([size (file-size p)])
                   (or (not (= size (snapshot-file-size f)))
                       (not (equal? (normalized-file-hash
                                     (snapshot-file-path f)
                                     (file->string p))
                                    (snapshot-file-sha256 f)))))))
           (when broken?
             (raise (make-exn:fail
                     (format "plan-snapshot: partial/corrupt snapshot entry ~a for campaign ~a"
                             (snapshot-file-path f) campaign-id)
                     (current-continuation-marks)))))
         m)))

;; Paths of live plan/wave documents that drifted from the snapshot
;; (content changed or file missing). Empty list = no drift.
(define (snapshot-drift? base-dir campaign-id)
  (define m (load-snapshot-manifest base-dir campaign-id))
  (for/list ([f (in-list (plan-snapshot-manifest-files m))])
    (define live (build-path base-dir ".planning" (snapshot-file-path f)))
    (define drifted?
      (or (not (file-exists? live))
          (not (equal? (normalized-file-hash
                        (snapshot-file-path f)
                        (file->string live))
                       (snapshot-file-sha256 f)))))
    (and drifted? (snapshot-file-path f))))

;; Restore live .planning documents from the verified snapshot. Returns the
;; list of restored relative paths. Snapshot itself is never modified.
(define (restore-plan-from-snapshot! base-dir campaign-id)
  (define m (load-snapshot-manifest base-dir campaign-id))
  (for/list ([f (in-list (plan-snapshot-manifest-files m))])
    (define src (build-path (snapshot-dir base-dir campaign-id)
                            (snapshot-file-path f)))
    (define dest (build-path base-dir ".planning" (snapshot-file-path f)))
    (make-directory* (path-only dest))
    (copy-file src dest #:exists? 'replace)
    (snapshot-file-path f)))
