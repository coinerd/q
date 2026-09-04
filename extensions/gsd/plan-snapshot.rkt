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
;;  - recovery may restore missing live files explicitly; existing authored
;;    content drift is reported and never overwritten; status-only projection
;;    changes are ignored by snapshot-drift?.

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
         exn:fail:gsd-missing-wave-doc-path
         exn:fail:gsd-missing-wave-doc?
         snapshot-manifest-digest
         normalize-wave-doc-content)

(struct exn:fail:gsd-missing-wave-doc exn:fail (path)
  #:transparent
  #:extra-constructor-name make-exn:fail:gsd-missing-wave-doc)

(struct snapshot-file (path size sha256) #:transparent)
(struct plan-snapshot-manifest (schema-version campaign created-at plan-id files) #:transparent)

(define lowercase-hex "0123456789abcdef")
(define canonical-wave-path-rx #rx"^waves/W[0-9]+-[a-z0-9][a-z0-9-]*\\.md$")

(define (sha256-hex? value)
  (and (string? value)
       (= (string-length value) 64)
       (for/and ([ch (in-string value)])
         (string-contains? lowercase-hex (string ch)))))

(define (canonical-snapshot-path? value)
  (and (string? value) (or (equal? value "PLAN.md") (regexp-match? canonical-wave-path-rx value))))

(define (snapshot-fail fmt . args)
  (raise (make-exn:fail (apply format fmt args) (current-continuation-marks))))

(define (snapshot-link? path)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #t)])
    (link-exists? path)))

;; ---------------------------------------------------------------------------
;; Wave document reference extraction (decoupled from campaign-state to avoid
;; a require cycle; campaign-state requires this module).
;; ---------------------------------------------------------------------------

(define plan-wave-ref-rx #rx"waves/W([0-9]+)-([a-z0-9][a-z0-9-]*)\\.md")

;; Extract canonical relative wave doc paths ("waves/W<idx>-<slug>.md")
;; referenced by a plan text, in first-reference order, deduplicated.
(define (plan-references->wave-doc-paths plan-text)
  (for/fold ([seen (hash)]
             [acc '()]
             #:result (reverse acc))
            ([m (in-list (regexp-match* plan-wave-ref-rx plan-text #:match-select values))])
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

(define wave-doc-status-header-rx #rx"^(# Wave [0-9]+\n)Status: [^\n]+\n")
(define plan-index-status-rx #px"(?m:^([-*] +)\\[[A-Za-z-]+\\]( +W[0-9]+:.*)$)")

;; Projection writers may also normalize a terminal newline while changing a
;; status marker. Canonical line joining makes that incidental byte difference
;; status-neutral without hiding authored changes inside the document.
(define (canonical-lines text)
  (string-join (string-split text "\n") "\n"))

;; Status fields in wave headers and PLAN.md index rows are mutable projection
;; state. Snapshot hashes ignore them while retaining every other line, so
;; normal progress is not mistaken for authored-content drift.
(define (normalize-wave-doc-content text)
  (define without-status (regexp-replace wave-doc-status-header-rx text "\\1"))
  (define lines (string-split without-status "\n" #:trim? #f))
  ;; Projection of a headerless authored wave prepends "# Wave / Status" to
  ;; the original document, so removing Status can expose duplicate titles.
  (define first-heading (and (pair? lines) (regexp-match #rx"^# Wave ([0-9]+)(?::.*)?$" (car lines))))
  (define authored-heading
    (and (>= (length lines) 3) (regexp-match #rx"^#+ Wave ([0-9]+)(?::.*)?$" (caddr lines))))
  (define without-generated-prefix
    (if (and first-heading
             authored-heading
             (string=? (cadr lines) "")
             (equal? (cadr first-heading) (cadr authored-heading)))
        (cddr lines)
        lines))
  (define normalized (canonical-lines (string-join without-generated-prefix "\n")))
  ;; Failure projection appends this advisory section; the durable campaign
  ;; record, not the projection text, is authoritative for the reason.
  (define failure-marker (regexp-match-positions #rx"\n+## Last Failure\n" normalized))
  (if failure-marker
      (substring normalized 0 (caar failure-marker))
      normalized))

(define (strip-plan-index-status text)
  (canonical-lines (regexp-replace* plan-index-status-rx text "\\1[STATUS]\\2")))

(define (normalized-file-hash rel-path raw-text)
  (string-sha256-hex (cond
                       [(string-prefix? rel-path "waves/") (normalize-wave-doc-content raw-text)]
                       [(equal? rel-path "PLAN.md") (strip-plan-index-status raw-text)]
                       [else raw-text])))

(define (file-sha256-hex path)
  (sha256-file path))

(define (string-sha256-hex s)
  (sha256-string s))

(define (sweep-stale-tmp-dirs! snapshot-parent)
  (for ([p (in-list (directory-list snapshot-parent #:build? #t))]
        #:when (and (directory-exists? p) (regexp-match? #rx"plan-snapshot\\.tmp-" (path->string p))))
    (delete-directory/files p)))

;; Create the immutable snapshot for a campaign. Fails with
;; exn:fail:gsd-missing-wave-doc if any referenced wave document is absent,
;; before any durable snapshot state is written. Returns the manifest.
(define (make-plan-snapshot! base-dir
                             campaign-id
                             plan-text
                             #:plan-id [plan-id campaign-id]
                             #:now-ms [now-ms #f])
  (unless (and (sha256-hex? campaign-id) (equal? plan-id campaign-id))
    (snapshot-fail "plan-snapshot: campaign and plan identity must match"))
  (define dest (snapshot-dir base-dir campaign-id))
  (when (or (directory-exists? dest) (snapshot-link? dest))
    (snapshot-fail "plan-snapshot: immutable snapshot already exists for ~a" campaign-id))
  (define wave-paths (plan-references->wave-doc-paths plan-text))
  ;; 1. Read every referenced doc BEFORE touching the filesystem target.
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (unless (file-exists? plan-path)
    (raise (make-exn:fail:gsd-missing-wave-doc (format "plan-snapshot: PLAN.md missing under ~a"
                                                       base-dir)
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
    (append (list (snapshot-file "PLAN.md"
                                 (bytes-length plan-bytes)
                                 (normalized-file-hash "PLAN.md" (bytes->string/utf-8 plan-bytes))))
            (for/list ([wb (in-list wave-bytes)])
              (snapshot-file (car wb)
                             (bytes-length (cdr wb))
                             (normalized-file-hash (car wb) (bytes->string/utf-8 (cdr wb)))))))
  (define manifest
    (plan-snapshot-manifest 1 campaign-id (or now-ms (current-inexact-milliseconds)) plan-id files))
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
       (call-with-output-file target (lambda (out) (write-bytes (cdr wb) out)) #:exists 'truncate))
     (call-with-output-file (build-path tmp "snapshot-manifest.rktd")
                            (lambda (out) (write (manifest->datum manifest) out))
                            #:exists 'truncate)
     ;; 3. Publish the create-once immutable snapshot atomically.
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
  (define existing (load-snapshot-manifest base-dir campaign-id))
  (define m
    (or existing
        (let* ([plan-path (build-path base-dir ".planning" "PLAN.md")]
               [plan-text (file->string plan-path)])
          (make-plan-snapshot! base-dir campaign-id plan-text #:plan-id campaign-id))))
  (values (path->string (snapshot-dir base-dir campaign-id)) (snapshot-manifest-digest m)))

(define (manifest->datum m)
  `#hasheq((schema-version . ,(plan-snapshot-manifest-schema-version m))
           (campaign . ,(plan-snapshot-manifest-campaign m))
           (created-at . ,(plan-snapshot-manifest-created-at m))
           (plan-id . ,(plan-snapshot-manifest-plan-id m))
           (files . ,(for/list ([f (in-list (plan-snapshot-manifest-files m))])
                       `#hasheq((path . ,(snapshot-file-path f))
                                (size . ,(snapshot-file-size f))
                                (sha256 . ,(snapshot-file-sha256 f)))))))

(define (snapshot-manifest-digest manifest)
  (string-sha256-hex (format "~s" (manifest->datum manifest))))

(define (datum->manifest d expected-campaign)
  (unless (and (hash? d) (sha256-hex? expected-campaign))
    (snapshot-fail "plan-snapshot: invalid manifest or campaign id"))
  (define schema-version (hash-ref d 'schema-version #f))
  (define campaign (hash-ref d 'campaign #f))
  (define created-at (hash-ref d 'created-at #f))
  (define plan-id (hash-ref d 'plan-id #f))
  (define files-datum (hash-ref d 'files #f))
  (unless (equal? schema-version 1)
    (snapshot-fail "plan-snapshot: unsupported manifest schema ~s" schema-version))
  (unless (and (equal? campaign expected-campaign) (equal? plan-id expected-campaign))
    (snapshot-fail "plan-snapshot: manifest identity mismatch"))
  (unless (real? created-at)
    (snapshot-fail "plan-snapshot: invalid creation timestamp"))
  (unless (and (list? files-datum) (pair? files-datum))
    (snapshot-fail "plan-snapshot: files must be a non-empty list"))
  (define-values (files seen)
    (for/fold ([files '()]
               [seen (hash)])
              ([entry (in-list files-datum)])
      (unless (hash? entry)
        (snapshot-fail "plan-snapshot: file entry must be a hash"))
      (define rel (hash-ref entry 'path #f))
      (define size (hash-ref entry 'size #f))
      (define digest (hash-ref entry 'sha256 #f))
      (unless (canonical-snapshot-path? rel)
        (snapshot-fail "plan-snapshot: unsafe snapshot path ~s" rel))
      (when (hash-has-key? seen rel)
        (snapshot-fail "plan-snapshot: duplicate snapshot path ~s" rel))
      (unless (exact-nonnegative-integer? size)
        (snapshot-fail "plan-snapshot: invalid size for ~a" rel))
      (unless (sha256-hex? digest)
        (snapshot-fail "plan-snapshot: invalid SHA-256 for ~a" rel))
      (values (cons (snapshot-file rel size digest) files) (hash-set seen rel #t))))
  (unless (hash-has-key? seen "PLAN.md")
    (snapshot-fail "plan-snapshot: manifest omits PLAN.md"))
  (plan-snapshot-manifest schema-version campaign created-at plan-id (reverse files)))

;; Load and fully verify the on-disk manifest: the manifest must exist and
;; every listed file must be present with the exact recorded size and hash.
;; Returns #f when no snapshot exists (and sweeps stale tmp directories);
;; raises on a corrupt/partial snapshot, which is never accepted.
(define (load-snapshot-manifest base-dir campaign-id)
  (unless (sha256-hex? campaign-id)
    (snapshot-fail "plan-snapshot: invalid campaign id ~s" campaign-id))
  (define snapshot-root (snapshot-dir base-dir campaign-id))
  (define mp (snapshot-manifest-path base-dir campaign-id))
  (when (or (snapshot-link? snapshot-root) (snapshot-link? mp))
    (snapshot-fail "plan-snapshot: refusing symlinked snapshot boundary"))
  (unless (file-exists? mp)
    (define snapshot-parent (build-path base-dir ".planning" "campaigns" campaign-id))
    (when (directory-exists? snapshot-parent)
      (sweep-stale-tmp-dirs! snapshot-parent)))
  (and (file-exists? mp)
       (let* ([m (datum->manifest (call-with-input-file mp read) campaign-id)])
         ;; Integrity check: every file present, size+normalized-hash exact.
         (for ([f (in-list (plan-snapshot-manifest-files m))])
           (define p (build-path (snapshot-dir base-dir campaign-id) (snapshot-file-path f)))
           (define broken?
             (or (snapshot-link? p)
                 (not (file-exists? p))
                 (let ([size (file-size p)])
                   (or (not (= size (snapshot-file-size f)))
                       (not (equal? (normalized-file-hash (snapshot-file-path f) (file->string p))
                                    (snapshot-file-sha256 f)))))))
           (when broken?
             (raise (make-exn:fail
                     (format "plan-snapshot: partial/corrupt snapshot entry ~a for campaign ~a"
                             (snapshot-file-path f)
                             campaign-id)
                     (current-continuation-marks)))))
         m)))

(define (live-path-has-link? base-dir rel)
  (define planning-root (build-path base-dir ".planning"))
  (define live (build-path planning-root rel))
  (or (snapshot-link? planning-root)
      (and (string-prefix? rel "waves/") (snapshot-link? (build-path planning-root "waves")))
      (snapshot-link? live)))

;; Paths of live plan/wave documents that drifted from the snapshot
;; (content changed, file missing, or untrusted symlink boundary). Empty list
;; means no drift. Symlinks are classified before any content read.
(define (snapshot-drift? base-dir campaign-id)
  (define m (load-snapshot-manifest base-dir campaign-id))
  (for/list ([f (in-list (plan-snapshot-manifest-files m))]
             #:when (let* ([rel (snapshot-file-path f)]
                           [live (build-path base-dir ".planning" rel)])
                      (or (live-path-has-link? base-dir rel)
                          (not (file-exists? live))
                          (not (equal? (normalized-file-hash rel (file->string live))
                                       (snapshot-file-sha256 f))))))
    (snapshot-file-path f)))

;; Restore missing live .planning documents from the verified snapshot.
;; Existing authored-content drift raises before any write; mutable status
;; projections are preserved. Returns restored paths; never modifies snapshot.
(define (restore-plan-from-snapshot! base-dir campaign-id)
  ;; Verify the snapshot and classify all drift before writing anything.
  (define drifted (snapshot-drift? base-dir campaign-id))
  (for ([rel (in-list drifted)])
    (when (live-path-has-link? base-dir rel)
      (snapshot-fail "plan-snapshot: refusing symlinked live restore boundary: ~a" rel)))
  (define existing-drift
    (filter (lambda (rel) (file-exists? (build-path base-dir ".planning" rel))) drifted))
  (unless (null? existing-drift)
    (raise (make-exn:fail (format "plan-snapshot: refusing to overwrite live content drift: ~a"
                                  existing-drift)
                          (current-continuation-marks))))
  ;; Missing files are safe to reconstruct. Existing status projections are
  ;; preserved verbatim rather than reset to capture-time markers.
  (for/list ([rel (in-list drifted)])
    (define src (build-path (snapshot-dir base-dir campaign-id) rel))
    (define dest (build-path base-dir ".planning" rel))
    (make-directory* (path-only dest))
    (copy-file src dest)
    rel))
