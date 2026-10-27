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
;;    the manifest and verify every file hash; stale staging directories are
;;    swept ONLY when they carry this module's ownership marker;
;;  - recovery may restore missing live files explicitly; existing authored
;;    content drift is reported and never overwritten; status-only projection
;;    changes are ignored by snapshot-drift?.
;;
;; W4 (GSD-SIMPLIFICATION-PLAN) — snapshot taxonomy, made explicit. A drift
;; class is never collapsed into a smaller ambiguous bucket:
;;
;;   'symlink                  live path crosses the symlink trust boundary.
;;                             NOT overridable, under any flag.
;;   'missing                  live file absent; reconstructible from the
;;                             verified snapshot.
;;   'content-drift            live wave-doc content differs beyond the
;;                             non-semantic projection exclusions.
;;   'frozen-contract-stale    the authored PLAN.md body differs from the
;;                             frozen one; the campaign identity cannot rebind.
;;
;; `missing`, `content-drift` and `frozen-contract-stale` may only be resolved
;; by the ONE existing explicit override (`#:override-existing-drift? #t`) or
;; by an operator decision made outside `/go`. Nothing here ever resnapshots,
;; resets, or substitutes the live plan automatically. A malformed stored
;; manifest/record raises as storage corruption (never a drift class), and
;; author-scope missing/non-literal FILES stays a separate contract class
;; owned by author-scope.rkt.

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/os
         racket/path
         racket/port
         racket/string
         (only-in "../../util/json/checksum.rkt" sha256-string sha256-file)
         (only-in "planning-paths.rkt"
                  campaign-dir
                  campaign-snapshot-dir
                  campaign-snapshot-manifest-file
                  plan-file
                  planning-path
                  planning-root
                  snapshot-manifest-filename
                  wave-doc-reference-rx
                  wave-doc-rel-path?)
         ;; W4: the ONE ownership predicate and the marker filename/kind it
         ;; validates. This module owns the staging marker WRITE; the predicate
         ;; that authorizes its later sweep is owned by
         ;; planning-artifact-ownership.rkt (no cycle: that module reads only).
         (only-in "planning-artifact-ownership.rkt"
                  authentic-planning-artifact-marker?
                  planning-artifact-marker-filename))

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
         classify-snapshot-drift
         restore-plan-from-snapshot!
         plan-references->wave-doc-paths
         exn:fail:gsd-missing-wave-doc
         exn:fail:gsd-missing-wave-doc-path
         exn:fail:gsd-missing-wave-doc?
         exn:fail:gsd-frozen-contract-stale
         exn:fail:gsd-frozen-contract-stale?
         plan-body-normalized-hash
         snapshot-manifest-digest
         normalize-wave-doc-content
         plan-snapshot-staging-marker-filename
         snapshot-drift-classes
         snapshot-drift-class-overridable?
         snapshot-drift-class-description)

(struct exn:fail:gsd-missing-wave-doc exn:fail (path)
  #:transparent
  #:extra-constructor-name make-exn:fail:gsd-missing-wave-doc)

;; F13 (v1.00.31 W5): raised when the authored plan body no longer matches
;; the frozen snapshot a campaign is about to bind to. The freeze is never
;; silently reused and never silently rewritten; the operator must re-freeze
;; under a fresh campaign identity or explicitly restore.
(struct exn:fail:gsd-frozen-contract-stale exn:fail (campaign expected-sha actual-sha)
  #:transparent
  #:extra-constructor-name make-exn:fail:gsd-frozen-contract-stale)

;; Normalized PLAN.md content hash: the shared projection-normalization used
;; by the snapshot files, the campaign manifest identity, and drift checks.
(define (plan-body-normalized-hash text)
  (normalized-file-hash "PLAN.md" text))

(struct snapshot-file (path size sha256) #:transparent)
(struct plan-snapshot-manifest (schema-version campaign created-at plan-id files) #:transparent)

(define lowercase-hex "0123456789abcdef")

(define (sha256-hex? value)
  (and (string? value)
       (= (string-length value) 64)
       (for/and ([ch (in-string value)])
         (string-contains? lowercase-hex (string ch)))))

(define (canonical-snapshot-path? value)
  (and (string? value) (or (equal? value "PLAN.md") (wave-doc-rel-path? value))))

(define (snapshot-fail fmt . args)
  (raise (make-exn:fail (apply format fmt args) (current-continuation-marks))))

(define (snapshot-link? path)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #t)])
    (link-exists? path)))

;; ---------------------------------------------------------------------------
;; Wave document reference extraction (decoupled from campaign-state to avoid
;; a require cycle; campaign-state requires this module).
;; ---------------------------------------------------------------------------

;; Extract canonical relative wave doc paths in first-reference order,
;; deduplicated, using the shared planning-paths grammar.
(define (plan-references->wave-doc-paths plan-text)
  (for/fold ([seen (hash)]
             [acc '()]
             #:result (reverse acc))
            ([rel (in-list (regexp-match* wave-doc-reference-rx plan-text))]
             #:when (and (wave-doc-rel-path? rel) (not (hash-has-key? seen rel))))
    (values (hash-set seen rel #t) (cons rel acc))))

;; ---------------------------------------------------------------------------
;; Snapshot creation — atomic via tmp sibling + rename
;; ---------------------------------------------------------------------------

(define (snapshot-dir base-dir campaign-id)
  (campaign-snapshot-dir base-dir campaign-id))

(define (snapshot-manifest-path base-dir campaign-id)
  (campaign-snapshot-manifest-file base-dir campaign-id))

(define plan-index-status-rx #px"(?m:^([-*] +)\\[[A-Za-z-]+\\]( +W[0-9]+:.*)$)")

;; W4: staging directories are SELF-IDENTIFYING and OWNED. A directory whose
;; name merely looks like staging is NOT ours: only a directory that carries an
;; AUTHENTIC ownership marker (real nofollow file, hash datum, schema-version 1,
;; kind `plan-snapshot-staging` — see planning-artifact-ownership.rkt) may be
;; swept. Unknown `plan-snapshot.tmp-*` directories, impostor markers,
;; `waves-pre-plan-backup`, and `.bak` files are never touched.
(define plan-snapshot-staging-marker-filename
  (planning-artifact-marker-filename 'plan-snapshot-staging))
(define plan-snapshot-staging-prefix "plan-snapshot.tmp-")

(define SNAPSHOT-DRIFT-CLASSES '(symlink missing content-drift frozen-contract-stale))

(define (snapshot-drift-classes)
  SNAPSHOT-DRIFT-CLASSES)

;; The one explicit override may only proceed for overridable classes; the
;; symlink trust boundary never is.
(define (snapshot-drift-class-overridable? kind)
  (and (memq kind SNAPSHOT-DRIFT-CLASSES)
       (not (eq? kind 'symlink))))

(define (snapshot-drift-class-description kind)
  (case kind
    [(symlink)
     "a live path crosses the symlink trust boundary (not overridable; fix the path)"]
    [(missing) "a live bound file is absent; it can be reconstructed from the verified snapshot"]
    [(content-drift)
     "live wave-doc content differs from the frozen snapshot beyond status projections"]
    [(frozen-contract-stale)
     "the authored PLAN.md body differs from the frozen one; the campaign identity cannot rebind"]
    [else (format "unknown snapshot drift class ~s" kind)]))

;; Projection writers may also normalize a terminal newline while changing a
;; status marker. Canonical line joining makes that incidental byte difference
;; status-neutral without hiding authored changes inside the document.
(define (canonical-lines text)
  (string-join (string-split text "\n") "\n"))

;; The managed wave-doc header block is MULTILINE: `# Wave N` + `Status: ...` +
;; a blank separator line. Authored docs frequently carry extra header lines
;; (a legacy second `Status:` marker, an `Attempt:` note, or no blank
;; separator), so the projection's header REWRITE cannot match and it PREPENDS
;; its managed block instead. A leading generated block is therefore projection
;; state — and repeated blocks are the same state after a replayed projection.
(define generated-header-heading-rx #rx"^# Wave [0-9]+(?:[:\u2014\u2013-].*)?$")
(define generated-header-status-rx #rx"^Status:[ \t]*[^\n]*$")

(define (strip-leading-generated-header-blocks lines)
  (let loop ([remaining lines])
    (cond
      [(and (>= (length remaining) 3)
            (regexp-match? generated-header-heading-rx (car remaining))
            (regexp-match? generated-header-status-rx (cadr remaining))
            (string=? (caddr remaining) ""))
       (loop (cdddr remaining))]
      [else remaining])))

;; `## Last Failure` is a projection-owned section (`record-wave-failure!`
;; appends or rewrites it). Normalization ignores THAT SECTION ONLY: the
;; heading through the next `#`/`##` heading. Authored content before or after
;; the section is still compared, so a failed-attempt projection can never hide
;; an authored edit that follows it.
(define failure-section-heading "## Last Failure")

(define (section-heading-level line)
  (define m (regexp-match #rx"^(#+)[ \t]" line))
  (and m (string-length (cadr m))))

(define (drop-failure-section-body lines)
  ;; lines begins just after the failure heading.
  (cond
    [(null? lines) '()]
    [else
     (define level (section-heading-level (car lines)))
     (if (and level (<= level 2))
         lines
         (drop-failure-section-body (cdr lines)))]))

(define (strip-failure-sections lines)
  (let loop ([remaining lines] [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(string=? (string-trim (car remaining)) failure-section-heading)
       (loop (drop-failure-section-body (cdr remaining)) acc)]
      [else (loop (cdr remaining) (cons (car remaining) acc))])))

(define (drop-trailing-blank-lines lines)
  (cond
    [(null? lines) '()]
    [(and (string=? (last lines) "") (pair? (drop-right lines 1)))
     (drop-trailing-blank-lines (drop-right lines 1))]
    [else lines]))

;; Status fields in wave headers and PLAN.md index rows are mutable projection
;; state. Snapshot hashes ignore them while retaining every other line, so
;; normal progress is not mistaken for authored-content drift.
(define (normalize-wave-doc-content text)
  (define lines (string-split text "\n" #:trim? #f))
  (define without-header (strip-leading-generated-header-blocks lines))
  (define without-failure (strip-failure-sections without-header))
  (canonical-lines (string-join (drop-trailing-blank-lines without-failure) "\n")))

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

;; Sweep ONE interrupted staging operation of OURS. Ownership is proven by the
;; AUTHENTIC marker file this module writes before it copies any bytes
;; (validated by planning-artifact-ownership.rkt: real nofollow marker, hash
;; datum, schema-version 1, kind `plan-snapshot-staging`); anything else under
;; the campaigns directory is unknown operator state and is preserved.
(define (plan-snapshot-staging-dir? p)
  (define name (file-name-from-path p))
  (and name
       ;; A symlink is never "ours": sweeping it could delete a target outside
       ;; the campaign directory.
       (not (link-exists? p))
       (directory-exists? p)
       (regexp-match? (regexp (string-append "^" (regexp-quote plan-snapshot-staging-prefix)))
                      (path->string name))
       (authentic-planning-artifact-marker? p 'plan-snapshot-staging)))

(define (sweep-stale-tmp-dirs! snapshot-parent)
  (for ([p (in-list (directory-list snapshot-parent #:build? #t))])
    (when (plan-snapshot-staging-dir? p)
      (delete-directory/files p))))

;; Record ownership inside a fresh staging directory. Diagnostic only: it never
;; grants authority, and it is what makes later cleanup provable.
(define (write-staging-marker! staging-dir)
  (call-with-output-file (build-path staging-dir plan-snapshot-staging-marker-filename)
                         (lambda (out)
                           (write (hasheq 'schema-version
                                          1
                                          'kind
                                          'plan-snapshot-staging
                                          'pid
                                          (getpid)
                                          'created-at
                                          (current-seconds))
                                  out))
                         #:exists 'truncate))

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
  (define plan-path (plan-file base-dir))
  (unless (file-exists? plan-path)
    (raise (make-exn:fail:gsd-missing-wave-doc (format "plan-snapshot: PLAN.md missing under ~a"
                                                       base-dir)
                                               (current-continuation-marks)
                                               "PLAN.md")))
  (define plan-bytes (file->bytes plan-path))
  (define wave-bytes
    (for/list ([rel (in-list wave-paths)])
      (define p (planning-path base-dir rel))
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
  (define campaigns-dir (campaign-dir base-dir campaign-id))
  (make-directory* campaigns-dir)
  (define tmp (make-temporary-file (string-append plan-snapshot-staging-prefix "~a") #f campaigns-dir))
  ;; make-temporary-file yields a file; the staging area must be a directory.
  (delete-file tmp)
  (make-directory tmp)
  (write-staging-marker! tmp)
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     (for ([wb (in-list (cons (cons "PLAN.md" plan-bytes) wave-bytes))])
       (define target (build-path tmp (car wb)))
       (make-directory* (path-only target))
       (call-with-output-file target (lambda (out) (write-bytes (cdr wb) out)) #:exists 'truncate))
     (call-with-output-file (build-path tmp snapshot-manifest-filename)
                            (lambda (out) (write (manifest->datum manifest) out))
                            #:exists 'truncate)
     ;; The ownership marker is staging-only: the published snapshot contains
     ;; exactly the manifest plus the captured documents.
     (delete-file (build-path tmp plan-snapshot-staging-marker-filename))
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
  (when existing
    ;; F13 (v1.00.31 W5): reuse is only sound when the authored plan body
    ;; still matches the frozen contract. Amended bodies are refused with a
    ;; typed failure — never silently rebound, never silently rewritten.
    (define live-plan-path (plan-file base-dir))
    (define frozen-entry
      (for/first ([f (in-list (plan-snapshot-manifest-files existing))]
                  #:when (equal? (snapshot-file-path f) "PLAN.md"))
        f))
    (define authored-sha (plan-body-normalized-hash (file->string live-plan-path)))
    (define frozen-sha (and frozen-entry (snapshot-file-sha256 frozen-entry)))
    (unless (equal? authored-sha frozen-sha)
      (raise (make-exn:fail:gsd-frozen-contract-stale
              (format (string-append "plan-snapshot: frozen contract is stale for campaign ~a: "
                                     "the authored plan body changed after the snapshot was frozen "
                                     "(authored ~a, frozen ~a); re-freeze under a new campaign "
                                     "identity or explicitly restore the snapshot content")
                      campaign-id
                      authored-sha
                      frozen-sha)
              (current-continuation-marks)
              campaign-id
              frozen-sha
              authored-sha))))
  (define m
    (or existing
        (let* ([plan-path (plan-file base-dir)]
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
;; Returns #f when no snapshot exists. Legacy callers sweep stale tmp
;; directories; read-only inspection passes #:sweep-stale? #f.
;; Corrupt/partial snapshots always raise and are never accepted.
(define (load-snapshot-manifest base-dir campaign-id #:sweep-stale? [sweep-stale? #t])
  (unless (sha256-hex? campaign-id)
    (snapshot-fail "plan-snapshot: invalid campaign id ~s" campaign-id))
  (define snapshot-root (snapshot-dir base-dir campaign-id))
  (define mp (snapshot-manifest-path base-dir campaign-id))
  (when (or (snapshot-link? snapshot-root) (snapshot-link? mp))
    (snapshot-fail "plan-snapshot: refusing symlinked snapshot boundary"))
  (unless (file-exists? mp)
    (define snapshot-parent (campaign-dir base-dir campaign-id))
    (when (and sweep-stale? (directory-exists? snapshot-parent))
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
  (define root (planning-root base-dir))
  (define live (build-path root rel))
  (or (snapshot-link? root)
      (and (string-prefix? rel "waves/") (snapshot-link? (build-path root "waves")))
      (snapshot-link? live)))

;; Paths of live plan/wave documents that drifted from the snapshot
;; (content changed, file missing, or untrusted symlink boundary). Empty list
;; means no drift. Symlinks are classified before any content read.
(define (snapshot-drift? base-dir campaign-id)
  (map car (classify-snapshot-drift base-dir campaign-id)))

;; Per-file drift classification for actionable resume diagnostics.
;; Returns a list of (list <rel-path> <kind>) with kind one of:
;;   'symlink       — live path crosses the symlink trust boundary
;;   'missing       — live file absent (recoverable from the snapshot)
;;   'content-drift — live content differs beyond normalized projection state
;; Ordered: symlink first (fail-closed), then missing, then content drift.
(define (classify-snapshot-drift base-dir campaign-id #:sweep-stale? [sweep-stale? #t])
  (define m (load-snapshot-manifest base-dir campaign-id #:sweep-stale? sweep-stale?))
  (reverse (for/fold ([acc '()])
                     ([f (in-list (if m
                                      (plan-snapshot-manifest-files m)
                                      '()))])
             (define rel (snapshot-file-path f))
             (define live (planning-path base-dir rel))
             (define kind
               (cond
                 [(live-path-has-link? base-dir rel) 'symlink]
                 [(not (file-exists? live)) 'missing]
                 [(not (equal? (normalized-file-hash rel (file->string live))
                               (snapshot-file-sha256 f)))
                  ;; F13 (v1.00.31 W5): a diverging PLAN.md body means the
                  ;; authored contract is no longer the frozen one — named
                  ;; distinctly from ordinary wave-doc content drift.
                  (if (equal? rel "PLAN.md") 'frozen-contract-stale 'content-drift)]
                 [else #f]))
             (if kind
                 (cons (list rel kind) acc)
                 acc))))

;; Restore drifted live .planning documents from the verified snapshot.
;; Default: existing content drift raises before any write — the operator must
;; decide (replan/archive or an explicit override). With
;; #:override-existing-drift? the sanctioned remedy becomes executable: every
;; existing drifted file is restored to snapshot content, with a logged
;; warning naming each overwritten path (audit trail for the post-hoc
;; forensics BUG-0068 called for). Symlink boundaries are always refused,
;; even under override (see snapshot-drift-class-overridable?). Returns
;; restored paths; never modifies snapshot.
(define (restore-plan-from-snapshot! base-dir campaign-id #:override-existing-drift? [override? #f])
  ;; Verify the snapshot and classify all drift before writing anything.
  (define classified (classify-snapshot-drift base-dir campaign-id))
  (for ([item (in-list classified)])
    (unless (snapshot-drift-class-overridable? (cadr item))
      (snapshot-fail "plan-snapshot: refusing ~a restore boundary: ~a"
                     (cadr item)
                     (car item))))
  (define existing-drift
    (for/list ([item (in-list classified)]
               #:when (and (snapshot-drift-class-overridable? (cadr item))
                           (file-exists? (planning-path base-dir (car item)))))
      (car item)))
  (unless (or (null? existing-drift) override?)
    (raise (make-exn:fail
            (format (string-append
                     "plan-snapshot: refusing to overwrite live content drift: ~a; "
                     "recovery: (restore-plan-from-snapshot! base campaign-id "
                     "#:override-existing-drift? #t), or explicitly replan/archive the changes")
                    existing-drift)
            (current-continuation-marks))))
  (when (and override? (pair? existing-drift))
    (log-warning "plan-snapshot: operator-overridden restore overwriting ~a drifted file(s): ~a"
                 (length existing-drift)
                 existing-drift))
  ;; Missing files are safe to reconstruct. Overridden drift is reset to the
  ;; capture-time snapshot content (the durable campaign record, not the doc,
  ;; remains authoritative for wave status).
  (for/list ([item (in-list classified)]
             #:when (memq (cadr item) '(missing content-drift frozen-contract-stale)))
    (define rel (car item))
    (define src (build-path (snapshot-dir base-dir campaign-id) rel))
    (define dest (planning-path base-dir rel))
    (make-directory* (path-only dest))
    ;; Overridden drift replaces the existing (non-symlink) live file.
    (when (and (memq (cadr item) '(content-drift frozen-contract-stale)) (file-exists? dest))
      (delete-file dest))
    (copy-file src dest)
    rel))
