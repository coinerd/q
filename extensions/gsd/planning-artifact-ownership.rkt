#lang racket/base

;; extensions/gsd/planning-artifact-ownership.rkt — the ONE authentic
;; ownership-marker predicate for planning artifacts q itself creates.
;;
;; W4 (GSD-SIMPLIFICATION-PLAN) blocker: three modules used to decide "is this
;; directory ours?" in three different ways, and they did not agree:
;;
;;   * `archive.rkt` deleted every empty non-protected `.planning/<dir>`, so an
;;     unknown empty `notes/` directory owned by the operator was destroyed;
;;   * `plan-snapshot.rkt` (staging sweep) and `command-handlers.rkt` (supersede
;;     backup rotation) only checked that a marker FILE EXISTED, so an impostor
;;     marker (any bytes, any kind, wrong schema, or a symlink) authorized
;;     deleting the directory;
;;   * `planning-artifact-validation.rkt` (the read-only doctor) already
;;     validated schema/kind, so the doctor and the destructive paths disagreed
;;     about ownership.
;;
;; The predicate now lives here — ONE implementation, consumed by all four
;; modules — and is deliberately strict. A marker proves ownership only when
;; it is:
;;
;;   1. a REAL regular file reached without following a symlink (`link-exists?`
;;      is false the final component is not a link), so a marker cannot point
;;      at bytes outside the directory it claims to own;
;;   2. small enough to be an ownership record (never a blocked FIFO, never a
;;      huge file that would be read just to prove ownership);
;;   3. a readable hash DATUM, not empty bytes or arbitrary text;
;;   4. `schema-version` exactly 1, and
;;   5. `kind` exactly the kind the creating module records.
;;
;; Anything else — a missing marker, an impostor marker, a `.bak` file, an
;; operator's manually named backup — is UNKNOWN operator state. Every consumer
;; preserves unknown state verbatim; none of them deletes or sweeps what it
;; cannot prove it created. This module is a PURE READ SHELL: it never writes,
;; creates, renames, or deletes anything.

(require racket/file
         racket/path)

(provide planning-artifact-ownership-kinds
         planning-artifact-marker-schema-version
         planning-artifact-marker-filename
         authentic-planning-artifact-marker?
         owned-planning-artifact-dir?)

;; The only schema version any ownership marker may carry. A newer schema is
;; not "close enough": an unknown record is unknown state.
(define planning-artifact-marker-schema-version 1)

;; Marker filename bound to the artifact kind the CREATOR records inside it.
;; The kind is never inferred from the directory name: a lookalike name with a
;; foreign marker stays unknown.
(define ownership-markers
  '((plan-snapshot-staging . ".owned-plan-snapshot-staging")
    (waves-pre-plan-backup . ".q-gsd-plan-supersede-owner")))

;; Every artifact kind the ownership contract knows. Ordered for determinism.
(define (planning-artifact-ownership-kinds)
  (map car ownership-markers))

;; kind -> marker filename. Raises for an unknown kind so a new writer cannot
;; silently invent a marker that no consumer validates.
(define (planning-artifact-marker-filename kind)
  (define entry (assq kind ownership-markers))
  (unless entry
    (raise-arguments-error 'planning-artifact-marker-filename
                           "unknown planning-artifact ownership kind"
                           "kind"
                           kind
                           "known kinds"
                           (planning-artifact-ownership-kinds)))
  (cdr entry))

;; A marker is read only when it is a real file of plausible size; the size
;; guard also keeps a FIFO or device from blocking this pure probe.
(define max-marker-bytes 4096)

(define (real-marker-file? marker)
  (and (eq? (file-or-directory-type marker #f) 'file)
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (define size (file-size marker))
         (and (exact-integer? size) (> size 0) (<= size max-marker-bytes)))))

;; The whole marker file must be exactly ONE readable hash datum: a valid record
;; followed by further bytes is not a record any creator writes, so it is
;; treated as unknown state rather than "the first hash wins".
(define (marker-datum marker)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file marker
                          (lambda (in)
                            (define datum (read in))
                            (and (hash? datum) (eof-object? (read in)) datum)))))

;; authentic-planning-artifact-marker? : path? symbol? -> boolean?
;; True only when `dir` carries the authentic marker for exactly `kind`:
;; a real (non-link) marker file whose datum is a hash with schema-version 1
;; and the matching kind. Unreadable, malformed, or foreign markers are #f.
(define (authentic-planning-artifact-marker? dir kind)
  (define marker (build-path dir (planning-artifact-marker-filename kind)))
  (and (not (link-exists? marker))
       (real-marker-file? marker)
       (let ([datum (marker-datum marker)])
         (and datum
              (equal? (hash-ref datum 'schema-version #f) planning-artifact-marker-schema-version)
              (eq? (hash-ref datum 'kind #f) kind)))))

;; owned-planning-artifact-dir? : path? -> (or/c symbol? #f)
;; The PROVEN ownership kind of an existing real directory, or #f. A symlinked
;; directory is never ours: deleting it could reach outside the planning root.
(define (owned-planning-artifact-dir? dir)
  (and (not (link-exists? dir))
       (directory-exists? dir)
       (for/first ([kind (in-list (planning-artifact-ownership-kinds))]
                   #:when (authentic-planning-artifact-marker? dir kind))
         kind)))
