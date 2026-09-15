#lang racket/base
;; Trusted datum boundary for the coordinator CLI. Never evaluates input.
;;
;; snapshot: verifies the immutable campaign plan snapshot through the trusted
;; extensions/gsd/plan-snapshot.rkt module (full per-file hash validation) and
;; reports the frozen wave facts (wave document, issue/milestone metadata and
;; the wave's declared gsd-wave trio outputs, q/-prefixed exactly as authored).
;; Wave index or file mtime alone is never sufficient: the manifest must name
;; exactly one wave document for the requested index and its content is read
;; from the verified snapshot, never from mutable live planning files.
;;
;; prepare: emits a deliberately gate-red but structurally complete schema-2
;; binding draft trio. required-checks carries the exact policy snapshot;
;; review verdict, digests, red-first/focused/fast evidence and remaining
;; items are honest PENDING placeholders — no approval is ever inferred.
(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/pretty
         racket/string
         (only-in "../extensions/gsd/plan-snapshot.rkt"
                  load-snapshot-manifest
                  plan-snapshot-manifest-files
                  snapshot-file-path
                  snapshot-dir))
(provide read-hash-datum
         read-any-datum
         binding-draft
         snapshot-facts)

(define (read-single-datum path who)
  (call-with-input-file path
                        (lambda (in)
                          (parameterize ([read-accept-reader #f]
                                         [read-accept-lang #f]
                                         [read-accept-graph #f])
                            (define datum (read in))
                            (when (eof-object? datum)
                              (error who "file is empty: ~a" path))
                            (unless (eof-object? (read in))
                              (error who "file contains multiple datums: ~a" path))
                            datum))))

(define (read-hash-datum path)
  (define datum (read-single-datum path 'delivery))
  (unless (hash? datum)
    (error 'delivery "expected exactly one hash datum"))
  datum)

(define (read-any-datum path)
  (read-single-datum path 'delivery))

(define (required-checks request)
  (define names (hash-ref request 'required-pr-checks #f))
  (unless (and (list? names)
               (pair? names)
               (andmap (lambda (n) (and (string? n) (not (string=? (string-trim n) "")))) names)
               (= (length names) (length (remove-duplicates names))))
    (error 'delivery "binding draft requires the exact required-check policy snapshot"))
  names)

(define (binding-draft request)
  ;; This exported/CLI boundary is callable without the Python controller.
  ;; Reject path-bearing identity before creating any output directory.
  (unless (and (hash? request)
               (string? (hash-ref request 'plan-id #f))
               (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref request 'plan-id))
               (string? (hash-ref request 'wave #f))
               (regexp-match? #px"^W[0-9]+$" (hash-ref request 'wave))
               (string? (hash-ref request 'merge-sha #f))
               (regexp-match? #px"^[0-9a-f]{40}$" (hash-ref request 'merge-sha)))
    (error 'delivery "invalid binding campaign/wave/merge identity"))
  (define stem
    (string-append (hash-ref request 'plan-id)
                   "-"
                   (string-downcase (hash-ref request 'wave))
                   ".rktd"))
  (define review (string-append "docs/reports/gsd-wave-reviews/" stem))
  (define validation (string-append "docs/reports/gsd-wave-validation/" stem))
  ;; Deliberately gate-red. The coordinator must obtain genuine fresh review,
  ;; validation and the actual excluded diff digest before protected publication.
  (define common
    (hash-set* request 'implementation-sha (hash-ref request 'merge-sha) 'content-digest "PENDING"))
  (values (hash-set* common
                     'schema-version
                     2
                     'merge-method
                     "squash"
                     'review-artifact
                     review
                     'validation-artifact
                     validation
                     'required-checks
                     (required-checks request)
                     'status
                     "pending-review")
          (hasheq 'reviewer
                  "PENDING"
                  'verdict
                  "PENDING"
                  'timestamp
                  "PENDING"
                  'reviewed-sha
                  (hash-ref request 'merge-sha)
                  'content-digest
                  "PENDING"
                  'scope
                  "Campaign-specific metadata binding; fresh independent review required"
                  'report
                  "PENDING")
          (hash-set* common
                     'status
                     "pending"
                     'planning-sync
                     "pending"
                     'review-artifact
                     review
                     'red-first
                     (hasheq 'command "PENDING" 'failure "PENDING")
                     'focused-tests
                     (hasheq 'result "PENDING" 'command "PENDING")
                     'format-compile
                     (hasheq 'result "PENDING" 'command "PENDING")
                     'lint
                     (hasheq 'result "PENDING" 'command "PENDING")
                     'fast
                     (hasheq 'result "PENDING" 'command "PENDING")
                     'remaining-items
                     (list (hasheq 'classification
                                   "deferred-noncritical"
                                   'owner
                                   "PENDING"
                                   'rationale
                                   "PENDING: fresh binding review and gates required")))))

;; ---------------------------------------------------------------------------
;; Frozen campaign snapshot provenance (trusted module, full hash validation)
;; ---------------------------------------------------------------------------

(define (first-group-number rx line)
  (define match (regexp-match rx line))
  (and match (string->number (cadr match))))

(define (declared-output-line? line)
  (regexp-match? #px"^\\s*[-*]\\s+File:" line))

(define (declared-in-line line kind)
  (define match
    (regexp-match (string-append "(?:q/)?docs/reports/gsd-wave-" kind "/[0-9A-Za-z._/-]+[.]rktd")
                  line))
  (and match (car match)))

(define (snapshot-facts campaign-root plan-id wave-index)
  (unless (and (string? plan-id) (regexp-match? #px"^[0-9a-f]{64}$" plan-id))
    (error 'delivery "invalid campaign id"))
  (unless (exact-nonnegative-integer? wave-index)
    (error 'delivery "invalid wave index"))
  ;; Full hash validation happens inside the trusted plan-snapshot module;
  ;; a partial, corrupt or tampered snapshot never verifies.
  (define manifest (load-snapshot-manifest campaign-root plan-id))
  (unless manifest
    (error 'delivery "immutable campaign snapshot is absent"))
  (define prefix (format "W~a-" wave-index))
  (define docs
    (filter (lambda (f)
              (define rel (snapshot-file-path f))
              (and (string-prefix? rel "waves/") (string-prefix? (substring rel 6) prefix)))
            (plan-snapshot-manifest-files manifest)))
  (unless (= 1 (length docs))
    (error 'delivery "snapshot does not freeze exactly one wave document for W~a" wave-index))
  (define rel (snapshot-file-path (car docs)))
  ;; Read the frozen content from the verified snapshot, never from live files.
  (define text (file->string (build-path (snapshot-dir campaign-root plan-id) rel)))
  (define lines (string-split text "\n"))
  (define branch-lines (filter (lambda (l) (string-prefix? (string-trim l) "Branch:")) lines))
  (define (exactly-one proc)
    (and (= 1 (length branch-lines)) (proc (car branch-lines))))
  (define issue (exactly-one (lambda (l) (first-group-number #px"issues/([0-9]+)" l))))
  (define milestone (exactly-one (lambda (l) (first-group-number #px"milestone\\s*#?([0-9]+)" l))))
  (define file-lines (filter declared-output-line? lines))
  (define (declared kind)
    (filter values
            (for/list ([line (in-list file-lines)])
              (declared-in-line line kind))))
  (apply hasheq
         (append (list 'status
                       "ok"
                       'plan-id
                       plan-id
                       'wave
                       wave-index
                       'wave-doc
                       rel
                       'declared
                       (hasheq 'evidence
                               (declared "evidence")
                               'review
                               (declared "reviews")
                               'validation
                               (declared "validation")))
                 (if issue
                     (list 'issue issue)
                     null)
                 (if milestone
                     (list 'milestone milestone)
                     null))))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (case (string->symbol (vector-ref (current-command-line-arguments) 0))
    [(read)
     (write-json (read-hash-datum (vector-ref (current-command-line-arguments) 1)))
     (newline)]
    [(read-any)
     (write-json (read-any-datum (vector-ref (current-command-line-arguments) 1)))
     (newline)]
    [(prepare)
     (define root (vector-ref (current-command-line-arguments) 2))
     (define-values (e r v)
       (binding-draft (call-with-input-file (vector-ref (current-command-line-arguments) 1)
                                            read-json)))
     (define evidence
       (string-append "docs/reports/gsd-wave-evidence/"
                      (hash-ref e 'plan-id)
                      "-"
                      (string-downcase (hash-ref e 'wave))
                      ".rktd"))
     (for ([datum (in-list (list e r v))]
           [relative (in-list (list evidence
                                    (hash-ref e 'review-artifact)
                                    (hash-ref e 'validation-artifact)))])
       (define path (build-path root relative))
       (make-parent-directory* path)
       (call-with-output-file path (lambda (out) (pretty-write datum out)) #:exists 'error))]
    [(snapshot)
     (unless (= 4 (length args))
       (error 'delivery "snapshot requires: <campaign-root> <plan-id> <wave-index>"))
     (write-json (snapshot-facts (second args) (third args) (string->number (fourth args))))
     (newline)]
    [else (error 'delivery "expected read, read-any, prepare or snapshot")]))
