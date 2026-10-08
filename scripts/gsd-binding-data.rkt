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
         racket/system
         (only-in "../extensions/gsd/plan-snapshot.rkt"
                  load-snapshot-manifest
                  plan-snapshot-manifest-files
                  snapshot-file-path
                  snapshot-dir)
         (only-in "../extensions/gsd/wave-docs.rkt"
                  parse-plan-index
                  wave-index-entry-idx
                  wave-index-entry-ref-path)
         (only-in "../extensions/gsd/delivery-journal.rkt"
                  load-delivery-journal
                  reconcile-merged-tip-receipt!))
(provide read-hash-datum
         read-any-datum
         binding-draft
         snapshot-facts
         active-source-path
         active-paths)

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
  (define generation (hash-ref request 'binding-generation 0))
  (unless (exact-nonnegative-integer? generation)
    (error 'delivery "invalid binding generation"))
  (define stem
    (string-append (hash-ref request 'plan-id)
                   "-"
                   (string-downcase (hash-ref request 'wave))
                   (if (zero? generation)
                       ""
                       (format "-r~a" generation))
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

(define (source-trio-path directory plan-id wave-index generation)
  (string-append "docs/reports/gsd-wave-"
                 directory
                 "/"
                 plan-id
                 "-w"
                 (number->string wave-index)
                 (if (zero? generation)
                     ""
                     (format "-r~a" generation))
                 ".rktd"))

(define (repair-generation-path path generation)
  (define match (regexp-match #px"^(.*/)?([^/]+)[.]rktd$" path))
  (unless match
    (error 'delivery "multiple/conflicting declared output path: ~a" path))
  (string-append (or (cadr match) "") (caddr match) (format "-r~a.rktd" generation)))

(define (active-source-path path generation)
  (unless (and (string? path) (positive? (string-length path)))
    (error 'delivery "multiple/conflicting declared output path"))
  (unless (exact-nonnegative-integer? generation)
    (error 'delivery "invalid binding generation"))
  (if (zero? generation)
      path
      (repair-generation-path path generation)))

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
  ;; BUG-0081: wave-doc identity is the declared arrow path of the FROZEN
  ;; plan's index row for this wave — a plan may deliberately route a wave
  ;; through another campaign's W-numbered carrier doc, so a W<n>- filename
  ;; prefix alone cannot select the document. The frozen PLAN.md is read
  ;; from the verified snapshot directory (never live planning files); the
  ;; unique-prefix scan remains the fallback for rows/snapshots without a
  ;; declared arrow path.
  (define frozen-plan-path (build-path (snapshot-dir campaign-root plan-id) "PLAN.md"))
  (define declared-path
    (and (file-exists? frozen-plan-path)
         (for/first ([e (in-list (parse-plan-index (file->string frozen-plan-path)))]
                     #:when (= (wave-index-entry-idx e) wave-index))
           (wave-index-entry-ref-path e))))
  (define docs
    (if declared-path
        (filter (lambda (f) (string=? (snapshot-file-path f) declared-path))
                (plan-snapshot-manifest-files manifest))
        (let ([prefix (format "W~a-" wave-index)])
          (filter (lambda (f)
                    (define rel (snapshot-file-path f))
                    (and (string-prefix? rel "waves/") (string-prefix? (substring rel 6) prefix)))
                  (plan-snapshot-manifest-files manifest)))))
  (unless (= 1 (length docs))
    (if declared-path
        (error 'delivery
               "snapshot does not freeze the declared wave document ~a for W~a"
               declared-path
               wave-index)
        (error 'delivery "snapshot does not freeze exactly one wave document for W~a" wave-index)))
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

(define (active-paths campaign-root plan-id wave-index generation)
  (unless (exact-nonnegative-integer? generation)
    (error 'delivery "invalid binding generation"))
  (define facts (snapshot-facts campaign-root plan-id wave-index))
  (define declared (hash-ref facts 'declared #hasheq()))
  (define (declared-list kind)
    (define value (hash-ref declared kind '()))
    (unless (list? value)
      (error 'delivery "multiple/conflicting declared output for ~a" kind))
    value)
  (define evidence (declared-list 'evidence))
  (define review (declared-list 'review))
  (define validation (declared-list 'validation))
  (define counts
    (hasheq 'evidence (length evidence) 'review (length review) 'validation (length validation)))
  (cond
    [(and (zero? (length evidence)) (zero? (length review)) (zero? (length validation)))
     ;; Zero-declared waves keep ONE deterministic path family (the
     ;; hash-named trio); a repair generation derives it by the same -rN
     ;; rule used for declared outputs — deterministic, never ambiguous,
     ;; gen0 paths never reused for an active repair generation.
     (hasheq 'evidence
             (source-trio-path "evidence" plan-id wave-index generation)
             'review
             (source-trio-path "reviews" plan-id wave-index generation)
             'validation
             (source-trio-path "validation" plan-id wave-index generation)
             'generation
             generation
             'declared-counts
             counts)]
    [(and (= 1 (length evidence)) (= 1 (length review)) (= 1 (length validation)))
     (hasheq 'evidence
             (active-source-path (car evidence) generation)
             'review
             (active-source-path (car review) generation)
             'validation
             (active-source-path (car validation) generation)
             'generation
             generation
             'declared-counts
             counts)]
    [else
     (error
      'delivery
      "multiple/conflicting declared outputs; exactly one evidence, review and validation required")]))

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
                      (path->string (file-name-from-path (hash-ref e 'review-artifact)))))
     (for ([datum (in-list (list e r v))]
           [relative (in-list (list evidence
                                    (hash-ref e 'review-artifact)
                                    (hash-ref e 'validation-artifact)))])
       (define path (build-path root relative))
       (make-parent-directory* path)
       (call-with-output-file path (lambda (out) (pretty-write datum out)) #:exists 'error))]
    [(reconcile-merged-tip)
     ;; BUG-0082: reconcile the SAME-ATTEMPT delivery receipt to a post-merge
     ;; branch tip (the merged PR head or a later republication head) over a
     ;; provably evidence-only tail, at journal stage implementation-merged.
     ;; argv: <repo> <campaign-root> <plan-id> <wave-index> <attempt-id>
     ;;       <fence> <new-head>
     (unless (= 8 (length args))
       (error
        'delivery
        "reconcile-merged-tip requires: <repo> <campaign-root> <plan-id> <wave-index> <attempt-id> <fence> <new-head>"))
     (define repo (second args))
     (define croot (third args))
     (define plan-id (fourth args))
     (define wave-idx (string->number (fifth args)))
     (define attempt-id (sixth args))
     (define fence-num (string->number (seventh args)))
     (define new-head (eighth args))
     (define (git . git-args)
       (define out (open-output-string))
       (define err (open-output-string))
       (define code
         (parameterize ([current-output-port out]
                        [current-error-port err])
           (apply system*/exit-code (find-executable-path "git") "-C" repo git-args)))
       (values code (string-trim (get-output-string out)) (string-trim (get-output-string err))))
     (define excludes
       '("--" "."
              ":(exclude)docs/reports/gsd-wave-evidence/**"
              ":(exclude)docs/reports/gsd-wave-reviews/**"
              ":(exclude)docs/reports/gsd-wave-validation/**"))
     (define journal (load-delivery-journal croot plan-id wave-idx))
     (unless (hash? journal)
       (error 'delivery "no durable journal to reconcile"))
     (define old-receipt (hash-ref journal 'receipt #f))
     (unless (hash? old-receipt)
       (error 'delivery "journal carries no receipt"))
     (define old-head (hash-ref old-receipt 'head #f))
     (define-values (anc-code _anc-out anc-err) (git "merge-base" "--is-ancestor" old-head new-head))
     (unless (eqv? anc-code 0)
       (error 'delivery (format "new head is not a descendant of the receipt head: ~a" anc-err)))
     (define-values (diff-code diff-out diff-err)
       (apply git "diff" "--name-only" old-head new-head excludes))
     (unless (eqv? diff-code 0)
       (error 'delivery (format "evidence-only tail probe failed: ~a" diff-err)))
     (unless (string=? diff-out "")
       (error 'delivery (format "tail over the receipt head is not evidence-only: ~a" diff-out)))
     (define-values (tree-code tree-out tree-err)
       (git "rev-parse" (string-append new-head "^{tree}")))
     (unless (eqv? tree-code 0)
       (error 'delivery (format "cannot resolve new head tree: ~a" tree-err)))
     (define reconciled
       (reconcile-merged-tip-receipt!
        croot
        plan-id
        wave-idx
        (make-immutable-hash
         (append
          (hash->list old-receipt)
          (list (cons 'head new-head)
                (cons 'tree tree-out)
                (cons 'verified-at (current-seconds))
                (cons 'evidence
                      "post-merge merged-tip reconciliation over an evidence-only tail (BUG-0082)"))))
        #:expected-attempt-id attempt-id
        #:expected-fence fence-num
        #:head-ancestor? (lambda (old new)
                           (define-values (code _out _err) (git "merge-base" "--is-ancestor" old new))
                           (eqv? code 0))
        #:tail-evidence-only?
        (lambda (old new)
          (define-values (code out _err) (apply git "diff" "--name-only" old new excludes))
          (and (eqv? code 0) (string=? out "")))))
     (write-json reconciled)
     (newline)]
    [(journal)
     (unless (= 4 (length args))
       (error 'delivery "journal requires: <campaign-root> <plan-id> <wave-index>"))
     (write-json (load-delivery-journal (second args) (third args) (string->number (fourth args))))
     (newline)]
    [(snapshot)
     (unless (= 4 (length args))
       (error 'delivery "snapshot requires: <campaign-root> <plan-id> <wave-index>"))
     (write-json (snapshot-facts (second args) (third args) (string->number (fourth args))))
     (newline)]
    [(active-paths)
     (unless (= 5 (length args))
       (error 'delivery "active-paths requires: <campaign-root> <plan-id> <wave-index> <generation>"))
     (write-json (active-paths (second args)
                               (third args)
                               (string->number (fourth args))
                               (string->number (fifth args))))
     (newline)]
    [else (error 'delivery "expected read, read-any, prepare, journal, snapshot or active-paths")]))
