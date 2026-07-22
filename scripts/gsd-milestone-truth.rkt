#lang racket/base

;; Canonical milestone truth is intentionally a pure, narrow boundary:
;; strict JSON in, deterministic projection bytes/digest, and one derived result.
;;
;; Canonicalization uses the restricted integer-only JCS profile implemented
;; below: JSON scalars plus safe integers only, UTF-16 object-key ordering, and
;; canonical UTF-8 emission.  This is deliberately not a claim of unrestricted
;; RFC 8785 support (notably, floating-point JSON numbers are rejected).

(require racket/cmdline
         racket/list
         racket/string
         "../util/json/strict-json.rkt"
         "../util/json/canonical-json.rkt")

(provide release-mechanics-values
         substantive-acceptance-values
         strict-json-read
         strict-json-read-file
         canonical-json-bytes
         canonical-json-sha256
         validate-milestone-document
         (struct-out milestone-truth)
         milestone-truth->jsexpr
         milestone-truth-gate-passed?
         evaluate-milestone-truth
         evaluate-milestone-file)

(define release-mechanics-values '("planned" "built" "smoked" "draft-verified" "published"))
(define substantive-acceptance-values '("planned" "in-progress" "rejected" "accepted"))

(define digest-pattern #px"^[0-9a-f]{64}$")

;; Closed milestone schema ---------------------------------------------------

(define top-level-keys
  '("acceptance-critical-remaining-items" "milestone"
                                          "release-evidence"
                                          "release-mechanics"
                                          "review"
                                          "schema-version"
                                          "substantive-acceptance"
                                          "validation"
                                          "work-items"))

(define (require-exact-keys who value expected)
  (unless (hash? value)
    (error who "expected an object, got ~e" value))
  (define actual (sort (hash-keys value) string<?))
  (define wanted (sort expected string<?))
  (unless (equal? actual wanted)
    (error who "object keys must be exactly ~e; got ~e" wanted actual)))

(define (non-empty-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

(define (member-string? value choices)
  (and (string? value) (member value choices) #t))

(define release-stage-names '("build" "exact-artifact-smoke" "draft-verification" "publication"))

(define (digest? value)
  (and (string? value) (regexp-match? digest-pattern value) #t))

(define (evidence-or-null? value)
  (or (eq? value 'null) (digest? value)))

(define (validate-milestone-document document)
  ;; This also enforces the restricted in-memory JSON model before schema checks.
  (canonical-json-bytes document)
  (require-exact-keys 'validate-milestone-document document top-level-keys)
  (unless (equal? (hash-ref document "schema-version") 1)
    (error 'validate-milestone-document "schema-version must be 1"))
  (unless (and (non-empty-string? (hash-ref document "milestone"))
               (regexp-match? #px"^v[0-9]+\\.[0-9]+\\.[0-9]+$" (hash-ref document "milestone")))
    (error 'validate-milestone-document "milestone must be a v-prefixed semantic version"))
  (unless (member-string? (hash-ref document "release-mechanics") release-mechanics-values)
    (error 'validate-milestone-document "release-mechanics is not a locked value"))
  (unless (member-string? (hash-ref document "substantive-acceptance") substantive-acceptance-values)
    (error 'validate-milestone-document "substantive-acceptance is not a locked value"))

  ;; A release-evidence array is an ordered completed prefix.  Consequently a
  ;; later stage can never appear before, or without, an earlier stage.
  (define release-evidence (hash-ref document "release-evidence"))
  (unless (and (list? release-evidence) (<= (length release-evidence) 4))
    (error 'validate-milestone-document "release-evidence must contain at most four stages"))
  (for ([entry (in-list release-evidence)]
        [expected-stage (in-list release-stage-names)])
    (require-exact-keys 'validate-milestone-document entry '("evidence-digest" "stage"))
    (unless (string=? (hash-ref entry "stage") expected-stage)
      (error 'validate-milestone-document
             "release-evidence must be the gap-free ordered stage prefix; expected ~a"
             expected-stage))
    (unless (digest? (hash-ref entry "evidence-digest"))
      (error 'validate-milestone-document "completed release stage requires a 64-hex digest")))

  (define work-items (hash-ref document "work-items"))
  (unless (and (list? work-items) (= (length work-items) 11))
    (error 'validate-milestone-document "work-items must contain exactly W0 through W10"))
  (for ([item (in-list work-items)]
        [index (in-range 11)])
    (require-exact-keys 'validate-milestone-document item '("criteria" "id" "status"))
    (unless (equal? (hash-ref item "id") (format "W~a" index))
      (error 'validate-milestone-document "work-items must be ordered exactly W0 through W10"))
    (unless (member-string? (hash-ref item "status") '("planned" "in-progress" "complete"))
      (error 'validate-milestone-document "work-item status is invalid for ~a" (hash-ref item "id")))
    (define criteria (hash-ref item "criteria"))
    (unless (and (list? criteria) (pair? criteria))
      (error 'validate-milestone-document "~a criteria must be non-empty" (hash-ref item "id")))
    (define criterion-ids '())
    (for ([criterion (in-list criteria)])
      (require-exact-keys 'validate-milestone-document criterion '("evidence-digest" "id" "met"))
      (unless (non-empty-string? (hash-ref criterion "id"))
        (error 'validate-milestone-document "criterion id must be a non-empty string"))
      (unless (boolean? (hash-ref criterion "met"))
        (error 'validate-milestone-document "criterion met must be boolean"))
      (unless (evidence-or-null? (hash-ref criterion "evidence-digest"))
        (error 'validate-milestone-document "criterion evidence-digest must be null or 64-hex"))
      (when (and (hash-ref criterion "met") (not (digest? (hash-ref criterion "evidence-digest"))))
        (error 'validate-milestone-document "met criterion requires a 64-hex evidence digest"))
      (set! criterion-ids (cons (hash-ref criterion "id") criterion-ids)))
    (unless (= (length criterion-ids) (length (remove-duplicates criterion-ids string=?)))
      (error 'validate-milestone-document
             "criterion ids must be unique within ~a"
             (hash-ref item "id")))
    (when (and (string=? (hash-ref item "status") "complete")
               (ormap (lambda (criterion) (not (hash-ref criterion "met"))) criteria))
      (error 'validate-milestone-document "complete work item requires every criterion met"))
    (when (and (string=? (hash-ref item "status") "planned")
               (ormap (lambda (criterion) (hash-ref criterion "met")) criteria))
      (error 'validate-milestone-document "planned work item cannot have met criteria")))

  (define remaining (hash-ref document "acceptance-critical-remaining-items"))
  (unless (and (list? remaining) (andmap non-empty-string? remaining))
    (error 'validate-milestone-document
           "acceptance-critical-remaining-items must be an array of non-empty strings"))
  (unless (= (length remaining) (length (remove-duplicates remaining string=?)))
    (error 'validate-milestone-document "acceptance-critical remaining items must be unique"))

  (define review (hash-ref document "review"))
  (require-exact-keys 'validate-milestone-document review '("evidence-digest" "independent" "status"))
  (unless (member-string? (hash-ref review "status") '("pending" "approved" "rejected"))
    (error 'validate-milestone-document "review status is invalid"))
  (unless (boolean? (hash-ref review "independent"))
    (error 'validate-milestone-document "review independent must be boolean"))
  (unless (evidence-or-null? (hash-ref review "evidence-digest"))
    (error 'validate-milestone-document "review evidence-digest must be null or 64-hex"))
  (when (and (string=? (hash-ref review "status") "pending")
             (not (eq? (hash-ref review "evidence-digest") 'null)))
    (error 'validate-milestone-document "pending review evidence-digest must be null"))
  (when (and (not (string=? (hash-ref review "status") "pending"))
             (not (digest? (hash-ref review "evidence-digest"))))
    (error 'validate-milestone-document "completed review requires a 64-hex evidence digest"))

  (define validation (hash-ref document "validation"))
  (require-exact-keys 'validate-milestone-document validation '("evidence-digest" "status"))
  (unless (member-string? (hash-ref validation "status") '("pending" "passed" "failed"))
    (error 'validate-milestone-document "validation status is invalid"))
  (unless (evidence-or-null? (hash-ref validation "evidence-digest"))
    (error 'validate-milestone-document "validation evidence-digest must be null or 64-hex"))
  (when (and (string=? (hash-ref validation "status") "pending")
             (not (eq? (hash-ref validation "evidence-digest") 'null)))
    (error 'validate-milestone-document "pending validation evidence-digest must be null"))
  (when (and (not (string=? (hash-ref validation "status") "pending"))
             (not (digest? (hash-ref validation "evidence-digest"))))
    (error 'validate-milestone-document "completed validation requires a 64-hex evidence digest"))
  #t)

;; Pure truth evaluator ------------------------------------------------------

(struct milestone-truth
        (milestone declared-release-mechanics
                   derived-release-mechanics
                   effective-release-mechanics
                   declared-substantive-acceptance
                   derived-substantive-acceptance
                   effective-substantive-acceptance
                   accepted?
                   reasons
                   projection-digest
                   digest-matches?)
  #:transparent)

(define (derived-release-mechanics document)
  (list-ref release-mechanics-values (length (hash-ref document "release-evidence"))))

(define (evaluate-milestone-truth document external-projection-digest)
  (validate-milestone-document document)
  (define canonical-digest (canonical-json-sha256 document))
  (define digest-matches?
    (and (digest? external-projection-digest) (string=? canonical-digest external-projection-digest)))
  (define declared-release (hash-ref document "release-mechanics"))
  (define derived-release (derived-release-mechanics document))
  (define release-consistent? (string=? declared-release derived-release))
  (define work-items (hash-ref document "work-items"))
  (define statuses (map (lambda (item) (hash-ref item "status")) work-items))
  (define all-complete? (andmap (lambda (status) (string=? status "complete")) statuses))
  (define all-planned? (andmap (lambda (status) (string=? status "planned")) statuses))
  (define all-criteria-met?
    (for*/and ([item (in-list work-items)]
               [criterion (in-list (hash-ref item "criteria"))])
      (hash-ref criterion "met")))
  (define remaining (hash-ref document "acceptance-critical-remaining-items"))
  (define review (hash-ref document "review"))
  (define validation (hash-ref document "validation"))
  (define review-approved? (string=? (hash-ref review "status") "approved"))
  (define independent? (hash-ref review "independent"))
  (define review-evidenced? (digest? (hash-ref review "evidence-digest")))
  (define validation-passed? (string=? (hash-ref validation "status") "passed"))
  (define validation-evidenced? (digest? (hash-ref validation "evidence-digest")))
  (define acceptance-gates-pass?
    (and all-complete?
         all-criteria-met?
         (null? remaining)
         review-approved?
         independent?
         review-evidenced?
         validation-passed?
         validation-evidenced?
         digest-matches?))
  (define derived-substantive
    (cond
      [acceptance-gates-pass? "accepted"]
      [(or (string=? (hash-ref review "status") "rejected")
           (string=? (hash-ref validation "status") "failed")
           (not digest-matches?)
           all-complete?)
       "rejected"]
      [(and all-planned?
            (string=? (hash-ref review "status") "pending")
            (string=? (hash-ref validation "status") "pending"))
       "planned"]
      [else "in-progress"]))
  (define declared-substantive (hash-ref document "substantive-acceptance"))
  (define substantive-consistent? (string=? declared-substantive derived-substantive))
  (define effective-substantive
    (if (and release-consistent? substantive-consistent?) derived-substantive "rejected"))
  (define reasons
    (append (if all-complete?
                '()
                '("work records W0-W10 are not all complete"))
            (if all-criteria-met?
                '()
                '("not all criteria are met"))
            (if (null? remaining)
                '()
                '("acceptance-critical remaining items are not empty"))
            (if review-approved?
                '()
                '("review is not approved"))
            (if independent?
                '()
                '("review is not independent"))
            (if review-evidenced?
                '()
                '("review does not have a 64-hex evidence digest"))
            (if validation-passed?
                '()
                '("validation has not passed"))
            (if validation-evidenced?
                '()
                '("validation does not have a 64-hex evidence digest"))
            (if digest-matches?
                '()
                '("projection digest does not match canonical projection"))
            (if release-consistent?
                '()
                (list (format "declared release-mechanics ~a differs from derived ~a"
                              declared-release
                              derived-release)))
            (if substantive-consistent?
                '()
                (list (format "declared substantive-acceptance ~a differs from derived ~a"
                              declared-substantive
                              derived-substantive)))))
  (milestone-truth (hash-ref document "milestone")
                   declared-release
                   derived-release
                   (if release-consistent? derived-release "inconsistent")
                   declared-substantive
                   derived-substantive
                   effective-substantive
                   (string=? effective-substantive "accepted")
                   reasons
                   canonical-digest
                   digest-matches?))

(define (evaluate-milestone-file path external-projection-digest)
  ;; The expected digest is intentionally a separate argument and is never read
  ;; from, or synthesized out of, the milestone file by this boundary.
  (evaluate-milestone-truth (strict-json-read-file path) external-projection-digest))

(define (milestone-truth-gate-passed? truth)
  ;; The standalone command is an acceptance gate by default. Keep every
  ;; condition explicit so a synthetic or stale result cannot pass merely by
  ;; setting accepted? while declarations disagree with their derivations.
  (and (milestone-truth? truth)
       (string? (milestone-truth-milestone truth))
       (regexp-match? #px"^v[0-9]+\\.[0-9]+\\.[0-9]+$" (milestone-truth-milestone truth))
       (digest? (milestone-truth-projection-digest truth))
       (eq? (milestone-truth-digest-matches? truth) #t)
       (equal? (milestone-truth-declared-release-mechanics truth)
               (milestone-truth-derived-release-mechanics truth))
       (equal? (milestone-truth-effective-release-mechanics truth)
               (milestone-truth-derived-release-mechanics truth))
       (equal? (milestone-truth-declared-substantive-acceptance truth)
               (milestone-truth-derived-substantive-acceptance truth))
       (equal? (milestone-truth-effective-substantive-acceptance truth)
               (milestone-truth-derived-substantive-acceptance truth))
       (equal? (milestone-truth-effective-substantive-acceptance truth) "accepted")
       (eq? (milestone-truth-accepted? truth) #t)))

(define (milestone-truth->jsexpr truth)
  (hash "milestone"
        (milestone-truth-milestone truth)
        "release-mechanics"
        (hash "declared"
              (milestone-truth-declared-release-mechanics truth)
              "derived"
              (milestone-truth-derived-release-mechanics truth)
              "effective"
              (milestone-truth-effective-release-mechanics truth))
        "substantive-acceptance"
        (hash "declared"
              (milestone-truth-declared-substantive-acceptance truth)
              "derived"
              (milestone-truth-derived-substantive-acceptance truth)
              "effective"
              (milestone-truth-effective-substantive-acceptance truth))
        "accepted"
        (milestone-truth-accepted? truth)
        "reasons"
        (milestone-truth-reasons truth)
        "projection-digest"
        (milestone-truth-projection-digest truth)
        "digest-matches"
        (milestone-truth-digest-matches? truth)))

(module+ main
  (define supplied-digest #f)
  (define report-only? #f)
  (command-line
   #:program "gsd-milestone-truth.rkt"
   #:usage-help
   "Default gate: exits nonzero unless digest-bound effective acceptance is accepted and declarations are consistent."
   "Use --report-only only to inspect valid in-progress or rejected history."
   #:once-each [("-d" "--digest")
                digest
                "independently supplied canonical projection SHA-256 (required)"
                (set! supplied-digest digest)]
   [("--report-only")
    "inspect valid in-progress/rejected history without gate exit semantics"
    (set! report-only? #t)]
   #:args (json-file)
   (unless supplied-digest
     (raise-user-error 'gsd-milestone-truth.rkt "--digest is required"))
   (let ([truth (evaluate-milestone-file json-file supplied-digest)])
     (write-bytes (canonical-json-bytes (milestone-truth->jsexpr truth)))
     (newline)
     (unless (or report-only? (milestone-truth-gate-passed? truth))
       (exit 1)))))
