#lang racket/base

;; scripts/gsd-wave-gate.rkt — Machine-checkable per-wave evidence gate.
;; Evidence is bound to an actual changed-content digest computed by CI. The
;; retained review and validation artifacts are read and cross-checked rather
;; than trusted as self-asserted path strings.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string)

(provide (struct-out wave-evidence-result)
         validate-wave-evidence
         validate-wave-evidence-file
         main)

(struct wave-evidence-result (passed? reasons evidence) #:transparent)

(define sha-pattern #px"^[0-9a-f]{40}$")
(define digest-pattern #px"^[0-9a-f]{64}$")

(define validation-required-fields
  '(status milestone
           wave
           issue
           branch
           implementation-sha
           content-digest
           red-first
           focused-tests
           format-compile
           lint
           fast
           review-artifact
           remaining-items
           planning-sync))

(define (non-empty-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

(define (valid-sha? value)
  (and (non-empty-string? value) (regexp-match? sha-pattern value)))

(define (valid-digest? value)
  (and (non-empty-string? value) (regexp-match? digest-pattern value)))

;; Register F12: a sentinel/placeholder is not evidence. A record whose
;; identity or narrative fields carry PENDING/TODO-style placeholders can
;; never pass the strict gate, so "finalized" is unreachable without genuine
;; content. Typed refusal: placeholder-evidence.
(define placeholder-pattern
  #px"(?i:^\\s*(?:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx|n/?a)[:.;!,?]*\\s*$)")

(define (placeholder? value)
  (and (string? value) (regexp-match? placeholder-pattern value)))

;; Substantive minimum content (F12): review scope/report must carry at least
;; a sentence, the red-first failure an actual observed failure, so a
;; "finalized" record cannot be reached by flipping status fields alone.
(define minimum-review-narrative-length 64)
(define minimum-red-first-failure-length 32)

(define (require-genuine! reject! label value minimum)
  (cond
    [(not (non-empty-string? value)) (reject! (format "~a is missing" label))]
    [(placeholder? value) (reject! (format "placeholder-evidence: ~a is a placeholder" label))]
    [(< (string-length (string-trim value)) minimum)
     (reject! (format "insufficient-review-content: ~a carries fewer than ~a characters of content"
                      label
                      minimum))]))

(define (read-single-datum path)
  (call-with-input-file path
                        (lambda (in)
                          (define datum (read in))
                          (when (eof-object? datum)
                            (error 'gsd-wave-gate "file is empty: ~a" path))
                          (unless (eof-object? (read in))
                            (error 'gsd-wave-gate "file contains multiple datums: ~a" path))
                          datum)))

(define (safe-relative-artifact root value)
  (and (non-empty-string? value)
       (let ([p (string->path value)])
         (and (relative-path? p) (not (member 'up (explode-path p))) (build-path root p)))))

(define (passed-result-hash? value)
  (and (hash? value) (equal? (hash-ref value 'result #f) "passed")))

(define (normalized-label value)
  (and (or (symbol? value) (string? value))
       (string-upcase (if (symbol? value)
                          (symbol->string value)
                          value))))

(define (hash-ref/label table label [failure #f])
  (or (for/first ([(key value) (in-hash table)]
                  #:when (equal? (normalized-label key) label))
        value)
      failure))

(define (normalized-classification value)
  (and (or (symbol? value) (string? value))
       (string-downcase (string-trim (if (symbol? value)
                                         (symbol->string value)
                                         value)))))

(define (acceptance-critical-item? item)
  (and (hash? item)
       (for/or ([label (in-list '("ACCEPTANCE-CRITICAL" "ACCEPTANCE-CRITICAL?"
                                                        "CLASSIFICATION"
                                                        "CATEGORY"
                                                        "CRITICALITY"))])
         (define value (hash-ref/label item label #f))
         (or (eq? value #t)
             (member (normalized-classification value)
                     '("acceptance-critical" "acceptance_critical"))))))

(define (valid-noncritical-remaining-item? item)
  (and (hash? item)
       (not (acceptance-critical-item? item))
       (member (normalized-classification (hash-ref/label item "CLASSIFICATION" #f))
               '("noncritical" "deferred-noncritical"))
       (non-empty-string? (hash-ref/label item "OWNER" #f))
       (non-empty-string? (hash-ref/label item "RATIONALE" #f))))

(define (known-acceptance-evidence? value)
  (and (non-empty-string? value)
       (member (string-downcase (string-trim value))
               '("verified" "pass" "passed" "present" "confirmed" "complete" "completed" "known"))))

(define (validate-wave-evidence evidence
                                #:root [root (current-directory)]
                                #:actual-content-digest [actual-content-digest #f]
                                #:receipt-head [receipt-head #f]
                                #:policy [policy-path
                                          (build-path root "scripts/required-pr-checks.policy")])
  (define reasons '())
  (define (reject! reason)
    (set! reasons (cons reason reasons)))
  (define root-path (path->complete-path root))
  ;; The acceptance block is an optional schema-v2 extension so committed
  ;; pre-extension fixtures remain readable. Its planning digests establish
  ;; projection consistency only; exact-head authenticity is not inferred
  ;; from committed evidence (the content boundary is CI-supplied).
  (define acceptance-reference #f)
  (define (validate-acceptance! acceptance expected-wave owner)
    (unless (hash? acceptance)
      (reject! (format "~a acceptance extension must be a hash" owner)))
    (when (hash? acceptance)
      (when (and acceptance-reference (not (equal? acceptance acceptance-reference)))
        (reject! (format "~a acceptance extension differs from other artifacts" owner)))
      (unless acceptance-reference
        (set! acceptance-reference acceptance))
      (unless (equal? (hash-ref acceptance 'status #f) "accepted")
        (reject! (format "~a acceptance status must be accepted" owner)))
      (define acceptance-wave (hash-ref acceptance 'wave #f))
      (unless (and (non-empty-string? acceptance-wave)
                   (regexp-match? #px"^W[0-9]+$" acceptance-wave)
                   (equal? acceptance-wave expected-wave))
        (reject! (format "~a acceptance wave is missing or differs from evidence" owner)))
      (unless (non-empty-string? (hash-ref acceptance 'criterion #f))
        (reject! (format "~a acceptance criterion is missing" owner)))
      (unless (known-acceptance-evidence? (hash-ref acceptance 'evidence #f))
        (reject! (format "~a acceptance evidence is missing or unknown" owner)))
      (define projections (hash-ref acceptance 'projections #f))
      (unless (hash? projections)
        (reject! (format "~a acceptance projections must be a hash" owner)))
      (when (hash? projections)
        (define expected-projections '("STATE" "VALIDATION" "HANDOFF"))
        (define actual-labels
          (for/list ([key (in-hash-keys projections)])
            (normalized-label key)))
        (unless (and (= (length actual-labels) (length expected-projections))
                     (andmap (lambda (label) (member label expected-projections)) actual-labels))
          (reject!
           (format "~a acceptance projections must contain only STATE, VALIDATION, and HANDOFF"
                   owner)))
        (define projection-digests
          (for/list ([label (in-list expected-projections)])
            (define projection (hash-ref/label projections label))
            (unless (hash? projection)
              (reject! (format "~a ~a projection must be a hash" owner label)))
            (define status (and (hash? projection) (hash-ref projection 'status #f)))
            (define digest (and (hash? projection) (hash-ref projection 'digest #f)))
            (unless (equal? status "current")
              (reject! (format "~a ~a projection is stale" owner label)))
            (unless (valid-digest? digest)
              (reject! (format "~a ~a projection digest is invalid" owner label)))
            digest))
        (when (and (andmap valid-digest? projection-digests)
                   (not (andmap (lambda (digest) (equal? digest (car projection-digests)))
                                (cdr projection-digests))))
          (reject! (format "~a STATE/VALIDATION/HANDOFF projection digests differ" owner))))))
  (define (read-artifact key label)
    (define value (and (hash? evidence) (hash-ref evidence key #f)))
    (define path (safe-relative-artifact root-path value))
    (cond
      [(not path)
       (reject! (format "~a path is missing or unsafe" label))
       #f]
      [(not (file-exists? path))
       (reject! (format "~a does not exist: ~a" label value))
       #f]
      [else
       (with-handlers ([exn:fail?
                        (lambda (error)
                          (reject! (format "~a is unreadable: ~a" label (exn-message error)))
                          #f)])
         (read-single-datum path))]))
  (cond
    [(not (hash? evidence)) (reject! "evidence is not a hash")]
    [else
     (unless (equal? (hash-ref evidence 'schema-version #f) 2)
       (reject! "schema-version must be 2"))
     (unless (exact-positive-integer? (hash-ref evidence 'milestone #f))
       (reject! "milestone must be a positive integer"))
     (define wave (hash-ref evidence 'wave #f))
     (unless (and (non-empty-string? wave) (regexp-match? #px"^W[0-9]+$" wave))
       (reject! "wave must be W followed by digits"))
     (unless (exact-positive-integer? (hash-ref evidence 'issue #f))
       (reject! "issue must be a positive integer"))
     (unless (equal? (hash-ref evidence 'status #f) "ready-for-merge")
       (reject! "status must be ready-for-merge"))
     (define implementation-sha (hash-ref evidence 'implementation-sha #f))
     (unless (valid-sha? implementation-sha)
       (reject! "implementation-sha must be a full lowercase Git SHA"))
     ;; Register F3: the evidence head is bound to the durable receipt head
     ;; recorded by verify-with-delivery-receipt. The check is conditional so
     ;; already-delivered historical trios remain readable and verifiable.
     (when (and (valid-sha? receipt-head) (valid-sha? implementation-sha))
       (unless (equal? implementation-sha receipt-head)
         (reject!
          (format
           "head-binding-mismatch: evidence implementation-sha ~a differs from the durable receipt head ~a"
           implementation-sha
           receipt-head))))
     (define content-digest (hash-ref evidence 'content-digest #f))
     (unless (valid-digest? content-digest)
       (reject! "content-digest must be a lowercase SHA-256"))
     (unless (and (valid-digest? actual-content-digest) (equal? content-digest actual-content-digest))
       (reject! "actual changed-content digest does not match evidence"))

     (when (hash-has-key? evidence 'acceptance)
       (validate-acceptance! (hash-ref evidence 'acceptance) wave "evidence"))

     (define required-checks (hash-ref evidence 'required-checks #f))
     (define policy
       (with-handlers ([exn:fail? (lambda (error)
                                    (reject! (format "required-check policy is unreadable: ~a"
                                                     (exn-message error)))
                                    #f)])
         (read-single-datum policy-path)))
     (unless (and (list? required-checks)
                  (pair? required-checks)
                  (andmap non-empty-string? required-checks)
                  (= (length required-checks) (length (remove-duplicates required-checks))))
       (reject! "required-checks must be a non-empty unique string list"))
     (unless (and (list? policy) (equal? required-checks policy))
       (reject! "required-checks differ from authoritative policy"))

     (define review (read-artifact 'review-artifact "review artifact"))
     (unless (hash? review)
       (reject! "review artifact must contain a hash"))
     (when (hash? review)
       (require-genuine! reject! "independent reviewer identity" (hash-ref review 'reviewer #f) 1)
       ;; A compound sentinel ("PENDING-INDEPENDENT-REVIEW") is not a whole-string
       ;; placeholder, but it still encodes placeholder semantics in the one field
       ;; that must name a genuine reviewer (register F12).
       (when (and
              (string? (hash-ref review 'reviewer #f))
              (regexp-match?
               #px"(?i:(?:^|[^a-z])(?:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx)(?:[^a-z]|$))"
               (hash-ref review 'reviewer "")))
         (reject! "placeholder-evidence: independent reviewer identity encodes a placeholder"))
       (unless (equal? (hash-ref review 'verdict #f) "APPROVED")
         (reject! "review verdict must be APPROVED"))
       (unless (and (valid-sha? (hash-ref review 'reviewed-sha #f))
                    (equal? (hash-ref review 'reviewed-sha #f) implementation-sha))
         (reject! "reviewed-sha differs from implementation-sha"))
       (when (and (valid-sha? receipt-head) (valid-sha? (hash-ref review 'reviewed-sha #f)))
         (unless (equal? (hash-ref review 'reviewed-sha #f) receipt-head)
           (reject!
            (format
             "head-binding-mismatch: review reviewed-sha ~a differs from the durable receipt head ~a"
             (hash-ref review 'reviewed-sha #f)
             receipt-head))))
       (unless (and (valid-digest? (hash-ref review 'content-digest #f))
                    (equal? (hash-ref review 'content-digest #f) content-digest))
         (reject! "review content digest differs from evidence"))
       (require-genuine! reject! "review timestamp" (hash-ref review 'timestamp #f) 1)
       (require-genuine! reject!
                         "review scope"
                         (hash-ref review 'scope #f)
                         minimum-review-narrative-length)
       (require-genuine! reject!
                         "review report"
                         (hash-ref review 'report #f)
                         minimum-review-narrative-length)
       (when (hash-has-key? review 'acceptance)
         (validate-acceptance! (hash-ref review 'acceptance) wave "review")))

     (define validation (read-artifact 'validation-artifact "validation artifact"))
     (unless (hash? validation)
       (reject! "validation artifact must contain a hash"))
     (when (hash? validation)
       (for ([key (in-list validation-required-fields)])
         (unless (hash-has-key? validation key)
           (reject! (format "validation field ~a is missing" key))))
       (unless (equal? (hash-ref validation 'status #f) "current")
         (reject! "validation status must be current"))
       (for ([key (in-list '(milestone wave issue implementation-sha content-digest))])
         (unless (equal? (hash-ref validation key #f) (hash-ref evidence key #f))
           (reject! (format "validation ~a differs from evidence" key))))
       (unless (non-empty-string? (hash-ref validation 'branch #f))
         (reject! "validation branch is missing"))
       (unless (equal? (hash-ref validation 'planning-sync #f) "current")
         (reject! "validation planning-sync must be current"))
       (define remaining-items (hash-ref validation 'remaining-items #f))
       (unless (list? remaining-items)
         (reject! "validation remaining-items must be a list"))
       (when (list? remaining-items)
         (cond
           [(ormap acceptance-critical-item? remaining-items)
            (reject! "validation has acceptance-critical remaining-items")]
           [(ormap (lambda (item)
                     (and (hash? item)
                          (or (placeholder? (hash-ref/label item "OWNER" #f))
                              (placeholder? (hash-ref/label item "RATIONALE" #f)))))
                   remaining-items)
            (reject! "placeholder-evidence: remaining-items owner or rationale is a placeholder")]
           [(not (andmap valid-noncritical-remaining-item? remaining-items))
            (reject! (string-append
                      "validation remaining-items must be structured noncritical or "
                      "deferred-noncritical items with nonempty owner and rationale"))]))
       (when (hash-has-key? validation 'acceptance)
         (validate-acceptance! (hash-ref validation 'acceptance) wave "validation"))
       (define red-first (hash-ref validation 'red-first #f))
       (unless (hash? red-first)
         (reject! "validation red-first evidence is incomplete"))
       (when (hash? red-first)
         (require-genuine! reject! "validation red-first command" (hash-ref red-first 'command #f) 1)
         (require-genuine! reject!
                           "validation red-first failure"
                           (hash-ref red-first 'failure #f)
                           minimum-red-first-failure-length))
       (unless (passed-result-hash? (hash-ref validation 'focused-tests #f))
         (reject! "validation focused-tests are incomplete"))
       (for ([key (in-list '(format-compile lint fast))])
         (unless (passed-result-hash? (hash-ref validation key #f))
           (reject! (format "validation ~a result is not passed" key))))
       (unless (equal? (hash-ref validation 'review-artifact #f)
                       (hash-ref evidence 'review-artifact #f))
         (reject! "validation review artifact differs from evidence")))])
  (wave-evidence-result (null? reasons) (reverse reasons) evidence))

(define (validate-wave-evidence-file path
                                     #:root [root (current-directory)]
                                     #:actual-content-digest [actual-content-digest #f]
                                     #:receipt-head [receipt-head #f]
                                     #:policy [policy-path
                                               (build-path root "scripts/required-pr-checks.policy")])
  (with-handlers ([exn:fail? (lambda (error)
                               (wave-evidence-result #f
                                                     (list (format "could not read evidence: ~a"
                                                                   (exn-message error)))
                                                     path))])
    (unless (file-exists? path)
      (error 'gsd-wave-gate "evidence file does not exist: ~a" path))
    (validate-wave-evidence (read-single-datum path)
                            #:root root
                            #:actual-content-digest actual-content-digest
                            #:receipt-head receipt-head
                            #:policy policy-path)))

(define (print-usage)
  (displayln (string-append
              "Usage: racket scripts/gsd-wave-gate.rkt <evidence.rktd> --content-digest <sha256>"
              " [--root <dir>] [--policy <file>] [--receipt-head <sha>]")))

;; Keyword-style option parse; the evidence path is the single positional.
;; Unknown flags, a missing --content-digest, or a wrong positional count are
;; usage errors (fail closed).
(define (parse-gate-options args)
  (let loop ([args args]
             [acc (hash)]
             [positional '()])
    (match args
      ['()
       (and (= (length positional) 1)
            (hash-ref acc '--content-digest #f)
            (hash-set acc '--evidence-path (first positional)))]
      [(list "--content-digest" value rest ...)
       (loop rest (hash-set acc '--content-digest value) positional)]
      [(list "--root" value rest ...) (loop rest (hash-set acc '--root value) positional)]
      [(list "--policy" value rest ...) (loop rest (hash-set acc '--policy value) positional)]
      [(list "--receipt-head" value rest ...)
       (loop rest (hash-set acc '--receipt-head value) positional)]
      [(list arg rest ...)
       #:when (not (string-prefix? arg "--"))
       (loop rest acc (cons arg positional))]
      [_
       (print-usage)
       #f])))

(define (main args)
  (define options (parse-gate-options args))
  (if options
      (let ([evidence-path (hash-ref options '--evidence-path)])
        (define result
          (validate-wave-evidence-file
           evidence-path
           #:root (hash-ref options '--root ".")
           #:actual-content-digest (hash-ref options '--content-digest)
           #:receipt-head (hash-ref options '--receipt-head #f)
           #:policy (hash-ref options '--policy "scripts/required-pr-checks.policy")))
        (if (wave-evidence-result-passed? result)
            (begin
              (printf "GSD wave evidence PASS: ~a~n" evidence-path)
              0)
            (begin
              (printf "GSD wave evidence FAIL: ~a~n" evidence-path)
              (for ([reason (in-list (wave-evidence-result-reasons result))])
                (printf "  - ~a~n" reason))
              1)))
      (begin
        (displayln "gsd-wave-gate: malformed or missing arguments; see usage above")
        1)))

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))
