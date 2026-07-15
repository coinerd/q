#lang racket/base

;; scripts/gsd-wave-gate.rkt — Machine-checkable per-wave evidence gate.
;; A wave evidence record is a single readable hash datum. Empty, malformed,
;; stale-review, non-approved, and Not-started evidence fails closed.

(require racket/file
         racket/list
         racket/match
         racket/string)

(provide (struct-out wave-evidence-result)
         validate-wave-evidence
         validate-wave-evidence-file
         main)

(struct wave-evidence-result (passed? reasons evidence) #:transparent)

(define sha-pattern #px"^[0-9a-f]{40}$")

(define (non-empty-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

(define (valid-sha? value)
  (and (non-empty-string? value) (regexp-match? sha-pattern value)))

(define (nested-hash evidence key)
  (define value (and (hash? evidence) (hash-ref evidence key #f)))
  (and (hash? value) value))

(define (validate-wave-evidence evidence)
  (define reasons '())
  (define (reject! reason)
    (set! reasons (cons reason reasons)))
  (cond
    [(not (hash? evidence)) (reject! "evidence is not a hash")]
    [else
     (unless (equal? (hash-ref evidence 'schema-version #f) 1)
       (reject! "schema-version must be 1"))
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

     (define red-first (nested-hash evidence 'red-first))
     (unless red-first
       (reject! "red-first evidence is missing or malformed"))
     (when red-first
       (unless (non-empty-string? (hash-ref red-first 'command #f))
         (reject! "red-first command is missing"))
       (unless (non-empty-string? (hash-ref red-first 'failure #f))
         (reject! "red-first failure is missing")))

     (define validation (nested-hash evidence 'validation))
     (unless validation
       (reject! "validation evidence is missing or malformed"))
     (when validation
       (unless (non-empty-string? (hash-ref validation 'path #f))
         (reject! "validation path is missing"))
       (define status (hash-ref validation 'status #f))
       (unless (equal? status "current")
         (reject! "validation status must be current")))

     (define review (nested-hash evidence 'review))
     (unless review
       (reject! "review evidence is missing or malformed"))
     (when review
       (unless (non-empty-string? (hash-ref review 'reviewer #f))
         (reject! "independent reviewer identity is missing"))
       (unless (equal? (hash-ref review 'verdict #f) "APPROVED")
         (reject! "review verdict must be APPROVED"))
       (unless (non-empty-string? (hash-ref review 'artifact #f))
         (reject! "review artifact is missing"))
       (define reviewed-sha (hash-ref review 'reviewed-sha #f))
       (unless (valid-sha? reviewed-sha)
         (reject! "reviewed-sha must be a full lowercase Git SHA"))
       (unless (and (valid-sha? implementation-sha) (equal? reviewed-sha implementation-sha))
         (reject! "review is stale: reviewed-sha differs from implementation-sha")))

     (define required-checks (hash-ref evidence 'required-checks #f))
     (unless (and (list? required-checks)
                  (pair? required-checks)
                  (andmap non-empty-string? required-checks)
                  (= (length required-checks) (length (remove-duplicates required-checks))))
       (reject! "required-checks must be a non-empty unique string list"))])
  (wave-evidence-result (null? reasons) (reverse reasons) evidence))

(define (validate-wave-evidence-file path)
  (with-handlers ([exn:fail? (lambda (error)
                               (wave-evidence-result #f
                                                     (list (format "could not read evidence: ~a"
                                                                   (exn-message error)))
                                                     path))])
    (unless (file-exists? path)
      (error 'gsd-wave-gate "evidence file does not exist: ~a" path))
    (call-with-input-file
     path
     (lambda (in)
       (define evidence (read in))
       (when (eof-object? evidence)
         (error 'gsd-wave-gate "evidence file is empty: ~a" path))
       (define trailing (read in))
       (unless (eof-object? trailing)
         (error 'gsd-wave-gate "evidence file contains multiple datums: ~a" path))
       (validate-wave-evidence evidence)))))

(define (print-usage)
  (displayln "Usage: racket scripts/gsd-wave-gate.rkt <wave-evidence.rktd>"))

(define (main args)
  (match args
    [(list path)
     (define result (validate-wave-evidence-file path))
     (if (wave-evidence-result-passed? result)
         (begin
           (printf "GSD wave evidence PASS: ~a~n" path)
           0)
         (begin
           (printf "GSD wave evidence FAIL: ~a~n" path)
           (for ([reason (in-list (wave-evidence-result-reasons result))])
             (printf "  - ~a~n" reason))
           1))]
    [_
     (print-usage)
     1]))

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))
