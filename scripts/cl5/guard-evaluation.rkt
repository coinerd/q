#lang racket/base

;; scripts/cl5/guard-evaluation.rkt — advisory evaluation of the isolated
;; fail-closed guard over the frozen corpus changed-file sets (issue #9996).
;;
;; ADVISORY ONLY. Nothing here runs `./qt quick`, executes tests, writes the
;; admission record, or changes selection/scheduling/CI policy. It answers:
;; "for the corpus changes we froze, what would the advisory guard decide, and
;; does it ever report a green without selectable, freshly admitted evidence?"
;;
;; Corpus source of truth: scripts/cl5/selection-cases.rkt (the executable CL0
;; cases) plus the coverage-gap audit for the declared detectors. Clean controls
;; come from the audit's covered rows so the evaluation also shows the guard does
;; not escalate a change that genuinely has an audited detector.

(require json
         racket/file
         racket/list
         racket/path
         "ownership-audit.rkt"
         "selection-cases.rkt"
         "selection-guard.rkt")

(provide corpus-changes
         clean-controls
         evaluate-guard-corpus
         evaluation->jsexpr
         run-guard-evaluation)

;; The frozen executable cases: one entry per corpus case id with its
;; registered changed-file set (the same data the W2 driver applies).
(define (corpus-changes)
  (for/list ([case (in-list selection-cases)])
    (hash 'case
          (hash-ref case 'id)
          'changed
          (hash-ref case 'changed)
          'expect
          (hash-ref case 'expect))))

;; A few low-fan-in covered modules: changes that DO have an audited detector.
;; Deterministic: the audit's `covered` class, path-sorted, first three.
(define (clean-controls snapshot #:count [count 3])
  (define covered
    (sort (for/list ([row (in-list (hash-ref snapshot 'rows))]
                     #:when (eq? (hash-ref row 'class) 'covered))
            (hash-ref row 'module))
          string<?))
  (take covered (min count (length covered))))

(define (escalation-codes decision)
  (map (lambda (e) (hash-ref e 'code)) (hash-ref decision 'escalations)))

(define (named-tests decision)
  (append* (for/list ([e (in-list (hash-ref decision 'escalations))]
                      #:when (hash-ref e 'module #f))
             (for/list ([t (in-list (hash-ref e 'tests '()))])
               (if (hash? t)
                   (hash-ref t 'test)
                   t)))))

(define (evaluate-guard-corpus root)
  (define snapshot (audit-snapshot root))
  (define controls (clean-controls snapshot))
  (define cases
    (for/list ([entry (in-list (corpus-changes))])
      (define result (guard-decide-repo root (hash-ref entry 'changed)))
      (define decision (hash-ref result 'decision))
      (hash 'case
            (hash-ref entry 'case)
            'kind
            'corpus
            'changed
            (hash-ref entry 'changed)
            'verdict
            (hash-ref decision 'verdict)
            'lane
            (hash-ref decision 'lane)
            'claims
            (hash-ref decision 'claims)
            'escalations
            (escalation-codes decision)
            'named-tests
            (remove-duplicates (named-tests decision))
            'selected
            (hash-ref decision 'selected)
            'evidence
            (hash-ref decision 'evidence))))
  (define control-results
    (for/list ([module (in-list controls)])
      (define result (guard-decide-repo root (list module)))
      (define decision (hash-ref result 'decision))
      (hash 'case
            (string->symbol (format "control:~a" module))
            'kind
            'control
            'changed
            (list module)
            'verdict
            (hash-ref decision 'verdict)
            'lane
            (hash-ref decision 'lane)
            'claims
            (hash-ref decision 'claims)
            'escalations
            (escalation-codes decision)
            'named-tests
            (remove-duplicates (named-tests decision))
            'selected
            (hash-ref decision 'selected)
            'evidence
            (hash-ref decision 'evidence))))
  (hash 'corpus
        cases
        'controls
        control-results
        'owners
        (hash-ref snapshot 'owner-count)
        'confirmed-gaps
        (length (hash-ref snapshot 'confirmed-gaps))))

;; Advisories are safe by construction when: every corpus change escalates with
;; at least one named reason, no corpus change reports 'selected, and every
;; clean control with an audited detector is selected without escalation.
(define (evaluation->jsexpr evaluation)
  (define (row->jsexpr row)
    (hash 'case
          (symbol->string (hash-ref row 'case))
          'kind
          (symbol->string (hash-ref row 'kind))
          'changed
          (hash-ref row 'changed)
          'verdict
          (symbol->string (hash-ref row 'verdict))
          'lane
          (symbol->string (hash-ref row 'lane))
          'escalations
          (map symbol->string (hash-ref row 'escalations))
          'named-tests
          (hash-ref row 'named-tests)
          'selected
          (hash-ref row 'selected)))
  (hash 'tool
        "scripts/cl5/guard-evaluation.rkt"
        'kind
        "advisory guard evaluation (CL5 W2 follow-up #9996)"
        'owners
        (hash-ref evaluation 'owners)
        'confirmed-gaps
        (hash-ref evaluation 'confirmed-gaps)
        'corpus
        (map row->jsexpr (hash-ref evaluation 'corpus))
        'controls
        (map row->jsexpr (hash-ref evaluation 'controls))))

(define (run-guard-evaluation root #:json-out [json-out #f])
  (define evaluation (evaluate-guard-corpus root))
  (for ([row (in-list (hash-ref evaluation 'corpus))])
    (printf "~a [~a] -> ~a (~a) escalations=~a\n"
            (hash-ref row 'case)
            (hash-ref row 'kind)
            (hash-ref row 'verdict)
            (hash-ref row 'lane)
            (hash-ref row 'escalations)))
  (for ([row (in-list (hash-ref evaluation 'controls))])
    (printf "~a [~a] -> ~a (~a) escalations=~a\n"
            (hash-ref row 'case)
            (hash-ref row 'kind)
            (hash-ref row 'verdict)
            (hash-ref row 'lane)
            (hash-ref row 'escalations)))
  (when json-out
    (call-with-output-file json-out
                           (lambda (out)
                             (write-json (evaluation->jsexpr evaluation) out)
                             (newline out))
                           #:exists 'truncate/replace)
    (printf "\nwrote ~a\n" json-out))
  evaluation)
