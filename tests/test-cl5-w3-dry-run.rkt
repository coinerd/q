#lang racket

;; Tests for scripts/cl5/w3-dry-run.rkt (CL5 W3.1 dry run).
;; Pure: plan parsing, digests, taxonomy, timing refusal, record verification.
;; No git, no network, no child processes, no file writes.

;; @suite default
;; @speed fast
;; @boundary unit

(require rackunit
         racket/list
         "../scripts/cl5/w3-dry-run.rkt")

;; ---- explicit plan parsing ---------------------------------------------------------

(define header
  (string-append "lane: fast\nlabel: fast only\n"
                 "selection: 2 files (selection only; nothing executed)\n"
                 "timeouts: @timeout metadata\n"))

(test-case "a plan's declared count and payload must agree"
  (define plan (parse-explain-files (string-append header "tests/b.rkt\ntests/a.rkt\n")))
  (check-equal? (hash-ref plan 'declared) 2)
  (check-equal? (hash-ref plan 'files) (list "tests/a.rkt" "tests/b.rkt")))

(test-case "a plan footer after the block does not contaminate the payload"
  (define text
    (string-append header
                   "tests/a.rkt\ntests/b.rkt\n"
                   "NOT PROVIDED by ./qt --lane (counted by other gates):\n"
                   "  - PR/CI verification of this commit\n"))
  (define plan (parse-explain-files text))
  (check-equal? (hash-ref plan 'files) (list "tests/a.rkt" "tests/b.rkt")))

(test-case "an extra path beyond the declared count is refused"
  (check-exn exn:fail?
             (lambda ()
               (parse-explain-files (string-append "selection: 1 files\n"
                                                   "tests/a.rkt\ntests/b.rkt\n")))))

(test-case "a payload shorter than the declared count is refused"
  (check-exn exn:fail? (lambda () (parse-explain-files (string-append header "tests/a.rkt\n")))))

(test-case "a plan without a declared count is refused"
  (check-exn exn:fail? (lambda () (parse-explain-files "tests/a.rkt\n"))))

(test-case "a plan whose payload is not a repo-relative Racket path is refused"
  (check-exn exn:fail?
             (lambda ()
               (parse-explain-files
                (string-append "selection: 2 files\n" "tests/a.rkt\n" "not a path\n")))))

(define selected-output
  (string-append "quick: changed-file impact selection\n"
                 "changed files: 1\n"
                 "changed escalations: none\n"
                 "  changed: extensions/gsd/wave-status.rkt\n"
                 "  included: tests/test-a.rkt (attributed to a changed file)\n"
                 "  included: tests/test-b.rkt (no @covers attribution; fail open)\n"
                 "  excluded: tests/test-c.rkt (attributed to none)\n"
                 "tests/test-a.rkt\n"
                 "tests/test-b.rkt\n"))

(test-case "the selected plan's two views must agree"
  (define plan (parse-selected-explain selected-output))
  (check-equal? (hash-ref plan 'files) (list "tests/test-a.rkt" "tests/test-b.rkt")))

(test-case "a selected plan whose views disagree is refused"
  (check-exn exn:fail?
             (lambda ()
               (parse-selected-explain (string-append "  included: tests/test-a.rkt\n"
                                                      "tests/test-b.rkt\n")))))

;; ---- digests -----------------------------------------------------------------------

(test-case "an enumeration digest is order-insensitive and content-sensitive"
  (check-equal? (enumeration-digest (list "tests/a.rkt" "tests/b.rkt"))
                (enumeration-digest (list "tests/b.rkt" "tests/a.rkt")))
  (check-not-equal? (enumeration-digest (list "tests/a.rkt"))
                    (enumeration-digest (list "tests/a.rkt" "tests/b.rkt"))))

;; ---- comprehensive producer combination --------------------------------------------

(define (producer name lane total files)
  (hash 'name name 'lane lane 'shard-total total 'declared (length files) 'files files))

(test-case "shards of one lane must be disjoint"
  (check-exn exn:fail?
             (lambda ()
               (combine-enumerations (list (producer "test (0)" "fast" 2 (list "tests/a.rkt"))
                                           (producer "test (1)" "fast" 2 (list "tests/a.rkt")))))))

(test-case "a producer's declared count is re-checked"
  (check-exn exn:fail?
             (lambda ()
               (combine-enumerations (list (hash 'name
                                                 "test (0)"
                                                 'lane
                                                 "fast"
                                                 'shard-total
                                                 2
                                                 'declared
                                                 2
                                                 'files
                                                 (list "tests/a.rkt")))))))

(test-case "the union deduplicates across producers but keeps every producer digest"
  (define plan
    (combine-enumerations (list (producer "test (0)" "fast" 2 (list "tests/a.rkt" "tests/b.rkt"))
                                (producer "test (1)" "fast" 2 (list "tests/c.rkt"))
                                (producer "security" "security" #f (list "tests/b.rkt")))))
  (check-equal? (hash-ref plan 'files) (list "tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))
  (check-equal? (hash-ref plan 'union-count) 3)
  (check-equal? (length (hash-ref plan 'producers)) 3)
  (check-equal? (hash-ref plan 'digest) (enumeration-digest (hash-ref plan 'files))))

;; ---- decision layer and taxonomy ---------------------------------------------------

(test-case "a revision without the contract layer has no escalation path"
  (check-eq? (decision-instruction (hash 'state 'absent-at-revision)) 'run-required))

(test-case "a present contract layer's instruction is used; unknown is fail-closed"
  ;; F3: an unresolvable instruction is unavailable -- never quietly treated as
  ;; an escalation, because no execution could be labelled from it.
  (check-eq? (decision-instruction (hash 'state 'present 'instruction 'run-comprehensive))
             'run-comprehensive)
  (check-eq? (decision-instruction (hash 'state 'present)) 'unavailable)
  (check-eq? (decision-instruction (hash 'state 'present 'status 'invalid
                                        'problems (list "x")))
             'unavailable)
  (check-eq? (decision-instruction (hash 'state 'unexpected)) 'unavailable))

(test-case "the W3 taxonomy is exhaustive over verdict combinations"
  ;; unavailable control or surfaces never become a detection
  (check-eq? (w3-outcome 'run-required 'fail 'pass 'unavailable) 'unavailable)
  (check-eq? (w3-outcome 'run-required 'fail 'pass 'fail) 'unavailable)
  (check-eq? (w3-outcome 'run-required 'unavailable 'pass 'pass) 'unavailable)
  (check-eq? (w3-outcome 'run-required 'fail 'unavailable 'pass) 'unavailable)
  ;; comprehensive passes: out of scope (the expected historical majority)
  (check-eq? (w3-outcome 'run-required 'pass 'pass 'pass) 'out-of-scope)
  ;; comprehensive fails: the four detected outcomes
  (check-eq? (w3-outcome 'run-required 'fail 'fail 'pass) 'correct-reduced)
  (check-eq? (w3-outcome 'run-required 'fail 'pass 'pass) 'unsafe-reduced)
  (check-eq? (w3-outcome 'run-comprehensive 'fail 'pass 'pass) 'correct-escalation)
  (check-eq? (w3-outcome 'run-comprehensive 'fail 'fail 'pass) 'safe-but-useless)
  ;; F3: an instruction that could not be resolved to a measurable set is
  ;; unavailable; it is never labelled as an escalation outcome.
  (check-eq? (w3-outcome 'unavailable 'fail 'pass 'pass) 'unavailable)
  (check-eq? (w3-outcome 'run-all 'fail 'fail 'pass) 'unavailable))

;; ---- timing ------------------------------------------------------------------------

(define host (hash 'hostname "test-host" 'processor-count 4 'os "unix" 'racket-version "8.10"))

(test-case "local timing is recorded as single-host and leaves M6 unavailable"
  (define timing
    (timing-record #:host host
                   #:selected-seconds 8.25
                   #:comprehensive-source 'unavailable
                   #:comprehensive-seconds 'unavailable))
  (check-eq? (hash-ref timing 'comprehensive-source) 'unavailable)
  (check-eq? (hash-ref timing 'm6) 'unavailable)
  (check-eq? (hash-ref timing 'same-host) 'unavailable)
  (check-false (hash-ref timing 'ci-timing-substituted?)))

(test-case "CI-derived comprehensive timing is refused, never substituted"
  (check-exn exn:fail?
             (lambda ()
               (timing-record #:host host
                              #:selected-seconds 8.25
                              #:comprehensive-source 'ci
                              #:comprehensive-seconds 90))))

;; ---- record verification -----------------------------------------------------------

(define selected-files (list "tests/test-a.rkt" "tests/test-b.rkt"))
(define comprehensive-files (list "tests/test-a.rkt" "tests/test-b.rkt" "tests/test-c.rkt"))

(define producers
  (list (hash 'name "test (0)" 'declared 2 'files-count 2
              'digest (enumeration-digest (list "tests/test-a.rkt" "tests/test-b.rkt")))
        (hash 'name "security" 'declared 1 'files-count 1
              'digest (enumeration-digest (list "tests/test-c.rkt")))))

(define (producers-digest-of producers)
  (enumeration-digest
   (for/list ([p (in-list producers)])
     (format "~a:~a:~a" (hash-ref p 'name) (hash-ref p 'declared) (hash-ref p 'digest)))))

(define (contract-block instruction)
  (case instruction
    [(run-comprehensive)
     (hash 'state
           'present
           'mode
           'historical
           'status
           'evaluated
           'evaluated-revision
           "rev-1"
           'contract-revision
           "rev-1"
           'layer-present-at-evaluated-revision?
           #t
           'decision
           (hash 'verdict
                 'escalate
                 'required
                 (list "tests/test-a.rkt")
                 'escalations
                 (list (hash 'code "no-consumer-detector" 'module "m.rkt" 'detail "d" 'tests '())))
           'selection
           (hash 'instruction 'run-comprehensive
                 'required (list "tests/test-a.rkt")
                 'tests comprehensive-files))]
    [(unavailable)
     (hash 'state
           'present
           'mode
           'historical
           'status
           'invalid
           'evaluated-revision
           "rev-1"
           'contract-revision
           "rev-1"
           'layer-present-at-evaluated-revision?
           #t
           'problems
           (list "unknown instruction")
           'decision
           #f
           'selection
           #f)]
    [else (hash 'state 'absent-at-revision 'instruction 'run-required)]))

(define (record #:w2 [w2 'missed]
                #:w3 [w3 'unsafe-reduced]
                #:instruction [instruction 'run-required]
                #:defect-selected [defect-selected 'pass]
                #:control-selected [control-selected 'pass]
                #:defect-comp [defect-comp 'fail]
                #:control-comp [control-comp 'pass]
                #:ci-substituted [ci-substituted #f]
                #:comprehensive-source [comprehensive-source 'unavailable]
                #:same-host [same-host 'unavailable]
                #:selected-seconds [selected-seconds 9.5])
  (hash 'selection-input
        (hash 'files selected-files 'digest (enumeration-digest selected-files))
        'comprehensive-input
        (hash 'files
              comprehensive-files
              'digest
              (enumeration-digest comprehensive-files)
              'producers
              producers
              'producers-digest
              (producers-digest-of producers))
        'provenance
        (hash 'provenance-ok?
              #t
              'committed-changes
              (list "extensions/gsd/wave-status.rkt")
              'declared-changes
              (list "extensions/gsd/wave-status.rkt"))
        'contract-layer
        (contract-block instruction)
        'defect
        (hash 'comprehensive-verdict
              defect-comp
              'selected
              (hash 'verdict defect-selected 'selected selected-files)
              'strategy
              (hash 'verdict defect-selected 'selected selected-files 'seconds selected-seconds))
        'control
        (hash 'comprehensive-verdict
              control-comp
              'selected
              (hash 'verdict control-selected 'selected selected-files))
        'classification
        (hash 'w2 w2 'w3-outcome w3 'mode 'historical)
        'timing
        (hash 'ci-timing-substituted?
              ci-substituted
              'comprehensive-source
              comprehensive-source
              'comprehensive-seconds
              (if (eq? comprehensive-source 'unavailable) 'unavailable 90)
              'same-host
              same-host
              'selected-seconds
              selected-seconds)))

(test-case "a faithful record re-derives its own classification"
  (check-true (verify-record (record))))

(test-case "a JSON round-tripped record (symbols as strings) still verifies"
  (define json-form
    (for/hash ([(k v) (in-hash (record))])
      (values k
              (cond
                [(hash? v)
                 (for/hash ([(k2 v2) (in-hash v)])
                   (values k2
                           (cond
                             [(hash? v2)
                              (for/hash ([(k3 v3) (in-hash v2)])
                                (values k3
                                        (if (symbol? v3)
                                            (symbol->string v3)
                                            v3)))]
                             [(symbol? v2) (symbol->string v2)]
                             [else v2])))]
                [else v]))))
  (check-true (verify-record json-form)))

(test-case "a tampered outcome label is caught by re-derivation"
  (define tampered
    (hash-set (record) 'classification (hash 'w2 'missed 'w3-outcome 'correct-reduced)))
  (check-exn exn:fail? (lambda () (verify-record tampered))))

(test-case "a deployed strategy whose set differs from the enumeration is caught"
  (define r (record))
  (define defect
    (hash-set (hash-ref r 'defect)
              'strategy
              (hash 'verdict 'pass 'selected (list "tests/test-a.rkt"))))
  (check-exn exn:fail? (lambda () (verify-record (hash-set r 'defect defect)))))

(test-case "an executed set that is not the enumerated selection is caught"
  (define r (record))
  (define defect
    (hash-set (hash-ref r 'defect)
              'selected
              (hash 'verdict 'pass 'selected (list "tests/test-a.rkt"))))
  (check-exn exn:fail? (lambda () (verify-record (hash-set r 'defect defect)))))

(test-case "a digest that does not match its file list is caught"
  (define r (record))
  (define selection (hash-set (hash-ref r 'selection-input) 'digest "0000"))
  (check-exn exn:fail? (lambda () (verify-record (hash-set r 'selection-input selection)))))

(test-case "broken provenance is caught even when the labels look right"
  (define r (record))
  (define provenance (hash-set (hash-ref r 'provenance) 'provenance-ok? #f))
  (check-exn exn:fail? (lambda () (verify-record (hash-set r 'provenance provenance)))))

(test-case "a record that substituted CI timing is refused"
  (check-exn exn:fail? (lambda () (verify-record (record #:ci-substituted #t)))))

(test-case "the W2 label stays the shadow-behaviour label when the layer escalates"
  ;; same shadow facts (reduced passes, comprehensive fails), different decision
  (check-true (verify-record (record #:instruction 'run-comprehensive #:w3 'correct-escalation))))

(test-case "a producer digest edit is caught by the aggregate producer digest"
  (define r (record))
  (define ci (hash-ref r 'comprehensive-input))
  (define tampered (hash-set ci 'producers
                             (list (hash 'name "test (0)" 'declared 2 'files-count 2
                                         'digest "0000"))))
  (check-exn exn:fail? (lambda () (verify-record (hash-set r 'comprehensive-input tampered)))))

(test-case "a CI-sourced comprehensive timing is caught even without the boolean flag"
  (check-exn exn:fail?
             (lambda ()
               (verify-record (record #:comprehensive-source 'ci #:selected-seconds 9.5)))))

(test-case "unavailable comprehensive timing must carry no value"
  (define r (record))
  (define timing (hash-set (hash-ref r 'timing) 'comprehensive-seconds 90))
  (check-exn exn:fail? (lambda () (verify-record (hash-set r 'timing timing)))))

(test-case "selected timing must be the deployed strategy's own measurement"
  (define r (record))
  (define timing (hash-set (hash-ref r 'timing) 'selected-seconds 1.0))
  (check-exn exn:fail? (lambda () (verify-record (hash-set r 'timing timing)))))

(test-case "same-host cannot be claimed without a comprehensive timing"
  (check-exn exn:fail?
             (lambda () (verify-record (record #:same-host #t)))))

;; ---- decision records (F3) ---------------------------------------------------------

(define decision-files (list "tests/test-a.rkt"))

(define (decision-layer #:tree [tree "tree-1"] #:revision [revision "rev-1"])
  (hash 'state
        'present
        'mode
        'historical
        'status
        'evaluated
        'evaluated-revision
        revision
        'contract-revision
        revision
        'evaluated-tree
        tree
        'layer-present-at-evaluated-revision?
        #t
        'decision
        (hash 'verdict
              'escalate
              'required
              decision-files
              'escalations
              (list (hash 'code "no-consumer-detector" 'module "m.rkt" 'detail "d" 'tests '())))
        'selection
        (hash 'instruction 'run-comprehensive 'required decision-files 'tests decision-files)))

(define (decision-record)
  (hash 'revision
        "rev-1"
        'measured-tree-sha
        "tree-1"
        'comprehensive
        (hash 'files
              decision-files
              'union-count
              1
              'digest
              (enumeration-digest decision-files))
        'contract-layer
        (decision-layer)
        'decision-verified?
        #t))

(test-case "a decision record binds the decision to its tree and revision"
  (check-true (verify-decision-record (decision-record))))

(test-case "a decision record whose tree or revision was edited is refused"
  (check-exn exn:fail?
             (lambda ()
               (verify-decision-record (hash-set (decision-record)
                                                'measured-tree-sha
                                                "tree-2"))))
  (check-exn exn:fail?
             (lambda () (verify-decision-record (hash-set (decision-record) 'revision "rev-2")))))

(test-case "a decision record with a tampered comprehensive list is refused"
  (define r (decision-record))
  (define comprehensive (hash-set (hash-ref r 'comprehensive) 'union-count 2))
  (check-exn exn:fail?
             (lambda () (verify-decision-record (hash-set r 'comprehensive comprehensive)))))

(test-case "a decision record must claim and carry a valid layer block"
  (check-exn exn:fail?
             (lambda ()
               (verify-decision-record (hash-set (decision-record) 'decision-verified? #f))))
  (define layer (hash-set (decision-layer) 'status 'invalid))
  (check-exn exn:fail?
             (lambda () (verify-decision-record (hash-set (decision-record) 'contract-layer layer)))))
