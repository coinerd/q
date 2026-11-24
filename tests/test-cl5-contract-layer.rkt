#lang racket

;; @suite default  ;; @speed medium
;; @boundary unit

;; tests/test-cl5-contract-layer.rkt — CL5 F3 contract-layer integration.
;;
;; The integration must use the tree's OWN decision layer, record enough
;; provenance to tell a historical decision from a counterfactual one, and fail
;; closed (unavailable) rather than invent or empty a selection.
;;
;; Synthetic payloads exercise every refusal branch; one real evaluation runs the
;; shipped layer on this checkout.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         (file "../scripts/cl5/contract-layer.rkt")
         (file "../scripts/cl5/selection-experiment.rkt"))

(define-runtime-path repo-root "..")

;; ---- fixtures ----------------------------------------------------------------------

(define (make-root files)
  (define root (make-temporary-file "cl5-root-~a" 'directory))
  (for ([rel (in-list files)])
    (define path (build-path root rel))
    (make-parent-directory* path)
    (call-with-output-file path
                           (lambda (out) (display ";; fixture\n" out))
                           #:exists 'truncate/replace))
  root)

(define root-a (make-root (list "tests/a.rkt" "tests/b.rkt" "tests/c.rkt" "tests/d.rkt")))

(define comprehensive (list "tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))

(define (escalation code)
  (hash 'code code 'module "extensions/gsd/wave-status.rkt" 'detail "d" 'tests '()))

(define (payload #:verdict [verdict "escalate"]
                 #:required [required (list "tests/a.rkt")]
                 #:escalations [escalations (list (escalation "no-consumer-detector"))]
                 #:instruction [instruction "run-comprehensive"]
                 #:tests [tests #f]
                 #:decision-required [decision-required #f])
  (hash 'error
        #f
        'decision
        (hash 'verdict
              verdict
              'required
              (or decision-required required)
              'escalations
              escalations
              'matched
              (list "extensions/gsd/wave-status.rkt")
              'unmatched-high-fan-in
              '())
        'selection
        (hash 'instruction instruction 'required required 'tests (or tests required))))

(define (problems-of payload*)
  (define-values (problems _selection) (validate-layer-output payload* root-a comprehensive))
  problems)

(define (check-pair? value)
  (check-true (pair? value)))

;; ---- the bridge's instruction is accepted only when it is defensible ---------------

(test-case "a run-required instruction over exactly the required set is accepted"
  (define-values (problems selection)
    (validate-layer-output
     (payload #:verdict "covered" #:escalations '() #:instruction "run-required")
     root-a
     comprehensive))
  (check-equal? problems '())
  (check-eq? (hash-ref selection 'instruction) 'run-required)
  (check-equal? (hash-ref selection 'required) (list "tests/a.rkt")))

(test-case "run-required with an empty required set is refused"
  (check-pair?
   (problems-of
    (payload #:verdict "covered" #:escalations '() #:instruction "run-required" #:required '()))))

(test-case "run-required that does not run exactly the required set is refused"
  (check-pair? (problems-of (payload #:verdict "covered"
                                     #:escalations '()
                                     #:instruction "run-required"
                                     #:required (list "tests/a.rkt" "tests/b.rkt")
                                     #:tests (list "tests/a.rkt")))))

(test-case "run-required against an escalated verdict is refused"
  (check-pair? (problems-of (payload #:verdict "escalate" #:instruction "run-required"))))

(test-case "run-required against recorded escalations is refused"
  (check-pair? (problems-of (payload #:verdict "covered" #:instruction "run-required"))))

(test-case "run-comprehensive must be exactly required union comprehensive"
  ;; valid: required + comprehensive
  (check-equal? (problems-of (payload #:tests (list "tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))) '())
  ;; omits a required test
  (check-pair? (problems-of (payload #:required (list "tests/a.rkt" "tests/d.rkt")
                                     #:tests (list "tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))))
  ;; adds a test that is neither required nor comprehensive
  (check-pair?
   (problems-of (payload #:tests (list "tests/a.rkt" "tests/b.rkt" "tests/c.rkt" "tests/d.rkt")))))

(test-case "run-comprehensive against a covered verdict is refused"
  (check-pair? (problems-of (payload #:verdict "covered" #:escalations '()))))

(test-case "a covered verdict with escalations is refused; an escalated verdict without one is refused"
  (check-pair? (problems-of (payload #:verdict "covered" #:escalations (list (escalation "x")))))
  (check-pair? (problems-of (payload #:verdict "escalate" #:escalations '()))))

(test-case "an unknown verdict or instruction is refused, not guessed"
  (check-pair? (problems-of (payload #:verdict "probably-fine")))
  (check-pair? (problems-of (payload #:instruction "run-some"))))

(test-case "run-all is unresolvable and therefore refused"
  (check-pair? (problems-of (payload #:instruction "run-all" #:tests "unresolved"))))

(test-case "a required or selected test that does not exist is refused"
  (check-pair?
   (problems-of (payload #:required (list "tests/missing.rkt")
                         #:tests
                         (list "tests/missing.rkt" "tests/a.rkt" "tests/b.rkt" "tests/c.rkt")))))

(test-case "the selection's required set must match the decision's"
  (check-pair? (problems-of (payload #:decision-required (list "tests/b.rkt")))))

;; ---- instruction derivation --------------------------------------------------------

(test-case "an absent layer keeps the historical deployed instruction"
  (check-eq? (layer-instruction (hash 'state 'absent-at-revision 'status 'absent)) 'run-required)
  ;; frozen W3.1 shape, read from JSON
  (check-eq? (layer-instruction (hash 'state "absent-at-revision" 'instruction "run-required"))
             'run-required))

(test-case "an evaluated layer's instruction is used; invalid or unknown is unavailable"
  (check-eq?
   (layer-instruction
    (hash 'state 'present 'status 'evaluated 'selection (hash 'instruction 'run-comprehensive)))
   'run-comprehensive)
  (check-eq? (layer-instruction
              (hash 'state 'present 'status 'evaluated 'selection (hash 'instruction 'run-all)))
             'unavailable)
  (check-eq? (layer-instruction (hash 'state 'present 'status 'invalid)) 'unavailable)
  (check-eq? (layer-instruction (hash 'state 'present)) 'unavailable)
  (check-eq? (layer-instruction (hash 'state 'something-else)) 'unavailable))

;; ---- record-level structural verification ------------------------------------------

(define (evaluated-record #:state [state 'present]
                          #:mode [mode 'historical]
                          #:verdict [verdict 'escalate]
                          #:instruction [instruction 'run-comprehensive]
                          #:required [required (list "tests/a.rkt")]
                          #:tests [tests (list "tests/a.rkt" "tests/b.rkt" "tests/c.rkt")])
  (hash 'state
        state
        'mode
        mode
        'status
        'evaluated
        'evaluated-revision
        "rev-evaluated"
        'contract-revision
        (if (eq? state 'counterfactual) "rev-contract" "rev-evaluated")
        'layer-present-at-evaluated-revision?
        (not (eq? state 'counterfactual))
        'counterfactual?
        (eq? state 'counterfactual)
        'decision
        (hash 'verdict verdict 'required required 'escalations (list (escalation "x")))
        'selection
        (hash 'instruction instruction 'required required 'tests tests)))

(test-case "a well-formed evaluated record verifies"
  (check-equal? (validate-layer-record (evaluated-record) comprehensive) '()))

(test-case "a recorded selection may not shrink the decision's required set"
  ;; Only the decision.required cross-check catches this: the union invariant
  ;; still holds because the dropped test is in the comprehensive enumeration.
  (define layer
    (hash-set
     (evaluated-record)
     'decision
     (hash-set (hash-ref (evaluated-record) 'decision) 'required (list "tests/a.rkt" "tests/b.rkt"))))
  (check-pair? (validate-layer-record layer comprehensive)))

(test-case "a historical evaluation may not be relabelled counterfactual"
  (check-pair? (validate-layer-record (hash-set (evaluated-record) 'mode 'counterfactual)
                                      comprehensive))
  (check-pair? (validate-layer-record (hash-set (evaluated-record) 'counterfactual? #t)
                                      comprehensive))
  ;; and a counterfactual must say so
  (check-pair?
   (validate-layer-record
    (hash-set (evaluated-record #:state 'counterfactual #:mode 'counterfactual) 'counterfactual? #f)
    comprehensive)))

(test-case "the frozen W3.1 shape is accepted only as absence"
  (check-equal? (validate-layer-record (hash 'state 'absent-at-revision 'instruction 'run-required)
                                       '())
                '())
  (check-pair? (validate-layer-record (hash 'state 'present 'instruction 'run-required) '()))
  (check-pair?
   (validate-layer-record (hash 'state 'absent-at-revision 'instruction 'run-comprehensive) '())))

(test-case "a counterfactual evaluation may not be recorded as historical"
  (check-pair? (validate-layer-record (evaluated-record #:state 'counterfactual) comprehensive))
  (check-equal? (validate-layer-record (evaluated-record #:state 'counterfactual
                                                         #:mode 'counterfactual)
                                       comprehensive)
                '()))

(test-case "a historical evaluation must use the evaluated revision's layer"
  (define layer (hash-set (evaluated-record) 'contract-revision "rev-other"))
  (check-pair? (validate-layer-record layer comprehensive)))

(test-case "a recorded run-required block must carry exactly its required set"
  (define layer
    (evaluated-record #:verdict 'covered
                      #:instruction 'run-required
                      #:tests (list "tests/a.rkt" "tests/b.rkt")))
  (check-pair? (validate-layer-record layer comprehensive)))

(test-case "a recorded run-comprehensive block must carry the union"
  (define layer (evaluated-record #:tests (list "tests/a.rkt")))
  (check-pair? (validate-layer-record layer comprehensive)))

(define (invalid-block #:problems [problems '()])
  (hash 'state
        'present
        'status
        'invalid
        'evaluated-revision
        "rev-evaluated"
        'contract-revision
        "rev-evaluated"
        'layer-present-at-evaluated-revision?
        #t
        'mode
        'historical
        'counterfactual?
        #f
        'problems
        problems))

(test-case "an invalid block must record why; a missing block is refused"
  (check-pair? (validate-layer-record (invalid-block) comprehensive))
  (check-equal? (validate-layer-record (invalid-block #:problems (list "unknown instruction"))
                                       comprehensive)
                '())
  (check-pair? (validate-layer-record 'not-a-hash comprehensive)))

;; ---- real evaluation of this checkout's layer --------------------------------------

(define repo (path->complete-path repo-root))
(define (rev-parse what)
  (string-trim (git! repo "rev-parse" what)))
(define git-head (rev-parse "HEAD"))
(define git-tree (rev-parse "HEAD^{tree}"))
(define layer-added
  (string-trim
   (git! repo "log" "--diff-filter=A" "--format=%H" "-1" "--" "scripts/cl5/contract-escalation.rkt")))
(define before-layer (rev-parse (string-append layer-added "^")))
(define d5-change (list "extensions/gsd/wave-status.rkt"))

(test-case "the shipped layer escalates D5 on this checkout and names its consumers' detector"
  (define layer
    (evaluate-layer #:root (path->complete-path repo-root)
                    #:changed d5-change
                    #:comprehensive (list "tests/test-gsd-wave-status.rkt")
                    #:evaluated-revision git-head
                    #:evaluated-tree git-tree))
  (check-eq? (hash-ref layer 'state) 'present)
  (check-eq? (hash-ref layer 'mode) 'historical)
  (check-eq? (hash-ref layer 'status) 'evaluated)
  (check-false (hash-ref layer 'counterfactual?))
  (check-false (hash-ref layer 'contract-modified?))
  (define decision (hash-ref layer 'decision))
  (check-eq? (hash-ref decision 'verdict) 'escalate)
  (check-true (pair? (hash-ref decision 'escalations)))
  (define required (hash-ref layer 'required))
  (check-not-false (member "tests/test-gsd-wave-status.rkt" required))
  (check-equal? (length required) 23)
  (check-eq? (layer-instruction layer) 'run-comprehensive)
  (define selection (hash-ref layer 'selection))
  (check-equal? (hash-ref selection 'required) required)
  (check-equal? (hash-ref selection 'tests)
                (sort (remove-duplicates (append required (list "tests/test-gsd-wave-status.rkt")))
                      string<?))
  (check-equal? (validate-layer-record layer (list "tests/test-gsd-wave-status.rkt")) '()))

(test-case "a historical revision without the layer plus a later layer is counterfactual"
  ;; `before-layer` has no contract layer; the checkout does. Evaluating the
  ;; historical revision with today's layer must be labelled counterfactual --
  ;; never a historical execution.
  (define layer
    (evaluate-layer #:root repo
                    #:changed d5-change
                    #:comprehensive (list "tests/test-gsd-wave-status.rkt")
                    #:evaluated-revision before-layer
                    #:evaluated-tree (rev-parse (string-append before-layer "^{tree}"))
                    #:contract-worktree repo
                    #:contract-revision git-head))
  (check-eq? (hash-ref layer 'state) 'counterfactual)
  (check-eq? (hash-ref layer 'mode) 'counterfactual)
  (check-true (hash-ref layer 'counterfactual?))
  (check-false (hash-ref layer 'layer-present-at-evaluated-revision?))
  (check-eq? (layer-instruction layer) 'run-comprehensive)
  (check-equal? (validate-layer-record layer (list "tests/test-gsd-wave-status.rkt")) '()))

(test-case "a tree with no layer at all records absence and keeps the deployed instruction"
  (define fixture (make-temporary-file "cl5-no-layer-~a" 'directory))
  (define git (find-executable-path "git"))
  (define (run . args)
    (define code (apply system*/exit-code git "-C" (path->string fixture) args))
    (unless (and (number? code) (zero? code))
      (error 'fixture "git failed: ~a" args)))
  (run "init" "-q")
  (run "config" "user.email" "fixture@example.invalid")
  (run "config" "user.name" "fixture")
  (call-with-output-file (build-path fixture "README.md")
                         (lambda (out) (display "fixture\n" out))
                         #:exists 'truncate/replace)
  (run "add" "README.md")
  (run "commit" "-q" "-m" "fixture")
  (define head (string-trim (git! fixture "rev-parse" "HEAD")))
  (define layer
    (evaluate-layer #:root fixture
                    #:changed d5-change
                    #:comprehensive '()
                    #:evaluated-revision head
                    #:evaluated-tree (string-trim (git! fixture "rev-parse" "HEAD^{tree}"))))
  (check-eq? (hash-ref layer 'state) 'absent-at-revision)
  (check-eq? (hash-ref layer 'status) 'absent)
  (check-eq? (layer-instruction layer) 'run-required)
  (check-true (pair? (hash-ref layer 'reasons)))
  (check-equal? (validate-layer-record layer '()) '()))
