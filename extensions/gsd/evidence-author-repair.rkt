#lang racket/base
;; Authenticated, separate default-one author lane. Preflight remains read-only;
;; only the explicit reservation entry point creates durable dispatch authority.
(require racket/list
         racket/string
         "author-scope.rkt"
         "evidence-readiness.rkt"
         "delivery-journal.rkt"
         (only-in "evidence-author-journal.rkt" reserve-evidence-author-repair!))
(provide literal-author-scope?
         evidence-author-repair-preflight
         authorize-evidence-author-repair!
         validate-author-phase-paths!)
(define (validate-author-phase-paths! phase files tests changed)
  (unless (and (literal-author-scope? files)
               (literal-author-scope? tests)
               (andmap (lambda (p) (member p files)) tests)
               (list? changed)
               (andmap string? changed))
    (error 'author-phase "invalid frozen phase contract"))
  (unless (memq phase '(test-author implementation))
    (error 'author-phase "unknown phase"))
  (for ([path (in-list changed)])
    (unless (and (member path files)
                 (if (eq? phase 'test-author)
                     (member path tests)
                     (not (member path tests))))
      (error 'author-phase "~a cannot change frozen path ~s" phase path)))
  #t)
(define (stop code message)
  (hasheq 'status "paused" 'code code 'message message 'author-work-started #f))
(define (existing-author-report marker)
  (define exhausted? (member (hash-ref marker 'state) '("completed" "retained")))
  (for/fold ([report
              (hash-set*
               (stop
                (if exhausted?
                    "evidence-author-allowance-exhausted"
                    "evidence-author-recovery-required")
                (if exhausted?
                    (string-append
                     "The permanent one-per-wave evidence-author allowance is spent; still blocked. "
                     "Any further evidence work requires an explicit operator-authorized governed supplemental replan; "
                     "this report grants no authority and does not establish readiness or resolution")
                    (string-append
                     "An existing evidence-author reservation requires owner recovery, not a new reservation; "
                     "resume only through the existing owner recovery route without redispatch")))
               'status
               (if exhausted? "blocked" "paused")
               'marker-state
               (hash-ref marker 'state)
               'marker-context
               (hash-ref marker 'context)
               'new-reservation-allowed
               #f
               'next-action
               (if exhausted? "explicit-governed-supplemental-replan" "existing-owner-recovery"))])
            ([key '(disposition successor-attempt
                                successor-fence
                                successor-receipt-sha256
                                retained-tree-sha256)]
             #:when (hash-has-key? marker key))
    (hash-set report key (hash-ref marker key))))
(define (evidence-author-repair-preflight base plan wave #:guard live?)
  (with-handlers ([exn:fail? (lambda (e) (stop "evidence-author-contract-blocked" (exn-message e)))])
    (cond
      [(not (live?)) (stop "stale-live-guard" "Author preflight lost campaign authority")]
      [else
       (define readiness (evidence-readiness base plan wave))
       (unless (and (equal? (hash-ref readiness 'status #f) "repair-required")
                    (equal? (hash-ref readiness 'code #f) "unsupported-harness")
                    (eq? (hash-ref readiness 'spec #f) 'null))
         (error 'author-preflight
                "only authenticated unsupported harnesses admit the separate author lane"))
       (define context (current-evidence-context base plan wave))
       (define files
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (frozen-author-files base plan wave)))
       (define marker (journal-evidence-author-repair (load-delivery-journal base plan wave)))
       (cond
         [(not (live?)) (stop "stale-live-guard" "Author preflight lost campaign authority")]
         [(or (not files) (not (ormap (lambda (p) (string-prefix? p "tests/")) files)))
          (stop "evidence-author-frozen-scope-blocked"
                "Frozen FILES has no literal test-author scope; no work launched or scope expanded")]
         [marker (existing-author-report marker)]
         [else
          (hash-set*
           (stop
            "evidence-author-repair-required"
            "Separate ordinary author successor, full Verify, receipt supersession and fresh review required")
           'context
           context
           'frozen-files
           files)])])))
(define (authorize-evidence-author-repair! base plan wave #:guard live?)
  (with-handlers ([exn:fail? (lambda (e)
                               (stop "evidence-author-authorization-blocked" (exn-message e)))])
    (define admission (evidence-author-repair-preflight base plan wave #:guard live?))
    (cond
      [(not (equal? (hash-ref admission 'code) "evidence-author-repair-required")) admission]
      [else
       (define context (hash-ref admission 'context))
       (define (context-live?)
         (and (live?) (equal? context (current-evidence-context base plan wave))))
       (reserve-evidence-author-repair! base
                                        plan
                                        wave
                                        context
                                        (hash-ref admission 'frozen-files)
                                        #:guard context-live?)
       (hash-set*
        admission
        'status
        "author-authorized"
        'code
        "evidence-author-repair-authorized"
        'message
        "Durable independent default-one author allowance reserved; no review rejection fabricated")])))
