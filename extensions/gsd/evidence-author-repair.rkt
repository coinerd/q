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
       (cond
         [(not (live?)) (stop "stale-live-guard" "Author preflight lost campaign authority")]
         [(or (not files) (not (ormap (lambda (p) (string-prefix? p "tests/")) files)))
          (stop "evidence-author-frozen-scope-blocked"
                "Frozen FILES has no literal test-author scope; no work launched or scope expanded")]
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
