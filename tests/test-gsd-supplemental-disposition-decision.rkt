#lang racket

;; @covers extensions/gsd/supplemental-disposition-decision.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit
;;
;; Pure decision matrix for the supplemental disposition gate (CL4 #9981).
;; No repository, lease, lock, journal or authority state is touched: every case
;; is built from immutable view facts, so precedence between simultaneous faults
;; can be asserted directly.

(require rackunit
         racket/list
         "../extensions/gsd/supplemental-disposition-decision.rkt")

(define plan-id (make-string 64 #\a))
(define parent-proposal (hasheq 'kind "task-proposal" 'sha256 (make-string 40 #\b)))
(define parent-approval (hasheq 'kind "task-approval" 'sha256 (make-string 40 #\c)))
(define allocation
  (hasheq 'attempt-id "attempt-1" 'attempt-fence "fence-1" 'coordinator-fence "coord-1"))
(define terminal-body (make-string 40 #\d))
(define terminal-record (make-string 40 #\e))

(define (amendment #:plan [plan plan-id] #:wave [wave 1])
  (disposition-amendment-view plan
                              wave
                              (make-string 40 #\f)
                              parent-proposal
                              parent-approval
                              terminal-body
                              terminal-record
                              allocation))

(define (pointer #:proposal [p parent-proposal] #:approval [a parent-approval])
  (hasheq 'schema "supplemental-pointer/v1" 'task-proposal p 'task-approval a))

(define (request #:plan [plan plan-id] #:wave [wave 1] #:ptr [ptr (pointer)])
  (disposition-request-view plan wave ptr (pointer)))

(define (terminal #:proposal [proposal parent-proposal]
                  #:sha [sha terminal-body]
                  #:digest [digest terminal-record]
                  #:allocation [alloc allocation]
                  #:late? [late? #f])
  (disposition-terminal-view proposal sha digest alloc late?))

(define (campaign #:record? [record? #t]
                  #:wave? [wave? #t]
                  #:attempt? [attempt? #t]
                  #:status [status 'failed]
                  #:cancelled? [cancelled? #f]
                  #:paused? [paused? #f]
                  #:attempt-id [attempt-id "attempt-1"]
                  #:attempt-fence [attempt-fence "fence-1"]
                  #:coordinator-fence [coordinator-fence "coord-1"])
  (disposition-campaign-view record?
                             wave?
                             attempt?
                             status
                             cancelled?
                             paused?
                             attempt-id
                             attempt-fence
                             coordinator-fence))

(define (context #:plan [plan plan-id]
                 #:wave [wave 1]
                 #:snapshot [snapshot (make-string 40 #\f)]
                 #:actual [actual #f])
  (define value (hasheq 'plan-id plan 'wave wave 'snapshot-sha256 snapshot))
  (disposition-context-view plan wave snapshot value (or actual value)))

(define (source #:source? [source? #t]
                #:proof? [proof? #t]
                #:cwd [cwd "/repo"]
                #:repo [repo "/repo"]
                #:refs? [refs? #t]
                #:head? [head? #t]
                #:tree? [tree? #t])
  (disposition-source-view source? proof? cwd repo refs? head? tree?))

(define (lineage #:request [req (request)]
                 #:amendment [amend (amendment)]
                 #:terminal [term (terminal)]
                 #:campaign [camp (campaign)])
  (decide-disposition-lineage req amend term camp))

(define (evidence #:amendment [amend (amendment)] #:context [ctx (context)] #:source [src (source)])
  (decide-disposition-evidence amend ctx src))

(define (status decision)
  (disposition-decision-status decision))
(define (code decision)
  (disposition-decision-code decision))
(define (message decision)
  (disposition-decision-message decision))
(define (allowed? decision)
  (eq? (status decision) 'allow))

(define (refusal decision)
  (list (status decision) (code decision) (message decision)))

;; ------------------------------------------------------- allow baseline --

(test-case "matching lineage and evidence allow the disposition"
  (check-true (allowed? (lineage)))
  (check-true (allowed? (evidence))))

;; ------------------------------------------------ E1–E4 individual faults --

(test-case "request outside the fixed amendment refuses first"
  (check-equal? (refusal (lineage #:request (request #:plan (make-string 64 #\9))))
                (list 'refuse 'outside-amendment "outside the fixed disposition amendment"))
  (check-equal? (code (lineage #:request (request #:wave 2))) 'outside-amendment)
  (check-equal? (code (lineage #:request (request #:ptr (pointer #:proposal parent-approval))))
                'outside-amendment))

(test-case "terminal or digest drift refuses second"
  (define bad-proposal (hasheq 'kind "task-proposal" 'sha256 (make-string 40 #\1)))
  (check-equal? (refusal (lineage #:terminal (terminal #:proposal bad-proposal)))
                (list 'refuse 'terminal-drift "parent terminal or digest namespace changed"))
  (check-equal? (code (lineage #:terminal (terminal #:sha (make-string 40 #\1)))) 'terminal-drift)
  (check-equal? (code (lineage #:terminal (terminal #:digest (make-string 40 #\1)))) 'terminal-drift))

(test-case "wrong allocation refuses third"
  (define other-allocation (hasheq 'attempt-id "x" 'attempt-fence "y" 'coordinator-fence "z"))
  (check-equal? (refusal (lineage #:terminal (terminal #:allocation other-allocation)))
                (list 'refuse 'wrong-allocation "wrong failed parent allocation")))

(test-case "campaign state refuses fourth and covers each condition"
  (define campaign-message
    (string-append "parent must own current failed (or exact fixed late awaiting-delivery) "
                   "uncancelled unpaused attempt and fences"))
  (define cases
    (list (campaign #:record? #f)
          (campaign #:wave? #f)
          (campaign #:attempt? #f)
          (campaign #:status 'pending)
          (campaign #:cancelled? #t)
          (campaign #:paused? #t)
          (campaign #:attempt-id "other")
          (campaign #:attempt-fence "other")
          (campaign #:coordinator-fence "other")))
  (for ([camp (in-list cases)])
    (check-equal? (refusal (lineage #:campaign camp))
                  (list 'refuse 'campaign-state campaign-message))))

(test-case "late terminal admits awaiting-delivery only for that exact lineage"
  (check-true (allowed? (lineage #:terminal (terminal #:late? #t)
                                 #:campaign (campaign #:status 'awaiting-delivery))))
  (check-equal? (code (lineage #:terminal (terminal #:late? #f)
                               #:campaign (campaign #:status 'awaiting-delivery)))
                'campaign-state)
  (check-equal? (code (lineage #:terminal (terminal #:late? #t) #:campaign (campaign #:status 'done)))
                'campaign-state))

;; ------------------------------------------------- E5–E6 individual faults --

(test-case "context drift refuses before source drift"
  (check-equal? (refusal (evidence #:context (context #:plan (make-string 64 #\9))))
                (list 'refuse 'context-changed "parent context, frozen snapshot or receipt changed"))
  (check-equal? (code (evidence #:context (context #:snapshot (make-string 40 #\9))))
                'context-changed)
  (check-equal? (code (evidence #:context (context #:actual (hasheq 'other "value"))))
                'context-changed))

(test-case "source and proof equality is the final gate"
  (define cases
    (list (source #:source? #f)
          (source #:proof? #f)
          (source #:cwd "/elsewhere")
          (source #:repo "/other")
          (source #:refs? #f)
          (source #:head? #f)
          (source #:tree? #f)))
  (for ([src (in-list cases)])
    (check-equal? (refusal (evidence #:source src))
                  (list 'refuse
                        'source-changed
                        "retained clean source or authenticated owned receipt proof changed"))))

;; ------------------------------------------------------------ precedence --

(test-case "multiple simultaneous faults keep the documented winner"
  (check-equal? (code (lineage #:request (request #:wave 9)
                               #:terminal (terminal #:sha (make-string 40 #\1))
                               #:campaign (campaign #:cancelled? #t)))
                'outside-amendment)
  (check-equal? (code (lineage #:terminal (terminal #:digest (make-string 40 #\1)
                                                    #:allocation (hasheq))
                               #:campaign (campaign #:paused? #t)))
                'terminal-drift)
  (check-equal? (code (lineage #:terminal (terminal #:allocation (hasheq))
                               #:campaign (campaign #:status 'pending)))
                'wrong-allocation)
  (check-equal? (code (lineage #:campaign (campaign #:cancelled? #t))) 'campaign-state)
  (check-equal? (code (evidence #:context (context #:plan (make-string 64 #\9))
                                #:source (source #:head? #f)))
                'context-changed))

(test-case "each stage decides from its own facts and composes in order"
  ;; Identity refuses with no terminal/campaign facts supplied at all.
  (check-equal? (code (decide-disposition-identity (request #:wave 9) (amendment)))
                'outside-amendment)
  (check-true (allowed? (decide-disposition-identity (request) (amendment))))
  ;; Terminal, allocation and campaign stages each gate one fact.
  (check-equal? (code (decide-disposition-terminal (amendment)
                                                   (terminal #:digest (make-string 40 #\1))))
                'terminal-drift)
  (check-equal? (code (decide-disposition-allocation (amendment) (terminal #:allocation (hasheq))))
                'wrong-allocation)
  (check-equal? (code (decide-disposition-campaign (amendment)
                                                   (terminal)
                                                   (campaign #:paused? #t)))
                'campaign-state)
  ;; Evidence stages gate context before source.
  (check-equal? (code (decide-disposition-context (amendment) (context #:wave 2)))
                'context-changed)
  (check-equal? (code (decide-disposition-source (source #:head? #f))) 'source-changed)
  ;; Compositions preserve the same order.
  (check-equal? (code (decide-disposition-lineage (request #:wave 9)
                                                  (amendment)
                                                  (terminal #:digest (make-string 40 #\1))
                                                  (campaign #:paused? #t)))
                'outside-amendment))

(test-case "refusal text has one owner and is stable"
  (check-equal? (message (lineage #:campaign (campaign #:paused? #t)))
                (string-append
                 "parent must own current failed (or exact fixed late awaiting-delivery) "
                 "uncancelled unpaused attempt and fences"))
  (check-false (message (lineage)))
  (check-equal? (code (evidence)) 'eligible))
