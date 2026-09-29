#lang racket
;; @covers extensions/gsd/tracker-reconciliation.rkt
;; @speed fast
;; @suite extensions
;; @boundary unit

(require rackunit
         racket/file
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/effect-ports.rkt"
         "../extensions/gsd/github-port.rkt"
         "../extensions/gsd/tracker-reconciliation.rkt")

(define MANIFEST
  (make-campaign-manifest 1
                          "Tracker reconciliation"
                          '()
                          (list (make-campaign-wave-descriptor 0 "Wave 0" "waves/W0-wave.md" "hash"))
                          "constraints"))
(define PLAN-ID (campaign-manifest-hash MANIFEST))
(define HEAD (make-string 40 #\b))
(define MERGE (make-string 40 #\c))
(define TREE (make-string 40 #\d))
(define BRANCH "campaign/aaaaaaaa/w0")

(define (receipt)
  (hasheq 'repo
          "owner/repo"
          'branch
          BRANCH
          'origin
          "origin"
          'evidence
          "docs/reports/gsd-wave-evidence/fixture.rktd"
          'head
          HEAD
          'tree
          TREE
          'verified-at
          1700000000))

(define (make-record status branch head)
  (define wave (make-campaign-wave 0 "Wave 0" status 1 #f))
  (set-campaign-wave-delivery-branch! wave branch)
  (set-campaign-wave-delivery-head-sha! wave head)
  (make-campaign-record PLAN-ID MANIFEST (list wave) #f 1 'test 1700000000 1700000000))

(define (seed-authoritative-delivery! dir
                                      #:status [status 'done]
                                      #:branch [branch BRANCH]
                                      #:head [head HEAD])
  (define record (make-record status branch head))
  (persist-campaign! dir record)
  (record-delivery-receipt! dir PLAN-ID 0 (receipt))
  (persist-delivery-handoff! dir PLAN-ID (car (campaign-record-waves record)) "pending")
  (reconcile-delivered-handoff! dir PLAN-ID 0 MERGE)
  record)

(struct gh-log-state (calls) #:mutable #:transparent)

(define (record-call! state call)
  (set-gh-log-state-calls! state (append (gh-log-state-calls state) (list call))))

(define (make-logging-github-port)
  (define state (gh-log-state '()))
  (define port
    (gsd-github-port (lambda (cmd)
                       (record-call! state
                                     (list (gsd-github-command-kind cmd)
                                           (gsd-github-command-correlation-id cmd)
                                           (gsd-github-command-params cmd)))
                       (gsd-github-command-result (gsd-github-command-correlation-id cmd)
                                                  (gsd-github-command-kind cmd)
                                                  #f
                                                  #f
                                                  #f
                                                  "ok"))
                     (lambda () #f)
                     (lambda () '())))
  (values port state))

(define (delivered-proof [merge MERGE])
  (hasheq 'status "delivered" 'plan-id PLAN-ID 'wave 0 'merge-sha merge))

(define (with-temp-dir proc)
  (define dir (make-temporary-file "gsd-tracker-~a" 'directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(module+ test
  (test-case "reconciles issue and board only after durable DONE plus receipt and delivered handoff"
    (with-temp-dir
     (lambda (dir)
       (seed-authoritative-delivery! dir)
       (define-values (port state) (make-logging-github-port))
       (define result
         (reconcile-tracker-after-delivery!
          dir
          PLAN-ID
          0
          (hasheq 'plan-id PLAN-ID 'wave 0 'issue-number 74 'board-field "Status" 'board-value "Done")
          port
          #:delivery-reader (lambda (_base _plan _wave) (delivered-proof))))
       (check-equal? (tracker-reconciliation-result-status result) 'reconciled)
       (check-equal? (tracker-reconciliation-result-merge-sha result) MERGE)
       (check-equal? (map car (gh-log-state-calls state)) '(board-set-field issue-close))
       (check-equal? (map cadr (gh-log-state-calls state))
                     (list (format "tracker:~a:w0:~a:board:74" PLAN-ID MERGE)
                           (format "tracker:~a:w0:~a:close:74" PLAN-ID MERGE))))))

  (test-case "stable deterministic correlations across restart/new github port"
    (with-temp-dir
     (lambda (dir)
       (seed-authoritative-delivery! dir)
       (define-values (port-a state-a) (make-logging-github-port))
       (define-values (port-b state-b) (make-logging-github-port))
       (define binding
         (hasheq 'plan-id PLAN-ID 'wave 0 'issue-number 74 'board-field "Status" 'board-value "Done"))
       (reconcile-tracker-after-delivery! dir
                                          PLAN-ID
                                          0
                                          binding
                                          port-a
                                          #:delivery-reader (lambda _ (delivered-proof)))
       (reconcile-tracker-after-delivery! dir
                                          PLAN-ID
                                          0
                                          binding
                                          port-b
                                          #:delivery-reader (lambda _ (delivered-proof)))
       (check-equal? (map cadr (gh-log-state-calls state-a))
                     (map cadr (gh-log-state-calls state-b))))))

  (test-case "no action on pending proof, missing/stale binding, non-DONE wave, receipt mismatch, or handoff mismatch"
    (for ([scenario
           (in-list
            '(pending missing-binding stale-binding non-done receipt-mismatch handoff-mismatch))])
      (with-temp-dir
       (lambda (dir)
         (case scenario
           [(non-done) (seed-authoritative-delivery! dir #:status 'verifying)]
           [(receipt-mismatch) (seed-authoritative-delivery! dir #:branch "other-branch")]
           [(handoff-mismatch)
            (seed-authoritative-delivery! dir)
            (reconcile-delivered-handoff! dir PLAN-ID 0 (make-string 40 #\e))]
           [else (seed-authoritative-delivery! dir)])
         (define-values (port state) (make-logging-github-port))
         (define binding
           (and (not (eq? scenario 'missing-binding))
                (hasheq 'plan-id
                        PLAN-ID
                        'wave
                        (if (eq? scenario 'stale-binding) 99 0)
                        'issue-number
                        74
                        'board-field
                        "Status"
                        'board-value
                        "Done")))
         (define proof
           (if (eq? scenario 'pending)
               (hasheq 'status "delivery-pending" 'reason "not yet")
               (delivered-proof)))
         (define readbacks (box 0))
         (define result
           (reconcile-tracker-after-delivery! dir
                                              PLAN-ID
                                              0
                                              binding
                                              port
                                              #:delivery-reader
                                              (lambda (_base _plan _wave)
                                                (set-box! readbacks (add1 (unbox readbacks)))
                                                proof)))
         (check-equal? (tracker-reconciliation-result-status result) 'blocked)
         (check-equal? (unbox readbacks) (if (memq scenario '(pending handoff-mismatch)) 1 0))
         (check-equal? (gh-log-state-calls state) '()))))))
