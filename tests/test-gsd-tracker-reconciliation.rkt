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
         "../runtime/settings.rkt"
         "../extensions/gsd/gh-cli-tracker-adapter.rkt"
         "../extensions/gsd/tracker-production-wiring.rkt"
         "../extensions/gsd/tracker-reconciliation.rkt"
         "helpers/gsd-port-fakes.rkt")

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
  (test-case "issue-close failure preserves the successful board action as a partial result"
    (with-temp-dir
     (lambda (dir)
       (seed-authoritative-delivery! dir)
       (define port
         (gsd-github-port (lambda (cmd)
                            (if (eq? (gsd-github-command-kind cmd) 'issue-close)
                                (error 'fake "close refused")
                                (gsd-github-command-result (gsd-github-command-correlation-id cmd)
                                                           'board-set-field
                                                           #f
                                                           #f
                                                           #f
                                                           "field set")))
                          (lambda () #f)
                          (lambda () '())))
       (define result
         (reconcile-tracker-after-delivery!
          dir
          PLAN-ID
          0
          (hasheq 'plan-id PLAN-ID 'wave 0 'issue-number 74 'board-field "Status" 'board-value "Done")
          port
          #:delivery-reader (lambda _ (delivered-proof))))
       (check-equal? (tracker-reconciliation-result-status result) 'blocked)
       (check-equal? (length (tracker-reconciliation-result-actions result)) 1)
       (check-equal?
        (gsd-github-command-result-kind (car (tracker-reconciliation-result-actions result)))
        'board-set-field))))

  (test-case "full configured pass routes authenticated delivery through the Racket adapter"
    (with-temp-dir
     (lambda (dir)
       (seed-authoritative-delivery! dir)
       (define calls '())
       (define (runner argv)
         (set! calls (append calls (list argv)))
         (cond
           [(equal? (car argv) "project") (values 0 "{\"id\":\"PVTI_item\"}" "")]
           [(equal? (take argv 2) '("api" "graphql"))
            (values
             0
             (format
              (string-append
               "{\"data\":{\"node\":{\"id\":\"PVTI_item\","
               "\"project\":{\"id\":\"PVT_project\"},"
               "\"content\":{\"number\":74,\"repository\":{\"nameWithOwner\":\"owner/repo\"}},"
               "\"fieldValueByName\":{\"field\":{\"id\":\"PVTSSF_status\","
               "\"options\":[{\"id\":\"inbox\"},{\"id\":\"inprogress\"},{\"id\":\"done123\"}]},"
               "\"optionId\":\"~a\"}}}}}")
              (if (= (length calls) 1) "inbox" "done123"))
             "")]
           [else (values 0 "{\"number\":74,\"state\":\"closed\"}" "")]))
       (define tracker
         (hasheq 'live
                 #t
                 'plan-id
                 PLAN-ID
                 'wave
                 0
                 'repository
                 "owner/repo"
                 'issue-number
                 74
                 'board-field
                 "Status"
                 'board-value
                 "Done"
                 'project-item-id
                 "PVTI_item"
                 'project-id
                 "PVT_project"
                 'field-id
                 "PVTSSF_status"
                 'option-id
                 "done123"))
       (define settings (make-minimal-settings #:overrides (hasheq 'gsd (hasheq 'tracker tracker))))
       (define reconciler
         (resolve-live-tracker-reconciler
          dir
          PLAN-ID
          0
          #:settings settings
          #:adapter-maker
          (lambda (#:live? live? #:repository repo)
            (make-gh-cli-tracker-adapter #:live? live? #:repository repo #:runner runner))))
       (define result (reconciler dir PLAN-ID 0 #:delivery-reader (lambda _ (delivered-proof))))
       (check-equal? (tracker-reconciliation-result-status result) 'reconciled)
       (check-equal? (map car calls) '("api" "project" "api" "api"))
       (check-equal? (length calls) 4))))

  (test-case "opaque project IDs from the exact binding reach the board command"
    (with-temp-dir (lambda (dir)
                     (seed-authoritative-delivery! dir)
                     (define-values (port state) (make-logging-github-port))
                     (reconcile-tracker-after-delivery! dir
                                                        PLAN-ID
                                                        0
                                                        (hasheq 'plan-id
                                                                PLAN-ID
                                                                'wave
                                                                0
                                                                'issue-number
                                                                74
                                                                'board-field
                                                                "Status"
                                                                'board-value
                                                                "Done"
                                                                'project-item-id
                                                                "PVTI_item"
                                                                'project-id
                                                                "PVT_project"
                                                                'field-id
                                                                "PVTSSF_status"
                                                                'option-id
                                                                "done123")
                                                        port
                                                        #:delivery-reader
                                                        (lambda _ (delivered-proof)))
                     (define params (caddr (car (gh-log-state-calls state))))
                     (for ([key '(project-item-id project-id field-id option-id)])
                       (check-true (string? (hash-ref params key #f)))))))

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

  (test-case "a repeat pass on an already-reconciled wave creates no new external writes"
    (with-temp-dir
     (lambda (dir)
       (seed-authoritative-delivery! dir)
       (define-values (adapter state) (make-fake-github-adapter))
       ;; A real port with journal replay: same correlation-id -> recorded
       ;; result, never a second external call.
       (define port (make-github-port adapter #:dry-run? #f))
       (define binding
         (hasheq 'plan-id PLAN-ID 'wave 0 'issue-number 74 'board-field "Status" 'board-value "Done"))
       (define first-pass
         (reconcile-tracker-after-delivery! dir
                                            PLAN-ID
                                            0
                                            binding
                                            port
                                            #:delivery-reader (lambda _ (delivered-proof))))
       (check-equal? (tracker-reconciliation-result-status first-pass) 'reconciled)
       (check-equal? (fake-github-call-count state 'close-issue!) 1)
       (check-equal? (fake-github-call-count state 'set-board-field!) 1)
       (define second-pass
         (reconcile-tracker-after-delivery! dir
                                            PLAN-ID
                                            0
                                            binding
                                            port
                                            #:delivery-reader (lambda _ (delivered-proof))))
       ;; Idempotent: the deterministic correlation ids replay from the port
       ;; journal, so the repeat pass converges to the same state and causes
       ;; exactly no second close and no second board write.
       (check-equal? (tracker-reconciliation-result-status second-pass) 'reconciled)
       (check-equal? (fake-github-call-count state 'close-issue!) 1)
       (check-equal? (fake-github-call-count state 'set-board-field!) 1)
       (check-true (andmap gsd-github-command-result-already-done?
                           (tracker-reconciliation-result-actions second-pass))))))

  (test-case "pre-existing historical already-done wave is skipped, untouched"
    (with-temp-dir
     (lambda (dir)
       ;; Historical: the durable wave is done from before this campaign's
       ;; reconciliation existed — no receipt, no delivery journal, no
       ;; delivered handoff. The pass must skip it (refuse without guessing,
       ;; zero tracker effects) and never flip it back open.
       (persist-campaign! dir (make-record 'done BRANCH HEAD))
       (define-values (port state) (make-logging-github-port))
       (define binding
         (hasheq 'plan-id PLAN-ID 'wave 0 'issue-number 74 'board-field "Status" 'board-value "Done"))
       (define result
         (reconcile-tracker-after-delivery! dir
                                            PLAN-ID
                                            0
                                            binding
                                            port
                                            #:delivery-reader (lambda _ (delivered-proof))))
       (check-equal? (tracker-reconciliation-result-status result) 'blocked)
       (check-equal? (tracker-reconciliation-result-actions result) '())
       (check-equal? (gh-log-state-calls state) '())
       (define rec (load-campaign-record dir PLAN-ID))
       (check-equal? (campaign-wave-status (car (campaign-record-waves rec))) 'done))))

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
