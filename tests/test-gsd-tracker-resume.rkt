#lang racket/base
;; @covers extensions/gsd/tracker-resume.rkt
;; @speed fast
;; @suite gsd
(require rackunit
         json
         racket/file
         racket/list
         racket/logging
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/gh-cli-tracker-adapter.rkt"
         "../extensions/gsd/tracker-production-wiring.rkt"
         "../extensions/gsd/reconciliation-checkpoint.rkt"
         "../extensions/gsd/tracker-reconciliation.rkt"
         "../extensions/gsd/tracker-resume.rkt")

(define (seed-wave! dir status)
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file
   (build-path dir ".planning" "PLAN.md")
   (lambda () (displayln "# Plan: Resume tracker\n\n- [Inbox] W0: Evidence → waves/W0-e.md"))
   #:exists 'replace)
  (with-output-to-file
   (build-path dir ".planning" "waves" "W0-e.md")
   (lambda ()
     (displayln
      "# Wave 0\nStatus: Inbox\n\n## Files\n\n- File: q/src/a.rkt\n\n## Verify\n\nraco test .\n\n## Done\n\nmeasured"))
   #:exists 'replace)
  (define rec (migrate-campaign! dir))
  (set-campaign-wave-status! (car (campaign-record-waves rec)) status)
  (persist-campaign! dir rec)
  rec)

(test-case "a restart after DONE but before tracker writes retries the targeted wave"
  (define dir (make-temporary-file "tracker-resume~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define rec (seed-wave! dir 'done))
     (define calls 0)
     (define (resolver _dir _plan _wave)
       (lambda (_base _plan _wave #:delivery-reader _reader)
         (set! calls (add1 calls))
         (tracker-reconciliation-result 'reconciled #f (make-string 40 #\a) '(board close))))
     ;; This fixture injects only the reconciler; the production resolver
     ;; additionally verifies receipt, handoff and authenticated readback.
     (define (target? _base plan wave)
       (and (equal? plan (campaign-plan-id rec)) (zero? wave)))
     (define (resume)
       (resume-tracker-reconciliation! dir
                                       rec
                                       #:target? target?
                                       #:resolver resolver
                                       #:delivery-reader (lambda _ (error 'test "fixture only"))))
     (check-equal? calls 0) ; process died after DONE before the checkpoint
     (define results (resume))
     (check-equal? (length results) 1)
     (check-equal? (reconciliation-checkpoint-result-status (car results)) 'reconciled)
     (check-equal? calls 1)
     ;; Replaying a completed pass is allowed: board Status=Done and issue
     ;; close are externally idempotent. Never rely on an untrusted marker.
     (resume)
     (check-equal? calls 2))
   (lambda () (delete-directory/files dir))))

(test-case "a non-DONE wave or absent live binding never constructs the port"
  (define dir (make-temporary-file "tracker-resume~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define rec (seed-wave! dir 'awaiting-delivery))
     (define (unexpected . _)
       (error 'test "tracker called without DONE and binding"))
     (check-equal?
      (resume-tracker-reconciliation! dir rec #:target? (lambda _ #t) #:resolver unexpected)
      '())
     (set-campaign-wave-status! (car (campaign-record-waves rec)) 'done)
     (persist-campaign! dir rec)
     (check-equal?
      (resume-tracker-reconciliation! dir rec #:target? (lambda _ #f) #:resolver unexpected)
      '()))
   (lambda () (delete-directory/files dir))))

;; ── R74 production resume composition ────────────────────────────────
;; The production defaults stay production: #:target? is the shipped
;; live-tracker-target? reading the real .q/config.json opt-in from the
;; campaign directory, and the injected resolver is the shipped
;; resolve-live-tracker-reconciler. Only the `gh` subprocess (the runner)
;; and the authenticated controller readback are faked.

(define HEAD (make-string 40 #\a))
(define MERGE (make-string 40 #\c))

;; Seed the durable post-DONE state with its full proof: an attempt-bound
;; receipt matching the wave's branch/head, a delivered handoff at MERGE and
;; a terminal delivery journal — the exact state a crash between the durable
;; DONE commit and the post-DONE checkpoint leaves behind.
(define (seed-delivered-proof! dir rec)
  (define plan-id (campaign-plan-id rec))
  (define wave (car (campaign-record-waves rec)))
  (set-campaign-wave-status! wave 'done)
  (set-campaign-wave-delivery-branch! wave "campaign/test")
  (set-campaign-wave-delivery-head-sha! wave HEAD)
  (persist-campaign! dir rec)
  (record-delivery-receipt!
   dir plan-id 0
   (hasheq 'repo
           "owner/repo"
           'branch
           "campaign/test"
           'origin
           "origin"
           'evidence
           "evidence"
           'head
           HEAD
           'tree
           (make-string 40 #\b)
           'verified-at
           0))
  (persist-delivery-handoff! dir plan-id wave "test precondition")
  (reconcile-delivered-handoff! dir plan-id 0 MERGE)
  (update-delivery-journal! dir plan-id 0 (hasheq 'stage "delivered"))
  (load-campaign-record dir plan-id))

(define (write-tracker-opt-in! dir plan-id)
  (make-directory* (build-path dir ".q"))
  (call-with-output-file
   (build-path dir ".q" "config.json")
   (lambda (out)
     (write-json
      (hasheq 'gsd
              (hasheq 'tracker
                      (hasheq 'live
                              #t
                              'plan-id
                              plan-id
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
                              "done123")))
      out)
     (newline out))
   #:exists 'replace))

(define (item-readback-json option-id)
  (format
   (string-append
    "{\"data\":{\"node\":{\"id\":\"PVTI_item\","
    "\"project\":{\"id\":\"PVT_project\"},"
    "\"content\":{\"number\":74,\"repository\":{\"nameWithOwner\":\"owner/repo\"}},"
    "\"fieldValueByName\":{\"field\":{\"id\":\"PVTSSF_status\","
    "\"options\":[{\"id\":\"inbox\"},{\"id\":\"inprogress\"},{\"id\":\"done123\"}]},"
    "\"optionId\":\"~a\"}}}}}")
   option-id))

;; Records every gh argv in `log`; #:close-state simulates an issue whose
;; close response does or does not attest state=closed.
(define (make-composition-runner log #:close-state [close-state "closed"])
  (lambda (argv)
    (set-box! log (append (unbox log) (list argv)))
    (cond
      [(equal? (take argv 2) '("api" "graphql"))
       (values 0 (item-readback-json (if (= (length (unbox log)) 1) "inbox" "done123")) "")]
      [(equal? (take argv 2) '("project" "item-edit"))
       (values 0 "{\"id\":\"PVTI_item\"}" "")]
      [else
       (values 0 (format "{\"number\":74,\"state\":\"~a\"}" close-state) "")])))

;; The shipped resolver with only the gh transport faked. Records the
;; live/repository arguments the production wiring hands the adapter maker.
(define (composition-resolver dir plan-id log #:close-state [close-state "closed"])
  (define made-with (box #f))
  (values
   (lambda (_base _plan _wave)
     (resolve-live-tracker-reconciler
      dir
      plan-id
      0
      #:adapter-maker
      (lambda (#:live? live? #:repository repository)
        (set-box! made-with (list live? repository))
        (make-gh-cli-tracker-adapter #:live? live?
                                     #:repository repository
                                     #:runner (make-composition-runner log #:close-state close-state)))))
   made-with))

(define (composition-reader pid)
  (lambda (_base _plan _wave)
    (hasheq 'status "delivered" 'plan-id pid 'wave 0 'merge-sha MERGE)))

(test-case "production resume: after death between DONE and tracker writes the real resolver reaches gh with exact identity"
  (define dir (make-temporary-file "tracker-resume~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define done-rec (seed-delivered-proof! dir (seed-wave! dir 'done)))
     (define pid (campaign-plan-id done-rec))
     (write-tracker-opt-in! dir pid)
     (define gh-log (box '()))
     (define reader-called (box 0))
     (define-values (resolver made-with) (composition-resolver dir pid gh-log))
     (define results
       (resume-tracker-reconciliation!
        dir
        done-rec
        #:resolver resolver
        #:delivery-reader (lambda (_b _p _w)
                            (set-box! reader-called (add1 (unbox reader-called)))
                            ((composition-reader pid) _b _p _w))))
     ;; The production wiring constructed the explicitly live adapter for the
     ;; exact bound repository, and the fresh readback was consulted once.
     (check-equal? (unbox made-with) (list #t "owner/repo"))
     (check-equal? (unbox reader-called) 1)
     (check-equal? (length results) 1)
     (check-equal? (reconciliation-checkpoint-result-status (car results)) 'reconciled)
     ;; Exact command list with exact identity: the authenticated pre-read,
     ;; the board mutation bound to the opaque IDs, the verified post-read,
     ;; then the close against the bound repository and issue. Nothing else.
     (define argvs (unbox gh-log))
     (check-equal? (map (lambda (argv) (take argv 2)) argvs)
                   (list '("api" "graphql")
                         '("project" "item-edit")
                         '("api" "graphql")
                         '("api" "repos/owner/repo/issues/74")))
     (check-equal? (list-ref (list-ref argvs 1) 3) "PVTI_item")
     (check-equal? (list-ref (list-ref argvs 3) 1) "repos/owner/repo/issues/74")
     (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                   'done))
   (lambda () (delete-directory/files dir))))

(test-case "duplicate resume across restarts replays an identical exact command list and stays DONE"
  (define dir (make-temporary-file "tracker-resume~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define done-rec (seed-delivered-proof! dir (seed-wave! dir 'done)))
     (define pid (campaign-plan-id done-rec))
     (write-tracker-opt-in! dir pid)
     (define gh-log (box '()))
     (define (resume!)
       (define-values (resolver _) (composition-resolver dir pid gh-log))
       (resume-tracker-reconciliation!
        dir done-rec #:resolver resolver #:delivery-reader (composition-reader pid)))
     (check-equal? (reconciliation-checkpoint-result-status (car (resume!))) 'reconciled)
     (define first-pass (unbox gh-log))
     ;; Each resume builds a fresh adapter and port — the restart shape. The
     ;; second pass is externally idempotent: identical deterministic command
     ;; argv sequences (board Status=Done, issue close), converging to the
     ;; same tracker state, and the wave stays DONE throughout.
     (define second-results (resume!))
     (define second-pass (list-tail (unbox gh-log) (length first-pass)))
     (check-equal? (reconciliation-checkpoint-result-status (car second-results)) 'reconciled)
     (check-equal? second-pass first-pass)
     (check-equal? (length second-pass) 4)
     (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                   'done))
   (lambda () (delete-directory/files dir))))

(test-case "a refused resume keeps DONE established and never re-enters implementation"
  (define dir (make-temporary-file "tracker-resume~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define done-rec (seed-delivered-proof! dir (seed-wave! dir 'done)))
     (define pid (campaign-plan-id done-rec))
     (write-tracker-opt-in! dir pid)
     (define gh-log (box '()))
     (define warnings (box '()))
     (define attempt-before
       (campaign-wave-current-attempt (car (campaign-record-waves (load-campaign-record dir pid)))))
     (define journal-before (load-delivery-journal dir pid 0))
     (define-values (resolver _) (composition-resolver dir pid gh-log #:close-state "open"))
     (define results
       (with-intercepted-logging
        (lambda (entry) (set-box! warnings (cons (vector-ref entry 1) (unbox warnings))))
        (lambda ()
          (resume-tracker-reconciliation!
           dir done-rec #:resolver resolver #:delivery-reader (composition-reader pid)))
        'warning))
     (check-equal? (reconciliation-checkpoint-result-status (car results)) 'blocked)
     ;; The attempted effect is visible: all four commands of the composed
     ;; pass ran, including the close PATCH whose response failed to attest
     ;; state=closed.
     (check-equal? (length (unbox gh-log)) 4)
     ;; The refusal is visible to the operator.
     (check-true
      (and
       (findf
        (lambda (msg)
          (and (string? msg)
               (regexp-match? #rx"tracker resume blocked" msg)
               (regexp-match? #rx"delivered wave remains DONE" msg)))
        (unbox warnings))
       #t))
     ;; Durable DONE stands and implementation is never re-entered: the
     ;; attempt and the terminal delivery journal are byte-identical.
     (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                   'done)
     (check-equal?
      (campaign-wave-current-attempt (car (campaign-record-waves (load-campaign-record dir pid))))
      attempt-before)
     (check-equal? (load-delivery-journal dir pid 0) journal-before))
   (lambda () (delete-directory/files dir))))
