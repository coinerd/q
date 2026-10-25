#lang racket/base
;; @covers extensions/gsd/reconciliation-checkpoint.rkt
;; @speed fast
;; @suite gsd
;; @boundary pure
;; tests/test-gsd-reconciliation-checkpoint.rkt
;; v1.00.33 W0 (BUG-0074 wiring): the post-DONE tracker reconciliation
;; checkpoint. tracker-reconciliation.rkt is already covered by
;; test-gsd-tracker-reconciliation.rkt (positive path, restart determinism,
;; every refusal). These tests cover the WIRING and the POLICY that only the
;; wiring can violate:
;;
;;   tracker state is an OUTPUT of delivery, never an INPUT to it.
;;
;; So: reconciliation is attempted only after a wave is provably done, a
;; tracker failure can never move that wave off `done`, and no tracker write
;; can happen for a wave that is not `done`.

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/list
         racket/logging
         (only-in "helpers/honest-delivery-fixture.rkt" test-delivered-reader)
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/campaign-result.rkt"
         "../extensions/gsd/wave-completion.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/effect-ports.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/gh-cli-tracker-adapter.rkt"
         "../extensions/gsd/reconciliation-checkpoint.rkt"
         "../extensions/gsd/tracker-production-wiring.rkt"
         "../extensions/gsd/tracker-reconciliation.rkt")

;; ── Fixtures ──────────────────────────────────────────────────

(define (write-wave-doc dir)
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file (build-path dir ".planning" "PLAN.md")
                       (lambda ()
                         (displayln "# Plan: Reconciliation checkpoint")
                         (newline)
                         (displayln "- [Inbox] W0: Evidence → waves/W0-e.md"))
                       #:exists 'replace)
  (with-output-to-file (build-path dir ".planning" "waves" "W0-e.md")
                       (lambda ()
                         (displayln "# Wave 0")
                         (displayln "Status: Inbox")
                         (newline)
                         (displayln "## Files")
                         (displayln "- File: q/src/a.rkt")
                         (newline)
                         (displayln "## Verify")
                         (newline)
                         (displayln "raco test .")
                         (newline)
                         (displayln "## Done")
                         (newline)
                         (displayln "measured"))
                       #:exists 'replace))

;; The single seeded campaign's plan id (directory-list yields paths).
(define (plan-id-of dir)
  (path->string (first (directory-list (build-path dir ".planning" "campaigns")))))

;; A one-wave campaign parked exactly where BUG-0077 parks it: verified
;; implementation, durable `awaiting-delivery`, no delivery proof yet.
(define (make-parked-campaign)
  (define dir (make-temporary-file "recon-ckpt~a" 'directory))
  (write-wave-doc dir)
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
  (persist-campaign! dir rec)
  (define attempt (campaign-wave-current-attempt (car (campaign-record-waves rec))))
  (try-complete-wave! dir
                      rec
                      0
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt))
  dir)

;; Park a wave AND deliver it honestly: attempt-bound receipt, terminal
;; delivered journal, delivered handoff, then the finalizing completion call.
;; Returns (values dir plan-id merge-sha final-status).
(define (make-delivered-wave)
  (define dir (make-temporary-file "recon-done~a" 'directory))
  (write-wave-doc dir)
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
  (persist-campaign! dir rec)
  (define pid (campaign-plan-id rec))
  (define attempt0 (campaign-wave-current-attempt (car (campaign-record-waves rec))))
  (try-complete-wave! dir
                      rec
                      0
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt0)
                      #:expected-fence-token (campaign-attempt-fence-token attempt0))
  (define durable (load-campaign-record dir pid))
  (define wave (car (campaign-record-waves durable)))
  (define attempt (campaign-wave-current-attempt wave))
  (define merge-sha (make-string 40 #\c))
  (define head (make-string 40 #\a))
  (set-campaign-wave-delivery-branch! wave "campaign/test")
  (set-campaign-wave-delivery-head-sha! wave head)
  (persist-campaign! dir durable)
  (record-delivery-receipt! dir
                            pid
                            0
                            (hasheq 'repo
                                    "/repo"
                                    'branch
                                    "campaign/test"
                                    'head
                                    head
                                    'tree
                                    (make-string 40 #\b)
                                    'origin
                                    "https://github.com/example/q.git"
                                    'verified-at
                                    0
                                    'evidence
                                    "Verify passed"
                                    'attempt-id
                                    (campaign-attempt-id attempt)
                                    'attempt-fence
                                    (campaign-attempt-fence-token attempt)))
  (persist-delivery-handoff! dir pid wave "test precondition")
  (reconcile-delivered-handoff! dir pid 0 merge-sha)
  (update-delivery-journal! dir pid 0 (hasheq 'stage "delivered"))
  (define fresh (load-campaign-record dir pid))
  (define fattempt (campaign-wave-current-attempt (car (campaign-record-waves fresh))))
  (define fin
    (try-complete-wave! dir
                        fresh
                        0
                        #:verifier-approve? #t
                        #:expected-attempt-id (campaign-attempt-id fattempt)
                        #:expected-fence-token (campaign-attempt-fence-token fattempt)
                        #:delivered-merge-sha merge-sha))
  (values dir pid merge-sha (completion-result-status fin)))

;; Park a wave and seed its delivery proof WITHOUT finalizing it. This is the
;; exact state the orchestrator's post-DONE checkpoint runs in: the wave is
;; still `awaiting-delivery` here, and run-delivery-finalization is what turns
;; the proof into DONE (and therefore what may offer it to reconciliation).
(define (make-parked-wave-with-proof)
  (define dir (make-temporary-file "recon-proof~a" 'directory))
  (write-wave-doc dir)
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
  (persist-campaign! dir rec)
  (define pid (campaign-plan-id rec))
  (define attempt0 (campaign-wave-current-attempt (car (campaign-record-waves rec))))
  (try-complete-wave! dir
                      rec
                      0
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt0)
                      #:expected-fence-token (campaign-attempt-fence-token attempt0))
  (define durable (load-campaign-record dir pid))
  (define wave (car (campaign-record-waves durable)))
  (define attempt (campaign-wave-current-attempt wave))
  (define merge-sha (make-string 40 #\c))
  (define head (make-string 40 #\a))
  (set-campaign-wave-delivery-branch! wave "campaign/test")
  (set-campaign-wave-delivery-head-sha! wave head)
  (persist-campaign! dir durable)
  (record-delivery-receipt! dir
                            pid
                            0
                            (hasheq 'repo
                                    "/repo"
                                    'branch
                                    "campaign/test"
                                    'head
                                    head
                                    'tree
                                    (make-string 40 #\b)
                                    'origin
                                    "https://github.com/example/q.git"
                                    'verified-at
                                    0
                                    'evidence
                                    "Verify passed"
                                    'attempt-id
                                    (campaign-attempt-id attempt)
                                    'attempt-fence
                                    (campaign-attempt-fence-token attempt)))
  (persist-delivery-handoff! dir pid wave "test precondition")
  (reconcile-delivered-handoff! dir pid 0 merge-sha)
  (update-delivery-journal! dir pid 0 (hasheq 'stage "delivered"))
  (define parked (load-campaign-record dir pid))
  (check-equal? (campaign-wave-status (car (campaign-record-waves parked))) 'awaiting-delivery)
  (values dir pid merge-sha parked))

;; A delivered readback for this plan/wave. The production default stays the
;; real authenticated controller readback; a unit test must not call GitHub,
;; and stubbing the reader is exactly what the existing tracker suite does.
(define (delivered-proof-for _plan-id merge-sha)
  (test-delivered-reader #:merge-sha merge-sha))

;; A github port that records every command it is asked to execute, so a test
;; can assert the ABSENCE of tracker writes, not just the absence of a status.
(struct gh-log (calls) #:mutable #:transparent)

(define (make-logging-port)
  (define state (gh-log '()))
  (values (gsd-github-port (lambda (cmd)
                             (set-gh-log-calls! state
                                                (append (gh-log-calls state)
                                                        (list (gsd-github-command-kind cmd))))
                             (gsd-github-command-result (gsd-github-command-correlation-id cmd)
                                                        (gsd-github-command-kind cmd)
                                                        #f
                                                        #f
                                                        #f
                                                        "ok"))
                           (lambda () #f)
                           (lambda () '()))
          state))

;; The production reconciler, bound to a real issue binding and the logging
;; port. Nothing else is stubbed: this is the shipped fail-closed pass.
(define (production-reconciler port issue-number)
  (lambda (base-dir plan-id wave-index #:delivery-reader reader)
    (reconcile-tracker-after-delivery!
     base-dir
     plan-id
     wave-index
     (hasheq 'plan-id plan-id 'wave wave-index 'issue-number issue-number)
     port
     #:delivery-reader reader)))

;; ── Policy: the checkpoint ────────────────────────────────────

(test-case "no reconciler configured is not-configured and performs no effects"
  (define dir (make-parked-campaign))
  (define r (run-tracker-reconciliation! dir (plan-id-of dir) 0 #f))
  (check-equal? (reconciliation-checkpoint-result-status r) 'not-configured)
  (check-equal? (reconciliation-checkpoint-result-actions r) '())
  (check-true (reconciliation-blocked-status? r) "not-configured counts as non-reconciled")
  (delete-directory/files dir #:must-exist? #f))

(test-case "a reconciler refusal is blocked with its reason and no actions"
  (define dir (make-parked-campaign))
  (define r
    (run-tracker-reconciliation!
     dir
     (plan-id-of dir)
     0
     (lambda (_b _p _w #:delivery-reader _r)
       (tracker-reconciliation-result 'blocked "missing safe tracker issue binding" #f '()))))
  (check-equal? (reconciliation-checkpoint-result-status r) 'blocked)
  (check-equal? (reconciliation-checkpoint-result-reason r) "missing safe tracker issue binding")
  (check-equal? (reconciliation-checkpoint-result-actions r) '())
  (delete-directory/files dir #:must-exist? #f))

(test-case "a partial board write remains visible when issue close is blocked"
  (define dir (make-parked-campaign))
  (define r
    (run-tracker-reconciliation!
     dir
     (plan-id-of dir)
     0
     (lambda (_b _p _w #:delivery-reader _r)
       (tracker-reconciliation-result 'blocked "issue close failed" #f '(board-updated)))))
  (check-equal? (reconciliation-checkpoint-result-status r) 'blocked)
  (check-equal? (reconciliation-checkpoint-result-actions r) '(board-updated))
  (delete-directory/files dir #:must-exist? #f))

(test-case "a reconciler that raises becomes a typed block, never an exception"
  (define dir (make-parked-campaign))
  (define r
    (run-tracker-reconciliation! dir
                                 (plan-id-of dir)
                                 0
                                 (lambda (_b _p _w #:delivery-reader _r)
                                   (error 'reconciler "credential boundary exploded"))))
  (check-equal? (reconciliation-checkpoint-result-status r) 'blocked)
  (check-true (regexp-match? #rx"credential boundary exploded"
                             (reconciliation-checkpoint-result-reason r)))
  (delete-directory/files dir #:must-exist? #f))

(test-case "a reconciler returning a non-result is refused"
  (define dir (make-parked-campaign))
  (define r
    (run-tracker-reconciliation! dir
                                 (plan-id-of dir)
                                 0
                                 (lambda (_b _p _w #:delivery-reader _r) 'not-a-result)))
  (check-equal? (reconciliation-checkpoint-result-status r) 'blocked)
  (check-equal? (reconciliation-checkpoint-result-reason r)
                "tracker reconciler returned a non-result")
  (delete-directory/files dir #:must-exist? #f))

(test-case "a reconciled outcome carries its actions through"
  (define dir (make-parked-campaign))
  (define r
    (run-tracker-reconciliation! dir
                                 (plan-id-of dir)
                                 0
                                 (lambda (_b _p _w #:delivery-reader _r)
                                   (tracker-reconciliation-result 'reconciled
                                                                  #f
                                                                  (make-string 40 #\c)
                                                                  (list 'issue-close
                                                                        'board-set-field)))))
  (check-equal? (reconciliation-checkpoint-result-status r) 'reconciled)
  (check-false (reconciliation-blocked-status? r))
  (check-equal? (reconciliation-checkpoint-result-actions r) (list 'issue-close 'board-set-field))
  (delete-directory/files dir #:must-exist? #f))

(test-case "a missing plan id or wave index is refused before any call"
  (define dir (make-parked-campaign))
  (check-equal? (reconciliation-checkpoint-result-status (run-tracker-reconciliation! dir #f 0 #f))
                'blocked)
  (check-equal?
   (reconciliation-checkpoint-result-status (run-tracker-reconciliation! dir (plan-id-of dir) #f #f))
   'blocked)
  (delete-directory/files dir #:must-exist? #f))

;; ── Policy: zero tracker writes without durable DONE ──────────

(test-case "a parked awaiting-delivery wave causes ZERO tracker writes"
  (define dir (make-parked-campaign))
  (define pid (plan-id-of dir))
  (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                'awaiting-delivery
                "fixture precondition: verified but undelivered stays parked")
  (define-values (port log) (make-logging-port))
  (define r (run-tracker-reconciliation! dir pid 0 (production-reconciler port 4242)))
  (check-equal? (gh-log-calls log) '() "no tracker command may be issued for a wave that is not DONE")
  (check-equal? (reconciliation-checkpoint-result-status r) 'blocked)
  (check-equal? (reconciliation-checkpoint-result-reason r) "durable campaign wave is not DONE")
  (delete-directory/files dir #:must-exist? #f))

;; ── Policy: a tracker failure cannot move a wave off `done` ───

(test-case "a delivered wave reaches done and a tracker block cannot re-open it"
  (define-values (dir pid merge-sha status) (make-delivered-wave))
  (check-equal? status 'done "fixture precondition: honest delivery completes the wave")
  (define blocked
    (run-tracker-reconciliation!
     dir
     pid
     0
     (lambda (_b _p _w #:delivery-reader _r)
       (tracker-reconciliation-result 'blocked "tracker unreachable" #f '()))))
  (check-equal? (reconciliation-checkpoint-result-status blocked) 'blocked)
  (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                'done
                "a tracker refusal must leave the wave DONE, never re-open it")
  (delete-directory/files dir #:must-exist? #f))

(test-case "a delivered wave with full proof reaches the tracker exactly once"
  (define-values (dir pid merge-sha status) (make-delivered-wave))
  (check-equal? status 'done)
  (define-values (port log) (make-logging-port))
  (define r
    (run-tracker-reconciliation! dir
                                 pid
                                 0
                                 (production-reconciler port 4242)
                                 #:delivery-reader (delivered-proof-for pid merge-sha)))
  ;; The shipped pass must have issued its close for a fully proven wave.
  (check-equal? (reconciliation-checkpoint-result-status r) 'reconciled)
  (check-equal? (gh-log-calls log)
                '(issue-close)
                "exactly one tracker command for a fully proven delivered wave")
  ;; The checkpoint passes the executed commands through, so the caller can
  ;; record exactly what the tracker was told.
  (check-equal? (length (reconciliation-checkpoint-result-actions r))
                1
                "the pass reports the one command it executed")
  (check-equal? (map gsd-github-command-result-kind (reconciliation-checkpoint-result-actions r))
                '(issue-close))
  (delete-directory/files dir #:must-exist? #f))

;; ── Wiring: the post-DONE path is the only caller ─────────────

(test-case "run-delivery-finalization does not reconcile a wave that is not provably done"
  (define dir (make-parked-campaign))
  (define pid (plan-id-of dir))
  (define called? #f)
  (define result
    (run-delivery-finalization
     dir
     pid
     (load-campaign-record dir pid)
     0
     (make-string 40 #\c)
     '()
     #:tracker-reconciler (lambda (_b _p _w #:delivery-reader _r)
                            (set! called? #t)
                            (tracker-reconciliation-result 'reconciled #f (make-string 40 #\c) '()))))
  (check-false called? "a wave without authenticated delivery must not trigger reconciliation")
  (check-equal? (campaign-result-status result)
                'wave-blocked
                "a wave without authenticated delivery blocks, and says so")
  (delete-directory/files dir #:must-exist? #f))

(test-case "run-delivery-finalization offers the finalized wave to the reconciler"
  ;; The real post-DONE state: parked, proof seeded, not yet DONE. The
  ;; finalizing call is what authorizes both DONE and (offered) reconciliation.
  (define-values (dir pid merge-sha rec) (make-parked-wave-with-proof))
  (define seen (box '()))
  (define on-delivered-called (box #f))
  (define result
    (run-delivery-finalization dir
                               pid
                               rec
                               0
                               merge-sha
                               '()
                               #:on-delivered (lambda () (set-box! on-delivered-called #t))
                               #:tracker-reconciler
                               (lambda (base-dir plan-id wave-index #:delivery-reader _r)
                                 (set-box! seen (list base-dir plan-id wave-index))
                                 (tracker-reconciliation-result 'blocked "no live port" #f '()))))
  (check-equal? (unbox seen)
                (list dir pid 0)
                "the reconciler is offered the finalized wave with its identity")
  (check-true (unbox on-delivered-called)
              "a tracker refusal does not prevent the post-DONE bookkeeping")
  (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                'done
                "the wave is genuinely done — proof, not tracker state, decided that")
  ;; The done branch returns whatever on-delivered returned (the orchestrator
  ;; passes its loop thunk), so the durable wave status is the real assertion.
  (check-true (void? result))
  (delete-directory/files dir #:must-exist? #f))

(test-case "run-delivery-finalization stays honest with no reconciler configured"
  (define-values (dir pid merge-sha rec) (make-parked-wave-with-proof))
  (define on-delivered-called (box #f))
  (define result
    (run-delivery-finalization dir
                               pid
                               rec
                               0
                               merge-sha
                               '()
                               #:on-delivered (lambda () (set-box! on-delivered-called #t))))
  (check-true (unbox on-delivered-called)
              "completion is reachable with reconciliation simply unconfigured")
  (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                'done)
  ;; The done branch returns whatever on-delivered returned (the orchestrator
  ;; passes its loop thunk), so the durable wave status is the real assertion.
  (check-true (void? result))
  (delete-directory/files dir #:must-exist? #f))

;; ── R74 production composition ─────────────────────────────────────────────
;; The reconciler handed to run-delivery-finalization below is the SHIPPED
;; production resolver: the real settings opt-in read from .q/config.json,
;; the real identity binding, the real adapter construction and a real LIVE
;; github port. Only the `gh` subprocess (the runner) and the authenticated
;; controller readback are faked — no real GitHub is reachable.

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

;; Records every gh argv in `log`. The identity pre-read attests the item in
;; the bound project/repo/issue with Status=Inbox; after the board mutation
;; the post-read attests Status=Done. #:close-state simulates an issue whose
;; close does or does not take effect upstream.
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

(define (composition-reconciler dir plan-id log #:close-state [close-state "closed"])
  (resolve-live-tracker-reconciler
   dir
   plan-id
   0
   #:adapter-maker
   (lambda (#:live? live? #:repository repository)
     (check-equal? live? #t "the production adapter requires explicit live authority")
     (check-equal? repository "owner/repo")
     (make-gh-cli-tracker-adapter #:live? live?
                                  #:repository repository
                                  #:runner (make-composition-runner log #:close-state close-state)))))

(test-case "production composition: finalization after durable DONE drives the real reconciler to gh with exact identity"
  (define-values (dir pid merge-sha rec) (make-parked-wave-with-proof))
  (write-tracker-opt-in! dir pid)
  (define gh-log (box '()))
  (define on-delivered (box #f))
  (run-delivery-finalization
   dir
   pid
   rec
   0
   merge-sha
   '()
   #:on-delivered (lambda () (set-box! on-delivered #t))
   #:tracker-reconciler (composition-reconciler dir pid gh-log)
   #:delivery-reader (delivered-proof-for pid merge-sha))
  (check-true (unbox on-delivered) "a reconciled pass still runs the post-DONE bookkeeping")
  (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                'done)
  ;; Exact command list: the authenticated identity pre-read, the board
  ;; mutation bound to the exact opaque IDs, the verified post-read, then the
  ;; issue close against the bound repository and issue. Nothing else runs,
  ;; and the reconciled outcome itself proves the port was live (a dry-run
  ;; port would have been refused before any read).
  (define argvs (unbox gh-log))
  (check-equal? (map (lambda (argv) (take argv 2)) argvs)
                (list '("api" "graphql")
                      '("project" "item-edit")
                      '("api" "graphql")
                      '("api" "repos/owner/repo/issues/74")))
  (check-equal? (list-ref (list-ref argvs 0) 5) "id=PVTI_item")
  (check-equal? (drop (list-ref argvs 1) 2)
                (list "--id"
                      "PVTI_item"
                      "--project-id"
                      "PVT_project"
                      "--field-id"
                      "PVTSSF_status"
                      "--single-select-option-id"
                      "done123"
                      "--format"
                      "json"))
  (check-equal? (list-ref (list-ref argvs 3) 5) "state=closed")
  (delete-directory/files dir #:must-exist? #f))

(test-case "a tracker refusal in the composed pass is visible and non-fatal to the established DONE"
  (define-values (dir pid merge-sha rec) (make-parked-wave-with-proof))
  (write-tracker-opt-in! dir pid)
  (define gh-log (box '()))
  (define on-delivered (box #f))
  (define warnings (box '()))
  (with-intercepted-logging
   (lambda (entry) (set-box! warnings (cons (vector-ref entry 1) (unbox warnings))))
   (lambda ()
     (run-delivery-finalization
      dir
      pid
      rec
      0
      merge-sha
      '()
      #:on-delivered (lambda () (set-box! on-delivered #t))
      #:tracker-reconciler (composition-reconciler dir pid gh-log #:close-state "open")
      #:delivery-reader (delivered-proof-for pid merge-sha)))
   'warning)
  ;; The board mutation succeeded and was verified, but the close's response
  ;; did not attest state=closed: exactly the four commands of the composed
  ;; pass ran, and the pass is refused as a whole (its board action stays
  ;; visible in the checkpoint result the finalization logs).
  (check-equal? (map (lambda (argv) (take argv 2)) (unbox gh-log))
                (list '("api" "graphql")
                      '("project" "item-edit")
                      '("api" "graphql")
                      '("api" "repos/owner/repo/issues/74")))
  (check-true (unbox on-delivered) "the refusal must not abort the post-DONE bookkeeping")
  (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                'done
                "a tracker refusal can never move the delivered wave off done")
  (check-true
   (and
    (findf
     (lambda (msg)
       (and (string? msg)
            (regexp-match? #rx"tracker reconciliation blocked for plan" msg)
            (regexp-match? #rx"wave 0" msg)
            (regexp-match? #rx"wave remains delivered" msg)))
     (unbox warnings))
    #t)
   "the refusal is visible to the operator")
  (delete-directory/files dir #:must-exist? #f))

(test-case "absent opt-in: finalization reconciles nothing, consults no readback, and still completes DONE"
  (define-values (dir pid merge-sha rec) (make-parked-wave-with-proof))
  ;; No .q/config.json tracker binding exists in this campaign directory, so
  ;; the production fallback resolver resolves to #f.
  (define on-delivered (box #f))
  (define reader-consulted (box 0))
  (define warnings (box '()))
  (with-intercepted-logging
   (lambda (entry) (set-box! warnings (cons (vector-ref entry 1) (unbox warnings))))
   (lambda ()
     (run-delivery-finalization
      dir
      pid
      rec
      0
      merge-sha
      '()
      #:on-delivered (lambda () (set-box! on-delivered #t))
      #:delivery-reader
      (lambda (_base _plan _wave)
        (set-box! reader-consulted (add1 (unbox reader-consulted)))
        (error 'test "no live tracker is configured"))))
   'warning)
  (check-true (unbox on-delivered) "completion is reachable with reconciliation simply unconfigured")
  (check-equal? (unbox reader-consulted)
                0
                "no reconciler is configured, so no authenticated readback is consulted")
  (check-equal? (campaign-wave-status (car (campaign-record-waves (load-campaign-record dir pid))))
                'done)
  (check-true
   (and
    (findf
     (lambda (msg) (and (string? msg) (regexp-match? #rx"tracker reconciliation not-configured" msg)))
     (unbox warnings))
    #t)
   "the unconfigured pass is visible to the operator")
  (delete-directory/files dir #:must-exist? #f))
