#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 1200
;; Isolated source-fixture consent only, never real human approval or live UI.
;; Positive authorization uses the keyboard owner handler and real opaque broker grant.
(require rackunit
         racket/file
         racket/port
         racket/string
         json
         racket/runtime-path
         "helpers/supplemental-disposition-fixture.rkt"
         "helpers/supplemental-go-fixture.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" test-support)
         "../extensions/gsd/supplemental-evidence-operator.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../runtime/approval/broker.rkt"
         "../tui/commands/evidence-owner.rkt"
         (only-in "../tui/context.rkt" tui-ctx-ui-state-box)
         (only-in "../tui/state.rkt" ui-state-transcript transcript-entry-text)
         (prefix-in shared: "../tui/tui-keybindings.rkt")
         "../util/event/event.rkt"
         "../util/event/event-bus.rkt")
(define-runtime-path cli "../scripts/gsd-evidence-replan.rkt")
(define-runtime-path frontend "../tui/commands/evidence-owner.rkt")
;; Compare durable bytes, not just decoded counters. Missing stores are empty.
(define (tree-bytes root [keep? (lambda (_) #t)])
  (if (directory-exists? root)
      (for/hash ([p (in-directory root)]
                 #:when (and (file-exists? p) (keep? p)))
        (values (path->string (find-relative-path root p)) (file->bytes p)))
      (hash)))
;; Lease owner/timestamp telemetry legitimately changes during proposal/install;
;; it is not campaign state, attempt allocation or a model-call counter.
(define (durable-file? p)
  (not (string-suffix? (path->string p) ".lock")))
(define (claim-file? p)
  (regexp-match? #rx"supplemental-(disposition|recovery)-[^/]+[.]json$" (path->string p)))
(define (git-text repo . args)
  (define out (open-output-string))
  (parameterize ([current-directory repo]
                 [current-output-port out])
    (unless (apply system* (find-executable-path "git") args)
      (error 'frontend-fixture "git read failed")))
  (get-output-string out))
(define (source-bytes repo)
  (list (git-text repo "status" "--porcelain")
        (git-text repo "rev-parse" "HEAD")
        (for/hash ([name (in-list (string-split (git-text repo "ls-files" "-z") "\u0000"))])
          (values name (file->bytes (build-path repo name))))))
(module+ test
  (test-case "keyboard owner installs exact v3 after fixture human consent, without dispatch"
    (define home (make-temporary-file "disposition-positive-ui-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (call-with-disposition-fixture
        (lambda (df)
          (define fx (disposition-fixture-fx df))
          (define base (supplemental-fixture-root fx))
          (define plan (supplemental-fixture-plan fx))
          ;; Outside project cwd: otherwise the authority current-directory guard
          ;; refuses before the keyboard route can exercise genuine authorization.
          (define private (build-path home "private"))
          (rename-file-or-directory (supplemental-fixture-private fx) private)
          (parameterize ([current-supplemental-authority-root private]
                         [current-directory base])
            (call-with-test-disposition-amendment
             (disposition-fixture-amendment df)
             (lambda ()
               (define parent (load-supplemental-pointer base plan 1))
               (define campaigns (build-path base ".planning" "campaigns"))
               (define operations (build-path private "supplemental-operations"))
               (define before-campaigns (tree-bytes campaigns durable-file?))
               (define before-operations (tree-bytes operations durable-file?))
               (define before-claims (tree-bytes private claim-file?))
               (define before-source (source-bytes (supplemental-fixture-repo fx)))
               (define runner-calls 0)
               (define (no-runner . _)
                 (set! runner-calls (add1 runner-calls))
                 (error 'frontend-fixture "authorization must not call ordinary/model/author runner"))
               (define (unchanged!)
                 (check-equal? (tree-bytes campaigns durable-file?) before-campaigns)
                 (check-equal? (tree-bytes operations durable-file?) before-operations)
                 (check-equal? (source-bytes (supplemental-fixture-repo fx)) before-source)
                 (check-equal? runner-calls 0))
               (define-values (ref task)
                 (propose-supplemental-disposition! base
                                                    plan
                                                    1
                                                    (hash-ref (hash-ref parent 'task-proposal)
                                                              'sha256)
                                                    (+ (current-seconds) 3600)))
               (define digest (hash-ref ref 'sha256))
               (define presentation (supplemental-approval-presentation ref))
               (check-equal? (hash-ref task 'kind) "supplemental-evidence-task/v3")
               (unchanged!)
               (check-equal? (tree-bytes private claim-file?) before-claims)
               (check-equal? (load-supplemental-pointer base plan 1) parent)
               (define bus (make-event-bus))
               (define ctx
                 (shared:make-tui-ctx #:session-runner no-runner #:event-bus bus #:session-queue #f))
               (define (await-installed-reports! count)
                 (let wait ([remaining 1500])
                   (define successes
                     (for/sum ([entry
                                (in-list (ui-state-transcript (unbox (tui-ctx-ui-state-box ctx))))])
                              (if (string-prefix? (transcript-entry-text entry)
                                                  "Exact disposition task/v3 approved and installed.")
                                  1
                                  0)))
                   (cond
                     [(= successes count) (void)]
                     [(zero? remaining) (error 'frontend-fixture "owner completion missing")]
                     [else
                      (sleep 0.01)
                      (wait (sub1 remaining))])))
               (define requests 0)
               (define requested (make-semaphore 0))
               (define permit-human (make-semaphore 0))
               (define decided (make-semaphore 0))
               (define observed #f)
               (define decision #f)
               (subscribe! bus
                           (lambda (event)
                             (when (equal? (event-ev event)
                                           "supplemental-evidence.approval-requested")
                               (set! requests (add1 requests))
                               (define payload (event-payload event))
                               (define id (hash-ref payload 'request-id))
                               (set! observed (list payload (approval-request-view id digest)))
                               (semaphore-post requested)
                               (semaphore-wait permit-human)
                               ;; ONLY TEST consent: the subscriber stands in for the trusted
                               ;; human frontend, never fabricates or consumes a grant itself.
                               (set! decision (approval-decide! id digest #t))
                               (semaphore-post decided))))
               (define raw (format "/evidence-replan authorize-disposition ~a 1 ~a" plan digest))
               (shared:process-slash-command ctx 'unknown raw)
               (for ([route '("authorize-task" "authorize-recovery")])
                 (check-true (handle-owner-evidence-command!
                              ctx
                              (format "/evidence-replan ~a ~a 1 ~a" route plan digest))))
               (check-equal? requests 0)
               (check-equal? (pending-approval-count) 0)
               ;; This is the actual keyboard-only command, NOT approve! or install!.
               (check-true (handle-owner-evidence-command! ctx raw))
               (check-not-false (sync/timeout 15 requested))
               (check-equal? requests 1)
               (define payload (car observed))
               (define id (hash-ref payload 'request-id))
               (check-equal? (hash-ref payload 'commitment-digest) digest)
               ;; Broker freezes into an equal?-keyed hash; the owner uses hasheq.
               (check-equal? (hash-count (cadr observed)) (hash-count presentation))
               (for ([(key value) (in-hash presentation)])
                 (check-equal? (hash-ref (cadr observed) key) value))
               (check-equal? (hash-ref (cadr observed) 'record-canonical-json)
                             (hash-ref presentation 'record-canonical-json))
               (check-true (approval-request-pending? id digest))
               (check-false
                (approval-decide! id (hash-ref (hash-ref parent 'task-proposal) 'sha256) #t))
               (unchanged!)
               (check-equal? (tree-bytes private claim-file?) before-claims)
               (check-equal? (load-supplemental-pointer base plan 1) parent)
               (semaphore-post permit-human)
               (check-not-false (sync/timeout 15 decided))
               (check-true decision)
               (define installed
                 (let wait ([remaining 1500])
                   (define p (load-supplemental-pointer base plan 1))
                   (cond
                     [(equal? (hash-ref p 'task-proposal) ref) p]
                     [(zero? remaining) (error 'frontend-fixture "owner handler did not install v3")]
                     [else
                      (sleep 0.01)
                      (wait (sub1 remaining))])))
               (await-installed-reports! 1)
               (define approval (hash-ref installed 'task-approval))
               ;; A live private approval plus installation proves that the owner
               ;; frontend consumed the broker's opaque grant, without exposing it.
               (check-not-false (validate-supplemental-approval approval ref))
               (check-false (approval-decide! id digest #t))
               (check-equal? (pending-approval-count) 0)
               (check-false (load-supplemental-operation approval))
               (define claim-path
                 (build-path private
                             (format "supplemental-disposition-~a.json"
                                     (hash-ref (disposition-fixture-amendment df) 'sha256))))
               (define claim (call-with-input-file claim-path read-json))
               (check-equal? (hash-ref claim 'target) installed)
               (check-equal? (hash-ref claim 'lineage) (hash-ref task 'disposition))
               (check-equal? (hash-ref claim 'amendment) (disposition-fixture-amendment df))
               (define after-claims (tree-bytes private claim-file?))
               (check-equal? (hash-count after-claims) (add1 (hash-count before-claims)))
               (for ([(name bytes) (in-hash before-claims)])
                 (check-equal? (hash-ref after-claims name) bytes))
               (check-true (supplemental-recovery-pointer-claimed?
                            (supplemental-pointer-path base plan 1)))
               ;; Historical v2 is no longer the active recovery pointer: use
               ;; the authenticated historical inspector, not the live loader.
               (define-values (historical-ref _historical-task historical-op)
                 (inspect-supplemental-disposition-parent (hash-ref parent 'task-approval)))
               (check-equal? historical-ref (hash-ref parent 'task-proposal))
               (check-equal? historical-op (disposition-fixture-parent-operation df))
               (check-equal? (load-supplemental-operation
                              (hash-ref (disposition-fixture-root-pointer df) 'task-approval))
                             (disposition-fixture-root-operation df))
               ;; No /go here: no reserve/allocation, attempt/fence change, source
               ;; edit, author/model session or operation creation is permitted.
               (unchanged!)
               (check-equal? requests 1)
               ;; Repeating the real owner command requires fresh exact consent,
               ;; but approval/claim installation deduplicates to the active refs.
               (check-true (handle-owner-evidence-command! ctx raw))
               (check-not-false (sync/timeout 15 requested))
               (check-equal? requests 2)
               (check-equal? (load-supplemental-pointer base plan 1) installed)
               (check-equal? (tree-bytes private claim-file?) after-claims)
               (semaphore-post permit-human)
               (check-not-false (sync/timeout 15 decided))
               (check-true decision)
               (await-installed-reports! 2)
               (check-equal? (load-supplemental-pointer base plan 1) installed)
               (check-equal? (tree-bytes private claim-file?) after-claims)
               (check-equal? (pending-approval-count) 0)
               (unchanged!)))))))
     (lambda () (delete-directory/files home))))
  (test-case "CLI recognizes explicit disposition verb without accepting replacement scope"
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (system*/exit-code (find-executable-path "racket") cli "--verb" "propose-disposition")))
    (check-equal? code 2)
    (check-regexp-match #rx"missing required --campaign-root" (get-output-string err))
    (check-false (regexp-match? #rx"--verb must be" (get-output-string err)))
    (define private (make-temporary-file "disposition-cli-private-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (file-or-directory-permissions private #o700)
                    (define errors (open-output-string))
                    (define result
                      (parameterize ([current-output-port (open-output-string)]
                                     [current-error-port errors])
                        (system*/exit-code (find-executable-path "racket")
                                           cli
                                           "--verb"
                                           "propose-disposition"
                                           "--authority-root"
                                           private
                                           "--campaign-root"
                                           private
                                           "--plan"
                                           (make-string 64 #\a)
                                           "--wave"
                                           "1"
                                           "--sha256"
                                           (make-string 64 #\b)
                                           "--expires-at"
                                           (number->string (+ (current-seconds) 3600))
                                           "--repo"
                                           "/caller-selected-scope")))
                    (check-equal? result 2)
                    (check-regexp-match #rx"propose-disposition does not accept --repo"
                                        (get-output-string errors)))
                  (lambda () (delete-directory/files private))))
  (test-case "keyboard prompt literally recognizes v3; no shared/model route"
    (define source (file->string frontend))
    (check-regexp-match #rx"authorize-disposition PLAN WAVE NEW-T-SHA256" source)
    (check-regexp-match #rx"supplemental-evidence-task/v3" source))
  (test-case "inert v3: real broker wrong digest, denial, cancellation and owner default routes refuse"
    (define home (make-temporary-file "disposition-frontend-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (call-with-disposition-fixture
        (lambda (df)
          (define fx (disposition-fixture-fx df))
          (define base (supplemental-fixture-root fx))
          (define plan (supplemental-fixture-plan fx))
          ;; UI's project cwd must not contain the private authority store.
          (define private (build-path home "private"))
          (rename-file-or-directory (supplemental-fixture-private fx) private)
          (parameterize ([current-supplemental-authority-root private])
            (call-with-test-disposition-amendment
             (disposition-fixture-amendment df)
             (lambda ()
               (define parent (load-supplemental-pointer base plan 1))
               (define-values (ref task)
                 (propose-supplemental-disposition! base
                                                    plan
                                                    1
                                                    (hash-ref (hash-ref parent 'task-proposal)
                                                              'sha256)
                                                    (+ (current-seconds) 3600)))
               (check-equal? (hash-ref task 'kind) "supplemental-evidence-task/v3")
               (check-equal? (load-supplemental-pointer base plan 1) parent)
               (check-exn exn:fail?
                          (lambda ()
                            (approve-supplemental-proposal!
                             ref
                             (make-policy-approval-grant (hash-ref ref 'sha256)))))
               (dynamic-wind
                (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
                (lambda ()
                  (define requests 0)
                  (define (publisher type payload)
                    (when (equal? type "supplemental-evidence.approval-requested")
                      (set! requests (add1 requests))
                      (define id (hash-ref payload 'request-id))
                      (define digest (hash-ref ref 'sha256))
                      (check-false
                       (approval-decide! id (hash-ref (hash-ref parent 'task-proposal) 'sha256) #t))
                      (check-equal?
                       (hash-ref (approval-request-view id digest) 'record-canonical-json)
                       (hash-ref (supplemental-approval-presentation ref) 'record-canonical-json))
                      (if (= requests 1)
                          (check-true (approval-decide! id digest #f))
                          (check-true (cancel-approval-request! id)))))
                  (check-false (request-supplemental-approval! ref publisher))
                  (check-false (request-supplemental-approval! ref publisher))
                  (check-equal? requests 2)
                  (parameterize ([current-directory base])
                    (define bus (make-event-bus))
                    (define ctx
                      (shared:make-tui-ctx #:session-runner void #:event-bus bus #:session-queue #f))
                    (define ui-requests 0)
                    (subscribe! bus
                                (lambda (event)
                                  (when (equal? (event-ev event)
                                                "supplemental-evidence.approval-requested")
                                    (set! ui-requests (add1 ui-requests)))))
                    (define raw
                      (format "/evidence-replan authorize-disposition ~a 1 ~a"
                              plan
                              (hash-ref ref 'sha256)))
                    (shared:process-slash-command ctx 'unknown raw)
                    ;; No input dispatch or y key. Wrong kind/campaign refuse synchronously.
                    (for ([route '("authorize-task" "authorize-recovery")])
                      (check-true
                       (handle-owner-evidence-command!
                        ctx
                        (format "/evidence-replan ~a ~a 1 ~a" route plan (hash-ref ref 'sha256)))))
                    (check-true (handle-owner-evidence-command!
                                 ctx
                                 (format "/evidence-replan authorize-disposition ~a 0 ~a"
                                         plan
                                         (hash-ref ref 'sha256))))
                    (check-equal? ui-requests 0))
                  (check-equal? (pending-approval-count) 0))
                clear-approval-channel!)
               (check-equal? (load-supplemental-pointer base plan 1) parent)
               (check-equal? (load-supplemental-operation (hash-ref parent 'task-approval))
                             (disposition-fixture-parent-operation df))))))))
     (lambda () (delete-directory/files home)))))
