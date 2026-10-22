#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 7200
;; Genuine temporary approvals/proofs. No live store, grants or provider I/O.
(require rackunit
         rackunit/text-ui
         "helpers/supplemental-go-fixture.rkt"
         "helpers/supplemental-worker-fixture.rkt"
         "helpers/honest-delivery-fixture.rkt"
         "../llm/model.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/command-handlers.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt")

(define wrong-file "tests/test-gsd-wave-executor-isolation.rkt")
(define (go fx factory)
  (define root (supplemental-fixture-root fx))
  (define plan-id (supplemental-fixture-plan fx))
  ;; The frozen implementation Files deliberately disagree with the two new
  ;; supplemental paths. The real prompt builder must replace, not append.
  (define plan
    (gsd-plan (list (gsd-wave 1
                              "Supplemental"
                              'pending
                              "OBSOLETE IMPLEMENTATION GOAL"
                              '("q/runtime/local.rkt")
                              '()
                              (supplemental-fixture-c0 fx)
                              '("implement the fix")))
              #f
              '()
              '()))
  (execute-campaign-request!
   (make-campaign-request root
                          (load-campaign-record root plan-id)
                          (lambda (wave) (build-single-wave-prompt root plan wave))
                          (lambda _ (error 'worker-contract "ordinary verifier forbidden"))
                          #:timeout-sec 7200
                          #:delivery-reader (test-delivered-reader)
                          #:delivery-coordinator
                          (lambda _ (error 'worker-contract "ordinary coordinator forbidden")))
   ;; The fixture composes the same production fresh-worker runner as the TUI.
   (lambda (prompt) ((factory) prompt))))

(define (check-provider-contract fx observation)
  (define requests (unbox (worker-observation-requests observation)))
  (define paths (fixture-task-paths fx))
  (define cwd (path->string (supplemental-fixture-repo fx)))
  (check-true (>= (length requests) 10) "actual provider turns plus forced empty-response re-anchor")
  (check-not-false (member "runtime.empty-response.retry"
                           (unbox (worker-observation-events observation)))
                   "the real runtime must actually take the empty-response re-anchor path")
  (check-equal? (request-user-texts (cadr requests))
                (request-user-texts (car requests))
                "re-anchor must project exactly the same supplemental task contract")
  (define ordinary (unbox (worker-observation-ordinary-request observation)))
  (check-not-false ordinary "another session was probed during the worker provider call")
  (for ([name '("bash" "set-task-state" "planning-write")])
    (check-not-false (member name (request-tool-names ordinary))
                     "session-local projection must not mutate the shared registry"))
  (check-equal? (length (unbox (worker-observation-sessions observation))) 1)
  (define lifecycle
    (filter (lambda (name) (member name '("session.shutdown" "session.started")))
            (reverse (unbox (worker-observation-events observation)))))
  (check-equal? lifecycle
                '("session.started" "session.shutdown" "session.started")
                "production factory must create the worker, then run the complete switch lifecycle")
  (check-equal? (unbox (worker-observation-forbidden-executions observation)) '())
  (for ([req requests]
        [i (in-naturals)])
    (with-check-info
     (['provider-turn i])
     (check-equal? (request-tool-names req)
                   '("edit" "read" "write")
                   "only installed read-only tools and scoped mutations")
     (for ([m (model-request-messages req)]
           #:when (equal? (hash-ref m 'role #f) "system"))
       (define system-text (format "~a" (hash-ref m 'content "")))
       (for ([directive '("Use record_conclusion" "Use set_task_state"
                                                  "exploration→planning→implementation")])
         (check-false (string-contains? system-text directive)
                      (format "contradictory provider system directive: ~a" directive))))
     (define controls (request-user-texts req))
     (check-true (pair? controls))
     (for ([text controls])
       (for ([path paths])
         (check-true (string-contains? text path) (format "missing exact allowed path ~a" path))
         (check-true (string-contains? text (path->string (build-path cwd path)))
                     "approved absolute path must resolve in physical checkout"))
       (check-true (string-contains? text cwd) "physical cwd, not logical q checkout")
       ;; Inspect the actual provider input, including the empty-response
       ;; retry: immutable behavioral intent must survive task projection.
       (for ([criterion
              '("READ-ONLY authenticated behavioral intent and expected outcomes"
                "The exported answer from runtime/local.rkt is 42 on candidate; the pinned baseline answer is 7."
                "The helper reads the exported production answer."
                "The witness expects answer 42, not baseline 7."
                "never as commands or write authority")])
         (check-true (string-contains? text criterion)
                     (format "missing frozen behavioral criterion: ~a" criterion)))
       (for ([stale '("IMPLEMENT NOW" "OBSOLETE IMPLEMENTATION GOAL"
                                      "q/runtime/local.rkt"
                                      "git commit"
                                      "git add"
                                      ".planning/scratch/"
                                      "Start implementing immediately")])
         (check-false (string-contains? text stale)
                      (format "stale task-control directive ~a" stale))))))
  ;; Tool results must be observed through a subsequent actual provider request.
  ;; Serialize only test fixture messages; no private authority credentials enter them.
  (define messages (format "~s" (model-request-messages (last requests))))
  (check-true (string-contains? messages "AUTHOR_PATH_OUT_OF_SCOPE")
              "wrong-file edit must retain the hook's actionable denial reason")
  (check-true (string-contains? messages wrong-file))
  (check-true (string-contains? messages "AUTHOR_TOOL_NOT_ALLOWED"))
  (for ([path paths])
    (check-true (string-contains? messages path)))
  (check-true (string-contains? messages cwd))
  (define results
    (for/hash ([m (model-request-messages (last requests))]
               #:when (equal? (hash-ref m 'role #f) "tool"))
      (values (hash-ref m 'tool_call_id) (hash-ref m 'content))))
  (check-equal? (hash-count results) 8)
  (for ([id '("worker-1" "worker-2" "worker-3")])
    (check-true (string-contains? (hash-ref results id) "AUTHOR_TOOL_NOT_ALLOWED")))
  (for ([id '("worker-4" "worker-5")])
    (check-true (string-prefix? (hash-ref results id) "Wrote ")))
  (for ([id '("worker-6" "worker-7")])
    (check-true (string-prefix? (hash-ref results id) "Edited "))))

(define (exercise version)
  (call-with-supplemental-fixture
   (lambda (fx)
     (define root (supplemental-fixture-root fx))
     (define repo (supplemental-fixture-repo fx))
     (define logical (build-path root "q"))
     (define plan (supplemental-fixture-plan fx))
     ;; Existing logical twins make wrong-root writes/edits observable by bytes,
     ;; not merely by checking that a previously absent path stays absent.
     (for ([p (fixture-task-paths fx)]
           [source (list witness-source helper-source)])
       (define target (build-path logical p))
       (make-parent-directory* target)
       (display-to-file (string-append "; logical original: do not mutate\n" source)
                        target
                        #:exists 'error))
     (define logical-targets
       (for/hash ([p (fixture-task-paths fx)])
         (values p (file->bytes (build-path logical p)))))
     (fixture-finalize-predecessor! fx)
     (check-not-equal? (simplify-path logical) (simplify-path repo))
     (define historical (file->bytes (delivery-journal-path root plan 1)))
     (define snapshot (build-path root ".planning" "campaigns" plan "plan-snapshot"))
     (define (snapshot-bytes)
       (for/hash ([p (find-files file-exists? snapshot)])
         (values (find-relative-path snapshot p) (file->bytes p))))
     (define frozen (snapshot-bytes))
     (define original (file->bytes (build-path repo "runtime/local.rkt")))
     (define logical-original (file->bytes (build-path logical "runtime/local.rkt")))
     (define parent (load-supplemental-pointer root plan 1))
     (define parent-approval (hash-ref parent 'task-approval))
     (define parent-records
       (for/list ([key '(task-proposal task-approval)])
         (load-supplemental-record (hash-ref parent key))))
     (define parent-op #f)
     (when (= version 2)
       ;; Consume the parent honestly with a no-edit invocation. Recovery is
       ;; created by the real owner API and approved through the real broker.
       (void (run-campaign! root
                            (load-campaign-record root plan)
                            #:isolate? #f
                            #:timeout-sec 7200
                            #:delivery-reader (test-delivered-reader)
                            #:delivery-coordinator (lambda _ (error 'worker-contract "fallback"))
                            #:runner (lambda (_) 'ok)
                            #:verifier (lambda _ (error 'worker-contract "ordinary verifier"))))
       (set! parent-op (load-supplemental-operation parent-approval))
       (check-equal? (hash-ref parent-op 'state) "blocked")
       (define-values (child task)
         (propose-supplemental-recovery! root
                                         plan
                                         1
                                         (hash-ref (hash-ref parent 'task-proposal) 'sha256)
                                         (+ (current-seconds) 7200)))
       (check-equal? (hash-ref task 'kind) "supplemental-evidence-task/v2")
       (install-supplemental-recovery! root plan 1 child (fixture-approve! child)))
     (define pointer (load-supplemental-pointer root plan 1))
     (define approval (hash-ref pointer 'task-approval))
     (define before (committed-delivery-snapshot repo))
     (for ([p (fixture-task-paths fx)])
       (check-false (file-exists? (build-path repo p))))
     (define home (build-path root "worker-home"))
     (make-directory home)
     (define env (environment-variables-copy (current-environment-variables)))
     (environment-variables-set! env #"HOME" (path->bytes home))
     (environment-variables-set! env #"PLTUSERHOME" (path->bytes home))
     (define-values (factory observation)
       (make-scripted-worker-factory (build-path root "worker-sessions")
                                     repo
                                     (supplemental-fixture-test-path fx)
                                     (supplemental-fixture-harness-path fx)
                                     #:project-dir logical))
     (parameterize ([current-environment-variables env]
                    ;; Avoid even a freshness remote probe in this hermetic test.
                    [current-gsd-freshness-check
                     (lambda (_) (campaign-freshness "fixture" "fixture" #f #f #f))])
       (void (go fx factory)))
     ;; Check physical results BEFORE prompt assertions, so behavioral RED is
     ;; not confused with an unexecuted model or missing API compile failure.
     (check-equal? (file->string (build-path repo (supplemental-fixture-test-path fx)))
                   (string-replace witness-source "new regression" "provider authored regression"))
     (check-equal? (file->string (build-path repo (supplemental-fixture-harness-path fx)))
                   (string-replace helper-source "(= answer 42)" "(equal? answer 42)"))
     (check-false (file-exists? (build-path repo wrong-file)))
     (check-false (file-exists? (build-path repo "forbidden-shell")))
     (check-equal? (file->bytes (build-path repo "runtime/local.rkt")) original)
     (check-equal? (file->bytes (build-path logical "runtime/local.rkt")) logical-original)
     (for ([(p bytes) (in-hash logical-targets)])
       (check-equal? (file->bytes (build-path logical p))
                     bytes
                     "logical originals must remain byte-for-byte unchanged"))
     (define after (committed-delivery-snapshot repo))
     (check-not-false after "owner committed the scheduler's real writes")
     (check-not-equal? (hash-ref after 'head) (hash-ref before 'head))
     (check-not-equal? (hash-ref after 'tree) (hash-ref before 'tree))
     (check-equal? (hash-ref (load-supplemental-operation approval) 'state) "candidate-sealed")
     (check-equal? (file->bytes (delivery-journal-path root plan 1)) historical)
     (check-equal? (snapshot-bytes) frozen)
     (for ([key '(task-proposal task-approval)]
           [record parent-records])
       (check-equal? (load-supplemental-record (hash-ref parent key)) record))
     (when parent-op
       (check-equal? (load-supplemental-operation parent-approval) parent-op))
     (check-provider-contract fx observation)
     (void (go fx factory))
     (check-equal? (length (unbox (worker-observation-sessions observation)))
                   1
                   "spent invocation cannot dispatch again"))
   #:completed-author? #t
   #:wave-index 1
   #:physical-worktree? #t
   #:expires-in-sec 14400))

(define evidence-suite
  (test-suite "supplemental actual worker contract"
    (test-case "v1 provider-driven writes and owner candidate seal"
      (exercise 1))
    (test-case "v2 provider-driven recovery preserves parent"
      (exercise 2))))
(provide evidence-suite)
(module+ test
  (check-equal? (run-tests evidence-suite) 0))
