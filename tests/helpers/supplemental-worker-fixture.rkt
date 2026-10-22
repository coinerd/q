#lang racket/base
;; A scripted provider, not an author stub: all mutations cross run-prompt!,
;; the scheduler, the registered production GSD hook, and installed builtins.
(require json
         racket/file
         racket/list
         racket/string
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         "../../runtime/agent-session.rkt"
         "../../tui/campaign-runner.rkt"
         "../../extensions/api.rkt"
         "../../extensions/gsd-planning.rkt"
         "../../tools/tool.rkt"
         "../../tools/registry-defaults.rkt"
         "../../tools/permission-gate.rkt"
         "../../util/event/event-bus.rkt"
         "../../util/event/event.rkt")
(provide (struct-out worker-observation)
         make-scripted-worker-factory
         witness-source
         helper-source
         request-tool-names
         request-user-texts)

(struct worker-observation (requests sessions forbidden-executions ordinary-request events)
  #:transparent)
(define helper-source
  (string-append "#lang racket/base\n(require \"../../runtime/local.rkt\")\n"
                 "(provide witness)\n(define (witness) (= answer 42))\n"))
(define witness-source
  (string-append
   "#lang racket\n;; @speed fast\n;; @suite extensions\n;; @boundary unit\n;; @timeout 120\n"
   "(require rackunit \"helpers/witness.rkt\")\n(provide evidence-suite)\n"
   "(define evidence-suite (test-suite \"witness\" (test-case \"new regression\" "
   "(check-true (witness)))))\n"
   "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))

(define (request-tool-names req)
  (sort (for/list ([schema (in-list (or (model-request-tools req) '()))])
          (hash-ref (hash-ref schema 'function schema) 'name))
        string<?))
(define (request-user-texts req)
  (for/list ([m (in-list (model-request-messages req))]
             #:when (member (hash-ref m 'role #f) '("user" user)))
    (define content (hash-ref m 'content ""))
    (if (string? content)
        content
        (string-join (for/list ([p (in-list content)]
                                #:when (equal? (hash-ref p 'type #f) "text"))
                       (hash-ref p 'text ""))
                     "\n"))))

(define (make-scripted-worker-factory store
                                      repo
                                      test-path
                                      helper-path
                                      #:project-dir [project-dir repo])
  (define requests (box '()))
  (define sessions (box '()))
  (define forbidden (box '()))
  (define ordinary-request (box #f))
  (define events (box '()))
  (define bus (make-event-bus))
  (subscribe! bus (lambda (event) (set-box! events (cons (event-ev event) (unbox events)))))
  (define observation (worker-observation requests sessions forbidden ordinary-request events))
  (define registry (make-tool-registry))
  (register-default-tools! registry #:only '("read" "write" "edit" "bash"))
  ;; Installed but forbidden mutations must also be rejected when forged.
  (for ([name '("set-task-state" "planning-write")])
    (register-tool! registry
                    (make-tool name
                               "Forbidden mutation sentinel"
                               (hasheq 'type "object" 'properties (hasheq))
                               (lambda (_args _ctx)
                                 (set-box! forbidden (cons name (unbox forbidden)))
                                 (make-success-result "MUTATION EXECUTED")))))
  (define extensions (make-extension-registry))
  (register-extension! extensions gsd-planning-extension)
  ;; Construct the ordinary session BEFORE the owned dispatch, sharing the
  ;; registry. Probe it while the worker's provider call is in flight.
  (define ordinary-provider
    (make-provider (lambda () "ordinary-script")
                   (lambda () (hasheq 'streaming #t))
                   (lambda (_) (error 'script "unexpected non-streaming ordinary request"))
                   (lambda (req)
                     (set-box! ordinary-request req)
                     (list (make-stream-chunk "Ordinary session unchanged." #f #f #f)
                           (make-stream-chunk #f #f (hasheq) #t)))))
  (define ordinary-session
    (make-agent-session (hasheq 'provider
                                ordinary-provider
                                'tool-registry
                                registry
                                'extension-registry
                                extensions
                                'event-bus
                                (make-event-bus)
                                'session-dir
                                (path->string (build-path store "ordinary"))
                                'project-dir
                                (path->string project-dir)
                                'model-name
                                "ordinary-script")))
  (define script
    (list
     (cons "edit"
           (hasheq 'path
                   "tests/test-gsd-wave-executor-isolation.rkt"
                   'old-text
                   "original"
                   'new-text
                   "CORRUPTED"))
     (cons "set-task-state" (hasheq 'state "implementation"))
     (cons "bash" (hasheq 'command "touch forbidden-shell"))
     (cons "planning-write" (hasheq 'artifact "STATE" 'content "forbidden"))
     (cons "write" (hasheq 'path helper-path 'content helper-source))
     (cons "write" (hasheq 'path test-path 'content witness-source))
     ;; Editing both newly created approved files proves the positive path is
     ;; not just an accepted write schema or a direct fixture filesystem write.
     (cons "edit" (hasheq 'path helper-path 'old-text "(= answer 42)" 'new-text "(equal? answer 42)"))
     (cons
      "edit"
      (hasheq 'path test-path 'old-text "new regression" 'new-text "provider authored regression"))))
  (define turn 0)
  (define provider
    (make-provider
     (lambda () "supplemental-script")
     (lambda () (hasheq 'streaming #t))
     (lambda (_) (error 'script "non-streaming request unexpected"))
     (lambda (req)
       (set-box! requests (append (unbox requests) (list req)))
       (unless (unbox ordinary-request)
         (call-with-values (lambda ()
                             (run-prompt! ordinary-session "Read-only ordinary session probe."))
                           (lambda _ (void))))
       ;; Force the real runtime's empty-response re-anchor before authoring.
       (define i (sub1 turn))
       (set! turn (add1 turn))
       (cond
         [(negative? i)
          (list (make-stream-chunk #f #f #f #f #:delta-thinking "Considering the author contract.")
                (make-stream-chunk #f #f (hasheq) #t))]
         [(< i (length script))
          (let ([call (list-ref script i)])
            (list (make-stream-chunk
                   #f
                   (hasheq 'index
                           0
                           'id
                           (format "worker-~a" i)
                           'function
                           (hasheq 'name (car call) 'arguments (jsexpr->string (cdr call))))
                   #f
                   #f)
                  (make-stream-chunk #f #f (hasheq) #t)))]
         [else
          (list (make-stream-chunk "The two approved evidence files are authored." #f #f #f)
                (make-stream-chunk #f #f (hasheq) #t))]))))
  (define (factory)
    (make-campaign-runner (hasheq 'provider
                                  provider
                                  'tool-registry
                                  registry
                                  'extension-registry
                                  extensions
                                  'event-bus
                                  bus
                                  'session-dir
                                  (path->string store)
                                  'project-dir
                                  (path->string project-dir)
                                  'repo-root
                                  (path->string repo)
                                  'model-name
                                  "supplemental-script"
                                  'parallel-tools
                                  #f
                                  'max-iterations
                                  24
                                  'permission-config
                                  (make-permissive-permission-config))
                          ordinary-session
                          #:on-created (lambda (session)
                                         (set-box! sessions (cons session (unbox sessions))))))
  (values factory observation))
