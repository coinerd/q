#lang racket/base
;; @speed fast
;; @suite default
;; @boundary integration
(require rackunit
         racket/file
         racket/dict
         racket/string
         "../util/worker-tool-policy.rkt"
         "../util/capability.rkt"
         "../util/hook-types.rkt"
         "../tools/scheduler-denial.rkt"
         "../tools/scheduler.rkt"
         "../tools/tool.rkt"
         "../tools/permission-gate.rkt"
         "../tools/registry-defaults.rkt"
         (only-in "../sandbox/gateway-bridge.rkt" current-execution-plane-enabled)
         "../runtime/session/session-config.rkt"
         "../runtime/turn-orchestrator.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../util/event/event-bus.rkt"
         (only-in "../runtime/agent-session.rkt" make-agent-session)
         (only-in "../runtime/session/session-types.rkt" agent-session-config))

(module+ test
  (define schema (hasheq 'name "write"))
  (define policy
    (worker-tool-policy
     (lambda (schemas) (filter (lambda (s) (equal? (hash-ref s 'name) "write")) schemas))
     (lambda (name args)
       (cond
         [(not (equal? name "write")) "TOOL_NOT_ALLOWED"]
         [(not (equal? (hash-ref args 'path #f) "allowed")) "PATH_OUT_OF_SCOPE: use allowed"]
         [else #f]))))
  (test-case "trusted capture survives config copies; JSON cannot select it"
    (define cfg
      (parameterize ([current-worker-tool-policy policy])
        (capture-worker-tool-policy (hash->session-config (hasheq)))))
    (check-eq? (config-worker-tool-policy
                (hash->session-config (hash-set (session-config->hash cfg) 'model-name "other")))
               policy)
    (check-false (config-worker-tool-policy (hasheq 'worker-tool-policy policy)))
    (check-false (config-worker-tool-policy (capture-worker-tool-policy cfg)))
    (check-false (current-worker-tool-policy)))
  (test-case "projection is a local intersection, malformed callbacks fail closed"
    (define schemas (list schema (hasheq 'name "hidden")))
    (check-equal? (filter-worker-tools schemas policy) (list schema))
    (check-eq? (filter-worker-tools schemas) schemas)
    (for ([filter-proc (in-list (list (lambda (_) #f)
                                      (lambda (_) (error 'bad "secret"))
                                      (lambda (_) (list (hasheq 'name "forged")))))])
      (check-equal? (filter-worker-tools schemas (worker-tool-policy filter-proc (lambda (_a _b) #f)))
                    '())))
  (test-case "fresh sessions capture only the trusted dynamic policy"
    (define root (make-temporary-file "worker-session~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (define cfg (hasheq 'session-dir root 'event-bus (make-event-bus)))
                    (define restricted
                      (parameterize ([current-worker-tool-policy policy])
                        (make-agent-session cfg)))
                    (define ordinary (make-agent-session cfg))
                    (check-eq? (config-worker-tool-policy (agent-session-config restricted)) policy)
                    (check-false (config-worker-tool-policy (agent-session-config ordinary))))
                  (lambda () (delete-directory/files root))))
  (test-case "actual provider requests apply policy after custom assembly on every turn"
    (define seen '())
    (define mock
      (make-mock-provider
       (model-response '()
                       (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                       "fixture"
                       'stop
                       #f)))
    (define prov
      (make-provider (lambda () "worker-policy-fixture")
                     (lambda () (hasheq 'streaming #f))
                     (lambda (request)
                       (set! seen (cons (model-request-tools request) seen))
                       (model-response '()
                                       (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                                       "fixture"
                                       'stop
                                       #f))
                     (lambda (request)
                       (set! seen (cons (model-request-tools request) seen))
                       (provider-stream mock request))))
    (define reg (make-tool-registry))
    (register-tool! reg
                    (make-tool "write"
                               "fixture"
                               (hasheq 'type "object" 'properties (hasheq))
                               (lambda (_args _ctx) (make-success-result "ok"))))
    (define schemas (list schema (hasheq 'name "hidden")))
    (define (turn p)
      (parameterize ([current-worker-tool-policy p])
        (run-provider-turn '()
                           prov
                           (make-event-bus)
                           reg
                           #f
                           "fixture-session"
                           "fixture-turn"
                           #f
                           (hash->session-config (hasheq))
                           #:tool-list-proc (lambda (_base _ext) schemas))))
    (turn policy)
    (turn #f)
    (turn policy)
    (check-equal? (reverse seen) (list (list schema) schemas (list schema))))
  (test-case "worker provider schemas intersect registered capability authority"
    (define root (make-temporary-file "worker-caps~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define reg (make-tool-registry))
       (register-default-tools! reg #:only '("write" "edit"))
       (define seen '())
       (define mock
         (make-mock-provider
          (model-response '()
                          (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                          "fixture"
                          'stop
                          #f)))
       (define prov
         (make-provider
          (lambda () "worker-caps-fixture")
          (lambda () (hasheq 'streaming #f))
          (lambda (request)
            (set! seen (model-request-tools request))
            (model-response '()
                            (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                            "fixture"
                            'stop
                            #f))
          (lambda (request)
            (set! seen (model-request-tools request))
            (provider-stream mock request))))
       (define p (worker-tool-policy values (lambda (_name _args) #f)))
       ;; Neither schema-supplied metadata nor unknown custom projections grant authority.
       (define custom
         (list (hasheq 'name "write" 'required-capability 'read-only)
               (hasheq 'name "edit")
               (hasheq 'name "unregistered")
               (hasheq 'name 42)))
       (define (turn caps worker? custom? [configured? #t] [inherited caps])
         (parameterize ([current-worker-tool-policy (and worker? p)]
                        [current-session-capabilities inherited])
           (run-provider-turn '()
                              prov
                              (make-event-bus)
                              reg
                              #f
                              "caps-session"
                              "caps-turn"
                              #f
                              (hash->session-config (if configured?
                                                        (hasheq 'capabilities caps)
                                                        (hasheq)))
                              #:tool-list-proc
                              (and custom?
                                   (lambda (_base _ext)
                                     ;; Assembly must not change the captured authority.
                                     (current-session-capabilities '(any))
                                     custom)))))
       (define (schema-name s)
         (hash-ref (hash-ref s 'function s) 'name))
       (for ([custom? '(#f #t)])
         (turn '(read-only) #t custom?)
         (check-equal? seen '())
         (turn '(file-write) #t custom?)
         (check-equal? (map schema-name seen) '("write" "edit")))
       (turn '(read-only) #t #t #f)
       (check-equal? seen '())
       (turn '(file-write) #t #t #f)
       (check-equal? (map schema-name seen) '("write" "edit"))
       ;; Configured authority wins over an ambient wildcard (and vice versa).
       (turn '(read-only) #t #t #t '(any))
       (check-equal? seen '())
       (turn '(file-write) #t #t #t '(read-only))
       (check-equal? (map schema-name seen) '("write" "edit"))
       (for ([invalid (in-list (list #f '(file-write invalid) "file-write"))])
         (turn invalid #t #t)
         (check-equal? seen '()))
       (turn '(read-only) #f #t)
       (check-equal? seen custom)
       (turn '(read-only) #f #f)
       (check-equal? (map schema-name seen) '("write" "edit"))
       (define target (path->string (build-path root "approved.txt")))
       (define (invoke caps name args)
         (parameterize ([current-worker-tool-policy p]
                        [current-execution-plane-enabled #f])
           (car (scheduler-result-results
                 (run-tool-batch (list (make-tool-call "caps-call" name args))
                                 reg
                                 #:exec-context
                                 (make-exec-context #:working-directory root
                                                    #:capabilities caps
                                                    #:permission-config
                                                    (make-permissive-permission-config)))))))
       (define write-args (hasheq 'path target 'content "before"))
       (check-true (tool-result-is-error? (invoke '(read-only) "write" write-args)))
       (check-false (file-exists? target))
       (check-false (tool-result-is-error? (invoke '(file-write) "write" write-args)))
       (define edit-args (hasheq 'path target 'old-text "before" 'new-text "after"))
       (check-true (tool-result-is-error? (invoke '(read-only) "edit" edit-args)))
       (check-false (tool-result-is-error? (invoke '(file-write) "edit" edit-args)))
       (check-equal? (file->string target) "after"))
     (lambda () (delete-directory/files root))))
  (test-case "safe reasons never serialize payloads and redact before bounding"
    (for ([reason (in-list (list #f 17 (hasheq 'secret "private") "" " \n\t"))])
      (check-equal? (safe-denial-reason reason) "authorization denied (no safe reason supplied)"))
    (define clean
      (safe-denial-reason (string-append "\u001b[31mPATH\u001b[0m\npassword=hunter2 token=abc "
                                         (make-string 2000 #\x))))
    (check-true (<= (string-length clean) 1024))
    (check-false (regexp-match? #px"[\u0000-\u001f\u007f-\u009f]" clean))
    (check-false (string-contains? clean "hunter2"))
    (check-false (string-contains? clean "token=abc")))
  (test-case "installed built-in write executes through the real scheduler"
    (define root (make-temporary-file "worker-builtin~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define reg (make-tool-registry))
       (register-default-tools! reg #:only '("write"))
       (define target (path->string (build-path root "approved.txt")))
       (define p
         (worker-tool-policy values
                             (lambda (name args)
                               (and (not (and (equal? name "write")
                                              (equal? (hash-ref args 'path #f) target)))
                                    "PATH_OUT_OF_SCOPE"))))
       (define result
         (parameterize ([current-worker-tool-policy p]
                        [current-execution-plane-enabled #f])
           (run-tool-batch
            (list (make-tool-call "builtin" "write" (hasheq 'path target 'content "built-in bytes")))
            reg
            #:exec-context (make-exec-context #:working-directory root
                                              #:permission-config
                                              (make-permissive-permission-config)))))
       (check-false (tool-result-is-error? (car (scheduler-result-results result))))
       (check-equal? (file->string target) "built-in bytes"))
     (lambda () (delete-directory/files root))))
  (test-case "real scheduler executes allowed write, denies hidden and amended invocations"
    (define root (make-temporary-file "worker-policy~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define reg (make-tool-registry))
       (define executions 0)
       (define hooks 0)
       (for ([name '("write" "hidden")])
         (register-tool! reg
                         (make-tool name
                                    "fixture write"
                                    (hasheq 'type
                                            "object"
                                            'required
                                            '("path")
                                            'properties
                                            (hasheq 'path (hasheq 'type "string")))
                                    (lambda (args ctx)
                                      (set! executions (add1 executions))
                                      (call-with-output-file (build-path root (hash-ref args 'path))
                                                             (lambda (out)
                                                               (display "actual bytes" out))
                                                             #:exists 'truncate)
                                      (make-success-result "written")))))
       (define (run name path [hook #f] [p policy])
         (parameterize ([current-worker-tool-policy p])
           (car (scheduler-result-results
                 (run-tool-batch (list (make-tool-call "one" name (hasheq 'path path)))
                                 reg
                                 #:exec-context
                                 (make-exec-context #:working-directory root
                                                    #:permission-config
                                                    (make-permissive-permission-config))
                                 #:hook-dispatcher hook)))))
       (check-false (tool-result-is-error? (run "write" "allowed")))
       (check-equal? (file->string (build-path root "allowed")) "actual bytes")
       (check-true (tool-result-is-error? (run "hidden" "forged")))
       (define amended
         (run "write"
              "allowed"
              (lambda (point payload)
                (and (eq? point 'tool-call-pre)
                     (begin
                       (set! hooks (add1 hooks))
                       (hook-amend (hasheq 'args (hasheq 'path "forbidden"))))))))
       (check-true (tool-result-is-error? amended))
       (check-equal? hooks 1)
       (check-equal? executions 1)
       (check-false (file-exists? (build-path root "forbidden")))
       (for ([admit (in-list (list (lambda (_a _b) #t)
                                   (lambda (_a _b) "")
                                   (lambda (_a _b) (error 'admit "password=private"))))])
         (check-true (tool-result-is-error?
                      (run "write" "allowed" #f (worker-tool-policy values admit)))))
       (check-true (tool-result-is-error?
                    (run "write" "allowed" (lambda (_a _b) (error 'hook "password=private")))))
       (for ([reason (in-list (list ""
                                    #f
                                    42
                                    (hasheq 'secret "private")
                                    (string-append "\u001b[31mPATH\u001b[0m password=hunter2 "
                                                   (make-string 2000 #\x))))])
         (define blocked
           (run "write"
                "allowed"
                (lambda (point _payload) (and (eq? point 'tool-call-pre) (hook-block reason)))))
         (check-true (tool-result-is-error? blocked))
         (define text (hash-ref (car (tool-result-content blocked)) 'text))
         (check-true (< (string-length text) 1150))
         (check-false (regexp-match? #px"[\u0000-\u001f\u007f-\u009f]" text))
         (check-false (string-contains? text "hunter2")))
       (check-equal? executions 1)
       ;; No shared registry mutation: a normal invocation can still use hidden.
       (check-false (tool-result-is-error? (run "hidden" "ordinary" #f #f)))
       (check-equal? executions 2))
     (lambda () (delete-directory/files root)))))
