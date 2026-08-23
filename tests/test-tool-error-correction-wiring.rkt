#lang racket

;; @speed fast
;; @suite runtime
;; @boundary integration
;; BOUNDARY: integration

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         "../runtime/agent-session.rkt"
         "../util/event/event-bus.rkt"
         "../util/message/protocol-types.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-registry
                  register-tool!
                  make-error-result
                  make-success-result)
         (only-in "../tools/permission-gate.rkt" make-permissive-permission-config))

(define (provider-message-text m)
  (define content (hash-ref m 'content ""))
  (if (string? content)
      content
      (format "~a" content)))

(define (correction-message? m)
  (regexp-match? #rx"Corrective checkpoint" (provider-message-text m)))

(define correction-wiring-tests
  (test-suite "production tool error correction wiring"
    (test-case "third equivalent failure injects one provider-visible corrective checkpoint"
      (define dir (make-temporary-file "q-error-correction-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define bus (make-event-bus))
         (define events (box '()))
         (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list evt)))))
         (define executions (box 0))
         (define registry (make-tool-registry))
         (register-tool!
          registry
          (make-tool
           "read"
           "deterministic failing read"
           (hasheq 'type
                   "object"
                   'properties
                   (hasheq 'path (hasheq 'type "string"))
                   'required
                   '("path"))
           (lambda (_args _ctx)
             (define n (add1 (unbox executions)))
             (set-box! executions n)
             (cond
               [(= n 2) (make-success-result (list "materially different success") (hasheq))]
               [(= n 5) (make-error-result "command not found: unavailable-probe")]
               [else (make-error-result "No such file or directory: /wrong/root/PLAN.md")]))))
         (define requests (box '()))
         (define turn (box 0))
         (define provider
           (make-provider
            (lambda () "error-correction-mock")
            (lambda () (hash 'streaming #t 'token-counting #t))
            (lambda (_req) (error "non-streaming path not expected"))
            (lambda (req)
              (set-box! requests (append (unbox requests) (list (model-request-messages req))))
              (define n (add1 (unbox turn)))
              (set-box! turn n)
              (if (<= n 5)
                  (list
                   (make-stream-chunk
                    #f
                    (hasheq 'index
                            0
                            'id
                            (format "failed-read-~a" n)
                            'function
                            (hasheq 'name "read" 'arguments "{\"path\":\"/wrong/root/PLAN.md\"}"))
                    #f
                    #f)
                   (make-stream-chunk #f
                                      #f
                                      (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                                      #t))
                  (list (make-stream-chunk "recovered" #f #f #f)
                        (make-stream-chunk
                         #f
                         #f
                         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                         #t))))))
         (define config
           (hash 'provider
                 provider
                 'tool-registry
                 registry
                 'event-bus
                 bus
                 'session-dir
                 dir
                 'project-dir
                 dir
                 'permission-config
                 (make-permissive-permission-config)))
         (define session (make-agent-session config))
         ;; The fourth tool turn is beyond the soft limit, proving that lane
         ;; observes errors too. Three file failures are interleaved with one
         ;; success but still fall within the frozen eight-result window.
         (run-prompt! session "Find the active plan" #:max-iterations 3)
         (check-equal? (unbox executions) 5)
         (check-equal? (length (unbox requests)) 6)
         (define correction-request (list-ref (unbox requests) 4))
         (define corrections (filter correction-message? correction-request))
         (check-equal? (length corrections) 1)
         (check-regexp-match #rx"file-not-found" (provider-message-text (first corrections)))
         (check-regexp-match #rx"Do not repeat" (provider-message-text (first corrections)))
         (define correction-checkpoints
           (filter (lambda (m) (regexp-match? #rx"Operational Checkpoint" (provider-message-text m)))
                   correction-request))
         (check-equal? (length correction-checkpoints) 1)
         (check-regexp-match #rx"last_error:[ ]+file-not-found \\(3\\)"
                             (provider-message-text (first correction-checkpoints)))
         ;; A materially different error starts a new episode and removes the
         ;; prior ephemeral correction before the next provider request.
         (define final-request (last (unbox requests)))
         (check-equal? (filter correction-message? final-request) '())
         (define final-checkpoints
           (filter (lambda (m) (regexp-match? #rx"Operational Checkpoint" (provider-message-text m)))
                   final-request))
         (check-equal? (length final-checkpoints) 1)
         (check-regexp-match #rx"last_error:[ ]+command-not-found \\(1\\)"
                             (provider-message-text (first final-checkpoints)))
         (define correction-events
           (filter (lambda (evt) (equal? (event-ev evt) "iteration.error-correction"))
                   (unbox events)))
         (check-equal? (length correction-events) 1)
         (check-equal? (hash-ref (event-payload (first correction-events)) 'error-class)
                       "file-not-found"))
       (lambda ()
         (when (directory-exists? dir)
           (delete-directory/files dir)))))

    (test-case "failures outside the last eight results do not trigger correction"
      (define dir (make-temporary-file "q-error-window-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define outcomes
           '(file-not-found file-not-found
                            success
                            success
                            success
                            success
                            success
                            success
                            file-not-found))
         (define bus (make-event-bus))
         (define events (box '()))
         (subscribe! bus (lambda (evt) (set-box! events (cons evt (unbox events)))))
         (define executions (box 0))
         (define registry (make-tool-registry))
         (register-tool! registry
                         (make-tool "read"
                                    "window probe"
                                    (hasheq 'type
                                            "object"
                                            'properties
                                            (hasheq 'path (hasheq 'type "string"))
                                            'required
                                            '("path"))
                                    (lambda (_args _ctx)
                                      (define n (unbox executions))
                                      (set-box! executions (add1 n))
                                      (if (eq? (list-ref outcomes n) 'success)
                                          (make-success-result (list "ok") (hasheq))
                                          (make-error-result
                                           "No such file or directory: /window/path")))))
         (define requests (box '()))
         (define turn (box 0))
         (define provider
           (make-provider
            (lambda () "error-window-mock")
            (lambda () (hash 'streaming #t 'token-counting #t))
            (lambda (_req) (error "non-streaming path not expected"))
            (lambda (req)
              (set-box! requests (append (unbox requests) (list (model-request-messages req))))
              (define n (unbox turn))
              (set-box! turn (add1 n))
              (if (< n (length outcomes))
                  (list (make-stream-chunk
                         #f
                         (hasheq 'index
                                 0
                                 'id
                                 (format "window-read-~a" n)
                                 'function
                                 (hasheq 'name "read" 'arguments "{\"path\":\"/window/path\"}"))
                         #f
                         #f)
                        (make-stream-chunk #f #f (hasheq 'total-tokens 1) #t))
                  (list (make-stream-chunk "done" #f #f #f)
                        (make-stream-chunk #f #f (hasheq 'total-tokens 1) #t))))))
         (define session
           (make-agent-session (hash 'provider
                                     provider
                                     'tool-registry
                                     registry
                                     'event-bus
                                     bus
                                     'session-dir
                                     dir
                                     'project-dir
                                     dir
                                     'permission-config
                                     (make-permissive-permission-config))))
         (run-prompt! session "Exercise the bounded window" #:max-iterations 20)
         (check-equal? (unbox executions) 9)
         (check-equal? (length (unbox requests)) 10)
         (check-equal? (filter correction-message? (last (unbox requests))) '())
         (check-equal? (filter (lambda (evt) (equal? (event-ev evt) "iteration.error-correction"))
                               (unbox events))
                       '()))
       (lambda ()
         (when (directory-exists? dir)
           (delete-directory/files dir)))))

    (test-case "trailing different result in crossing batch removes correction"
      (define dir (make-temporary-file "q-error-batch-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define bus (make-event-bus))
         (define events (box '()))
         (subscribe! bus (lambda (evt) (set-box! events (cons evt (unbox events)))))
         (define executions (box 0))
         (define execution-lock (make-semaphore 1))
         (define registry (make-tool-registry))
         (register-tool! registry
                         (make-tool "read"
                                    "ordered batch probe"
                                    (hasheq 'type
                                            "object"
                                            'properties
                                            (hasheq 'path (hasheq 'type "string"))
                                            'required
                                            '("path"))
                                    (lambda (args _ctx)
                                      (call-with-semaphore
                                       execution-lock
                                       (lambda () (set-box! executions (add1 (unbox executions)))))
                                      (if (equal? (hash-ref args 'path) "/different")
                                          (make-error-result "command not found: different-probe")
                                          (make-error-result "No such file or directory: /same")))))
         (define requests (box '()))
         (define turn (box 0))
         (define provider
           (make-provider
            (lambda () "ordered-batch-mock")
            (lambda () (hash 'streaming #t 'token-counting #t))
            (lambda (_req) (error "non-streaming path not expected"))
            (lambda (req)
              (set-box! requests (append (unbox requests) (list (model-request-messages req))))
              (define n (add1 (unbox turn)))
              (set-box! turn n)
              (cond
                [(<= n 2)
                 (define path (if (= n 1) "/different" "/same"))
                 (list (make-stream-chunk
                        #f
                        (hasheq 'index
                                0
                                'id
                                (format "prior-~a" n)
                                'function
                                (hasheq 'name "read" 'arguments (format "{\"path\":\"~a\"}" path)))
                        #f
                        #f)
                       (make-stream-chunk #f #f (hasheq 'total-tokens 1) #t))]
                [(= n 3)
                 (list
                  (make-stream-chunk #f
                                     (hasheq 'index
                                             0
                                             'id
                                             "crossing-same-1"
                                             'function
                                             (hasheq 'name "read" 'arguments "{\"path\":\"/same\"}"))
                                     #f
                                     #f)
                  (make-stream-chunk #f
                                     (hasheq 'index
                                             1
                                             'id
                                             "crossing-same-2"
                                             'function
                                             (hasheq 'name "read" 'arguments "{\"path\":\"/same\"}"))
                                     #f
                                     #f)
                  (make-stream-chunk
                   #f
                   (hasheq 'index
                           2
                           'id
                           "trailing-different"
                           'function
                           (hasheq 'name "read" 'arguments "{\"path\":\"/different\"}"))
                   #f
                   #f)
                  (make-stream-chunk #f #f (hasheq 'total-tokens 1) #t))]
                [else
                 (list (make-stream-chunk "done" #f #f #f)
                       (make-stream-chunk #f #f (hasheq 'total-tokens 1) #t))]))))
         (define session
           (make-agent-session (hash 'provider
                                     provider
                                     'tool-registry
                                     registry
                                     'event-bus
                                     bus
                                     'session-dir
                                     dir
                                     'project-dir
                                     dir
                                     'permission-config
                                     (make-permissive-permission-config))))
         (run-prompt! session "Exercise ordered batch recovery" #:max-iterations 10)
         (check-equal? (unbox executions) 5)
         (check-equal? (length (unbox requests)) 4)
         (define final-request (last (unbox requests)))
         (check-equal? (filter correction-message? final-request) '())
         (define checkpoints
           (filter (lambda (m) (regexp-match? #rx"Operational Checkpoint" (provider-message-text m)))
                   final-request))
         (check-equal? (length checkpoints) 1)
         (check-regexp-match #rx"last_error:[ ]+command-not-found \\(1\\)"
                             (provider-message-text (first checkpoints)))
         (define corrections
           (filter (lambda (evt) (equal? (event-ev evt) "iteration.error-correction"))
                   (unbox events)))
         (check-equal? (length corrections) 1)
         (check-equal? (hash-ref (event-payload (first corrections)) 'error-class) "file-not-found"))
       (lambda ()
         (when (directory-exists? dir)
           (delete-directory/files dir)))))))

(module+ test
  (run-tests correction-wiring-tests))
