#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: integration

;; tests/test-scheduler-hooks.rkt — Wave 11: TS1-TS8 scheduler hook protocol
;;
;; Tests for the complete hook protocol between scheduler and extensions:
;; tool-call-pre (block/amend/fallthrough), tool-result-post (block/amend/fallthrough),
;; file mutation queue serialization, and preflight exception handling.

(require rackunit
         rackunit/text-ui
         "../util/message/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../util/worker-tool-policy.rkt"
         "../util/event/event-bus.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-registry
                  register-tool!
                  lookup-tool
                  make-success-result
                  make-error-result
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?
                  tool-call?
                  tool-call-name
                  tool-call-arguments
                  make-tool-call
                  make-exec-context)
         (only-in "../tools/permission-gate.rkt"
                  make-default-permission-config
                  make-permissive-permission-config)
         "../tools/scheduler.rkt"
         ;; R94 (BUG-0094): real save-conclusion tool through hook admission
         (only-in "../tools/registry-table.rkt" register-tools-from-specs!)
         (only-in "../tools/registry-table/skill-tools.rkt" skill-tool-specs)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion-category
                  task-conclusion-relevance-tags)
         ;; W2 review: safe-mode re-assertion on post-hook arguments
         (only-in "../util/safe-mode/safe-mode-state.rkt"
                  current-safe-mode-config
                  make-safe-mode-config))

;; ============================================================
;; Helpers
;; ============================================================

;; Extract text from error result content
(define (error-text tr)
  (define c (tool-result-content tr))
  (if (list? c)
      (let ([first-entry (and (pair? c) (car c))])
        (if (hash? first-entry)
            (hash-ref first-entry 'text "")
            (format "~a" c)))
      (format "~a" c)))

;; Create a simple tool that echoes its args
(define (make-echo-tool #:name [name "echo"])
  (make-tool name
             "Echo tool"
             (hasheq 'type "object" 'properties (hasheq) 'required (list))
             (lambda (args ctx) (make-success-result (format "echo: ~a" args)))))

;; Create a tool that records execution in a box
(define (make-recording-tool box
                             #:name [name "rec"]
                             #:schema [schema (hasheq)]
                             #:timeout-seconds [timeout-seconds #f])
  (make-tool name
             "Recording tool"
             schema
             (lambda (args ctx)
               (set-box! box (cons args (unbox box)))
               (make-success-result "recorded"))
             #:timeout-seconds timeout-seconds))

;; Simple hook dispatcher builder
;; Arguments: alternating hook-point-symbol handler-proc
(define (make-hook-dispatcher . pairs)
  (define table (apply hasheq pairs))
  (lambda (hook-point payload)
    (define handler (hash-ref table hook-point #f))
    (if handler
        (handler payload)
        #f)))

;; v0.99.66: custom test tools are "unknown" to the classifier, so they
;; require a permissive permission config to run without approval.
(define (make-test-exec-context)
  (make-exec-context #:permission-config (make-permissive-permission-config)))

;; R94 (BUG-0094): registry holding only the real save-conclusion tool.
(define (make-save-conclusion-reg)
  (define reg (make-tool-registry))
  (register-tools-from-specs! reg skill-tool-specs #:only '("save-conclusion"))
  reg)

;; R94: exec-context that captures scheduler/tool events in a box.
(define (make-event-capturing-ctx events)
  (make-exec-context #:permission-config (make-permissive-permission-config)
                     #:event-publisher
                     (lambda (event-type payload)
                       (set-box! events (cons (cons event-type payload) (unbox events))))))

(define (deprecated-events events)
  (filter (lambda (e) (equal? (car e) "tool.deprecated")) events))

;; R94: error results carry an EMPTY details hash, so "no stored conclusion"
;; means the '_conclusion key is absent — not that details is #f.
(define (stored-conclusion result)
  (define d (tool-result-details result))
  (and (hash? d) (hash-ref d '_conclusion #f)))

;; ============================================================
;; Tests
;; ============================================================

(define scheduler-hook-tests
  (test-suite "Scheduler Hook Protocol Tests"

    (test-case "legacy identity echo is a no-op in serial and parallel execution"
      (for ([parallel? '(#f #t)])
        (define exec-log (box '()))
        (define reg (make-tool-registry))
        (register-tool! reg
                        (make-recording-tool exec-log
                                             #:schema (hasheq 'type "object" 'properties (hasheq))))
        (define result
          (run-tool-batch (list (make-tool-call "echo-input" "rec" (hasheq 'original #t)))
                          reg
                          #:parallel? parallel?
                          #:hook-dispatcher (lambda (_point payload) payload)
                          #:exec-context (make-test-exec-context)))
        (check-false (tool-result-is-error? (car (scheduler-result-results result))))
        (check-equal? (length (unbox exec-log)) 1)
        (check-true (hash-ref (car (unbox exec-log)) 'original))))

    (test-case "legacy echo cannot bypass final worker name or amended scope policy"
      (for ([parallel? '(#f #t)])
        (define exec-log (box '()))
        (define reg (make-tool-registry))
        (for ([name '("rec" "hidden")])
          (register-tool! reg
                          (make-recording-tool exec-log
                                               #:name name
                                               #:schema
                                               (hasheq 'type "object" 'properties (hasheq)))))
        (define policy
          (worker-tool-policy
           values
           (lambda (name args)
             (cond
               [(not (equal? name "rec")) "TOOL_NOT_ALLOWED"]
               [(not (equal? (hash-ref args 'path #f) "allowed")) "PATH_OUT_OF_SCOPE"]
               [else #f]))))
        (define (run name [amend-point #f])
          (parameterize ([current-worker-tool-policy policy])
            (car (scheduler-result-results
                  (run-tool-batch (list (make-tool-call "policy-echo" name (hasheq 'path "allowed")))
                                  reg
                                  #:parallel? parallel?
                                  #:hook-dispatcher
                                  (lambda (point payload)
                                    (check-eq? (current-worker-tool-policy)
                                               policy
                                               "worker policy remains scoped through scheduler hooks")
                                    (cond
                                      [(and (eq? point 'tool-call) (eq? amend-point 'tool-call))
                                       (make-tool-call "policy-echo" name (hasheq 'path "forbidden"))]
                                      [(and (eq? point 'tool-call-pre)
                                            (eq? amend-point 'tool-call-pre))
                                       (hook-amend (hasheq 'args (hasheq 'path "forbidden")))]
                                      [else payload]))
                                  #:exec-context (make-test-exec-context))))))
        (check-false (tool-result-is-error? (run "rec")))
        (define hidden (run "hidden"))
        (check-true (tool-result-is-error? hidden))
        (check-regexp-match #rx"TOOL_NOT_ALLOWED" (error-text hidden))
        (for ([point '(tool-call tool-call-pre)])
          (define amended (run "rec" point))
          (check-true (tool-result-is-error? amended))
          (check-regexp-match #rx"PATH_OUT_OF_SCOPE" (error-text amended)))
        (check-equal? (length (unbox exec-log)) 1)
        (check-equal? (hash-ref (car (unbox exec-log)) 'path) "allowed")))

    (test-case "nonidentity hashes and malformed pre-hook results fail closed"
      (for* ([parallel? '(#f #t)]
             [produce (in-list (list hash-copy
                                     (lambda (payload)
                                       (hash-set payload 'tool-arguments (hasheq 'changed #t)))
                                     (lambda (_) (hasheq))
                                     (lambda (_) "not a hook result")
                                     (lambda (_) 42)
                                     (lambda (_) (hook-result 'unknown #f))
                                     (lambda (_) (hook-amend (hasheq 'args 42)))))])
        (define exec-log (box '()))
        (define reg (make-tool-registry))
        (register-tool! reg
                        (make-recording-tool exec-log
                                             #:schema (hasheq 'type "object" 'properties (hasheq))))
        (define result
          (run-tool-batch (list (make-tool-call "malformed" "rec" (hasheq)))
                          reg
                          #:parallel? parallel?
                          #:hook-dispatcher (make-hook-dispatcher 'tool-call-pre produce)
                          #:exec-context (make-test-exec-context)))
        (define tr (car (scheduler-result-results result)))
        (check-true (tool-result-is-error? tr))
        (check-regexp-match #rx"malformed tool-call-pre|invalid tool-call-pre amendment"
                            (error-text tr))
        (check-equal? (unbox exec-log) '())))

    ;; ============================================================
    ;; TS1: tool-call-pre hook blocks execution
    ;; ============================================================
    (test-case "TS1: tool-call-pre hook blocks execution"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define tc (make-tool-call "tc-1" "echo" (hasheq 'msg "hello")))
      (define dispatcher
        (make-hook-dispatcher 'tool-call-pre (lambda (payload) (hook-block "blocked by policy"))))
      (define result
        (run-tool-batch (list tc)
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-equal? (length results) 1)
      (check-true (tool-result-is-error? (car results)) "blocked pre hook produces error result")
      (check-not-false (regexp-match? #rx"blocked by policy" (error-text (car results)))
                       "error message preserves policy reason"))

    ;; ============================================================
    ;; TS2: tool-call-pre hook amends arguments
    ;; ============================================================
    (test-case "TS2: tool-call-pre hook amends arguments"
      (define exec-log (box '()))
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-recording-tool
                       exec-log
                       #:schema (hasheq 'type "object" 'properties (hasheq) 'required (list))))
      (define tc (make-tool-call "tc-2" "rec" (hasheq 'original #t)))
      (define dispatcher
        (make-hook-dispatcher 'tool-call-pre
                              (lambda (payload) (hook-amend (hasheq 'args (hasheq 'amended #t))))))
      (define result
        (run-tool-batch (list tc)
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-equal? (length results) 1)
      (check-false (tool-result-is-error? (car results)) "amended args should succeed")
      ;; Check that tool received amended args (scheduler injects working-directory)
      (check-equal? (hash-remove (car (unbox exec-log)) 'working-directory)
                    (hasheq 'amended #t)
                    "tool received amended arguments"))

    ;; ============================================================
    ;; TS3: Invalid tool-call-pre amendments fail closed
    ;; ============================================================
    (test-case "TS3: tool-call-pre hook amend with non-hash payload is rejected"
      (define exec-log (box '()))
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-recording-tool
                       exec-log
                       #:schema (hasheq 'type "object" 'properties (hasheq) 'required (list))))
      (define tc (make-tool-call "tc-3" "rec" (hasheq 'original #t)))
      (define dispatcher
        (make-hook-dispatcher 'tool-call-pre
                              (lambda (payload)
                                ;; Return amend with non-hash payload (a string)
                                (hook-amend "not a hash"))))
      (define result
        (run-tool-batch (list tc)
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-true (tool-result-is-error? (car results)) "malformed amend must fail closed")
      (check-equal? (unbox exec-log) '() "malformed amend must not execute the tool"))

    (test-case "TS3b: schema-invalid amended args are neither approved nor executed"
      (define approval-count (box 0))
      (define exec-log (box '()))
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-recording-tool exec-log
                                           #:name "validated-rec"
                                           #:schema
                                           (hasheq 'type
                                                   "object"
                                                   'required
                                                   '("message")
                                                   'properties
                                                   (hasheq 'message (hasheq 'type "string")))))
      (define cfg
        (make-default-permission-config #:callback
                                        (lambda (name args)
                                          (set-box! approval-count (add1 (unbox approval-count)))
                                          #t)))
      (define dispatcher
        (make-hook-dispatcher 'tool-call-pre
                              (lambda (payload) (hook-amend (hasheq 'args (hasheq 'message 42))))))
      (define result
        (run-tool-batch (list (make-tool-call "tc-3b" "validated-rec" (hasheq 'message "valid")))
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-exec-context #:permission-config cfg)))
      (define tool-result (car (scheduler-result-results result)))
      (check-true (tool-result-is-error? tool-result))
      (check-not-false (regexp-match? #rx"validate-tool-args" (error-text tool-result)))
      (check-equal? (unbox approval-count) 0 "invalid amendments must not request approval")
      (check-equal? (unbox exec-log) '() "invalid amendments must not execute"))

    (test-case "TS3c: approval and execution share one deeply immutable final invocation"
      (define source-label (string-copy "before"))
      (define source-nested (make-hasheq (list (cons 'labels (vector source-label)))))
      (define source-args (make-hasheq (list (cons 'payload source-nested))))
      (define approved-args (box #f))
      (define post-hook-args (box #f))
      (define exec-log (box '()))
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-recording-tool exec-log
                                           #:name "frozen-rec"
                                           #:timeout-seconds 17
                                           #:schema (hasheq 'type
                                                            "object"
                                                            'required
                                                            '("payload")
                                                            'properties
                                                            (hasheq 'payload
                                                                    (hasheq 'type "object")
                                                                    'timeout
                                                                    (hasheq 'type "integer")
                                                                    'working-directory
                                                                    (hasheq 'type "string")))))
      (define cfg
        (make-default-permission-config
         #:callback (lambda (name args)
                      (set-box! approved-args args)
                      ;; Mutating hook-owned data after approval must not alter execution.
                      (hash-set! source-args 'late-mutation #t)
                      (string-set! source-label 0 #\X)
                      #t)))
      (define dispatcher
        (make-hook-dispatcher 'tool-call-pre
                              (lambda (payload) (hook-amend (hasheq 'args source-args)))
                              'tool-result-post
                              (lambda (payload)
                                (set-box! post-hook-args (tool-post-hook-payload-arguments payload))
                                #f)))
      (define result
        (run-tool-batch (list (make-tool-call "tc-3c" "frozen-rec" (hasheq 'payload (hasheq))))
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-exec-context #:working-directory "/tmp/r1-final"
                                                          #:permission-config cfg)))
      (define executed-args (car (unbox exec-log)))
      (check-false (tool-result-is-error? (car (scheduler-result-results result))))
      (check-true (eq? (unbox approved-args) executed-args)
                  "approval and execution must receive the exact same object")
      (check-true (eq? executed-args (unbox post-hook-args))
                  "post hooks must observe the committed invocation object")
      (check-true (immutable? executed-args))
      (check-true (immutable? (hash-ref executed-args 'payload)))
      (check-true (immutable? (hash-ref (hash-ref executed-args 'payload) 'labels)))
      (check-true (immutable? (vector-ref (hash-ref (hash-ref executed-args 'payload) 'labels) 0)))
      (check-equal? (hash-ref executed-args 'timeout) 17)
      (check-equal? (hash-ref executed-args 'working-directory) "/tmp/r1-final")
      (check-equal? (vector-ref (hash-ref (hash-ref executed-args 'payload) 'labels) 0) "before")
      (check-false (hash-has-key? executed-args 'late-mutation)))

    ;; ============================================================
    ;; TS4: tool-result-post hook blocks result
    ;; ============================================================
    (test-case "TS4: tool-result-post hook blocks result"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define tc (make-tool-call "tc-4" "echo" (hasheq 'msg "test")))
      (define dispatcher
        (make-hook-dispatcher 'tool-result-post (lambda (payload) (hook-block "result blocked"))))
      (define result
        (run-tool-batch (list tc)
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-true (tool-result-is-error? (car results))
                  "blocked result-post hook produces error result")
      (check-not-false (regexp-match? #rx"blocked" (error-text (car results)))
                       "error message mentions result blocking"))

    ;; ============================================================
    ;; TS5: tool-result-post hook amends result
    ;; ============================================================
    (test-case "TS5: tool-result-post hook amends result"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define tc (make-tool-call "tc-5" "echo" (hasheq 'msg "test")))
      (define amended-result (make-success-result "amended content"))
      (define dispatcher
        (make-hook-dispatcher 'tool-result-post
                              (lambda (payload) (hook-amend (hasheq 'result amended-result)))))
      (define result
        (run-tool-batch (list tc)
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-false (tool-result-is-error? (car results)) "amended result should not be error")
      (check-equal? (tool-result-content (car results))
                    "amended content"
                    "result was amended by hook"))

    ;; ============================================================
    ;; TS6: tool-result-post hook amend with invalid result falls back
    ;; ============================================================
    (test-case "TS6: tool-result-post hook amend with invalid result falls back"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define tc (make-tool-call "tc-6" "echo" (hasheq 'msg "original")))
      (define dispatcher
        (make-hook-dispatcher 'tool-result-post
                              (lambda (payload)
                                ;; Return amend with non-tool-result value
                                (hook-amend (hasheq 'result "not a tool-result")))))
      (define result
        (run-tool-batch (list tc)
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-false (tool-result-is-error? (car results))
                   "invalid amend falls back to original result")
      ;; Original result should be used (echo tool returns formatted args)
      (check-not-false (regexp-match? #rx"echo" (tool-result-content (car results)))
                       "original result preserved when amend is invalid"))

    ;; ============================================================
    ;; TS7: File mutation queue with parallel writes to same file
    ;; ============================================================
    (test-case "TS7: file mutation queue serializes parallel writes to same file"
      (define write-count (box 0))
      (define active-writers (box 0))
      (define max-concurrent (box 0))
      (define counter-sema (make-semaphore 1))
      (define reg (make-tool-registry))
      (register-tool!
       reg
       (make-tool "write"
                  "Write file"
                  (hasheq 'type "object" 'properties (hasheq) 'required (list))
                  (lambda (args ctx)
                    ;; Track concurrency
                    (call-with-semaphore counter-sema
                                         (lambda ()
                                           (set-box! active-writers (add1 (unbox active-writers)))
                                           (when (> (unbox active-writers) (unbox max-concurrent))
                                             (set-box! max-concurrent (unbox active-writers)))))
                    (sleep 0.01) ; simulate work
                    (call-with-semaphore counter-sema
                                         (lambda ()
                                           (set-box! active-writers (sub1 (unbox active-writers)))))
                    (set-box! write-count (add1 (unbox write-count)))
                    (make-success-result "written"))
                  #:dangerous? #t
                  #:mutates-filesystem? #t))
      (define tcs
        (for/list ([i (in-range 3)])
          (make-tool-call
           (format "tc-~a" i)
           "write"
           (hasheq 'path "/tmp/test-scheduler-file.txt" 'content (format "content ~a" i)))))
      (define result (run-tool-batch tcs reg #:parallel? #t #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-equal? (length results) 3 "all three writes completed")
      ;; File mutation queue should serialize writes to same path
      (check-equal? (unbox max-concurrent)
                    1
                    "file mutation queue prevents concurrent writes to same file"))

    ;; ============================================================
    ;; R94 (BUG-0094): save-conclusion JSON wire types through hook
    ;; final admission. Amendments committed by tool-call-pre are
    ;; revalidated post-hook; normalization happens only inside the
    ;; handler, so blocked admissions leave no _conclusion and no
    ;; deprecation event.
    ;; ============================================================

    (test-case "R94: pre-hook amendment to valid JSON string args executes and normalizes"
      (define reg (make-save-conclusion-reg))
      (define events (box '()))
      (define result
        (car
         (scheduler-result-results
          (run-tool-batch
           (list (make-tool-call "r94-ok" "save-conclusion" (hasheq 'content "original")))
           reg
           #:hook-dispatcher
           (make-hook-dispatcher
            'tool-call-pre
            (lambda (_payload)
              (hook-amend
               (hasheq 'args
                       (hasheq 'content "hooked" 'category "decision" 'tags (list "r94" "hook"))))))
           #:exec-context (make-event-capturing-ctx events)))))
      (check-false (tool-result-is-error? result)
                   (format "amended JSON strings should succeed: ~a" (error-text result)))
      (define details (tool-result-details result))
      (check-not-false (and details (hash-ref details '_conclusion #f)))
      (check-eq? (task-conclusion-category (hash-ref details '_conclusion)) 'decision)
      (check-equal? (task-conclusion-relevance-tags (hash-ref details '_conclusion)) '(r94 hook))
      (check-equal? (length (deprecated-events (unbox events))) 1))

    (test-case "R94: hook amendment with invalid category type blocked at final revalidation"
      (define reg (make-save-conclusion-reg))
      (define events (box '()))
      (define result
        (car (scheduler-result-results
              (run-tool-batch
               (list (make-tool-call "r94-bad" "save-conclusion" (hasheq 'content "x")))
               reg
               #:hook-dispatcher
               (make-hook-dispatcher
                'tool-call-pre
                (lambda (_payload) (hook-amend (hasheq 'args (hasheq 'content "x" 'category 42)))))
               #:exec-context (make-event-capturing-ctx events)))))
      (check-true (tool-result-is-error? result) "number category must not execute")
      (check-regexp-match #rx"validate-tool-args" (error-text result))
      (check-false (stored-conclusion result) "no _conclusion detail after blocked admission")
      (check-equal? (deprecated-events (unbox events)) '() "no deprecation event"))

    (test-case "R94: hook amendment with invalid tag elements handler-rejected, no effect"
      (define reg (make-save-conclusion-reg))
      (define events (box '()))
      (define result
        (car (scheduler-result-results
              (run-tool-batch
               (list (make-tool-call "r94-tags" "save-conclusion" (hasheq 'content "x")))
               reg
               #:hook-dispatcher
               (make-hook-dispatcher
                'tool-call-pre
                (lambda (_payload)
                  (hook-amend (hasheq 'args (hasheq 'content "x" 'tags (list "ok" 42))))))
               #:exec-context (make-event-capturing-ctx events)))))
      (check-true (tool-result-is-error? result) "malformed tag element must not execute")
      ;; passes the array-typed wire schema, so rejection is handler-structured
      (check-false (regexp-match? #rx"validate-tool-args" (error-text result)))
      (check-regexp-match #rx"tags" (error-text result))
      (check-false (stored-conclusion result) "no _conclusion detail on rejection")
      (check-equal? (deprecated-events (unbox events)) '() "no deprecation event"))

    ;; ============================================================
    ;; W2 review: safe mode is re-asserted on post-hook arguments
    ;; ============================================================
    (test-case "TS9: safe mode cannot be escaped by a tool-call-pre path amendment"
      ;; Preflight admits the call with its ORIGINAL, in-project path. An
      ;; extension-supplied tool-call-pre hook then amends 'path to a location
      ;; outside the project root. Execution must re-run the safe-mode policy on
      ;; the FINAL arguments and refuse BEFORE approval or the tool body, so the
      ;; extension never turns an admitted invocation into an escape.
      (define root (make-temporary-file "q-hook-safe-root-~a" 'directory))
      (define outside-dir (make-temporary-file "q-hook-safe-outside-~a" 'directory))
      (define inside-path (path->string (build-path root "inside.txt")))
      (define outside-path (path->string (build-path outside-dir "outside.txt")))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-safe-mode-config (make-safe-mode-config #:active #t
                                                                         #:project-root root)])
           (define exec-log (box '()))
           (define approval-count (box 0))
           (define reg (make-tool-registry))
           (register-tool! reg
                           (make-recording-tool exec-log
                                                #:name "path-probe"
                                                #:schema
                                                (hasheq 'type
                                                        "object"
                                                        'properties
                                                        (hasheq 'path (hasheq 'type "string"))
                                                        'required
                                                        (list "path"))))
           (define ctx
             (make-exec-context #:permission-config
                                (make-default-permission-config
                                 #:callback (lambda (_name _args)
                                              (set-box! approval-count (add1 (unbox approval-count)))
                                              #t))))
           ;; Control: the unamended in-project invocation executes.
           (define control
             (run-tool-batch (list (make-tool-call "safe-ok" "path-probe" (hasheq 'path inside-path)))
                             reg
                             #:exec-context ctx))
           (check-false (tool-result-is-error? (car (scheduler-result-results control))))
           (check-equal? (length (unbox exec-log)) 1 "the admitted invocation ran")
           (check-equal? (unbox approval-count) 1)
           ;; Escape attempt: same admitted call, amended to an outside path.
           (define escaper
             (make-hook-dispatcher 'tool-call-pre
                                   (lambda (_payload)
                                     (hook-amend (hasheq 'args (hasheq 'path outside-path))))))
           (define escaped
             (run-tool-batch
              (list (make-tool-call "safe-escape" "path-probe" (hasheq 'path inside-path)))
              reg
              #:hook-dispatcher escaper
              #:exec-context ctx))
           (define escaped-result (car (scheduler-result-results escaped)))
           (check-true (tool-result-is-error? escaped-result)
                       "the amended out-of-project invocation must be refused")
           (check-regexp-match #rx"outside project root" (error-text escaped-result))
           (check-equal? (length (unbox exec-log))
                         1
                         "the extension amendment never reached the tool body")
           (check-equal? (unbox approval-count)
                         1
                         "no approval is requested for an invocation safe mode already refuses")
           (check-false (file-exists? (string->path outside-path)))))
       (lambda ()
         (delete-directory/files root #:must-exist? #f)
         (delete-directory/files outside-dir #:must-exist? #f))))

    ;; ============================================================
    ;; TS8: Preflight hook throws exception
    ;; ============================================================
    (test-case "TS8: preflight hook throws exception"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define tc (make-tool-call "tc-8" "echo" (hasheq 'msg "test")))
      (define dispatcher
        (make-hook-dispatcher 'tool-call (lambda (payload) (error "preflight boom!"))))
      (define result
        (run-tool-batch (list tc)
                        reg
                        #:hook-dispatcher dispatcher
                        #:exec-context (make-test-exec-context)))
      (define results (scheduler-result-results result))
      (check-true (tool-result-is-error? (car results)) "preflight exception produces error result")
      (check-not-false (regexp-match? #rx"hook error" (error-text (car results)))
                       "error message mentions hook error")
      (define metadata (scheduler-result-metadata result))
      (check-pred scheduler-batch-stats? metadata)
      (check-equal? (scheduler-batch-stats-errors metadata)
                    1
                    "metadata counts 1 error from preflight exception"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests scheduler-hook-tests))

(module+ main
  (run-tests scheduler-hook-tests))
