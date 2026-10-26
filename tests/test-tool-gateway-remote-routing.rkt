#lang racket

;; @speed fast  ;; @suite security
;; @boundary unit

;; tests/test-tool-gateway-remote-routing.rkt — W3 (v0.99.12) Remote Routing Tests
;;
;; Tests the risk-based routing integration in tool-gateway.rkt:
;; - High/critical risk routes to 'remote when policy is 'risk-based
;; - Remote executor function is called for 'remote decisions
;; R97 (BUG-0097) additions: find/read worker affinity — absolute paths
;; returned by find executed on worker A are only promised readable in
;; worker A's namespace. The gateway passes paths VERBATIM (no automatic
;; cross-host remapping, no model-cwd trust); a follow-up read routed to
;; the SAME worker succeeds, and the same path on a different worker is
;; denied instead of silently resolving a same-named foreign file.
;; - Default remote executor returns fail-closed error
;; - Local execution unchanged when routing to 'local
;; - Routing metadata correctly reflects execution location

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         "../agent/roles/tool-gateway.rkt"
         (only-in "../agent/roles/base.rkt" agent-role-handle-envelope)
         (only-in "../util/message/mas-envelope.rkt"
                  make-mas-envelope
                  mas-envelope?
                  mas-envelope-payload
                  mas-envelope-risk-level)
         (only-in "../tools/tool.rkt"
                  make-error-result
                  tool-result-content
                  tool-result-is-error?)
         (only-in "../tools/builtins/find.rkt" tool-find)
         (only-in "../tools/builtins/read.rkt" tool-read)
         "helpers/fixtures.rkt")

;; ── Helpers ──

(define (make-test-envelope risk-level)
  (make-mas-envelope 'supervisor
                     'tool-gateway
                     'shell-exec
                     (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo test"))
                     #:risk-level risk-level))

(define (make-stub-remote-executor [response-status 'ok])
  (lambda (envelope)
    (hasheq 'status response-status 'role 'tool-gateway 'result "executed-remotely" 'trace-id #f)))

;; ── Tests ──


;; ── R97 (BUG-0097) worker-affinity helpers ──────────────────
;; A worker stub executes the REAL find/read handlers inside its own
;; filesystem namespace (fixture root). It performs NO path translation:
;; envelope arguments pass through verbatim, pinning that the gateway never
;; remaps paths across namespaces. Worker isolation is simulated by denying
;; reads of paths outside the worker's own root (a real remote worker on a
;; different host would not see foreign paths in its filesystem at all).

(define (make-r97-worker-stub worker-name ns-root seen-args-box)
  (lambda (envelope)
    (define payload (mas-envelope-payload envelope))
    (define tool-name (hash-ref payload 'tool-name))
    (define args (hash-ref payload 'arguments))
    (set-box! seen-args-box args)
    (define result
      (case tool-name
        [("find") (tool-find args)]
        [("read")
         (define p (hash-ref args 'path #f))
         (if (and (string? p)
                  (absolute-path? p)
                  (string-prefix? p (path->string ns-root)))
             (tool-read args)
             (make-error-result
              (format "read: path not readable on worker ~a (different filesystem namespace): ~a"
                      worker-name p)))]

        [else
         (make-error-result
          (format "tool ~a not available on worker ~a" tool-name worker-name))]))
    (hasheq 'status 'ok 'worker worker-name 'result result)))

(define (r97-result-strings r)
  (for/list ([part (in-list (tool-result-content r))])
    (cond
      [(string? part) part]
      [(hash? part) (hash-ref part 'text "")]
      [else (~a part)])))

(define (r97-result-text r)
  (string-join (r97-result-strings r) ""))


(define suite
  (test-suite "Tool Gateway Remote Routing (W3 v0.99.12)"

    ;; ════════════════════════════════════════════════════════════
    ;; routing-decision->execution-route now returns 'remote (not tagged)
    ;; ════════════════════════════════════════════════════════════

    (test-case "routing-decision->execution-route returns 'remote for remote"
      (check-equal? (routing-decision->execution-route 'remote) 'remote))

    (test-case "routing-decision->execution-route returns 'local for local"
      (check-equal? (routing-decision->execution-route 'local) 'local))

    ;; ════════════════════════════════════════════════════════════
    ;; Default remote executor is fail-closed
    ;; ════════════════════════════════════════════════════════════

    (test-case "default-remote-tool-executor returns fail-closed error"
      (define env (make-test-envelope 'high))
      (define result (default-remote-tool-executor env))
      (check-equal? (hash-ref result 'status) 'error)
      (check-true (string-contains? (hash-ref result 'error-message "")
                                    "remote executor not configured")))

    ;; ════════════════════════════════════════════════════════════
    ;; Risk-based routing dispatches to remote executor for high/critical
    ;; ════════════════════════════════════════════════════════════

    (test-case "risk-based routing dispatches high risk to remote executor"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'high))
      (define remote-called? (box #f))
      (parameterize ([current-routing-policy 'risk-based]
                     [current-remote-tool-executor (lambda (envelope)
                                                     (set-box! remote-called? #t)
                                                     (hasheq 'status 'ok 'result "remote-result"))])
        (define result (agent-role-handle-envelope role env))
        (check-true (unbox remote-called?) "remote executor was called")
        (check-equal? (hash-ref result 'routing-decision) 'remote)
        (check-equal? (hash-ref result 'route) 'remote)
        (check-equal? (hash-ref result 'executed-locally?) #f)
        (check-equal? (hash-ref result 'result) "remote-result")))

    (test-case "risk-based routing dispatches critical risk to remote executor"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      (parameterize ([current-routing-policy 'risk-based]
                     [current-remote-tool-executor (make-stub-remote-executor)])
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'routing-decision) 'remote)
        (check-equal? (hash-ref result 'route) 'remote)))

    ;; ════════════════════════════════════════════════════════════
    ;; Risk-based routing still routes low/medium to local
    ;; ════════════════════════════════════════════════════════════

    (test-case "risk-based routing dispatches low risk to local executor"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'low))
      (define remote-called? (box #f))
      (parameterize
          ([current-routing-policy 'risk-based]
           [current-tool-executor (lambda (envelope) (hasheq 'status 'ok 'result "local-result"))]
           [current-remote-tool-executor (lambda (envelope)
                                           (set-box! remote-called? #t)
                                           (hasheq 'status 'ok 'result "should-not-happen"))])
        (define result (agent-role-handle-envelope role env))
        (check-false (unbox remote-called?) "remote executor should NOT be called for low risk")
        (check-equal? (hash-ref result 'routing-decision) 'local)
        (check-equal? (hash-ref result 'route) 'local)
        (check-equal? (hash-ref result 'executed-locally?) #t)))

    ;; ════════════════════════════════════════════════════════════
    ;; Local-only policy never dispatches to remote
    ;; ════════════════════════════════════════════════════════════

    (test-case "local-only policy never dispatches to remote even for critical risk"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      (define remote-called? (box #f))
      (parameterize ([current-routing-policy 'local-only]
                     [current-remote-tool-executor (lambda (envelope)
                                                     (set-box! remote-called? #t)
                                                     (hasheq 'status 'ok))])
        (define result (agent-role-handle-envelope role env))
        (check-false (unbox remote-called?) "remote executor should NOT be called in local-only")
        (check-equal? (hash-ref result 'routing-decision) 'local)
        (check-equal? (hash-ref result 'route) 'local)))

    ;; ════════════════════════════════════════════════════════════
    ;; Default state: no remote executor configured → fail-closed
    ;; ════════════════════════════════════════════════════════════

    (test-case "risk-based routing without configured remote executor returns error"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      ;; Default current-remote-tool-executor is default-remote-tool-executor
      (parameterize ([current-routing-policy 'risk-based])
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status) 'error)
        (check-true (string-contains? (hash-ref result 'error-message "")
                                      "remote executor not configured"))
        (check-equal? (hash-ref result 'route) 'remote)))

    ;; ════════════════════════════════════════════════════════════
    ;; Remote executor error propagates correctly
    ;; ════════════════════════════════════════════════════════════

    (test-case "remote executor error is propagated in routing metadata"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'high))
      (parameterize ([current-routing-policy 'risk-based]
                     [current-remote-tool-executor
                      (lambda (envelope)
                        (hasheq 'status 'error 'error-message "connection refused"))])
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status) 'error)
        (check-equal? (hash-ref result 'error-message) "connection refused")
        (check-equal? (hash-ref result 'routing-decision) 'remote)))

    ;; ════════════════════════════════════════════════════════════
    ;; current-remote-tool-executor parameter is thread-local
    ;; ════════════════════════════════════════════════════════════

    (test-case "parameterize scope restores default after exit"
      (check-eq? (current-remote-tool-executor) default-remote-tool-executor)
      (parameterize ([current-remote-tool-executor (lambda (e) (hasheq 'custom #t))])
        (check-false (eq? (current-remote-tool-executor) default-remote-tool-executor)))
      (check-eq? (current-remote-tool-executor) default-remote-tool-executor))
    ;; ════════════════════════════════════════════════════════════
    ;; R97 (BUG-0097): find/read worker affinity over remote routing
    ;; ════════════════════════════════════════════════════════════

    (test-case "R97: find on worker A returns absolute paths; read affinity on same worker"
      (with-temp-dir
       (lambda (worker-a-root)
         (make-directory* (build-path worker-a-root "docs" "contracts"))
         (call-with-output-file (build-path worker-a-root "docs" "contracts" "spec.txt")
                                (lambda (out) (display "worker-a-sentinel" out))
                                #:exists 'replace)
         (define seen-args (box #f))
         (define worker-a (make-r97-worker-stub 'A worker-a-root seen-args))
         (define role (make-tool-gateway-role))
         (define find-env
           (make-mas-envelope 'supervisor
                              'tool-gateway
                              'shell-exec
                              (hasheq 'tool-name "find"
                                      'arguments (hasheq 'path (path->string worker-a-root)))
                              #:risk-level 'high))
         (parameterize ([current-routing-policy 'risk-based]
                        [current-remote-tool-executor worker-a])
           (define find-res (agent-role-handle-envelope role find-env))
           ;; execution was routed to the worker, not the coordinator
           (check-equal? (hash-ref find-res 'route) 'remote)
           (check-false (hash-ref find-res 'executed-locally?))
           (check-equal? (hash-ref find-res 'worker) 'A)
           (define find-result (hash-ref find-res 'result))
           (check-false (tool-result-is-error? find-result)
                        (r97-result-text find-result))
           (define found
             (findf (lambda (s) (string-suffix? s "docs/contracts/spec.txt"))
                    (r97-result-strings find-result)))
           ;; R97: worker find results are ABSOLUTE paths in the worker namespace
           (check-not-false found (format "expected absolute result, got: ~a"
                                          (r97-result-text find-result)))
           (when found
             (check-true (absolute-path? found))
             (check-true (string-prefix? found (path->string worker-a-root)))
             ;; follow-up read of that path routed to the SAME worker succeeds
             (define read-env
               (make-mas-envelope 'supervisor
                                  'tool-gateway
                                  'shell-exec
                                  (hasheq 'tool-name "read" 'arguments (hasheq 'path found))
                                  #:risk-level 'high))
             (define read-res (agent-role-handle-envelope role read-env))
             (check-equal? (hash-ref read-res 'worker) 'A)
             (define read-result (hash-ref read-res 'result))
             (check-false (tool-result-is-error? read-result)
                          (r97-result-text read-result))
             (check-true (string-contains? (r97-result-text read-result)
                                           "worker-a-sentinel"))
             ;; the gateway delivered the read args VERBATIM — no remapping
             (check-equal? (hash-ref (unbox seen-args) 'path) found)))))

    (test-case "R97: worker-A find path is not readable on worker B (no remapping)"
      (with-temp-dir
       (lambda (worker-a-root)
         (with-temp-dir
          (lambda (worker-b-root)
            (make-directory* (build-path worker-a-root "docs" "contracts"))
            (call-with-output-file (build-path worker-a-root "docs" "contracts" "spec.txt")
                                   (lambda (out) (display "worker-a-sentinel" out))
                                   #:exists 'replace)
            ;; worker B has a DIFFERENT file at the same relative location
            (make-directory* (build-path worker-b-root "docs" "contracts"))
            (call-with-output-file (build-path worker-b-root "docs" "contracts" "spec.txt")
                                   (lambda (out) (display "worker-b-decoy-sentinel" out))
                                   #:exists 'replace)
            (define seen-a (box #f))
            (define seen-b (box #f))
            (define worker-a (make-r97-worker-stub 'A worker-a-root seen-a))
            (define worker-b (make-r97-worker-stub 'B worker-b-root seen-b))
            (define role (make-tool-gateway-role))
(parameterize ([current-routing-policy 'risk-based]
               [current-remote-tool-executor worker-a])
  (define find-res
    (agent-role-handle-envelope
     role
     (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec
                        (hasheq 'tool-name "find"
                                'arguments (hasheq 'path (path->string worker-a-root)))
                        #:risk-level 'high)))
  (define found
    (findf (lambda (s) (string-suffix? s "docs/contracts/spec.txt"))
           (r97-result-strings (hash-ref find-res 'result))))
  (check-not-false found)
  ;; now route the SAME absolute path to worker B
  (define read-res
    (parameterize ([current-routing-policy 'risk-based]
                   [current-remote-tool-executor worker-b])
      (agent-role-handle-envelope
       role
       (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec
                          (hasheq 'tool-name "read" 'arguments (hasheq 'path found))
                          #:risk-level 'high))))
  (check-equal? (hash-ref read-res 'worker) 'B)
  (define read-result (hash-ref read-res 'result))
  ;; B denies the foreign path instead of silently resolving a
  ;; same-named file in its own namespace
  (check-true (tool-result-is-error? read-result)
              (format "worker B must deny foreign path, got: ~a"
                      (r97-result-text read-result)))
  (check-true (string-contains? (r97-result-text read-result) "worker B"))
  (check-false (string-contains? (r97-result-text read-result)
                                 "worker-b-decoy-sentinel"))
  ;; B received the path verbatim: no remapping to B's namespace
  (check-equal? (hash-ref (unbox seen-b) 'path) found)))))))

)))
(run-tests suite)
