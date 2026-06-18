#lang racket

;; @speed fast
;; @suite tools

;; tests/test-execution-plane-error-label.rkt
;; v0.99.26 W1b: Verify F-2 fix — error messages from tool execution
;; show stderr and exit code instead of generic "execution plane error".

(require rackunit
         rackunit/text-ui
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         "../sandbox/ipc-protocol.rkt")

(define suite
  (test-suite "Execution Plane Error Label Fix (v0.99.26 W1b F-2)"

    (test-case "status 'error with stderr shows command failed message"
      (define resp
        (ipc-response "req-1"
                      'error
                      #f
                      (hasheq 'exit-code 2 'stderr "find: missing argument")
                      "command exited with code 2"
                      1))
      (define result (ipc-response->tool-result resp))
      (check-true (tool-result-is-error? result))
      (define content (tool-result-content result))
      (define msg
        (if (list? content)
            (hash-ref (car content) 'text "")
            (format "~a" content)))
      (check-true (string-contains? msg "command failed") "should say 'command failed'")
      (check-true (string-contains? msg "exit 2") "should include exit code")
      (check-true (string-contains? msg "find: missing argument") "should include stderr"))

    (test-case "status 'error without details falls back to err-msg"
      (define resp (ipc-response "req-2" 'error #f #f "some error message" 1))
      (define result (ipc-response->tool-result resp))
      (check-true (tool-result-is-error? result))
      (define content (tool-result-content result))
      (define msg
        (if (list? content)
            (hash-ref (car content) 'text "")
            (format "~a" content)))
      (check-true (string-contains? msg "command failed") "should say 'command failed'")
      (check-true (string-contains? msg "some error message") "should include error message"))

    (test-case "status 'ok returns success"
      (define resp (ipc-response "req-3" 'ok "operation succeeded" #f #f 1))
      (define result (ipc-response->tool-result resp))
      (check-false (tool-result-is-error? result)))

    (test-case "status 'timeout returns timeout message"
      (define resp (ipc-response "req-4" 'timeout #f #f "request timed out" 1))
      (define result (ipc-response->tool-result resp))
      (check-true (tool-result-is-error? result))
      (define content (tool-result-content result))
      (define msg
        (if (list? content)
            (hash-ref (car content) 'text "")
            (format "~a" content)))
      (check-true (string-contains? msg "timed out") "should mention timeout"))

    (test-case "status 'error with exit-code 1"
      (define resp
        (ipc-response "req-5" 'error #f (hasheq 'exit-code 1 'stderr "permission denied") "exit 1" 1))
      (define result (ipc-response->tool-result resp))
      (check-true (tool-result-is-error? result))
      (define content (tool-result-content result))
      (define msg
        (if (list? content)
            (hash-ref (car content) 'text "")
            (format "~a" content)))
      (check-true (string-contains? msg "exit 1") "should include exit code 1")
      (check-true (string-contains? msg "permission denied") "should include stderr"))))

(run-tests suite)
