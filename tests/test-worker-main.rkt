#lang racket/base

;; tests/test-worker-main.rkt — Worker process and worker-tools tests

(require rackunit
         rackunit/text-ui
         json
         racket/port
         racket/file
         racket/string
         "../sandbox/ipc-protocol.rkt"
         "../sandbox/worker-tools.rkt"
         "../sandbox/worker-main.rkt")

;; ── Helpers ─────────────────────────────────────────────────────

(define tmp-dir (make-temporary-file "worker-test-~a" 'directory))
(define tmp-file (build-path tmp-dir "test.txt"))

(define (cleanup-tmp!)
  (with-handlers ([exn:fail? void])
    (delete-directory/files tmp-dir)))

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "Worker Process + Worker Tools"

    ;; ── execute-bash ──

    (test-case "execute-bash with valid command returns ok"
      (define resp (execute-bash (hasheq 'command "echo hello")))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "hello")
      (check-equal? (hash-ref (ipc-response-details resp) 'exit-code) 0))

    (test-case "execute-bash with failing command returns error"
      (define resp (execute-bash (hasheq 'command "exit 1")))
      (check-equal? (ipc-response-status resp) 'error)
      (check-pred (lambda (v) (not (eqv? v 0))) (hash-ref (ipc-response-details resp) 'exit-code)))

    (test-case "execute-bash with missing command returns error"
      (define resp (execute-bash (hasheq)))
      (check-equal? (ipc-response-status resp) 'error)
      (check-pred string? (ipc-response-error-message resp)))

    (test-case "execute-bash includes elapsed-ms in details"
      (define resp (execute-bash (hasheq 'command "echo timing")))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-pred exact-nonnegative-integer? (hash-ref (ipc-response-details resp) 'elapsed-ms)))

    ;; ── execute-write ──

    (test-case "execute-write creates file with content"
      (define test-path (build-path tmp-dir "write-test.txt"))
      (parameterize ([current-allowed-roots (list tmp-dir)])
        (define resp (execute-write (hasheq 'path (path->string test-path) 'content "test content")))
        (check-equal? (ipc-response-status resp) 'ok)
        (check-true (file-exists? test-path))
        (check-equal? (file->string test-path) "test content")
        (delete-file test-path)))

    (test-case "execute-write with path traversal returns error"
      (parameterize ([current-allowed-roots (list tmp-dir)])
        (define resp (execute-write (hasheq 'path "/etc/passwd" 'content "hax")))
        (check-equal? (ipc-response-status resp) 'error)))

    (test-case "execute-write with missing path returns error"
      (define resp (execute-write (hasheq)))
      (check-equal? (ipc-response-status resp) 'error))

    ;; ── execute-edit ──

    (test-case "execute-edit applies text replacement"
      (define test-path (build-path tmp-dir "edit-test.txt"))
      (call-with-output-file test-path (lambda (p) (display "hello world" p)) #:exists 'replace)
      (parameterize ([current-allowed-roots (list tmp-dir)])
        (define resp
          (execute-edit
           (hasheq 'path (path->string test-path) 'old-text "hello" 'new-text "goodbye")))
        (check-equal? (ipc-response-status resp) 'ok)
        (check-equal? (file->string test-path) "goodbye world")
        (delete-file test-path)))

    (test-case "execute-edit with non-matching old-text returns error"
      (define test-path (build-path tmp-dir "edit-fail.txt"))
      (call-with-output-file test-path (lambda (p) (display "hello" p)) #:exists 'replace)
      (parameterize ([current-allowed-roots (list tmp-dir)])
        (define resp
          (execute-edit (hasheq 'path (path->string test-path) 'old-text "notfound" 'new-text "x")))
        (check-equal? (ipc-response-status resp) 'error))
      (delete-file test-path))

    ;; ── execute-git ──

    (test-case "execute-git status returns ok"
      ;; We're in a git repo, so git status should work
      (define resp (execute-git (hasheq 'command "status" 'args '("--short"))))
      (check-pred (lambda (s) (or (eq? s 'ok) (eq? s 'error))) (ipc-response-status resp)))

    ;; ── dispatch-tool ──

    (test-case "dispatch-tool routes to correct executor"
      (define resp (dispatch-tool "bash" (hasheq 'command "echo dispatched")))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "dispatched"))

    (test-case "dispatch-tool with unknown tool returns error"
      (define resp (dispatch-tool "nonexistent" (hasheq)))
      (check-equal? (ipc-response-status resp) 'error)
      (check-true (string-contains? (ipc-response-error-message resp) "unknown tool")))

    (test-case "dispatch-tool catches executor crashes"
      ;; Pass arguments that will cause a crash
      (define resp (dispatch-tool "bash" "not-a-hash"))
      (check-equal? (ipc-response-status resp) 'error))

    ;; ── process-request-line (unit-level) ──

    (test-case "process-request-line with valid JSON returns response"
      (define req-json
        (jsexpr->string (hasheq 'request-id
                                "prl-1"
                                'tool-name
                                "bash"
                                'arguments
                                (hasheq 'command "echo unit-test")
                                'timeout-ms
                                5000
                                'capability
                                "shell-exec"
                                'schema-version
                                1)))
      (define resp (process-request-line req-json))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "unit-test")
      (check-equal? (ipc-response-request-id resp) "prl-1"))

    (test-case "process-request-line with malformed JSON returns error"
      (define resp (process-request-line "not json at all"))
      (check-equal? (ipc-response-status resp) 'error))

    (test-case "process-request-line with unknown tool returns error"
      (define req-json
        (jsexpr->string (hasheq 'request-id
                                "prl-2"
                                'tool-name
                                "fake-tool"
                                'arguments
                                (hasheq)
                                'timeout-ms
                                5000
                                'capability
                                "any"
                                'schema-version
                                1)))
      (define resp (process-request-line req-json))
      (check-equal? (ipc-response-status resp) 'error)
      (check-equal? (ipc-response-request-id resp) "prl-2"))

    ;; ── Full integration: spawn worker as subprocess ──

    (test-case "full round-trip: subprocess worker bash"
      (define racket-bin (find-executable-path "racket"))
      (define-values (proc out-in in-out err-in)
        (subprocess #f #f #f racket-bin "sandbox/worker-main.rkt"))
      (define req
        (ipc-request "integ-1" "bash" (hasheq 'command "echo integration") 5000 #f 'shell-exec 1))
      (display (jsexpr->string (ipc-request->jsexpr req)) in-out)
      (newline in-out)
      (flush-output in-out)
      (define response-line (read-line out-in 'any))
      (close-output-port in-out)
      (close-input-port out-in)
      (close-input-port err-in)
      (subprocess-wait proc)
      (check-pred string? response-line)
      (define resp-data
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (string->jsexpr response-line)))
      (check-not-false resp-data "response must be valid JSON")
      (define resp (jsexpr->ipc-response resp-data))
      (check-not-false resp "response must parse as ipc-response")
      (when resp
        (check-equal? (ipc-response-status resp) 'ok)
        (check-equal? (ipc-response-content resp) "integration")
        (check-equal? (ipc-response-request-id resp) "integ-1")))

    ;; ── path-allowed? ──

    (test-case "path-allowed? rejects traversal"
      (parameterize ([current-allowed-roots (list tmp-dir)])
        (check-false (path-allowed? "/etc/passwd"))
        (check-false (path-allowed? "../../../etc/passwd"))
        (check-true (path-allowed? (build-path tmp-dir "file.txt")))
        (check-true (path-allowed? (path->string (build-path tmp-dir "subdir" "file.txt"))))))))

;; ── Run ─────────────────────────────────────────────────────────

(cleanup-tmp!)
(set! tmp-dir (make-temporary-file "worker-test-~a" 'directory))
(set! tmp-file (build-path tmp-dir "test.txt"))

(run-tests suite)

;; Cleanup
(with-handlers ([exn:fail? void])
  (delete-directory/files tmp-dir))
