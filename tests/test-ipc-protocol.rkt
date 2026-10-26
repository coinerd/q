#lang racket/base

;; tests/test-ipc-protocol.rkt — IPC protocol unit tests

;; @speed fast
;; @suite default
;; @boundary unit
(require rackunit
         rackunit/text-ui
         racket/file
         "../sandbox/ipc-protocol.rkt")

(define suite
  (test-suite "IPC Protocol"

    ;; ── Struct construction ──

    (test-case "ipc-request construction and accessors"
      (define req (make-ipc-request "req-1" "bash" (hasheq 'command "ls") 5000 #f 'shell-exec 1))
      (check-equal? (ipc-request-request-id req) "req-1")
      (check-equal? (ipc-request-tool-name req) "bash")
      (check-equal? (ipc-request-capability req) 'shell-exec)
      (check-equal? (ipc-request-timeout-ms req) 5000)
      (check-true (ipc-request? req)))

    (test-case "ipc-response construction and accessors"
      (define resp (ipc-response "req-1" 'ok "hello world" (hasheq 'exit-code 0) #f 1))
      (check-equal? (ipc-response-request-id resp) "req-1")
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "hello world")
      (check-true (ipc-response? resp)))

    ;; ── Serialization round-trip ──

    (test-case "request round-trip: request → jsexpr → request equality"
      (define req
        (make-ipc-request "test-123"
                          "write"
                          (hasheq 'path "/tmp/test" 'content "hello")
                          10000
                          "/workspace"
                          'file-write
                          1))
      (define jsexpr (ipc-request->jsexpr req))
      (define restored (jsexpr->ipc-request jsexpr))
      (check-not-false restored)
      (check-equal? (ipc-request-request-id restored) "test-123")
      (check-equal? (ipc-request-tool-name restored) "write")
      (check-equal? (ipc-request-timeout-ms restored) 10000)
      (check-equal? (ipc-request-working-dir restored) "/workspace")
      (check-equal? (ipc-request-capability restored) 'file-write)
      (check-equal? (ipc-request-schema-version restored) 1))

    (test-case "response round-trip: response → jsexpr → response equality"
      (define resp
        (ipc-response "test-456"
                      'ok
                      (hasheq 'stdout "hello\n")
                      (hasheq 'exit-code 0 'elapsed-ms 42)
                      #f
                      1))
      (define jsexpr (ipc-response->jsexpr resp))
      (define restored (jsexpr->ipc-response jsexpr))
      (check-not-false restored)
      (check-equal? (ipc-response-request-id restored) "test-456")
      (check-equal? (ipc-response-status restored) 'ok)
      (check-equal? (ipc-response-details restored) (hasheq 'exit-code 0 'elapsed-ms 42)))

    ;; ── Malformed input ──

    (test-case "jsexpr->ipc-request returns #f for malformed input"
      (check-false (jsexpr->ipc-request #f))
      (check-false (jsexpr->ipc-request "not-a-hash"))
      (check-false (jsexpr->ipc-request (hasheq))) ; missing required fields
      (check-false (jsexpr->ipc-request (hasheq 'request-id 123 'tool-name "bash")))) ; wrong type

    (test-case "jsexpr->ipc-response returns #f for malformed input"
      (check-false (jsexpr->ipc-response #f))
      (check-false (jsexpr->ipc-response "string"))
      (check-false (jsexpr->ipc-response (hasheq))) ; missing fields
      (check-false (jsexpr->ipc-response
                    (hasheq 'request-id "x" 'status 'bogus-status)))) ; invalid status

    ;; ── Missing fields default gracefully ──

    (test-case "jsexpr->ipc-request uses defaults for missing optional fields"
      (define req
        (jsexpr->ipc-request
         (hasheq 'request-id "r1" 'tool-name "bash" 'arguments (hasheq 'cmd "ls"))))
      (check-not-false req)
      (check-equal? (ipc-request-timeout-ms req) IPC-DEFAULT-TIMEOUT-MS)
      (check-false (ipc-request-working-dir req))
      (check-equal? (ipc-request-capability req) 'any)
      (check-equal? (ipc-request-schema-version req) IPC-SCHEMA-VERSION))

    (test-case "jsexpr->ipc-response defaults missing details to empty hash"
      (define resp (jsexpr->ipc-response (hasheq 'request-id "r2" 'status "ok" 'content "hello")))
      (check-not-false resp)
      (check-equal? (ipc-response-details resp) (hasheq))
      (check-false (ipc-response-error-message resp)))

    ;; ── Status validation ──

    (test-case "valid statuses accepted in jsexpr->ipc-response"
      (for ([status '("ok" "error" "timeout" "crashed")])
        (define resp (jsexpr->ipc-response (hasheq 'request-id "r" 'status status 'content "")))
        (check-not-false resp (format "status ~a should be valid" status))
        (when resp
          (check-equal? (ipc-response-status resp) (string->symbol status)))))

    (test-case "invalid status rejected in jsexpr->ipc-response"
      (check-false (jsexpr->ipc-response (hasheq 'request-id "r" 'status "invalid" 'content ""))))

    ;; ── Convenience constructors ──

    (test-case "make-error-response produces correct struct"
      (define resp (make-error-response "req-err" "something failed"))
      (check-equal? (ipc-response-status resp) 'error)
      (check-equal? (ipc-response-error-message resp) "something failed")
      (check-equal? (ipc-response-request-id resp) "req-err")
      (check-equal? (ipc-response-schema-version resp) IPC-SCHEMA-VERSION))

    (test-case "make-timeout-response produces correct struct"
      (define resp (make-timeout-response "req-to"))
      (check-equal? (ipc-response-status resp) 'timeout)
      (check-not-false (ipc-response-error-message resp)))

    (test-case "make-error-response with default message"
      (define resp (make-error-response #f))
      (check-equal? (ipc-response-status resp) 'error)
      (check-equal? (ipc-response-request-id resp) ""))

    ;; ── Schema version ──

    (test-case "schema version preserved through round-trip"
      (define req (make-ipc-request "s1" "git" (hasheq) 1000 #f 'git-write 1))
      (check-equal? (ipc-request-schema-version (jsexpr->ipc-request (ipc-request->jsexpr req)))
                    IPC-SCHEMA-VERSION)
      (define resp (ipc-response "s2" 'crashed (void) (hasheq) "boom" 1))
      (check-equal? (ipc-response-schema-version (jsexpr->ipc-response (ipc-response->jsexpr resp)))
                    IPC-SCHEMA-VERSION))

    ;; ── Size validation ──

    (test-case "ipc-request-too-large? returns #f for normal request"
      (define req (make-ipc-request "small" "bash" (hasheq 'command "echo hi") 1000 #f 'shell-exec 1))
      (check-false (ipc-request-too-large? req)))

    (test-case "ipc-request-too-large? returns #t for oversized request"
      (define big-string (make-string 2000000 #\x))
      (define req (make-ipc-request "big" "bash" (hasheq 'command big-string) 1000 #f 'shell-exec 1))
      (check-true (ipc-request-too-large? req)))

    ;; ── R91/BUG-0091: trusted working directory across the wire ──

    (test-case "R91: round-trip preserves a valid trusted working directory"
      (define twd (make-temporary-file "r91-twd-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define req
           (make-ipc-request "twd-1"
                             "write"
                             (hasheq 'path "/tmp/test" 'content "hello")
                             10000
                             "/workspace"
                             'file-write
                             1
                             (path->string twd)))
         (define jsexpr (ipc-request->jsexpr req))
         (check-equal? (hash-ref jsexpr 'trusted-working-dir #f) (path->string twd))
         (define restored (jsexpr->ipc-request jsexpr))
         (check-not-false restored)
         (check-equal? (ipc-request-trusted-working-dir restored) (path->string twd))
         (check-equal? (ipc-request-working-dir restored) "/workspace"))
       (lambda () (delete-directory/files twd #:must-exist? #f))))

    (test-case "R91: legacy absent-field requests omit trust and decode #f"
      (define req (make-ipc-request "legacy-1" "bash" (hasheq 'command "ls") 5000 #f 'shell-exec 1))
      (define jsexpr (ipc-request->jsexpr req))
      (check-false (hash-has-key? jsexpr 'trusted-working-dir))
      (define restored (jsexpr->ipc-request jsexpr))
      (check-not-false restored)
      (check-false (ipc-request-trusted-working-dir restored)))

    (test-case "R91: producer normalizes path objects to JSON strings"
      (define twd (make-temporary-file "r91-twd-path-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         ;; JSON cannot encode path objects; the producer seam must normalize.
         (define req
           (make-ipc-request "norm-1"
                             "bash"
                             (hasheq 'command "pwd")
                             5000
                             twd
                             'shell-exec
                             1
                             twd))
         (check-true (string? (ipc-request-trusted-working-dir req)))
         (check-true (string? (ipc-request-working-dir req)))
         (define jsexpr (ipc-request->jsexpr req))
         (check-true (string? (hash-ref jsexpr 'trusted-working-dir #f)))
         (check-true (string? (hash-ref jsexpr 'working-dir #f)))
         (define restored (jsexpr->ipc-request jsexpr))
         (check-not-false restored)
         (check-equal? (ipc-request-trusted-working-dir restored) (path->string twd)))
       (lambda () (delete-directory/files twd #:must-exist? #f))))

    (test-case "R91: decoder fails closed on invalid trusted working directory"
      (define (decode-with-twd twd)
        (jsexpr->ipc-request
         (hasheq 'request-id
                 "invalid-trust"
                 'tool-name
                 "bash"
                 'arguments
                 (hasheq 'command "echo hi")
                 'trusted-working-dir
                 twd)))
      ;; Non-string trust values.
      (check-false (decode-with-twd 5))
      (check-false (decode-with-twd (list "/tmp")))
      ;; Relative / ambiguous trust values.
      (check-false (decode-with-twd "relative/path"))
      (check-false (decode-with-twd "./here"))
      (check-false (decode-with-twd "~"))
      ;; Traversal (`..` as a path component).
      (check-false (decode-with-twd "/tmp/../etc"))
      (check-false (decode-with-twd "/tmp/allowed/../../evil"))
      ;; NUL bytes.
      (check-false (decode-with-twd (string-append "/tmp/nul" (string #\null) "x")))
      ;; Symlink spelling: an existing symlinked directory is not a valid
      ;; trusted root spelling, including with a non-existent tail behind it.
      (define link-fixture (make-temporary-file "r91-twd-link-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (make-directory (build-path link-fixture "real"))
         (make-file-or-directory-link (string->path "/etc")
                                      (build-path link-fixture "evil-link"))
         (make-file-or-directory-link (string->path "/nonexistent-r91-target")
                                      (build-path link-fixture "broken-link"))
         (check-false (decode-with-twd (path->string (build-path link-fixture "evil-link"))))
         (check-false (decode-with-twd
                       (path->string (build-path link-fixture "evil-link" "sub"))))
         (check-false (decode-with-twd (path->string (build-path link-fixture "broken-link"))))
         ;; A clean absolute spelling under the same fixture root is valid.
         (check-not-false (decode-with-twd (path->string (build-path link-fixture "real")))))
       (lambda () (delete-directory/files link-fixture #:must-exist? #f))))

    (test-case "R91: valid absolute trust decodes (frozen contract enumeration)"
      ;; The frozen contract requires string, absolute, no `up` component,
      ;; no NUL, no symlink spelling. Existence is not part of the decoder
      ;; enumeration; a clean non-existent absolute directory remains valid.
      (define restored
        (jsexpr->ipc-request
         (hasheq 'request-id
                 "valid-trust"
                 'tool-name
                 "bash"
                 'arguments
                 (hasheq)
                 'trusted-working-dir
                 "/r91-clean-nonexistent-target")))
      (check-not-false restored)
      (check-equal? (ipc-request-trusted-working-dir restored) "/r91-clean-nonexistent-target"))))

(run-tests suite)
