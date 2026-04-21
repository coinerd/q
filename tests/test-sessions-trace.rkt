#lang racket

;; tests/test-sessions-trace.rkt — v0.15.0 Wave 3
;;
;; Tests for the `q sessions trace` CLI subcommand.

(require rackunit
         racket/file
         json
         "../interfaces/sessions.rkt")

(define (make-temp-session-with-trace)
  (define dir (make-temporary-file "sess-trace-~a" 'directory (find-system-path 'temp-dir)))
  (define trace-path (build-path dir "trace.jsonl"))
  (call-with-output-file trace-path
                         (lambda (out)
                           (write-json (hasheq 'ts
                                               "2026-04-21T14:32:01Z"
                                               'seq
                                               1
                                               'phase
                                               "model.request.started"
                                               'sessionId
                                               "test-sess"
                                               'turnId
                                               "t1"
                                               'data
                                               (hasheq 'model "glm-5.1" 'max_tokens 32768))
                                       out)
                           (newline out)
                           (write-json (hasheq 'ts
                                               "2026-04-21T14:32:06Z"
                                               'seq
                                               2
                                               'phase
                                               "model.stream.completed"
                                               'sessionId
                                               "test-sess"
                                               'turnId
                                               "t1"
                                               'data
                                               (hasheq 'finish_reason "stop" 'usage (hasheq)))
                                       out)
                           (newline out)
                           (write-json (hasheq 'ts
                                               "2026-04-21T14:32:06Z"
                                               'seq
                                               3
                                               'phase
                                               "iteration.decision"
                                               'sessionId
                                               "test-sess"
                                               'turnId
                                               "t1"
                                               'data
                                               (hasheq 'iteration 1 'termination "completed"))
                                       out)
                           (newline out))
                         #:exists 'replace)
  dir)

;; ============================================================
;; read-trace-entries reads JSONL
;; ============================================================

(test-case "read-trace-entries parses valid trace.jsonl"
  (define dir (make-temp-session-with-trace))
  (define trace-path (build-path dir "trace.jsonl"))
  (define entries (read-trace-entries trace-path))
  (check-equal? (length entries) 3)
  (check-equal? (hash-ref (car entries) 'phase #f) "model.request.started")
  (check-equal? (hash-ref (cadr entries) 'phase #f) "model.stream.completed")
  (check-equal? (hash-ref (caddr entries) 'phase #f) "iteration.decision")
  (delete-directory/files dir))

;; ============================================================
;; display-trace-summary outputs correct counts
;; ============================================================

(test-case "display-trace-summary outputs event counts"
  (define dir (make-temp-session-with-trace))
  (define trace-path (build-path dir "trace.jsonl"))
  (define output (with-output-to-string (lambda () (display-trace-summary trace-path))))
  (check-not-false (regexp-match #rx"Events: 3" output))
  (check-not-false (regexp-match #rx"Iterations: 1" output))
  (check-not-false (regexp-match #rx"LLM requests: 1" output))
  (check-not-false (regexp-match #rx"Finish reasons: stop" output))
  (delete-directory/files dir))

(test-case "display-trace-summary handles empty trace"
  (define dir (make-temporary-file "sess-empty-~a" 'directory (find-system-path 'temp-dir)))
  (define trace-path (build-path dir "trace.jsonl"))
  (call-with-output-file trace-path void #:exists 'replace)
  (define output (with-output-to-string (lambda () (display-trace-summary trace-path))))
  (check-not-false (regexp-match #rx"Events: 0" output))
  (delete-directory/files dir))
