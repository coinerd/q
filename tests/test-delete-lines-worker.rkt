#lang racket/base

;; tests/test-delete-lines-worker.rkt
;; v0.99.20 W2: Verify delete-lines tool externalization to worker process.
;;
;; Tests:
;; 1. delete-lines is in externalizable-tool-names
;; 2. Worker execute-delete-lines correctly removes a line range
;; 3. execute-delete-lines validates path safety
;; 4. execute-delete-lines validates line range bounds
;; 5. dispatch-tool routes delete-lines to execute-delete-lines

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         "../tools/registry-table.rkt"
         "../sandbox/ipc-protocol.rkt"
         "../sandbox/worker-tools.rkt")

(define temp-base (build-path (find-system-path 'temp-dir) "delete-lines-worker-test"))

(when (directory-exists? temp-base)
  (delete-directory/files temp-base))
(make-directory* temp-base)

(current-allowed-roots (list temp-base))

(define (make-test-file name content)
  (define path (build-path temp-base name))
  (display-to-file content path #:exists 'replace)
  path)

(define suite
  (test-suite "delete-lines Worker Externalization (v0.99.20 W2 §3.2)"

    (test-case "delete-lines in externalizable-tool-names"
      (check-not-false (member "delete-lines" externalizable-tool-names)
                       "delete-lines should be in externalizable-tool-names"))

    (test-case "execute-delete-lines removes correct line range"
      (define f (make-test-file "dl-test-1.txt" "line1\nline2\nline3\nline4\nline5"))
      (define resp
        (execute-delete-lines
         (hash 'path (path->string f) 'start-line 2 'end-line 3)))
      (check-equal? (ipc-response-status resp) 'ok)
      (define remaining (file->lines f))
      (check-equal? remaining '("line1" "line4" "line5")))

    (test-case "execute-delete-lines rejects paths outside allowed roots"
      (define resp
        (execute-delete-lines
         (hash 'path "/etc/passwd" 'start-line 1 'end-line 2)))
      (check-equal? (ipc-response-status resp) 'error))

    (test-case "execute-delete-lines validates line range"
      (define f (make-test-file "dl-test-2.txt" "only-line"))
      (define resp
        (execute-delete-lines
         (hash 'path (path->string f) 'start-line 1 'end-line 5)))
      (check-equal? (ipc-response-status resp) 'error)
      (check-true (string-contains? (or (ipc-response-error-message resp) "")
                                    "exceeds file length")))

    (test-case "dispatch-tool routes delete-lines"
      (define f (make-test-file "dl-test-3.txt" "a\nb\nc"))
      (define resp (dispatch-tool "delete-lines"
                                  (hash 'path (path->string f) 'start-line 1 'end-line 1)))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (file->lines f) '("b" "c")))))

(run-tests suite)
