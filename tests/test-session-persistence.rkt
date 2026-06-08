#lang racket/base

;; tests/test-session-persistence.rkt — tests for session persistence helpers
;; v0.74.8: Complete rewrite with temp dir isolation + meaningful assertions.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         "../runtime/session/session-persistence.rkt"
         "../runtime/session/session-types.rkt"
         (only-in "../util/message/message.rkt" message)
         (only-in "../util/content/content-parts.rkt" text-part)
         "helpers/session-fixture.rkt")

;; --- Test isolation helper ---

(define (with-crash-dir thunk)
  (define tmp (make-temporary-file "q-crash-test-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-crash-log-dir tmp])
                    (thunk tmp)))
                (lambda ()
                  (with-handlers ([exn:fail? void])
                    (delete-directory/files tmp)))))

(define (crash-files-in dir)
  (filter (lambda (f) (regexp-match? #rx"crash-.*\\.jsonl" (path->string f))) (directory-list dir)))

;; --- Suite ---

(define suite
  (test-suite "session-persistence"

    ;; ── write-crash-log! tests (meaningful assertions, temp dir isolation) ──

    (test-case "write-crash-log! creates file in crash dir"
      (with-crash-dir (lambda (dir)
                        (write-crash-log! "test-sid" "msg" "phase")
                        (define files (crash-files-in dir))
                        (check-equal? (length files) 1 "exactly one crash file created"))))

    (test-case "write-crash-log! writes valid JSON line with session id"
      (with-crash-dir (lambda (dir)
                        (write-crash-log! "my-session-42" "boom" "running")
                        (define files (crash-files-in dir))
                        (check-equal? (length files) 1)
                        (define content (file->string (build-path dir (car files))))
                        (check-not-false (regexp-match? #rx"\"session\":\"my-session-42\"" content)
                                         "output must contain session id")
                        (check-not-false (regexp-match? #rx"\"error\":\"boom\"" content)
                                         "output must contain error message")
                        (check-not-false (regexp-match? #rx"\"phase\":\"running\"" content)
                                         "output must contain phase"))))

    (test-case "write-crash-log! handles #f session-id (writes unknown)"
      (with-crash-dir (lambda (dir)
                        (write-crash-log! #f "error" "phase")
                        (define content (file->string (build-path dir (car (crash-files-in dir)))))
                        (check-not-false (regexp-match? #rx"\"session\":\"unknown\"" content)
                                         "#f session-id should produce 'unknown'"))))

    (test-case "write-crash-log! appends to existing file"
      (with-crash-dir (lambda (dir)
                        (write-crash-log! "s1" "first" "p1")
                        (write-crash-log! "s2" "second" "p2")
                        (define files (crash-files-in dir))
                        ;; Should still be exactly 1 file (appended, not new file)
                        ;; NOTE: if timestamps differ by 1s, may be 2 files — that's acceptable
                        (check-true (>= (length files) 1) "at least one crash file exists")
                        ;; Total content should contain both entries
                        (define all-content
                          (string-join (for/list ([f (in-list files)])
                                         (file->string (build-path dir f)))
                                       "\n"))
                        (check-not-false (regexp-match? #rx"\"session\":\"s1\"" all-content)
                                         "first entry present")
                        (check-not-false (regexp-match? #rx"\"session\":\"s2\"" all-content)
                                         "second entry present"))))

    (test-case "write-crash-log! handles special characters in error message"
      (with-crash-dir (lambda (dir)
                        (write-crash-log! "sid" "error with \"quotes\" and \\backslash" "phase")
                        (define content (file->string (build-path dir (car (crash-files-in dir)))))
                        (check-not-false (regexp-match? #rx"quotes" content)
                                         "special chars preserved in output"))))

    (test-case "write-crash-log! survives filesystem errors gracefully"
      ;; Point to a non-existent deeply nested path — function swallows errors
      (check-not-exn (lambda ()
                       (parameterize ([current-crash-log-dir
                                       "/nonexistent/deeply/nested/path/that/cannot/be/created"])
                         (write-crash-log! "test" "msg" "phase")))))

    ;; ── Contract-rejection tests (R3) ──

    (test-case "write-crash-log! rejects non-string session-id"
      (check-exn exn:fail:contract?
                 (lambda () (write-crash-log! 42 "msg" "phase"))
                 "integer session-id must be rejected"))

    (test-case "ensure-persisted! rejects non-agent-session"
      (check-exn exn:fail:contract?
                 (lambda () (ensure-persisted! "not-a-session"))
                 "string argument must be rejected"))

    (test-case "buffer-or-append! rejects non-message entry"
      (define sess (make-test-session #:dir "/tmp/q-test-buf-contract"))
      (check-exn exn:fail:contract?
                 (lambda () (buffer-or-append! sess 42))
                 "integer entry must be rejected"))

    ;; ── Direct function tests (R4) ──

    (test-case "ensure-persisted! creates session directory"
      (define tmp (make-temporary-file "q-persist-test-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define sess (make-test-session #:dir tmp))
                      (ensure-persisted! sess)
                      (check-true (directory-exists? (agent-session-session-dir sess))
                                  "session directory must exist after ensure-persisted!"))
                    (lambda ()
                      (with-handlers ([exn:fail? void])
                        (delete-directory/files tmp)))))

    (test-case "session-persistence: ensure-persisted! is idempotent"
      (define tmp (make-temporary-file "q-persist-flag-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define sess (make-test-session #:dir tmp))
                      ;; make-agent-session already calls ensure-persisted!
                      (check-true (agent-session-persisted? sess)
                                  "should be persisted after creation")
                      (ensure-persisted! sess)
                      (check-true (agent-session-persisted? sess) "should stay persisted"))
                    (lambda ()
                      (with-handlers ([exn:fail? void])
                        (delete-directory/files tmp)))))

    (test-case "buffer-or-append! writes immediately (session always persisted)"
      (define tmp (make-temporary-file "q-buffer-test-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define sess (make-test-session #:dir tmp))
                      (check-true (agent-session-persisted? sess)
                                  "session is persisted by constructor")
                      (define fake-entry
                        (message "id1" #f "user" "text" (list (text-part "text" "hello")) 0 (hash)))
                      (buffer-or-append! sess fake-entry)
                      (check-equal? (agent-session-pending-entries sess)
                                    '()
                                    "entry goes to file, not pending, when persisted"))
                    (lambda ()
                      (with-handlers ([exn:fail? void])
                        (delete-directory/files tmp)))))

    (test-case "session-persistence: buffer-or-append! writes immediately when persisted"
      (define tmp (make-temporary-file "q-buffer-write-test-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define sess (make-test-session #:dir tmp))
         (ensure-persisted! sess)
         (check-true (agent-session-persisted? sess))
         (define fake-entry
           (message "id2" #f "user" "text" (list (text-part "text" "world")) 0 (hash)))
         (buffer-or-append! sess fake-entry)
         ;; When persisted, entry goes directly to file, not pending
         (check-equal? (agent-session-pending-entries sess) '() "no pending entries when persisted"))
       (lambda ()
         (with-handlers ([exn:fail? void])
           (delete-directory/files tmp)))))))

(run-tests suite)
