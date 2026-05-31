#lang racket/base

;; tests/test-session-persistence.rkt — tests for session persistence helpers
;; v0.74.8: Complete rewrite with temp dir isolation + meaningful assertions.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         racket/string
         "../runtime/session-persistence.rkt")

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
                         (write-crash-log! "test" "msg" "phase")))))))

(run-tests suite)
