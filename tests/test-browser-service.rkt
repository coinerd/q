#lang racket

;; tests/test-browser-service.rkt — SecureBrowserService tests
;;
;; End-to-end tests using mock adapter:
;; happy path, policy blocks, expired session, max-actions, audit completeness.

(require rackunit
         racket/file
         "../browser/service.rkt"
         "../browser/settings.rkt"
         "../browser/adapters/mock.rkt"
         "../browser/types.rkt"
         "../browser/audit.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-svc #:artifact-dir [dir #f]
                       #:max-actions [max-actions 100]
                       #:max-sessions [max-sessions 3])
  (define adapter (make-mock-adapter))
  (define settings (browser-settings
                    #t '("https" "http") '() '(3000 8080 5173)
                    #t '("/admin" "/debug" "/internal")
                    max-sessions max-actions 30000 524288 #f 60000 'ephemeral))
  (make-secure-browser-service adapter #:settings settings #:artifact-dir dir))

(define (make-temp-dir)
  (define dir (build-path (find-system-path 'temp-dir)
                          (format "q-browser-svc-test-~a" (current-milliseconds))))
  (make-directory* dir)
  dir)

;; ---------------------------------------------------------------------------
;; Happy path: open → observe → act → close (5 tests)
;; ---------------------------------------------------------------------------

(test-case "happy path: open returns session-id and observation"
  (define svc (make-test-svc))
  (define-values (sid obs) (browser-open! svc "https://example.com"))
  (check-true (string? sid))
  (check-true (browser-observation? obs)))

(test-case "happy path: observe after open"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define obs (browser-observe! svc sid))
  (check-true (browser-observation? obs)))

(test-case "happy path: act after open"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define action (browser-action-click "#btn" "left"))
  (define obs (browser-act! svc sid action))
  (check-true (browser-observation? obs)))

(test-case "happy path: navigate after open"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define obs (browser-navigate! svc sid "https://example.com/page2"))
  (check-true (browser-observation? obs)))

(test-case "happy path: close after open"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (check-not-exn (lambda () (browser-close! svc sid))))

;; ---------------------------------------------------------------------------
;; Policy enforcement (5 tests)
;; ---------------------------------------------------------------------------

(test-case "policy: blocked scheme raises error"
  (define svc (make-test-svc))
  (check-exn q-browser-error?
             (lambda () (browser-open! svc "file:///etc/passwd"))))

(test-case "policy: blocked path raises error"
  (define svc (make-test-svc))
  (check-exn q-browser-error?
             (lambda () (browser-open! svc "https://example.com/admin"))))

(test-case "policy: private IP raises error"
  (define svc (make-test-svc))
  (check-exn q-browser-error?
             (lambda () (browser-open! svc "http://10.0.0.1/page"))))

(test-case "policy: navigate to blocked URL raises error"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (check-exn q-browser-error?
             (lambda () (browser-navigate! svc sid "http://192.168.1.1/secret"))))

(test-case "policy: localhost with allowed port works"
  (define svc (make-test-svc))
  (define-values (sid obs) (browser-open! svc "http://localhost:3000/app"))
  (check-true (string? sid)))

;; ---------------------------------------------------------------------------
;; Session lifecycle (4 tests)
;; ---------------------------------------------------------------------------

(test-case "session: observe on closed session raises error"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-close! svc sid)
  (check-exn q-browser-error?
             (lambda () (browser-observe! svc sid))))

(test-case "session: act on non-existent session raises error"
  (define svc (make-test-svc))
  (check-exn q-browser-error?
             (lambda () (browser-act! svc "no-such-session" (browser-action-click "#x" "left")))))

(test-case "session: max-sessions enforced"
  (define svc (make-test-svc #:max-sessions 1))
  (browser-open! svc "https://a.com")
  (check-exn q-browser-error?
             (lambda () (browser-open! svc "https://b.com"))))

(test-case "session: max-actions enforced"
  (define svc (make-test-svc #:max-actions 2))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-observe! svc sid)
  (browser-observe! svc sid)
  (check-exn q-browser-error?
             (lambda () (browser-observe! svc sid))))

;; ---------------------------------------------------------------------------
;; Error wrapping (2 tests)
;; ---------------------------------------------------------------------------

(test-case "error: adapter error wrapped as q-browser-error"
  (define adapter (make-mock-adapter #:error-mode 'timeout))
  (define settings (browser-settings
                    #t '("https") '() '() #t '() 3 100 30000 524288 #f 60000 'ephemeral))
  (define svc (make-secure-browser-service adapter #:settings settings))
  (check-exn q-browser-error?
             (lambda () (browser-open! svc "https://example.com"))))

(test-case "error: screenshot on missing session"
  (define svc (make-test-svc))
  (check-exn q-browser-error?
             (lambda () (browser-screenshot! svc "no-such"))))

;; ---------------------------------------------------------------------------
;; Audit log completeness (3 tests)
;; ---------------------------------------------------------------------------

(test-case "audit: open creates log entry"
  (define dir (make-temp-dir))
  (define svc (make-test-svc #:artifact-dir dir))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define log-file (audit-log-path dir))
  (check-true (file-exists? log-file))
  (define lines (file->lines log-file))
  (check >= (length lines) 1)
  (delete-directory/files dir #:must-exist? #t))

(test-case "audit: full lifecycle creates 4 entries"
  (define dir (make-temp-dir))
  (define svc (make-test-svc #:artifact-dir dir))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-observe! svc sid)
  (browser-act! svc sid (browser-action-click "#btn" "left"))
  (browser-close! svc sid)
  (define lines (file->lines (audit-log-path dir)))
  (check-equal? (length lines) 4)
  (delete-directory/files dir #:must-exist? #t))

(test-case "audit: log entry has correct action types"
  (define dir (make-temp-dir))
  (define svc (make-test-svc #:artifact-dir dir))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-close! svc sid)
  (define lines (file->lines (audit-log-path dir)))
  (check-true (regexp-match? #rx"open" (first lines)))
  (check-true (regexp-match? #rx"close" (second lines)))
  (delete-directory/files dir #:must-exist? #t))

;; ---------------------------------------------------------------------------
;; current-browser-service parameter (1 test)
;; ---------------------------------------------------------------------------

(test-case "current-browser-service parameter"
  (check-false (current-browser-service))
  (define svc (make-test-svc))
  (parameterize ([current-browser-service svc])
    (check-true (secure-browser-service? (current-browser-service)))))

;; ---------------------------------------------------------------------------
;; Additional coverage (6 tests)
;; ---------------------------------------------------------------------------

(test-case "screenshot: successful screenshot returns observation"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define obs (browser-screenshot! svc sid))
  (check-true (browser-observation? obs)))

(test-case "service: multiple sessions work"
  (define svc (make-test-svc #:max-sessions 3))
  (define-values (s1 _) (browser-open! svc "https://a.com"))
  (define-values (s2 _2) (browser-open! svc "https://b.com"))
  (browser-observe! svc s1)
  (browser-observe! svc s2)
  (browser-close! svc s1)
  (browser-close! svc s2))

(test-case "service: destroy frees session slot for reuse"
  (define svc (make-test-svc #:max-sessions 1))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-close! svc sid)
  (check-not-exn (lambda () (browser-open! svc "https://other.com"))))

(test-case "service: close on already-closed session raises error"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-close! svc sid)
  (check-exn q-browser-error?
             (lambda () (browser-close! svc sid))))

(test-case "service: navigate to allowed URL succeeds"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define obs (browser-navigate! svc sid "https://example.com/page2"))
  (check-true (browser-observation? obs)))

(test-case "service: observe with selector"
  (define svc (make-test-svc))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define obs (browser-observe! svc sid #:selector "#main"))
  (check-true (browser-observation? obs)))
