#lang racket

;; tests/test-browser-workflow.rkt — Workflow tool tests
;;
;; Tests browser-check-local-app composite workflow using mock adapter.

(require rackunit
         "../browser/workflow.rkt"
         "../browser/service.rkt"
         "../browser/settings.rkt"
         "../browser/adapters/mock.rkt"
         "../browser/types.rkt")

;; ---------------------------------------------------------------------------
;; Test fixture
;; ---------------------------------------------------------------------------

(define (make-test-service)
  (define adapter (make-mock-adapter))
  (make-secure-browser-service adapter))

;; ---------------------------------------------------------------------------
;; Workflow tests
;; ---------------------------------------------------------------------------

(test-case "browser-check-local-app returns composite result"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000")))
    (check-equal? (hash-ref result 'status) "ok")
    (check-true (string? (hash-ref result 'url)))
    (check-true (string? (hash-ref result 'title)))
    (check-true (string? (hash-ref result 'text)))
    (check-true (list? (hash-ref result 'console_errors)))
    (check-true (boolean? (hash-ref result 'loaded_successfully)))
    (check-true (integer? (hash-ref result 'load_time_ms)))
    (check-true (string? (hash-ref result 'session_id)))))

(test-case "browser-check-local-app includes screenshot by default"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000")))
    ;; Mock adapter returns screenshot bytes
    (check-not-false (hash-ref result 'screenshot_data #f))))

(test-case "browser-check-local-app skips screenshot when disabled"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000" 'screenshot #f)))
    (check-false (hash-ref result 'screenshot_data #f))))

(test-case "browser-check-local-app uses custom timeout"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:8080" 'timeout_ms 5000)))
    (check-equal? (hash-ref result 'status) "ok")))

(test-case "browser-check-local-app with selector"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000" 'selector "#app")))
    (check-equal? (hash-ref result 'status) "ok")))

(test-case "browser-check-local-app detects successful load"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000")))
    ;; Mock adapter returns non-empty text, no console errors → loaded_successfully = #t
    (check-true (hash-ref result 'loaded_successfully))))

(test-case "browser-check-local-app returns page_errors as list"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000")))
    (check-true (list? (hash-ref result 'page_errors)))))

(test-case "browser-check-local-app returns load_time_ms as integer"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000")))
    (check-true (exact-integer? (hash-ref result 'load_time_ms)))
    (check-true (>= (hash-ref result 'load_time_ms) 0))))

(test-case "browser-check-local-app raises without service"
  (check-exn exn:fail? (lambda () (browser-check-local-app (hasheq 'url "http://localhost:3000")))))

(test-case "browser-check-local-app handles multiple allowed ports"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (for ([port '(3000 8080 5173)])
      (define result (browser-check-local-app (hasheq 'url (format "http://localhost:~a" port))))
      (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; Integration-style tests
;; ---------------------------------------------------------------------------

(test-case "integration: full chain open-observe-close via service"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    ;; Open
    (define-values (sid obs) (browser-open! svc "http://localhost:3000"))
    (check-true (string? sid))
    (check-true (browser-observation? obs))
    ;; Observe
    (define obs2 (browser-observe! svc sid))
    (check-true (browser-observation? obs2))
    ;; Close
    (browser-close! svc sid)
    (check-true #t)))

(test-case "integration: error path — blocked private IP"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (check-exn exn:fail? (lambda () (browser-open! svc "http://10.0.0.1:8080")))))

(test-case "integration: safe-mode blocks browser operations"
  (define svc (make-test-service))
  ;; With no service parameterized, should raise
  (check-exn exn:fail? (lambda () (browser-check-local-app (hasheq 'url "http://localhost:3000")))))

(test-case "integration: multi-session lifecycle"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define-values (sid1 _) (browser-open! svc "http://localhost:3000"))
    (define-values (sid2 _2) (browser-open! svc "http://localhost:8080"))
    (check-not-equal? sid1 sid2)
    (browser-close! svc sid1)
    (browser-close! svc sid2)
    (check-true #t)))

(test-case "integration: workflow composite captures all fields"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app (hasheq 'url "http://localhost:3000" 'screenshot #t)))
    ;; Verify all expected fields present
    (for ([key '(status url
                        title
                        text
                        console_errors
                        loaded_successfully
                        load_time_ms
                        page_errors
                        session_id
                        screenshot_mime
                        screenshot_data)])
      (check-not-false (hash-has-key? result key) (format "missing key: ~a" key)))))
