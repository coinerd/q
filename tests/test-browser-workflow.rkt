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
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:3000")))
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
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:3000")))
    ;; Mock adapter returns screenshot bytes
    (check-not-false (hash-ref result 'screenshot_data #f))))

(test-case "browser-check-local-app skips screenshot when disabled"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:3000" 'screenshot #f)))
    (check-false (hash-ref result 'screenshot_data #f))))

(test-case "browser-check-local-app uses custom timeout"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:8080" 'timeout_ms 5000)))
    (check-equal? (hash-ref result 'status) "ok")))

(test-case "browser-check-local-app with selector"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:3000" 'selector "#app")))
    (check-equal? (hash-ref result 'status) "ok")))

(test-case "browser-check-local-app detects successful load"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:3000")))
    ;; Mock adapter returns non-empty text, no console errors → loaded_successfully = #t
    (check-true (hash-ref result 'loaded_successfully))))

(test-case "browser-check-local-app returns page_errors as list"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:3000")))
    (check-true (list? (hash-ref result 'page_errors)))))

(test-case "browser-check-local-app returns load_time_ms as integer"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (define result (browser-check-local-app
                    (hasheq 'url "http://localhost:3000")))
    (check-true (exact-integer? (hash-ref result 'load_time_ms)))
    (check-true (>= (hash-ref result 'load_time_ms) 0))))

(test-case "browser-check-local-app raises without service"
  (check-exn exn:fail?
             (lambda ()
               (browser-check-local-app
                (hasheq 'url "http://localhost:3000")))))

(test-case "browser-check-local-app handles multiple allowed ports"
  (define svc (make-test-service))
  (parameterize ([current-browser-service svc])
    (for ([port '(3000 8080 5173)])
      (define result (browser-check-local-app
                      (hasheq 'url (format "http://localhost:~a" port))))
      (check-equal? (hash-ref result 'status) "ok"))))
