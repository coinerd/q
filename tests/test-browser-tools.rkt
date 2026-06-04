#lang racket

;; tests/test-browser-tools.rkt — Browser tool handler tests

(require rackunit
         json
         "../browser/adapter.rkt"
         "../tools/builtins/browser-tools.rkt"
         "../browser/service.rkt"
         "../browser/settings.rkt"
         "../browser/adapters/mock.rkt"
         "../tools/tool.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-svc)
  (define mock (make-mock-adapter))
  (define adapter
    (make-browser-adapter #:open (lambda (sid target) (mock-open mock target #f))
                          #:close (lambda (sid) (mock-close mock sid))
                          #:navigate (lambda (sid url) (mock-navigate mock sid url))
                          #:observe (lambda (sid selector) (mock-observe mock sid selector))
                          #:act (lambda (sid action) (mock-act mock sid action))
                          #:screenshot (lambda (sid sel fp) (mock-screenshot mock sid sel "png"))))
  (define settings
    (browser-settings #t
                      '("https" "http")
                      '()
                      '(3000 8080)
                      #t
                      '()
                      3
                      100
                      30000
                      524288
                      #f
                      60000
                      'ephemeral))
  (make-secure-browser-service adapter #:settings settings))

(define (with-svc thunk)
  (parameterize ([current-browser-service (make-test-svc)])
    (thunk)))

;; Handlers return tool-result? structs — extract the content hash.
(define (content-of result)
  (tool-result-content result))

(define (open-session)
  (content-of (handle-browser-open (hasheq 'url "https://example.com"))))

;; ---------------------------------------------------------------------------
;; browser_open (3 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_open: valid URL returns session-id"
  (with-svc (lambda ()
              (define result (content-of (handle-browser-open (hasheq 'url "https://example.com"))))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (string? (hash-ref result 'session-id))))))

(test-case "browser_open: blocked URL raises error"
  (with-svc (lambda ()
              (check-exn q-browser-error?
                         (lambda () (handle-browser-open (hasheq 'url "file:///etc/passwd")))))))

(test-case "browser_open: no service returns graceful error"
  (parameterize ([current-browser-service #f])
    (define result (handle-browser-open (hasheq 'url "https://example.com")))
    (check-true (tool-result-is-error? result))
    (define content (tool-result-content result))
    (define text-part (car content))
    (define msg (hash-ref text-part 'text))
    (check-true (string-contains? msg "config.json"))))

;; ---------------------------------------------------------------------------
;; browser_observe (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_observe: after open returns observation"
  (with-svc (lambda ()
              (define open-result (open-session))
              (define sid (hash-ref open-result 'session-id))
              (define result (content-of (handle-browser-observe (hasheq 'session-id sid))))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (hash-has-key? result 'observation)))))

(test-case "browser_observe: with selector"
  (with-svc (lambda ()
              (define open-result (open-session))
              (define sid (hash-ref open-result 'session-id))
              (define result
                (content-of (handle-browser-observe (hasheq 'session-id sid 'selector "#main"))))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_click (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_click: clicks and returns observation"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (content-of (handle-browser-click (hasheq 'session-id sid 'selector "#btn"))))
              (check-equal? (hash-ref result 'status) "ok"))))

(test-case "browser_click: with explicit button"
  (with-svc
   (lambda ()
     (define sid (hash-ref (open-session) 'session-id))
     (define result
       (content-of (handle-browser-click (hasheq 'session-id sid 'selector "#btn" 'button "right"))))
     (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_type (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_type: types text into selector"
  (with-svc
   (lambda ()
     (define sid (hash-ref (open-session) 'session-id))
     (define result
       (content-of (handle-browser-type (hasheq 'session-id sid 'selector "#input" 'text "hello"))))
     (check-equal? (hash-ref result 'status) "ok"))))

(test-case "browser_type: with clear_first"
  (with-svc
   (lambda ()
     (define sid (hash-ref (open-session) 'session-id))
     (define result
       (content-of (handle-browser-type
                    (hasheq 'session-id sid 'selector "#input" 'text "world" 'clear-first? #t))))
     (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_press (1 test)
;; ---------------------------------------------------------------------------

(test-case "browser_press: sends key press"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (content-of (handle-browser-press (hasheq 'session-id sid 'key "Enter"))))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_extract (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_extract: extracts text by default"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (content-of (handle-browser-extract
                             (hasheq 'session-id sid 'selector "#content" 'extract-type "text"))))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (hash-has-key? result 'data)))))

(test-case "browser_extract: extracts html"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (content-of (handle-browser-extract
                             (hasheq 'session-id sid 'selector "#main" 'extract-type "html"))))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_screenshot (1 test)
;; ---------------------------------------------------------------------------

(test-case "browser_screenshot: captures screenshot"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result (content-of (handle-browser-screenshot (hasheq 'session-id sid))))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (hash-has-key? result 'mime-type)))))

;; ---------------------------------------------------------------------------
;; browser_scroll (1 test)
;; ---------------------------------------------------------------------------

(test-case "browser_scroll: scrolls page"
  (with-svc
   (lambda ()
     (define sid (hash-ref (open-session) 'session-id))
     (define result
       (content-of (handle-browser-scroll (hasheq 'session-id sid 'direction "down" 'amount 5))))
     (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_close (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_close: closes session"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result (content-of (handle-browser-close (hasheq 'session-id sid))))
              (check-equal? (hash-ref result 'status) "ok")
              (check-equal? (hash-ref result 'session-id) sid))))

(test-case "browser_close: closed session cannot be used"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (handle-browser-close (hasheq 'session-id sid))
              (check-exn q-browser-error?
                         (lambda () (handle-browser-observe (hasheq 'session-id sid)))))))

(test-case "browser_open: missing URL raises error"
  (with-svc (lambda ()
              (check-exn exn:fail? (lambda () (handle-browser-open (hasheq 'session-id "s1")))))))

(test-case "browser_observe: null session-id raises error"
  (with-svc (lambda ()
              (check-exn exn:fail? (lambda () (handle-browser-observe (hasheq 'session-id #f)))))))

(test-case "browser_check_local_app: returns status"
  (with-svc (lambda ()
              (define result (handle-browser-check-local-app (hasheq 'url "http://localhost:3000")))
              (check-true (tool-result? result)))))

(test-case "browser_screenshot: with full-page option"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (content-of (handle-browser-screenshot (hasheq 'session-id sid 'full-page #t))))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; Scheduler-compatible 2-arg dispatch tests (Bug 1 regression)
;; ---------------------------------------------------------------------------

(test-case "browser_open: 2-arg dispatch works (args + exec-ctx)"
  (with-svc (lambda ()
              ;; Scheduler calls: ((tool-execute t) args exec-ctx)
              (define result
                (content-of (handle-browser-open (hasheq 'url "https://example.com") #f)))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (string? (hash-ref result 'session-id))))))

(test-case "browser_open: 2-arg dispatch with no service returns graceful error"
  (parameterize ([current-browser-service #f])
    (define result (handle-browser-open (hasheq 'url "https://example.com") #f))
    (check-true (tool-result-is-error? result))
    (define msg (hash-ref (car (tool-result-content result)) 'text))
    (check-true (string-contains? msg "config.json"))))

(test-case "browser_close: 2-arg dispatch works"
  (with-svc (lambda ()
              (define open-result
                (content-of (handle-browser-open (hasheq 'url "https://example.com") #f)))
              (define sid (hash-ref open-result 'session-id))
              (define result (content-of (handle-browser-close (hasheq 'session-id sid) #f)))
              (check-equal? (hash-ref result 'status) "ok"))))
