#lang racket

;; tests/test-browser-tools.rkt — Browser tool handler tests

(require rackunit
         json
         "../browser/adapter.rkt"
         "../tools/builtins/browser-tools.rkt"
         "../browser/service.rkt"
         "../browser/settings.rkt"
         "../browser/adapters/mock.rkt"
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

(define (open-session)
  (handle-browser-open (hasheq 'url "https://example.com")))

;; ---------------------------------------------------------------------------
;; browser_open (3 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_open: valid URL returns session-id"
  (with-svc (lambda ()
              (define result (handle-browser-open (hasheq 'url "https://example.com")))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (string? (hash-ref result 'session-id))))))

(test-case "browser_open: blocked URL raises error"
  (with-svc (lambda ()
              (check-exn q-browser-error?
                         (lambda () (handle-browser-open (hasheq 'url "file:///etc/passwd")))))))

(test-case "browser_open: no service raises error"
  (parameterize ([current-browser-service #f])
    (check-exn q-browser-error?
               (lambda () (handle-browser-open (hasheq 'url "https://example.com"))))))

;; ---------------------------------------------------------------------------
;; browser_observe (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_observe: after open returns observation"
  (with-svc (lambda ()
              (define open-result (open-session))
              (define sid (hash-ref open-result 'session-id))
              (define result (handle-browser-observe (hasheq 'session-id sid)))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (hash-has-key? result 'observation)))))

(test-case "browser_observe: with selector"
  (with-svc (lambda ()
              (define open-result (open-session))
              (define sid (hash-ref open-result 'session-id))
              (define result (handle-browser-observe (hasheq 'session-id sid 'selector "#main")))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_click (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_click: clicks and returns observation"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result (handle-browser-click (hasheq 'session-id sid 'selector "#btn")))
              (check-equal? (hash-ref result 'status) "ok"))))

(test-case "browser_click: with explicit button"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (handle-browser-click (hasheq 'session-id sid 'selector "#btn" 'button "right")))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_type (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_type: types text into selector"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (handle-browser-type (hasheq 'session-id sid 'selector "#input" 'text "hello")))
              (check-equal? (hash-ref result 'status) "ok"))))

(test-case "browser_type: with clear_first"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (handle-browser-type
                 (hasheq 'session-id sid 'selector "#input" 'text "world" 'clear-first? #t)))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_press (1 test)
;; ---------------------------------------------------------------------------

(test-case "browser_press: sends key press"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result (handle-browser-press (hasheq 'session-id sid 'key "Enter")))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_extract (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_extract: extracts text by default"
  (with-svc
   (lambda ()
     (define sid (hash-ref (open-session) 'session-id))
     (define result
       (handle-browser-extract (hasheq 'session-id sid 'selector "#content" 'extract-type "text")))
     (check-equal? (hash-ref result 'status) "ok")
     (check-true (hash-has-key? result 'data)))))

(test-case "browser_extract: extracts html"
  (with-svc
   (lambda ()
     (define sid (hash-ref (open-session) 'session-id))
     (define result
       (handle-browser-extract (hasheq 'session-id sid 'selector "#main" 'extract-type "html")))
     (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_screenshot (1 test)
;; ---------------------------------------------------------------------------

(test-case "browser_screenshot: captures screenshot"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result (handle-browser-screenshot (hasheq 'session-id sid)))
              (check-equal? (hash-ref result 'status) "ok")
              (check-true (hash-has-key? result 'mime-type)))))

;; ---------------------------------------------------------------------------
;; browser_scroll (1 test)
;; ---------------------------------------------------------------------------

(test-case "browser_scroll: scrolls page"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result
                (handle-browser-scroll (hasheq 'session-id sid 'direction "down" 'amount 5)))
              (check-equal? (hash-ref result 'status) "ok"))))

;; ---------------------------------------------------------------------------
;; browser_close (2 tests)
;; ---------------------------------------------------------------------------

(test-case "browser_close: closes session"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (define result (handle-browser-close (hasheq 'session-id sid)))
              (check-equal? (hash-ref result 'status) "ok")
              (check-equal? (hash-ref result 'session-id) sid))))

(test-case "browser_close: closed session cannot be used"
  (with-svc (lambda ()
              (define sid (hash-ref (open-session) 'session-id))
              (handle-browser-close (hasheq 'session-id sid))
              (check-exn q-browser-error?
                         (lambda () (handle-browser-observe (hasheq 'session-id sid)))))))
