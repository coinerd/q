#lang racket

;; tests/test-browser-service-adapter.rkt — F4: service uses adapter interface

(require rackunit
         "../browser/adapter.rkt"
         "../browser/service.rkt"
         "../browser/types.rkt"
         "../browser/settings.rkt"
         "../browser/adapters/mock.rkt")

;; ---------------------------------------------------------------------------
;; Verify service dispatches through browser-adapter-* interface
;; ---------------------------------------------------------------------------

(test-case "service browser-open! dispatches via adapter interface"
  (define opened-with #f)
  (define test-adapter
    (make-browser-adapter
     #:open (lambda (session-id target) (set! opened-with (list session-id target)) 'ok)
     #:close (lambda (sid) 'ok)
     #:navigate (lambda (sid url) 'ok)
     #:observe (lambda (sid selector) (browser-observation "https://example.com" "Test" "text" "vis" "dom" #f #f #f '() '() #f (hash)))
     #:act (lambda (sid action) 'ok)
     #:screenshot (lambda (sid sel fp) 'ok)))
  (define svc (make-secure-browser-service test-adapter
               #:settings (browser-settings #t '("https") '() '(3000 8080) #t '() 3 100 30000 524288 #f 60000 'ephemeral)))
  (define-values (sid result) (browser-open! svc "https://example.com"))
  (check-equal? result 'ok)
  (check-not-false opened-with)
  (check-equal? (length opened-with) 2)
  (check-true (string? (first opened-with)))
  (check-equal? (second opened-with) "https://example.com"))

(test-case "service browser-observe! dispatches via adapter interface"
  (define observed-with #f)
  (define test-adapter
    (make-browser-adapter
     #:open (lambda (sid target) 'ok)
     #:close (lambda (sid) 'ok)
     #:navigate (lambda (sid url) 'ok)
     #:observe (lambda (sid selector) (set! observed-with (list sid selector))
                 (browser-observation "https://example.com" "Test" "text" "vis" "dom" #f #f #f '() '() #f (hash)))
     #:act (lambda (sid action) 'ok)
     #:screenshot (lambda (sid sel fp) 'ok)))
  (define svc (make-secure-browser-service test-adapter
               #:settings (browser-settings #t '("https") '() '(3000) #t '() 3 100 30000 524288 #f 60000 'ephemeral)))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define obs (browser-observe! svc sid #:selector "h1"))
  (check-not-false observed-with)
  (check-equal? (second observed-with) "h1")
  (check-true (browser-observation? obs)))

(test-case "service browser-act! dispatches via adapter interface"
  (define acted-with #f)
  (define test-adapter
    (make-browser-adapter
     #:open (lambda (sid target) 'ok)
     #:close (lambda (sid) 'ok)
     #:navigate (lambda (sid url) 'ok)
     #:observe (lambda (sid selector) (browser-observation "https://example.com" "Test" "text" "vis" "dom" #f #f #f '() '() #f (hash)))
     #:act (lambda (sid action) (set! acted-with (list sid action)) 'clicked)
     #:screenshot (lambda (sid sel fp) 'ok)))
  (define svc (make-secure-browser-service test-adapter
               #:settings (browser-settings #t '("https") '() '(3000) #t '() 3 100 30000 524288 #f 60000 'ephemeral)))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (define result (browser-act! svc sid (browser-action-click "btn" "click")))
  (check-not-false acted-with)
  (check-true (browser-action-click? (second acted-with)))
  (check-equal? result 'clicked))

(test-case "service browser-navigate! dispatches via adapter interface"
  (define navigated-with #f)
  (define test-adapter
    (make-browser-adapter
     #:open (lambda (sid target) 'ok)
     #:close (lambda (sid) 'ok)
     #:navigate (lambda (sid url) (set! navigated-with url) 'ok)
     #:observe (lambda (sid selector) (browser-observation "https://example.com" "Test" "text" "vis" "dom" #f #f #f '() '() #f (hash)))
     #:act (lambda (sid action) 'ok)
     #:screenshot (lambda (sid sel fp) 'ok)))
  (define svc (make-secure-browser-service test-adapter
               #:settings (browser-settings #t '("https") '() '(3000) #t '() 3 100 30000 524288 #f 60000 'ephemeral)))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-navigate! svc sid "https://example.com/page2")
  (check-equal? navigated-with "https://example.com/page2"))

(test-case "service browser-screenshot! dispatches via adapter interface"
  (define screenshot-args #f)
  (define test-adapter
    (make-browser-adapter
     #:open (lambda (sid target) 'ok)
     #:close (lambda (sid) 'ok)
     #:navigate (lambda (sid url) 'ok)
     #:observe (lambda (sid selector) (browser-observation "https://example.com" "Test" "text" "vis" "dom" #f #f #f '() '() #f (hash)))
     #:act (lambda (sid action) 'ok)
     #:screenshot (lambda (sid sel fp) (set! screenshot-args (list sid sel fp)) 'png-bytes)))
  (define svc (make-secure-browser-service test-adapter
               #:settings (browser-settings #t '("https") '() '(3000) #t '() 3 100 30000 524288 #f 60000 'ephemeral)))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-screenshot! svc sid #:selector "body")
  (check-not-false screenshot-args)
  (check-equal? (second screenshot-args) "body")
  (check-false (third screenshot-args)))

(test-case "service browser-close! dispatches via adapter interface"
  (define closed-sid #f)
  (define test-adapter
    (make-browser-adapter
     #:open (lambda (sid target) 'ok)
     #:close (lambda (sid) (set! closed-sid sid) 'ok)
     #:navigate (lambda (sid url) 'ok)
     #:observe (lambda (sid selector) (browser-observation "https://example.com" "Test" "text" "vis" "dom" #f #f #f '() '() #f (hash)))
     #:act (lambda (sid action) 'ok)
     #:screenshot (lambda (sid sel fp) 'ok)))
  (define svc (make-secure-browser-service test-adapter
               #:settings (browser-settings #t '("https") '() '(3000) #t '() 3 100 30000 524288 #f 60000 'ephemeral)))
  (define-values (sid _) (browser-open! svc "https://example.com"))
  (browser-close! svc sid)
  (check-equal? closed-sid sid))

(test-case "service works with mock adapter through interface"
  ;; Wrap mock functions in browser-adapter interface
  (define mock (make-mock-adapter))
  (define wrapped-adapter
    (make-browser-adapter
     #:open (lambda (sid target) (mock-open mock target #f))
     #:close (lambda (sid) (mock-close mock sid))
     #:navigate (lambda (sid url) (mock-navigate mock sid url))
     #:observe (lambda (sid selector) (mock-observe mock sid selector))
     #:act (lambda (sid action) (mock-act mock sid action))
     #:screenshot (lambda (sid sel fp) (mock-screenshot mock sid sel "png"))))
  (define svc (make-secure-browser-service wrapped-adapter
               #:settings (browser-settings #t '("https") '() '(3000) #t '() 3 100 30000 524288 #f 60000 'ephemeral)))
  (define-values (sid result) (browser-open! svc "https://example.com"))
  (check-true (string? sid))
  (check-not-false result))

(test-case "service.rkt does not import adapters/mock.rkt"
  ;; Read the file and verify no mock import
  (define content (file->string "../browser/service.rkt"))
  (check-false (string-contains? content "adapters/mock.rkt"))
  (check-not-false (string-contains? content "adapter.rkt")))
