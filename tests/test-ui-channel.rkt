#lang racket

;; tests/test-ui-channel.rkt — FEAT-59: channel-based UI interaction

(require rackunit
         "../extensions/ui-channel.rkt")

;; ============================================================
;; Helper: run a UI call in a thread, return request + result channel
;; ============================================================

(define (run-ui-in-thread thunk)
  (define result-ch (make-channel))
  (define ui-ch (make-ui-channel))
  (thread (lambda ()
            (define r (thunk ui-ch))
            (channel-put result-ch (list 'done r))))
  (define req (channel-get ui-ch))
  (values req result-ch))

;; ============================================================
;; Struct shape tests
;; ============================================================

(test-case "ui-confirm-request has correct type, prompt, default"
  (define-values (req result-ch)
    (run-ui-in-thread (lambda (ch) (ui-confirm! ch "Continue?" #:default #t #:timeout 5))))
  (check-equal? (ui-request-type req) 'confirm)
  (check-equal? (ui-request-prompt req) "Continue?")
  (check-equal? (ui-confirm-request-default req) #t)
  (channel-put (ui-request-response-ch req) (ui-response #t #f))
  (channel-get result-ch))

(test-case "ui-select-request has options and multi?"
  (define-values (req result-ch)
    (run-ui-in-thread (lambda (ch)
                        (ui-select! ch
                                    '(("a" . "Option A") ("b" . "Option B"))
                                    #:prompt "Pick one"
                                    #:multi? #t
                                    #:timeout 5))))
  (check-equal? (ui-request-type req) 'select)
  (check-equal? (ui-select-request-multi? req) #t)
  (check-equal? (length (ui-select-request-options req)) 2)
  (channel-put (ui-request-response-ch req) (ui-response '("a") #f))
  (channel-get result-ch))

(test-case "ui-input-request has placeholder and default"
  (define-values (req result-ch)
    (run-ui-in-thread
     (lambda (ch) (ui-input! ch "Name?" #:default "anon" #:placeholder "type here" #:timeout 5))))
  (check-equal? (ui-request-type req) 'input)
  (check-equal? (ui-input-request-placeholder req) "type here")
  (check-equal? (ui-input-request-default req) "anon")
  (channel-put (ui-request-response-ch req) (ui-response "anon" #f))
  (channel-get result-ch))

;; ============================================================
;; Confirm response handling
;; ============================================================

(test-case "ui-confirm! returns user value on confirm"
  (define-values (req result-ch)
    (run-ui-in-thread (lambda (ch) (ui-confirm! ch "OK?" #:default #f #:timeout 5))))
  (channel-put (ui-request-response-ch req) (ui-response #t #f))
  (define result-msg (channel-get result-ch))
  (check-equal? (cadr result-msg) #t))

(test-case "ui-confirm! returns #f on cancelled"
  (define-values (req result-ch)
    (run-ui-in-thread (lambda (ch) (ui-confirm! ch "Sure?" #:timeout 5))))
  (channel-put (ui-request-response-ch req) (ui-response #t #t))
  (define result-msg (channel-get result-ch))
  (check-equal? (cadr result-msg) #f))

;; ============================================================
;; Select response handling
;; ============================================================

(test-case "ui-select! returns selected id"
  (define-values (req result-ch)
    (run-ui-in-thread (lambda (ch) (ui-select! ch '(("x" . "X") ("y" . "Y")) #:timeout 5))))
  (channel-put (ui-request-response-ch req) (ui-response '("x") #f))
  (define result-msg (channel-get result-ch))
  (check-equal? (cadr result-msg) '("x")))

(test-case "ui-select! returns #f on empty options"
  (check-equal? (ui-select! (make-ui-channel) '() #:timeout 1) #f))

(test-case "ui-select! returns #f on cancelled"
  (define-values (req result-ch)
    (run-ui-in-thread (lambda (ch) (ui-select! ch '(("a" . "A")) #:timeout 5))))
  (channel-put (ui-request-response-ch req) (ui-response #f #t))
  (define result-msg (channel-get result-ch))
  (check-false (cadr result-msg)))

;; ============================================================
;; Input response handling
;; ============================================================

(test-case "ui-input! returns user text"
  (define-values (req result-ch) (run-ui-in-thread (lambda (ch) (ui-input! ch "Enter:" #:timeout 5))))
  (channel-put (ui-request-response-ch req) (ui-response "hello" #f))
  (define result-msg (channel-get result-ch))
  (check-equal? (cadr result-msg) "hello"))

(test-case "ui-input! returns #f on cancelled"
  (define-values (req result-ch) (run-ui-in-thread (lambda (ch) (ui-input! ch "Name?" #:timeout 5))))
  (channel-put (ui-request-response-ch req) (ui-response "" #t))
  (define result-msg (channel-get result-ch))
  (check-false (cadr result-msg)))

;; ============================================================
;; Timeout handling
;; ============================================================

(test-case "ui-confirm! returns default on timeout"
  (define ch (make-ui-channel))
  (thread (lambda () (channel-get ch))) ; drain request, don't respond
  (define result (ui-confirm! ch "Wait?" #:default #t #:timeout 0.1))
  (check-equal? result #t))

(test-case "ui-select! returns timed-out on timeout"
  (define ch (make-ui-channel))
  (thread (lambda () (channel-get ch))) ; drain request
  (define result (ui-select! ch '(("a" . "A")) #:timeout 0.1))
  (check-equal? result 'timed-out))

(test-case "ui-input! returns timed-out on timeout"
  (define ch (make-ui-channel))
  (thread (lambda () (channel-get ch))) ; drain request
  (define result (ui-input! ch "Quick!" #:timeout 0.1))
  (check-equal? result 'timed-out))

;; ============================================================
;; Channel type
;; ============================================================

(test-case "make-ui-channel creates a ui-channel"
  (check-true (ui-channel? (make-ui-channel))))
