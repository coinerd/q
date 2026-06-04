#lang racket

;; tests/test-browser-audit.rkt — Browser audit logging tests

(require rackunit
         racket/file
         json
         "../browser/audit.rkt"
         "../browser/types.rkt"
         "../agent/event-bus.rkt"
         "../util/event/event.rkt")

(define (make-temp-dir)
  (define dir (build-path (find-system-path 'temp-dir)
                          (format "q-browser-audit-test-~a" (current-milliseconds))))
  (make-directory* dir)
  dir)

(test-case "log-browser-action! creates log directory and file"
  (define dir (make-temp-dir))
  (log-browser-action! "s1" 'open 'ok dir)
  (check-true (file-exists? (audit-log-path dir)))
  (delete-directory/files dir #:must-exist? #t))

(test-case "log-browser-action! appends valid JSONL"
  (define dir (make-temp-dir))
  (log-browser-action! "s1" 'open 'ok dir)
  (log-browser-action! "s1" 'navigate 'ok dir)
  (log-browser-action! "s1" 'close 'ok dir)
  (define lines (file->lines (audit-log-path dir)))
  (check-equal? (length lines) 3)
  (delete-directory/files dir #:must-exist? #t))

(test-case "log-browser-action! with observation result"
  (define dir (make-temp-dir))
  (define obs (browser-observation "https://x.com" "Title" "text" "visible"
                                    #f #f #f #f '() '() (hash) (hash)))
  (log-browser-action! "s1" 'observe obs dir)
  (define lines (file->lines (audit-log-path dir)))
  (check-equal? (length lines) 1)
  (check-not-false (regexp-match? #rx"observation" (first lines)))
  (delete-directory/files dir #:must-exist? #t))

(test-case "log-browser-action! with #f artifact-dir does nothing"
  (check-not-exn (lambda () (log-browser-action! "s1" 'open 'ok #f))))

(test-case "log-browser-action! entries contain required fields"
  (define dir (make-temp-dir))
  (log-browser-action! "s1" 'open 'ok dir)
  (define line (first (file->lines (audit-log-path dir))))
  (define entry (string->jsexpr line))
  (check-not-false (hash-has-key? entry 'timestamp))
  (check-not-false (hash-has-key? entry 'session-id))
  (check-not-false (hash-has-key? entry 'action))
  (check-not-false (hash-has-key? entry 'result))
  (check-equal? (hash-ref entry 'session-id) "s1")
  (check-equal? (hash-ref entry 'action) "open")
  (delete-directory/files dir #:must-exist? #t))

(test-case "emit-browser-event! with #f bus does nothing"
  (check-not-exn (lambda () (emit-browser-event! #f 'test "s1" (hash)))))

(test-case "emit-browser-event! publishes to event bus"
  (define bus (make-event-bus))
  (define received (box #f))
  (subscribe! bus
               (lambda (evt)
                 (set-box! received evt)))
  (emit-browser-event! bus 'test.event "s1" (hasheq 'key 'value))
  (check-not-false (unbox received)
                   "event should have been received by subscriber"))

(test-case "emit-browser-event! with symbol event-type converts to string"
  (define bus (make-event-bus))
  (define received-topic (box #f))
  (subscribe! bus
               (lambda (evt)
                 (set-box! received-topic (event-ev evt))))
  (emit-browser-event! bus 'browser.action.started "s1" (hasheq))
  (check-equal? (unbox received-topic) "browser.action.started"))
