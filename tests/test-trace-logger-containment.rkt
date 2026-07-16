#lang racket

;; @speed fast
;; @suite security

(require rackunit
         racket/file
         "../runtime/trace-logger.rkt"
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt")

(test-case "trace logger rejects a trace artifact symlink before opening or writing"
  (define parent (make-temporary-file "q-trace-containment-~a" 'directory))
  (define session-dir (build-path parent "safe-session"))
  (define outside-dir (build-path parent "outside"))
  (make-directory* session-dir)
  (make-directory* outside-dir)
  (define sentinel (build-path outside-dir "sentinel.txt"))
  (display-to-file "unchanged" sentinel #:exists 'truncate)
  (make-file-or-directory-link sentinel (build-path session-dir "trace.jsonl"))
  (define logger (make-trace-logger (make-event-bus) session-dir #:enabled? #t))
  (check-exn exn:fail? (lambda () (start-trace-logger! logger)))
  (check-equal? (file->string sentinel) "unchanged")
  (delete-directory/files parent))

(test-case "async trace logger keeps the validated descriptor after pathname replacement"
  (define parent (make-temporary-file "q-trace-async-containment-~a" 'directory))
  (define session-dir (build-path parent "safe-session"))
  (define outside-dir (build-path parent "outside"))
  (make-directory* session-dir)
  (make-directory* outside-dir)
  (define sentinel (build-path outside-dir "sentinel.txt"))
  (display-to-file "unchanged" sentinel #:exists 'truncate)
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus session-dir #:enabled? #t #:async? #t))
  (start-trace-logger! logger)
  (define trace-path (build-path session-dir "trace.jsonl"))
  (define retained-path (build-path session-dir "trace.validated.jsonl"))
  (rename-file-or-directory trace-path retained-path)
  (make-file-or-directory-link sentinel trace-path)
  (publish! bus (make-event "test.safe-descriptor" 1000 "s1" #f (hasheq 'safe #t)))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (check-equal? (file->string sentinel) "unchanged")
  (check-regexp-match #rx"test.safe-descriptor" (file->string retained-path))
  (delete-directory/files parent))
