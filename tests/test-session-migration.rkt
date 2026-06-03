#lang racket
;; BOUNDARY: serialization

;; BOUNDARY: integration

;;; tests/test-session-migration.rkt — tests for session format versioning (#773)

(require rackunit
         rackunit/text-ui
         racket/file
         json
         "../util/json/jsonl.rkt"
         "../util/message/protocol-types.rkt"
         "../runtime/session/session-migration.rkt")

(define (make-temp-file)
  (make-temporary-file "q-migration-test-~a.jsonl"))

(define (write-v1-file! path)
  ;; Write a v1 format file (no version header, just messages)
  (call-with-output-file path
                         (lambda (out)
                           (write-json (message->jsexpr (make-message "m1"
                                                                      #f
                                                                      'user
                                                                      'message
                                                                      (list (make-text-part "hello"))
                                                                      (current-seconds)
                                                                      (hasheq)))
                                       out)
                           (newline out)
                           (write-json (message->jsexpr (make-message "m2"
                                                                      #f
                                                                      'assistant
                                                                      'message
                                                                      (list (make-text-part "hi"))
                                                                      (current-seconds)
                                                                      (hasheq)))
                                       out)
                           (newline out))
                         #:exists 'truncate))

(define (write-v2-file! path)
  ;; Write a v2 format file (with session-info version header)
  (call-with-output-file path
                         (lambda (out)
                           (write-json (message->jsexpr (make-message "session-version-header-test"
                                                                      #f
                                                                      'system
                                                                      'session-info
                                                                      '()
                                                                      (current-seconds)
                                                                      (hasheq 'version 2)))
                                       out)
                           (newline out)
                           (write-json (message->jsexpr (make-message "m1"
                                                                      #f
                                                                      'user
                                                                      'message
                                                                      (list (make-text-part "hello"))
                                                                      (current-seconds)
                                                                      (hasheq)))
                                       out)
                           (newline out))
                         #:exists 'truncate))

(test-case "current-session-version is 2"
  (check-equal? current-session-version 2))

(test-case "read-session-version returns 1 for v1 file (no header)"
  (define path (make-temp-file))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (write-v1-file! path)
                  (check-equal? (read-session-version path) 1))
                (lambda () (delete-file path))))

(test-case "read-session-version returns 2 for v2 file (with header)"
  (define path (make-temp-file))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (write-v2-file! path)
                  (check-equal? (read-session-version path) 2))
                (lambda () (delete-file path))))

(test-case "migrate-v1-to-v2 adds version header"
  (define path (make-temp-file))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (write-v1-file! path)
                  ;; Migrate
                  (migrate-session-log! path)
                  ;; Check version is now 2
                  (check-equal? (read-session-version path) 2)
                  ;; Check all entries are preserved (header + 2 originals)
                  (define entries (jsonl-read-all-valid path))
                  (check-equal? (length entries) 3)
                  ;; First entry should be the version header
                  (define header (jsexpr->message (car entries)))
                  (check-equal? (message-kind header) 'session-info)
                  (check-equal? (hash-ref (message-meta header) 'version) 2)
                  ;; Original entries preserved
                  (define msg1 (jsexpr->message (cadr entries)))
                  (check-equal? (message-id msg1) "m1")
                  (define msg2 (jsexpr->message (caddr entries)))
                  (check-equal? (message-id msg2) "m2"))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "migration is idempotent — v2 file unchanged"
  (define path (make-temp-file))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (write-v2-file! path)
                  (define before-count (length (jsonl-read-all-valid path)))
                  (migrate-session-log! path)
                  (define after-count (length (jsonl-read-all-valid path)))
                  (check-equal? after-count before-count)
                  (check-equal? (read-session-version path) 2))
                (lambda () (delete-file path))))

(test-case "migration creates backup of v1 file"
  (define path (make-temp-file))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (write-v1-file! path)
                  (migrate-session-log! path)
                  (define bak-path (format "~a.v1.bak" path))
                  (check-true (file-exists? bak-path))
                  (delete-file bak-path))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "ensure-session-version! creates header for new file"
  (define path (make-temporary-file "q-version-test-~a.jsonl"))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (delete-file path)
                  (ensure-session-version! path)
                  (check-true (file-exists? path))
                  (check-equal? (read-session-version path) 2))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "session migration: persistence round-trip integrity"
  (define path (make-temporary-file "q-session-rt-~a.jsonl"))
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     ;; Write v2 entries using jsonl-append! (handles serialization)
     (jsonl-append! path (hasheq 'kind "session-info" 'version 2 'timestamp (current-seconds)))
     (jsonl-append! path (hasheq 'kind "user" 'content "hello" 'id "msg-1"))
     (jsonl-append! path (hasheq 'kind "assistant" 'content "hi there" 'id "msg-2"))
     ;; Read back
     (define entries
       (for/list ([line (file->lines path)])
         (with-input-from-string line read-json)))
     (check-equal? (length entries) 3)
     (check-equal? (hash-ref (first entries) 'version) 2)
     ;; Append new message
     (jsonl-append! path (hasheq 'kind "user" 'content "follow-up" 'id "msg-3"))
     ;; Read again
     (define entries-2
       (for/list ([line (file->lines path)])
         (with-input-from-string line read-json)))
     (check-equal? (length entries-2) 4)
     (check-equal? (hash-ref (fourth entries-2) 'content) "follow-up")
     ;; Verify version header preserved
     (check-equal? (hash-ref (first entries-2) 'kind) "session-info"))
   (lambda ()
     (when (file-exists? path)
       (delete-file path)))))

(test-case "migration registry: register-migration! + run-migrations!"
  (define path (make-temp-file))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (write-v1-file! path)
                  ;; Should still migrate v1->v2 using registry
                  (migrate-session-log! path)
                  (check-equal? (read-session-version path) 2)
                  (define entries (jsonl-read-all-valid path))
                  (check-equal? (length entries) 3))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path))
                  (when (file-exists? (format "~a.v1.bak" path))
                    (delete-file (format "~a.v1.bak" path))))))

(test-case "migration registry: register-migration! adds to chain"
  ;; Verify the v1 migration is registered
  (define path (make-temp-file))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (write-v1-file! path)
                  ;; v1->v2 migration should fire via registry
                  (migrate-session-log! path)
                  (check-equal? (read-session-version path) 2)
                  ;; Verify backup was created (proves migrate-v1->v2! ran)
                  (check-true (file-exists? (format "~a.v1.bak" path))))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path))
                  (when (file-exists? (format "~a.v1.bak" path))
                    (delete-file (format "~a.v1.bak" path))))))
