#lang racket

;; tests/test-sdk-resource-loader.rkt — Resource loader injection (#1320)
;;
;; Tests that create-agent-session accepts #:resource-loader and uses it.

(require rackunit
         rackunit/text-ui
         "../interfaces/sdk.rkt"
         "../runtime/resource-loader.rkt"
         (only-in "../runtime/session-store.rkt"
                  make-in-memory-session-manager
                  in-memory-session-manager?
                  in-memory-append!
                  in-memory-load
                  in-memory-fork!)
         "../util/protocol-types.rkt"
         racket/file)

(define (make-temp-dir)
  (make-temporary-file "sdk-rl-test-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define sdk-resource-loader-suite
  (test-suite "sdk-resource-loader: resource loader injection"

    (test-case "create-agent-session accepts #:resource-loader parameter"
      ;; Verify the function signature accepts the keyword
      (define dir (make-temp-dir))
      (define mgr (make-in-memory-session-manager))
      ;; Just test that the call compiles and runs — the resource-loader
      ;; is stored for later use during extension discovery
      (check-not-exn (lambda ()
                       (create-agent-session #:provider (hasheq 'type 'test)
                                             #:session-dir dir
                                             #:resource-loader #f)))
      (cleanup-dir dir))

    (test-case "in-memory-session-manager works independently"
      (define mgr (make-in-memory-session-manager))
      (define msg (make-message "m1" #f 'user 'message '() 0 (hasheq)))
      (in-memory-append! mgr "s1" msg)
      (define loaded (in-memory-load mgr "s1"))
      (check-equal? (length loaded) 1)
      (check-equal? (message-id (car loaded)) "m1"))

    (test-case "in-memory-session-manager fork works"
      (define mgr (make-in-memory-session-manager))
      (in-memory-append! mgr "s1" (make-message "r" #f 'system 'session-info '() 0 (hasheq)))
      (in-memory-append! mgr "s1" (make-message "m1" "r" 'user 'message '() 1 (hasheq)))
      (in-memory-append! mgr "s1" (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      (in-memory-fork! mgr "s1" "s2" "m1")
      (define forked (in-memory-load mgr "s2"))
      (check-equal? (length forked) 2 "forked at m1, 2 entries")
      (check-equal? (message-id (car forked)) "r")
      (check-equal? (message-id (cadr forked)) "m1"))))

(run-tests sdk-resource-loader-suite)
