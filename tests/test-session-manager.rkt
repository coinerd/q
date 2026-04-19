#lang racket

;; tests/test-session-manager.rkt — Session manager abstraction (#1321)
;;
;; Tests in-memory and persistent session manager implementations.

(require rackunit
         rackunit/text-ui
         "../runtime/session-manager.rkt"
         "../util/protocol-types.rkt"
         racket/file)

(define (make-temp-dir)
  (make-temporary-file "sm-test-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define session-manager-suite
  (test-suite "session-manager: unified interface"

    ;; ============================================================
    ;; In-memory session manager
    ;; ============================================================

    (test-case "in-memory: append and load"
      (define mgr (make-in-memory-session-manager))
      (sm-append! mgr "s1" (make-message "m1" #f 'user 'message '() 0 (hasheq)))
      (sm-append! mgr "s1" (make-message "m2" "m1" 'assistant 'message '() 1 (hasheq)))
      (define loaded (sm-load mgr "s1"))
      (check-equal? (length loaded) 2)
      (check-equal? (message-id (car loaded)) "m1"))

    (test-case "in-memory: list sessions"
      (define mgr (make-in-memory-session-manager))
      (sm-append! mgr "s1" (make-message "m1" #f 'user 'message '() 0 (hasheq)))
      (sm-append! mgr "s2" (make-message "m2" #f 'user 'message '() 0 (hasheq)))
      (check-not-false (member "s1" (sm-list mgr)))
      (check-not-false (member "s2" (sm-list mgr))))

    (test-case "in-memory: fork"
      (define mgr (make-in-memory-session-manager))
      (sm-append! mgr "s1" (make-message "r" #f 'system 'session-info '() 0 (hasheq)))
      (sm-append! mgr "s1" (make-message "m1" "r" 'user 'message '() 1 (hasheq)))
      (sm-append! mgr "s1" (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      (sm-fork! mgr "s1" "s2" "m1")
      (define forked (sm-load mgr "s2"))
      (check-equal? (length forked) 2 "forked at m1, 2 entries"))

    ;; ============================================================
    ;; Persistent session manager
    ;; ============================================================

    (test-case "persistent: append and load"
      (define dir (make-temp-dir))
      (define mgr (make-persistent-session-manager dir))
      (sm-append! mgr "s1" (make-message "m1" #f 'user 'message '() 0 (hasheq)))
      (sm-append! mgr "s1" (make-message "m2" "m1" 'assistant 'message '() 1 (hasheq)))
      (define loaded (sm-load mgr "s1"))
      (check-equal? (length loaded) 2)
      (check-equal? (message-id (car loaded)) "m1")
      (cleanup-dir dir))

    (test-case "persistent: list sessions"
      (define dir (make-temp-dir))
      (define mgr (make-persistent-session-manager dir))
      (sm-append! mgr "s1" (make-message "m1" #f 'user 'message '() 0 (hasheq)))
      (sm-append! mgr "s2" (make-message "m2" #f 'user 'message '() 0 (hasheq)))
      (define sessions (sm-list mgr))
      (check-not-false (member "s1" sessions))
      (check-not-false (member "s2" sessions))
      (cleanup-dir dir))

    (test-case "persistent: fork"
      (define dir (make-temp-dir))
      (define mgr (make-persistent-session-manager dir))
      (sm-append! mgr "s1" (make-message "r" #f 'system 'session-info '() 0 (hasheq)))
      (sm-append! mgr "s1" (make-message "m1" "r" 'user 'message '() 1 (hasheq)))
      (sm-append! mgr "s1" (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      (sm-fork! mgr "s1" "s2" "m1")
      (define forked (sm-load mgr "s2"))
      (check >= (length forked) 1 "fork preserves at least 1 entry")
      (cleanup-dir dir))

    (test-case "persistent: load non-existent returns empty"
      (define dir (make-temp-dir))
      (define mgr (make-persistent-session-manager dir))
      (define loaded (sm-load mgr "nonexistent"))
      (check-equal? loaded '())
      (cleanup-dir dir))

    ;; ============================================================
    ;; Unified interface
    ;; ============================================================

    (test-case "session-manager? accepts both types"
      (check-true (session-manager? (make-in-memory-session-manager)))
      (define dir (make-temp-dir))
      (check-true (session-manager? (make-persistent-session-manager dir)))
      (cleanup-dir dir))))

(run-tests session-manager-suite)
