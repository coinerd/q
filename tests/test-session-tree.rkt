#lang racket

(require rackunit
         rackunit/text-ui
         "../runtime/session-store.rkt"
         "../util/protocol-types.rkt"
         "../extensions/hooks.rkt"
         racket/file)

;; Helper: create a temp session dir
(define (make-temp-session-dir)
  (define dir (make-temporary-file "session-tree-test-~a" 'directory))
  dir)

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define session-tree-suite
  (test-suite "session-tree: tree store operations"

    ;; ============================================================
    ;; #1315: append-tree-entry!
    ;; ============================================================
    (test-case "append-tree-entry! writes branch-entry to JSONL"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "m0" #f 'system 'session-info '() 0 (hasheq)))
      (define branch (make-branch-entry "b1" "m0" "fix-bug"))
      (append-tree-entry! path branch)
      (define loaded (load-session-log path))
      (check-equal? (length loaded) 2)
      (check-true (branch-entry? (second loaded)))
      (check-equal? (branch-entry-name (second loaded)) "fix-bug")
      (delete-directory/files dir))

    (test-case "append-tree-entry! writes navigation-entry to JSONL"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "m0" #f 'system 'session-info '() 0 (hasheq)))
      (define nav (make-tree-navigation-entry "n1" "m0" "m0"))
      (append-tree-entry! path nav)
      (define loaded (load-session-log path))
      (check-true (tree-navigation-entry? (second loaded)))
      (delete-directory/files dir))

    ;; ============================================================
    ;; #1315: load-tree
    ;; ============================================================
    (test-case "load-tree builds tree from JSONL"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      ;; Write: root -> m1 -> branch at m1 -> b1_msg
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry!
       path
       (make-message "m1" "root" 'user 'message (list (make-text-part "hi")) 1 (hasheq)))
      (append-entry! path (make-branch-entry "branch-1" "m1" "fix-bug"))
      (append-entry!
       path
       (make-message "m2" "branch-1" 'assistant 'message (list (make-text-part "fix")) 2 (hasheq)))
      ;; Load tree
      (define tree (load-tree path))
      (check-not-false (hash-ref tree "root" #f))
      (check-not-false (hash-ref tree "m1" #f))
      (check-not-false (hash-ref tree "branch-1" #f))
      (check-not-false (hash-ref tree "m2" #f))
      (delete-directory/files dir))

    (test-case "load-tree returns id->children map"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-message "m2" "root" 'user 'message '() 2 (hasheq)))
      (define tree (load-tree path))
      ;; root should have m1 and m2 as children
      (define root-children (hash-ref tree "root"))
      (check-equal? (length root-children) 2)
      (check-equal? (sort (map message-id root-children) string<?) '("m1" "m2"))
      (delete-directory/files dir))

    ;; ============================================================
    ;; #1315: get-tree-branch
    ;; ============================================================
    (test-case "get-tree-branch returns path from root to entry"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      (define tree (load-tree path))
      (define branch (get-tree-branch tree "m2"))
      (check-equal? (map message-id branch) '("root" "m1" "m2"))
      (delete-directory/files dir))

    (test-case "get-tree-branch works with branch entries"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-branch-entry "b1" "m1" "fix"))
      (append-entry! path (make-message "m2" "b1" 'assistant 'message '() 2 (hasheq)))
      (define tree (load-tree path))
      (define branch (get-tree-branch tree "m2"))
      (check-equal? (map message-id branch) '("root" "m1" "b1" "m2"))
      (delete-directory/files dir))

    ;; ============================================================
    ;; #1315: get-children
    ;; ============================================================
    (test-case "get-children returns direct children"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-branch-entry "b1" "root" "fix"))
      (define tree (load-tree path))
      (define children (get-children tree "root"))
      (check-equal? (length children) 2)
      (check-equal? (sort (map message-id children) string<?) '("b1" "m1"))
      (delete-directory/files dir))

    (test-case "get-children returns empty for leaf"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (define tree (load-tree path))
      (check-equal? (get-children tree "root") '())
      (delete-directory/files dir))

    ;; ============================================================
    ;; #1315: resolve-active-branch
    ;; ============================================================
    (test-case "resolve-active-branch finds last entry's path"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      (define tree (load-tree path))
      (define active (resolve-active-branch tree path))
      (check-equal? (map message-id active) '("root" "m1" "m2"))
      (delete-directory/files dir))

    (test-case "resolve-active-branch follows main path after branch"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-branch-entry "b1" "m1" "fix"))
      (append-entry! path (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      ;; Last entry is m2 (on main path), so active branch is root->m1->m2
      (define tree (load-tree path))
      (define active (resolve-active-branch tree path))
      (check-equal? (map message-id active) '("root" "m1" "m2"))
      (delete-directory/files dir))

    ;; ============================================================
    ;; #1315: hash chain maintained for tree entries
    ;; ============================================================
    (test-case "tree entries maintain hash chain integrity"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-tree-entry! path (make-branch-entry "b1" "root" "fix"))
      (define report (verify-hash-chain path))
      (check-true (hash-ref report 'valid?))
      (delete-directory/files dir))

    ;; ============================================================
    ;; #1316: Tree navigation events
    ;; ============================================================
    (test-case "branch! emits session-before-tree and session-tree events"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      ;; Track events
      (define events '())
      (define (on-event name payload)
        (set! events (append events (list (cons name payload)))))
      ;; Create branch with event callback
      (define branch (make-branch-entry "b1" "m1" "fix"))
      (append-tree-entry! path branch #:before-hook on-event #:after-hook on-event)
      ;; Check events were emitted
      (check >= (length events) 1 "at least one event emitted")
      (delete-directory/files dir))

    (test-case "navigate! emits session-before-tree and session-tree events"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      ;; Track events
      (define events '())
      (define (on-event name payload)
        (set! events (append events (list (cons name payload)))))
      ;; Navigate with event callback
      (define nav (make-tree-navigation-entry "n1" "m1" "root"))
      (append-tree-entry! path nav #:before-hook on-event #:after-hook on-event)
      (check >= (length events) 1 "at least one event emitted")
      (delete-directory/files dir))

    (test-case "before-hook can block branch creation"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      ;; Hook that blocks
      (define (blocking-hook name payload)
        (when (equal? name 'session-before-tree)
          (error 'blocked "blocked")))
      ;; Branch creation should be blocked
      (define branch (make-branch-entry "b1" "root" "fix"))
      (check-exn exn:fail? (lambda () (append-tree-entry! path branch #:before-hook blocking-hook)))
      ;; Verify entry was NOT written
      (define loaded (load-session-log path))
      (check-equal? (length loaded) 1 "only root entry exists")
      (delete-directory/files dir))))

(run-tests session-tree-suite)
