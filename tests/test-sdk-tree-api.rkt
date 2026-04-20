#lang racket

;; tests/test-sdk-tree-api.rkt — SDK tree API integration tests (#1336 Finding 9)
;;
;; Tests the public SDK tree API: q:session-branch, q:session-navigate,
;; and tree-related operations through the SDK interface.

(require rackunit
         rackunit/text-ui
         "../interfaces/sdk.rkt"
         "../util/protocol-types.rkt"
         (only-in "../runtime/session-store.rkt"
                  append-entry!
                  load-tree
                  append-tree-entry!
                  resolve-active-branch
                  tree-info)
         racket/file
         racket/list)

(define (make-temp-session-dir)
  (make-temporary-file "sdk-tree-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define sdk-tree-suite
  (test-suite "sdk-tree-api: integration tests"

    ;; ============================================================
    ;; q:session-branch
    ;; ============================================================

    (test-case "q:session-branch returns branch info"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      ;; Create a minimal session
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      ;; Load tree
      (define tree (load-tree path))
      ;; Create a branch from last entry (m2)
      (define entries (hash-ref tree '%%entries%% '()))
      (define last-entry (last entries))
      (define branch (make-branch-entry "b1" (message-id last-entry) "fix-branch"))
      (append-tree-entry! path branch)
      ;; Verify branch exists
      (define tree2 (load-tree path))
      (define entries2 (hash-ref tree2 '%%entries%% '()))
      (check-true (for/or ([e (in-list entries2)])
                    (and (eq? (message-kind e) 'branch)
                         (equal? (message-id e) "b1"))))
      (delete-directory/files dir))

    (test-case "q:session-navigate creates navigation entry"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      (append-entry! path (make-message "m3" "m2" 'user 'message '() 3 (hasheq)))
      ;; Navigate from last entry (m3) to m1
      (define nav (make-tree-navigation-entry "nav1" "m3" "m1"))
      (append-tree-entry! path nav)
      ;; Verify navigation exists
      (define tree (load-tree path))
      (define entries (hash-ref tree '%%entries%% '()))
      (check-true (for/or ([e (in-list entries)])
                    (and (eq? (message-kind e) 'tree-navigation)
                         (equal? (message-id e) "nav1")
                         (equal? (message-parent-id e) "m3"))))
      (delete-directory/files dir))

    ;; ============================================================
    ;; resolve-active-branch
    ;; ============================================================

    (test-case "resolve-active-branch returns main path"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-message "m2" "m1" 'assistant 'message '() 2 (hasheq)))
      (define tree (load-tree path))
      (define active (resolve-active-branch tree))
      (check-equal? (map message-id active) '("root" "m1" "m2"))
      (delete-directory/files dir))

    (test-case "resolve-active-branch follows branch"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (append-entry! path (make-branch-entry "b1" "m1" "fix"))
      (append-entry! path (make-message "m2" "b1" 'user 'message '() 2 (hasheq)))
      ;; Last entry is m2 on branch b1, so active is root->m1->b1->m2
      (define tree (load-tree path))
      (define active (resolve-active-branch tree))
      (check-equal? (map message-id active) '("root" "m1" "b1" "m2"))
      (delete-directory/files dir))

    (test-case "resolve-active-branch empty tree"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (define tree (load-tree path))
      (define active (resolve-active-branch tree))
      (check-equal? (map message-id active) '("root"))
      (delete-directory/files dir))

    ;; ============================================================
    ;; tree-info
    ;; ============================================================

    (test-case "tree-info returns summary"
      (define dir (make-temp-session-dir))
      (define path (session-path dir))
      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry! path (make-message "m1" "root" 'user 'message '() 1 (hasheq)))
      (define tree (load-tree path))
      (define info (tree-info tree))
      (check-true (hash-has-key? info 'total-entries))
      (check-true (hash-has-key? info 'branch-count))
      (delete-directory/files dir))))

(run-tests sdk-tree-suite)
