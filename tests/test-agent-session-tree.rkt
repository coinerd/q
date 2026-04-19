#lang racket

;; tests/test-agent-session-tree.rkt — Tree operations in agent-session (#1317, #1318, #1319)
;;
;; Tests tree integration: branch!, navigate!, branch summaries, SDK tree API.
;; Uses session-store primitives directly to verify persistence and querying.

(require rackunit
         rackunit/text-ui
         racket/file
         "../runtime/session-store.rkt"
         "../util/protocol-types.rkt")

;; Helper: create temp session directory
(define (make-temp-dir)
  (make-temporary-file "session-tree-agent-test-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define (make-session-path dir)
  (build-path dir "session.jsonl"))

(define agent-session-tree-suite
  (test-suite "agent-session-tree: tree integration"

    ;; ============================================================
    ;; #1317: branch! and navigate! create tree entries in session log
    ;; ============================================================

    (test-case "branch! creates branch-entry in session log"
      (define dir (make-temp-dir))
      (define path (make-session-path dir))

      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry!
       path
       (make-message "m1" "root" 'user 'message (list (make-text-part "hello")) 1 (hasheq)))
      (append-entry!
       path
       (make-message "m2" "m1" 'assistant 'message (list (make-text-part "world")) 2 (hasheq)))

      ;; Branch at m1
      (define branch (make-branch-entry "b1" "m1" "fix-bug"))
      (append-tree-entry! path branch)

      (define entries (load-session-log path))
      (check-equal? (length entries) 4 "4 entries total after branch")
      (define last-entry (list-ref entries 3))
      (check-equal? (message-kind last-entry) 'branch "last entry is branch")
      (check-pred branch-entry? last-entry)
      (check-equal? (branch-entry-parent-entry-id last-entry) "m1")
      (check-equal? (branch-entry-name last-entry) "fix-bug")

      (cleanup-dir dir))

    (test-case "navigate! creates navigation-entry in session log"
      (define dir (make-temp-dir))
      (define path (make-session-path dir))

      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry!
       path
       (make-message "m1" "root" 'user 'message (list (make-text-part "hello")) 1 (hasheq)))

      (define nav (make-tree-navigation-entry "n1" "root" "m1"))
      (append-tree-entry! path nav)

      (define entries (load-session-log path))
      (check-equal? (length entries) 3 "3 entries after navigate")
      (define last-entry (list-ref entries 2))
      (check-equal? (message-kind last-entry) 'tree-navigation)
      (check-pred tree-navigation-entry? last-entry)
      (check-equal? (tree-navigation-entry-target-entry-id last-entry) "m1")
      (check-equal? (tree-navigation-entry-from-entry-id last-entry) "root")

      (cleanup-dir dir))

    (test-case "branch and navigate maintain parent chain"
      (define dir (make-temp-dir))
      (define path (make-session-path dir))

      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry!
       path
       (make-message "m1" "root" 'user 'message (list (make-text-part "q1")) 1 (hasheq)))
      (append-entry!
       path
       (make-message "m2" "m1" 'assistant 'message (list (make-text-part "a1")) 2 (hasheq)))

      (define branch (make-branch-entry "b1" "m1" "experiment"))
      (append-tree-entry! path branch)

      (define nav (make-tree-navigation-entry "n1" "root" "m1"))
      (append-tree-entry! path nav)

      (define tree (load-tree path))
      (define path-to-root (get-tree-branch tree "root"))
      (check-equal? (length path-to-root) 1 "root path length 1")
      (check-equal? (message-id (car path-to-root)) "root")

      (define path-to-m2 (get-tree-branch tree "m2"))
      (check-equal? (length path-to-m2) 3 "root -> m1 -> m2")

      (cleanup-dir dir))

    (test-case "fork preserves tree entries"
      (define dir (make-temp-dir))
      (define path (make-session-path dir))

      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry!
       path
       (make-message "m1" "root" 'user 'message (list (make-text-part "q1")) 1 (hasheq)))

      (define branch (make-branch-entry "b1" "root" "fix"))
      (append-tree-entry! path branch)

      ;; Fork using session-store fork
      (define fork-dir (build-path dir "fork"))
      (make-directory fork-dir)
      (define fork-path (build-path fork-dir "session.jsonl"))
      (fork-session! path "m1" fork-path)

      (define fork-entries (load-session-log fork-path))
      (check >= (length fork-entries) 2 "fork preserves entries")

      (cleanup-dir dir))

    ;; ============================================================
    ;; #1318: Branch summaries
    ;; ============================================================

    (test-case "branch-summary-entry stores summary and token count"
      (define summary
        (make-branch-summary-entry "s1"
                                   "m1"
                                   "Branch discusses bug fix approach"
                                   (hasheq 'start-entry-id "m1" 'end-entry-id "m5")
                                   150))
      (check-pred branch-summary-entry? summary)
      (check-equal? (branch-summary-entry-summary summary) "Branch discusses bug fix approach")
      (check-equal? (branch-summary-entry-token-count summary) 150))

    (test-case "branch-summary-entry roundtrips through JSONL"
      (define dir (make-temp-dir))
      (define path (make-session-path dir))

      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))

      (define summary
        (make-branch-summary-entry "s1"
                                   "root"
                                   "Summary text"
                                   (hasheq 'start-entry-id "root" 'end-entry-id "root")
                                   42))
      (append-tree-entry! path summary)

      (define entries (load-session-log path))
      (check-equal? (length entries) 2)
      (define loaded (list-ref entries 1))
      (check-pred branch-summary-entry? loaded)
      (check-equal? (branch-summary-entry-token-count loaded) 42)

      (cleanup-dir dir))

    ;; ============================================================
    ;; #1319: tree-info returns tree metadata
    ;; ============================================================

    (test-case "tree-info returns structure from loaded tree"
      (define dir (make-temp-dir))
      (define path (make-session-path dir))

      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry!
       path
       (make-message "m1" "root" 'user 'message (list (make-text-part "q1")) 1 (hasheq)))
      (append-entry!
       path
       (make-message "m2" "m1" 'assistant 'message (list (make-text-part "a1")) 2 (hasheq)))
      (append-entry! path
                     (make-message "m3" "m1" 'user 'message (list (make-text-part "q2")) 3 (hasheq)))

      (define tree (load-tree path))
      (define info (tree-info tree))
      (check-equal? (hash-ref info 'total-entries) 4 "4 entries")
      (check-true (hash-has-key? info 'branch-count))

      (cleanup-dir dir))

    (test-case "tree-info detects branches"
      (define dir (make-temp-dir))
      (define path (make-session-path dir))

      (append-entry! path (make-message "root" #f 'system 'session-info '() 0 (hasheq)))
      (append-entry!
       path
       (make-message "m1" "root" 'user 'message (list (make-text-part "q1")) 1 (hasheq)))
      (append-entry!
       path
       (make-message "m2" "m1" 'assistant 'message (list (make-text-part "a1")) 2 (hasheq)))

      (define branch (make-branch-entry "b1" "m1" "experiment"))
      (append-tree-entry! path branch)

      (define tree (load-tree path))
      (define info (tree-info tree))
      (check-equal? (hash-ref info 'branch-count) 1 "1 branch detected")

      (cleanup-dir dir))))

(run-tests agent-session-tree-suite)
