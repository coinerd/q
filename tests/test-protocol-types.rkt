#lang racket

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt")

(define protocol-types-suite
  (test-suite "protocol-types: tree entry types"

    ;; ============================================================
    ;; #1314: branch-entry
    ;; ============================================================
    (test-case "make-branch-entry creates message with kind=branch"
      (define entry (make-branch-entry "entry-1" "entry-0" "fix-bug"))
      (check-true (message? entry))
      (check-equal? (message-kind entry) 'branch)
      (check-equal? (message-parent-id entry) "entry-0")
      (check-equal? (hash-ref (message-meta entry) 'branchName) "fix-bug"))

    (test-case "branch-entry? predicate"
      (define entry (make-branch-entry "e1" "e0" "feature"))
      (check-true (branch-entry? entry))
      (check-false (branch-entry? (make-message "m1" #f 'user 'message '() 0 (hasheq)))))

    (test-case "branch-entry accessors"
      (define entry (make-branch-entry "e1" "e0" "my-branch"))
      (check-equal? (branch-entry-parent-entry-id entry) "e0")
      (check-equal? (branch-entry-name entry) "my-branch"))

    (test-case "branch-entry JSON roundtrip"
      (define entry (make-branch-entry "e1" "e0" "fix"))
      (define json (message->jsexpr entry))
      (define restored (jsexpr->message json))
      (check-true (branch-entry? restored))
      (check-equal? (branch-entry-name restored) "fix")
      (check-equal? (branch-entry-parent-entry-id restored) "e0"))

    ;; ============================================================
    ;; #1314: tree-navigation-entry
    ;; ============================================================
    (test-case "make-tree-navigation-entry creates message with kind=tree-navigation"
      (define entry (make-tree-navigation-entry "e2" "e1" "target-456"))
      (check-true (message? entry))
      (check-equal? (message-kind entry) 'tree-navigation)
      (check-equal? (message-parent-id entry) "e1")
      (check-equal? (hash-ref (message-meta entry) 'targetEntryId) "target-456"))

    (test-case "tree-navigation-entry? predicate"
      (define entry (make-tree-navigation-entry "e2" "e1" "t1"))
      (check-true (tree-navigation-entry? entry))
      (check-false (tree-navigation-entry? (make-message "m1" #f 'user 'message '() 0 (hasheq)))))

    (test-case "tree-navigation-entry accessors"
      (define entry (make-tree-navigation-entry "e2" "e1" "t1"))
      (check-equal? (tree-navigation-entry-target-entry-id entry) "t1")
      (check-equal? (tree-navigation-entry-from-entry-id entry) "e1"))

    (test-case "tree-navigation-entry JSON roundtrip"
      (define entry (make-tree-navigation-entry "e2" "e1" "t1"))
      (define json (message->jsexpr entry))
      (define restored (jsexpr->message json))
      (check-true (tree-navigation-entry? restored))
      (check-equal? (tree-navigation-entry-target-entry-id restored) "t1"))

    ;; ============================================================
    ;; #1314: branch-summary-entry (different from existing predicate)
    ;; ============================================================
    (test-case "make-branch-summary-entry creates message with kind=branch-summary"
      (define entry (make-branch-summary-entry "e3" "e1" "Summary text" '("e0" "e1") 150))
      (check-true (message? entry))
      (check-equal? (message-kind entry) 'branch-summary)
      (check-equal? (hash-ref (message-meta entry) 'summary) "Summary text")
      (check-equal? (hash-ref (message-meta entry) 'tokenCount) 150))

    (test-case "branch-summary-entry predicate matches new entries"
      ;; The existing branch-summary-entry? predicate checks kind='branch-summary
      ;; so this should work with our new constructor
      (define entry (make-branch-summary-entry "e3" "e1" "Summary" '("e0" "e1") 150))
      (check-true (branch-summary-entry? entry)))

    (test-case "branch-summary-entry accessors"
      (define entry (make-branch-summary-entry "e3" "e1" "Summary text" '("e0" "e1") 150))
      (check-equal? (branch-summary-entry-summary entry) "Summary text")
      (check-equal? (branch-summary-entry-entry-range entry) '("e0" "e1"))
      (check-equal? (branch-summary-entry-token-count entry) 150))

    (test-case "branch-summary-entry JSON roundtrip"
      (define entry (make-branch-summary-entry "e3" "e1" "Summary text" '("e0" "e1") 150))
      (define json (message->jsexpr entry))
      (define restored (jsexpr->message json))
      (check-true (branch-summary-entry? restored))
      (check-equal? (branch-summary-entry-summary restored) "Summary text")
      (check-equal? (branch-summary-entry-token-count restored) 150))

    ;; ============================================================
    ;; General entry kind predicate: tree-entry?
    ;; ============================================================
    (test-case "tree-entry? matches all tree entry types"
      (check-true (tree-entry? (make-branch-entry "e1" "e0" "b")))
      (check-true (tree-entry? (make-tree-navigation-entry "e2" "e1" "t")))
      (check-true (tree-entry? (make-branch-summary-entry "e3" "e1" "S" '("e0") 50)))
      (check-false (tree-entry? (make-message "m1" #f 'user 'message '() 0 (hasheq)))))))

(run-tests protocol-types-suite)
