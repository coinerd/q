#lang racket

;; tests/test-session-tree-nav.rkt — tests for session tree navigation functions
;;
;; Covers GC-23 (remediation for GC-13):
;;   - navigate-to-entry!
;;   - navigate-to-leaf!
;;   - navigate-next-leaf!
;;   - navigate-prev-leaf!
;;   - navigate-result struct fields
;;   - Wrapping behavior (next/prev at edges)
;;   - Interaction with append-to-leaf!
;;   - branch-with-summary!
;;   - get-branch

(require rackunit
         racket/hash
         json
         "../util/protocol-types.rkt"
         "../runtime/session-index.rkt")

;; ============================================================
;; Test helpers
;; ============================================================

;; Create an empty in-memory session-index
(define (make-empty-index)
  (session-index (make-hash) (make-hash) (vector) (make-hash) (box #f) (make-semaphore 1)))

;; Make a simple message with id, parent-id, and text content
(define (make-test-msg id parent-id [text (format "content-~a" id)])
  (make-message id parent-id 'user 'message (list (make-text-part text)) (current-seconds) (hasheq)))

;; Build a session-index from a spec list for testing.
;; spec: list of (list id parent-id text)
;; Uses build-index! with a temp JSONL file for a proper index.
(require racket/file
         racket/port)

(define (write-session-jsonl! path messages)
  ;; Write messages as JSONL to path
  ;; messages: list of (list id parent-id role kind text)
  (call-with-output-file path
                         (lambda (out)
                           (for ([m (in-list messages)])
                             (define id (car m))
                             (define pid (cadr m))
                             (define role (caddr m))
                             (define kind (cadddr m))
                             (define text (car (cddddr m)))
                             (write-json (hasheq 'id
                                                 id
                                                 'parentId
                                                 pid
                                                 'role
                                                 (symbol->string role)
                                                 'kind
                                                 (symbol->string kind)
                                                 'content
                                                 (list (hasheq 'type "text" 'text text))
                                                 'timestamp
                                                 (current-seconds)
                                                 'meta
                                                 '#hasheq())
                                         out)
                             (newline out)))
                         #:mode 'text
                         #:exists 'truncate))

(define (build-index-from-spec spec)
  ;; spec: list of (list id parent-id text)
  ;; Builds a session-index from the spec
  (define tmp-dir (make-temporary-file "session-nav-test-~a" 'directory))
  (define session-path (build-path tmp-dir "session.jsonl"))
  (define index-path (build-path tmp-dir "session.index"))
  (define messages
    (for/list ([s (in-list spec)])
      (list (car s) (cadr s) 'user 'message (caddr s))))
  (write-session-jsonl! session-path messages)
  (define idx (build-index! session-path index-path))
  (delete-directory/files tmp-dir #:must-exist? #f)
  idx)

;; ============================================================
;; Tests: navigate-to-entry!
;; ============================================================

(test-case "navigate-to-entry! returns navigate-result for existing entry"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content")
                                                        ("c" "b" "c content"))))
  (define result (navigate-to-entry! idx "a"))
  (check-true (navigate-result? result))
  (check-equal? (message-id (navigate-result-entry result)) "a")
  ;; Branch should be path from root to entry
  (check-equal? (map message-id (navigate-result-branch result)) '("root" "a"))
  ;; 'a' has one child 'b', so not a leaf
  (check-false (navigate-result-leaf? result))
  (check-equal? (length (navigate-result-children result)) 1))

(test-case "navigate-to-entry! returns #f for missing entry"
  (define idx (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content"))))
  (check-false (navigate-to-entry! idx "nonexistent")))

(test-case "navigate-to-entry! for leaf node has leaf? = #t"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content")
                                                        ("c" "b" "c content"))))
  (define result (navigate-to-entry! idx "c"))
  (check-true (navigate-result? result))
  (check-true (navigate-result-leaf? result))
  (check-equal? (navigate-result-children result) '()))

(test-case "navigate-to-entry! for root node has branch = self only"
  (define idx (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content"))))
  (define result (navigate-to-entry! idx "root"))
  (check-equal? (map message-id (navigate-result-branch result)) '("root"))
  (check-false (navigate-result-leaf? result)))

(test-case "navigate-to-entry! updates active leaf"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content"))))
  (navigate-to-entry! idx "b")
  (define active (active-leaf idx))
  (check-not-false active)
  (check-equal? (message-id active) "b"))

;; ============================================================
;; Tests: navigate-to-leaf!
;; ============================================================

(test-case "navigate-to-leaf! succeeds for leaf node"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("c" "a" "c content"))))
  (define result (navigate-to-leaf! idx "c"))
  (check-true (navigate-result? result))
  (check-equal? (message-id (navigate-result-entry result)) "c")
  (check-true (navigate-result-leaf? result)))

(test-case "navigate-to-leaf! returns #f for non-leaf node"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content"))))
  ;; 'root' has children, not a leaf
  (check-false (navigate-to-leaf! idx "root"))
  ;; 'a' has child 'b', not a leaf
  (check-false (navigate-to-leaf! idx "a")))

(test-case "navigate-to-leaf! returns #f for missing entry"
  (define idx (build-index-from-spec '(("root" #f "root content"))))
  (check-false (navigate-to-leaf! idx "nonexistent")))

;; ============================================================
;; Tests: navigate-next-leaf! and navigate-prev-leaf!
;; ============================================================

(test-case "navigate-next-leaf! advances through leaves"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2")
                                                        ("r2" "r1" "right 2"))))
  ;; Leaves are l2, r2 (in append order)
  ;; Start at l2
  (navigate-to-entry! idx "l2")
  (define result (navigate-next-leaf! idx))
  (check-true (navigate-result? result))
  (check-equal? (message-id (navigate-result-entry result)) "r2"))

(test-case "navigate-prev-leaf! goes to previous leaf"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2")
                                                        ("r2" "r1" "right 2"))))
  ;; Start at r2
  (navigate-to-entry! idx "r2")
  (define prev (navigate-prev-leaf! idx))
  (check-true (navigate-result? prev))
  (check-equal? (message-id (navigate-result-entry prev)) "l2"))

(test-case "navigate-next-leaf! wraps from last to first"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2")
                                                        ("r2" "r1" "right 2"))))
  ;; Start at r2 (last leaf), next should wrap to l2 (first leaf)
  (navigate-to-entry! idx "r2")
  (define wrapped (navigate-next-leaf! idx))
  (check-true (navigate-result? wrapped))
  (check-equal? (message-id (navigate-result-entry wrapped)) "l2"))

(test-case "navigate-prev-leaf! wraps from first to last"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2")
                                                        ("r2" "r1" "right 2"))))
  ;; Start at l2 (first leaf), prev should wrap to r2 (last leaf)
  (navigate-to-entry! idx "l2")
  (define wrapped (navigate-prev-leaf! idx))
  (check-true (navigate-result? wrapped))
  (check-equal? (message-id (navigate-result-entry wrapped)) "r2"))

(test-case "navigate-next-leaf! returns #f on empty index"
  (define idx (build-index-from-spec '()))
  (check-false (navigate-next-leaf! idx)))

(test-case "navigate-prev-leaf! returns #f on empty index"
  (define idx (build-index-from-spec '()))
  (check-false (navigate-prev-leaf! idx)))

(test-case "navigate-next-leaf! with single leaf wraps to self"
  (define idx (build-index-from-spec '(("only" #f "only content"))))
  (navigate-to-entry! idx "only")
  (define result (navigate-next-leaf! idx))
  (check-true (navigate-result? result))
  (check-equal? (message-id (navigate-result-entry result)) "only"))

(test-case "navigate-next-leaf! starts from default when no active leaf"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2")
                                                        ("r2" "r1" "right 2"))))
  ;; No active leaf set — reset to default
  (reset-leaf! idx)
  (define result (navigate-next-leaf! idx))
  ;; Should navigate to first leaf (l2) since default position is -1 → next = 0
  (check-true (navigate-result? result))
  (check-equal? (message-id (navigate-result-entry result)) "l2"))

;; ============================================================
;; Tests: navigate-result struct
;; ============================================================

(test-case "navigate-result has all expected fields"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2"))))
  (define result (navigate-to-entry! idx "l1"))
  (check-true (navigate-result? result))
  (check-not-false (navigate-result-entry result))
  (check-not-false (navigate-result-branch result))
  (check-not-false (navigate-result-children result))
  ;; l1 has child l2, so it's not a leaf
  (check-false (navigate-result-leaf? result)))

(test-case "navigate-result branch path is root-first for deep node"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content")
                                                        ("c" "b" "c content"))))
  (define result (navigate-to-entry! idx "c"))
  (define branch-ids (map message-id (navigate-result-branch result)))
  (check-equal? branch-ids '("root" "a" "b" "c")))

(test-case "navigate-result branch path in branching tree"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2")
                                                        ("r2" "r1" "right 2"))))
  (define result (navigate-to-entry! idx "r2"))
  (define branch-ids (map message-id (navigate-result-branch result)))
  (check-equal? branch-ids '("root" "r1" "r2")))

;; ============================================================
;; Tests: append-to-leaf! interaction with navigation
;; ============================================================

(test-case "append-to-leaf! after navigation creates child of navigated node"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content")
                                                        ("c" "b" "c content"))))
  ;; Navigate to 'b'
  (navigate-to-entry! idx "b")
  ;; Append a child of 'b'
  (define new-entry (make-test-msg "new-child" "b" "new content"))
  (append-to-leaf! idx new-entry)
  ;; 'b' should now have two children: c and new-child
  (define children-of-b (children-of idx "b"))
  (check-equal? (length children-of-b) 2)
  (define child-ids (map message-id children-of-b))
  (check-not-false (member "c" child-ids))
  (check-not-false (member "new-child" child-ids)))

(test-case "append-to-leaf! advances active leaf to new entry"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content"))))
  (navigate-to-entry! idx "b")
  (define new-entry (make-test-msg "new-child" "b"))
  (append-to-leaf! idx new-entry)
  (define active (active-leaf idx))
  (check-not-false active)
  (check-equal? (message-id active) "new-child"))

;; ============================================================
;; Tests: branch-with-summary! (also covers GC-24)
;; ============================================================

(test-case "branch-with-summary! creates branch-summary message"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content")
                                                        ("c" "b" "c content"))))
  (define summary (branch-with-summary! idx "b" "Summary of branch b"))
  (check-true (message? summary))
  (check-equal? (message-kind summary) 'branch-summary)
  (check-equal? (message-parent-id summary) "b"))

(test-case "branch-with-summary! summary has correct content"
  (define idx (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content"))))
  (define summary (branch-with-summary! idx "a" "Test summary"))
  (define content-text
    (string-join (for/list ([part (message-content summary)])
                   (if (text-part? part)
                       (text-part-text part)
                       ""))
                 ""))
  (check-equal? content-text "Test summary"))

(test-case "branch-with-summary! moves active leaf to summary child"
  (define idx (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content"))))
  (branch-with-summary! idx "a" "Summary")
  ;; Active leaf should be set (to the new summary message)
  (define active (active-leaf idx))
  (check-not-false active)
  (check-equal? (message-kind active) 'branch-summary))

(test-case "branch-with-summary! returns #f for nonexistent entry"
  (define idx (build-index-from-spec '(("root" #f "root content"))))
  (check-false (branch-with-summary! idx "nonexistent" "Summary")))

(test-case "branch-with-summary! adds child to target entry"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content"))))
  (define summary (branch-with-summary! idx "a" "Summary"))
  ;; 'a' originally had child 'b'; now also has the summary as child
  (define children-of-a (children-of idx "a"))
  (check-equal? (length children-of-a) 2))

;; ============================================================
;; Tests: get-branch (supporting navigation)
;; ============================================================

(test-case "get-branch returns path from root to entry"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content")
                                                        ("b" "a" "b content")
                                                        ("c" "b" "c content"))))
  (define branch (get-branch idx "c"))
  (check-equal? (map message-id branch) '("root" "a" "b" "c")))

(test-case "get-branch returns #f for nonexistent entry"
  (define idx (build-index-from-spec '(("root" #f "root content"))))
  (check-false (get-branch idx "nonexistent")))

(test-case "get-branch for root returns just root"
  (define idx (build-index-from-spec '(("root" #f "root content") ("a" "root" "a content"))))
  (define branch (get-branch idx "root"))
  (check-equal? (map message-id branch) '("root")))

(test-case "get-branch in branching tree takes correct path"
  (define idx
    (build-index-from-spec '(("root" #f "root content") ("l1" "root" "left 1")
                                                        ("r1" "root" "right 1")
                                                        ("l2" "l1" "left 2")
                                                        ("r2" "r1" "right 2"))))
  (define branch (get-branch idx "r2"))
  (check-equal? (map message-id branch) '("root" "r1" "r2"))
  (define branch2 (get-branch idx "l2"))
  (check-equal? (map message-id branch2) '("root" "l1" "l2")))
