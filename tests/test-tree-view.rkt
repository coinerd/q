#lang racket

;;; tests/test-tree-view.rkt — TDD tests for tui/tree-view.rkt
;;;
;;; Covers:
;;;   - Tree node creation and accessors
;;;   - render-session-tree with single root, multiple roots, nested children
;;;   - Active leaf marker (◄)
;;;   - Truncation of long text
;;;   - build-tree-nodes from messages
;;;   - selected-node by index

(require rackunit
         (only-in "../tui/tree-view.rkt"
                  render-session-tree
                  tree-node?
                  make-tree-node
                  build-tree-nodes
                  selected-node)
         (only-in "../util/protocol-types.rkt" make-text-part make-message))

;; ============================================================
;; Tree node struct
;; ============================================================

(test-case "make-tree-node creates transparent struct"
  (define node (make-tree-node "n1" 'user "hello" 0 '()))
  (check-true (tree-node? node)))

;; ============================================================
;; render-session-tree — single root, no children
;; ============================================================

(test-case "render-session-tree: single root entry"
  (define entries (list (list "id1" #f 'user "hello world")))
  (define lines (render-session-tree entries "id1" 80))
  (check-equal? (length lines) 1)
  (check-true (string-contains? (car lines) "hello world"))
  (check-true (string-contains? (car lines) "[user]"))
  ;; Active leaf marker
  (check-true (string-contains? (car lines) "◄")))

(test-case "render-session-tree: single root without active marker"
  (define entries (list (list "id1" #f 'assistant "done")))
  (define lines (render-session-tree entries "other-id" 80))
  (check-equal? (length lines) 1)
  (check-false (string-contains? (car lines) "◄")))

;; ============================================================
;; render-session-tree — root with children
;; ============================================================

(test-case "render-session-tree: root with two children"
  (define entries
    (list (list "root" #f 'user "start")
          (list "c1" "root" 'assistant "response 1")
          (list "c2" "root" 'assistant "response 2")))
  (define lines (render-session-tree entries "c2" 80))
  (check-equal? (length lines) 3)
  ;; First line is root
  (check-true (string-contains? (first lines) "start"))
  ;; Second child uses └── (is-last)
  (check-true (string-contains? (third lines) "└──"))
  ;; First child uses ├── (not is-last)
  (check-true (string-contains? (second lines) "├──"))
  ;; Active marker on last child
  (check-true (string-contains? (third lines) "◄")))

;; ============================================================
;; render-session-tree — multiple roots
;; ============================================================

(test-case "render-session-tree: two root entries"
  (define entries (list (list "r1" #f 'user "first root") (list "r2" #f 'user "second root")))
  (define lines (render-session-tree entries #f 80))
  (check-equal? (length lines) 2)
  ;; Last root uses └──
  (check-true (string-contains? (second lines) "└──"))
  ;; First root uses ├──
  (check-true (string-contains? (first lines) "├──")))

;; ============================================================
;; render-session-tree — deeply nested tree
;; ============================================================

(test-case "render-session-tree: deeply nested 3 levels"
  (define entries
    (list (list "root" #f 'user "root")
          (list "child" "root" 'assistant "child")
          (list "grandchild" "child" 'user "grandchild")))
  (define lines (render-session-tree entries #f 80))
  (check-equal? (length lines) 3)
  ;; All single-child: each uses └── with increasing indent
  (check-true (string-contains? (first lines) "root"))
  (check-true (string-contains? (second lines) "child"))
  (check-true (string-contains? (third lines) "grandchild"))
  ;; Deeper lines have more indentation
  (check-true (< (string-length (first lines)) (string-length (third lines)))))

;; ============================================================
;; render-session-tree — truncation of long text
;; ============================================================

(test-case "render-session-tree: truncates long text at narrow width"
  (define long-text (make-string 200 #\X))
  (define entries (list (list "id1" #f 'user long-text)))
  (define lines (render-session-tree entries #f 40))
  (check-equal? (length lines) 1)
  ;; Line should be truncated (contains ... and shorter than full text)
  (check-true (string-contains? (car lines) "...") "long text should be truncated with ellipsis"))

;; ============================================================
;; build-tree-nodes from messages
;; ============================================================

(test-case "build-tree-nodes: extracts id, parent-id, role, text from messages"
  (define msgs
    (list (make-message "m1" #f 'user 'message (list (make-text-part "hello world")) 1001 (hasheq))))
  (define nodes (build-tree-nodes msgs))
  (check-equal? (length nodes) 1)
  (check-equal? (first (first nodes)) "m1") ; id
  (check-equal? (second (first nodes)) #f) ; parent-id
  (check-equal? (third (first nodes)) 'user) ; role
  (check-true (string-contains? (fourth (first nodes)) "hello world")))

(test-case "build-tree-nodes: truncates text over 80 chars"
  (define long-text (make-string 100 #\A))
  (define msgs
    (list (make-message "m1" #f 'user 'message (list (make-text-part long-text)) 1001 (hasheq))))
  (define nodes (build-tree-nodes msgs))
  (define snapshot (fourth (first nodes)))
  (check-true (string-contains? snapshot "..."))
  (check-true (<= (string-length snapshot) 80)))

(test-case "build-tree-nodes: empty content parts produce empty text"
  (define msgs (list (make-message "m1" #f 'user 'message '() 1001 (hasheq))))
  (define nodes (build-tree-nodes msgs))
  (check-equal? (fourth (first nodes)) ""))

;; ============================================================
;; selected-node
;; ============================================================

(test-case "selected-node returns node at valid index"
  (define nodes (list (list "n1" #f 'user "a") (list "n2" #f 'user "b") (list "n3" #f 'user "c")))
  (check-equal? (selected-node nodes 0) (first nodes))
  (check-equal? (selected-node nodes 2) (third nodes)))

(test-case "selected-node returns #f for out-of-range index"
  (define nodes (list (list "n1" #f 'user "a")))
  (check-false (selected-node nodes -1))
  (check-false (selected-node nodes 1))
  (check-false (selected-node nodes 100)))
