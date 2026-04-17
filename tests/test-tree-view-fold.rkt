#lang racket

;;; tests/test-tree-view-fold.rkt — Tests for interactive tree selector
;;;
;;; Covers:
;;;   - Basic rendering (backward compat)
;;;   - Fold/unfold of branches
;;;   - Active-path highlighting
;;;   - Timestamp display
;;;   - Navigation helpers (next, prev, toggle-fold, enter)
;;;   - Branch abandonment detection

(require rackunit
         racket/set
         "../tui/tree-view.rkt")

;; ============================================================
;; Test data
;; ============================================================

;; Helper: create test entries (id parent-id role text [timestamp])
(define (test-entry id parent-id role text [ts 1000])
  (list id parent-id role text ts))

(define test-tree
  (list (test-entry "1" #f "user" "Hello")
        (test-entry "2" "1" "assistant" "Hi there")
        (test-entry "3" "2" "user" "Tell me more")
        (test-entry "4" "2" "user" "Something else") ; sibling branch
        (test-entry "5" "3" "assistant" "Sure...")))

;; ============================================================
;; Basic rendering
;; ============================================================

(test-case "render-session-tree basic rendering"
  (define lines (render-session-tree test-tree "5" 80))
  (check-true (>= (length lines) 3)))

;; ============================================================
;; Fold/unfold
;; ============================================================

(test-case "render-session-tree-folded collapses children"
  (define lines-all (render-session-tree test-tree "5" 80))
  (define lines-folded (render-session-tree-folded test-tree "5" 80 (set "2")))
  ;; When node "2" is folded, its children "3","4","5" should not appear
  (check-true (< (length lines-folded) (length lines-all))))

(test-case "render-session-tree-folded shows fold indicator"
  (define lines-folded (render-session-tree-folded test-tree "5" 80 (set "2")))
  ;; Folded node "2" should have ▸ indicator
  (check-true (for/or ([line lines-folded])
                (string-contains? line "▸"))))

(test-case "render-session-tree-folded shows unfold indicator"
  (define lines (render-session-tree-folded test-tree "5" 80 (set)))
  ;; Unfolded node "2" should have ▾ indicator
  (check-true (for/or ([line lines])
                (string-contains? line "▾"))))

(test-case "render-session-tree-folded with empty tree"
  (define lines (render-session-tree-folded '() #f 80 (set)))
  (check-equal? lines '()))

;; ============================================================
;; Navigation helpers
;; ============================================================

(test-case "tree-toggle-fold adds and removes from set"
  (define s1 (tree-toggle-fold (set) "node-1"))
  (check-true (set-member? s1 "node-1"))
  (define s2 (tree-toggle-fold s1 "node-1"))
  (check-false (set-member? s2 "node-1")))

(test-case "tree-next-node advances index"
  (define nodes (list "a" "b" "c"))
  (check-equal? (tree-next-node nodes 0) 1)
  (check-equal? (tree-next-node nodes 2) 2)) ; stays at end

(test-case "tree-prev-node decrements index"
  (define nodes (list "a" "b" "c"))
  (check-equal? (tree-prev-node nodes 1) 0)
  (check-equal? (tree-prev-node nodes 0) 0)) ; stays at start

(test-case "tree-enter-node unfolds folded node"
  (define entries (list (list "a" #f "user" "hi" 1000)))
  (define folded (set "a"))
  (define result (tree-enter-node entries 0 folded))
  (check-false (set-member? result "a")))

(test-case "tree-enter-node returns same set for unfolded node"
  (define entries (list (list "a" #f "user" "hi" 1000)))
  (define folded (set))
  (define result (tree-enter-node entries 0 folded))
  (check-equal? result folded))

(test-case "tree-enter-node handles out-of-range index"
  (define entries (list (list "a" #f "user" "hi" 1000)))
  (define folded (set "a"))
  (check-equal? (tree-enter-node entries -1 folded) folded)
  (check-equal? (tree-enter-node entries 5 folded) folded))

;; ============================================================
;; Active-path highlighting
;; ============================================================

(test-case "active-path highlighting marks ancestors"
  (define active-path (set "1" "2" "3" "5"))
  (define lines (render-session-tree test-tree "5" 80 #:active-path-ids active-path))
  ;; Active path nodes should have a marker
  (check-true (for/or ([line lines])
                (string-contains? line "*"))))

(test-case "active-path empty set produces no marker"
  (define lines (render-session-tree test-tree "5" 80 #:active-path-ids (set)))
  (check-false (for/or ([line lines])
                 (string-contains? line "*"))))

;; ============================================================
;; Timestamps
;; ============================================================

(test-case "render-session-tree with timestamps"
  (define lines (render-session-tree test-tree "5" 80 #:show-timestamps? #t))
  (check-true (for/or ([line lines])
                (regexp-match? #px"\\d{2}:\\d{2}" line))))

(test-case "render-session-tree without timestamps by default"
  (define lines (render-session-tree test-tree "5" 80))
  (check-false (for/or ([line lines])
                 (regexp-match? #px"\\d{2}:\\d{2}" line))))

;; ============================================================
;; Branch abandonment detection
;; ============================================================

(test-case "would-abandon-branch? detects branch abandonment"
  ;; Navigating from leaf "5" to sibling "4" abandons the "5" subtree
  (check-true (would-abandon-branch? test-tree "5" "4")))

(test-case "would-abandon-branch? returns #f for same node"
  (check-false (would-abandon-branch? test-tree "5" "5")))

(test-case "would-abandon-branch? returns #f when going deeper"
  ;; Navigating from "4" (depth 3) to "5" (depth 4) — going deeper
  (check-false (would-abandon-branch? test-tree "4" "5")))

(test-case "would-abandon-branch? returns #t when going to ancestor"
  ;; Navigating from "5" (depth 4) to "2" (depth 2) — going up
  (check-true (would-abandon-branch? test-tree "5" "2")))

(test-case "would-abandon-branch? returns #f for same-depth siblings"
  ;; "3" and "4" are siblings at same depth
  (check-false (would-abandon-branch? test-tree "3" "4")))
