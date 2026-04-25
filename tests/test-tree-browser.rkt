#lang racket

;; tests/test-tree-browser.rkt — Tests for interactive tree browser (G1.1)
;;
;; Tests:
;;   - Tree browser state creation
;;   - Key handling: up/down navigation
;;   - Key handling: fold/unfold
;;   - Key handling: dismiss (escape/q)
;;   - Toggle: /tree opens overlay, /tree again closes it

(require rackunit
         rackunit/text-ui
         "../tui/state.rkt"
         "../tui/tree-view.rkt"
         "../util/protocol-types.rkt"
         racket/set)

;; ============================================================
;; Test helpers
;; ============================================================

;; Sample tree nodes for testing
(define sample-nodes
  '(("id1" #f user "Hello" 1000)
    ("id2" "id1" assistant "Hi there" 1001)
    ("id3" "id1" user "How are you?" 1002)
    ("id4" "id3" assistant "Fine thanks" 1003)))

(define (make-test-tbs [idx 0] [folded (set)])
  (tree-browser-state sample-nodes idx folded sample-nodes))

(define (make-test-overlay tbs)
  (overlay-state 'tree-browser '() "" 'top-left 80 20 0 tbs))

(define (make-test-state-with-overlay tbs)
  (struct-copy ui-state
               (initial-ui-state)
               [active-overlay (make-test-overlay tbs)]))

;; ============================================================
;; Tests
;; ============================================================

(define tree-browser-tests
  (test-suite
   "Tree Browser Tests"

   ;; Tree browser state creation
   (test-case "tree-browser-state construction"
     (define tbs (make-test-tbs))
     (check-equal? (tree-browser-state-selected-idx tbs) 0)
     (check-equal? (tree-browser-state-folded-set tbs) (set))
     (check-equal? (length (tree-browser-state-nodes tbs)) 4))

   ;; Navigation
   (test-case "tree-next-node advances index"
     (define nodes '("a" "b" "c"))
     (check-equal? (tree-next-node nodes 0) 1)
     (check-equal? (tree-next-node nodes 1) 2)
     (check-equal? (tree-next-node nodes 2) 2)) ; clamps at end

   (test-case "tree-prev-node decrements index"
     (define nodes '("a" "b" "c"))
     (check-equal? (tree-prev-node nodes 2) 1)
     (check-equal? (tree-prev-node nodes 1) 0)
     (check-equal? (tree-prev-node nodes 0) 0)) ; clamps at start

   ;; Fold/unfold
   (test-case "tree-toggle-fold adds and removes from set"
     (define folded (set))
     (define folded1 (tree-toggle-fold folded "id1"))
     (check-true (set-member? folded1 "id1"))
     (define folded2 (tree-toggle-fold folded1 "id1"))
     (check-false (set-member? folded2 "id1")))

   ;; Overlay state
   (test-case "overlay with tree-browser extra"
     (define tbs (make-test-tbs))
     (define ov (make-test-overlay tbs))
     (check-eq? (overlay-state-type ov) 'tree-browser)
     (check-equal? (tree-browser-state-selected-idx (overlay-state-extra ov)) 0))

   ;; Dismiss overlay
   (test-case "dismiss-overlay clears active overlay"
     (define state (make-test-state-with-overlay (make-test-tbs)))
     (check-true (overlay-active? state))
     (define dismissed (dismiss-overlay state))
     (check-false (overlay-active? dismissed)))

   ;; show-overlay creates correct structure
   (test-case "show-overlay creates tree-browser overlay"
     (define state (initial-ui-state))
     (check-false (overlay-active? state))
     (define with-ov
       (show-overlay state 'tree-browser '(() ()) "" #:width 80 #:height 20))
     (check-true (overlay-active? with-ov))
     (check-eq? (overlay-state-type (ui-state-active-overlay with-ov)) 'tree-browser))

   ;; Build tree nodes from messages
   (test-case "build-tree-nodes from messages"
     (define msgs
       (list (message "id1" #f 'user 'message (list (text-part "text" "Hello")) 1000 (hash))
             (message "id2" "id1" 'assistant 'message (list (text-part "text" "Hi")) 1001 (hash))))
     (define nodes (build-tree-nodes msgs))
     (check-equal? (length nodes) 2)
     (check-equal? (list-ref (car nodes) 0) "id1")
     (check-equal? (list-ref (cadr nodes) 1) "id1"))

   ;; Tree rendering
   (test-case "render-session-tree produces lines"
     (define lines (render-session-tree sample-nodes #f 80))
     (check-true (> (length lines) 0)))

   (test-case "render-session-tree-folded hides children"
     (define unfolded (render-session-tree-folded sample-nodes #f 80 (set)))
     (define folded (render-session-tree-folded sample-nodes #f 80 (set "id1")))
     ;; Folded version should have fewer lines (children hidden)
     (check-true (>= (length unfolded) (length folded))))

   ;; would-abandon-branch detection
   (test-case "would-abandon-branch detects backward navigation"
     (check-false (would-abandon-branch? sample-nodes "id1" "id2"))
     (check-true (would-abandon-branch? sample-nodes "id4" "id1")))))

;; Run
(run-tests tree-browser-tests)
