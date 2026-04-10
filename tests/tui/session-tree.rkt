#lang racket

;; tests/tui/session-tree.rkt — TDD tests for session-tree module

(require rackunit
         racket/string
         racket/set
         "../../tui/render.rkt"
         "../../tui/session-tree.rkt")

;; ============================================================
;; Test scaffolding — build tree-node instances directly
;; ============================================================

(define root-node
  (tree-node "root" #f "user" 0 #f #f '() "Hello"))

(define child-1
  (tree-node "c1" "root" "assistant" 1 #f #f '() "Response 1"))

(define child-2
  (tree-node "c2" "root" "assistant" 1 #t #f '() "Response 2"))

(define grandchild-1
  (tree-node "gc1" "c1" "tool" 2 #t #f '() "Tool result"))

(define tree-with-children
  (list (struct-copy tree-node root-node
                     [is-leaf #f]
                     [children (list (struct-copy tree-node child-1
                                                  [is-leaf #f]
                                                  [children (list grandchild-1)])
                                     child-2)])))

(define single-node-tree
  (list root-node))

;; ============================================================
;; tree-node struct
;; ============================================================

(check-true (tree-node? root-node)
            "tree-node: construction works")
(check-equal? (tree-node-id root-node) "root"
              "tree-node: id accessor")
(check-equal? (tree-node-parent-id root-node) #f
              "tree-node: parent-id accessor")
(check-equal? (tree-node-role root-node) "user"
              "tree-node: role accessor")
(check-equal? (tree-node-depth root-node) 0
              "tree-node: depth accessor")
(check-false (tree-node-is-leaf root-node)
             "tree-node: is-leaf accessor")
(check-false (tree-node-is-active root-node)
             "tree-node: is-active accessor")
(check-equal? (tree-node-children root-node) '()
              "tree-node: children accessor")
(check-equal? (tree-node-content-preview root-node) "Hello"
              "tree-node: content-preview accessor")

;; Transparency
(check-not-false (struct->vector root-node)
                 "tree-node: transparent (struct->vector works)")

;; ============================================================
;; content-preview
;; ============================================================

(check-equal? (content-preview "short") "short"
              "content-preview: short string returned as-is")
(check-equal? (content-preview "Hello, World!") "Hello, World!"
              "content-preview: short string with punctuation")

(define long-str (make-string 50 #\x))
(check-true (string-contains? (content-preview long-str) "...")
            "content-preview: long string truncated with ...")
(check-true (<= (string-length (content-preview long-str)) 44)
            "content-preview: truncated result <= max-len + 3")
(check-equal? (content-preview "abc" 2) "ab..."
              "content-preview: custom max-len")
(check-equal? (content-preview #f) "(empty)"
              "content-preview: #f returns (empty)")
(check-equal? (content-preview '()) "(empty)"
              "content-preview: empty list returns (empty)")
(check-equal? (content-preview (hasheq 'type "tool-call")) "[tool-call]"
              "content-preview: hash with type returns bracket notation")
(check-equal? (content-preview (hasheq 'foo "bar")) "[data]"
              "content-preview: hash without type returns [data]")
(check-equal? (content-preview (list 1 2 3)) "[multi-part]"
              "content-preview: non-empty non-content-part list returns [multi-part]")

;; ============================================================
;; build-session-tree
;; ============================================================

(check-equal? (build-session-tree #f) '()
              "build-session-tree: #f index returns empty list")
(check-equal? (build-session-tree #f 5) '()
              "build-session-tree: #f index with max-depth returns empty")

;; ============================================================
;; render-session-tree
;; ============================================================

(check-equal? (render-session-tree '() 80) '()
              "render-session-tree: empty tree returns empty list")

(let ([lines (render-session-tree single-node-tree 80)])
  (check-equal? (length lines) 1
                "render-session-tree: single node produces one line")
  (check-true (styled-line? (car lines))
              "render-session-tree: returns styled-lines"))

(let ([lines (render-session-tree tree-with-children 80)])
  (check-true (>= (length lines) 3)
              "render-session-tree: tree with children has multiple lines")
  ;; Root + 2 children + grandchild = 4 lines
  (check-equal? (length lines) 4
                "render-session-tree: root + 2 children + 1 grandchild = 4 lines"))

;; ============================================================
;; render-tree-node
;; ============================================================

(let ([lines (render-tree-node root-node "" #t 80)])
  (check-true (styled-line? (car lines))
              "render-tree-node: produces styled-line")
  ;; Check that role text appears in the rendered line
  (define text (styled-line->text (car lines)))
  (check-true (string-contains? text "user")
              "render-tree-node: contains role text")
  (check-true (string-contains? text "Hello")
              "render-tree-node: contains content-preview"))

(let ([lines (render-tree-node child-1 "" #f 80)])
  (define text (styled-line->text (car lines)))
  (check-true (string-contains? text "├──")
              "render-tree-node: non-last child uses ├")
  (check-true (string-contains? text "assistant")
              "render-tree-node: assistant role shown"))

(let ([lines (render-tree-node child-2 "" #t 80)])
  (define text (styled-line->text (car lines)))
  (check-true (string-contains? text "└──")
              "render-tree-node: last child uses └"))

;; Active node highlighting
(define active-root (struct-copy tree-node root-node [is-active #t]))
(let ([lines (render-tree-node active-root "" #t 80)])
  (define segs (styled-line-segments (car lines)))
  ;; At least one segment should have 'inverse style
  (check-not-false (for/or ([seg (in-list segs)])
                (member 'inverse (styled-segment-style seg)))
              "render-tree-node: active node has inverse style"))

;; Root node has no connector
(let ([lines (render-tree-node root-node "" #t 80)])
  (define text (styled-line->text (car lines)))
  (check-false (string-contains? text "├──")
               "render-tree-node: root has no ├ connector")
  (check-false (string-contains? text "└──")
               "render-tree-node: root has no └ connector"))

;; ============================================================
;; nav-state struct
;; ============================================================

(define empty-nav (nav-state '() (set) #f))
(check-true (nav-state? empty-nav)
            "nav-state: construction works")
(check-equal? (nav-state-tree empty-nav) '()
              "nav-state: tree accessor")
(check-true (set-empty? (nav-state-expanded empty-nav))
            "nav-state: expanded accessor")
(check-equal? (nav-state-cursor-id empty-nav) #f
              "nav-state: cursor-id accessor")

;; ============================================================
;; make-nav-state
;; ============================================================

(let ([ns (make-nav-state '())])
  (check-equal? (nav-state-tree ns) '()
                "make-nav-state: empty tree preserved")
  (check-false (nav-state-cursor-id ns)
               "make-nav-state: empty tree has #f cursor"))

(let ([ns (make-nav-state single-node-tree)])
  (check-equal? (nav-state-cursor-id ns) "root"
                "make-nav-state: cursor on first node")
  (check-true (set-member? (nav-state-expanded ns) "root")
              "make-nav-state: root is expanded"))

(let ([ns (make-nav-state tree-with-children)])
  (check-equal? (nav-state-cursor-id ns) "root"
                "make-nav-state: cursor on root of multi-node tree")
  ;; All nodes should be expanded by default
  (check-true (set-member? (nav-state-expanded ns) "root")
              "make-nav-state: root expanded in multi-node tree")
  (check-true (set-member? (nav-state-expanded ns) "c1")
              "make-nav-state: children expanded by default"))

;; ============================================================
;; visible-nodes
;; ============================================================

(check-equal? (visible-nodes empty-nav) '()
              "visible-nodes: empty tree returns empty list")

(let ([ns (make-nav-state single-node-tree)])
  (check-equal? (length (visible-nodes ns)) 1
                "visible-nodes: single node tree shows 1 node")
  (check-equal? (tree-node-id (car (visible-nodes ns))) "root"
                "visible-nodes: root is visible"))

(let ([ns (make-nav-state tree-with-children)])
  ;; Root + c1 + gc1 + c2 = 4 (all expanded)
  (check-equal? (length (visible-nodes ns)) 4
                "visible-nodes: all expanded shows all 4 nodes"))

;; Test collapsed state — empty expanded set means root not expanded, shows only itself
(let* ([ns (make-nav-state tree-with-children)]
       [collapsed (struct-copy nav-state ns [expanded (set)])])
  (check-equal? (length (visible-nodes collapsed)) 1
                "visible-nodes: collapsed (empty set) shows only root"))

(let* ([ns (make-nav-state tree-with-children)]
       ;; Expand root only (not c1) — root + c1 + c2
       [partial (struct-copy nav-state ns [expanded (set "root" "c1")])])
  ;; root + c1 + gc1 + c2 = 4 (c1 expanded shows gc1)
  (check-equal? (length (visible-nodes partial)) 4
                "visible-nodes: root + c1 expanded shows 4"))

(let* ([ns (make-nav-state tree-with-children)]
       ;; Expand root but not c1 — root + c1 + c2
       [partial (struct-copy nav-state ns [expanded (set "root")])])
  ;; root + c1 + c2 = 3 (c1 collapsed, so gc1 hidden)
  ;; Wait, root expanded means root's children are shown, but c1 not expanded
  ;; means c1's children (gc1) are hidden. So root + c1 + c2 = 3.
  (check-equal? (length (visible-nodes partial)) 3
                "visible-nodes: root expanded, c1 collapsed shows 3"))

;; ============================================================
;; nav-current-node
;; ============================================================

(check-false (nav-current-node empty-nav)
             "nav-current-node: #f for empty tree")

(let ([ns (make-nav-state single-node-tree)])
  (check-not-false (nav-current-node ns)
                   "nav-current-node: returns node for single-node tree")
  (check-equal? (tree-node-id (nav-current-node ns)) "root"
                "nav-current-node: returns correct node"))

;; ============================================================
;; nav-up / nav-down
;; ============================================================

;; On empty tree, returns same state
(check-equal? (nav-up empty-nav) empty-nav
              "nav-up: empty tree returns same state")
(check-equal? (nav-down empty-nav) empty-nav
              "nav-down: empty tree returns same state")

;; On single-node tree, stays same
(let ([ns (make-nav-state single-node-tree)])
  (check-equal? (nav-state-cursor-id (nav-up ns)) "root"
                "nav-up: single node stays on root")
  (check-equal? (nav-state-cursor-id (nav-down ns)) "root"
                "nav-down: single node stays on root"))

;; Multi-node navigation
(let* ([ns (make-nav-state tree-with-children)]
       ;; cursor starts on root, move down
       [down1 (nav-down ns)])
  (check-equal? (nav-state-cursor-id down1) "c1"
                "nav-down: moves from root to c1")
  ;; Move down again
  (define down2 (nav-down down1))
  (check-equal? (nav-state-cursor-id down2) "gc1"
                "nav-down: moves from c1 to gc1")
  ;; Move down again
  (define down3 (nav-down down2))
  (check-equal? (nav-state-cursor-id down3) "c2"
                "nav-down: moves from gc1 to c2")
  ;; At bottom, stays
  (define down4 (nav-down down3))
  (check-equal? (nav-state-cursor-id down4) "c2"
                "nav-down: stays at bottom"))

(let* ([ns (make-nav-state tree-with-children)]
       ;; Move to bottom first
       [at-bottom (nav-down (nav-down (nav-down ns)))]
       ;; Now move up
       [up1 (nav-up at-bottom)])
  (check-equal? (nav-state-cursor-id up1) "gc1"
                "nav-up: moves from c2 to gc1")
  (define up2 (nav-up up1))
  (check-equal? (nav-state-cursor-id up2) "c1"
                "nav-up: moves from gc1 to c1")
  ;; At top stays
  (define at-top (nav-up (nav-up up2)))
  (check-equal? (nav-state-cursor-id at-top) "root"
                "nav-up: stays at top"))

;; ============================================================
;; nav-toggle-expand
;; ============================================================

;; Toggle on leaf returns same state
(define leaf-root (struct-copy tree-node root-node [is-leaf #t]))
(define leaf-tree (list leaf-root))
(let ([ns (make-nav-state leaf-tree)])
  (check-true (tree-node-is-leaf (nav-current-node ns))
              "nav-toggle-expand: single node is leaf")
  (check-equal? (nav-state-expanded (nav-toggle-expand ns))
                (nav-state-expanded ns)
                "nav-toggle-expand: leaf node returns same expanded set"))

;; Toggle on expanded node collapses it
(let* ([ns (make-nav-state tree-with-children)]
       ;; Move cursor to c1 (which has children)
       [at-c1 (nav-down ns)]
       [toggled (nav-toggle-expand at-c1)])
  (check-false (set-member? (nav-state-expanded toggled) "c1")
               "nav-toggle-expand: collapsed c1"))

;; Toggle again re-expands
(let* ([ns (make-nav-state tree-with-children)]
       [at-c1 (nav-down ns)]
       [collapsed (nav-toggle-expand at-c1)]
       [re-expanded (nav-toggle-expand collapsed)])
  (check-true (set-member? (nav-state-expanded re-expanded) "c1")
              "nav-toggle-expand: re-expanded c1"))

;; Toggle on #f cursor returns same
(check-equal? (nav-toggle-expand empty-nav) empty-nav
              "nav-toggle-expand: #f cursor returns same state")

;; ============================================================
;; nav-expand-all
;; ============================================================

(let* ([ns (make-nav-state tree-with-children)]
       ;; Collapse everything first
       [collapsed (struct-copy nav-state ns [expanded (set)])]
       [expanded (nav-expand-all collapsed)])
  (check-true (set-member? (nav-state-expanded expanded) "root")
              "nav-expand-all: root expanded")
  (check-true (set-member? (nav-state-expanded expanded) "c1")
              "nav-expand-all: c1 expanded")
  (check-true (set-member? (nav-state-expanded expanded) "c2")
              "nav-expand-all: c2 expanded")
  (check-true (set-member? (nav-state-expanded expanded) "gc1")
              "nav-expand-all: gc1 expanded"))

;; ============================================================
;; nav-collapse-all
;; ============================================================

(let* ([ns (make-nav-state tree-with-children)]
       [collapsed (nav-collapse-all ns)])
  (check-true (set-member? (nav-state-expanded collapsed) "root")
              "nav-collapse-all: root still expanded")
  (check-false (set-member? (nav-state-expanded collapsed) "c1")
               "nav-collapse-all: c1 collapsed")
  (check-false (set-member? (nav-state-expanded collapsed) "gc1")
               "nav-collapse-all: gc1 collapsed"))

;; ============================================================
;; render-nav-tree
;; ============================================================

(check-equal? (render-nav-tree empty-nav 80) '()
              "render-nav-tree: empty tree returns empty list")

(let ([ns (make-nav-state single-node-tree)])
  (define lines (render-nav-tree ns 80))
  (check-equal? (length lines) 1
                "render-nav-tree: single node produces one line"))

(let ([ns (make-nav-state tree-with-children)])
  (define lines (render-nav-tree ns 80))
  (check-equal? (length lines) 4
                "render-nav-tree: full tree produces 4 lines")
  ;; The cursor line should have inverse style
  (define first-line (car lines))
  (define segs (styled-line-segments first-line))
  (check-not-false (for/or ([seg (in-list segs)])
                (member 'inverse (styled-segment-style seg)))
              "render-nav-tree: cursor node has inverse highlighting"))

;; Collapsed tree renders fewer lines
(let* ([ns (make-nav-state tree-with-children)]
       [collapsed (struct-copy nav-state ns [expanded (set "root")])])
  (define lines (render-nav-tree collapsed 80))
  (check-equal? (length lines) 3
                "render-nav-tree: collapsed c1 shows 3 lines (root+c1+c2, no gc1)"))

;; ============================================================
;; Role color mapping
;; ============================================================

(define tool-node (tree-node "t1" "root" "tool" 1 #t #f '() "Tool output"))
(let ([lines (render-tree-node tool-node "" #t 80)])
  (define segs (styled-line-segments (car lines)))
  ;; Find segment with role text — should have 'magenta
  (define role-seg (findf (lambda (s)
                            (string-contains? (styled-segment-text s) "tool"))
                          segs))
  (check-not-false role-seg "role-color: tool role segment exists")
  (when role-seg
    (check-not-false (member 'magenta (styled-segment-style role-seg))
                "role-color: tool has magenta style")))

(define sys-node (tree-node "s1" #f "system" 0 #t #f '() "System msg"))
(let ([lines (render-tree-node sys-node "" #t 80)])
  (define segs (styled-line-segments (car lines)))
  (define role-seg (findf (lambda (s)
                            (string-contains? (styled-segment-text s) "system"))
                          segs))
  (check-not-false role-seg "role-color: system role segment exists")
  (when role-seg
    (check-not-false (member 'yellow (styled-segment-style role-seg))
                "role-color: system has yellow style")))

;; ============================================================
;; build-tree-from-node
;; ============================================================

(check-equal? (build-tree-from-node #f "anything") '()
              "build-tree-from-node: #f index returns empty")
(check-equal? (build-tree-from-node #f "anything" 5) '()
              "build-tree-from-node: #f index with max-depth returns empty")

;; ============================================================
;; Edge cases
;; ============================================================

;; content-preview with exactly 40 chars
(check-equal? (content-preview (make-string 40 #\a))
              (make-string 40 #\a)
              "content-preview: exactly 40 chars not truncated")

;; content-preview with 41 chars
(check-equal? (content-preview (make-string 41 #\a))
              (string-append (make-string 40 #\a) "...")
              "content-preview: 41 chars truncated to 40 + ...")

;; nav operations preserve cursor when no movement possible
(let ([ns (make-nav-state single-node-tree)])
  (check-equal? (nav-state-cursor-id (nav-up (nav-down ns))) "root"
                "edge: up then down on single node preserves cursor"))

;; Empty tree operations don't crash
(check-equal? (nav-expand-all empty-nav) (struct-copy nav-state empty-nav [expanded (set)])
              "edge: expand-all on empty tree")

(let ([collapsed (nav-collapse-all empty-nav)])
  (check-true (set-empty? (nav-state-expanded collapsed))
              "edge: collapse-all on empty tree"))

;; Deep tree rendering
(define deep-node
  (tree-node "d3" "d2" "user" 3 #t #f '() "Deep"))
(define d2-node
  (tree-node "d2" "d1" "assistant" 2 #f #f (list deep-node) "Mid2"))
(define d1-node
  (tree-node "d1" "root" "user" 1 #f #f (list d2-node) "Mid1"))
(define deep-tree
  (list (struct-copy tree-node root-node
                     [is-leaf #f]
                     [children (list d1-node)])))

(let ([lines (render-session-tree deep-tree 80)])
  (check-equal? (length lines) 4
                "deep-tree: 4 levels = 4 lines"))

(let ([ns (make-nav-state deep-tree)])
  (define vis (visible-nodes ns))
  (check-equal? (length vis) 4
                "deep-tree: all 4 nodes visible when expanded"))
