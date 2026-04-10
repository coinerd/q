#lang racket/base

;; tui/session-tree.rkt — ASCII tree renderer with navigation support
;;
;; Renders session-index trees as ASCII art with box-drawing characters
;; (│ ├ └ ─) and provides keyboard-driven navigation state.

(require racket/string
         racket/list
         racket/set
         "render.rkt"
         "../runtime/session-index.rkt"
         "../agent/types.rkt")

(provide
 (struct-out tree-node)
 (struct-out nav-state)
 build-session-tree
 build-tree-from-node
 render-session-tree
 render-tree-node
 make-nav-state
 nav-up
 nav-down
 nav-toggle-expand
 nav-expand-all
 nav-collapse-all
 visible-nodes
 nav-current-node
 render-nav-tree
 content-preview)

;; ============================================================
;; Tree Node Structure
;; ============================================================

;; A displayable tree node with rendering metadata
(struct tree-node
  (id              ; string — message-id
   parent-id       ; (or/c string? #f)
   role            ; string — "user"/"assistant"/"system"/"tool"
   depth           ; non-negative-integer — nesting level (0 = root)
   is-leaf         ; boolean — has no children
   is-active       ; boolean — currently selected/highlighted
   children        ; (listof tree-node)
   content-preview ; string — first ~40 chars of content
   )
  #:transparent)

;; ============================================================
;; Navigation State
;; ============================================================

;; Navigation state for arrow-key tree browsing
(struct nav-state
  (tree          ; (listof tree-node) — full tree
   expanded      ; (set/c string?) — set of expanded node IDs
   cursor-id     ; (or/c string? #f) — currently highlighted node
   )
  #:transparent)

;; ============================================================
;; Content Preview
;; ============================================================

(define (content-preview msg [max-len 40])
  ;; Extract first N chars of message content for display.
  ;; msg can be a message struct, #f, a string, a hash, or a list.
  (define raw-content
    (cond
      [(message? msg) (message-content msg)]
      [else msg]))
  (define text
    (cond
      [(not raw-content) "(empty)"]
      [(string? raw-content) raw-content]
      [(null? raw-content) "(empty)"]
      [(list? raw-content)
       ;; list of content parts — extract text from text-parts
       (define texts
         (for/list ([part (in-list raw-content)]
                    #:when (text-part? part))
           (text-part-text part)))
       (if (null? texts)
           (if (null? raw-content) "(empty)" "[multi-part]")
           (string-join texts " "))]
      [(hash? raw-content)
       (cond
         [(hash-has-key? raw-content 'type)
          (format "[~a]" (hash-ref raw-content 'type))]
         [else "[data]"])]
      [else (format "~a" raw-content)]))
  (if (> (string-length text) max-len)
      (string-append (substring text 0 max-len) "...")
      text))

;; ============================================================
;; Tree Construction
;; ============================================================

(define (build-session-tree idx [max-depth 0])
  ;; Build a tree from session-index starting from root nodes.
  ;; Returns (listof tree-node) — top-level nodes (usually just one root).
  ;; max-depth: limit tree depth (0 = unlimited).
  (if (not idx)
      '()
      (let ()
        (define by-id (session-index-by-id idx))
        ;; Find root nodes: those with parent-id = #f
        (define roots
          (for/list ([msg (in-hash-values by-id)]
                     #:when (not (message-parent-id msg)))
            msg))
        ;; Sort roots by timestamp
        (define sorted-roots
          (sort roots string<? #:key message-timestamp))
        (for/list ([root (in-list sorted-roots)])
          (build-node idx root 0 max-depth)))))

(define (build-tree-from-node idx node-id [max-depth 0])
  ;; Build tree from a specific node.
  ;; Returns (listof tree-node) with the specified node as root.
  (if (not idx)
      '()
      (let ()
        (define msg (lookup-entry idx node-id))
        (if (not msg)
            '()
            (list (build-node idx msg 0 max-depth))))))

(define (build-node idx msg depth max-depth)
  ;; Recursively build a tree-node from a message.
  (define id (message-id msg))
  (define kids (children-of idx id))
  (define is-leaf (null? kids))
  (define preview (content-preview msg))
  (define at-limit (and (> max-depth 0) (>= depth max-depth)))
  (define child-nodes
    (if at-limit
        '()
        (for/list ([child (in-list kids)])
          (build-node idx child (add1 depth) max-depth))))
  (tree-node id
             (message-parent-id msg)
             (symbol->string (message-role msg))
             depth
             is-leaf
             #f
             child-nodes
             preview))

;; ============================================================
;; ASCII Rendering
;; ============================================================

(define (render-session-tree nodes cols)
  ;; Render tree as ASCII lines.
  ;; Returns (listof styled-line?) — one line per visible node.
  ;; Uses box-drawing characters: │ ├ └ ─
  (if (null? nodes)
      '()
      (let ()
        (define-values (lines _)
          (for/fold ([acc '()] [idx 0])
                    ([node (in-list nodes)])
            (define is-last (>= (add1 idx) (length nodes)))
            (define rendered (render-tree-node node "" is-last cols))
            (values (append acc rendered) (add1 idx))))
        lines)))

(define (render-node-line node prefix is-last)
  ;; Render a single tree node as one styled-line (no children).
  ;; Returns styled-line (not a list).
  (define connector
    (cond
      [(= (tree-node-depth node) 0) ""]
      [is-last "└── "]
      [else    "├── "]))
  (define role (tree-node-role node))
  (define role-style
    (cond
      [(string=? role "user")      '(green bold)]
      [(string=? role "assistant") '(blue bold)]
      [(string=? role "system")    '(yellow bold)]
      [(string=? role "tool")      '(magenta bold)]
      [else                        '()]))
  (define active-style (if (tree-node-is-active node) '(inverse) '()))
  (define prefix-seg (styled-segment prefix '(dim)))
  (define connector-seg (styled-segment connector '(dim)))
  (define role-text (format "[~a] " role))
  (define role-seg (styled-segment role-text (append active-style role-style)))
  (define preview-text (tree-node-content-preview node))
  (define preview-seg (styled-segment preview-text active-style))
  (styled-line (list prefix-seg connector-seg role-seg preview-seg)))

(define (render-tree-node node prefix is-last cols)
  ;; Render a single tree node line plus all children recursively.
  ;; prefix: string of │ characters for indentation
  ;; is-last: whether this is the last child (affects └ vs ├)
  ;; Returns (listof styled-line) — this node's line + children's lines.
  (define line (render-node-line node prefix is-last))
  ;; Render children
  (define children (tree-node-children node))
  (define child-prefix
    (cond
      [(= (tree-node-depth node) 0) ""]
      [is-last (string-append prefix "    ")]
      [else    (string-append prefix "│   ")]))
  (define child-lines
    (if (null? children)
        '()
        (let ()
          (define-values (clines _)
            (for/fold ([acc '()] [idx 0])
                      ([child (in-list children)])
              (define child-is-last (>= (add1 idx) (length children)))
              (define rendered (render-tree-node child child-prefix child-is-last cols))
              (values (append acc rendered) (add1 idx))))
          clines)))
  (cons line child-lines))

;; ============================================================
;; Navigation
;; ============================================================

(define (make-nav-state tree)
  ;; Create initial nav state — root expanded, cursor on first node.
  (define all-ids (collect-all-ids tree))
  (define expanded (apply set all-ids))
  (define cursor-id
    (if (null? tree)
        #f
        (tree-node-id (car tree))))
  (nav-state tree expanded cursor-id))

(define (collect-all-ids nodes)
  ;; Collect all node IDs in the tree recursively.
  (apply append
         (for/list ([node (in-list nodes)])
           (cons (tree-node-id node)
                 (collect-all-ids (tree-node-children node))))))

(define (visible-nodes ns)
  ;; Get visible nodes in order (respecting expand state).
  (define tree (nav-state-tree ns))
  (define expanded (nav-state-expanded ns))
  (collect-visible tree expanded))

(define (collect-visible nodes expanded)
  ;; Collect visible nodes recursively, respecting expand state.
  (apply append
         (for/list ([node (in-list nodes)])
           (if (set-member? expanded (tree-node-id node))
               (cons node (collect-visible (tree-node-children node) expanded))
               (list node)))))

(define (nav-current-node ns)
  ;; Get currently selected node. Returns tree-node or #f.
  (define cursor-id (nav-state-cursor-id ns))
  (if (not cursor-id)
      #f
      (find-node-by-id (nav-state-tree ns) cursor-id)))

(define (find-node-by-id nodes id)
  ;; Find a node by ID in the tree.
  (for/or ([node (in-list nodes)])
    (cond
      [(string=? (tree-node-id node) id) node]
      [else (find-node-by-id (tree-node-children node) id)])))

(define (nav-up ns)
  ;; Move cursor to previous visible node.
  (define vis (visible-nodes ns))
  (define cursor-id (nav-state-cursor-id ns))
  (cond
    [(null? vis) ns]
    [(not cursor-id) ns]
    [else
     (define idx (index-of-node vis cursor-id))
     (cond
       [(not idx) ns]
       [(= idx 0) ns]  ; already at top
       [else
        (define prev-node (list-ref vis (sub1 idx)))
        (struct-copy nav-state ns [cursor-id (tree-node-id prev-node)])])]))

(define (nav-down ns)
  ;; Move cursor to next visible node.
  (define vis (visible-nodes ns))
  (define cursor-id (nav-state-cursor-id ns))
  (cond
    [(null? vis) ns]
    [(not cursor-id) ns]
    [else
     (define idx (index-of-node vis cursor-id))
     (cond
       [(not idx) ns]
       [(>= (add1 idx) (length vis)) ns]  ; already at bottom
       [else
        (define next-node (list-ref vis (add1 idx)))
        (struct-copy nav-state ns [cursor-id (tree-node-id next-node)])])]))

(define (index-of-node nodes id)
  ;; Find index of node with given id in list.
  (for/or ([node (in-list nodes)]
           [i (in-naturals)])
    (and (string=? (tree-node-id node) id) i)))

(define (nav-toggle-expand ns)
  ;; Toggle expand/collapse of cursor node.
  (define cursor-id (nav-state-cursor-id ns))
  (cond
    [(not cursor-id) ns]
    [else
     (define node (nav-current-node ns))
     (cond
       [(not node) ns]
       [(tree-node-is-leaf node) ns]  ; leaf nodes can't be toggled
       [else
        (define expanded (nav-state-expanded ns))
        (define new-expanded
          (if (set-member? expanded cursor-id)
              (set-remove expanded cursor-id)
              (set-add expanded cursor-id)))
        (struct-copy nav-state ns [expanded new-expanded])])]))

(define (nav-expand-all ns)
  ;; Expand all nodes.
  (define all-ids (collect-all-ids (nav-state-tree ns)))
  (struct-copy nav-state ns [expanded (apply set all-ids)]))

(define (nav-collapse-all ns)
  ;; Collapse all except root-level nodes.
  (define tree (nav-state-tree ns))
  (define root-ids (for/list ([node (in-list tree)])
                     (tree-node-id node)))
  (struct-copy nav-state ns [expanded (apply set root-ids)]))

;; ============================================================
;; Render Navigation View
;; ============================================================

(define (render-nav-tree ns cols)
  ;; Render the tree for navigation mode.
  ;; Like render-session-tree but respects expand state and highlights cursor.
  (define tree (nav-state-tree ns))
  (define cursor-id (nav-state-cursor-id ns))
  (define expanded (nav-state-expanded ns))
  (if (null? tree)
      '()
      (render-nav-nodes tree "" expanded cursor-id cols #t)))

(define (render-nav-nodes nodes prefix expanded cursor-id cols is-top-level)
  ;; Render navigation nodes recursively.
  (if (null? nodes)
      '()
      (let ()
        (define-values (lines _)
          (for/fold ([acc '()] [idx 0])
                    ([node (in-list nodes)])
            (define is-last (>= (add1 idx) (length nodes)))
            (define is-cursor (and cursor-id
                                   (string=? (tree-node-id node) cursor-id)))
            ;; Create a copy with is-active set for cursor highlighting
            (define active-node (struct-copy tree-node node [is-active is-cursor]))
            (define node-line (render-node-line active-node prefix is-last))
            ;; If this node is expanded, render children
            (define is-expanded (set-member? expanded (tree-node-id node)))
            (define child-lines
              (if (and is-expanded (not (null? (tree-node-children node))))
                  (let ()
                    (define child-prefix
                      (cond
                        [(= (tree-node-depth node) 0) ""]
                        [is-last (string-append prefix "    ")]
                        [else    (string-append prefix "│   ")]))
                    (render-nav-nodes (tree-node-children node)
                                      child-prefix
                                      expanded
                                      cursor-id
                                      cols
                                      #f))
                  '()))
            (values (append acc (cons node-line child-lines)) (add1 idx))))
        lines)))
