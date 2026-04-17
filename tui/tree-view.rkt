#lang racket/base

;; q/tui/tree-view.rkt — Session tree rendering and navigation
;;
;; Renders the session tree as a depth-first text representation
;; with Unicode box-drawing characters (├ └ │ ─).
;;
;; Supports:
;;   - Fold/unfold of branches (render-session-tree-folded)
;;   - Active-path highlighting (* prefix)
;;   - Timestamp display (HH:MM)
;;   - Bookmark labels ([label])
;;   - Keyboard navigation helpers
;;   - Branch abandonment detection

(require racket/list
         racket/string
         racket/set
         "../util/protocol-types.rkt")

(provide render-session-tree
         render-session-tree-folded
         tree-node?
         tree-node-timestamp
         make-tree-node
         build-tree-nodes
         selected-node
         tree-next-node
         tree-prev-node
         tree-toggle-fold
         tree-enter-node
         would-abandon-branch?)

;; ============================================================
;; Tree node struct
;; ============================================================

;; Tree node for rendering, with optional timestamp
(struct tree-node (id role text depth children timestamp) #:transparent)

(define (make-tree-node id role text depth children [timestamp #f])
  (tree-node id role text depth children timestamp))

;; ============================================================
;; Entry helpers
;; ============================================================

;; Extract timestamp from flat entry (optional 5th element)
(define (entry-timestamp e)
  (if (>= (length e) 5)
      (list-ref e 4)
      #f))

;; Pad integer to 2 digits with leading zero
(define (pad2 n)
  (define s (number->string n))
  (if (< (string-length s) 2)
      (string-append "0" s)
      s))

;; Format a Unix timestamp as HH:MM
(define (format-timestamp ts)
  (cond
    [(not ts) ""]
    [(integer? ts)
     (define d (seconds->date ts #t))
     (format "~a:~a" (pad2 (date-hour d)) (pad2 (date-minute d)))]
    [else ""]))

;; Truncate text to fit within max-width columns
(define (truncate-text text max-width)
  (if (> (string-length text) max-width)
      (string-append (substring text 0 (max 0 (- max-width 3))) "...")
      text))

;; Build parent->children map and root list from flat entries
(define (build-children-map entries)
  (define children-map (make-hash))
  (define root-entries '())
  (for ([e (in-list entries)])
    (define pid (list-ref e 1))
    (if pid
        (hash-update! children-map pid (lambda (lst) (append lst (list e))) '())
        (set! root-entries (append root-entries (list e)))))
  (values children-map root-entries))

;; Build bookmark lookup: entry-id -> " [label]"
;; Accepts alist pairs: ((entry-id . label) ...)
(define (build-bookmark-map bookmarks)
  (define h (make-hash))
  (for ([bm (in-list bookmarks)])
    (hash-set! h (car bm) (format " [~a]" (cdr bm))))
  h)

;; ============================================================
;; Node line rendering
;; ============================================================

;; Render a single node as a text line with all features
(define (make-node-line entry
                        prefix
                        connector
                        active-leaf-id
                        active-path-ids
                        show-timestamps?
                        bookmark-map
                        max-width
                        fold-enabled?
                        folded-set
                        children-map)
  (define id (list-ref entry 0))
  (define role (list-ref entry 2))
  (define text (list-ref entry 3))
  (define ts (entry-timestamp entry))
  (define children (hash-ref children-map id '()))
  (define has-children (not (null? children)))
  (define is-folded (set-member? folded-set id))

  ;; Active leaf marker
  (define marker (if (equal? id active-leaf-id) " ◄" ""))

  ;; Active path marker (* for nodes on the active path)
  (define active-marker (if (set-member? active-path-ids id) "*" ""))

  ;; Fold indicator: only shown when fold mode is enabled and node has children
  (define fold-indicator
    (cond
      [(not fold-enabled?) ""]
      [(not has-children) ""]
      [is-folded "▸"]
      [else "▾"]))

  ;; Timestamp string
  (define ts-str
    (if show-timestamps?
        (let ([formatted (format-timestamp ts)])
          (if (string=? formatted "")
              ""
              (string-append formatted " ")))
        ""))

  ;; Bookmark label
  (define bm-str (hash-ref bookmark-map id ""))

  (define role-tag (format "[~a]" role))

  ;; Calculate available width for text
  (define extra-len
    (+ (string-length active-marker)
       (string-length fold-indicator)
       (string-length ts-str)
       (string-length bm-str)
       (string-length marker)))
  (define max-text-width
    (max 10
         (- max-width
            (string-length prefix)
            (string-length connector)
            (string-length role-tag)
            extra-len
            1)))
  (define truncated (truncate-text text max-text-width))

  (string-append prefix
                 connector
                 active-marker
                 fold-indicator
                 role-tag
                 " "
                 ts-str
                 truncated
                 bm-str
                 marker))

;; ============================================================
;; Internal rendering engine
;; ============================================================

;; Shared rendering logic used by both render-session-tree and
;; render-session-tree-folded.
(define (render-session-tree-internal entries
                                      active-leaf-id
                                      max-width
                                      folded-set
                                      fold-enabled?
                                      #:show-timestamps? [show-timestamps? #f]
                                      #:active-path-ids [active-path-ids (set)]
                                      #:bookmarks [bookmarks '()])
  (define-values (children-map root-entries) (build-children-map entries))
  (define bookmark-map (build-bookmark-map bookmarks))

  (define lines '())

  (define (render-node entry prefix is-last)
    (define id (list-ref entry 0))
    (define is-folded (set-member? folded-set id))
    (define connector (if is-last "└── " "├── "))

    (define line
      (make-node-line entry
                      prefix
                      connector
                      active-leaf-id
                      active-path-ids
                      show-timestamps?
                      bookmark-map
                      max-width
                      fold-enabled?
                      folded-set
                      children-map))
    (set! lines (append lines (list line)))

    ;; Only render children if node is not folded
    (unless is-folded
      (define children (hash-ref children-map id '()))
      (define child-prefix (string-append prefix (if is-last "    " "│   ")))
      (for ([child (in-list children)]
            [i (in-naturals)])
        (render-node child child-prefix (= i (- (length children) 1))))))

  ;; Render from each root
  (for ([root (in-list root-entries)]
        [i (in-naturals)])
    (render-node root "" (= i (- (length root-entries) 1))))

  lines)

;; ============================================================
;; Public rendering API
;; ============================================================

;; Render session tree (no fold/unfold).
;; Backward-compatible: optional keyword params add features.
(define (render-session-tree entries
                             active-leaf-id
                             max-width
                             #:show-timestamps? [show-timestamps? #f]
                             #:active-path-ids [active-path-ids (set)]
                             #:bookmarks [bookmarks '()])
  (render-session-tree-internal entries
                                active-leaf-id
                                max-width
                                (set)
                                #f
                                #:show-timestamps? show-timestamps?
                                #:active-path-ids active-path-ids
                                #:bookmarks bookmarks))

;; Render session tree with fold/unfold support.
;; folded-set: set of node IDs that are collapsed.
(define (render-session-tree-folded entries
                                    active-leaf-id
                                    max-width
                                    folded-set
                                    #:show-timestamps? [show-timestamps? #f]
                                    #:active-path-ids [active-path-ids (set)]
                                    #:bookmarks [bookmarks '()])
  (render-session-tree-internal entries
                                active-leaf-id
                                max-width
                                folded-set
                                #t
                                #:show-timestamps? show-timestamps?
                                #:active-path-ids active-path-ids
                                #:bookmarks bookmarks))

;; ============================================================
;; Navigation helpers
;; ============================================================

;; Move to next visible node (index-based, clamps at end)
(define (tree-next-node nodes current-idx)
  (if (< current-idx (sub1 (length nodes)))
      (add1 current-idx)
      current-idx))

;; Move to previous visible node (index-based, clamps at start)
(define (tree-prev-node nodes current-idx)
  (if (> current-idx 0)
      (sub1 current-idx)
      current-idx))

;; Toggle fold state for a node in the folded set
(define (tree-toggle-fold folded-set node-id)
  (if (set-member? folded-set node-id)
      (set-remove folded-set node-id)
      (set-add folded-set node-id)))

;; Enter a node: unfold it if folded, return updated folded-set
(define (tree-enter-node nodes current-idx folded-set)
  (cond
    [(or (< current-idx 0) (>= current-idx (length nodes))) folded-set]
    [else
     (define entry (list-ref nodes current-idx))
     (define node-id (list-ref entry 0))
     (if (set-member? folded-set node-id)
         (set-remove folded-set node-id)
         folded-set)]))

;; ============================================================
;; Branch abandonment detection
;; ============================================================

;; Check if navigating from one leaf to another would abandon a branch.
;; entries: flat list of (list id parent-id role text [timestamp])
;; Returns #t if from-leaf-id's path to root is deeper than to-entry-id's,
;; meaning navigation goes "back up" and abandons the deeper subtree.
(define (would-abandon-branch? entries from-leaf-id to-entry-id)
  (define by-id (make-hash))
  (for ([e (in-list entries)])
    (hash-set! by-id (list-ref e 0) e))

  (define (path-length id)
    (let loop ([current id]
               [depth 0])
      (define e (hash-ref by-id current #f))
      (cond
        [(not e) depth]
        [(not (list-ref e 1)) (add1 depth)] ; root reached
        [else (loop (list-ref e 1) (add1 depth))])))

  (and (not (equal? from-leaf-id to-entry-id))
       (> (path-length from-leaf-id) (path-length to-entry-id))))

;; ============================================================
;; Build tree nodes from messages
;; ============================================================

;; Build flat entry list from messages for tree rendering.
;; Returns: (listof (list id parent-id role text-snapshot timestamp))
(define (build-tree-nodes messages)
  (for/list ([msg (in-list messages)])
    (define text
      (with-handlers ([exn:fail? (lambda (e) "")])
        (string-join (for/list ([part (in-list (message-content msg))]
                                #:when (text-part? part))
                       (text-part-text part))
                     " ")))
    ;; Truncate snapshot
    (define snapshot
      (if (> (string-length text) 80)
          (string-append (substring text 0 77) "...")
          text))
    (list (message-id msg)
          (message-parent-id msg)
          (message-role msg)
          snapshot
          (message-timestamp msg))))

;; Find selected node by index in rendered output
(define (selected-node nodes index)
  (if (and (>= index 0) (< index (length nodes)))
      (list-ref nodes index)
      #f))
