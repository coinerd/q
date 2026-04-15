#lang racket/base

;; q/tui/tree-view.rkt — Session tree rendering and navigation
;;
;; Renders the session tree as a depth-first text representation
;; with Unicode box-drawing characters (├ └ │ ─).

(require racket/list
         racket/string
         "../util/protocol-types.rkt")

(provide render-session-tree
         tree-node?
         make-tree-node
         build-tree-nodes
         selected-node)

;; Tree node for rendering
(struct tree-node (id role text depth children) #:transparent)

(define (make-tree-node id role text depth children)
  (tree-node id role text depth children))

;; Truncate text to fit within max-width columns
(define (truncate-text text max-width)
  (if (> (string-length text) max-width)
      (string-append (substring text 0 (max 0 (- max-width 3))) "...")
      text))

;; Render a tree node and its children as styled lines
;; Returns (listof string)
(define (render-session-tree entries active-leaf-id max-width)
  ;; entries: flat list of (list id parent-id role text-snapshot)
  ;; Build parent→children map
  (define children-map (make-hash))
  (define root-entries '())
  (for ([e (in-list entries)])
    (define pid (list-ref e 1))
    (if pid
        (hash-update! children-map pid (lambda (lst) (append lst (list e))) '())
        (set! root-entries (append root-entries (list e)))))

  ;; Depth-first render starting from roots
  (define lines '())
  (define (render-node entry prefix is-last)
    (define id (list-ref entry 0))
    (define role (list-ref entry 2))
    (define text (list-ref entry 3))
    (define children (hash-ref children-map id '()))
    (define connector (if is-last "└── " "├── "))
    (define marker (if (equal? id active-leaf-id) " ◄" ""))
    (define role-tag (format "[~a]" role))
    (define max-text-width
      (max 10
           (- max-width
              (string-length prefix)
              (string-length connector)
              (string-length role-tag)
              (string-length marker)
              1)))
    (define truncated (truncate-text text max-text-width))
    (define line (format "~a~a~a ~a~a" prefix connector role-tag truncated marker))
    (set! lines (append lines (list line)))
    (define child-prefix (string-append prefix (if is-last "    " "│   ")))
    (for ([child (in-list children)]
          [i (in-naturals)])
      (render-node child child-prefix (= i (- (length children) 1)))))

  ;; Render from each root
  (for ([root (in-list root-entries)]
        [i (in-naturals)])
    (render-node root "" (= i (- (length root-entries) 1))))

  lines)

;; Build flat entry list from messages for tree rendering
;; Returns: (listof (list id parent-id role text-snapshot))
(define (build-tree-nodes messages)
  (for/list ([msg (in-list messages)])
    (define text
      (with-handlers ([exn:fail? (lambda (e) "")])
        (string-join (for/list ([part (in-list (message-content msg))]
                                #:when (text-part? part))
                       (text-part-text part))
                     " ")))
    ;; Truncate snapshot
    (list (message-id msg)
          (message-parent-id msg)
          (message-role msg)
          (if (> (string-length text) 80)
              (string-append (substring text 0 77) "...")
              text))))

;; Find selected node by index in rendered output
(define (selected-node nodes index)
  (if (and (>= index 0) (< index (length nodes)))
      (list-ref nodes index)
      #f))
