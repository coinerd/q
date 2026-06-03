#lang racket/base

;; tui/commands/branch.rkt — branch/tree command handlers
;;
;; Extracted from commands.rkt (ARCH-06).
;; Handles /branches, /leaves, /switch, /children, /tree commands.

(require racket/list
         racket/set
         "../state.rkt"
         "../render.rkt"
         "../terminal.rkt"
         "../tree-view.rkt"
         (only-in "../../util/message/message.rkt" message-id message-parent-id message-role)
         "../../runtime/session-index.rkt"
         "context.rkt")

(provide handle-branches-command
         handle-leaves-command
         handle-switch-command
         handle-children-command
         handle-tree-command
         get-session-index)

;; ============================================================
;; Branch inspection helpers
;; ============================================================

;; Build branch-info structs from session index
(define (build-branch-info-list idx)
  (if (not idx)
      '()
      (let ([leaves (map message-id (leaf-nodes idx))]
            [entries (vector->list (session-index-entry-order idx))])
        (for/list ([msg (in-list entries)])
          (branch-info (message-id msg)
                       (message-parent-id msg)
                       (message-role msg)
                       (member (message-id msg) leaves)
                       #f))))) ; active? will be set separately

;; Mark active branch in the list
(define (mark-active-branch branches active-id)
  (for/list ([b (in-list branches)])
    (struct-copy branch-info b [active? (equal? (branch-info-id b) active-id)])))

;; Get session index from current session (if available)
(define (get-session-index cctx)
  (define dir (cmd-ctx-session-dir cctx))
  (if dir
      (with-handlers ([exn:fail? (lambda (e)
                                   (fprintf (current-error-port)
                                            "WARNING: Failed to load session index: ~a~n"
                                            (exn-message e))
                                   #f)])
        (define state (unbox (cmd-ctx-state-box cctx)))
        (define sid (ui-state-session-id state))
        (if sid
            (let ([index-path (build-path dir sid "session.index")]) (load-index index-path))
            #f))
      #f))

;; ============================================================
;; Branch command handlers
;; ============================================================

;; Handle /branches command
(define (handle-branches-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define branches (build-branch-info-list idx))
  (define active-id
    (or (ui-state-current-branch state)
        (and (not (null? branches)) (branch-info-id (last branches)))))
  (define branches-with-active (mark-active-branch branches active-id))
  ;; Render branch list as transcript entries
  (define-values (cols rows) (tui-screen-size))
  (define lines (render-branch-list branches-with-active cols))
  (define new-state
    (for/fold ([s state]) ([line (in-list lines)])
      (add-transcript-entry s (make-entry 'system (styled-line->text line) 0 (hash)))))
  (set-box! (cmd-ctx-state-box cctx) (set-visible-branches new-state branches-with-active))
  'continue)

;; Handle /leaves command
(define (handle-leaves-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define branches (build-branch-info-list idx))
  (define active-id
    (or (ui-state-current-branch state)
        (and (not (null? branches)) (branch-info-id (last branches)))))
  (define branches-with-active (mark-active-branch branches active-id))
  ;; Render leaf nodes as transcript entries
  (define-values (cols rows) (tui-screen-size))
  (define lines (render-leaf-nodes branches-with-active cols))
  (define new-state
    (for/fold ([s state]) ([line (in-list lines)])
      (add-transcript-entry s (make-entry 'system (styled-line->text line) 0 (hash)))))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; Handle /switch <id> command
(define (handle-switch-command cctx branch-id)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define entry
    (if (and idx (lookup-entry idx branch-id))
        (make-entry 'system
                    (format "[switched to branch: ~a]" branch-id)
                    0
                    (hasheq 'branch-id branch-id))
        (make-entry 'error (format "Branch not found: ~a" branch-id) 0 (hasheq))))
  (define new-state (add-transcript-entry state entry))
  (when (and idx (lookup-entry idx branch-id))
    (set-box! (cmd-ctx-state-box cctx) (set-current-branch new-state branch-id)))
  'continue)

;; Handle /children <id> command
(define (handle-children-command cctx node-id)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define-values (lines new-state)
    (if (not idx)
        (values (list (styled-line (list (styled-segment "  No session index available" '(dim)))))
                state)
        (let ([children-msgs (children-of idx node-id)])
          (if (null? children-msgs)
              (values (list (styled-line (list (styled-segment (format "  Node ~a has no children"
                                                                       node-id)
                                                               '(dim)))))
                      state)
              (let*-values ([(cols rows) (tui-screen-size)])
                (let* ([children-info (for/list ([msg (in-list children-msgs)])
                                        (branch-info (message-id msg)
                                                     node-id
                                                     (message-role msg)
                                                     (null? (children-of idx (message-id msg)))
                                                     #f))]
                       [rendered (render-children-list node-id children-info cols)])
                  (values rendered state)))))))
  ;; Add lines to transcript
  (define final-state
    (for/fold ([s new-state]) ([line (in-list lines)])
      (add-transcript-entry s (make-entry 'system (styled-line->text line) 0 (hash)))))
  (set-box! (cmd-ctx-state-box cctx) final-state)
  'continue)

;; Handle /tree command
(define (handle-tree-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  ;; If tree overlay already active, dismiss it (toggle)
  (define ov (ui-state-active-overlay state))
  (cond
    [(and ov (eq? (overlay-state-type ov) 'tree-browser))
     (set-box! (cmd-ctx-state-box cctx) (dismiss-overlay state))
     (set-box! (cmd-ctx-needs-redraw-box cctx) #t)
     'continue]
    [else
     (define idx (get-session-index cctx))
     (cond
       [(not idx)
        (define entry (make-entry 'error "No session index available" 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else
        (define entries (session-index-entry-order idx))
        (if (zero? (vector-length entries))
            (let ([entry (make-entry 'system "Session is empty." 0 (hash))])
              (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
              'continue)
            (let ()
              (define msgs (vector->list entries))
              (define nodes (build-tree-nodes msgs))
              (define active-leaf-id (let ([leaf (active-leaf idx)]) (and leaf (message-id leaf))))
              (define-values (cols rows) (tui-screen-size))
              ;; Build interactive tree overlay
              (define rendered (render-session-tree nodes active-leaf-id cols))
              (define styled-lines
                (for/list ([line (in-list rendered)]
                           [i (in-naturals)])
                  (if (= i 0)
                      (list (cons (format "► ~a" line) '()))
                      (list (cons (format "  ~a" line) '())))))
              (define tbs (tree-browser-state nodes 0 (set) rendered))
              (define header-line
                (list (cons "Session Tree (↑↓ navigate, Enter/f fold, q/Esc close)" '((bold)))))
              (define all-styled (append (list header-line) styled-lines))
              (define new-state
                (show-overlay state
                              'tree-browser
                              all-styled
                              ""
                              #:anchor 'top-left
                              #:width cols
                              #:height (min (add1 (length rendered)) (- rows 4))
                              #:margin 1))
              ;; Store tree-browser-state in overlay extra
              (define new-ov (ui-state-active-overlay new-state))
              (define final-state
                (struct-copy ui-state
                             new-state
                             [active-overlay (struct-copy overlay-state new-ov [extra tbs])]))
              (set-box! (cmd-ctx-state-box cctx) final-state)
              (set-box! (cmd-ctx-needs-redraw-box cctx) #t)
              'continue))])]))
