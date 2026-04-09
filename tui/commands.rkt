#lang racket/base

;; q/tui/commands.rkt — Slash command handlers for the TUI
;;
;; Processes all slash commands dispatched from the TUI main loop:
;;   /help /clear /compact /interrupt /quit /branches /leaves /switch /children
;;
;; Uses cmd-ctx (lightweight context struct) to avoid circular dependency
;; with interfaces/tui.rkt where tui-ctx is defined.
;;
;; Extracted from interfaces/tui.rkt for modularity.

(require racket/base
         racket/list
         "state.rkt"
         "render.rkt"
         "terminal.rkt"
         "../agent/types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/session-index.rkt")

(provide
 ;; Command context struct (lightweight, avoids circular dep with interfaces/tui)
 cmd-ctx
 cmd-ctx?
 cmd-ctx-state-box
 cmd-ctx-running-box
 cmd-ctx-event-bus
 cmd-ctx-session-dir
 cmd-ctx-needs-redraw-box

 ;; Main command dispatcher
 process-slash-command)

;; ============================================================
;; Command context — lightweight alternative to tui-ctx
;; ============================================================

;; Holds the mutable references that command handlers need.
;; Created by interfaces/tui.rkt from a tui-ctx.
(struct cmd-ctx
  (state-box         ; (boxof ui-state)
   running-box       ; (boxof boolean)
   event-bus         ; event-bus? or #f
   session-dir       ; (or/c path-string? #f)
   needs-redraw-box) ; (boxof boolean)
  #:transparent)

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
          (branch-info
           (message-id msg)
           (message-parent-id msg)
           (message-role msg)
           (member (message-id msg) leaves)
           #f)))))  ; active? will be set separately

;; Mark active branch in the list
(define (mark-active-branch branches active-id)
  (for/list ([b (in-list branches)])
    (struct-copy branch-info b [active? (equal? (branch-info-id b) active-id)])))

;; Get session index from current session (if available)
(define (get-session-index cctx)
  (define dir (cmd-ctx-session-dir cctx))
  (if dir
      (with-handlers ([exn:fail? (lambda (e) #f)])
        (load-index dir))
      #f))

;; ============================================================
;; Branch inspection command handlers
;; ============================================================

;; Handle /branches command
(define (handle-branches-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define branches (build-branch-info-list idx))
  (define active-id (or (ui-state-current-branch state)
                        (and (not (null? branches))
                             (branch-info-id (last branches)))))
  (define branches-with-active (mark-active-branch branches active-id))
  ;; Render branch list as transcript entries
  (define-values (cols rows) (tui-screen-size))
  (define lines (render-branch-list branches-with-active cols))
  (define new-state
    (for/fold ([s state])
              ([line (in-list lines)])
      (add-transcript-entry s (transcript-entry 'system
                                                  (styled-line->text line)
                                                  0
                                                  (hash)))))
  (set-box! (cmd-ctx-state-box cctx)
            (set-visible-branches new-state branches-with-active))
  'continue)

;; Handle /leaves command
(define (handle-leaves-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define branches (build-branch-info-list idx))
  (define active-id (or (ui-state-current-branch state)
                        (and (not (null? branches))
                             (branch-info-id (last branches)))))
  (define branches-with-active (mark-active-branch branches active-id))
  ;; Render leaf nodes as transcript entries
  (define-values (cols rows) (tui-screen-size))
  (define lines (render-leaf-nodes branches-with-active cols))
  (define new-state
    (for/fold ([s state])
              ([line (in-list lines)])
      (add-transcript-entry s (transcript-entry 'system
                                                  (styled-line->text line)
                                                  0
                                                  (hash)))))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; Handle /switch <id> command
(define (handle-switch-command cctx branch-id)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define entry (if (and idx (lookup-entry idx branch-id))
                    (transcript-entry 'system
                                      (format "[switched to branch: ~a]" branch-id)
                                      0
                                      (hash 'branch-id branch-id))
                    (transcript-entry 'error
                                      (format "Branch not found: ~a" branch-id)
                                      0
                                      (hash))))
  (define new-state (add-transcript-entry state entry))
  (when (and idx (lookup-entry idx branch-id))
    (set-box! (cmd-ctx-state-box cctx)
              (set-current-branch new-state branch-id)))
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
              (values (list (styled-line (list (styled-segment (format "  Node ~a has no children" node-id) '(dim)))))
                      state)
              (let*-values ([(cols rows) (tui-screen-size)])
                (let* ([children-info
                        (for/list ([msg (in-list children-msgs)])
                          (branch-info
                           (message-id msg)
                           node-id
                           (message-role msg)
                           (null? (children-of idx (message-id msg)))
                           #f))]
                       [rendered (render-children-list node-id children-info cols)])
                  (values rendered state)))))))
  ;; Add lines to transcript
  (define final-state
    (for/fold ([s new-state])
              ([line (in-list lines)])
      (add-transcript-entry s (transcript-entry 'system
                                                  (styled-line->text line)
                                                  0
                                                  (hash)))))
  (set-box! (cmd-ctx-state-box cctx) final-state)
  'continue)

;; ============================================================
;; Main command dispatcher
;; ============================================================

;; Process a slash command. Returns 'continue | 'quit
;; cmd can be: symbol | (list symbol args...)
(define (process-slash-command cctx cmd)
  ;; Mark dirty (defensive: slash commands always change state)
  (set-box! (cmd-ctx-needs-redraw-box cctx) #t)
  (define state (unbox (cmd-ctx-state-box cctx)))
  ;; Handle structured commands (lists)
  (cond
    [(list? cmd)
     (case (car cmd)
       [(switch) (handle-switch-command cctx (cadr cmd))]
       [(children) (handle-children-command cctx (cadr cmd))]
       [(switch-error children-error)
        (define entry (transcript-entry 'error (cadr cmd) 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry state entry))
        'continue]
       [else 'continue])]
    ;; Handle simple symbol commands
    [else
     (case cmd
       [(help)
        (define help-text "Commands: /help /clear /compact /interrupt /quit /branches /leaves /children /switch")
        (define entry (transcript-entry 'system help-text 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry state entry))
        'continue]
       [(clear)
        (set-box! (cmd-ctx-state-box cctx)
                  (struct-copy ui-state state [transcript '()]))
        'continue]
       [(compact)
        ;; Compact: add status message and notify runtime
        (define entry (transcript-entry 'system "[compact requested]" 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry state entry))
        (when (cmd-ctx-event-bus cctx)
          (publish! (cmd-ctx-event-bus cctx)
                    (make-event "compact.requested"
                                (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                                (or (ui-state-session-id state) "")
                                #f
                                (hash))))
        'continue]
       [(interrupt)
        ;; Interrupt: notify runtime
        (when (cmd-ctx-event-bus cctx)
          (publish! (cmd-ctx-event-bus cctx)
                    (make-event "interrupt.requested"
                                (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                                (or (ui-state-session-id state) "")
                                #f
                                (hash))))
        (define entry (transcript-entry 'system "[interrupt requested]" (current-inexact-milliseconds) (hash)))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry state entry))
        'continue]
       [(branches) (handle-branches-command cctx)]
       [(leaves) (handle-leaves-command cctx)]
       [(quit)
        (set-box! (cmd-ctx-running-box cctx) #f)
        'quit]
       [(unknown)
        (define entry (transcript-entry 'error "Unknown command. Type /help for commands." 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry state entry))
        'continue]
       [else 'continue])]))
