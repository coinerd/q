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
         racket/string
         racket/list
         "state.rkt"
         "render.rkt"
         "terminal.rkt"
         "palette.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/session-index.rkt"
         "../runtime/settings.rkt"
         "../interfaces/sessions.rkt"
         "../runtime/model-registry.rkt")

;; Command context struct (lightweight, avoids circular dep with interfaces/tui)
(provide cmd-ctx
         cmd-ctx?
         cmd-ctx-state-box
         cmd-ctx-running-box
         cmd-ctx-event-bus
         cmd-ctx-session-dir
         cmd-ctx-needs-redraw-box
         cmd-ctx-model-registry-box

         ;; Main command dispatcher
         process-slash-command)

;; ============================================================
;; Command context — lightweight alternative to tui-ctx
;; ============================================================

;; Holds the mutable references that command handlers need.
;; Created by interfaces/tui.rkt from a tui-ctx.
(struct cmd-ctx
        (state-box ; (boxof ui-state)
         running-box ; (boxof boolean)
         event-bus ; event-bus? or #f
         session-dir ; (or/c path-string? #f)
         needs-redraw-box ; (boxof boolean)
         model-registry-box) ; (or/c (boxof (or/c model-registry? #f)) #f)
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
  (define active-id
    (or (ui-state-current-branch state)
        (and (not (null? branches)) (branch-info-id (last branches)))))
  (define branches-with-active (mark-active-branch branches active-id))
  ;; Render branch list as transcript entries
  (define-values (cols rows) (tui-screen-size))
  (define lines (render-branch-list branches-with-active cols))
  (define new-state
    (for/fold ([s state]) ([line (in-list lines)])
      (add-transcript-entry s (transcript-entry 'system (styled-line->text line) 0 (hash)))))
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
      (add-transcript-entry s (transcript-entry 'system (styled-line->text line) 0 (hash)))))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; Handle /switch <id> command
(define (handle-switch-command cctx branch-id)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (define entry
    (if (and idx (lookup-entry idx branch-id))
        (transcript-entry 'system
                          (format "[switched to branch: ~a]" branch-id)
                          0
                          (hasheq 'branch-id branch-id))
        (transcript-entry 'error (format "Branch not found: ~a" branch-id) 0 (hasheq))))
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
      (add-transcript-entry s (transcript-entry 'system (styled-line->text line) 0 (hash)))))
  (set-box! (cmd-ctx-state-box cctx) final-state)
  'continue)

;; ============================================================
;; /history command handler
;; ============================================================

(define (handle-history-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define idx (get-session-index cctx))
  (cond
    [(not idx)
     (define entry (transcript-entry 'error "No session index available" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    [else
     (define entries (session-index-entry-order idx))
     (if (null? entries)
         (let ([entry (transcript-entry 'system "Session is empty." 0 (hash))])
           (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
           'continue)
         (let ()
           (define header (transcript-entry 'system "Session history:" 0 (hash)))
           (define entries-out
             (for/list ([msg (in-vector entries)])
               (transcript-entry 'system (format "  [~a]" (message-role msg)) 0 (hash))))
           (define all-entries (cons header entries-out))
           (define new-state
             (for/fold ([s state]) ([e (in-list all-entries)])
               (add-transcript-entry s e)))
           (set-box! (cmd-ctx-state-box cctx) new-state)
           'continue))]))

;; ============================================================
;; /fork command handler
;; ============================================================

(define (handle-fork-command cctx [entry-id #f])
  (define state (unbox (cmd-ctx-state-box cctx)))
  (cond
    [(not entry-id)
     (define entry (transcript-entry 'error "Usage: /fork <entry-id>" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    [else
     (define entry
       (transcript-entry 'system
                         (format "[fork requested at: ~a]" entry-id)
                         0
                         (hasheq 'fork-entry-id entry-id)))
     ;; Publish fork event for runtime to handle
     (when (cmd-ctx-event-bus cctx)
       (publish! (cmd-ctx-event-bus cctx)
                 (make-event "fork.requested"
                             (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                             (or (ui-state-session-id state) "")
                             #f
                             (hasheq 'entry-id entry-id))))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]))

;; ============================================================
;; /model command handler
;; ============================================================

;; Handle /model command — list or switch models
(define (handle-model-command cctx [arg #f])
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define reg-box (cmd-ctx-model-registry-box cctx))
  (define reg (and reg-box (unbox reg-box)))
  (cond
    ;; No registry available
    [(not reg)
     (define entry (transcript-entry 'error "[no model registry available]" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    ;; No argument — list available models
    [(not arg)
     (define models (available-models reg))
     (define default (default-model reg))
     (define header (transcript-entry 'system "Available models:" 0 (hash)))
     (define model-entries
       (for/list ([m (in-list models)])
         (define marker (if (equal? (model-entry-name m) default) " *" "  "))
         (transcript-entry
          'system
          (format "~a ~a (~a)" marker (model-entry-name m) (model-entry-provider-name m))
          0
          (hash))))
     (define all-entries (cons header model-entries))
     (define new-state
       (for/fold ([s state]) ([e (in-list all-entries)])
         (add-transcript-entry s e)))
     (set-box! (cmd-ctx-state-box cctx) new-state)
     'continue]
    ;; Argument provided — switch model
    [else
     (define resolution (resolve-model reg arg))
     (cond
       [(not resolution)
        (define entry
          (transcript-entry 'error
                            (format "Model not found: ~a. Use /model to list available models." arg)
                            0
                            (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else
        ;; Publish model.switched event
        (when (cmd-ctx-event-bus cctx)
          (publish! (cmd-ctx-event-bus cctx)
                    (make-event "model.switched"
                                (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                                (or (ui-state-session-id state) "")
                                #f
                                (hasheq 'model
                                      (model-resolution-model-name resolution)
                                      'provider
                                      (model-resolution-provider-name resolution)))))
        (define entry
          (transcript-entry 'system
                            (format "[switched to model: ~a (provider: ~a)]"
                                    (model-resolution-model-name resolution)
                                    (model-resolution-provider-name resolution))
                            0
                            (hasheq 'model
                                  (model-resolution-model-name resolution)
                                  'provider
                                  (model-resolution-provider-name resolution))))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue])]))

;; ============================================================
;; /sessions command handler
;; ============================================================

(define (handle-sessions-tui-command cctx cmd)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define session-dir (or (cmd-ctx-session-dir cctx) (default-session-dir)))
  (define-values (entries)
    (cond
      ;; /sessions or /sessions list
      [(or (not cmd) (eq? cmd 'sessions) (equal? cmd '(sessions)) (equal? cmd '(sessions list)))
       (define sess-list (sessions-list session-dir #:limit 10))
       (define strings (sessions-list->strings sess-list))
       (if (null? sess-list)
           (list (transcript-entry 'system "No sessions found." 0 (hash)))
           (for/list ([s (in-list strings)])
             (transcript-entry 'system s 0 (hash))))]
      ;; /sessions info <id>
      [(and (list? cmd) (>= (length cmd) 3) (eq? (cadr cmd) 'info))
       (define sid (caddr cmd))
       (define info (sessions-info session-dir sid))
       (list (transcript-entry 'system (sessions-info->string info) 0 (hash)))]
      ;; /sessions delete <id>
      [(and (list? cmd) (>= (length cmd) 3) (eq? (cadr cmd) 'delete))
       (define sid (caddr cmd))
       (define result (sessions-delete session-dir sid))
       (list (case result
               [(ok) (transcript-entry 'system (format "Session ~a deleted." sid) 0 (hash))]
               [(not-found) (transcript-entry 'error (format "Session not found: ~a" sid) 0 (hash))]
               [(cancelled) (transcript-entry 'system "Cancelled." 0 (hash))]))]
      ;; Fallback
      [else
       (list (transcript-entry 'system "Usage: /sessions [list|info <id>|delete <id>]" 0 (hash)))]))
  (define new-state
    (for/fold ([s state]) ([e (in-list entries)])
      (add-transcript-entry s e)))
  (set-box! (cmd-ctx-state-box cctx) new-state)
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
       [(model) (handle-model-command cctx (and (>= (length cmd) 2) (cadr cmd)))]
       [(fork) (handle-fork-command cctx (and (>= (length cmd) 2) (cadr cmd)))]
       [(sessions) (handle-sessions-tui-command cctx cmd)]
       [(switch-error children-error)
        (define entry (transcript-entry 'error (cadr cmd) 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else 'continue])]
    ;; Handle simple symbol commands
    [else
     (case cmd
       [(model) (handle-model-command cctx)]
       [(history) (handle-history-command cctx)]
       [(help)
        ;; Generate help from palette registry
        (define reg (make-command-registry))
        (define cmds (all-commands reg))
        (define help-entries
          (cons
           (transcript-entry 'system "Commands:" 0 (hash))
           (for/list ([c (in-list cmds)])
             (define args-str (if (null? (cmd-entry-args-spec c))
                                  ""
                                  (string-append " " (string-join (cmd-entry-args-spec c) " "))))
             (define aliases-str (if (null? (cmd-entry-aliases c))
                                     ""
                                     (format " (~a)" (string-join (cmd-entry-aliases c) ", "))))
             (transcript-entry
              'system
              (format "  ~a~a~a  ~a"
                      (cmd-entry-name c)
                      args-str
                      aliases-str
                      (cmd-entry-summary c))
              0
              (hash)))))
        (define new-state
          (for/fold ([s state]) ([e (in-list help-entries)])
            (add-transcript-entry s e)))
        (set-box! (cmd-ctx-state-box cctx) new-state)
        'continue]
       [(clear)
        (set-box! (cmd-ctx-state-box cctx) (struct-copy ui-state state [transcript '()]))
        'continue]
       [(compact)
        ;; Compact: add status message and notify runtime
        (define entry (transcript-entry 'system "[compact requested]" 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
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
        (define entry
          (transcript-entry 'system "[interrupt requested]" (current-inexact-milliseconds) (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [(branches) (handle-branches-command cctx)]
       [(leaves) (handle-leaves-command cctx)]
       [(sessions) (handle-sessions-tui-command cctx #f)]
       [(quit)
        (set-box! (cmd-ctx-running-box cctx) #f)
        'quit]
       [(unknown)
        (define entry (transcript-entry 'error "Unknown command. Type /help for commands." 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else 'continue])]))
