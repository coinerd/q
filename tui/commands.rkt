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
         racket/file
         racket/path
         "state.rkt"
         "render.rkt"
         "terminal.rkt"
         "palette.rkt"
         "tree-view.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/session-index.rkt"
         "../runtime/settings.rkt"
         "../interfaces/sessions.rkt"
         "../runtime/model-registry.rkt"
         "../runtime/extension-catalog.rkt"
         "../extensions/hooks.rkt"
         "../extensions/loader.rkt")

;; Command context struct (lightweight, avoids circular dep with interfaces/tui)
(provide cmd-ctx
         cmd-ctx?
         cmd-ctx-state-box
         cmd-ctx-running-box
         cmd-ctx-event-bus
         cmd-ctx-session-dir
         cmd-ctx-needs-redraw-box
         cmd-ctx-model-registry-box
         cmd-ctx-last-prompt-box
         cmd-ctx-session-runner
         cmd-ctx-input-text-box
         cmd-ctx-extension-registry-box

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
         model-registry-box ; (or/c (boxof (or/c model-registry? #f)) #f)
         last-prompt-box ; (boxof (or/c string? #f)) — last user prompt for /retry
         session-runner ; (string -> void) or #f — for /retry resubmission
         input-text-box ; (boxof string?) — raw input text for commands like /activate
         extension-registry-box) ; (or/c (boxof (or/c extension-registry? #f)) #f) — for ext command dispatch
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

;; ============================================================
;; /history command handler
;; ============================================================

(define (handle-history-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
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
           (define header (make-entry 'system "Session history:" 0 (hash)))
           (define entries-out
             (for/list ([msg (in-vector entries)])
               (make-entry 'system (format "  [~a]" (message-role msg)) 0 (hash))))
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
     (define entry (make-entry 'error "Usage: /fork <entry-id>" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    [else
     (define entry
       (make-entry 'system
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
;; /tree command handler
;; ============================================================

(define (handle-tree-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
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
           (define lines (render-session-tree nodes active-leaf-id cols))
           (define header (make-entry 'system "Session tree:" 0 (hash)))
           (define tree-entries
             (for/list ([line (in-list lines)])
               (make-entry 'system line 0 (hash))))
           (define all-entries (cons header tree-entries))
           (define new-state
             (for/fold ([s state]) ([e (in-list all-entries)])
               (add-transcript-entry s e)))
           (set-box! (cmd-ctx-state-box cctx) new-state)
           'continue))]))

;; ============================================================
;; /name command handler
;; ============================================================

(define (handle-name-command cctx [title #f])
  (define state (unbox (cmd-ctx-state-box cctx)))
  (cond
    [(not title)
     (define entry (make-entry 'error "Usage: /name <title>" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    [else
     ;; Publish name event for runtime to persist
     (when (cmd-ctx-event-bus cctx)
       (publish! (cmd-ctx-event-bus cctx)
                 (make-event "session.name"
                             (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                             (or (ui-state-session-id state) "")
                             #f
                             (hasheq 'name title))))
     (define entry (make-entry 'system (format "[session named: ~a]" title) 0 (hash)))
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
     (define entry (make-entry 'error "[no model registry available]" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    ;; No argument — list available models
    [(not arg)
     (define models (available-models reg))
     (define default (default-model reg))
     (define header (make-entry 'system "Available models:" 0 (hash)))
     (define model-entries
       (for/list ([m (in-list models)])
         (define marker (if (equal? (model-entry-name m) default) " *" "  "))
         (make-entry 'system
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
          (make-entry 'error
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
          (make-entry 'system
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
           (list (make-entry 'system "No sessions found." 0 (hash)))
           (for/list ([s (in-list strings)])
             (make-entry 'system s 0 (hash))))]
      ;; /sessions info <id>
      [(and (list? cmd) (>= (length cmd) 3) (eq? (cadr cmd) 'info))
       (define sid (caddr cmd))
       (define info (sessions-info session-dir sid))
       (list (make-entry 'system (sessions-info->string info) 0 (hash)))]
      ;; /sessions delete <id>
      [(and (list? cmd) (>= (length cmd) 3) (eq? (cadr cmd) 'delete))
       (define sid (caddr cmd))
       (define result (sessions-delete session-dir sid))
       (list (case result
               [(ok) (make-entry 'system (format "Session ~a deleted." sid) 0 (hash))]
               [(not-found) (make-entry 'error (format "Session not found: ~a" sid) 0 (hash))]
               [(cancelled) (make-entry 'system "Cancelled." 0 (hash))]))]
      ;; Fallback
      [else (list (make-entry 'system "Usage: /sessions [list|info <id>|delete <id>]" 0 (hash)))]))
  (define new-state
    (for/fold ([s state]) ([e (in-list entries)])
      (add-transcript-entry s e)))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; ============================================================
;; /activate command — extension management
;; ============================================================

;; Try to hot-load a newly activated extension into the running registry.
;; Returns a status entry (success or warning) or #f if no registry available.
(define (try-hot-load-extension cctx name target-dir)
  (define ext-reg-box (cmd-ctx-extension-registry-box cctx))
  (define ext-reg (and ext-reg-box (unbox ext-reg-box)))
  (define bus (cmd-ctx-event-bus cctx))
  (cond
    [(not ext-reg) #f]
    [else
     (define ext-path (build-path target-dir (string-append name ".rkt")))
     (cond
       [(not (file-exists? ext-path))
        (make-entry 'system
                    (format "  Warning: extension file not found for hot-load: ~a" ext-path)
                    (current-inexact-milliseconds)
                    (hash))]
       [else
        (with-handlers ([exn:fail? (λ (e)
                                     (make-entry 'system
                                                 (format "  Warning: hot-load failed: ~a"
                                                         (exn-message e))
                                                 (current-inexact-milliseconds)
                                                 (hash)))])
          (load-extension! ext-reg ext-path #:event-bus bus)
          (make-entry 'system
                      (format "  Extension '~a' loaded into running session." name)
                      (current-inexact-milliseconds)
                      (hash)))])]))

;; handle-activate-command : cmd-ctx? -> 'continue
;; Supports:
;;   /activate              — show status (active + available)
;;   /activate --available  — list all known extensions
;;   /activate <name>       — activate extension in project dir
;;   /activate --global <name> — activate in ~/.q/extensions/
(define (handle-activate-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define project-dir (current-directory))
  (define entries
    (cond
      [else
       ;; Parse args from raw input text (after /activate)
       (define input (unbox (cmd-ctx-input-text-box cctx)))
       (define args-str (string-trim (regexp-replace #rx"^/activate\\s*" input "")))
       (define args (filter (λ (a) (not (string=? a ""))) (string-split args-str)))
       (cond
         ;; /activate --available
         [(member "--available" args) (list-available-entries)]
         ;; /activate --global <name>
         [(member "--global" args)
          (define name
            (for/or ([a (in-list args)]
                     #:when (not (string-prefix? a "--")))
              a))
          (cond
            [(not name) (list (make-entry 'error "Usage: /activate --global <name>" 0 (hash)))]
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define q-home (build-path (find-system-path 'home-dir) ".q"))
             (define target-dir (build-path q-home "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (activate-extension! name target-dir)
               (define hot-load-entry (try-hot-load-extension cctx name target-dir))
               (if hot-load-entry
                   (list (make-entry 'system
                                     (format "Extension '~a' activated globally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash))
                         hot-load-entry)
                   (list (make-entry 'system
                                     (format "Extension '~a' activated globally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash)))))])]
         ;; /activate <name> — activate in project-local dir
         [(and (pair? args) (not (string-prefix? (car args) "--")))
          (define name (car args))
          (cond
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define target-dir (build-path project-dir ".q" "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (activate-extension! name target-dir)
               (define hot-load-entry (try-hot-load-extension cctx name target-dir))
               (if hot-load-entry
                   (list (make-entry 'system
                                     (format "Extension '~a' activated locally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash))
                         hot-load-entry)
                   (list (make-entry 'system
                                     (format "Extension '~a' activated locally (~a)" name target-dir)
                                     (current-inexact-milliseconds)
                                     (hash)))))])]
         ;; /activate with no args — show status
         [else (list-status-entries project-dir)])]))
  (define new-state
    (for/fold ([s state]) ([e (in-list entries)])
      (add-transcript-entry s e)))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; list-status-entries : path? -> (listof entry?)
;; Show active and available extensions.
(define (list-status-entries project-dir)
  (define local-dir (build-path project-dir ".q" "extensions"))
  (define global-dir (build-path (find-system-path 'home-dir) ".q" "extensions"))
  (define active-local
    (if (directory-exists? local-dir)
        (map path->string (directory-list local-dir))
        '()))
  (define active-global
    (if (directory-exists? global-dir)
        (map path->string (directory-list global-dir))
        '()))
  (define known (list-known-extensions))
  (define known-names (map ext-info-name known))
  (define active-names (append active-global active-local))
  (append (list (make-entry 'system "Extension Status:" 0 (hash)))
          (if (null? active-names)
              (list (make-entry 'system "  No extensions activated." 0 (hash)))
              (for/list ([n (in-list (remove-duplicates active-names))])
                (make-entry 'system (format "  ● ~a" n) 0 (hash))))
          (list (make-entry 'system "" 0 (hash))
                (make-entry 'system "Available extensions (use /activate <name>):" 0 (hash)))
          (for/list ([n (in-list known-names)])
            (make-entry 'system (format "  ○ ~a" n) 0 (hash)))
          (list (make-entry 'system "" 0 (hash))
                (make-entry
                 'system
                 "Use /activate <name> for project-local, /activate --global <name> for global."
                 0
                 (hash)))))

;; ============================================================
;; /deactivate command — extension removal
;; ============================================================

(define (handle-deactivate-command cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define project-dir (current-directory))
  (define entries
    (cond
      [else
       ;; Parse args from raw input text (after /deactivate)
       (define input (unbox (cmd-ctx-input-text-box cctx)))
       (define args-str (string-trim (regexp-replace #rx"^/deactivate\\s*" input "")))
       (define args (filter (λ (a) (not (string=? a ""))) (string-split args-str)))
       (cond
         [(null? args)
          (list
           (make-entry 'error "Usage: /deactivate <name> or /deactivate --global <name>" 0 (hash)))]
         [(member "--global" args)
          (define name
            (for/or ([a (in-list args)]
                     #:when (not (string-prefix? a "--")))
              a))
          (cond
            [(not name) (list (make-entry 'error "Usage: /deactivate --global <name>" 0 (hash)))]
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define q-home (build-path (find-system-path 'home-dir) ".q"))
             (define target-dir (build-path q-home "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (deactivate-extension! name target-dir)
               (list (make-entry 'system
                                 (format "Extension '~a' deactivated globally (~a)" name target-dir)
                                 (current-inexact-milliseconds)
                                 (hash))))])]
         [else
          (define name (car args))
          (cond
            [(not (valid-extension-name? name))
             (list (make-entry 'error (format "Invalid extension name: ~a" name) 0 (hash)))]
            [else
             (define target-dir (build-path project-dir ".q" "extensions"))
             (with-handlers ([exn:fail? (λ (e) (list (make-entry 'error (exn-message e) 0 (hash))))])
               (deactivate-extension! name target-dir)
               (list (make-entry 'system
                                 (format "Extension '~a' deactivated locally (~a)" name target-dir)
                                 (current-inexact-milliseconds)
                                 (hash))))])])]))
  (define new-state
    (for/fold ([s state]) ([e (in-list entries)])
      (add-transcript-entry s e)))
  (set-box! (cmd-ctx-state-box cctx) new-state)
  'continue)

;; list-available-entries : -> (listof entry?)
;; List all known extensions from the source tree.
(define (list-available-entries)
  (define known (list-known-extensions))
  (append
   (list (make-entry 'system "Available extensions:" 0 (hash)))
   (for/list ([e (in-list known)])
     (make-entry 'system (format "  ~a (~a)" (ext-info-name e) (ext-info-source-path e)) 0 (hash)))
   (list (make-entry 'system "" 0 (hash))
         (make-entry 'system "Use /activate <name> or /activate --global <name>" 0 (hash)))))

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
       [(name) (handle-name-command cctx (and (>= (length cmd) 2) (cadr cmd)))]
       [(fork) (handle-fork-command cctx (and (>= (length cmd) 2) (cadr cmd)))]
       [(sessions) (handle-sessions-tui-command cctx cmd)]
       [(switch-error children-error)
        (define entry (make-entry 'error (cadr cmd) 0 (hash)))
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
           (make-entry 'system "Commands:" 0 (hash))
           (for/list ([c (in-list cmds)])
             (define args-str
               (if (null? (cmd-entry-args-spec c))
                   ""
                   (string-append " " (string-join (cmd-entry-args-spec c) " "))))
             (define aliases-str
               (if (null? (cmd-entry-aliases c))
                   ""
                   (format " (~a)" (string-join (cmd-entry-aliases c) ", "))))
             (make-entry
              'system
              (format "  ~a~a~a  ~a" (cmd-entry-name c) args-str aliases-str (cmd-entry-summary c))
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
        (define entry (make-entry 'system "[compact requested]" 0 (hash)))
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
          (make-entry 'system "[interrupt requested]" (current-inexact-milliseconds) (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [(tree) (handle-tree-command cctx)]
       [(branches) (handle-branches-command cctx)]
       [(leaves) (handle-leaves-command cctx)]
       [(name) (handle-name-command cctx)]
       [(sessions) (handle-sessions-tui-command cctx #f)]
       [(retry)
        ;; /retry: resubmit last prompt, enriched with previous turn context
        (define last-prompt (unbox (cmd-ctx-last-prompt-box cctx)))
        (cond
          [last-prompt
           (define entry
             (make-entry 'system
                         (format "[retry: resubmitting]")
                         (current-inexact-milliseconds)
                         (hash)))
           (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
           ;; v0.14.2 Wave 2: Enrich retry with tool summary from previous turn
           (define tool-summary (get-last-turn-tool-summary (unbox (cmd-ctx-state-box cctx))))
           (define enriched-prompt
             (if tool-summary
                 (format "~a\n\n[Context from previous attempt: ~a]" last-prompt tool-summary)
                 last-prompt))
           (define runner (cmd-ctx-session-runner cctx))
           (when runner
             (thread
              (lambda ()
                (with-handlers ([exn:fail?
                                 (lambda (e)
                                   (define bus (cmd-ctx-event-bus cctx))
                                   (define sid (ui-state-session-id (unbox (cmd-ctx-state-box cctx))))
                                   (when (and bus sid)
                                     (publish!
                                      bus
                                      (make-event
                                       "runtime.error"
                                       (current-inexact-milliseconds)
                                       sid
                                       #f
                                       (hasheq 'error (exn-message e) 'errorType 'internal-error)))
                                     (publish! bus
                                               (make-event "turn.completed"
                                                           (current-inexact-milliseconds)
                                                           sid
                                                           #f
                                                           (hasheq 'reason "error")))))])
                  (runner enriched-prompt)))))]
          [else
           (define entry
             (make-entry 'error "No previous prompt to retry." (current-inexact-milliseconds) (hash)))
           (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))])
        'continue]
       [(activate) (handle-activate-command cctx)]
       [(deactivate) (handle-deactivate-command cctx)]
       [(quit)
        (set-box! (cmd-ctx-running-box cctx) #f)
        'quit]
       [(unknown)
        ;; Try extension command dispatch before showing error
        (define ext-reg-box (cmd-ctx-extension-registry-box cctx))
        (define ext-reg (and ext-reg-box (unbox ext-reg-box)))
        (define input-text (unbox (cmd-ctx-input-text-box cctx)))
        (define cmd-name
          (let ([trimmed (string-trim input-text)])
            (and (> (string-length trimmed) 0)
                 (char=? (string-ref trimmed 0) #\/)
                 (let ([parts (string-split trimmed)]) (and (pair? parts) (car parts))))))
        (with-output-to-file "/tmp/q-cmd-dispatch.log"
                             (lambda ()
                               (printf "[~a] cmd-name=~a has-ext-reg=~a input='~a'\n"
                                       (current-inexact-milliseconds)
                                       cmd-name
                                       (and ext-reg #t)
                                       input-text))
                             #:exists 'append)
        (define ext-result
          (and
           ext-reg
           cmd-name
           (dispatch-hooks 'execute-command (hasheq 'command cmd-name 'input input-text) ext-reg)))
        (with-output-to-file "/tmp/q-cmd-dispatch.log"
                             (lambda ()
                               (printf "[~a] result-action=~a\n"
                                       (current-inexact-milliseconds)
                                       (and ext-result (hook-result-action ext-result))))
                             #:exists 'append)
        (cond
          [(and ext-result (hook-result? ext-result) (eq? (hook-result-action ext-result) 'amend))
           ;; Extension handled the command — display result text
           (define payload (hook-result-payload ext-result))
           (define text (hash-ref payload 'text #f))
           (when text
             (define entry (make-entry 'system text (current-inexact-milliseconds) (hash)))
             (set-box! (cmd-ctx-state-box cctx)
                       (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry)))
           'continue]
          [else
           (with-output-to-file "/tmp/q-cmd-dispatch.log"
                                (lambda ()
                                  (printf "[~a] FELL-THROUGH: result=~a cmd-name=~a\n"
                                          (current-inexact-milliseconds)
                                          (if ext-result
                                              (hook-result-action ext-result)
                                              #f)
                                          cmd-name))
                                #:exists 'append)
           (define entry (make-entry 'error "Unknown command. Type /help for commands." 0 (hash)))
           (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
           'continue])]
       [else 'continue])]))
