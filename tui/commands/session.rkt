#lang racket/base

;; tui/commands/session.rkt — session management command handlers
;;
;; Extracted from commands.rkt (ARCH-06).
;; Handles /history, /fork, /name, /sessions commands.

(require racket/string
         racket/list
         "../state.rkt"
         (only-in "../../util/event/event.rkt" make-event)
         (only-in "../../util/message/message.rkt"
                  message-role
                  message-id
                  message-content
                  message-timestamp
                  message-meta)
         (only-in "../../util/content/content-parts.rkt" text-part? text-part-text)
         "../../util/event/event-bus.rkt"
         "../../runtime/session-index.rkt"
         "../../interfaces/sessions.rkt"
         (only-in "../../runtime/settings.rkt" default-session-dir)
         "context.rkt")

(provide handle-history-command
         handle-fork-command
         handle-name-command
         handle-sessions-tui-command
         message-excerpt
         format-ts
         pad2)

;; ============================================================
;; /history command handler
;; ============================================================

;; Extract a bounded excerpt from message content parts.
(define (message-excerpt msg [max-len 60])
  (define parts (message-content msg))
  (define text
    (string-join (for/list ([p (in-list parts)]
                            #:when (text-part? p))
                   (text-part-text p))
                 " "))
  (define cleaned (string-normalize-spaces (string-trim text)))
  (if (> (string-length cleaned) max-len)
      (string-append (substring cleaned 0 max-len) "...")
      (if (string=? cleaned "") "(non-text)" cleaned)))

;; Format a timestamp (epoch seconds) to HH:MM:SS.
(define (pad2 n)
  (define s
    (if (number? n)
        (number->string n)
        (format "~a" n)))
  (if (< (string-length s) 2)
      (string-append "0" s)
      s))

(define (format-ts ts)
  (if (and (number? ts) (> ts 0))
      (let* ([secs (inexact->exact (truncate ts))]
             [date (seconds->date secs #f)])
        (format "~a:~a:~a"
                (pad2 (date-hour date))
                (pad2 (date-minute date))
                (pad2 (date-second date))))
      "--:--:--"))

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
             (for/list ([msg (in-vector entries)]
                        [i (in-naturals)])
               (define role (message-role msg))
               (define mid (or (message-id msg) "—"))
               (define ts (message-timestamp msg))
               (define meta (message-meta msg))
               (define tool-name (and (hash? meta) (hash-ref meta 'tool-name #f)))
               (define label
                 (if tool-name
                     (format "~a/~a" role tool-name)
                     (symbol->string role)))
               (make-entry
                'system
                (format "  [~a] ~a ~a [~a] ~a" i (format-ts ts) mid label (message-excerpt msg))
                0
                (hash))))
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

;; Helper: re-use get-session-index from branch module for history
(require (only-in "branch.rkt" get-session-index))
