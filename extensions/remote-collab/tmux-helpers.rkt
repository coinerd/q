#lang racket/base

;; extensions/remote-collab/tmux-helpers.rkt — Tmux session management
;;
;; Provides tmux session management via subprocess.
;; Used for managing remote pi/hermes sessions.

(require racket/contract
         racket/string
         racket/port)

(provide tmux-new-session
         tmux-send-keys
         tmux-capture-pane
         tmux-kill-session
         tmux-session-exists?
         tmux-list-sessions)

;; Run a tmux command, returns (values exit-code stdout stderr)
(define (run-tmux args)
  (define-values (sp-out sp-in sp-err sp-proc)
    (apply subprocess #f #f #f (find-executable-path "tmux") args))
  (subprocess-wait sp-proc)
  (define stdout-str (port->string sp-out))
  (define stderr-str (port->string sp-err))
  (close-input-port sp-out)
  (close-output-port sp-in)
  (close-input-port sp-err)
  (values (subprocess-status sp-proc) stdout-str stderr-str))

;; Create a new tmux session
(define (tmux-new-session name command
                          #:cwd [cwd #f]
                          #:detached [detached #t])
  (define args
    (append '("new-session")
            (if detached '("-d") '())
            (if name `("-s" ,name) '())
            (if cwd `("-c" ,cwd) '())
            (list command)))
  (define-values (ec out err) (run-tmux args))
  (if (zero? ec)
      #t
      (begin (log-warning "tmux-new-session failed: ~a" (string-trim err)) #f)))

;; Send keys to a tmux session
(define (tmux-send-keys name keys #:enter [enter #t])
  (define args
    (append '("send-keys" "-t")
            (list name)
            (list keys)
            (if enter '("Enter") '())))
  (define-values (ec out err) (run-tmux args))
  (zero? ec))

;; Capture pane content from a tmux session
(define (tmux-capture-pane name #:lines [lines 80])
  (define args `("capture-pane" "-t" ,name "-p" "-S" ,(format "-~a" lines)))
  (define-values (ec out err) (run-tmux args))
  (if (zero? ec)
      (string-trim out)
      (format "Error capturing pane: ~a" (string-trim err))))

;; Kill a tmux session
(define (tmux-kill-session name)
  (define-values (ec out err) (run-tmux `("kill-session" "-t" ,name)))
  (zero? ec))

;; Check if a tmux session exists
(define (tmux-session-exists? name)
  (define-values (ec out err) (run-tmux `("has-session" "-t" ,name)))
  (zero? ec))

;; List all tmux sessions
(define (tmux-list-sessions)
  (define-values (ec out err) (run-tmux '("list-sessions" "-F" "#{session_name}")))
  (if (zero? ec)
      (filter non-empty-string? (string-split out "\n"))
      '()))
