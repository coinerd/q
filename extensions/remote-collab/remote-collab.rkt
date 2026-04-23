#lang racket/base

;; extensions/remote-collab/remote-collab.rkt — Remote Collaboration Extension
;;
;; Wave C1: Registers remote-q tool for controlling remote q instances
;; via SSH + tmux. Actions: status, start, send, capture, wait, interrupt, stop.

(require racket/contract
         racket/string
         racket/port
         "../define-extension.rkt"
         "../dynamic-tools.rkt"
         "../context.rkt"
         "../hooks.rkt"
         "../../tools/tool.rkt"
         "ssh-helpers.rkt"
         "tmux-helpers.rkt")

(provide the-extension
         handle-remote-q)

;; ============================================================
;; Remote session management
;; ============================================================

;; Build a remote command that wraps tmux operations via SSH
(define (remote-tmux host session action args)
  (case action
    [(status)
     (format "tmux has-session -t ~a 2>/dev/null && echo 'running' || echo 'stopped'" session)]
    [(start)
     (define cwd (hash-ref args 'cwd "~/src/q-agent"))
     (define provider (hash-ref args 'provider "default"))
     (define model (hash-ref args 'model ""))
     (define thinking (hash-ref args 'thinking "medium"))
     (define base-cmd "pi")
     (define cmd
       (if (non-empty-string? model)
           (format "~a --model ~a --thinking ~a" base-cmd model thinking)
           base-cmd))
     (format "tmux new-session -d -s ~a -c ~a '~a'" session cwd cmd)]
    [(send)
     (define prompt (hash-ref args 'prompt ""))
     ;; Escape single quotes in prompt
     (define escaped (string-replace prompt "'" "'\\''"))
     (format "tmux send-keys -t ~a '~a' Enter" session escaped)]
    [(capture)
     (define lines (hash-ref args 'lines 80))
     (format "tmux capture-pane -t ~a -p -S -~a" session lines)]
    [(wait)
     ;; Wait for prompt to appear (simple: sleep + check for prompt marker)
     (define timeout (hash-ref args 'timeout 60))
     (format
      "for i in $(seq 1 ~a); do tmux capture-pane -t ~a -p | tail -5 | grep -q '> ' && exit 0; sleep 2; done; echo 'timeout'"
      timeout
      session)]
    [(interrupt) (format "tmux send-keys -t ~a Escape" session)]
    [(stop) (format "tmux kill-session -t ~a 2>/dev/null; echo 'stopped'" session)]
    [else (error 'remote-q "unknown action: ~a" action)]))

;; ============================================================
;; Tool handler
;; ============================================================

(define (handle-remote-q args [exec-ctx #f])
  (define action-str (hash-ref args 'action "status"))
  (define action (string->symbol action-str))
  (define host (hash-ref args 'host ""))
  (define session (hash-ref args 'session "q-agent"))
  (define opts (hash-ref args 'ssh_options '()))

  (when (string=? host "")
    (error 'remote-q "host is required"))

  (define cmd (remote-tmux host session action args))

  ;; Execute via SSH
  (define-values (ec stdout stderr)
    (ssh-execute host cmd #:options (append (default-ssh-options) opts)))

  (if (zero? ec)
      (make-success-result (list (hasheq 'type "text" 'text (string-trim stdout))))
      (make-error-result (format "remote-q error (exit ~a): ~a~a"
                                 ec
                                 (string-trim stdout)
                                 (if (non-empty-string? (string-trim stderr))
                                     (format "\n~a" (string-trim stderr))
                                     "")))))

;; ============================================================
;; Extension definition
;; ============================================================

(define (register-remote-tools ctx)
  (ext-register-tool!
   ctx
   "remote-q"
   (string-append "Control a remote q agent instance via SSH + tmux. "
                  "Actions: status (check session), start (new session), "
                  "send (send prompt), capture (read output), "
                  "wait (wait for idle), interrupt (stop task), stop (kill session).")
   (hasheq
    'type
    "object"
    'required
    '("host" "action")
    'properties
    (hasheq 'host
            (hasheq 'type "string" 'description "SSH host (user@host)")
            'action
            (hasheq 'type "string" 'description "status|start|send|capture|wait|interrupt|stop")
            'session
            (hasheq 'type "string" 'description "Tmux session name (default: q-agent)")
            'prompt
            (hasheq 'type "string" 'description "Prompt text (for send)")
            'lines
            (hasheq 'type "integer" 'description "Lines to capture (default: 80)")
            'cwd
            (hasheq 'type "string" 'description "Working directory (for start)")
            'provider
            (hasheq 'type "string" 'description "Pi provider")
            'model
            (hasheq 'type "string" 'description "Pi model")
            'thinking
            (hasheq 'type "string" 'description "Thinking level")
            'timeout
            (hasheq 'type "integer" 'description "Wait timeout seconds (default: 60)")
            'ssh_options
            (hasheq 'type "array" 'description "Extra SSH options")))
   handle-remote-q)
  (hook-pass ctx))

(define-q-extension the-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-remote-tools)
