#lang racket/base

;; wiring/run-interactive.rkt — Interactive and single-shot mode runners
;;
;; Contains:
;;   - run-interactive: full interactive REPL session
;;   - run-single-shot: single prompt → response
;;   - run-resume: resume an existing session
;;   - make-terminal-subscriber: streaming Markdown event renderer
;;   - handle-sessions-interactive-command: /sessions command handler

(require racket/string
         "../interfaces/cli.rkt"
         "../interfaces/sessions.rkt"
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  session-history
                  fork-session)
         "../runtime/settings.rkt"
         "../runtime/model-registry.rkt"
         (only-in "../runtime/provider-factory.rkt" build-provider)
         "../tools/tool.rkt"
         "../agent/event-bus.rkt"
         (only-in "../util/protocol-types.rkt"
                  event-ev
                  event-payload
                  message-role
                  message-content
                  text-part?
                  text-part-text)
         "../extensions/api.rkt"
         (only-in "../extensions/loader.rkt" load-extension!))

(provide make-terminal-subscriber
         handle-sessions-interactive-command
         run-interactive
         run-single-shot
         run-resume)

;; ============================================================
;; Terminal event subscriber
;; ============================================================

;; Create a terminal event subscriber with line-buffered Markdown rendering.
;; Tracks stream state per-turn: deltas go through the Markdown writer,
;; non-delta events are printed as before.

(define (make-terminal-subscriber)
  (define-values (writer flush!) (make-stream-markdown-writer))
  (lambda (evt)
    (define ev (event-ev evt))
    (cond
      [(equal? ev "model.stream.delta")
       (define delta (hash-ref (event-payload evt) 'delta ""))
       (when (> (string-length delta) 0)
         (writer delta))]
      [(equal? ev "model.stream.completed")
       (flush!)
       ;; Reset writer for next turn
       (define-values (new-writer new-flush!) (make-stream-markdown-writer))
       (set! writer new-writer)
       (set! flush! new-flush!)]
      [(equal? ev "turn.started")
       ;; Reset writer at turn start
       (define-values (new-writer new-flush!) (make-stream-markdown-writer))
       (set! writer new-writer)
       (set! flush! new-flush!)]
      [else
       (define text (format-event-for-terminal evt))
       (unless (string=? text "")
         (displayln text))])))

;; ============================================================
;; /sessions interactive command handler
;; ============================================================

(define (handle-sessions-interactive-command cmd out session-dir)
  (define sdir (or session-dir (default-session-dir)))
  (cond
    [(or (equal? cmd '(sessions)) (equal? cmd '(sessions list)))
     (define sess-list (sessions-list sdir #:limit 10))
     (for-each displayln (sessions-list->strings sess-list))]
    [(and (list? cmd) (>= (length cmd) 3) (equal? (cadr cmd) 'info))
     (define sid (caddr cmd))
     (define info (sessions-info sdir sid))
     (displayln (sessions-info->string info))]
    [(and (list? cmd) (>= (length cmd) 3) (equal? (cadr cmd) 'delete))
     (define sid (caddr cmd))
     (define result (sessions-delete sdir sid #:confirm? #t #:out out))
     (case result
       [(ok) (displayln (format "Session ~a deleted." sid))]
       [(not-found) (displayln (format "Session not found: ~a" sid))]
       [(cancelled) (displayln "Cancelled.")])]
    [else (displayln "Usage: /sessions [list|info <id>|delete <id>]")]))

;; ============================================================
;; Mode runners — interactive, single-shot, resume
;; ============================================================

(define (run-interactive cfg rt-config #:provider-name [prov-name #f])
  (define sess (make-agent-session rt-config))
  (define bus (hash-ref rt-config 'event-bus))
  (subscribe! bus (make-terminal-subscriber))
  (run-cli-interactive
   cfg
   #:session-fn (lambda (prompt) (run-prompt! sess prompt))
   #:compact-fn (lambda () (displayln "[compaction requested]"))
   #:history-fn (lambda ([out (current-output-port)])
                  (define hist (session-history sess))
                  (for ([msg (in-list hist)])
                    (define role (message-role msg))
                    (define text-parts (filter text-part? (message-content msg)))
                    (define text (string-join (map text-part-text text-parts) " "))
                    (displayln (format "[~a] ~a" role text) out)))
   #:fork-fn (lambda (entry-id)
               (define new-sess (fork-session sess entry-id))
               (displayln (format "[forked session: ~a]" (session-id new-sess))))
   #:model-fn
   (lambda (arg)
     (define reg (hash-ref rt-config 'model-registry #f))
     (cond
       [(not reg) (displayln "[model registry not available]")]
       [(not arg)
        (displayln "Available models:")
        (for ([m (in-list (available-models reg))])
          (define marker (if (equal? (model-entry-name m) (default-model reg)) " *" "  "))
          (displayln
           (format "~a ~a (~a)" marker (model-entry-name m) (model-entry-provider-name m))))]
       [else
        (define resolution (resolve-model reg arg))
        (if resolution
            (displayln (format "[switched to model: ~a (provider: ~a)]"
                               (model-resolution-model-name resolution)
                               (model-resolution-provider-name resolution)))
            (displayln (format "Model not found: ~a. Use /model to list." arg)))]))
   #:sessions-fn (lambda (cmd out)
                   (define sdir (hash-ref rt-config 'session-dir #f))
                   (handle-sessions-interactive-command cmd out sdir))
   #:provider-name prov-name))

(define (run-single-shot cfg rt-config)
  (define sess (make-agent-session rt-config))
  (define bus (hash-ref rt-config 'event-bus))
  (subscribe! bus (make-terminal-subscriber))
  (run-cli-single cfg #:session-fn (lambda (prompt) (run-prompt! sess prompt))))

(define (run-resume cfg rt-config)
  (define sid (cli-config-session-id cfg))
  (define sess (resume-agent-session sid rt-config))
  (define bus (hash-ref rt-config 'event-bus))
  (subscribe! bus (make-terminal-subscriber))
  (run-cli-interactive
   cfg
   #:session-fn (lambda (prompt) (run-prompt! sess prompt))
   #:compact-fn (lambda () (displayln "[compaction requested]"))
   #:history-fn (lambda ([out (current-output-port)])
                  (define hist (session-history sess))
                  (for ([msg (in-list hist)])
                    (define role (message-role msg))
                    (define text-parts (filter text-part? (message-content msg)))
                    (define text (string-join (map text-part-text text-parts) " "))
                    (displayln (format "[~a] ~a" role text) out)))
   #:fork-fn (lambda (entry-id)
               (define new-sess (fork-session sess entry-id))
               (displayln (format "[forked session: ~a]" (session-id new-sess))))
   #:model-fn
   (lambda (arg)
     (define reg (hash-ref rt-config 'model-registry #f))
     (cond
       [(not reg) (displayln "[model registry not available]")]
       [(not arg)
        (displayln "Available models:")
        (for ([m (in-list (available-models reg))])
          (define marker (if (equal? (model-entry-name m) (default-model reg)) " *" "  "))
          (displayln
           (format "~a ~a (~a)" marker (model-entry-name m) (model-entry-provider-name m))))]
       [else
        (define resolution (resolve-model reg arg))
        (if resolution
            (displayln (format "[switched to model: ~a (provider: ~a)]"
                               (model-resolution-model-name resolution)
                               (model-resolution-provider-name resolution)))
            (displayln (format "Model not found: ~a. Use /model to list." arg)))]))
   #:sessions-fn (lambda (cmd out)
                   (define sdir (hash-ref rt-config 'session-dir #f))
                   (handle-sessions-interactive-command cmd out sdir))))
