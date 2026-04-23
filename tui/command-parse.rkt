#lang racket/base

;; q/tui/command-parse.rkt — Lightweight slash command name→symbol parsing
;;
;; Single source of truth for command name → dispatch symbol mapping.
;; No dependency on render.rkt, input.rkt, or commands.rkt.
;; Consumed by palette.rkt (for alias resolution) and input.rkt (for parsing).

(require racket/string)

(provide make-command-table
         parse-command-name
         resolve-command-name)

;; Command table: maps command name strings (including aliases) to
;; (cons canonical-symbol arg-kind)
;; arg-kind: 'none = never takes args, 'optional = works with or without, 'required = needs arg
;; e.g. "/help" → (cons 'help 'none), "/switch" → (cons 'switch 'required)

(define (make-command-table)
  ;; General
  (hash "/help"
        (cons 'help 'none)
        "/h"
        (cons 'help 'none)
        "/?"
        (cons 'help 'none)
        "/quit"
        (cons 'quit 'none)
        "/q"
        (cons 'quit 'none)
        "/exit"
        (cons 'quit 'none)
        "/clear"
        (cons 'clear 'none)
        "/cls"
        (cons 'clear 'none)
        ;; Session
        "/compact"
        (cons 'compact 'none)
        "/interrupt"
        (cons 'interrupt 'none)
        "/stop"
        (cons 'interrupt 'none)
        "/cancel"
        (cons 'interrupt 'none)
        "/branches"
        (cons 'branches 'none)
        "/leaves"
        (cons 'leaves 'none)
        "/switch"
        (cons 'switch 'required)
        "/children"
        (cons 'children 'required)
        "/history"
        (cons 'history 'none)
        "/fork"
        (cons 'fork 'optional)
        "/sessions"
        (cons 'sessions 'optional)
        ;; Model
        "/tree"
        (cons 'tree 'none)
        "/name"
        (cons 'name 'optional)
        "/model"
        (cons 'model 'optional)
        "/m"
        (cons 'model 'optional)
        "/retry"
        (cons 'retry 'none)
        "/r"
        (cons 'retry 'none)
        ;; Extensions (multi-arg: parsed manually in handler)
        "/activate"
        (cons 'activate 'none)
        "/a"
        (cons 'activate 'none)))

;; Parse a slash command string into a dispatch symbol + args list.
;; Returns: symbol | (list symbol args...) | #f
(define (parse-command-name text)
  (define trimmed (string-trim text))
  (cond
    [(string=? trimmed "") #f]
    [(not (char=? (string-ref trimmed 0) #\/)) #f]
    [else
     (define parts (string-split trimmed))
     (define cmd (car parts))
     (define args (cdr parts))
     (define table (make-command-table))
     (define entry (hash-ref table cmd #f))
     (cond
       [(not entry) 'unknown]
       [(eq? (cdr entry) 'none) (car entry)]
       [(eq? (cdr entry) 'optional)
        (if (null? args)
            (car entry)
            `(,(car entry) ,(car args)))]
       [(eq? (cdr entry) 'required)
        (if (null? args)
            (list (string->symbol (format "~a-error" (symbol->string (car entry))))
                  (case (car entry)
                    [(switch) "Usage: /switch <branch-id>"]
                    [(children) "Usage: /children <node-id>"]
                    [else (format "Usage: /~a <id>" (symbol->string (car entry)))]))
            `(,(car entry) ,(car args)))]
       [else (car entry)])]))

;; Resolve a command name (with alias) to canonical symbol.
;; Returns: symbol or #f
(define (resolve-command-name cmd-str)
  (define table (make-command-table))
  (define entry (hash-ref table cmd-str #f))
  (and entry (car entry)))
