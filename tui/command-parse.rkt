#lang racket/base

;; q/tui/command-parse.rkt — Lightweight slash command name→symbol parsing
;;
;; Single source of truth for command name → dispatch symbol mapping.
;; No dependency on render.rkt, input.rkt, or commands.rkt.
;; Consumed by palette.rkt (for alias resolution) and input.rkt (for parsing).

(require racket/contract
         racket/string)

(provide parsed-command) ; raw struct constructor for use in or/c contracts elsewhere
(provide (contract-out
          [make-command-table (-> (hash/c string? (cons/c symbol? symbol?)))]
          [parse-command-name (-> string? (or/c parsed-command? symbol? #f))]
          [resolve-command-name (-> string? (or/c symbol? #f))]
          ;; R-17: Structured command result / R-0: test structural equality
          [parsed-command? (-> any/c boolean?)]
          [parsed-command-canonical-name (-> parsed-command? symbol?)]
          [parsed-command-args (-> parsed-command? list?)]
          [parsed-command-arg-kind (-> parsed-command? symbol?)]
          ;; Pipeline stage extraction for testability
          [tokenize (-> string? any/c)]
          [lookup-command-entry
           (-> string? (hash/c string? (cons/c symbol? symbol?)) (or/c #f (cons/c symbol? symbol?)))]
          [validate-args (-> (cons/c symbol? symbol?) (listof string?) parsed-command?)]))

;; Command table: maps command name strings (including aliases) to
;; (cons canonical-symbol arg-kind)
;; arg-kind: 'none = never takes args, 'optional = works with or without, 'required = needs arg
;; e.g. "/help" → (cons 'help 'none), "/switch" → (cons 'switch 'required)

;; R-17: Structured command result replacing ad-hoc symbol/list returns.
(struct parsed-command (canonical-name args arg-kind) #:transparent)

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
        ;; Session status
        "/status"
        (cons 'status 'none)
        "/st"
        (cons 'status 'none)
        "/info"
        (cons 'status 'none)
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
        (cons 'activate 'none)
        ;; Reload
        "/reload"
        (cons 'reload 'none)))

;; Parse a slash command string into a dispatch symbol + args list.
;; Returns: symbol | (list symbol args...) | #f
;; R-17: Returns parsed-command or #f for non-commands.
;; For backward compat, also returns 'unknown for unrecognized slash commands.
;; F13: Pipeline stage extraction for testability.
;; tokenize: string -> (values cmd-string (listof string)) or #f
(define (tokenize text)
  (define trimmed (string-trim text))
  (cond
    [(string=? trimmed "") #f]
    [(not (char=? (string-ref trimmed 0) #\/)) #f]
    [else
     (define parts (string-split trimmed))
     (values (car parts) (cdr parts))]))

;; lookup: cmd-string table -> (cons canonical arg-kind) or #f
(define (lookup-command-entry cmd table)
  (hash-ref table cmd #f))

;; validate-args: entry args -> parsed-command
(define (validate-args entry args)
  (cond
    [(eq? (cdr entry) 'none) (parsed-command (car entry) '() 'none)]
    [(eq? (cdr entry) 'optional)
     (parsed-command (car entry)
                     (if (null? args)
                         '()
                         (list (car args)))
                     'optional)]
    [(eq? (cdr entry) 'required)
     (if (null? args)
         (parsed-command (string->symbol (format "~a-error" (symbol->string (car entry))))
                         (list (case (car entry)
                                 [(switch) "Usage: /switch <branch-id>"]
                                 [(children) "Usage: /children <node-id>"]
                                 [else (format "Usage: /~a <id>" (symbol->string (car entry)))]))
                         'error)
         (parsed-command (car entry) (list (car args)) 'required))]
    [else (parsed-command (car entry) '() 'none)]))

(define (parse-command-name text)
  ;; v0.47.0 (D-5): Cleaner handling of tokenize multi-value or #f return.
  ;; tokenize returns (values cmd-string args-list) or a single #f.
  (define result (call-with-values (λ () (tokenize text)) list))
  (cond
    [(null? result) #f]
    [(= (length result) 2)
     (define cmd (car result))
     (define args (cadr result))
     (define table (make-command-table))
     (define entry (lookup-command-entry cmd table))
     (cond
       [(not entry) 'unknown]
       [else (validate-args entry args)])]
    [else #f]))

;; Resolve a command name (with alias) to canonical symbol.
;; Returns: symbol or #f
(define (resolve-command-name cmd-str)
  (define table (make-command-table))
  (define entry (hash-ref table cmd-str #f))
  (and entry (car entry)))
