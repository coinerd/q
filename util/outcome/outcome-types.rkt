#lang racket/base

;; util/outcome/outcome-types.rkt
;; STABILITY: internal (importable by tools, agent, runtime, extensions)
;;
;; W3A (#8940): Typed outcome contracts at the tool boundary.
;;
;; Problem: tool execution produces (tool-call, tool-result) pairs with raw
;; content strings, shell arguments, and possibly secrets. The task-ledger
;; must receive SAFE typed outcomes — no raw content, no credentials.
;;
;; This module defines:
;;   - typed-tool-outcome: a safe, correlated projection of a tool execution
;;   - classify-tool-outcome: maps (tool-call, tool-result) → typed-tool-outcome
;;   - classify-command-class: heuristically classifies shell commands
;;   - extract-safe-path: extracts file paths from tool arguments
;;
;; Layering: this module is in util/ — importable by tools, agent, runtime,
;; and extensions without violating the tools→runtime dependency boundary.
;; Tools/scheduler emit typed outcomes; runtime/wiring translates them into
;; ledger events (W3B).

(require racket/contract
         racket/match
         racket/set
         racket/string
         (only-in "../tool/tool-types.rkt"
                  tool-call?
                  tool-call-id
                  tool-call-name
                  tool-call-arguments
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?))

;; ============================================================
;; Enums
;; ============================================================

(define outcome-kinds
  (set 'prompt
       'artifact
       'test
       'git-status
       'commit
       'push
       'conclusion
       'task-transition
       'checkpoint
       'archive))

(define outcome-statuses (set 'success 'error 'cancelled 'timeout 'partial))

(define (valid-outcome-kind? k)
  (and (symbol? k) (set-member? outcome-kinds k)))

(define (valid-outcome-status? s)
  (and (symbol? s) (set-member? outcome-statuses s)))

;; ============================================================
;; typed-tool-outcome struct
;; ============================================================

(struct typed-tool-outcome
        (kind ; one of outcome-kinds
         tool-call-id ; string or #f (correlation)
         tool-name ; string
         status ; one of outcome-statuses
         payload ; hash: safe, typed, no raw content/secrets
         timestamp) ; exact integer or #f (audit only)
  #:transparent
  #:constructor-name make-typed-outcome-internal)

;; Public keyword-based constructor with validation.
(define (make-typed-tool-outcome #:kind kind
                                 #:tool-call-id tool-call-id
                                 #:tool-name tool-name
                                 #:status status
                                 #:payload payload
                                 #:timestamp timestamp)
  (unless (valid-outcome-kind? kind)
    (error 'make-typed-tool-outcome "invalid outcome kind: ~v" kind))
  (unless (or (not tool-call-id) (string? tool-call-id))
    (error 'make-typed-tool-outcome "tool-call-id must be string or #f: ~v" tool-call-id))
  (unless (or (not tool-name) (string? tool-name))
    (error 'make-typed-tool-outcome "tool-name must be string or #f: ~v" tool-name))
  (unless (valid-outcome-status? status)
    (error 'make-typed-tool-outcome "invalid outcome status: ~v" status))
  (unless (hash? payload)
    (error 'make-typed-tool-outcome "payload must be a hash: ~v" payload))
  (unless (or (not timestamp) (exact-integer? timestamp))
    (error 'make-typed-tool-outcome "timestamp must be exact integer or #f: ~v" timestamp))
  (make-typed-outcome-internal kind tool-call-id tool-name status payload timestamp))

;; ============================================================
;; Command classification (for bash tool)
;; ============================================================

(provide classify-command-class)

(define command-classes
  '((raco-test . "raco test") (raco-make . "raco make")
                              (raco-fmt . "raco fmt")
                              (git-status . "git status")
                              (git-commit . "git commit")
                              (git-push . "git push")
                              (git-diff . "git diff")
                              (git-add . "git add")))

;; Classify a shell command string into a canonical command class symbol.
;; Returns 'other if no known class matches.
(define (classify-command-class command)
  (if (not (string? command))
      'other
      (let ([norm (string-downcase (string-trim command))])
        (or (for/first ([pair (in-list command-classes)]
                        #:when (string-prefix? norm (cdr pair)))
              (car pair))
            'other))))

;; ============================================================
;; Safe extraction helpers
;; ============================================================

(provide extract-safe-path)

;; Extract a file path from tool arguments if present.
;; Handles both hash and non-hash arguments.
(define (extract-safe-path args)
  (cond
    [(and (hash? args) (hash-has-key? args 'path))
     (define p (hash-ref args 'path))
     (and (string? p) p)]
    [(and (hash? args) (hash-has-key? args 'file))
     (define p (hash-ref args 'file))
     (and (string? p) p)]
    [else #f]))

;; ============================================================
;; Outcome classifier
;; ============================================================

(provide classify-tool-outcome)

;; Classify a (tool-call, tool-result) pair into a typed-tool-outcome.
;; Returns #f if the outcome cannot be classified (unknown tool, no args).
;; The payload contains ONLY safe, typed fields — never raw content.
(define (classify-tool-outcome tc tr)
  (define name (and (tool-call? tc) (tool-call-name tc)))
  (define call-id (and (tool-call? tc) (tool-call-id tc)))
  (define args (and (tool-call? tc) (tool-call-arguments tc)))
  (define is-error? (and (tool-result? tr) (tool-result-is-error? tr)))
  (define details (and (tool-result? tr) (tool-result-details tr)))

  ;; Derive exit code from details if available.
  (define exit-code
    (and (hash? details) (hash-has-key? details 'exit-code) (hash-ref details 'exit-code)))

  ;; Map error/exit-code to status.
  (define status (outcome-status is-error? exit-code))

  (define kind+payload
    (cond
      ;; Unable to classify: no tool name or no arguments
      [(or (not name) (not args)) #f]
      ;; edit / write → artifact
      [(member name '("edit" "write")) (cons 'artifact (artifact-payload args))]
      ;; bash → depends on command class
      [(equal? name "bash") (bash-outcome args details status)]
      ;; record_conclusion / save_conclusion → conclusion
      [(member name '("record_conclusion" "save_conclusion")) (cons 'conclusion (hash))]
      [else #f]))

  (and kind+payload
       (make-typed-tool-outcome #:kind (car kind+payload)
                                #:tool-call-id call-id
                                #:tool-name name
                                #:status status
                                #:payload (cdr kind+payload)
                                #:timestamp #f)))

;; Determine outcome status from error flag and exit code.
(define (outcome-status is-error? exit-code)
  (cond
    [is-error? 'error]
    [(and (exact-integer? exit-code) (not (zero? exit-code))) 'error]
    [else 'success]))

;; Build the artifact payload: path + monotonic generation placeholder.
(define (artifact-payload args)
  (define path (extract-safe-path args))
  (if path
      (hash 'path path)
      (hash)))

;; Build bash outcome based on command classification.
;; This is a second-order classifier that re-examines the command.
(define (bash-outcome args details status)
  (define command (and (hash? args) (hash-has-key? args 'command) (hash-ref args 'command)))
  (define cmd-class (classify-command-class command))
  (define exit-code
    (and (hash? details) (hash-has-key? details 'exit-code) (hash-ref details 'exit-code)))
  (cons (command-class->outcome-kind cmd-class)
        (hash 'command-class cmd-class 'exit-code exit-code 'status status)))

;; Map a command class to an outcome kind.
(define (command-class->outcome-kind cmd-class)
  (case cmd-class
    [(raco-test) 'test]
    [(git-status) 'git-status]
    [(git-commit) 'commit]
    [(git-push) 'push]
    [(raco-make raco-fmt) 'test]
    [else 'test]))

;; ============================================================
;; Exports
;; ============================================================

(provide outcome-kinds
         outcome-statuses
         valid-outcome-kind?
         valid-outcome-status?
         typed-tool-outcome
         typed-tool-outcome?
         typed-tool-outcome-kind
         typed-tool-outcome-tool-call-id
         typed-tool-outcome-tool-name
         typed-tool-outcome-status
         typed-tool-outcome-payload
         typed-tool-outcome-timestamp
         make-typed-tool-outcome)
