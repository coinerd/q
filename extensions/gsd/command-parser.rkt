#lang racket/base

;; extensions/gsd/command-parser.rkt — Pure GSD command parsing (R-04/R-16)
;; STABILITY: evolving
;;
;; Extracts the parse phase from handle-execute-command into a pure function.
;; No I/O, no side effects — just string → AST transformation.

(require racket/match
         racket/string)

(provide (struct-out parsed-gsd-command)
         (struct-out gsd-cmd-go)
         (struct-out gsd-cmd-plan)
         (struct-out gsd-cmd-skip)
         (struct-out gsd-cmd-done)
         (struct-out gsd-cmd-status)
         (struct-out gsd-cmd-replan)
         (struct-out gsd-cmd-reset)
         (struct-out gsd-cmd-wave-done)
         (struct-out gsd-cmd-artifact)
         parsed-gsd-command?
         parse-gsd-command)

;; ── Base struct ──

(struct parsed-gsd-command (canonical-name args) #:transparent)

;; ── Subtypes ──

(struct gsd-cmd-go parsed-gsd-command (wave-arg) #:transparent)
(struct gsd-cmd-plan parsed-gsd-command (plan-text) #:transparent)
(struct gsd-cmd-skip parsed-gsd-command (skip-arg) #:transparent)
(struct gsd-cmd-done parsed-gsd-command (force?) #:transparent)
(struct gsd-cmd-status parsed-gsd-command () #:transparent)
(struct gsd-cmd-replan parsed-gsd-command () #:transparent)
(struct gsd-cmd-reset parsed-gsd-command () #:transparent)
(struct gsd-cmd-wave-done parsed-gsd-command (wave-arg) #:transparent)
(struct gsd-cmd-artifact parsed-gsd-command (artifact-name) #:transparent)

;; ── Pure parser ──

;; Command alias tables — pure data, no I/O.
(define go-aliases '("/go" "/implement" "/i"))
(define plan-aliases '("/plan" "/p"))
(define state-aliases '("/state" "/s"))
(define handoff-aliases '("/handoff" "/ho"))
(define wave-done-aliases '("/wave-done" "/wd"))

(define (extract-cmd-args input-text)
  (define trimmed (string-trim input-text))
  (if (and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\/))
      (let ([parts (string-split trimmed)])
        (if (>= (length parts) 2)
            (string-trim (string-join (cdr parts) " "))
            ""))
      ""))

;; Parse a slash command string into a typed AST node.
;; Returns #f if the command is not recognized as a GSD command.
(define (parse-gsd-command cmd input-text)
  (define args-text (extract-cmd-args input-text))
  (cond
    [(member cmd go-aliases) (gsd-cmd-go cmd args-text args-text)]
    [(member cmd plan-aliases)
     (gsd-cmd-plan cmd args-text (if (> (string-length args-text) 0) args-text #f))]
    [(member cmd state-aliases) (gsd-cmd-artifact cmd args-text "STATE")]
    [(member cmd handoff-aliases) (gsd-cmd-artifact cmd args-text "HANDOFF")]
    [(equal? cmd "/gsd") (gsd-cmd-status cmd args-text)]
    [(equal? cmd "/replan") (gsd-cmd-replan cmd args-text)]
    [(equal? cmd "/skip") (gsd-cmd-skip cmd args-text args-text)]
    [(equal? cmd "/reset") (gsd-cmd-reset cmd args-text)]
    [(member cmd wave-done-aliases) (gsd-cmd-wave-done cmd args-text args-text)]
    [(equal? cmd "/done")
     (gsd-cmd-done cmd args-text (and args-text (string-contains? args-text "--force")))]
    ;; /plan without args → artifact display
    [(member cmd plan-aliases) (gsd-cmd-artifact cmd args-text "PLAN")]
    [else #f]))
