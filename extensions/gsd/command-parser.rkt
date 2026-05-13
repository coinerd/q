#lang racket/base

;; extensions/gsd/command-parser.rkt -- Pure GSD command parsing (R-04/R-16)
;; STABILITY: evolving
;;
;; Extracts the parse phase from handle-execute-command into a pure function.
;; No I/O, no side effects -- just string -> AST transformation.

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
         (struct-out gsd-command-spec)
         parsed-gsd-command?
         parse-gsd-command
         gsd-command-specs
         aliases-for)

;; -- Base struct --

(struct parsed-gsd-command (canonical-name args) #:transparent)

;; -- Subtypes --

(struct gsd-cmd-go parsed-gsd-command (wave-arg) #:transparent)
(struct gsd-cmd-plan parsed-gsd-command (plan-text) #:transparent)
(struct gsd-cmd-skip parsed-gsd-command (skip-arg) #:transparent)
(struct gsd-cmd-done parsed-gsd-command (force?) #:transparent)
(struct gsd-cmd-status parsed-gsd-command () #:transparent)
(struct gsd-cmd-replan parsed-gsd-command () #:transparent)
(struct gsd-cmd-reset parsed-gsd-command () #:transparent)
(struct gsd-cmd-wave-done parsed-gsd-command (wave-arg) #:transparent)
(struct gsd-cmd-artifact parsed-gsd-command (artifact-name) #:transparent)

;; -- Command spec (single source of truth for aliases) --

(struct gsd-command-spec (canonical description aliases) #:transparent)

(define gsd-command-specs
  (list
   (gsd-command-spec "/plan"      "Display current GSD plan"            '(("/p")))
   (gsd-command-spec "/state"     "Display current project state"       '(("/s")))
   (gsd-command-spec "/handoff"   "Display handoff status"              '(("/ho")))
   (gsd-command-spec "/go"        "Start implementing the current plan" '(("/implement") ("/i")))
   (gsd-command-spec "/wave-done" "Mark wave N complete, update PLAN.md and STATE.md" '(("/wd")))
   (gsd-command-spec "/done"      "Archive completed plan"              '(("/d")))))

(define (aliases-for canonical)
  (define spec
    (findf (lambda (s) (equal? (gsd-command-spec-canonical s) canonical))
           gsd-command-specs))
  (if spec (cons canonical (apply append (gsd-command-spec-aliases spec))) (list canonical)))

;; -- Pure parser --

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
    [(member cmd (aliases-for "/go"))        (gsd-cmd-go cmd args-text args-text)]
    [(member cmd (aliases-for "/plan"))
     (gsd-cmd-plan cmd args-text (if (> (string-length args-text) 0) args-text #f))]
    [(member cmd (aliases-for "/state"))     (gsd-cmd-artifact cmd args-text "STATE")]
    [(member cmd (aliases-for "/handoff"))   (gsd-cmd-artifact cmd args-text "HANDOFF")]
    [(equal? cmd "/gsd")                     (gsd-cmd-status cmd args-text)]
    [(equal? cmd "/replan")                  (gsd-cmd-replan cmd args-text)]
    [(equal? cmd "/skip")                    (gsd-cmd-skip cmd args-text args-text)]
    [(equal? cmd "/reset")                   (gsd-cmd-reset cmd args-text)]
    [(member cmd (aliases-for "/wave-done")) (gsd-cmd-wave-done cmd args-text args-text)]
    [(equal? cmd "/done")
     (gsd-cmd-done cmd args-text (and args-text (string-contains? args-text "--force")))]
    [else #f]))
