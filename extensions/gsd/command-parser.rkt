#lang racket/base

;; extensions/gsd/command-parser.rkt -- Pure GSD command parsing (R-04/R-16)
;; STABILITY: evolving
;;
;; Extracts the parse phase from handle-execute-command into a pure function.
;; No I/O, no side effects -- just string -> AST transformation.

(require racket/match
         racket/string
         (only-in "../../util/command-helpers.rkt" extract-cmd-args)
         (only-in "../../util/command-types.rkt" shared-command shared-command?))

(provide parsed-gsd-command
         parsed-gsd-command?
         parsed-gsd-command-canonical-name
         parsed-gsd-command-args
         gsd-cmd-go
         gsd-cmd-go?
         gsd-cmd-go-wave-arg
         gsd-cmd-plan
         gsd-cmd-plan?
         gsd-cmd-plan-plan-text
         gsd-cmd-skip
         gsd-cmd-skip?
         gsd-cmd-skip-skip-arg
         gsd-cmd-done
         gsd-cmd-done?
         gsd-cmd-done-force?
         gsd-cmd-status
         gsd-cmd-status?
         gsd-cmd-replan
         gsd-cmd-replan?
         gsd-cmd-reset
         gsd-cmd-reset?
         gsd-cmd-wave-done
         gsd-cmd-wave-done?
         gsd-cmd-wave-done-wave-arg
         gsd-cmd-artifact
         gsd-cmd-artifact?
         gsd-cmd-artifact-artifact-name
         gsd-command-spec
         gsd-command-spec?
         gsd-command-spec-canonical
         gsd-command-spec-description
         gsd-command-spec-aliases
         parse-gsd-command
         gsd-command-specs
         aliases-for
         ;; v0.99.89 W3: pure intent boundary
         command-wave-intent
         gsd-command-intent
         go-wave-valid?)

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
  (list (gsd-command-spec "/plan" "Display current GSD plan" '("/p"))
        (gsd-command-spec "/state" "Display current project state" '("/s"))
        (gsd-command-spec "/handoff" "Display handoff status" '("/ho"))
        (gsd-command-spec "/go" "Start implementing the current plan" '("/implement" "/i"))
        (gsd-command-spec "/wave-done" "Mark wave N complete" '("/wd"))
        (gsd-command-spec "/done" "Archive completed plan" '("/d"))
        (gsd-command-spec "/replan" "Return to planning phase" '())
        (gsd-command-spec "/skip" "Skip a wave (usage: /skip N)" '())
        (gsd-command-spec "/reset" "Reset GSD to idle state" '())
        (gsd-command-spec "/gsd" "Show GSD workflow status" '())))

(define (aliases-for canonical)
  (define spec
    (findf (lambda (s) (equal? (gsd-command-spec-canonical s) canonical)) gsd-command-specs))
  (if spec
      (cons canonical (gsd-command-spec-aliases spec))
      (list canonical)))

;; -- Pure parser --

;; Parse a slash command string into a typed AST node.
;; Returns #f if the command is not recognized as a GSD command.
(define (parse-gsd-command cmd input-text)
  (define args-text (extract-cmd-args input-text))
  (cond
    [(member cmd (aliases-for "/go")) (gsd-cmd-go cmd args-text args-text)]
    [(member cmd (aliases-for "/plan"))
     (gsd-cmd-plan cmd args-text (if (> (string-length args-text) 0) args-text #f))]
    [(member cmd (aliases-for "/state")) (gsd-cmd-artifact cmd args-text "STATE")]
    [(member cmd (aliases-for "/handoff")) (gsd-cmd-artifact cmd args-text "HANDOFF")]
    [(equal? cmd "/gsd") (gsd-cmd-status cmd args-text)]
    [(equal? cmd "/replan") (gsd-cmd-replan cmd args-text)]
    [(equal? cmd "/skip") (gsd-cmd-skip cmd args-text args-text)]
    [(equal? cmd "/reset") (gsd-cmd-reset cmd args-text)]
    [(member cmd (aliases-for "/wave-done")) (gsd-cmd-wave-done cmd args-text args-text)]
    [(equal? cmd "/done")
     (gsd-cmd-done cmd args-text (and args-text (string-contains? args-text "--force")))]
    [else #f]))

;; ============================================================
;; Pure intent boundary (v0.99.89 W3)
;; ============================================================

;; The executor side (command-handlers.rkt) previously re-parsed the trailing
;; numeric token of /go N from the raw input text. That intent extraction
;; belongs to the pure parser layer: the executor consumes the parsed intent
;; instead of re-parsing. All functions here are I/O-free.

;; Extract the trailing numeric token from an arg/input string.
;; "/go 3" → 3; "/go" → #f; "/go 3 extra" → #f (last token non-numeric).
;; Byte-identical semantics to the executor's former requested-wave-index.
(define (command-wave-intent arg-text)
  (define trimmed (string-trim (or arg-text "")))
  (define parts (string-split trimmed))
  (define last-part (and (pair? parts) (list-ref parts (sub1 (length parts)))))
  (and last-part (regexp-match? #rx"^[0-9]+$" last-part) (string->number last-part)))

;; Classify a parsed command into a neutral intent spec:
;;   (go-wave n) | (go-all) | (skip-wave n) | (skip-all)
;;   (wave-done-wave n) | (wave-done-unspecified) | (done-force) | (done-default)
;;   (plan-text s) | (plan-display) | (display artifact)
;;   (status) | (replan) | (reset) | (unknown)
(define (gsd-command-intent parsed)
  (cond
    [(gsd-cmd-go? parsed)
     (define n (command-wave-intent (gsd-cmd-go-wave-arg parsed)))
     (if n
         (list 'go-wave n)
         (list 'go-all))]
    [(gsd-cmd-skip? parsed)
     (define n (command-wave-intent (gsd-cmd-skip-skip-arg parsed)))
     (if n
         (list 'skip-wave n)
         (list 'skip-all))]
    [(gsd-cmd-wave-done? parsed)
     (define n (command-wave-intent (gsd-cmd-wave-done-wave-arg parsed)))
     (if n
         (list 'wave-done-wave n)
         (list 'wave-done-unspecified))]
    [(gsd-cmd-done? parsed)
     (if (gsd-cmd-done-force? parsed)
         (list 'done-force)
         (list 'done-default))]
    [(gsd-cmd-plan? parsed)
     (define text (gsd-cmd-plan-plan-text parsed))
     (if text
         (list 'plan-text text)
         (list 'plan-display))]
    [(gsd-cmd-artifact? parsed) (list 'display (gsd-cmd-artifact-artifact-name parsed))]
    [(gsd-cmd-status? parsed) (list 'status)]
    [(gsd-cmd-replan? parsed) (list 'replan)]
    [(gsd-cmd-reset? parsed) (list 'reset)]
    [else (list 'unknown)]))

;; Pure mirror of the executor-side /go N assertion (go-orchestrator
;; assert-go-n): a requested wave is valid exactly when it equals the
;; earliest actionable wave. Keeps the /go N assertion semantics checkable at
;; the intent boundary without touching the campaign record.
(define (go-wave-valid? requested next-actionable)
  (and next-actionable (= requested next-actionable)))
