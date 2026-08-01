#lang racket
;; W4 v0.99.78 (G-6, G-7): Goal-loop operating guidance contract tests.
;;
;; Enforces that the goal turn prompt always contains the operating-rules
;; block (sequential waves, background gates, turn cap, re-verify after
;; base change) so a future prompt edit cannot silently drop a rule.
;; Also guards the positives from G-1..G-3 (evidence prompt, turn budget,
;; report discipline) against regression.

(require rackunit)
(require "../runtime/goal/goal-evidence.rkt")
(require "../runtime/goal/goal-state.rkt")

;; ------------------------------------------------------------
;; Operating-rules block must be present in the turn prompt
;; ------------------------------------------------------------

(define operating-rules (operating-rules-block))

(define (contains-all? s frags)
  (andmap (lambda (f) (string-contains? s f)) frags))

(test-case "W4: operating-rules block contains all four mandatory rules"
  (check-true (contains-all?
               operating-rules
               '("SEQUENTIAL WAVES" "BACKGROUND GATES" "TURN CAP" "RE-VERIFY AFTER BASE CHANGE"))))

(test-case "W4: sequential wave ordering rule (G-6)"
  (check-true (contains-all? operating-rules
                             '("branches from the previous wave's branch"
                               "PRs merge strictly in order"))))

(test-case "W4: background gate rule (G-7)"
  (check-true (contains-all? operating-rules '("nohup" "background" "polled" "FORBIDDEN"))))

(test-case "W4: turn cap rule (W0 cross-reference)"
  (check-true (contains-all? operating-rules
                             '("wall-clock turn cap" "timed-out turn" "not-achieved"))))

(test-case "W4: re-verify after base change rule (W3 cross-reference)"
  (check-true (contains-all? operating-rules '("base SHA" "STALE" "re-run the verification"))))

;; ------------------------------------------------------------
;; Prompt shape contract (Decision D5): every turn prompt built
;; from goal-system-instructions must include the rules block.
;; ------------------------------------------------------------

(test-case "W4: goal-system-instructions includes the operating-rules block"
  (define goal-st (make-goal-state #:goal-text "test goal" #:max-turns 4))
  (define blocks (goal-system-instructions goal-st))
  (check-not-false (member operating-rules blocks))
  ;; Evidence prompt (G-1 positive guard) must also be present.
  (check-not-false (member GOAL-EVIDENCE-SYSTEM-PROMPT blocks))
  ;; Turn budget (G-2 positive guard): active-goal line shows turn counts.
  (check-not-false (ormap (lambda (b) (string-contains? b "turn 0/4")) blocks)))

(test-case "W4: GOAL-OPERATING-RULES constant is non-empty and stable"
  (check-true (> (string-length GOAL-OPERATING-RULES) 100))
  (check-equal? GOAL-OPERATING-RULES (operating-rules-block)))

;; ------------------------------------------------------------
;; Config parsing (Decision D4): --turns N and --turn-timeout-secs M
;; ------------------------------------------------------------

(define (parse-turn-flags args)
  ;; Mirrors q/tui/commands.rkt parse of --turns/--turn-timeout-secs.
  ;; Returns (values turns turn-timeout-secs) with defaults applied.
  (define parts (string-split (string-join args " ")))
  (define (flag-value flag default)
    (define idx (index-of parts flag))
    (if (and idx (< (add1 idx) (length parts)))
        (let ([v (string->number (list-ref parts (add1 idx)))]) (if v v default))
        default))
  (values (flag-value "--turns" 8) (flag-value "--turn-timeout-secs" 1800)))

(test-case "W4: --turns N parses"
  (define-values (turns tmo) (parse-turn-flags '("--turns" "4")))
  (check-equal? turns 4)
  (check-equal? tmo 1800))

(test-case "W4: --turn-timeout-secs M parses"
  (define-values (turns tmo) (parse-turn-flags '("--turn-timeout-secs" "120")))
  (check-equal? turns 8)
  (check-equal? tmo 120))

(test-case "W4: defaults applied when flags absent"
  (define-values (turns tmo) (parse-turn-flags '()))
  (check-equal? turns 8)
  (check-equal? tmo 1800))

(test-case "W4: both flags parse together"
  (define-values (turns tmo) (parse-turn-flags '("--turns" "3" "--turn-timeout-secs" "60")))
  (check-equal? turns 3)
  (check-equal? tmo 60))
