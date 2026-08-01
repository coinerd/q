#lang racket/base

;; q/runtime/goal-evidence.rkt — Evidence discipline + no-progress detection
;;
;; Two responsibilities:
;; 1. System prompt injection: forces worker agent to produce verifiable evidence
;; 2. No-progress detector: detects stall conditions from consecutive evaluations

(require racket/contract
         racket/match
         racket/format
         racket/string
         racket/list
         "goal-state.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide GOAL-EVIDENCE-SYSTEM-PROMPT
         GOAL-OPERATING-RULES
         operating-rules-block
         goal-system-instructions
         detect-no-progress
         consecutive-same-reason?
         evidence-prompt-for-goal
         ;; W3 v0.99.78 (G-5): verification evidence provenance
         evidence-provenance
         evidence-provenance?
         make-evidence-provenance
         evidence-provenance-evidence-id
         evidence-provenance-kind
         evidence-provenance-base-sha
         evidence-provenance-tree-hash
         evidence-provenance-captured-at
         evidence-provenance-result
         evidence-current?
         evidence-stale?
         reverify-instruction
         evidence-provenance->hash
         hash->evidence-provenance)

;; ============================================================
;; Evidence system prompt
;; ============================================================

(define GOAL-EVIDENCE-SYSTEM-PROMPT
  (string-append "You are working under an active q goal. Before stopping:\n"
                 "1. Report which goal criteria are satisfied with specific evidence\n"
                 "2. List exact checks you ran and their outputs (commands, exit codes, file diffs)\n"
                 "3. Describe remaining blockers precisely\n"
                 "4. If you believe the goal is achieved, state 'GOAL ACHIEVED' with proof\n"
                 "5. Never claim success without running at least one verification command"))

;; ============================================================
;; W4 v0.99.78 (G-6, G-7): Operating rules
;; ============================================================
;;
;; Scope/operating discipline injected into every turn prompt so a
;; future prompt edit cannot silently drop a rule. The shape of this
;; block is enforced by test-goal-workflow-contract.rkt.

(define GOAL-OPERATING-RULES
  (string-append "Operating rules (MANDATORY):\n"
                 "a. SEQUENTIAL WAVES: complete waves strictly in order. Each wave\n"
                 "   branches from the previous wave's branch; PRs merge strictly in order.\n"
                 "   Never start a later wave before the current wave's gate is green.\n"
                 "b. BACKGROUND GATES: long-running commands (gates, heavy builds, test\n"
                 "   suites) MUST run in the background (nohup CMD > log 2>&1 &) and be\n"
                 "   polled. Foreground long-running tool calls are FORBIDDEN — they\n"
                 "   freeze the goal turn.\n"
                 "c. TURN CAP: a wall-clock turn cap exists. A timed-out turn is recorded\n"
                 "   and evaluated as not-achieved. Do NOT rely on unbounded turns; keep\n"
                 "   each turn within the cap.\n"
                 "d. RE-VERIFY AFTER BASE CHANGE: verification evidence is bound to the\n"
                 "   base SHA and working-tree hash it was captured on. If the base or\n"
                 "   tree changed, stored evidence is STALE — re-run the verification on\n"
                 "   the current code state and record fresh evidence."))

;; Render the operating-rules block for injection into a turn prompt.
(define/contract (operating-rules-block)
  (-> string?)
  GOAL-OPERATING-RULES)
;; Returns a list of strings to append to system-instructions.
(define/contract (goal-system-instructions goal-st)
  (-> goal-state? (listof string?))
  (list GOAL-EVIDENCE-SYSTEM-PROMPT
        GOAL-OPERATING-RULES
        (format "Active goal: ~a (turn ~a/~a)"
                (goal-state-goal-text goal-st)
                (goal-state-turns-used goal-st)
                (goal-state-max-turns goal-st))))

;; Build a goal-specific evidence prompt for the continuation message.
(define/contract (evidence-prompt-for-goal goal-text evaluation)
  (-> string? (or/c evaluation-result? #f) string?)
  (define base (format "Continue working toward: ~a" goal-text))
  (if evaluation
      (format
       "~a\n\nPrevious evaluation: ~a\n\nProvide specific evidence: run commands, check files, show outputs."
       base
       (evaluation-result-reason evaluation))
      (format "~a\n\nProvide specific evidence: run commands, check files, show outputs." base)))

;; ============================================================
;; No-progress detection
;; ============================================================

;; Check if an evaluation result is an evaluator infrastructure error
;; (HTTP crash, timeout, etc.) rather than a genuine "not achieved" verdict.
(define (evaluator-infrastructure-error? er)
  (define r (evaluation-result-reason er))
  (or (string-prefix? r "Agent evaluator error:") (string-prefix? r "Evaluator error:")))

;; Returns #t if the last NO-PROGRESS-THRESHOLD evaluations have the same reason
;; AND none achieved the goal.
;; Evaluator infrastructure errors (HTTP crashes, timeouts) are excluded — they
;; indicate the evaluator itself failed, not that the goal made no progress.
(define/contract (consecutive-same-reason? evaluations)
  (-> (listof evaluation-result?) boolean?)
  (cond
    [(< (length evaluations) NO-PROGRESS-THRESHOLD) #f]
    [else
     (define last-n (take-right evaluations NO-PROGRESS-THRESHOLD))
     ;; Filter out evaluator infrastructure errors — these are NOT lack of progress
     (define meaningful (filter (lambda (e) (not (evaluator-infrastructure-error? e))) last-n))
     (cond
       [(< (length meaningful) NO-PROGRESS-THRESHOLD) #f]
       [else
        (define reasons (map evaluation-result-reason meaningful))
        (define all-failed? (andmap (lambda (e) (not (evaluation-result-achieved? e))) meaningful))
        (define all-same? (andmap (lambda (r) (equal? r (car reasons))) (cdr reasons)))
        (and all-failed? all-same?)])]))

;; Detect no-progress from a list of evaluation results.
;; Returns #t if stall is detected, #f otherwise.
(define/contract (detect-no-progress evaluations)
  (-> (listof evaluation-result?) boolean?)
  (cond
    [(< (length evaluations) NO-PROGRESS-THRESHOLD) #f]
    [else (consecutive-same-reason? evaluations)]))

;; ============================================================
;; W3 v0.99.78 (G-5): Verification evidence provenance
;; ============================================================
;;
;; A provenance record binds a verification result to the exact code
;; state it was produced on: the base commit SHA AND the working-tree
;; hash. If either changes, the evidence is STALE and must be rejected
;; until re-verified.

(struct evidence-provenance
        (evidence-id ;; string — unique id of the verification run
         kind ;; symbol: 'fast-gate | 'check | 'focused
         base-sha ;; string — git base commit SHA at capture time
         tree-hash ;; string — working-tree hash at capture time
         captured-at ;; exact-nonnegative-integer — unix ms timestamp
         result) ;; any/c — check result payload ("PASS", check-result list, ...)
  #:transparent)

;; Contracted constructor
(define/contract (make-evidence-provenance #:evidence-id evidence-id
                                           #:kind kind
                                           #:base-sha base-sha
                                           #:tree-hash tree-hash
                                           #:captured-at captured-at
                                           #:result result)
  (->* (#:evidence-id string?
                      #:kind (or/c 'fast-gate 'check 'focused)
                      #:base-sha string?
                      #:tree-hash string?
                      #:captured-at exact-nonnegative-integer?
                      #:result any/c)
       ()
       evidence-provenance?)
  (evidence-provenance evidence-id kind base-sha tree-hash captured-at result))

;; Is the evidence still current for the given base SHA and tree hash?
(define/contract (evidence-current? ev base-sha tree-hash)
  (-> evidence-provenance? string? string? boolean?)
  (and (equal? (evidence-provenance-base-sha ev) base-sha)
       (equal? (evidence-provenance-tree-hash ev) tree-hash)))

;; Is the evidence stale (base or tree moved since capture)?
(define/contract (evidence-stale? ev base-sha tree-hash)
  (-> evidence-provenance? string? string? boolean?)
  (not (evidence-current? ev base-sha tree-hash)))

;; Build the re-verify instruction the goal loop injects into the next
;; turn when stored evidence is stale (base changed A -> B).
(define/contract (reverify-instruction ev new-base-sha new-tree-hash)
  (-> evidence-provenance? string? string? string?)
  (format (string-append "RE-VERIFY REQUIRED: stored evidence ~a (captured on base ~a, tree ~a) "
                         "is STALE because the base changed to ~a (tree ~a). Do NOT accept the old "
                         "result. Re-run the verification on the current code state and record "
                         "fresh evidence.")
          (evidence-provenance-evidence-id ev)
          (evidence-provenance-base-sha ev)
          (evidence-provenance-tree-hash ev)
          new-base-sha
          new-tree-hash))

;; ------------------------------------------------------------
;; Hash round-trip for persistence (kind `goal.evidence` entries)
;; ------------------------------------------------------------

(define/contract (evidence-provenance->hash ev)
  (-> evidence-provenance? hash?)
  (hash 'kind
        (format "~a" (evidence-provenance-kind ev))
        'evidence-id
        (evidence-provenance-evidence-id ev)
        'base-sha
        (evidence-provenance-base-sha ev)
        'tree-hash
        (evidence-provenance-tree-hash ev)
        'captured-at
        (evidence-provenance-captured-at ev)
        'result
        (format "~a" (evidence-provenance-result ev))))

(define/contract (hash->evidence-provenance h)
  (-> hash? evidence-provenance?)
  (define (sym-or-string->symbol v)
    (if (symbol? v)
        v
        (string->symbol (format "~a" v))))
  (evidence-provenance (hash-ref h 'evidence-id (lambda () (hash-ref h "evidence-id")))
                       (sym-or-string->symbol (hash-ref h 'kind (lambda () (hash-ref h "kind"))))
                       (hash-ref h 'base-sha (lambda () (hash-ref h "base-sha")))
                       (hash-ref h 'tree-hash (lambda () (hash-ref h "tree-hash")))
                       (hash-ref h 'captured-at (lambda () (hash-ref h "captured-at")))
                       (hash-ref h 'result (lambda () (hash-ref h "result")))))
