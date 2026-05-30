#lang racket/base

;; q/runtime/goal-types.rkt — Goal state data types for /goal autonomous loop
;;
;; Provides: goal-state, goal-check, evaluation-result, check-result structs
;; with contracts, constructors, predicates, and constants.

(require racket/contract
         racket/format
         "../util/ids.rkt")

;; Structs (no struct-out — use contracted constructors)
(provide goal-state
         goal-state?
         goal-check
         goal-check?
         evaluation-result
         evaluation-result?
         check-result
         check-result?
         ;; Constructors with contracts
         make-goal-state
         make-goal-check
         make-evaluation-result
         make-check-result
         ;; Accessors needed by goal-codec
         goal-state-id
         goal-state-goal-text
         goal-state-status
         goal-state-turns-used
         goal-state-max-turns
         goal-state-evaluator-model
         goal-state-evaluator-mode
         goal-state-checks
         goal-state-evaluations
         goal-state-last-evaluation
         goal-state-started-at
         goal-state-updated-at
         goal-state-meta
         goal-check-command
         goal-check-expected-exit
         goal-check-label
         check-result-label
         check-result-exit-code
         check-result-stdout
         check-result-stderr
         check-result-timed-out?
         check-result-elapsed-ms
         evaluation-result-achieved?
         evaluation-result-reason
         evaluation-result-check-results
         evaluation-result-model-used
         evaluation-result-token-cost
         ;; Predicates
         goal-status?
         evaluator-mode?
         ;; Constants
         DEFAULT-GOAL-MAX-TURNS
         DEFAULT-EVALUATOR-MODE
         NO-PROGRESS-THRESHOLD
         ;; Helpers
         string-truncate
         ;; I4 (v0.72.7): Moved from goal-runner.rkt
         goal-state-total-token-cost)

;; --------------------------------------------------
;; Constants
;; --------------------------------------------------

(define DEFAULT-GOAL-MAX-TURNS 8)
(define DEFAULT-EVALUATOR-MODE 'transcript)
(define NO-PROGRESS-THRESHOLD 3)

;; --------------------------------------------------
;; Predicates
;; --------------------------------------------------

(define (goal-status? v)
  (memq v '(active achieved failed cancelled)))

(define (evaluator-mode? v)
  (memq v '(transcript agent)))

;; --------------------------------------------------
;; Structs
;; --------------------------------------------------

;; A deterministic check the user specifies via --check
(struct goal-check
        (command ; string — shell command to run
         expected-exit ; (or/c exact-integer? #f) — expected exit code (default 0)
         label) ; string
  #:transparent)

;; Result of executing a single check
(struct check-result
        (label ; string
         exit-code ; exact-integer?
         stdout ; string (truncated to 500 chars)
         stderr ; string (truncated to 200 chars)
         timed-out? ; boolean
         elapsed-ms) ; exact-nonnegative-integer?
  #:transparent)

;; Result of evaluating a transcript against a goal
(struct evaluation-result
        (achieved? ; boolean
         reason ; string
         check-results ; (listof check-result?)
         model-used ; string
         token-cost) ; exact-nonnegative-integer?
  #:transparent)

;; Main goal state — persisted via JSONL
(struct goal-state
        (id ; string (UUID)
         goal-text ; string — user's goal condition
         status ; (or/c 'active 'achieved 'failed 'cancelled)
         turns-used ; exact-nonnegative-integer?
         max-turns ; exact-nonnegative-integer?
         evaluator-model ; string (default "auto")
         evaluator-mode ; (or/c 'transcript 'agent)
         checks ; (listof goal-check?)
         evaluations ; (listof evaluation-result?)
         last-evaluation ; (or/c evaluation-result? #f)
         started-at ; exact-nonnegative-integer? (epoch ms)
         updated-at ; exact-nonnegative-integer? (epoch ms)
         meta) ; (or/c hash? #f)
  #:transparent)

;; --------------------------------------------------
;; Helpers
;; --------------------------------------------------

(define (string-truncate s max-len)
  ;; Truncate string to max-len chars, appending "..." if truncated.
  ;; Note: output length is max-len + 3 when truncation occurs (max-len text + "..." indicator).
  (if (> (string-length s) max-len)
      (string-append (substring s 0 max-len) "...")
      s))

;; --------------------------------------------------
;; Constructors with contracts
;; --------------------------------------------------

(define/contract (make-goal-check #:command command #:label label #:expected-exit [expected-exit 0])
  (->* (#:command string? #:label string?) (#:expected-exit (or/c exact-integer? #f)) goal-check?)
  (goal-check command expected-exit label))

(define/contract (make-check-result #:label label
                                    #:exit-code exit-code
                                    #:stdout [stdout ""]
                                    #:stderr [stderr ""]
                                    #:timed-out? [timed-out? #f]
                                    #:elapsed-ms [elapsed-ms 0])
  (->*
   (#:label string? #:exit-code exact-integer?)
   (#:stdout string? #:stderr string? #:timed-out? boolean? #:elapsed-ms exact-nonnegative-integer?)
   check-result?)
  (check-result label
                exit-code
                (string-truncate stdout 500)
                (string-truncate stderr 200)
                timed-out?
                elapsed-ms))

(define/contract (make-evaluation-result #:achieved? achieved?
                                         #:reason reason
                                         #:check-results [check-results '()]
                                         #:model-used [model-used ""]
                                         #:token-cost [token-cost 0])
  (->* (#:achieved? boolean? #:reason string?)
       (#:check-results (listof check-result?)
                        #:model-used string?
                        #:token-cost exact-nonnegative-integer?)
       evaluation-result?)
  (evaluation-result achieved? reason check-results model-used token-cost))

(define/contract (make-goal-state #:goal-text goal-text
                                  #:status [status 'active]
                                  #:max-turns [max-turns DEFAULT-GOAL-MAX-TURNS]
                                  #:evaluator-model [evaluator-model "auto"]
                                  #:evaluator-mode [evaluator-mode DEFAULT-EVALUATOR-MODE]
                                  #:checks [checks '()]
                                  #:evaluations [evaluations '()]
                                  #:id [id #f]
                                  #:turns-used [turns-used 0]
                                  #:last-evaluation [last-evaluation #f]
                                  #:started-at [started-at #f]
                                  #:updated-at [updated-at #f]
                                  #:meta [meta #f])
  (->* (#:goal-text string?)
       (#:status goal-status?
                 #:max-turns exact-nonnegative-integer?
                 #:evaluator-model string?
                 #:evaluator-mode evaluator-mode?
                 #:checks (listof goal-check?)
                 #:evaluations (listof evaluation-result?)
                 #:id (or/c string? #f)
                 #:turns-used exact-nonnegative-integer?
                 #:last-evaluation (or/c evaluation-result? #f)
                 #:started-at (or/c exact-nonnegative-integer? #f)
                 #:updated-at (or/c exact-nonnegative-integer? #f)
                 #:meta (or/c hash? #f))
       goal-state?)
  (goal-state (or id (generate-id))
              goal-text
              status
              turns-used
              max-turns
              evaluator-model
              evaluator-mode
              checks
              evaluations
              last-evaluation
              (or started-at (inexact->exact (round (current-inexact-milliseconds))))
              (or updated-at (inexact->exact (round (current-inexact-milliseconds))))
              meta))

;; I4 (v0.72.7): Moved from goal-runner.rkt to be alongside goal-state struct
(define (goal-state-total-token-cost gs)
  (for/sum ([er (in-list (goal-state-evaluations gs))])
    (evaluation-result-token-cost er)))
