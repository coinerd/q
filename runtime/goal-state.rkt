#lang racket/base

;; q/runtime/goal-state.rkt — Goal state data model for /goal autonomous loop
;;
;; Provides: goal-state, goal-check, evaluation-result, check-result structs
;; with contracts, serialization (hash round-trip), and construction helpers.

(require racket/contract
         racket/match
         racket/format
         "../util/ids.rkt")

;; Structs
(provide (struct-out goal-state)
         (struct-out goal-check)
         (struct-out evaluation-result)
         (struct-out check-result)
         ;; Constructors with contracts
         make-goal-state
         make-goal-check
         make-evaluation-result
         make-check-result
         ;; Serialization
         goal-state->hash
         hash->goal-state
         goal-check->hash
         hash->goal-check
         evaluation-result->hash
         hash->evaluation-result
         check-result->hash
         hash->check-result
         ;; Predicates
         goal-status?
         evaluator-mode?
         ;; Constants
         DEFAULT-GOAL-MAX-TURNS
         DEFAULT-EVALUATOR-MODE
         NO-PROGRESS-THRESHOLD)

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
         last-evaluation ; (or/c evaluation-result? #f)
         started-at ; exact-nonnegative-integer? (epoch ms)
         updated-at ; exact-nonnegative-integer? (epoch ms)
         meta) ; (or/c hash? #f)
  #:transparent)

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
              last-evaluation
              (or started-at (inexact->exact (round (current-inexact-milliseconds))))
              (or updated-at (inexact->exact (round (current-inexact-milliseconds))))
              meta))

;; --------------------------------------------------
;; Serialization
;; --------------------------------------------------

(define (string-truncate s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 max-len) "...")
      s))

;; goal-check
(define/contract (goal-check->hash gc)
  (-> goal-check? hash?)
  (hasheq 'command
          (goal-check-command gc)
          'expected-exit
          (goal-check-expected-exit gc)
          'label
          (goal-check-label gc)))

(define/contract (hash->goal-check h)
  (-> hash? goal-check?)
  (goal-check (hash-ref h 'command) (hash-ref h 'expected-exit 0) (hash-ref h 'label "")))

;; check-result
(define/contract (check-result->hash cr)
  (-> check-result? hash?)
  (hasheq 'label
          (check-result-label cr)
          'exit-code
          (check-result-exit-code cr)
          'stdout
          (check-result-stdout cr)
          'stderr
          (check-result-stderr cr)
          'timed-out?
          (check-result-timed-out? cr)
          'elapsed-ms
          (check-result-elapsed-ms cr)))

(define/contract (hash->check-result h)
  (-> hash? check-result?)
  (check-result (hash-ref h 'label "")
                (hash-ref h 'exit-code 1)
                (hash-ref h 'stdout "")
                (hash-ref h 'stderr "")
                (hash-ref h 'timed-out? #f)
                (hash-ref h 'elapsed-ms 0)))

;; evaluation-result
(define/contract (evaluation-result->hash er)
  (-> evaluation-result? hash?)
  (hasheq 'achieved?
          (evaluation-result-achieved? er)
          'reason
          (evaluation-result-reason er)
          'check-results
          (map check-result->hash (evaluation-result-check-results er))
          'model-used
          (evaluation-result-model-used er)
          'token-cost
          (evaluation-result-token-cost er)))

(define/contract (hash->evaluation-result h)
  (-> hash? evaluation-result?)
  (evaluation-result (hash-ref h 'achieved? #f)
                     (hash-ref h 'reason "")
                     (map hash->check-result (hash-ref h 'check-results '()))
                     (hash-ref h 'model-used "")
                     (hash-ref h 'token-cost 0)))

;; goal-state
(define/contract (goal-state->hash gs)
  (-> goal-state? hash?)
  (hasheq 'id
          (goal-state-id gs)
          'goal-text
          (goal-state-goal-text gs)
          'status
          (symbol->string (goal-state-status gs))
          'turns-used
          (goal-state-turns-used gs)
          'max-turns
          (goal-state-max-turns gs)
          'evaluator-model
          (goal-state-evaluator-model gs)
          'evaluator-mode
          (symbol->string (goal-state-evaluator-mode gs))
          'checks
          (map goal-check->hash (goal-state-checks gs))
          'last-evaluation
          (let ([le (goal-state-last-evaluation gs)]) (and le (evaluation-result->hash le)))
          'started-at
          (goal-state-started-at gs)
          'updated-at
          (goal-state-updated-at gs)
          'meta
          (goal-state-meta gs)))

(define/contract (hash->goal-state h)
  (-> hash? goal-state?)
  (goal-state (hash-ref h 'id "")
              (hash-ref h 'goal-text "")
              (let ([s (hash-ref h 'status "active")])
                (if (symbol? s)
                    s
                    (string->symbol s)))
              (hash-ref h 'turns-used 0)
              (hash-ref h 'max-turns DEFAULT-GOAL-MAX-TURNS)
              (hash-ref h 'evaluator-model "auto")
              (let ([m (hash-ref h 'evaluator-mode "transcript")])
                (if (symbol? m)
                    m
                    (string->symbol m)))
              (map hash->goal-check (hash-ref h 'checks '()))
              (let ([le (hash-ref h 'last-evaluation #f)]) (and le (hash->evaluation-result le)))
              (hash-ref h 'started-at 0)
              (hash-ref h 'updated-at 0)
              (hash-ref h 'meta #f)))
