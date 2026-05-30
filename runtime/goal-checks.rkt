#lang racket/base

;; q/runtime/goal-checks.rkt — Deterministic check execution for goals
;;
;; Parses --check arguments from /goal command, classifies risk,
;; executes via subprocess with timeout + custodian isolation.

(require racket/contract
         racket/match
         racket/format
         racket/string
         racket/list
         "goal-state.rkt"
         "../sandbox/subprocess.rkt"
         "../tools/shell-risk.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide parse-goal-checks
         execute-goal-check
         execute-all-checks
         validate-check-safety
         checks-summary)

;; ============================================================
;; Parsing --check arguments
;; ============================================================

;; Parse a goal argument string to extract checks.
;; Input: "all tests pass --check 'raco test tests/' --check \"raco make\""
;; Returns: (values goal-text (listof goal-check?))
(define/contract (parse-goal-checks args)
  (-> string? (values string? (listof goal-check?)))
  (define parts (split-check-args args))
  (define goal-text (string-trim (car parts)))
  (define check-strs (cdr parts))
  (define checks
    (for/list ([cs (in-list check-strs)]
               [i (in-naturals 1)])
      (define cmd (string-trim cs))
      (define label (format "check-~a" i))
      (make-goal-check #:command cmd #:label label)))
  (values goal-text checks))

;; Split argument string on --check boundaries, preserving quoted strings.
;; Returns list: first element is goal text, rest are check commands.
(define (split-check-args args)
  (define tokens (tokenize-with-quotes args))
  ;; Find --check positions
  (define check-indices
    (for/list ([t (in-list tokens)]
               [i (in-naturals)]
               #:when (equal? t "--check"))
      i))
  (cond
    [(null? check-indices) (list args)]
    [else
     ;; Goal text is everything before first --check
     (define goal-end (car check-indices))
     (define goal-text (string-join (take tokens goal-end) " "))
     ;; Each --check consumes the next token
     (define check-commands
       (for/list ([ci (in-list check-indices)])
         (define next-idx (add1 ci))
         (if (< next-idx (length tokens))
             (list-ref tokens next-idx)
             "")))
     (cons goal-text check-commands)]))

;; Simple tokenizer that respects single and double quotes.
(define (tokenize-with-quotes s)
  (define chars (string->list s))
  (let loop ([cs chars]
             [current '()]
             [tokens '()]
             [in-quote #f])
    (cond
      [(null? cs)
       (define final (reverse current))
       (if (null? final)
           (reverse tokens)
           (reverse (cons (list->string final) tokens)))]
      [(and (not in-quote) (char=? (car cs) #\space))
       (define tok (reverse current))
       (loop (cdr cs)
             '()
             (if (null? tok)
                 tokens
                 (cons (list->string tok) tokens))
             #f)]
      [(and (not in-quote) (or (char=? (car cs) #\') (char=? (car cs) #\")))
       (loop (cdr cs) current tokens (car cs))]
      [(and in-quote (char=? (car cs) in-quote)) (loop (cdr cs) current tokens #f)]
      [else (loop (cdr cs) (cons (car cs) current) tokens in-quote)])))

;; ============================================================
;; Safety validation
;; ============================================================

;; Validate that check commands are safe to execute.
;; Returns (listof string) — list of rejection reasons. Empty = safe.
(define/contract (validate-check-safety checks)
  (-> (listof goal-check?) (listof string?))
  (for/fold ([reasons '()]) ([c (in-list checks)])
    (define cmd (goal-check-command c))
    (define tokens (tokenize-shell-command cmd))
    (define findings (classify-shell-risks tokens))
    (define summary (shell-risk-summary findings))
    (define max-sev (hash-ref summary 'max-severity 'info))
    (define severity-order '(info low medium high critical))
    (define too-risky? (> (index-of severity-order max-sev) (index-of severity-order 'medium)))
    (if too-risky?
        (append reasons
                (list (format "Check '~a' has ~a severity risks: ~a"
                              (goal-check-label c)
                              max-sev
                              (string-join (map shell-risk-finding-message findings) "; "))))
        reasons)))

;; ============================================================
;; Check execution
;; ============================================================

;; Execute a single goal check via subprocess.
(define/contract (execute-goal-check check #:timeout [timeout 30] #:directory [directory #f])
  (->* (goal-check?)
       (#:timeout exact-nonnegative-integer? #:directory (or/c path? string? #f))
       check-result?)
  (define cmd (goal-check-command check))
  (define label (goal-check-label check))
  (define start-ms (inexact->exact (round (current-inexact-milliseconds))))
  (define result
    (run-subprocess "/bin/sh"
                    #:args (list "-c" cmd)
                    #:timeout timeout
                    #:directory (or directory (current-directory))))
  (define elapsed (- (inexact->exact (round (current-inexact-milliseconds))) start-ms))
  (check-result label
                (or (subprocess-result-exit-code result) -1)
                (string-truncate (or (subprocess-result-stdout result) "") 500)
                (string-truncate (or (subprocess-result-stderr result) "") 200)
                (subprocess-result-timed-out? result)
                elapsed))

;; Execute all checks, return list of check-results.
(define/contract (execute-all-checks checks #:timeout [timeout 30] #:directory [directory #f])
  (->* ((listof goal-check?))
       (#:timeout exact-nonnegative-integer? #:directory (or/c path? string? #f))
       (listof check-result?))
  (for/list ([c (in-list checks)])
    (execute-goal-check c #:timeout timeout #:directory directory)))

;; ============================================================
;; Helpers
;; ============================================================

;; string-truncate is provided by goal-state.rkt (consolidated in v0.71.7)

;; Summarize check results: all passed? summary string.
(define/contract (checks-summary results)
  (-> (listof check-result?) (values boolean? string?))
  (cond
    [(null? results) (values #t "no checks")]
    [else
     (define all-passed? (andmap (lambda (r) (= (check-result-exit-code r) 0)) results))
     (define summary
       (string-join (for/list ([r (in-list results)])
                      (format "~a: ~a"
                              (check-result-label r)
                              (if (= (check-result-exit-code r) 0) "PASS" "FAIL")))
                    "; "))
     (values all-passed? summary)]))
