#lang racket/base

;; scripts/benchmark/scorer.rkt — 5-dimension scoring engine for benchmark results
;;
;; Scores an execution-result against a benchmark-task across 5 dimensions:
;;   correctness      (weight 0.40) — files created, content checks, tests pass
;;   tool-discipline  (weight 0.20) — tools used/not-used compliance
;;   efficiency       (weight 0.15) — iteration budget usage
;;   skill-compliance (weight 0.15) — placeholder (always 100)
;;   no-regressions   (weight 0.10) — files-not-modified preservation
;;
;; Verdicts: PASS ≥ 70, PARTIAL ≥ 40, FAIL < 40

(require racket/file
         racket/format
         racket/list
         racket/port
         racket/string
         json
         racket/system
         (only-in "../../util/jsonl.rkt" jsonl-read-all-valid)
         "task.rkt"
         (only-in "executor.rkt"
                  execution-result?
                  execution-result-task-name
                  execution-result-trace-path
                  execution-result-session-dir
                  execution-result-duration-ms
                  execution-result-iterations-used
                  execution-result-outcome
                  execution-result-error-msg
                  execution-result-project-dir
                  execution-result-raw-result))

(provide (struct-out score-result)
         score-execution
         score-correctness
         score-tool-discipline
         score-efficiency
         score-skill-compliance
         score-no-regressions
         aggregate-scores
         score->verdict)

;; ============================================================
;; Struct
;; ============================================================

(struct score-result
        (task-name ; string?
         total-score ; 0–100
         verdict ; 'PASS | 'PARTIAL | 'FAIL
         correctness ; 0–100
         tool-discipline ; 0–100
         efficiency ; 0–100
         skill-compliance ; 0–100
         no-regressions ; 0–100
         details) ; hash — per-dimension breakdown
  #:transparent)

;; ============================================================
;; Verdict
;; ============================================================

;; score->verdict : real? -> (or/c 'PASS 'PARTIAL 'FAIL)
(define (score->verdict total)
  (cond
    [(>= total 70) 'PASS]
    [(>= total 40) 'PARTIAL]
    [else 'FAIL]))

;; ============================================================
;; Aggregation
;; ============================================================

;; aggregate-scores : real? real? real? real? real? -> real?
;; Weighted sum → 0–100.
(define (aggregate-scores correctness tool-discipline efficiency skill-compliance no-regressions)
  (+ (* correctness 0.40)
     (* tool-discipline 0.20)
     (* efficiency 0.15)
     (* skill-compliance 0.15)
     (* no-regressions 0.10)))

;; ============================================================
;; Dimension 1: Correctness (40%)
;; ============================================================

;; score-correctness : execution-result? benchmark-task? -> (values real? hash?)
;; Checks: files-created exist with must-contain, tests-pass via raco test.
(define (score-correctness exec-res task)
  (define spec (benchmark-task-scoring task))
  (define project-dir (execution-result-project-dir exec-res))
  (define checks (scoring-spec-files-created spec))
  (define test-paths (scoring-spec-tests-pass spec))

  (define n-file-checks (length checks))
  (define n-test-checks (length test-paths))
  (define n-total (+ n-file-checks n-test-checks))

  ;; If nothing to check, give full credit
  (cond
    [(zero? n-total) (values 100 (hash 'note "no correctness criteria specified"))]

    [else
     ;; --- File existence + content checks ---
     (define file-results
       (for/list ([fc (in-list checks)])
         (define rel-path (file-check-path fc))
         (define must-contain (file-check-must-contain fc))
         (define full-path
           (if project-dir
               (build-path project-dir rel-path)
               rel-path))
         (define exists? (file-exists? full-path))
         (cond
           [(not exists?) (hash 'path rel-path 'exists? #f 'content-ok? #f 'reason "file missing")]
           [(null? must-contain)
            (hash 'path rel-path 'exists? #t 'content-ok? #t 'reason "no content requirements")]
           [else
            (define content (file->string full-path))
            (define content-ok?
              (for/and ([needle (in-list must-contain)])
                (string-contains? content needle)))
            (hash 'path
                  rel-path
                  'exists?
                  #t
                  'content-ok?
                  content-ok?
                  'reason
                  (if content-ok? "ok" "missing required content"))])))

     (define file-score
       (if (zero? n-file-checks)
           1.0
           (/ (for/sum ([r (in-list file-results)])
                       (if (and (hash-ref r 'exists? #f) (hash-ref r 'content-ok? #f)) 1 0))
              n-file-checks)))

     ;; --- Test checks ---
     (define test-results
       (for/list ([tp (in-list test-paths)])
         (define full-path
           (if project-dir
               (build-path project-dir tp)
               tp))
         (define-values (passed? detail) (run-raco-test full-path))
         (hash 'path tp 'passed? passed? 'detail detail)))

     (define test-score
       (if (zero? n-test-checks)
           1.0
           (/ (for/sum ([r (in-list test-results)]) (if (hash-ref r 'passed? #f) 1 0))
              n-test-checks)))

     ;; Combine: file checks and test checks weighted equally
     (define file-weight
       (if (zero? n-file-checks)
           0
           (/ n-file-checks n-total)))
     (define test-weight
       (if (zero? n-test-checks)
           0
           (/ n-test-checks n-total)))
     (define raw-score (+ (* file-score file-weight) (* test-score test-weight)))
     (define score (inexact->exact (round (* raw-score 100))))

     (values score
             (hash 'file-results
                   file-results
                   'test-results
                   test-results
                   'file-score
                   file-score
                   'test-score
                   test-score))]))

;; run-raco-test : path? -> (values boolean? string?)
;; Runs raco test on a path, returns whether it passed and detail string.
(define (run-raco-test path)
  (if (not (file-exists? path))
      (values #f (format "file does not exist: ~a" path))
      (let ()
        (define cmd (format "raco test ~a 2>&1" (shell-escape-path path)))
        (define out (open-output-string))
        (define exit-code
          (parameterize ([current-output-port out]
                         [current-error-port out])
            (system/exit-code cmd)))
        (define exit-ok? (zero? exit-code))
        (values exit-ok?
                (if exit-ok?
                    "passed"
                    (get-output-string out))))))

;; shell-escape-path : path-string? -> string?
;; Simple shell escaping for paths.
(define (shell-escape-path p)
  (define s
    (if (path? p)
        (path->string p)
        p))
  (if (string-contains? s " ")
      (format "\"~a\"" s)
      s))

;; ============================================================
;; Dimension 2: Tool Discipline (20%)
;; ============================================================

;; score-tool-discipline : execution-result? benchmark-task? -> (values real? hash?)
;; Checks tools-used (must appear) and tools-not-used (must not appear).
(define (score-tool-discipline exec-res task)
  (define spec (benchmark-task-scoring task))
  (define tools-required (scoring-spec-tools-used spec))
  (define tools-forbidden (scoring-spec-tools-not-used spec))
  (define trace-path (execution-result-trace-path exec-res))

  ;; Extract tool names from trace
  (define trace-tools
    (if (and trace-path (file-exists? trace-path))
        (extract-tool-names-from-trace trace-path)
        '()))

  (define n-required (length tools-required))
  (define n-forbidden (length tools-forbidden))
  (define n-total (+ n-required n-forbidden))

  (cond
    [(zero? n-total) (values 100 (hash 'note "no tool discipline criteria" 'trace-tools trace-tools))]

    [else
     ;; Check required tools
     (define required-results
       (for/list ([tool (in-list tools-required)])
         (define found? (member tool trace-tools))
         (hash 'tool tool 'required? #t 'found? (and found? #t))))

     ;; Check forbidden tools
     (define forbidden-results
       (for/list ([tool (in-list tools-forbidden)])
         (define found? (member tool trace-tools))
         (hash 'tool tool 'forbidden? #t 'violated? (and found? #t))))

     (define required-pass
       (for/sum ([r (in-list required-results)]) (if (hash-ref r 'found? #f) 1 0)))

     (define forbidden-pass
       (for/sum ([r (in-list forbidden-results)]) (if (not (hash-ref r 'violated? #f)) 1 0)))

     (define total-pass (+ required-pass forbidden-pass))
     (define raw-score (/ total-pass n-total))
     (define score (inexact->exact (round (* raw-score 100))))

     (values score
             (hash 'required-results
                   required-results
                   'forbidden-results
                   forbidden-results
                   'trace-tools
                   trace-tools))]))

;; extract-tool-names-from-trace : path? -> (listof string?)
;; Reads trace JSONL and extracts tool_use event names.
(define (extract-tool-names-from-trace trace-path)
  (define entries
    (with-handlers ([exn:fail? (lambda (e) '())])
      (jsonl-read-all-valid trace-path)))
  (append*
   (for/list ([entry (in-list entries)]
              #:when (hash? entry))
     (cond
       ;; Direct tool_use event with 'name' field
       [(equal? (hash-ref entry 'type #f) "tool_use")
        (list (hash-ref entry 'name "unknown"))]
       ;; Assistant message with tool_calls array
       [(equal? (hash-ref entry 'role #f) "assistant")
        (for/list ([tc (in-list (hash-ref entry 'tool_calls '()))])
          (hash-ref (hash-ref tc 'function (hasheq)) 'name "unknown"))]
       [else '()]))))

;; ============================================================
;; Dimension 3: Efficiency (15%)
;; ============================================================

;; score-efficiency : execution-result? benchmark-task? -> (values real? hash?)
;; Fewer iterations = better. Linear scale against max-iterations.
(define (score-efficiency exec-res task)
  (define spec (benchmark-task-scoring task))
  (define max-iter (or (scoring-spec-max-iterations spec) (benchmark-task-max-iterations task)))
  (define used (execution-result-iterations-used exec-res))

  (cond
    [(or (not max-iter) (<= max-iter 0))
     (values 100 (hash 'iterations-used used 'max-iterations max-iter 'note "unlimited budget"))]
    [(>= used max-iter)
     ;; Used full budget — minimal score
     (define score 10)
     (values score
             (hash 'iterations-used
                   used
                   'max-iterations
                   max-iter
                   'ratio
                   (/ used max-iter)
                   'note
                   "used full iteration budget"))]
    [else
     ;; Score inversely proportional to iterations used.
     ;; 0 iterations → 100, max-iter → 10 (floor)
     (define ratio (/ used max-iter))
     (define raw-score (* (- 1.0 ratio) 100))
     ;; Floor at 10 if some iterations were used
     (define score (max 10 (inexact->exact (round raw-score))))
     (values score
             (hash 'iterations-used used 'max-iterations max-iter 'ratio (exact->inexact ratio)))]))

;; ============================================================
;; Dimension 4: Skill Compliance (15%) — Placeholder
;; ============================================================

;; score-skill-compliance : execution-result? benchmark-task? -> (values real? hash?)
;; Placeholder: always returns 100 until skill trace parsing is implemented.
(define (score-skill-compliance exec-res task)
  (values 100 (hash 'note "placeholder — no skill trace parsing yet")))

;; ============================================================
;; Dimension 5: No Regressions (10%)
;; ============================================================

;; score-no-regressions : execution-result? benchmark-task? -> (values real? hash?)
;; Checks that files-not-modified are unchanged.
;; Compares current file content against original content from setup spec.
(define (score-no-regressions exec-res task)
  (define spec (benchmark-task-scoring task))
  (define protected-paths (scoring-spec-files-not-modified spec))
  (define project-dir (execution-result-project-dir exec-res))
  (define setup-spec (benchmark-task-setup task))
  (define setup-files (task-setup-spec-files setup-spec))

  (cond
    [(null? protected-paths) (values 100 (hash 'note "no protected files"))]

    [else
     (define results
       (for/list ([rel-path (in-list protected-paths)])
         (define full-path
           (if project-dir
               (build-path project-dir rel-path)
               rel-path))
         (define current-exists? (file-exists? full-path))

         ;; Look up original content from setup spec
         (define original-content (hash-ref setup-files rel-path #f))

         (cond
           ;; File was deleted — regression
           [(not current-exists?) (hash 'path rel-path 'status 'deleted)]
           ;; No baseline to compare against — assume ok
           [(not original-content) (hash 'path rel-path 'status 'no-baseline)]
           [else
            (define current-content (file->string full-path))
            (define unchanged? (string=? current-content original-content))
            (hash 'path rel-path 'status (if unchanged? 'preserved 'modified))])))

     (define n-ok
       (for/sum ([r (in-list results)])
                (if (member (hash-ref r 'status) '(preserved no-baseline)) 1 0)))
     (define raw-score (/ n-ok (length results)))
     (define score (inexact->exact (round (* raw-score 100))))

     (values score (hash 'results results))]))

;; ============================================================
;; Top-level scoring
;; ============================================================

;; score-execution : execution-result? benchmark-task? -> score-result?
(define (score-execution exec-res task)
  (define-values (correctness correctness-details) (score-correctness exec-res task))

  (define-values (tool-discipline tool-details) (score-tool-discipline exec-res task))

  (define-values (efficiency efficiency-details) (score-efficiency exec-res task))

  (define-values (skill-compliance skill-details) (score-skill-compliance exec-res task))

  (define-values (no-regressions regression-details) (score-no-regressions exec-res task))

  (define total
    (aggregate-scores correctness tool-discipline efficiency skill-compliance no-regressions))

  (define verdict (score->verdict total))

  (score-result (benchmark-task-name task)
                (exact->inexact total)
                verdict
                correctness
                tool-discipline
                efficiency
                skill-compliance
                no-regressions
                (hash 'correctness
                      correctness-details
                      'tool-discipline
                      tool-details
                      'efficiency
                      efficiency-details
                      'skill-compliance
                      skill-details
                      'no-regressions
                      regression-details)))
