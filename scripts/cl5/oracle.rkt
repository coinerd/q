#lang racket/base

;; scripts/cl5/oracle.rkt — mechanical graders for the frozen CL0 agent-task
;; benchmark (CL0 report #9977; CL5 W1 harness #9982).
;;
;; GRADING CONTRACT
;; - Navigation tasks (T1/T3/T4) are graded from answer TEXT by grade-task,
;;   against the frozen answer sets in scripts/cl5/task-spec.rkt. Grading is
;;   conservative and presence-based: facts are extracted mechanically
;;   (regexp path extraction, symbol-boundary token matches), never judged.
;;   A grader that can only detect presence cannot distinguish "wrong value"
;;   from "right value also mentioned"; the recorded hash keeps the raw
;;   extracted facts so a human can audit borderline verdicts.
;; - Implementation tasks (T2/T5) have NO text grading: grade-task returns #f
;;   for them. They are graded by EXECUTION via grade-task-by-execution
;;   (t2-verdict / t5-verdict) against a trial tree.
;;
;; Execution graders never mutate the caller's tree: the M-STOP-1 mutation is
;; applied to extensions/gsd/delivery-outcome.rkt and restored in a
;; dynamic-wind around the mutated test run. All subprocess helpers take an
;; explicit working directory and timeout.
;;
;; T4 near-miss rule (CL0 §2): a classification answer that resolves only to
;; runtime/iteration/retry-policy.rkt is a recorded NEAR-MISS, not a failure.
;; Presence-based attribution: naming llm/provider-errors.rkt is an
;; unambiguous classification pin (upgrades to 'pass); otherwise, if the
;; iteration-side retry-policy path appears, the verdict is 'near-miss.

(require json
         racket/file
         racket/list
         racket/port
         racket/string
         "task-spec.rkt")

(provide extract-answer-paths
         grade-task
         grade-task-by-execution
         m-stop-1-substitution
         apply-substitution
         apply-m-stop-1
         t2-test-command
         t2-verdict
         t5-check-program
         t5-command
         t5-verdict
         suite-result-summary
         file-at-revision
         changed-paths
         diff-constraint
         run-captured)

;; ---- conservative path extraction ------------------------------------------

;; Repo-relative *.rkt paths only; prose that does not spell a path is never
;; promoted to a path claim.
(define answer-path-rx #px"/?[A-Za-z0-9_.+~-]+(?:/[A-Za-z0-9_.+~-]+)*\\.rkt")

;; Normalize an extracted path: strip an absolute prefix under repo-root,
;; then a leading "./", then a leading "q/". Anything else is returned
;; unchanged (and will simply not match any frozen fact).
(define (normalize-answer-path raw repo-root)
  (define root
    (and repo-root
         (string-trim (if (path? repo-root)
                          (path->string repo-root)
                          repo-root)
                      "/"
                      #:left? #f
                      #:repeat? #t)))
  (define without-root
    (if (and root (string-prefix? raw (string-append root "/")))
        (substring raw (add1 (string-length root)))
        raw))
  (define without-dot
    (if (string-prefix? without-root "./")
        (substring without-root 2)
        without-root))
  (if (string-prefix? without-dot "q/")
      (substring without-dot 2)
      without-dot))

;; text -> sorted, deduplicated list of normalized .rkt paths.
(define (extract-answer-paths text #:repo-root [repo-root #f])
  (sort (remove-duplicates (map (lambda (raw) (normalize-answer-path raw repo-root))
                                (regexp-match* answer-path-rx text)))
        string<?))

;; ---- text grading helpers ----------------------------------------------------

;; A symbol token with quotation optional, delimited so that 'complete does
;; not match inside 'campaign-complete.
(define (symbol-mentioned? text sym)
  (define pattern (string-append "(?:^|[^A-Za-z0-9-])'?" (regexp-quote sym) "(?:$|[^A-Za-z0-9-])"))
  (regexp-match? (pregexp pattern) text))

(define (grade-result task-id verdict reasons recorded)
  (hash 'task (symbol->string task-id) 'verdict verdict 'reasons reasons 'recorded recorded))

;; T1: the five decision fields exactly 'complete 'none 'campaign-complete #f
;; #f plus the decision/interpreter/writer module paths, order-insensitive.
(define (grade-t1 answer-text answer-spec)
  (define paths (extract-answer-paths answer-text))
  (define required-paths
    (list (hash-ref answer-spec 'decision-module) (hash-ref answer-spec 'interpreter-module)))
  (define writers (hash-ref answer-spec 'writer-modules))
  (define all-required (append required-paths writers))
  (define missing-paths (filter (lambda (p) (not (member p paths))) all-required))
  (define symbols-found
    (filter (lambda (s) (symbol-mentioned? answer-text s)) '("complete" "none" "campaign-complete")))
  (define hash-false-count (length (regexp-match* #rx"#f" answer-text)))
  ;; wave-index and merge-sha are BOTH #f in the frozen tuple, and CL0 requires
  ;; every field to be reported. One #f plus the three keywords is not enough
  ;; (it cannot pin two distinct fields), so both labels AND two #f occurrences
  ;; are required for a pass.
  (define wave-label? (regexp-match? #px"(?i:wave[-_ ]?index)" answer-text))
  (define merge-label? (regexp-match? #px"(?i:merge[-_ ]?sha)" answer-text))
  (define false-fields-ok? (and wave-label? merge-label? (>= hash-false-count 2)))
  (define facts-ok?
    (and (= (length symbols-found) 3) false-fields-ok? (null? missing-paths)))
  (define nothing-found?
    (and (null? symbols-found) (zero? hash-false-count) (= (length missing-paths) 4)))
  (define reasons
    (append (for/list ([p (in-list missing-paths)])
              (format "missing module path: ~a" p))
            (for/list ([s (in-list '("complete" "none" "campaign-complete"))]
                       #:unless (member s symbols-found))
              (format "missing decision value: ~a" s))
            (cond
              [(not wave-label?) (list "wave-index field not reported")]
              [(not merge-label?) (list "merge-sha field not reported")]
              [(zero? hash-false-count) (list "missing #f for wave-index/merge-sha")]
              [(= hash-false-count 1)
               (list "only one #f seen; wave-index and merge-sha not independently confirmed")]
              [else '()])))
  (grade-result 'T1
                (cond
                  [facts-ok? 'pass]
                  [nothing-found? 'fail]
                  [else 'partial])
                reasons
                (hash 'paths-found
                      paths
                      'symbols-found
                      symbols-found
                      'hash-false-count
                      hash-false-count
                      'wave-index-label wave-label?
                      'merge-sha-label merge-label?
                      'missing-paths
                      missing-paths)))

;; Accepted phrasings for "agent/state.rkt has no outgoing internal requires
;; (fan-out 0)" — the causal claim the T3 oracle requires (CL0 §2).
(define fanout-zero-patterns
  (list #px"(?i:fan[- ]out)\\s*(?:of|=|is|:)?\\s*0\\b"
        #px"(?i:\\bno\\s+(?:outgoing\\s+)?(?:internal\\s+)?requires)\\b"
        #px"(?i:\\bzero\\s+(?:outgoing\\s+)?(?:internal\\s+)?requires)\\b"
        #px"(?i:requires\\s+no\\s+(?:other\\s+)?(?:internal\\s+)?(?:module|file|librar))"
        #px"(?i:does\\s+not\\s+require\\s+any)"))

(define (fan-out-zero-stated? text)
  (for/or ([pattern (in-list fanout-zero-patterns)])
    (regexp-match? pattern text)))

;; T3: all 13 direct dependents (order-insensitive), no extra .rkt path
;; claims beyond the dependents and the target itself, and the fan-out-0
;; statement present.
(define (grade-t3 answer-text answer-spec)
  (define dependents (hash-ref answer-spec 'direct-dependents))
  (define target (hash-ref answer-spec 'target-module))
  (define allowed (cons target dependents))
  (define paths (extract-answer-paths answer-text))
  (define missing (filter (lambda (p) (not (member p paths))) dependents))
  (define extras (filter (lambda (p) (not (member p allowed))) paths))
  (define fanout-ok? (fan-out-zero-stated? answer-text))
  (define reasons
    (append (for/list ([p (in-list missing)])
              (format "missing dependent: ~a" p))
            (for/list ([p (in-list extras)])
              (format "extra .rkt path claim not in the frozen answer set: ~a" p))
            (if fanout-ok?
                '()
                (list "missing the fan-out-0 statement (no outgoing internal requires)"))))
  (grade-result
   'T3
   (cond
     [(= (length missing) (length dependents)) 'fail]
     [(and (null? missing) (null? extras) fanout-ok?) 'pass]
     [else 'partial])
   reasons
   (hash 'paths-found paths 'missing missing 'extras extras 'fan-out-zero-stated fanout-ok?)))

;; T4: classification owner from the accepted set + the policy owner + at
;; least one accepted test pin. Near-miss recorded when the classification is
;; not pinned to llm/provider-errors.rkt and the iteration-side
;; retry-policy path appears (CL0 §2: recorded, not failed).
(define (grade-t4 answer-text answer-spec)
  (define paths (extract-answer-paths answer-text))
  (define accepted (hash-ref answer-spec 'classification-accepted))
  (define near-miss-path (hash-ref answer-spec 'classification-near-miss))
  (define policy-owner (hash-ref answer-spec 'policy-owner))
  (define pins (hash-ref answer-spec 'accepted-test-pins))
  (define classification-strong? (member (first accepted) paths)) ; llm/provider-errors.rkt
  (define classification-ok?
    (for/or ([p (in-list accepted)])
      (member p paths)))
  (define policy-ok? (member policy-owner paths))
  (define pin-found
    (for/first ([p (in-list pins)]
                #:when (member p paths))
      p))
  (define near-miss-mentioned? (member near-miss-path paths))
  (define reasons
    (append (if classification-ok?
                '()
                (list (format "classification owner missing; accepted: ~a" accepted)))
            (if policy-ok?
                '()
                (list (format "policy owner missing: ~a" policy-owner)))
            (if pin-found
                '()
                (list (format "no accepted test pin; accepted: ~a" pins)))))
  (define core-ok? (and classification-ok? policy-ok? pin-found))
  (grade-result 'T4
                (cond
                  [(and core-ok? (not classification-strong?) near-miss-mentioned?) 'near-miss]
                  [core-ok? 'pass]
                  [(and (not classification-ok?) (not policy-ok?) (not pin-found)) 'fail]
                  [else 'partial])
                reasons
                (hash 'paths-found
                      paths
                      'classification-strong
                      classification-strong?
                      'policy-owner-found
                      policy-ok?
                      'test-pin-found
                      (or pin-found #f)
                      'near-miss-path-mentioned
                      near-miss-mentioned?)))

;; Navigation tasks: grade answer text. Implementation tasks (T2/T5): text
;; grading is not defined — returns #f; use grade-task-by-execution.
(define (grade-task task-id answer-text)
  (define spec (task-spec task-id))
  (case (hash-ref spec 'kind)
    [(implementation) #f]
    [(navigation)
     (case task-id
       [(T1) (grade-t1 answer-text (hash-ref spec 'answer-spec))]
       [(T3) (grade-t3 answer-text (hash-ref spec 'answer-spec))]
       [(T4) (grade-t4 answer-text (hash-ref spec 'answer-spec))]
       [else (raise-arguments-error 'grade-task "no text grader for task" "task" task-id)])]))

;; ---- M-STOP-1 (T2 mutation, CL0 report §3) ----------------------------------

;; The one exact string substitution every T2 trial uses: in
;; delivery-stop-outcome, misclassify the review-infra case as 'retryable.
;; The from-string must occur EXACTLY once in the module; drift in the
;; module text is an error, never a silent skip.
(define (m-stop-1-substitution)
  (list
   "(delivery-outcome\n          'review-infra\n          (hash-ref data 'reason (or reason-fallback \"review infrastructure failure\")))"
   "(delivery-outcome\n          'retryable\n          (hash-ref data 'reason (or reason-fallback \"review infrastructure failure\")))"))

;; Pure: text from to -> mutated text; raises unless from occurs exactly once.
(define (apply-substitution text from to)
  (define occurrences (regexp-match* (regexp-quote from) text))
  (unless (= (length occurrences) 1)
    (raise-arguments-error 'apply-substitution
                           "substitution source must occur exactly once"
                           "occurrences"
                           (length occurrences)
                           "from"
                           from))
  (regexp-replace (regexp-quote from) text to))

;; Apply M-STOP-1 to the module at source-path IN PLACE. Callers that must
;; preserve the tree wrap this in dynamic-wind and restore the original text
;; (see t2-verdict).
(define (apply-m-stop-1 source-path)
  (define text (file->string source-path))
  (define substitution (m-stop-1-substitution))
  (define mutated (apply-substitution text (first substitution) (second substitution)))
  (call-with-output-file source-path (lambda (out) (display mutated out)) #:exists 'truncate/replace))

;; ---- subprocess helper ---------------------------------------------------------

;; Run argv with an explicit working directory and timeout.
;; -> (values exit-code stdout stderr timed-out?)
(define (run-captured argv #:cwd cwd #:timeout-ms timeout-ms)
  (define executable (or (find-executable-path (first argv)) (first argv)))
  (define-values (proc out-port in-port err-port)
    (parameterize ([current-directory cwd])
      (apply subprocess #f #f #f executable (rest argv))))
  (close-output-port in-port)
  (define out-box (box ""))
  (define err-box (box ""))
  (define out-thread
    (thread (lambda ()
              (set-box! out-box
                        (with-handlers ([exn:fail? (lambda (_) "")])
                          (port->string out-port))))))
  (define err-thread
    (thread (lambda ()
              (set-box! err-box
                        (with-handlers ([exn:fail? (lambda (_) "")])
                          (port->string err-port))))))
  (define done (make-semaphore 0))
  (thread (lambda ()
            (subprocess-wait proc)
            (semaphore-post done)))
  (define finished? (sync/timeout (/ timeout-ms 1000.0) done))
  (unless finished?
    (subprocess-kill proc #t)
    (subprocess-wait proc)
    ;; Release the reader threads if grandchildren inherited the pipes.
    (close-input-port out-port)
    (close-input-port err-port))
  (thread-wait out-thread)
  (thread-wait err-thread)
  (define status (subprocess-status proc))
  (sync/timeout 0 done) ; drain the semaphore on the timeout path
  (values (if (exact-integer? status) status -1) (unbox out-box) (unbox err-box) (not finished?)))

;; ---- suite evidence ------------------------------------------------------------

;; `raco test` prints either "<n> tests passed" (clean) or
;; "<failed>/<total> test failures" (red). Parse both shapes; #f when the
;; output carries neither, so an unparsable run is a failure rather than 0.
(define (suite-result-summary output)
  (define failure-match (regexp-match #px"([0-9]+)/([0-9]+) test failures" output))
  (define pass-match (regexp-match #px"([0-9]+) tests? passed" output))
  (cond
    [failure-match
     (hash 'failed (string->number (list-ref failure-match 1))
           'total (string->number (list-ref failure-match 2)))]
    [pass-match
     (hash 'failed 0 'total (string->number (list-ref pass-match 1)))]
    [else #f]))

;; File content at a revision (used to run the pre-existing suite against the
;; same mutation). Raises when the revision does not contain the path.
(define (file-at-revision worktree revision path #:timeout-ms [timeout-ms 120000])
  (define-values (exit-code stdout stderr _timed-out?)
    (run-captured (list "git" "show" (format "~a:~a" revision path))
                  #:cwd worktree
                  #:timeout-ms timeout-ms))
  (unless (equal? exit-code 0)
    (raise-arguments-error 'file-at-revision
                           "cannot read the file at the pinned revision"
                           "revision"
                           revision
                           "path"
                           path
                           "stderr"
                           stderr))
  stdout)

;; ---- T2 execution grader ---------------------------------------------------------

;; Pure command builder: the oracle command from CL0 §3 T2.
(define (t2-test-command test-rel)
  (list "raco" "test" test-rel))

;; First failing assertion line in a raco test output: the "expected:" line
;; of the first rackunit failure (column-alignment whitespace collapsed),
;; else the FAILURE header, else #f.
(define (collapse-spaces text)
  (regexp-replace* #px"[ ]+" text " "))
(define (extract-failing-assertion output)
  (define lines (string-split output "\n"))
  (or (for/first ([line (in-list lines)]
                  #:when (regexp-match? #rx"expected:" line))
        (collapse-spaces (string-trim line)))
      (for/first ([line (in-list lines)]
                  #:when (regexp-match? #rx"FAILURE" line))
        (string-trim line))
      #f))

;; T2 verdict on a trial tree. The oracle has three parts and all must hold:
;;   1. the agent's test file passes against the clean module;
;;   2. the SAME run fails under M-STOP-1 (it detects the mutation);
;;   3. the PRE-EXISTING suite (the file as checked out at the arm revision)
;;      passes under M-STOP-1 — so the added case, not the old ones, is what
;;      detects the mutation — and the case count strictly increased.
;; The module and the test file are restored before returning.
(define (t2-verdict tree-path
                    #:revision [revision #f]
                    #:baseline-test-text [baseline-test-text #f]
                    #:test-file [test-rel "tests/test-gsd-delivery-stop-decision.rkt"]
                    #:module-file [module-rel "extensions/gsd/delivery-outcome.rkt"]
                    #:timeout-ms [timeout-ms 180000])
  (define test-path (build-path tree-path test-rel))
  (define module-path (build-path tree-path module-rel))
  (define original-module (file->string module-path))
  (define agent-test (file->string test-path))
  ;; The pre-existing suite comes from the pinned revision, or from an explicit
  ;; fixture in unit tests. One of them is required: without a baseline the
  ;; "the added case detects the mutation" guarantee cannot be established.
  (define baseline-test
    (or baseline-test-text
        (and revision (file-at-revision tree-path revision test-rel #:timeout-ms timeout-ms))
        (raise-arguments-error 't2-verdict
                               "no baseline suite: pass #:revision or #:baseline-test-text")))
  (define (write-text path text)
    (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate/replace))
  (define (run-suite)
    (define-values (exit-code stdout stderr timed-out?)
      (run-captured (t2-test-command test-rel) #:cwd tree-path #:timeout-ms timeout-ms))
    (hash 'exit
          exit-code
          'timed-out
          timed-out?
          'summary
          (suite-result-summary (string-append stdout "\n" stderr))
          'output
          (string-append stdout "\n" stderr)))
  (define clean-agent (run-suite))
  ;; Pre-existing suite: clean, then under the mutation it must NOT detect.
  (define baseline-clean (dynamic-wind void (lambda () (write-text test-path baseline-test) (run-suite)) void))
  (define baseline-mutated
    (dynamic-wind
     (lambda () (write-text test-path baseline-test) (apply-m-stop-1 module-path))
     run-suite
     (lambda () (write-text module-path original-module) (write-text test-path agent-test))))
  (define agent-mutated
    (dynamic-wind (lambda () (apply-m-stop-1 module-path))
                  run-suite
                  (lambda () (write-text module-path original-module))))
  (define agent-cases (and (hash-ref clean-agent 'summary) (hash-ref (hash-ref clean-agent 'summary) 'total)))
  (define baseline-cases
    (and (hash-ref baseline-clean 'summary) (hash-ref (hash-ref baseline-clean 'summary) 'total)))
  (define new-case? (and agent-cases baseline-cases (> agent-cases baseline-cases)))
  (define pass?
    (and (equal? (hash-ref clean-agent 'exit) 0)
         (not (hash-ref clean-agent 'timed-out))
         (equal? (hash-ref baseline-clean 'exit) 0)
         (equal? (hash-ref baseline-mutated 'exit) 0)
         (not (hash-ref baseline-mutated 'timed-out))
         (not (equal? (hash-ref agent-mutated 'exit) 0))
         (not (hash-ref agent-mutated 'timed-out))
         new-case?))
  (hash 'verdict
        (if pass? 'pass 'fail)
        'clean-exit
        (hash-ref clean-agent 'exit)
        'mutated-exit
        (hash-ref agent-mutated 'exit)
        'baseline-mutated-exit
        (hash-ref baseline-mutated 'exit)
        'agent-cases
        agent-cases
        'baseline-cases
        baseline-cases
        'new-test-case?
        new-case?
        'failing-assertion
        (extract-failing-assertion (hash-ref agent-mutated 'output))
        'timed-out
        (or (hash-ref clean-agent 'timed-out)
            (hash-ref baseline-mutated 'timed-out)
            (hash-ref agent-mutated 'timed-out))))

;; ---- T5 execution grader ---------------------------------------------------------

;; The six independent behavioral assertions from CL0 §3 T5, evaluated by one
;; `racket -e` program that requires tui/char-width.rkt at the trial tree's
;; HEAD and prints a list of (label . boolean). Pure builder: returns the
;; program text.
(define (t5-check-program)
  (string-append
   "(begin\n"
   (string-join
    (list "(require racket/list racket/string)"
          "(define root (current-directory))"
          "(define module-path (build-path root \"tui\" \"char-width.rkt\"))"
          ;; A path module path (or (list 'file path)) resolves here; a plain
          ;; string does not. Exceptions mean the module cannot provide the
          ;; binding, which the guards below record as a failed assertion.
          "(define (try-require name)"
          "  (with-handlers ([exn:fail? (lambda (e) #f)])"
          "    (dynamic-require module-path name)))"
          "(define trunc (try-require 'truncate-with-ellipsis))"
          "(define width (try-require 'string-visible-width))"
          "(define graphemes (try-require 'substring-by-graphemes))"
          "(define gcount (try-require 'grapheme-count))"
          "(define ellipsis \"\\u2026\")"
          "(define (strip-ellipsis s)"
          "  (if (string-suffix? s ellipsis) (substring s 0 (sub1 (string-length s))) s))"
          "(define ansi-rx (pregexp \"\\u001b\\\\[[0-9;]*m\"))"
          "(define (strip-ansi s) (regexp-replace* ansi-rx s \"\"))"
          "(define (escapes-complete? s) (not (string-contains? (strip-ansi s) \"\\u001b\")))"
          "(define labels"
          "  '(a1-zero-budget a2-exact-fit a3-ascii-truncation a4-ansi-escape"
          "    a5-unicode-width a6-ellipsis-accounting))"
          "(define results (make-hash (map (lambda (l) (cons l #f)) labels)))"
          "(define (record! label ok?) (hash-set! results label (and ok? #t)))"
          "(define (guard thunk) (with-handlers ([exn:fail? (lambda (e) #f)]) (thunk)))"
          "(when (and trunc width)"
          "  (record! 'a1-zero-budget"
          "           (guard (lambda () (= (width (trunc \"anything\" 0)) 0))))"
          "  (record! 'a2-exact-fit"
          "           (guard (lambda () (equal? (trunc \"abcdef\" 6) \"abcdef\"))))"
          "  (record! 'a3-ascii-truncation"
          "           (guard (lambda ()"
          "                    (define r (trunc \"abcdef\" 4))"
          "                    (and (= (width r) 4)"
          "                         (string-prefix? \"abcdef\" (strip-ellipsis (strip-ansi r)))))))"
          "  (record! 'a4-ansi-escape"
          "           (guard (lambda ()"
          "                    (define s \"\\u001b[31mabcdef\\u001b[0m\")"
          "                    (define r (trunc s 4))"
          "                    (and (= (width r) 4)"
          "                         (escapes-complete? r)"
          "                         (string-prefix? \"abcdef\" (strip-ellipsis (strip-ansi r)))))))"
          "  (record! 'a5-unicode-width"
          "           (guard (lambda ()"
          "                    (define r (trunc \"\\u6F22\\u5B57\" 3))"
          "                    (define s5b \"e\\u0301bcdef\")"
          "                    (define r5b (trunc s5b 2))"
          "                    (define vis5b (strip-ellipsis (strip-ansi r5b)))"
          "                    (and (= (width r) 3)"
          "                         (or (not (and graphemes gcount))"
          "                             (equal? vis5b (graphemes s5b 0 (gcount vis5b))))))))"
          "  (record! 'a6-ellipsis-accounting"
          "           (guard (lambda ()"
          "                    (and (for/and ([b (list 1 2 3 4 5 7)])"
          "                           (= (width (trunc \"abcdefgh\" b)) b))"
          "                         (= (width (trunc \"abcdefgh\" 0)) 0))))))"
          "(write (map (lambda (l) (cons l (hash-ref results l))) labels))"
          "(newline)")
    "\n")
   "\n)"))

;; Pure command builder.
(define (t5-command)
  (list "racket" "-e" (t5-check-program)))

;; T5 verdict on a trial tree: the six behavioral assertions hold AND the
;; owner test file gained at least one case relative to the arm revision (the
;; task explicitly asks for one added test-case). Both the module and the test
;; file are restored before returning.
(define (t5-verdict tree-path
                    #:revision [revision #f]
                    #:baseline-test-text [baseline-test-text #f]
                    #:test-file [test-rel "tests/tui/test-char-width.rkt"]
                    #:timeout-ms [timeout-ms 180000])
  (define test-path (build-path tree-path test-rel))
  (define agent-test (file->string test-path))
  ;; The pre-existing suite comes from the pinned revision, or from an explicit
  ;; fixture in unit tests. One of them is required: without a baseline the
  ;; "the added case detects the mutation" guarantee cannot be established.
  (define baseline-test
    (or baseline-test-text
        (and revision (file-at-revision tree-path revision test-rel #:timeout-ms timeout-ms))
        (raise-arguments-error 't2-verdict
                               "no baseline suite: pass #:revision or #:baseline-test-text")))
  (define-values (exit-code stdout stderr timed-out?)
    (run-captured (t5-command) #:cwd tree-path #:timeout-ms timeout-ms))
  (define results
    (with-handlers ([exn:fail? (lambda (_) #f)]) (read (open-input-string stdout))))
  (define pairs (if (list? results) results '()))
  (define failing (for/first ([pair (in-list pairs)] #:unless (cdr pair)) (car pair)))
  (define assertions-ok? (and (= (length pairs) 6) (andmap cdr pairs)))
  (define (run-cases)
    (define-values (suite-exit suite-out suite-err _t)
      (run-captured (t2-test-command test-rel) #:cwd tree-path #:timeout-ms timeout-ms))
    (hash 'exit suite-exit 'summary (suite-result-summary (string-append suite-out "\n" suite-err))))
  (define agent-cases-run (run-cases))
  (define baseline-cases-run
    (dynamic-wind
     (lambda ()
       (call-with-output-file test-path
                              (lambda (out) (display baseline-test out))
                              #:exists 'truncate/replace))
     run-cases
     (lambda ()
       (call-with-output-file test-path
                              (lambda (out) (display agent-test out))
                              #:exists 'truncate/replace))))
  (define agent-cases (and (hash-ref agent-cases-run 'summary)
                           (hash-ref (hash-ref agent-cases-run 'summary) 'total)))
  (define baseline-cases (and (hash-ref baseline-cases-run 'summary)
                              (hash-ref (hash-ref baseline-cases-run 'summary) 'total)))
  (define new-case? (and agent-cases baseline-cases (> agent-cases baseline-cases)))
  (hash 'verdict
        (if (and assertions-ok? new-case? (equal? (hash-ref agent-cases-run 'exit) 0))
            'pass
            'fail)
        'assertions
        pairs
        'failing
        failing
        'exit-code
        exit-code
        'timed-out
        timed-out?
        'agent-cases
        agent-cases
        'baseline-cases
        baseline-cases
        'new-test-case?
        new-case?
        'stderr
        stderr))

;; Dispatcher: execution-graded tasks (T2/T5) need the arm revision so the
;; pre-existing suite and the case-count baseline come from the pinned tree.
(define (grade-task-by-execution task-id tree-path #:revision revision #:timeout-ms [timeout-ms 180000])
  (case task-id
    [(T2) (t2-verdict tree-path #:revision revision #:timeout-ms timeout-ms)]
    [(T5) (t5-verdict tree-path #:revision revision #:timeout-ms timeout-ms)]
    [else
     (raise-arguments-error 'grade-task-by-execution "task is not execution-graded" "task" task-id)]))

;; ---- diff constraint (CL0 §3: the patch must stay inside its file set) -------

;; changed-paths : worktree repo-revision -> (listof string), sorted. Tracked
;; modifications, deletions and untracked additions relative to repo-revision.
(define (changed-paths worktree repo-revision #:timeout-ms [timeout-ms 120000])
  (define-values (diff-exit diff-out _diff-err _diff-timeout)
    (run-captured (list "git" "diff" "--name-only" "--no-renames" repo-revision)
                  #:cwd worktree
                  #:timeout-ms timeout-ms))
  (define-values (status-exit status-out _status-err _status-timeout)
    (run-captured (list "git" "status" "--porcelain=v1" "--no-renames")
                  #:cwd worktree
                  #:timeout-ms timeout-ms))
  (define (lines text)
    (if (equal? text "")
        '()
        (filter (lambda (line) (not (equal? line ""))) (string-split text "\n"))))
  (define tracked
    (if (equal? diff-exit 0)
        (lines diff-out)
        '()))
  (define status-paths
    (if (equal? status-exit 0)
        (for/list ([line (in-list (lines status-out))]
                   #:when (>= (string-length line) 4))
          (string-trim (substring line 3) #:left? #t #:repeat? #t))
        '()))
  ;; Deleted paths cannot be "changed files" the oracle keeps; they still count
  ;; against the constraint, so they stay in the list.
  (sort (remove-duplicates (append tracked status-paths)) string<?))

;; Diff constraint: every changed path is inside allowed-set, and each
;; required path was actually changed. Returns a hash with the verdict and the
;; two path lists so a failure is diagnosable without re-running.
(define (diff-constraint changed allowed required)
  (define stray (filter (lambda (path) (not (member path allowed))) changed))
  (define missing (filter (lambda (path) (not (member path changed))) required))
  (hash 'verdict
        (if (and (null? stray) (null? missing)) 'pass 'fail)
        'changed
        changed
        'stray
        stray
        'missing
        missing))
