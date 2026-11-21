#lang racket

;; @speed slow  ;; @suite default
;; @boundary unit

;; tests/test-cl5-oracle.rkt — CL5 W1 (#9982): mechanical oracle graders.
;;
;; Pure coverage: path extraction/normalization, the M-STOP-1 substitution
;; (pinned against the real extensions/gsd/delivery-outcome.rkt text),
;; T1/T3/T4 text grading (pass/partial/fail/near-miss), command construction.
;; Execution coverage uses SYNTHETIC trees under a temp dir — a tiny
;; delivery-outcome module and a tiny char-width module — never the q tree.
;; @speed slow because the t2/t5 verdict tests spawn raco/racket subprocesses.

(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../scripts/cl5/oracle.rkt"
         "../scripts/cl5/task-spec.rkt")

(define-runtime-path delivery-outcome-module "../extensions/gsd/delivery-outcome.rkt")

(define (verdict-of result)
  (hash-ref result 'verdict))

(module+ test
  ;; ---- path extraction -------------------------------------------------------

  (test-case "path extraction finds repo-relative .rkt paths and ignores prose"
    (check-equal? (extract-answer-paths "see agent/loop.rkt and util/hash.rkt for details")
                  '("agent/loop.rkt" "util/hash.rkt"))
    (check-equal? (extract-answer-paths "no paths here, just words") '()))

  (test-case "path extraction normalizes q/, ./, and repo-root prefixes"
    (check-equal? (extract-answer-paths "q/agent/loop.rkt") '("agent/loop.rkt"))
    (check-equal? (extract-answer-paths "./agent/loop.rkt") '("agent/loop.rkt"))
    (let ([root (path->string (build-path (find-system-path 'temp-dir) "trial"))])
      (check-equal? (extract-answer-paths (string-append "edited " root "/agent/loop.rkt yesterday")
                                          #:repo-root root)
                    '("agent/loop.rkt"))))

  ;; ---- M-STOP-1 substitution ---------------------------------------------------

  (test-case "M-STOP-1 substitution matches the real delivery-outcome module exactly once"
    (define text (file->string delivery-outcome-module))
    (define substitution (m-stop-1-substitution))
    (define mutated (apply-substitution text (first substitution) (second substitution)))
    (check-false (equal? mutated text))
    (check-true (string-contains? mutated "'retryable\n          (hash-ref data"))
    ;; the case key is untouched — only the outcome kind changed
    (check-true (string-contains? mutated "[(review-infra)"))
    ;; the mutation removes exactly one 'review-infra outcome kind and adds
    ;; exactly one 'retryable
    (check-equal? (length (regexp-match* #rx"'review-infra" mutated))
                  (sub1 (length (regexp-match* #rx"'review-infra" text))))
    (check-equal? (length (regexp-match* #rx"'retryable" mutated))
                  (add1 (length (regexp-match* #rx"'retryable" text)))))

  (test-case "apply-substitution raises unless the source occurs exactly once"
    (check-exn #rx"exactly once" (lambda () (apply-substitution "nothing here" "a" "b")))
    (check-exn #rx"exactly once" (lambda () (apply-substitution "a and a" "a" "b"))))

  ;; ---- T1 grading ----------------------------------------------------------------

  (define t1-good
    (string-append
     "kind: 'complete, route: 'none, code: 'campaign-complete, wave-index: #f, merge-sha: #f.\n"
     "Decision module: extensions/gsd/campaign-decision.rkt.\n"
     "Interpreter: extensions/gsd/go-orchestrator.rkt.\n"
     "Writers: extensions/gsd/campaign-state.rkt and extensions/gsd/campaign-repository.rkt."))

  (test-case "T1 pass: full tuple and all module paths, order-insensitive"
    (define result (grade-task 'T1 t1-good))
    (check-equal? (verdict-of result) 'pass)
    (check-equal? (hash-ref result 'reasons) '()))

  (test-case "T1 fail: wrong decision tuple (no correct facts at all)"
    (define result (grade-task 'T1 "I have no idea; the decision is probably 'retry."))
    (check-equal? (verdict-of result) 'fail))

  (test-case "T1 partial: correct fields but a missing module path"
    (define partial
      "kind 'complete, route 'none, code 'campaign-complete, wave-index #f, merge-sha #f;
       decision module extensions/gsd/campaign-decision.rkt")
    (define result (grade-task 'T1 partial))
    (check-equal? (verdict-of result) 'partial)
    (check-true (for/or ([reason (in-list (hash-ref result 'reasons))])
                  (string-contains? reason "go-orchestrator"))))

  (test-case "T1: 'complete does not match inside 'campaign-complete"
    ;; only campaign-complete mentioned: 'complete must be reported missing
    (define result (grade-task 'T1 "code is 'campaign-complete, and q/x.rkt"))
    (check-equal? (verdict-of result) 'partial)
    (define recorded (hash-ref result 'recorded))
    (check-equal? (hash-ref recorded 'symbols-found) '("campaign-complete")))

  ;; ---- T3 grading ----------------------------------------------------------------

  (define t3-dependents (hash-ref (hash-ref (task-spec 'T3) 'answer-spec) 'direct-dependents))

  (define t3-good
    (string-append
     "agent/state.rkt has no outgoing internal requires (fan-out 0), so editing it changes\n"
     "nothing it depends on; the owners are selected because audited tests transitively\n"
     "exercise the loop/session modules that require it (an impact-graph inbound closure).\n"
     "Direct dependents: "
     (string-join (reverse t3-dependents) ", ")
     ".")) ; reversed: order-insensitive

  (test-case "T3 pass: all 13 dependents, order-insensitive, fan-out-0 statement"
    (define result (grade-task 'T3 t3-good))
    (check-equal? (verdict-of result) 'pass)
    (check-equal? (hash-ref result 'reasons) '()))

  (test-case "T3 accepted fan-out phrasings"
    (for ([text (in-list (list "fan-out 0"
                               "fan out = 0"
                               "fan-out of 0"
                               "no outgoing internal requires"
                               "zero internal requires"
                               "requires no other internal modules"
                               "does not require any internal module"))])
      (define answer (string-append text ". Dependents: " (string-join t3-dependents ", ")))
      (check-equal? (verdict-of (grade-task 'T3 answer)) 'pass text)))

  (test-case "T3 fail: none of the dependents named"
    (check-equal? (verdict-of (grade-task 'T3 "it is required by some modules")) 'fail))

  (test-case "T3 partial: missing dependents or extra invented .rkt path claims"
    (define missing-one (string-join (rest t3-dependents) ", "))
    (define result (grade-task 'T3 (string-append "fan-out 0. Dependents: " missing-one)))
    (check-equal? (verdict-of result) 'partial)
    (define with-extra (string-append t3-good " Also agent/totally-invented.rkt requires it."))
    (check-equal? (verdict-of (grade-task 'T3 with-extra)) 'partial))

  ;; ---- T4 grading ----------------------------------------------------------------

  (define t4-good
    (string-append "(a) llm/provider-errors.rkt classifies the held request as non-retryable;\n"
                   "(b) runtime/provider-retry.rkt owns the retry/backoff policy.\n"
                   "Pinned by tests/test-retry-error-classification.rkt."))

  (test-case "T4 pass: accepted classification + policy owner + test pin"
    (define result (grade-task 'T4 t4-good))
    (check-equal? (verdict-of result) 'pass))

  (test-case "T4 near-miss: classification resolves only to the iteration-side policy"
    (define near-miss
      (string-append "(a) runtime/iteration/retry-policy.rkt classifies it;\n"
                     "(b) runtime/provider-retry.rkt owns the policy.\n"
                     "Pinned by tests/test-provider-retry-telemetry.rkt."))
    (define result (grade-task 'T4 near-miss))
    (check-equal? (verdict-of result) 'near-miss))

  (test-case "T4 partial: policy owner but no test pin"
    (define result
      (grade-task 'T4 "llm/provider-errors.rkt and runtime/provider-retry.rkt, no test."))
    (check-equal? (verdict-of result) 'partial))

  (test-case "T4 fail: nothing relevant"
    (check-equal? (verdict-of (grade-task 'T4 "somewhere in the llm code, I think")) 'fail))

  ;; ---- grading contract -----------------------------------------------------------

  (test-case "implementation tasks have no text grading"
    (check-false (grade-task 'T2 "any text"))
    (check-false (grade-task 'T5 "any text"))
    (check-exn #rx"not execution-graded" (lambda () (grade-task-by-execution 'T1 "/tmp"))))

  (test-case "pure command builders"
    (check-equal? (t2-test-command "tests/test-gsd-delivery-stop-decision.rkt")
                  '("raco" "test" "tests/test-gsd-delivery-stop-decision.rkt"))
    (define t5 (t5-command))
    (check-equal? (take t5 2) '("racket" "-e"))
    (check-true (string-contains? (third t5) "truncate-with-ellipsis"))
    (check-true (string-contains? (third t5) "a6-ellipsis-accounting")))

  ;; ---- T2 execution grader over a synthetic tree -----------------------------------

  (test-case "t2-verdict: clean run passes, M-STOP-1 run fails, module restored"
    (define tree (make-temporary-file "cl5-t2-~a" 'directory))
    (dynamic-wind
     (lambda () (void))
     (lambda ()
       (define module-dir (build-path tree "extensions" "gsd"))
       (define test-dir (build-path tree "tests"))
       (make-directory* module-dir)
       (make-directory* test-dir)
       (define substitution (m-stop-1-substitution))
       ;; Tiny synthetic module embedding the exact M-STOP-1 source block.
       (define module-text
         (string-append
          "#lang racket/base\n"
          "(provide delivery-stop-outcome)\n"
          "(define (delivery-outcome kind message) (cons kind message))\n"
          "(define (delivery-stop-outcome kind data #:reason-fallback [reason-fallback #f])\n"
          "  (case kind\n"
          "    [(review-infra)\n     "
          (first substitution)
          "]\n"
          "    [else (delivery-outcome 'blocked \"blocked\")]))\n"))
       (define module-path (build-path module-dir "delivery-outcome.rkt"))
       (call-with-output-file module-path
                              (lambda (out) (display module-text out))
                              #:exists 'truncate/replace)
       (call-with-output-file
        (build-path test-dir "test-gsd-delivery-stop-decision.rkt")
        (lambda (out)
          (display
           (string-append "#lang racket\n"
                          "(require rackunit \"../extensions/gsd/delivery-outcome.rkt\")\n"
                          "(test-case \"review-infra stop classifies as review-infra\"\n"
                          "  (check-equal? (delivery-stop-outcome 'review-infra (hash))\n"
                          "                '(review-infra . \"review infrastructure failure\")))\n")
           out))
        #:exists 'truncate/replace)
       (define result (t2-verdict tree #:timeout-ms 120000))
       (check-equal? (hash-ref result 'verdict) 'pass)
       (check-equal? (hash-ref result 'clean-exit) 0)
       (check-true (not (equal? (hash-ref result 'mutated-exit) 0)))
       (check-equal? (hash-ref result 'failing-assertion)
                     "expected: '(review-infra . \"review infrastructure failure\")")
       ;; the tree is restored byte-identically
       (check-equal? (file->string module-path) module-text))
     (lambda () (delete-directory/files tree))))

  ;; ---- T5 execution grader over a synthetic tree ------------------------------------

  ;; A tiny correct-by-construction char-width module: ANSI escapes have zero
  ;; width, CJK chars are 2, combining marks 0, everything else 1; graphemes =
  ;; base char plus following combining marks.
  (define synthetic-char-width-module
    (string-append
     "#lang racket\n"
     "(provide truncate-with-ellipsis string-visible-width substring-by-graphemes grapheme-count)\n"
     "(define ansi-rx (pregexp \"\\u001b\\\\[[0-9;]*m\"))\n"
     "(define (char-width c)\n"
     "  (define cp (char->integer c))\n"
     "  (cond [(<= #x0300 cp #x036F) 0] [(>= cp #x2E80) 2] [else 1]))\n"
     "(define (visible-chars s) (regexp-replace* ansi-rx s \"\"))\n"
     "(define (string-visible-width s)\n"
     "  (for/sum ([c (in-string (visible-chars s))]) (char-width c)))\n"
     "(define (grapheme-list s)\n"
     "  (let loop ([chars (string->list s)] [current '()] [acc '()])\n"
     "    (cond [(null? chars) (reverse (if (null? current) acc (cons current acc)))]\n"
     "          [(zero? (char-width (car chars)))\n"
     "           (loop (cdr chars) (append current (list (car chars))) acc)]\n"
     "          [else (loop (cdr chars) (list (car chars))\n"
     "                      (if (null? current) acc (cons current acc)))])))\n"
     "(define (grapheme-count s) (length (grapheme-list s)))\n"
     "(define (substring-by-graphemes s start end)\n"
     "  (apply string-append\n"
     "         (map list->string (take (drop (grapheme-list s) start) (- end start)))))\n"
     "(define (truncate-with-ellipsis s budget)\n"
     "  (define plain (visible-chars s))\n"
     "  (cond [(zero? budget) \"\"]\n"
     "        [(<= (string-visible-width s) budget) s]\n"
     "        [else\n"
     "         (let loop ([gs (grapheme-list plain)] [used 0] [acc '()])\n"
     "           (if (or (null? gs)\n"
     "                   (> (+ used (for/sum ([c (in-list (car gs))]) (char-width c)) 1) budget))\n"
     "               (string-append (apply string-append (map list->string (reverse acc)))\n"
     "                              \"\\u2026\")\n"
     "               (loop (cdr gs)\n"
     "                     (+ used (for/sum ([c (in-list (car gs))]) (char-width c)))\n"
     "                     (cons (car gs) acc))))]))\n"))

  (test-case "t5-verdict: all six assertions hold on a correct synthetic module"
    (define tree (make-temporary-file "cl5-t5-~a" 'directory))
    (dynamic-wind (lambda () (void))
                  (lambda ()
                    (make-directory* (build-path tree "tui"))
                    (call-with-output-file (build-path tree "tui" "char-width.rkt")
                                           (lambda (out) (display synthetic-char-width-module out))
                                           #:exists 'truncate/replace)
                    (define result (t5-verdict tree #:timeout-ms 120000))
                    (check-equal? (hash-ref result 'verdict) 'pass)
                    (check-equal? (length (hash-ref result 'assertions)) 6)
                    (check-false (hash-ref result 'failing)))
                  (lambda () (delete-directory/files tree))))

  (test-case "t5-verdict: a budget-ignoring module fails with the failing assertion named"
    (define tree (make-temporary-file "cl5-t5bad-~a" 'directory))
    (dynamic-wind
     (lambda () (void))
     (lambda ()
       (make-directory* (build-path tree "tui"))
       (call-with-output-file
        (build-path tree "tui" "char-width.rkt")
        (lambda (out)
          (display (string-append "#lang racket\n"
                                  "(provide truncate-with-ellipsis string-visible-width)\n"
                                  "(define (string-visible-width s) (string-length s))\n"
                                  ;; wrong: ignores the budget entirely
                                  "(define (truncate-with-ellipsis s budget) s)\n")
                   out))
        #:exists 'truncate/replace)
       (define result (t5-verdict tree #:timeout-ms 120000))
       (check-equal? (hash-ref result 'verdict) 'fail)
       ;; budget-0 (a1) is the first violated assertion
       (check-equal? (hash-ref result 'failing) 'a1-zero-budget))
     (lambda () (delete-directory/files tree))))

  (test-case "t5-verdict: a module without truncate-with-ellipsis fails all assertions"
    (define tree (make-temporary-file "cl5-t5none-~a" 'directory))
    (dynamic-wind
     (lambda () (void))
     (lambda ()
       (make-directory* (build-path tree "tui"))
       (call-with-output-file (build-path tree "tui" "char-width.rkt")
                              (lambda (out)
                                (display (string-append "#lang racket\n"
                                                        "(provide string-visible-width)\n"
                                                        "(define (string-visible-width s) 0)\n")
                                         out))
                              #:exists 'truncate/replace)
       (define result (t5-verdict tree #:timeout-ms 120000))
       (check-equal? (hash-ref result 'verdict) 'fail)
       (check-equal? (hash-ref result 'failing) 'a1-zero-budget))
     (lambda () (delete-directory/files tree)))))
