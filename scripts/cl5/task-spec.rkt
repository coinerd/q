#lang racket/base

;; scripts/cl5/task-spec.rkt — machine-readable projection of the frozen CL0
;; agent-task benchmark corpus (CL0 report #9977; CL5 W1 harness #9982).
;;
;; Single authority: docs/reports/CL0-AGENT-TASK-BENCHMARK-003e425d.md.
;;
;; Embed-vs-parse decision: the prompts and frozen answer facts below are
;; EMBEDDED copies, not parsed from the report at load time. Parsing prose
;; markdown would make every harness run depend on the report's exact
;; typography (line wrapping, list layout) and would silently produce
;; different data the day the report is reformatted; embedding plus a drift
;; test is the simpler, more honest projection. tests/test-cl5-task-spec.rkt
;; asserts that every embedded prompt string and every embedded .rkt path
;; appears VERBATIM in the report file, so the report stays the single
;; authority and any corpus amendment must land in the report first.
;;
;; Plain data + accessors by design: five fixed tasks, no framework, no
;; structs — a hash per spec is the weakest sufficient representation.

(require racket/list
         racket/string)

(provide corpus-revision
         task-specs
         task-spec
         validate-task-specs)

;; The frozen starting revision for every task (CL0 report header).
(define corpus-revision "003e425d2f1206436eb9c0b84353604ce85bb082")

(define report-file "docs/reports/CL0-AGENT-TASK-BENCHMARK-003e425d.md")

;; ---- verbatim prompts (CL0 report §2–§3, including surrounding quotes) ----

(define t1-prompt
  "\"A campaign has waves `[w0 done, w1 deferred]`, no cancellation, no pending delivery,
no authenticated proofs, lane view `#f`, stale flag `#f`. What single decision does
`decide-campaign-action` return? Report every decision field: kind, route, code, wave-index,
merge-sha. Name the module that owns the decision function and the module that is its only
effect interpreter. Answer with: kind, route, code, wave-index, merge-sha, decision module
path, interpreter module path, durable-state writer module paths.\"")

(define t3-prompt
  "\"An edit to `agent/state.rkt` makes `./qt quick --changed HEAD` select roughly a dozen
audited test owners. Explain why, and list the production modules that directly require
`agent/state.rkt`.\"")

(define t4-prompt
  "\"A DeepSeek-compatible provider stalls: HTTP 200 with SSE headers but zero chunks
before the stream timeout ('held request'). Where does the code live that (a) classifies this
case as non-retryable for the circuit breaker, (b) owns the retry/backoff policy configuration?
Answer with the two owning module paths and one test file that pins each.\"")

(define t2-prompt
  "\"Add one rackunit `test-case` to `tests/test-gsd-delivery-stop-decision.rkt` that pins
one additional row of the pure typed-stop classification table of
`extensions/gsd/delivery-outcome.rkt` (read the module's contract comments first). Do not
modify any production file.\"")

(define t5-prompt
  "\"Add a pure function `truncate-with-ellipsis : string? exact-nonnegative-integer? ->
string?` to `tui/char-width.rkt` that returns the input unchanged when its visible width fits
the budget, and otherwise truncates it (reusing the module's grapheme/width machinery) and
appends a single-cell ellipsis so the result's visible width never exceeds the budget. Add
one `test-case` for it in `tests/tui/test-char-width.rkt`. Nothing else changes.\"")

;; ---- frozen answer facts (CL0 report §2–§3 oracles) ----

(define t1-answer
  ;; Graded tuple in order kind/route/code/wave-index/merge-sha, plus the
  ;; three module roles (decision, interpreter, durable-state writers).
  (hash 'decision-tuple
        (list 'complete 'none 'campaign-complete #f #f)
        'decision-module
        "extensions/gsd/campaign-decision.rkt"
        'interpreter-module
        "extensions/gsd/go-orchestrator.rkt"
        'writer-modules
        (list "extensions/gsd/campaign-state.rkt" "extensions/gsd/campaign-repository.rkt")))

(define t3-answer
  ;; 13 direct static dependents of agent/state.rkt measured at the pinned
  ;; revision; require fan-out of agent/state.rkt itself is 0.
  (hash 'target-module
        "agent/state.rkt"
        'direct-dependents
        (list "agent/effect-executor.rkt"
              "agent/event-emitter.rkt"
              "agent/iteration/main-loop.rkt"
              "agent/loop-dispatch.rkt"
              "agent/loop-phases.rkt"
              "agent/loop-stream.rkt"
              "agent/loop.rkt"
              "agent/stream-runner.rkt"
              "extensions/gsd/author-execution.rkt"
              "extensions/gsd/command-handlers.rkt"
              "extensions/gsd/go-orchestrator.rkt"
              "extensions/gsd/stall-policy.rkt"
              "runtime/iteration/step-executor.rkt")
        'require-fan-out
        0))

(define t4-answer
  ;; (a) classification: provider error/retry layer; (b) policy owner. A
  ;; classification answer naming only the iteration-side typed policy is a
  ;; recorded near-miss, not a failure.
  (hash 'classification-accepted
        (list "llm/provider-errors.rkt" "runtime/provider-retry.rkt")
        'classification-near-miss
        "runtime/iteration/retry-policy.rkt"
        'policy-owner
        "runtime/provider-retry.rkt"
        'accepted-test-pins
        (list "tests/test-retry-error-classification.rkt"
              "tests/test-provider-retry-ceiling-config.rkt"
              "tests/test-provider-retry-telemetry.rkt")))

(define t2-answer
  (hash 'mutation
        "M-STOP-1"
        'owner-test
        "tests/test-gsd-delivery-stop-decision.rkt"
        'mutated-module
        "extensions/gsd/delivery-outcome.rkt"))

(define t5-answer
  (hash 'module "tui/char-width.rkt" 'test-file "tests/tui/test-char-width.rkt" 'assertion-count 6))

;; ---- task specs ----

;; Each spec:
;;   id                  — 'T1 .. 'T5
;;   kind                — 'navigation or 'implementation
;;   prompt              — verbatim prompt text (incl. surrounding quotes)
;;   corpus-revision     — the frozen starting revision
;;   provenance          — report section the spec projects
;;   ownership-set       — paths the task may read/write; '() for navigation
;;                         tasks (any write there is unrelated by definition)
;;   write-paths         — the subset the oracle's diff constraint requires to
;;                         have changed (T2 may modify only its test file; the
;;                         production reference is read-only)
;;   owner-location-paths— the authoritative implementation paths used by the
;;                         owner-location-time metric (CL0 report §1 step 4)
;;   answer-spec         — task-specific frozen facts (hash)
;;   oracle              — 'text (grade answer text) or 'execution (grade the
;;                         resulting tree); see scripts/cl5/oracle.rkt

(define task-specs
  (list (hash 'id
              'T1
              'kind
              'navigation
              'prompt
              t1-prompt
              'corpus-revision
              corpus-revision
              'provenance
              (string-append report-file " §2 T1")
              'ownership-set
              '()
              'write-paths
              '()
              'owner-location-paths
              (list "extensions/gsd/campaign-decision.rkt")
              'answer-spec
              t1-answer
              'oracle
              'text)
        (hash 'id
              'T3
              'kind
              'navigation
              'prompt
              t3-prompt
              'corpus-revision
              corpus-revision
              'provenance
              (string-append report-file " §2 T3")
              'ownership-set
              '()
              'write-paths
              '()
              'owner-location-paths
              (list "agent/state.rkt")
              'answer-spec
              t3-answer
              'oracle
              'text)
        (hash 'id
              'T4
              'kind
              'navigation
              'prompt
              t4-prompt
              'corpus-revision
              corpus-revision
              'provenance
              (string-append report-file " §2 T4")
              'ownership-set
              '()
              'write-paths
              '()
              'owner-location-paths
              (list "llm/provider-errors.rkt" "runtime/provider-retry.rkt")
              'answer-spec
              t4-answer
              'oracle
              'text)
        (hash 'id
              'T2
              'kind
              'implementation
              'prompt
              t2-prompt
              'corpus-revision
              corpus-revision
              'provenance
              (string-append report-file " §3 T2")
              'ownership-set
              (list "extensions/gsd/delivery-outcome.rkt" "tests/test-gsd-delivery-stop-decision.rkt")
              'write-paths
              (list "tests/test-gsd-delivery-stop-decision.rkt")
              'owner-location-paths
              (list "extensions/gsd/delivery-outcome.rkt")
              'answer-spec
              t2-answer
              'oracle
              'execution)
        (hash 'id
              'T5
              'kind
              'implementation
              'prompt
              t5-prompt
              'corpus-revision
              corpus-revision
              'provenance
              (string-append report-file " §3 T5")
              'ownership-set
              (list "tui/char-width.rkt" "tests/tui/test-char-width.rkt")
              'write-paths
              (list "tui/char-width.rkt" "tests/tui/test-char-width.rkt")
              'owner-location-paths
              (list "tui/char-width.rkt")
              'answer-spec
              t5-answer
              'oracle
              'execution)))

;; ---- accessors ----

;; Lookup by task id; raises a clear error on an unknown id.
(define (task-spec id)
  (or (for/first ([spec (in-list task-specs)]
                  #:when (eq? (hash-ref spec 'id) id))
        spec)
      (raise-arguments-error 'task-spec
                             "unknown task id"
                             "id"
                             id
                             "known"
                             (map (lambda (spec) (hash-ref spec 'id)) task-specs))))

(define (list-of-strings? value)
  (and (list? value) (andmap string? value)))

(define (validate-one spec)
  (define id (hash-ref spec 'id #f))
  (unless (memq id '(T1 T2 T3 T4 T5))
    (raise-arguments-error 'validate-task-specs "spec has an unknown task id" "id" id))
  (define kind (hash-ref spec 'kind #f))
  (unless (memq kind '(navigation implementation))
    (raise-arguments-error 'validate-task-specs "spec has an unknown kind" "task" id "kind" kind))
  (define prompt (hash-ref spec 'prompt #f))
  (unless (and (string? prompt) (non-empty-string? prompt))
    (raise-arguments-error 'validate-task-specs "spec has an empty prompt" "task" id))
  (unless (equal? (hash-ref spec 'corpus-revision #f) corpus-revision)
    (raise-arguments-error 'validate-task-specs
                           "spec cites the wrong corpus revision"
                           "task"
                           id
                           "corpus-revision"
                           (hash-ref spec 'corpus-revision #f)))
  (define ownership-set (hash-ref spec 'ownership-set #f))
  (unless (list-of-strings? ownership-set)
    (raise-arguments-error 'validate-task-specs
                           "spec has no ownership set (must be a list of paths)"
                           "task"
                           id))
  (when (and (eq? kind 'implementation) (null? ownership-set))
    (raise-arguments-error 'validate-task-specs
                           "implementation task must own at least one path"
                           "task"
                           id))
  (define write-paths (hash-ref spec 'write-paths #f))
  (unless (list-of-strings? write-paths)
    (raise-arguments-error 'validate-task-specs
                           "spec has no write-paths list (may be empty for navigation tasks)"
                           "task"
                           id))
  (for ([path (in-list write-paths)])
    (unless (member path ownership-set)
      (raise-arguments-error 'validate-task-specs
                             "write-path is not inside the ownership set"
                             "task"
                             id
                             "path"
                             path)))
  (when (and (eq? kind 'implementation) (null? write-paths))
    (raise-arguments-error 'validate-task-specs
                           "implementation task must require at least one written file"
                           "task"
                           id))
  (unless (list-of-strings? (hash-ref spec 'owner-location-paths #f))
    (raise-arguments-error 'validate-task-specs "spec has no owner-location paths" "task" id))
  (unless (memq (hash-ref spec 'oracle #f) '(text execution))
    (raise-arguments-error 'validate-task-specs
                           "spec has an unknown oracle kind"
                           "task"
                           id
                           "oracle"
                           (hash-ref spec 'oracle #f)))
  (unless (hash? (hash-ref spec 'answer-spec #f))
    (raise-arguments-error 'validate-task-specs "spec has no answer-spec hash" "task" id))
  spec)

;; Raises exn:fail with a clear message on the first malformed spec;
;; returns the validated list otherwise.
(define (validate-task-specs specs)
  (for-each validate-one specs)
  (define ids (map (lambda (spec) (hash-ref spec 'id)) specs))
  (unless (equal? ids '(T1 T3 T4 T2 T5))
    (raise-arguments-error 'validate-task-specs
                           "specs must be the five frozen tasks in corpus order T1 T3 T4 T2 T5"
                           "got"
                           ids))
  specs)

;; Fail fast at require time: a malformed projection is a harness bug.
(void (validate-task-specs task-specs))
