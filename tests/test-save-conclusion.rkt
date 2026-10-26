#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-save-conclusion.rkt — tests for save-conclusion and set-task-state tools
;; v0.75.1 W0: Tool definitions + validation

(require rackunit
         rackunit/text-ui
         "../tools/builtins/save-conclusion.rkt"
         "../tools/builtins/set-task-state.rkt"
         "../tools/tool.rkt"
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion-text
                  task-conclusion-category
                  task-conclusion-relevance-tags))

;; save-conclusion and set-task-state are tool structs created by define-tool

(define (call-tool t args)
  ((tool-execute t) args #f))

;; R94: models the actual route for LLM tool calls — JSON-decoded arguments
;; hash -> validate-tool-args -> handler execution.
(define (pipeline-execute t args)
  (validate-tool-args t args)
  ((tool-execute t) args #f))

;; R94: handler execution with an event-capturing exec-context so rejection
;; paths can be asserted side-effect free (no event, no conclusion).
(define (call-tool-with-events t args)
  (define events (box '()))
  (define ctx
    (make-exec-context
     #:event-publisher
     (lambda (event-type payload)
       (set-box! events (cons (cons event-type payload) (unbox events))))))
  (define result ((tool-execute t) args ctx))
  (values result (unbox events)))

(define (deprecated-events events)
  (filter (lambda (e) (equal? (car e) "tool.deprecated")) events))

;; R94: error results carry an EMPTY details hash, so "no stored conclusion"
;; means the '_conclusion key is absent — not that details is #f.
(define (stored-conclusion result)
  (define d (tool-result-details result))
  (and (hash? d) (hash-ref d '_conclusion #f)))

(define suite
  (test-suite "task-tools"

    ;; ── Tool metadata ──

    (test-case "save-conclusion is a valid tool"
      (check-true (tool? save-conclusion))
      (check-equal? (tool-name save-conclusion) "save-conclusion"))

    (test-case "set-task-state is a valid tool"
      (check-true (tool? set-task-state))
      (check-equal? (tool-name set-task-state) "set-task-state"))

    ;; ── save-conclusion handler ──

    (test-case "save-conclusion creates conclusion with content"
      (define result (call-tool save-conclusion (hasheq 'content "Found the bug")))
      (check-false (tool-result-is-error? result))
      (check-not-false (tool-result-details result)))

    (test-case "save-conclusion rejects missing content"
      (define result (call-tool save-conclusion (hasheq)))
      (check-true (tool-result-is-error? result)))

    (test-case "save-conclusion rejects invalid category"
      (define result (call-tool save-conclusion (hasheq 'content "test" 'category 'invalid)))
      (check-true (tool-result-is-error? result)))

    (test-case "save-conclusion accepts all valid categories"
      (for ([cat (in-list '(fact decision pattern error-cause test-result))])
        (define result (call-tool save-conclusion (hasheq 'content "test" 'category cat)))
        (check-false (tool-result-is-error? result) (format "category ~a should be accepted" cat))))

    (test-case "save-conclusion with tags"
      (define result (call-tool save-conclusion (hasheq 'content "test" 'tags '(bug pattern))))
      (check-false (tool-result-is-error? result)))

    (test-case "save-conclusion creates task-conclusion in details"
      (define result (call-tool save-conclusion (hasheq 'content "test conclusion" 'tags '(arch))))
      (define details (tool-result-details result))
      (check-not-false (hash-ref details '_conclusion #f))
      (define c (hash-ref details '_conclusion))
      (check-equal? (task-conclusion-text c) "test conclusion")
      (check-equal? (task-conclusion-relevance-tags c) '(arch)))

    ;; ── set-task-state handler ──

    (test-case "set-task-state accepts valid state + event"
      (define result (call-tool set-task-state (hasheq 'state 'exploration 'event 'begin-explore)))
      (check-false (tool-result-is-error? result)))

    (test-case "set-task-state rejects missing state"
      (define result (call-tool set-task-state (hasheq 'event 'begin-explore)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state rejects missing event"
      (define result (call-tool set-task-state (hasheq 'state 'exploration)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state rejects invalid state"
      (define result (call-tool set-task-state (hasheq 'state 'invalid 'event 'begin-explore)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state rejects invalid event"
      (define result (call-tool set-task-state (hasheq 'state 'exploration 'event 'invalid)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state packages target and event in details"
      (define result (call-tool set-task-state (hasheq 'state 'planning 'event 'begin-plan)))
      (define details (tool-result-details result))
      (check-equal? (hash-ref details '_task-state-target #f) 'planning)
      (check-equal? (hash-ref details '_task-state-event #f) 'begin-plan))

    ;; v0.75.9: LLM sends string arguments, not symbols
    (test-case "set-task-state accepts string args (LLM path)"
      (define result (call-tool set-task-state (hasheq 'state "exploration" 'event "begin-explore")))
      (check-false (tool-result-is-error? result)
                   (format "string args should work, got: ~a" (tool-result-content result))))

    (test-case "set-task-state rejects invalid string state"
      (define result (call-tool set-task-state (hasheq 'state "invalid" 'event "begin-explore")))
      (check-true (tool-result-is-error? result)))

    ;; ============================================================
    ;; R94 (BUG-0094): JSON wire types -> internal symbols at the
    ;; handler boundary. Wire schema keeps strings as advertised types;
    ;; normalization happens only inside the handler, after scheduler
    ;; approval/hook final admission. Default ONLY an omitted category.
    ;; ============================================================

    (test-case "R94: all five JSON string categories pass schema and normalize to symbols"
      (for ([cat-str (in-list '("fact" "decision" "pattern" "error-cause" "test-result"))]
            [cat-sym (in-list '(fact decision pattern error-cause test-result))])
        (define result
          (pipeline-execute save-conclusion (hasheq 'content "json cat" 'category cat-str)))
        (check-false (tool-result-is-error? result)
                     (format "string category ~a should succeed" cat-str))
        (define c (hash-ref (tool-result-details result) '_conclusion #f))
        (check-not-false c)
        (check-eq? (task-conclusion-category c) cat-sym)))

    (test-case "R94: nonempty JSON string tag array passes schema and normalizes to symbols"
      (define result
        (pipeline-execute save-conclusion (hasheq 'content "t" 'tags '("alpha" "beta"))))
      (check-false (tool-result-is-error? result))
      (define c (hash-ref (tool-result-details result) '_conclusion #f))
      (check-not-false c)
      (check-equal? (task-conclusion-relevance-tags c) '(alpha beta)))

    (test-case "R94: omitted category and tags default (fact / empty), and nothing else"
      (define result (pipeline-execute save-conclusion (hasheq 'content "defaults")))
      (check-false (tool-result-is-error? result))
      (define c (hash-ref (tool-result-details result) '_conclusion #f))
      (check-not-false c)
      (check-eq? (task-conclusion-category c) 'fact)
      (check-equal? (task-conclusion-relevance-tags c) '()))

    (test-case "R94: explicitly empty tags list is accepted"
      (define result (pipeline-execute save-conclusion (hasheq 'content "t" 'tags '())))
      (check-false (tool-result-is-error? result)))

    (test-case "R94: valid internal symbol calls still succeed at the handler boundary"
      (for ([cat (in-list '(fact decision pattern error-cause test-result))])
        (define result (call-tool save-conclusion (hasheq 'content "sym" 'category cat)))
        (check-false (tool-result-is-error? result)
                     (format "symbol category ~a should be preserved" cat))))

    (test-case "R94: mixed string/symbol internal tags normalize to symbols"
      (define result
        (pipeline-execute save-conclusion (hasheq 'content "mixed" 'tags '("wire" internal))))
      (check-false (tool-result-is-error? result))
      (define c (hash-ref (tool-result-details result) '_conclusion #f))
      (check-not-false c)
      (check-equal? (task-conclusion-relevance-tags c) '(wire internal)))

    (test-case "R94: unknown/empty/wrong-case string categories are handler-rejected"
      (for ([bad (in-list '("unknown-thing" "Fact" "DECISION" "test_result" ""))])
        ;; type-valid strings pass the wire schema (no broadening) ...
        (check-true (validate-tool-args save-conclusion (hasheq 'content "t" 'category bad)))
        ;; ... and are rejected by the handler with a structured error
        (define-values (result events)
          (call-tool-with-events save-conclusion (hasheq 'content "t" 'category bad)))
        (check-true (tool-result-is-error? result) (format "category ~v must be rejected" bad))
        (check-false (stored-conclusion result) "no _conclusion detail on rejection")
        (check-equal? (deprecated-events events) '() "no deprecation event on rejection")))

    (test-case "R94: null/boolean/number/object categories rejected at schema and handler"
      (for ([bad (in-list (list 'null #t 42 (hasheq 'x 1)))])
        ;; JSON null/boolean/number/object fail the string-typed wire schema ...
        (check-exn exn:fail?
                   (lambda ()
                     (validate-tool-args save-conclusion (hasheq 'content "t" 'category bad)))
                   (format "category ~v must fail schema validation" bad))
        ;; ... and the handler still rejects them (defense in depth, no side effects)
        (define-values (result events)
          (call-tool-with-events save-conclusion (hasheq 'content "t" 'category bad)))
        (check-true (tool-result-is-error? result) (format "category ~v must be rejected" bad))
        (check-false (stored-conclusion result))
        (check-equal? (deprecated-events events) '())))

    (test-case "R94: invalid internal symbol category is handler-rejected"
      (define result (call-tool save-conclusion (hasheq 'content "t" 'category 'Fact)))
      (check-true (tool-result-is-error? result)))

    (test-case "R94: non-list tags rejected at schema and handler with no side effects"
      (for ([bad (in-list (list "not-a-list" 42 'null))])
        (check-exn exn:fail?
                   (lambda () (validate-tool-args save-conclusion (hasheq 'content "t" 'tags bad)))
                   (format "tags ~v must fail schema validation" bad))
        (define-values (result events)
          (call-tool-with-events save-conclusion (hasheq 'content "t" 'tags bad)))
        (check-true (tool-result-is-error? result) (format "tags ~v must be rejected" bad))
        (check-false (stored-conclusion result))
        (check-equal? (deprecated-events events) '())))

    (test-case "R94: null/number/boolean/object tag elements are handler-rejected"
      (for ([bad-tags
             (in-list (list (list "ok" 'null) (list "ok" 42) (list "ok" #f) (list "ok" (hasheq))))])
        ;; a well-formed list passes the array-typed wire schema (element
        ;; types are not schema-checked) ...
        (check-true (validate-tool-args save-conclusion (hasheq 'content "t" 'tags bad-tags)))
        ;; ... and the handler rejects the malformed element
        (define-values (result events)
          (call-tool-with-events save-conclusion (hasheq 'content "t" 'tags bad-tags)))
        (check-true (tool-result-is-error? result) (format "tags ~v must be rejected" bad-tags))
        (check-false (stored-conclusion result) "no _conclusion detail on rejection")
        (check-equal? (deprecated-events events) '())))

    (test-case "R94: missing/non-string content rejected at schema and handler"
      (check-exn exn:fail? (lambda () (validate-tool-args save-conclusion (hasheq))))
      (check-exn exn:fail?
                 (lambda () (validate-tool-args save-conclusion (hasheq 'content 42))))
      (define-values (r1 e1) (call-tool-with-events save-conclusion (hasheq)))
      (check-true (tool-result-is-error? r1))
      (check-false (stored-conclusion r1))
      (check-equal? (deprecated-events e1) '())
      (define-values (r2 e2) (call-tool-with-events save-conclusion (hasheq 'content 'null)))
      (check-true (tool-result-is-error? r2))
      (check-false (stored-conclusion r2))
      (check-equal? (deprecated-events e2) '()))

    (test-case "R94: success emits exactly one deprecation event and stores the conclusion"
      (define-values (result events)
        (call-tool-with-events
         save-conclusion (hasheq 'content "ok" 'category "pattern" 'tags '("x"))))
      (check-false (tool-result-is-error? result))
      (check-equal? (length (deprecated-events events)) 1)
      (check-eq? (task-conclusion-category
                   (hash-ref (tool-result-details result) '_conclusion))
                 'pattern))))

(run-tests suite)

;; ---------------------------------------------------------------------------
;; R99a (BUG-0099): public/internal category split — 'provenance is stored-only.
;; The public save surface keeps exactly the five insight categories.
;; ---------------------------------------------------------------------------

(require (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion-category?
                  stored-task-conclusion-category?
                  valid-categories
                  stored-categories))

(define-test-suite r99a-save-public-category-tests
  (test-case "R99a: public predicate keeps exactly five categories; provenance not public"
    (check-false (task-conclusion-category? 'provenance))
    (check-true (stored-task-conclusion-category? 'provenance))
    (check-false (member 'provenance valid-categories))
    (check-not-false (member 'provenance stored-categories))
    (check-false (stored-task-conclusion-category? 'mystery)))

  (test-case "R99a: save_conclusion rejects symbol category 'provenance (side-effect free)"
    (define-values (result events)
      (call-tool-with-events save-conclusion (hasheq 'content "x" 'category 'provenance)))
    (check-true (tool-result-is-error? result))
    (check-false (stored-conclusion result))
    (check-equal? (deprecated-events events) '()))

  (test-case "R99a: save_conclusion rejects wire string category \"provenance\""
    (define result (pipeline-execute save-conclusion (hasheq 'content "x" 'category "provenance")))
    (check-true (tool-result-is-error? result))
    (check-false (stored-conclusion result)))

  (test-case "R99a: save_conclusion still accepts all five public categories"
    (for ([cat (in-list '("fact" "decision" "pattern" "error-cause" "test-result"))])
      (define result (pipeline-execute save-conclusion (hasheq 'content "x" 'category cat)))
      (check-false (tool-result-is-error? result) (format "~a should be accepted" cat)))))

(run-tests r99a-save-public-category-tests)
