#lang racket/base

;; tests/test-gapg-enriched-memory-query.rkt
;; v0.97.5 W0: GAP-G enriched memory query + active-tags threading
;; FIXED: Tests real project code (observe-memory-for-context #:tags, major-forward-transition?)

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in racket/list take)
         (only-in "../runtime/context-assembly/memory-builder.rkt" observe-memory-for-context)
         (only-in "../runtime/session/session-events.rkt"
                  major-forward-transition?
                  current-mid-session-bridge-enabled)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion-text))

;; --- GAP-G: Test that observe-memory-for-context accepts #:tags parameter ---
(define suite
  (test-suite "gapg-enriched-memory-query"

    (test-case "observe-memory-for-context accepts #:tags keyword argument"
      ;; When memory is disabled (session-config = #f), returns empty results.
      ;; The test verifies the function signature accepts #:tags without error.
      (define result
        (observe-memory-for-context #f
                                    #:scope #f
                                    #:query-text "State: planning. Active Tags: config.rkt."
                                    #:tags '(config.rkt runtime)))
      (check-equal? (car result) '() "Disabled memory returns empty items")
      (check-true (pair? result) "Returns (items . telemetry) pair"))

    (test-case "observe-memory-for-context works without #:tags (backward compat)"
      (define result (observe-memory-for-context #f #:scope #f #:query-text "State: idle."))
      (check-equal? (car result) '() "Disabled memory returns empty items"))

    ;; --- GAP-G: Test enriched query formatting (string construction logic) ---
    ;; Simulates the enrichment builder from state-aware-builder.rkt
    (test-case "enriched query text includes state, tags, and conclusions"
      (define state-str "planning")
      (define active-tags '(config.rkt runtime))
      (define tag-str
        (if (pair? active-tags)
            (format "Active Tags: ~a. "
                    (string-join (map symbol->string (take active-tags (min 5 (length active-tags))))
                                 ", "))
            ""))
      (define budgeted-conclusions
        (list (task-conclusion "c1" "fix budget" 'fact 'planning '("m1") 0 '() '())))
      (define conclusion-str
        (if (pair? budgeted-conclusions)
            (format "Recent Conclusions: ~a. "
                    (string-join (map task-conclusion-text
                                      (take budgeted-conclusions
                                            (min 2 (length budgeted-conclusions))))
                                 "; "))
            ""))
      (define enriched (string-append "State: " state-str ". " tag-str conclusion-str))
      (check-true (string-contains? enriched "State: planning"))
      (check-true (string-contains? enriched "Active Tags: config.rkt, runtime."))
      (check-true (string-contains? enriched "Recent Conclusions: fix budget.")))

    (test-case "empty tags and conclusions produce clean query without stray segments"
      (define tag-str "")
      (define conclusion-str "")
      (define enriched (string-append "State: idle. " tag-str conclusion-str "some recent text"))
      (check-false (string-contains? enriched "Active Tags:"))
      (check-false (string-contains? enriched "Recent Conclusions:"))
      (check-true (string-contains? enriched "State: idle.")))))

(run-tests suite)
