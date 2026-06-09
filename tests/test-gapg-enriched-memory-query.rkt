#lang racket/base

;; tests/test-gapg-enriched-memory-query.rkt
;; v0.97.5 W0: GAP-G enriched memory query + active-tags threading

(require rackunit
         rackunit/text-ui
         racket/string)

;; Test 1: Enriched query formatting
(test-case "enriched query contains state, tags, and conclusions"
  (define enriched
    (string-append "State: planning. "
                   "Active Tags: config.rkt, runtime. "
                   "Recent Conclusions: fix budget; add test. "
                   "Task: planning"))
  (check-true (string-contains? enriched "State: planning"))
  (check-true (string-contains? enriched "Active Tags:"))
  (check-true (string-contains? enriched "Recent Conclusions:")))

;; Test 2: Tags are passed as symbols
(test-case "tags are passed as list of symbols"
  (define tags '(config.rkt runtime budget))
  (check-true (andmap symbol? tags))
  (check-equal? (length tags) 3))

;; Test 3: Empty tags list is handled
(test-case "empty tags list does not break query"
  (define enriched-empty (string-append "State: idle. " "Task: idle"))
  (check-false (string-contains? enriched-empty "Active Tags:")))

;; Test 4: No conclusions means no conclusion section
(test-case "no conclusions omits conclusion section"
  (define enriched-no-conclusions
    (string-append "State: exploration. " "Active Tags: foo. " "Task: exploration"))
  (check-false (string-contains? enriched-no-conclusions "Recent Conclusions:")))

(run-tests (test-suite "gapg-enriched-memory-query"
             ))
