#lang racket

;; Tests for the CL5 P3 case collector (#9982). The collector's job is to record
;; cases fail-closed: every test below either proves a derivation or proves that a
;; refusal/append-only rule holds. Git, the CI API and the classifier are injected,
;; so these tests do not depend on network or on this repository's history depth.

(require json
         rackunit
         racket/file
         racket/port
         racket/string
         "../scripts/cl5/p3-collect.rkt")

;; ---- eligibility predicates -------------------------------------------------------

(test-case "E6: version-bump-only requires a non-empty change of version files only"
  (check-true (changed-files->version-bump-only? '("info.rkt")))
  (check-true (changed-files->version-bump-only? '("info.rkt" "util/version.rkt")))
  (check-false (changed-files->version-bump-only? '("info.rkt" "agent/loop.rkt")))
  (check-false (changed-files->version-bump-only? '())))

(test-case "E2: partial engine presence is absence"
  (check-true
   (engine-present?
    (hash "qt" #t "scripts/testing/quick-audit.rkt" #t "scripts/testing/full-plan.rkt" #t)))
  (check-false
   (engine-present?
    (hash "qt" #t "scripts/testing/quick-audit.rkt" #t "scripts/testing/full-plan.rkt" #f)))
  (check-false (engine-present? (hash))))

(define (job name conclusion)
  (hash 'name name 'conclusion conclusion))

(define (all-required-jobs)
  (for/list ([n (in-list required-job-names)])
    (job n "success")))

(define (run #:name [name "CI"] #:event [event "push"] #:conclusion [conclusion "success"])
  (hash 'name name 'event event 'conclusion conclusion))

(test-case "E5: the pinned producer set must all be green — a missing job is not a pass"
  (check-true (run-eligible? (run) (all-required-jobs)))
  (check-false (run-eligible? (run) (drop-right (all-required-jobs) 1)))
  (check-false (run-eligible? (run) (cons (job "security" "failure") (drop (all-required-jobs) 1)))))

(test-case "E5: only a successful CI push run counts"
  (check-false (run-eligible? (run #:conclusion "failure") (all-required-jobs)))
  (check-false (run-eligible? (run #:event "pull_request") (all-required-jobs)))
  (check-false (run-eligible? (run #:name "pr-latency-guard") (all-required-jobs)))
  (check-false (run-eligible? #f '())))

;; ---- surface shape and explain parsing --------------------------------------------

(test-case "explain output contributes only planned test files"
  (define text
    (string-append "header line\n"
                   "tests/test-a.rkt\n"
                   "tests/test-b.rkt\n"
                   "some note about tests\n"
                   ";; files: 2\n"))
  (check-equal? (parse-explain-files text) '("tests/test-a.rkt" "tests/test-b.rkt"))
  (check-equal? (parse-explain-files "") '()))

(test-case "the surface counts executions (slots) and unique coverage separately"
  (define shape (comprehensive-shape '(("tests/a.rkt" "tests/b.rkt") ("tests/b.rkt" "tests/c.rkt"))))
  (check-equal? (hash-ref shape 'slot_count) 4)
  (check-equal? (hash-ref shape 'unique_files) 3))

;; ---- classifier plumbing ----------------------------------------------------------

(test-case "a classified case yields its stratum, its modules and its cones"
  (define m
    (hash 'cases
          (list (hash 'stratum
                      'S2
                      'production-modules
                      (list "util/message/message.rkt" "agent/loop.rkt")
                      'cone-keys
                      (list "gov:util/message/message.rkt")))))
  (define s (strata-of-classification m))
  (check-equal? (hash-ref s 'stratum) 'S2)
  (check-equal? (length (hash-ref s 'production_modules)) 2))

(test-case "a classifier exclusion leaves the case without a stratum (E7)"
  (check-false (hash-ref (strata-of-classification (hash 'cases '())) 'stratum)))

;; ---- CI run pagination ------------------------------------------------------------

(test-case "the CI run is found when it sits behind a full first page"
  ;; Real shape: a busy commit can carry more workflow_run entries than one page.
  (define pages
    (hash "page-1"
          (hash 'total_count
                12
                'workflow_runs
                (for/list ([i (in-range 10)])
                  (hash 'name "Shard plan telemetry" 'id i)))
          "page-2"
          (hash 'total_count
                12
                'workflow_runs
                (list (hash 'name "pr-latency-guard" 'id 10)
                      (hash 'name "CI" 'id 11 'event "push" 'conclusion "success")))))
  (define (fetch path)
    (hash-ref pages (if (regexp-match? #rx"[?&]page=1$" path) "page-1" "page-2")))
  (define runs (runs-for-sha-paged fetch "abc"))
  (check-equal? (length runs) 12)
  (check-equal? (hash-ref (for/first ([r (in-list runs)]
                                      #:when (equal? (hash-ref r 'name) "CI"))
                            r)
                          'id)
                11))

;; ---- append-only storage ---------------------------------------------------------

(define (sample-case #:sha [sha "0123456789abcdef0123456789abcdef01234567"])
  (case-record
   sha
   "89abcdef0123456789abcdef0123456789abcdef"
   "2026-11-25T00:00:00Z"
   "H"
   "subject"
   '("agent/loop.rkt")
   #f
   (hash 'qt #t 'quick-audit #t 'full-plan #t)
   "manifest"
   (hash 'stratum 'S1 'production_modules '("agent/loop.rkt") 'cone_keys '("gov:agent/loop.rkt"))
   (hash 'run_id 1 'conclusion "success" 'event "push" 'jobs '())
   (hash 'unique_files 3 'slot_count 4)
   "eligible"
   #f
   #f))

(test-case "cases append once and are never rewritten"
  (define path (make-temporary-file "p3-cases-~a.jsonl"))
  (check-equal? (length (append-cases! path (list (sample-case)))) 1)
  (check-equal? (length (append-cases! path (list (sample-case)))) 0)
  (check-equal? (length (file->lines path)) 1)
  ;; a second, different case appends a second line
  (check-equal?
   (length (append-cases! path (list (sample-case #:sha "1111111111111111111111111111111111111111"))))
   1)
  (check-equal? (length (file->lines path)) 2)
  (check-equal? (set->list (existing-shas path))
                (list "0123456789abcdef0123456789abcdef01234567"
                      "1111111111111111111111111111111111111111")))

(test-case "the JSON form uses null (not false) for an absent reason and detail"
  (define j (case->jsexpr (sample-case)))
  (check-equal? (hash-ref j 'reason) 'null)
  (check-equal? (hash-ref j 'detail) 'null)
  (check-equal? (hash-ref j 'schema) "cl5-p3-case/1")
  ;; it must be writable as JSON
  (check-true (string? (jsexpr->string j))))
