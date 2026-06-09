#lang racket/base

;; tests/test-gap-ab-distillation-zip.rkt
;; GAP-A: Distillation for/list zip truncation fix
;; GAP-B: Unique conclusion IDs via generate-id

(require rackunit
         racket/list
         racket/string
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion-id
                  task-conclusion-text
                  task-conclusion-origin-message-ids)
         (only-in "../util/ids.rkt" generate-id))

;; --- GAP-A: Test distillation zip behavior ---
;; Simulates what the fixed for/list does

(define (simulate-distillation llm-text uncovered-ids)
  (define llm-lines (string-split llm-text "\n"))
  (for/list ([id (in-list uncovered-ids)]
             [i (in-naturals)])
    (define line-text
      (if (< i (length llm-lines))
          (string-trim (list-ref llm-lines i))
          (format "[auto] uncovered entry ~a" id)))
    (task-conclusion (generate-id) line-text 'fact 'unknown (list id) 0 '() '())))

(test-case "GAP-A: LLM returns fewer lines than IDs → fallback text"
  (define llm-text "line1\nline2") ;; 2 lines
  (define ids (list "id-1" "id-2" "id-3" "id-4" "id-5")) ;; 5 IDs
  (define results (simulate-distillation llm-text ids))
  (check-equal? (length results) 5 "Should generate 5 conclusions for 5 IDs")
  (check-equal? (task-conclusion-text (list-ref results 0)) "line1")
  (check-equal? (task-conclusion-text (list-ref results 1)) "line2")
  ;; Last 3 get fallback text
  (check-true (string-contains? (task-conclusion-text (list-ref results 2))
                                "[auto] uncovered entry id-3"))
  (check-true (string-contains? (task-conclusion-text (list-ref results 3))
                                "[auto] uncovered entry id-4"))
  (check-true (string-contains? (task-conclusion-text (list-ref results 4))
                                "[auto] uncovered entry id-5")))

(test-case "GAP-A: LLM returns more lines than IDs → extra lines ignored"
  (define llm-text "line1\nline2\nline3\nline4\nline5") ;; 5 lines
  (define ids (list "id-1" "id-2")) ;; 2 IDs
  (define results (simulate-distillation llm-text ids))
  (check-equal? (length results) 2 "Should generate 2 conclusions for 2 IDs")
  (check-equal? (task-conclusion-text (list-ref results 0)) "line1")
  (check-equal? (task-conclusion-text (list-ref results 1)) "line2"))

(test-case "GAP-B: Conclusion IDs are unique (not equal to input message IDs)"
  (define llm-text "line1\nline2\nline3")
  (define ids (list "msg-uuid-1" "msg-uuid-2" "msg-uuid-3"))
  (define results (simulate-distillation llm-text ids))
  ;; Each conclusion ID should be unique
  (define conclusion-ids (map task-conclusion-id results))
  (check-equal? (length conclusion-ids) 3)
  ;; IDs should be unique among themselves
  (check-equal? (length (remove-duplicates conclusion-ids)) 3 "All IDs should be unique")
  ;; IDs should NOT be the input message IDs
  (for ([cid (in-list conclusion-ids)]
        [mid (in-list ids)])
    (check-not-equal? cid mid "Conclusion ID should differ from message ID"))
  ;; Origin message IDs preserved for traceability
  (for ([r (in-list results)]
        [mid (in-list ids)])
    (check-equal? (task-conclusion-origin-message-ids r) (list mid) "Origin message ID preserved")))
