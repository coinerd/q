#lang racket

;;; tests/test-branch-summarization.rkt — tests for branch summarization (#776)

(require rackunit
         rackunit/text-ui
         racket/file
         "../util/protocol-types.rkt"
         "../runtime/session-index.rkt"
         "../runtime/session-store.rkt")

(define (make-temp-dir)
  (make-temporary-file "q-branch-test-~a" 'directory))

(define (make-test-index messages)
  ;; Create a temp dir, write messages as JSONL, build index
  (define tmpdir (make-temp-dir))
  (define log-path (build-path tmpdir "session.jsonl"))
  (define idx-path (build-path tmpdir "session.index"))
  (append-entries! log-path messages)
  (define idx (build-index! log-path idx-path))
  (values idx tmpdir))

(define (cleanup! tmpdir)
  (delete-directory/files tmpdir #:must-exist? #f))

(test-case "find-common-ancestor returns #f for unrelated entries"
  (define m1
    (make-message "a" #f 'user 'message (list (make-text-part "hello")) (current-seconds) (hasheq)))
  (define m2
    (make-message "b" #f 'user 'message (list (make-text-part "world")) (current-seconds) (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2)))
  (dynamic-wind void
                (lambda () (check-equal? (find-common-ancestor idx "a" "b") #f))
                (lambda () (cleanup! tmpdir))))

(test-case "find-common-ancestor finds parent"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define m2
    (make-message "child-a"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part "a"))
                  (current-seconds)
                  (hasheq)))
  (define m3
    (make-message "child-b"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part "b"))
                  (current-seconds)
                  (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2 m3)))
  (dynamic-wind void
                (lambda () (check-equal? (find-common-ancestor idx "child-a" "child-b") "root"))
                (lambda () (cleanup! tmpdir))))

(test-case "find-common-ancestor finds grandparent"
  (define m1
    (make-message "gp" #f 'user 'message (list (make-text-part "gp")) (current-seconds) (hasheq)))
  (define m2
    (make-message "parent"
                  "gp"
                  'assistant
                  'message
                  (list (make-text-part "parent"))
                  (current-seconds)
                  (hasheq)))
  (define m3
    (make-message "child"
                  "parent"
                  'user
                  'message
                  (list (make-text-part "child"))
                  (current-seconds)
                  (hasheq)))
  (define m4
    (make-message "other"
                  "gp"
                  'assistant
                  'message
                  (list (make-text-part "other"))
                  (current-seconds)
                  (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2 m3 m4)))
  (dynamic-wind void
                (lambda () (check-equal? (find-common-ancestor idx "child" "other") "gp"))
                (lambda () (cleanup! tmpdir))))

(test-case "find-common-ancestor returns self for same node"
  (define m1
    (make-message "a" #f 'user 'message (list (make-text-part "a")) (current-seconds) (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1)))
  (dynamic-wind void
                (lambda () (check-equal? (find-common-ancestor idx "a" "a") "a"))
                (lambda () (cleanup! tmpdir))))

(test-case "collect-branch-entries collects along path"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define m2
    (make-message "mid"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part "mid"))
                  (current-seconds)
                  (hasheq)))
  (define m3
    (make-message "leaf"
                  "mid"
                  'user
                  'message
                  (list (make-text-part "leaf"))
                  (current-seconds)
                  (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2 m3)))
  (dynamic-wind void
                (lambda ()
                  (define entries (collect-branch-entries idx "leaf" "root" 10000))
                  ;; Should include m3 (leaf) and m2 (mid), stopping at root
                  (check-equal? (length entries) 2)
                  (check-equal? (message-id (first entries)) "mid")
                  (check-equal? (message-id (second entries)) "leaf"))
                (lambda () (cleanup! tmpdir))))

;; ============================================================
;; Tests: branch-with-summary!
;; ============================================================

(test-case "branch-with-summary! creates summary message with correct kind"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define m2
    (make-message "child"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part "child"))
                  (current-seconds)
                  (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2)))
  (dynamic-wind void
                (lambda ()
                  (define summary (branch-with-summary! idx "child" "Summary of child branch"))
                  (check-true (message? summary))
                  (check-equal? (message-kind summary) 'branch-summary)
                  (check-equal? (message-parent-id summary) "child"))
                (lambda () (cleanup! tmpdir))))

(test-case "branch-with-summary! summary text content is correct"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1)))
  (dynamic-wind void
                (lambda ()
                  (define summary (branch-with-summary! idx "root" "Custom summary text"))
                  (define content-text
                    (string-join (for/list ([part (message-content summary)])
                                   (if (text-part? part)
                                       (text-part-text part)
                                       ""))
                                 ""))
                  (check-equal? content-text "Custom summary text"))
                (lambda () (cleanup! tmpdir))))

(test-case "branch-with-summary! moves active leaf to summary message"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define m2
    (make-message "child"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part "child"))
                  (current-seconds)
                  (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2)))
  (dynamic-wind void
                (lambda ()
                  (branch-with-summary! idx "root" "Summary")
                  (define active (active-leaf idx))
                  (check-not-false active)
                  (check-equal? (message-kind active) 'branch-summary))
                (lambda () (cleanup! tmpdir))))

(test-case "branch-with-summary! returns #f for nonexistent entry"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1)))
  (dynamic-wind void
                (lambda () (check-false (branch-with-summary! idx "nonexistent" "Summary")))
                (lambda () (cleanup! tmpdir))))

(test-case "branch-with-summary! adds summary child to target entry"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define m2
    (make-message "child"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part "child"))
                  (current-seconds)
                  (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2)))
  (dynamic-wind void
                (lambda ()
                  (define summary (branch-with-summary! idx "root" "Summary"))
                  ;; root should now have 2 children: child + summary
                  (define kids (children-of idx "root"))
                  (check-equal? (length kids) 2)
                  (define kid-ids (map message-id kids))
                  (check-not-false (member "child" kid-ids))
                  (check-not-false (member (message-id summary) kid-ids)))
                (lambda () (cleanup! tmpdir))))

(test-case "branch-with-summary! summary has correct parent-id"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define m2
    (make-message "a"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part "a"))
                  (current-seconds)
                  (hasheq)))
  (define m3
    (make-message "b" "a" 'user 'message (list (make-text-part "b")) (current-seconds) (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2 m3)))
  (dynamic-wind void
                (lambda ()
                  (define summary (branch-with-summary! idx "a" "Summary of a"))
                  (check-equal? (message-parent-id summary) "a")
                  ;; Verify the summary is a child of 'a'
                  (define a-children (children-of idx "a"))
                  (check-equal? (length a-children) 2) ; b + summary
                  )
                (lambda () (cleanup! tmpdir))))

(test-case "collect-branch-entries respects token budget"
  (define m1
    (make-message "root" #f 'user 'message (list (make-text-part "root")) (current-seconds) (hasheq)))
  (define m2
    (make-message "mid"
                  "root"
                  'assistant
                  'message
                  (list (make-text-part (make-string 1000 #\x)))
                  (current-seconds)
                  (hasheq)))
  (define m3
    (make-message "leaf"
                  "mid"
                  'user
                  'message
                  (list (make-text-part (make-string 1000 #\y)))
                  (current-seconds)
                  (hasheq)))
  (define-values (idx tmpdir) (make-test-index (list m1 m2 m3)))
  (dynamic-wind void
                (lambda ()
                  ;; With a small budget, should not collect everything
                  (define entries (collect-branch-entries idx "leaf" "root" 10))
                  ;; Budget of 10 tokens means only ~40 chars — just m3 (leaf) barely fits
                  (check-true (< (length entries) 3)))
                (lambda () (cleanup! tmpdir))))
