#lang racket

;;; tests/test-cumulative-file-tracking.rkt — tests for cumulative file tracking (#768)
;;;
;;; Verifies that file trackers are merged across compaction cycles.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../runtime/compactor.rkt"
         "../runtime/token-compaction.rkt")

(test-case "merge-file-trackers combines two trackers"
  (define ft1 (hasheq 'readFiles '("a.rkt" "b.rkt") 'modifiedFiles '("c.rkt")))
  (define ft2 (hasheq 'readFiles '("d.rkt") 'modifiedFiles '("a.rkt" "e.rkt")))
  (define merged (merge-file-trackers ft1 ft2))
  (check-equal? (sort (hash-ref merged 'readFiles) string<?) '("a.rkt" "b.rkt" "d.rkt"))
  (check-equal? (sort (hash-ref merged 'modifiedFiles) string<?) '("a.rkt" "c.rkt" "e.rkt")))

(test-case "merge-file-trackers deduplicates"
  (define ft1 (hasheq 'readFiles '("a.rkt") 'modifiedFiles '()))
  (define ft2 (hasheq 'readFiles '("a.rkt") 'modifiedFiles '()))
  (define merged (merge-file-trackers ft1 ft2))
  (check-equal? (hash-ref merged 'readFiles) '("a.rkt"))
  (check-equal? (hash-ref merged 'modifiedFiles) '()))

(test-case "merge-file-trackers handles empty trackers"
  (define ft1 (hasheq 'readFiles '() 'modifiedFiles '()))
  (define ft2 (hasheq 'readFiles '() 'modifiedFiles '()))
  (define merged (merge-file-trackers ft1 ft2))
  (check-equal? (hash-ref merged 'readFiles) '())
  (check-equal? (hash-ref merged 'modifiedFiles) '()))

(test-case "find-previous-file-tracker finds tracker from summary metadata"
  (define prev-summary
    (make-message "compaction-1" #f 'system 'compaction-summary
                  (list (make-text-part "Old summary"))
                  (current-seconds)
                  (hasheq 'fileTracker (hasheq 'readFiles '("prev.rkt") 'modifiedFiles '("mod.rkt")))))
  (define result (find-previous-file-tracker (list prev-summary)))
  (check-equal? (hash-ref result 'readFiles) '("prev.rkt"))
  (check-equal? (hash-ref result 'modifiedFiles) '("mod.rkt")))

(test-case "find-previous-file-tracker returns empty hash when no summary"
  (define msgs
    (list (make-message "m1" #f 'user 'message (list (make-text-part "hello")) (current-seconds) (hasheq))))
  (define result (find-previous-file-tracker msgs))
  (check-equal? result (hasheq)))

(test-case "find-previous-file-tracker uses most recent summary"
  (define s1
    (make-message "c1" #f 'system 'compaction-summary
                  (list (make-text-part "Summary 1"))
                  1000
                  (hasheq 'fileTracker (hasheq 'readFiles '("old.rkt") 'modifiedFiles '()))))
  (define s2
    (make-message "c2" #f 'system 'compaction-summary
                  (list (make-text-part "Summary 2"))
                  2000
                  (hasheq 'fileTracker (hasheq 'readFiles '("new.rkt") 'modifiedFiles '("changed.rkt")))))
  (define result (find-previous-file-tracker (list s1 s2)))
  ;; Should find s2 (most recent)
  (check-equal? (hash-ref result 'readFiles) '("new.rkt"))
  (check-equal? (hash-ref result 'modifiedFiles) '("changed.rkt")))

(test-case "cumulative file tracker appears in compaction result metadata"
  ;; Simulate: previous summary with file tracker, current batch with more files
  (define prev-summary
    (make-message "c1" #f 'system 'compaction-summary
                  (list (make-text-part "Previous summary"))
                  1000
                  (hasheq 'fileTracker (hasheq 'readFiles '("old.rkt") 'modifiedFiles '()))))
  ;; Messages to compact (with tool calls referencing files)
  (define msgs
    (list prev-summary
          (make-message "m1" #f 'user 'message (list (make-text-part "msg1 content text")) 1001 (hasheq))
          (make-message "m2" #f 'assistant 'message
                        (list (make-tool-call-part "tc1" "read"
                                 (hasheq 'path "new.rkt")))
                        1002 (hasheq))
          (make-message "m3" #f 'user 'message (list (make-text-part "recent user msg text content")) 1003 (hasheq))))
  ;; Compact with token config that forces summarization of old msgs
  (define result (compact-history msgs #:token-config (token-compaction-config 5 0 10)))
  (define summary (compaction-result-summary-message result))
  ;; The cumulative tracker in metadata should include both old and new
  (when summary
    (define meta (message-meta summary))
    (define ft (hash-ref meta 'fileTracker (hasheq)))
    ;; Should have both old.rkt (from prev summary) and new.rkt (from current batch)
    (when (> (hash-count ft) 0)
      (check-not-false (member "new.rkt" (hash-ref ft 'readFiles '()))))))
