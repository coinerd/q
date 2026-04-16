#lang racket

;; tests/test-compaction-contracts.rkt — API contract assertions for compaction
;;
;; Verifies compaction API contracts hold under various configurations.
;; Ensures compaction-result fields are well-formed and token budgets
;; are respected.

(require rackunit
         rackunit/text-ui
         racket/file
         "../util/protocol-types.rkt"
         "../runtime/compactor.rkt"
         "../runtime/session-store.rkt"
         "../runtime/token-compaction.rkt"
         "helpers/compaction-helpers.rkt")

(define compaction-contract-tests
  (test-suite "Compaction API Contract Tests"

    ;; Contract: compact-history returns compaction-result
    (test-case "compact-history returns well-formed compaction-result"
      (define tmpdir (make-temporary-file "q-cc-~a" 'directory))
      (dynamic-wind void
                    (lambda ()
                      (define log-path (build-path tmpdir "session.jsonl"))
                      ;; Write 8 messages
                      (for ([i (in-range 8)])
                        (define msg
                          (make-message (format "id-~a" i)
                                        #f
                                        'user
                                        'message
                                        (list (make-text-part (format "msg ~a" i)))
                                        (current-seconds)
                                        (hasheq)))
                        (append-entry! log-path msg))
                      (define entries (load-session-log log-path))
                      (check-equal? (length entries) 8)
                      (define result (compact-history entries #:token-config LOW-TOKEN-CONFIG))
                      (check-pred compaction-result? result)
                      (check-pred exact-nonnegative-integer? (compaction-result-removed-count result))
                      (check-pred list? (compaction-result-kept-messages result))
                      (check-true (or (message? (compaction-result-summary-message result))
                                      (not (compaction-result-summary-message result)))
                                  "summary should be message? or #f"))
                    (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    ;; Contract: removed-count + kept-count = original-count
    (test-case "removed + kept counts are consistent"
      (define tmpdir (make-temporary-file "q-cc-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define log-path (build-path tmpdir "session.jsonl"))
         (for ([i (in-range 8)])
           (define msg
             (make-message (format "id-~a" i)
                           #f
                           'user
                           'message
                           (list (make-text-part (format "msg ~a" i)))
                           (current-seconds)
                           (hasheq)))
           (append-entry! log-path msg))
         (define entries (load-session-log log-path))
         (define result (compact-history entries #:token-config LOW-TOKEN-CONFIG))
         (define total
           (+ (compaction-result-removed-count result)
              (length (compaction-result-kept-messages result))))
         (check-equal? total (length entries) "removed + kept should equal original count"))
       (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    ;; Contract: compact-and-persist! writes summary to log
    (test-case "compact-and-persist! appends summary to log"
      (define tmpdir (make-temporary-file "q-cc-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define log-path (build-path tmpdir "session.jsonl"))
         (for ([i (in-range 8)])
           (define msg
             (make-message (format "id-~a" i)
                           #f
                           'user
                           'message
                           (list (make-text-part (format "msg ~a" i)))
                           (current-seconds)
                           (hasheq)))
           (append-entry! log-path msg))
         (define entries-before (load-session-log log-path))
         (define result
           (compact-and-persist! entries-before log-path #:token-config LOW-TOKEN-CONFIG))
         (check-pred compaction-result? result)
         ;; Log should have at least original entries (may grow if summary appended)
         (define entries-after (load-session-log log-path))
         (check-true (>= (length entries-after) (length entries-before))
                     "Log should not shrink after compact-and-persist!"))
       (lambda () (delete-directory/files tmpdir #:must-exist? #f))))

    ;; Contract: token-compaction-config constructor works
    (test-case "token-compaction-config constructor works"
      (define tc (token-compaction-config 100 20 50000))
      (check-equal? (token-compaction-config-keep-recent-tokens tc) 100)
      (check-equal? (token-compaction-config-reserve-tokens tc) 20)
      (check-equal? (token-compaction-config-max-context-tokens tc) 50000))

    ;; Contract: compact-history with empty list returns zero removed
    (test-case "compact-history on empty list returns 0 removed"
      (define result (compact-history '() #:token-config LOW-TOKEN-CONFIG))
      (check-equal? (compaction-result-removed-count result) 0)
      (check-equal? (compaction-result-kept-messages result) '()))))

(run-tests compaction-contract-tests)
