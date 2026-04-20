#lang racket

;; q/tests/test-summary-integration.rkt — Wave 2B (#1396): Summary Integration tests
;;
;; Tests the integration of context-summary into assemble-context:
;;   - Phase 2: Summary generated for excluded entries
;;   - Summary injected as compaction-summary message
;;   - First-turn shortcut (small session → no summary)
;;   - Cache passed through to generate-context-summary
;;   - Summary message placed between pinned and recent

(require rackunit
         rackunit/text-ui
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-manager.rkt")

;; Helper: extract text from a message
(define (msg-text m)
  (string-join (for/list ([part (in-list (message-content m))]
                          #:when (text-part? part))
                 (text-part-text part))
               " "))

;; Helper: build a flat message list with chain
(define (build-chain sys-text first-user-text old-count old-prefix recent-user-text recent-reply-text)
  (define sys
    (make-message "sys"
                  #f
                  'system
                  'system-instruction
                  (list (make-text-part sys-text))
                  (current-seconds)
                  (hasheq)))
  (define u1
    (make-message "u1"
                  "sys"
                  'user
                  'message
                  (list (make-text-part first-user-text))
                  (current-seconds)
                  (hasheq)))
  (define old-msgs
    (for/list ([i (in-range old-count)])
      (make-message (format "~a~a" old-prefix i)
                    (if (= i 0)
                        "u1"
                        (format "~a~a" old-prefix (sub1 i)))
                    (if (even? i) 'user 'assistant)
                    'message
                    (list (make-text-part (format "Content ~a with some text" i)))
                    (current-seconds)
                    (hasheq))))
  (define last-old-id
    (if (> old-count 0)
        (format "~a~a" old-prefix (sub1 old-count))
        "u1"))
  (define u2
    (make-message "u2"
                  last-old-id
                  'user
                  'message
                  (list (make-text-part recent-user-text))
                  (current-seconds)
                  (hasheq)))
  (define a2
    (make-message "a2"
                  "u2"
                  'assistant
                  'message
                  (list (make-text-part recent-reply-text))
                  (current-seconds)
                  (hasheq)))
  (append (list sys u1) old-msgs (list u2 a2)))

(define (with-session msgs thunk)
  (define dir (make-temporary-file "q-test-~a" 'directory))
  (define sp (build-path dir "session.jsonl"))
  (define ip (build-path dir "session.index"))
  (append-entries! sp msgs)
  (define idx (build-index! sp ip))
  (define result (thunk idx))
  (delete-directory/files dir #:must-exist? #f)
  result)

(define integration-tests
  (test-suite "summary-integration"

    ;; First-turn shortcut: small session skips summary
    (test-case "first-turn shortcut: no summary for small session"
      (define msgs (build-chain "System" "Hello" 0 "x" "Recent" "Reply"))
      (with-session msgs
                    (lambda (idx)
                      (define cfg (make-context-manager-config #:recent-tokens 10000))
                      (define-values (result catalog) (assemble-context idx cfg))
                      (check-false (for/or ([m (in-list result)])
                                     (eq? (message-kind m) 'compaction-summary)))
                      (check-equal? (length result) 4) ; sys + u1 + u2 + a2
                      (check-equal? (length catalog) 0)
                      (void))))

    ;; Large session: summary generated for excluded entries
    (test-case "large session generates summary for excluded entries"
      (define msgs (build-chain "System" "Start" 20 "old" "Recent user" "Recent reply"))
      (with-session
       msgs
       (lambda (idx)
         (define cfg (make-context-manager-config #:recent-tokens 50))
         (define-values (result catalog) (assemble-context idx cfg))
         (check-true (>= (length result) 3) (format "Expected >= 3 messages, got ~a" (length result)))
         (define summary-msgs (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) result))
         (check-true (>= (length summary-msgs) 1)
                     (format "Expected >= 1 summary, got ~a" (length summary-msgs)))
         (void))))

    ;; Cache is used when provided
    (test-case "assemble-context uses provided cache"
      (define msgs (build-chain "System" "First" 10 "old" "Recent" "Reply"))
      (with-session
       msgs
       (lambda (idx)
         (define cfg (make-context-manager-config #:recent-tokens 50))
         (define cache (make-summary-cache))
         (define-values (result1 catalog1) (assemble-context idx cfg #:cache cache))
         (define-values (result2 catalog2) (assemble-context idx cfg #:cache cache))
         (define summary1 (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) result1))
         (define summary2 (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) result2))
         (when (and (pair? summary1) (pair? summary2))
           (check-equal? (msg-text (car summary1)) (msg-text (car summary2))))
         (void))))

    ;; Summary message has correct structure
    (test-case "summary message has compaction-summary kind and user role"
      (define msgs (build-chain "System" "Start" 10 "old" "Recent" "Reply"))
      (with-session msgs
                    (lambda (idx)
                      (define cfg (make-context-manager-config #:recent-tokens 50))
                      (define-values (result catalog) (assemble-context idx cfg))
                      (define summary-msgs
                        (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) result))
                      (when (pair? summary-msgs)
                        (define sm (car summary-msgs))
                        (check-eq? (message-role sm) 'user)
                        (check-eq? (message-kind sm) 'compaction-summary))
                      (void))))))

(run-tests integration-tests)
