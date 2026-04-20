#lang racket

;; tests/test-context-manager.rkt — Tests for runtime/context-manager.rkt (#1390)
;; Wave 0: Context Manager Core — replace truncation with assembly

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-manager.rkt")

;; Helpers
(define (make-temp-dir)
  (make-temporary-file "q-ctx-mgr-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (index-path dir)
  (build-path dir "session.index"))

(define (make-test-msg id parent-id role kind text)
  (make-message id parent-id role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define context-manager-tests
  (test-suite "context-manager"

    ;; ============================================================
    ;; Config tests
    ;; ============================================================

    (test-case "config: default construction"
      (define cfg (make-context-manager-config))
      (check-true (context-manager-config? cfg))
      (check-equal? (context-manager-config-recent-tokens cfg) 30000)
      (check-equal? (context-manager-config-max-catalog-entries cfg) 40)
      (check-equal? (context-manager-config-max-catalog-tokens cfg) 2000))

    (test-case "config: custom values"
      (define cfg
        (make-context-manager-config #:recent-tokens 10000
                                     #:max-catalog-entries 20
                                     #:max-catalog-tokens 1000))
      (check-equal? (context-manager-config-recent-tokens cfg) 10000)
      (check-equal? (context-manager-config-max-catalog-entries cfg) 20))

    ;; ============================================================
    ;; assemble-context tests
    ;; ============================================================

    (test-case "assemble-context: empty session returns empty"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (append-entries! sp '())
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config #:recent-tokens 5000))
      (define-values (messages catalog) (assemble-context idx cfg))
      (check-equal? messages '())
      (check-equal? catalog '())
      (delete-directory/files dir #:must-exist? #f))

    (test-case "assemble-context: small session fits entirely in recent window"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (list (make-test-msg "sys" #f 'system 'system-instruction "You are a helpful assistant.")
              (make-test-msg "u1" "sys" 'user 'message "Hello")
              (make-test-msg "a1" "u1" 'assistant 'message "Hi there!")))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config #:recent-tokens 5000))
      (define-values (messages catalog) (assemble-context idx cfg))
      (check-equal? (length messages) 3)
      (check-equal? (message-id (first messages)) "sys")
      (check-equal? (message-id (last messages)) "a1")
      (check-equal? catalog '())
      (delete-directory/files dir #:must-exist? #f))

    (test-case "assemble-context: pins system prompt and first user message"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (cons (make-test-msg "sys" #f 'system 'system-instruction "System prompt")
              (for/list ([i (in-range 50)])
                (make-test-msg (format "msg-~a" i)
                               (if (= i 0)
                                   "sys"
                                   (format "msg-~a" (sub1 i)))
                               (if (even? i) 'user 'assistant)
                               'message
                               (format "Message ~a with enough text to use tokens" i)))))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config #:recent-tokens 200))
      (define-values (messages catalog) (assemble-context idx cfg))
      (define ids (map message-id messages))
      (check-not-false (member "sys" ids) "system prompt is pinned")
      ;; First user msg should be pinned (msg-0)
      (check-not-false (member "msg-0" ids) "first user message is pinned")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "assemble-context: generates catalog for excluded entries"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (cons (make-test-msg "sys" #f 'system 'system-instruction "System")
              (for/list ([i (in-range 20)])
                (make-test-msg (format "msg-~a" i)
                               (if (= i 0)
                                   "sys"
                                   (format "msg-~a" (sub1 i)))
                               (if (even? i) 'user 'assistant)
                               'message
                               (format "Message number ~a content here" i)))))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config #:recent-tokens 100))
      (define-values (messages catalog) (assemble-context idx cfg))
      (check-true (> (length catalog) 0) "catalog contains excluded entries")
      (for ([c (in-list catalog)])
        (check-true (catalog-entry? c))
        (check-true (string? (catalog-entry-id c)))
        (check-true (string? (catalog-entry-summary c))))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "assemble-context: budget enforcement — total within bounds"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (cons (make-test-msg "sys" #f 'system 'system-instruction "System")
              (for/list ([i (in-range 30)])
                (make-test-msg (format "msg-~a" i)
                               (if (= i 0)
                                   "sys"
                                   (format "msg-~a" (sub1 i)))
                               (if (even? i) 'user 'assistant)
                               'message
                               (format "A somewhat longer message ~a to consume budget" i)))))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config #:recent-tokens 500))
      (define-values (messages catalog) (assemble-context idx cfg))
      (define total-tokens (for/sum ([m (in-list messages)]) (estimate-cm-message-tokens m)))
      (check-true (<= total-tokens 800)
                  (format "total tokens ~a within budget+overhead" total-tokens))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "assemble-context: respects max-catalog-entries cap"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (cons (make-test-msg "sys" #f 'system 'system-instruction "System")
              (for/list ([i (in-range 100)])
                (make-test-msg (format "msg-~a" i)
                               (if (= i 0)
                                   "sys"
                                   (format "msg-~a" (sub1 i)))
                               (if (even? i) 'user 'assistant)
                               'message
                               (format "Message ~a" i)))))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config #:recent-tokens 100 #:max-catalog-entries 10))
      (define-values (messages catalog) (assemble-context idx cfg))
      (check-true (<= (length catalog) 10) (format "catalog ~a ≤ 10" (length catalog)))
      (delete-directory/files dir #:must-exist? #f))

    ;; ============================================================
    ;; generate-catalog tests
    ;; ============================================================

    (test-case "generate-catalog: one-line-per-entry summary"
      (define entries
        (list (make-test-msg "m1" #f 'user 'message "Analyze the training_pdf.py failure")
              (make-test-msg "m2" "m1" 'assistant 'message "Called bash python3 ~/training_pdf.py")
              (make-test-msg "m3" "m2" 'tool 'tool-result "3 errors found in fpdf2")))
      (define catalog (generate-catalog entries))
      (check-equal? (length catalog) 3)
      (for ([c (in-list catalog)])
        (check-true (<= (string-length (catalog-entry-summary c)) 80))))

    (test-case "generate-catalog: empty entries returns empty catalog"
      (check-equal? (generate-catalog '()) '()))

    ;; ============================================================
    ;; catalog-entry struct tests
    ;; ============================================================

    (test-case "catalog-entry struct: fields"
      (define ce (catalog-entry "msg-001" "user" "Analyze the failure"))
      (check-equal? (catalog-entry-id ce) "msg-001")
      (check-equal? (catalog-entry-role ce) "user")
      (check-equal? (catalog-entry-summary ce) "Analyze the failure"))))

(run-tests context-manager-tests)
