#lang racket

;; tests/test-session-recall.rkt — Tests for tools/builtins/session-recall.rkt (#1391)
;; Wave 1: Session Recall Tool

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-id
                  message-role
                  message-kind
                  message-content)
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../tools/tool.rkt"
         "../tools/builtins/session-recall.rkt")

;; Helpers
(define (make-temp-dir)
  (make-temporary-file "q-session-recall-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (index-path dir)
  (build-path dir "session.index"))

(define (make-test-msg id parent-id role kind text)
  (make-message id parent-id role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-exec-ctx-with-index idx)
  (make-exec-context #:session-metadata (hasheq 'session-index idx)))

(define session-recall-tests
  (test-suite "session-recall-tool"

    ;; ============================================================
    ;; Tool definition
    ;; ============================================================

    (test-case "tool-session-recall is a procedure"
      (check-true (procedure? tool-session-recall)))

    ;; ============================================================
    ;; Recall by entry_ids
    ;; ============================================================

    (test-case "recall by entry_ids returns matching messages"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (list (make-test-msg "root" #f 'user 'message "Hello")
              (make-test-msg "c1" "root" 'assistant 'message "Hi")
              (make-test-msg "c2" "c1" 'user 'message "How are you?")
              (make-test-msg "c3" "c2" 'assistant 'message "I'm good")))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define ctx (make-exec-ctx-with-index idx))
      (define result (tool-session-recall (hasheq 'entry_ids '("c1" "c3")) ctx))
      (check-false (tool-result-is-error? result))
      (define content (tool-result-content result))
      (check-true (list? content))
      (define text (hash-ref (car content) 'text ""))
      (check-true (string-contains? text "c1") "contains c1")
      (check-true (string-contains? text "c3") "contains c3")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "recall by entry_ids with non-existent id returns error"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries (list (make-test-msg "root" #f 'user 'message "Hello")))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define ctx (make-exec-ctx-with-index idx))
      (define result (tool-session-recall (hasheq 'entry_ids '("nonexistent")) ctx))
      (check-true (tool-result-is-error? result)))

    ;; ============================================================
    ;; Recall by query (text search)
    ;; ============================================================

    (test-case "recall by query finds matching entries"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (list (make-test-msg "m1" #f 'user 'message "Analyze the fpdf2 failure")
              (make-test-msg "m2" "m1" 'assistant 'message "Running python3 training_pdf.py")
              (make-test-msg "m3" "m2" 'tool 'tool-result "ImportError in fpdf2 module")
              (make-test-msg "m4" "m3" 'assistant 'message "The issue is with fpdf2")))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define ctx (make-exec-ctx-with-index idx))
      (define result (tool-session-recall (hasheq 'query "fpdf2") ctx))
      (check-false (tool-result-is-error? result))
      (define text (hash-ref (car (tool-result-content result)) 'text ""))
      (check-true (string-contains? text "fpdf2") "result contains fpdf2"))

    (test-case "recall by query with no matches returns info message"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries (list (make-test-msg "m1" #f 'user 'message "Hello")))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define ctx (make-exec-ctx-with-index idx))
      (define result (tool-session-recall (hasheq 'query "nonexistent_topic") ctx))
      (check-false (tool-result-is-error? result))
      (define text (hash-ref (car (tool-result-content result)) 'text ""))
      (check-true (string-contains? text "No matching") "no matches info"))

    ;; ============================================================
    ;; Recall by range
    ;; ============================================================

    (test-case "recall by range returns entries between from and to"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries
        (for/list ([i (in-range 10)])
          (make-test-msg (format "msg-~a" i)
                         (if (= i 0)
                             #f
                             (format "msg-~a" (sub1 i)))
                         (if (even? i) 'user 'assistant)
                         'message
                         (format "Message ~a" i))))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define ctx (make-exec-ctx-with-index idx))
      (define result (tool-session-recall (hasheq 'range (hasheq 'from "msg-2" 'to "msg-5")) ctx))
      (check-false (tool-result-is-error? result))
      (define text (hash-ref (car (tool-result-content result)) 'text ""))
      (check-true (string-contains? text "msg-2") "contains msg-2")
      (check-true (string-contains? text "msg-5") "contains msg-5")
      (check-false (string-contains? text "msg-0") "excludes msg-0")
      (delete-directory/files dir #:must-exist? #f))

    ;; ============================================================
    ;; Result format
    ;; ============================================================

    (test-case "recall result has structured format with entry headers"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (define entries (list (make-test-msg "m1" #f 'user 'message "Analyze the failure")))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define ctx (make-exec-ctx-with-index idx))
      (define result (tool-session-recall (hasheq 'entry_ids '("m1")) ctx))
      (define text (hash-ref (car (tool-result-content result)) 'text ""))
      (check-true (string-contains? text "Recalled") "has recall header")
      (check-true (string-contains? text "m1") "has entry id"))

    ;; ============================================================
    ;; Edge cases
    ;; ============================================================

    (test-case "recall with no session index returns error"
      (define ctx (make-exec-context)) ; no session-metadata
      (define result (tool-session-recall (hasheq 'entry_ids '("m1")) ctx))
      (check-true (tool-result-is-error? result)))

    (test-case "recall with empty args returns error"
      (define ctx (make-exec-context #:session-metadata (hasheq 'session-index #f)))
      (define result (tool-session-recall (hasheq) ctx))
      (check-true (tool-result-is-error? result)))))

(run-tests session-recall-tests)
