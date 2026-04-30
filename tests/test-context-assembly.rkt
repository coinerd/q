#lang racket
;; STABILITY: evolving

;; q/tests/test-context-assembly.rkt — Tests for context-assembly.rkt

(require rackunit
         racket/list
         racket/file
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-assembly.rkt")

;; Helper: create a test message
(define (make-test-msg id role kind text [parent #f])
  (make-message id parent role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-timestamped-message id parent-id role kind ts)
  (make-message id parent-id role kind (list (make-text-part (format "~a-entry" id))) ts (hasheq)))

(define (make-temp-dir)
  (make-temporary-file "q-ctx-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (index-path dir)
  (build-path dir "session.index"))

;; ============================================================
;; Core exports
;; ============================================================

(test-case "exports: build-assembled-context is procedure"
  (check-true (procedure? build-assembled-context)))

(test-case "exports: build-session-context is procedure"
  (check-true (procedure? build-session-context)))

(test-case "exports: make-context-assembly-config is procedure"
  (check-true (procedure? make-context-assembly-config)))

(test-case "config rejects negative recent-tokens"
  (check-exn exn:fail? (lambda () (make-context-assembly-config #:recent-tokens -1))))

(test-case "config rejects zero recent-tokens"
  (check-exn exn:fail? (lambda () (make-context-assembly-config #:recent-tokens 0))))

(test-case "config rejects string max-catalog-entries"
  (check-exn exn:fail? (lambda () (make-context-assembly-config #:max-catalog-entries "forty"))))

(test-case "config rejects negative summary-window"
  (check-exn exn:fail? (lambda () (make-context-assembly-config #:summary-window -100))))

(test-case "config accepts valid defaults"
  (check-not-exn (lambda () (make-context-assembly-config))))

;; ============================================================
;; context-result: with session index
;; ============================================================

(test-case "context-result: linear path"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  (define entries
    (list (make-timestamped-message "root" #f 'user 'message 1000)
          (make-timestamped-message "c1" "root" 'assistant 'message 1001)
          (make-timestamped-message "c2" "c1" 'user 'message 1002)))
  (append-entries! sp entries)
  (define idx (build-index! sp ip))
  (define result (build-assembled-context idx (make-context-assembly-config)))
  (check-true (context-result? result))
  (check >= (length (context-result-messages result)) 3)
  (check >= (context-result-total-tokens result) 0)
  (check-false (context-result-over-budget? result)))

(test-case "context-result: with system instruction pinned"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  (define entries
    (list (make-timestamped-message "sys" #f 'system 'system-instruction 1000)
          (make-timestamped-message "u1" "sys" 'user 'message 1001)
          (make-timestamped-message "a1" "u1" 'assistant 'message 1002)))
  (append-entries! sp entries)
  (define idx (build-index! sp ip))
  (define result (build-assembled-context idx (make-context-assembly-config)))
  (check >= (context-result-pinned-count result) 1))

(test-case "context-result: excluded messages with small budget"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  ;; Each message ~70 chars ≈ 15 tokens; 30 messages ≈ 450 tokens
  ;; Use small budget to force exclusion
  (define entries
    (for/list ([i (in-range 30)])
      (make-message (format "m~a" i)
                    (if (= i 0)
                        #f
                        (format "m~a" (sub1 i)))
                    (if (even? i) 'user 'assistant)
                    'message
                    (list (make-text-part
                           (format "This is message number ~a with enough text to consume tokens" i)))
                    (+ 1000 i)
                    (hasheq))))
  (append-entries! sp entries)
  (define idx (build-index! sp ip))
  (define result (build-assembled-context idx (make-context-assembly-config #:recent-tokens 50)))
  (check >= (context-result-excluded-count result) 1))

;; ============================================================
;; build-session-context: backward compat
;; ============================================================

(test-case "build-session-context: returns messages list"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  (define entries
    (list (make-timestamped-message "u1" #f 'user 'message 1000)
          (make-timestamped-message "a1" "u1" 'assistant 'message 1001)))
  (append-entries! sp entries)
  (define idx (build-index! sp ip))
  (define result (build-session-context idx))
  (check-pred list? result)
  (check >= (length result) 2))

;; ============================================================
;; Tiered context
;; ============================================================

(test-case "tiered-context struct"
  (define tc (tiered-context '() '() '()))
  (check-true (tiered-context? tc))
  (check-equal? (tiered-context->message-list tc) '()))

(test-case "build-tiered-context: basic split"
  (define msgs
    (list (make-test-msg "s1" 'system 'system-instruction "System")
          (make-test-msg "u1" 'user 'message "Hello")
          (make-test-msg "a1" 'assistant 'message "Hi")
          (make-test-msg "u2" 'user 'message "How are you?")))
  (define tc (build-tiered-context msgs))
  (check-true (tiered-context? tc))
  (check >= (length (tiered-context-tier-a tc)) 1)
  (check-equal? (length (tiered-context->message-list tc)) (length msgs)))

(test-case "build-tiered-context-with-hooks: no dispatcher"
  (define msgs (list (make-test-msg "u1" 'user 'message "Hello")))
  (define-values (tc hook-result) (build-tiered-context-with-hooks msgs))
  (check-true (tiered-context? tc))
  (check-false hook-result))

;; ============================================================
;; context-assembly-summary distinct from compaction-summary
;; ============================================================

(test-case "summary uses context-assembly-summary kind"
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  (define entries
    (for/list ([i (in-range 10)])
      (make-timestamped-message (format "m~a" i)
                                (if (= i 0)
                                    #f
                                    (format "m~a" (sub1 i)))
                                (if (even? i) 'user 'assistant)
                                'message
                                (+ 1000 i))))
  (append-entries! sp entries)
  (define idx (build-index! sp ip))
  (define result (build-assembled-context idx (make-context-assembly-config #:recent-tokens 100)))
  (define summary-msgs
    (filter (lambda (m) (eq? (message-kind m) 'context-assembly-summary))
            (context-result-messages result)))
  ;; If summary was injected, it should have the right kind
  (for ([m (in-list summary-msgs)])
    (check-equal? (message-kind m) 'context-assembly-summary))
  ;; No compaction-summary should appear from assembly (those come from session store)
  (define compaction-msgs
    (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) (context-result-messages result)))
  (check-equal? compaction-msgs '()))

;; ============================================================
;; Catalog
;; ============================================================

(test-case "generate-catalog: basic"
  (define msgs
    (list (make-test-msg "u1" 'user 'message "Hello world")
          (make-test-msg "a1" 'assistant 'message "Hi there")))
  (define catalog (generate-catalog msgs))
  (check >= (length catalog) 1)
  (for ([entry (in-list catalog)])
    (check-true (catalog-entry? entry))))

(test-case "generate-catalog: respects max-entries"
  (define msgs
    (for/list ([i (in-range 50)])
      (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i))))
  (define catalog (generate-catalog msgs #:max-entries 5))
  (check <= (length catalog) 5))

;; ============================================================
;; Summary cache
;; ============================================================

(test-case "summary-cache: store and lookup"
  (define cache (make-summary-cache))
  (summary-cache-store! cache "from1" "to1" "summary text")
  (check-equal? (summary-cache-lookup cache "from1" "to1") "summary text")
  (check-false (summary-cache-lookup cache "from2" "to2")))

(test-case "summary-cache: miss returns #f"
  (define cache (make-summary-cache))
  (check-false (summary-cache-lookup cache "x" "y")))
