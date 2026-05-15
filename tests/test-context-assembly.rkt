#lang racket
;; BOUNDARY: integration

;; STABILITY: evolving

;; q/tests/test-context-assembly.rkt — Tests for context-assembly.rkt

(require rackunit
         racket/list
         racket/string
         racket/file
         "../util/protocol-types.rkt"
         "../util/content-parts.rkt"
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

;; ============================================================
;; v0.28.21 W4: Dynamic Tier-B sizing
;; ============================================================

(test-case "compute-dynamic-tier-b-count: minimum is 20"
  (check-equal? (compute-dynamic-tier-b-count 5) 20)
  (check-equal? (compute-dynamic-tier-b-count 0) 20)
  (check-equal? (compute-dynamic-tier-b-count 100) 20))

(test-case "compute-dynamic-tier-b-count: scales at total/10"
  (check-equal? (compute-dynamic-tier-b-count 200) 20)
  (check-equal? (compute-dynamic-tier-b-count 300) 30)
  (check-equal? (compute-dynamic-tier-b-count 400) 40)
  (check-equal? (compute-dynamic-tier-b-count 500) 50))

(test-case "compute-dynamic-tier-b-count: capped at 50"
  (check-equal? (compute-dynamic-tier-b-count 600) 50)
  (check-equal? (compute-dynamic-tier-b-count 1000) 50))

(test-case "build-tiered-context: dynamic tier-b grows with messages"
  ;; With 300 messages, dynamic tier-b = 30
  (define msgs-300
    (for/list ([i (in-range 300)])
      (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i))))
  (define tiered-300 (build-tiered-context msgs-300 #:tier-c-count 4))
  (define tier-b-300 (tiered-context-tier-b tiered-300))
  ;; Tier-B should be ~30 (300/10 = 30), minus tier-c (4)
  (check-true (>= (length tier-b-300) 20) "tier-b scales with message count"))

(test-case "build-tiered-context: explicit tier-b-count overrides dynamic"
  (define msgs
    (for/list ([i (in-range 300)])
      (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i))))
  (define tiered (build-tiered-context msgs #:tier-b-count 5 #:tier-c-count 4))
  (check-true (<= (length (tiered-context-tier-b tiered)) 5)
              "explicit tier-b-count overrides dynamic"))

;; ============================================================
;; v0.28.21 W5: Tool result summarization
;; ============================================================

(test-case "summarize-tool-result: short result unchanged"
  (define short-msg (make-test-msg "tr1" 'tool 'tool-result "short output"))
  (define result (summarize-tool-result short-msg))
  (check-equal? result short-msg "short tool result not modified"))

(test-case "summarize-tool-result: long result gets truncated"
  (define long-text
    (string-join (for/list ([i (in-range 100)])
                   (format "Line ~a: ~a" i (make-string 100 #\x)))
                 "\n"))
  (define long-msg (make-test-msg "tr2" 'tool 'tool-result long-text))
  (define result (summarize-tool-result long-msg))
  (define result-text
    (string-join (for/list ([part (in-list (message-content result))]
                            #:when (text-part? part))
                   (text-part-text part))
                 "\n"))
  (check-true (< (string-length result-text) (string-length long-text)) "long tool result truncated")
  (check-true (string-contains? result-text "lines truncated") "truncation indicator present"))

;; ============================================================
;; v0.28.21 W7: GSD progress pinning
;; ============================================================

(define (make-gsd-msg id text)
  (make-message id
                #f
                'assistant
                'message
                (list (make-text-part text))
                (current-seconds)
                (hasheq 'gsd-pin #t)))

(test-case "GSD progress messages pinned in Tier A"
  (define msgs
    (append (for/list ([i (in-range 30)])
              (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i)))
            (list (make-gsd-msg "gsd1" "Wave 1 complete"))
            (for/list ([i (in-range 30 60)])
              (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i)))))
  (define tiered (build-tiered-context msgs #:tier-c-count 4))
  (define tier-a (tiered-context-tier-a tiered))
  (define gsd-in-a (filter (lambda (m) (hash-ref (message-meta m) 'gsd-pin #f)) tier-a))
  (check-equal? (length gsd-in-a) 1 "GSD progress message in Tier A"))

(test-case "Non-GSD messages not pinned in Tier A"
  (define msgs
    (for/list ([i (in-range 30)])
      (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i))))
  (define tiered (build-tiered-context msgs #:tier-c-count 4))
  (define tier-a (tiered-context-tier-a tiered))
  (define any-pinned (filter (lambda (m) (hash-ref (message-meta m) 'gsd-pin #f)) tier-a))
  (check-equal? (length any-pinned) 0 "no GSD-pinned messages in Tier A"))

;; ============================================================
;; v0.28.22 W2: GSD progress auto-detection (no gsd-pin meta needed)
;; ============================================================

(define (make-assistant-msg id text)
  (make-message id #f 'assistant 'message (list (make-text-part text)) (current-seconds) (hasheq)))

(test-case "T7: GSD progress auto-detected by text pattern in Tier A"
  (define msgs
    (append (for/list ([i (in-range 30)])
              (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i)))
            (list (make-assistant-msg "gsd-auto1"
                                      "Wave 2 marked complete. PLAN.md and STATE.md updated."))
            (for/list ([i (in-range 30 60)])
              (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i)))))
  (define tiered (build-tiered-context msgs #:tier-c-count 4))
  (define tier-a (tiered-context-tier-a tiered))
  (define gsd-text
    (filter (lambda (m)
              (string-contains? (string-join (map (lambda (p)
                                                    (if (text-part? p)
                                                        (text-part-text p)
                                                        ""))
                                                  (message-content m)))
                                "Wave 2 marked"))
            tier-a))
  (check-equal? (length gsd-text) 1 "GSD progress message auto-detected in Tier A"))

(test-case "T8: make-text-part creates valid text-part"
  (define tp (make-text-part "test content"))
  (check-true (text-part? tp))
  (check-equal? (text-part-text tp) "test content"))

;; ============================================================
;; v0.28.23 W1: GSD role guard tests (user messages NOT pinned)
;; ============================================================

(test-case "T9: user message with wave-done text NOT pinned to Tier A"
  ;; Build 100 messages so Tier A is selective (only recent messages)
  (define base-msgs
    (for/list ([i (in-range 100)])
      (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i))))
  ;; Add user message with GSD-like text at position 50 (middle)
  (define user-gsd
    (make-message "ug"
                  #f
                  'user
                  'message
                  (list (make-text-part "I want to run wave-done 0"))
                  (current-seconds)
                  (hasheq)))
  (define msgs-with-user (append (take base-msgs 50) (list user-gsd) (drop base-msgs 50)))
  (define tiered (build-tiered-context msgs-with-user #:tier-c-count 4))
  (define tier-a (tiered-context-tier-a tiered))
  ;; User message at position 50 should NOT appear in Tier A
  ;; (too old for recency, NOT GSD-pinned because role=user)
  (define user-in-a?
    (for/or ([m (in-list tier-a)])
      (and (eq? (message-role m) 'user)
           (string-contains? (string-join (map (lambda (p)
                                                 (if (text-part? p)
                                                     (text-part-text p)
                                                     ""))
                                               (message-content m)))
                             "wave-done"))))
  (check-false user-in-a? "user message with wave-done NOT pinned to Tier A"))

(test-case "T10: tool message with Wave N marked complete IS pinned to Tier A"
  ;; Same setup but with tool message containing GSD text
  (define base-msgs
    (for/list ([i (in-range 100)])
      (make-test-msg (format "m~a" i) 'user 'message (format "Msg ~a" i))))
  (define tool-gsd
    (make-message "tg"
                  #f
                  'tool
                  'message
                  (list (make-text-part "Wave 3 marked complete. PLAN.md updated."))
                  (current-seconds)
                  (hasheq)))
  (define msgs-with-tool (append (take base-msgs 50) (list tool-gsd) (drop base-msgs 50)))
  (define tiered (build-tiered-context msgs-with-tool #:tier-c-count 4))
  (define tier-a (tiered-context-tier-a tiered))
  ;; Tool message at position 50 SHOULD appear in Tier A (GSD-pinned)
  (define tool-in-a?
    (for/or ([m (in-list tier-a)])
      (and (eq? (message-role m) 'tool)
           (string-contains? (string-join (map (lambda (p)
                                                 (if (text-part? p)
                                                     (text-part-text p)
                                                     ""))
                                               (message-content m)))
                             "Wave 3 marked complete"))))
  (check-true tool-in-a? "tool message with GSD text IS pinned to Tier A"))

;; ============================================================
;; I-04: Direct test for assemble-context/pure
;; ============================================================

(require (only-in "../runtime/turn-orchestrator.rkt" (assemble-context/pure ctx-assemble-pure))
         (only-in "../runtime/session-config.rkt" hash->session-config))

(define ctx-assemble ctx-assemble-pure)

(test-case "I-04: ctx-assemble returns message list without side effects"
  (define msgs
    (list (make-test-msg "u1" 'user 'message "Hello")
          (make-test-msg "a1" 'assistant 'message "Hi there")
          (make-test-msg "u2" 'user 'message "How are you?")))
  (define config (hash->session-config (hash 'tier-b-count 10 'tier-c-count 2 'max-tokens 4096)))
  (define-values (result _hook-res _tc) (ctx-assemble msgs config))
  (check-pred list? result)
  (check >= (length result) 1)
  ;; Verify all results are messages
  (for ([m (in-list result)])
    (check-true (message? m))))

;; v0.45.7 (NF3): OBS metrics test — verify tiered-context carries real data
(test-case "NF3: assemble-context/pure returns tiered-context with real structure"
  (define msgs
    (list (make-test-msg "u1" 'user 'message "Hello")
          (make-test-msg "a1" 'assistant 'message "Response")
          (make-test-msg "u2" 'user 'message "Follow up")))
  (define config (hash->session-config (hash 'tier-b-count 10 'tier-c-count 2 'max-tokens 4096)))
  (define-values (result _hook-res tc) (ctx-assemble msgs config))
  (check-pred tiered-context? tc)
  ;; Total across tiers should equal assembled count
  (define tier-total
    (+ (length (tiered-context-tier-a tc))
       (length (tiered-context-tier-b tc))
       (length (tiered-context-tier-c tc))))
  (check-equal? tier-total (length result) "tier total should match assembled count"))

;; v0.45.7 (NF4/S10): Regression test for first-turn context quality via tiered path
;; Simulates session-lifecycle migration: tree walk → tiered assembly → message list
(test-case "NF4: session-lifecycle tiered path preserves first-user and summaries"
  ;; Create messages including a compaction summary and first user message
  (define msgs
    (list (make-test-msg "sys" 'system 'system-instruction "You are helpful.")
          (make-test-msg "u1" 'user 'message "First question")
          (make-test-msg "a1" 'assistant 'message "First answer")
          (make-test-msg "c1" 'assistant 'compaction-summary "Summary of prior context")
          (make-test-msg "u2" 'user 'message "Second question")
          (make-test-msg "a2" 'assistant 'message "Second answer")))
  ;; Run tiered assembly (mimics session-lifecycle migration path)
  (define-values (tc _hook-res) (build-tiered-context-with-hooks msgs #:max-tokens 8192))
  (define result (tiered-context->message-list tc))
  (check > (length result) 0 "tiered assembly should return messages")
  ;; Verify all results are valid messages
  (for ([m (in-list result)])
    (check-true (message? m)))
  ;; Verify compaction summary survives
  (define summary-msgs (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) result))
  (check > (length summary-msgs) 0 "compaction summary should survive tiered assembly")
  ;; Verify first user message survives (Tier A pinned)
  (define user-msgs (filter (lambda (m) (eq? (message-role m) 'user)) result))
  (check > (length user-msgs) 0 "at least one user message should survive"))
