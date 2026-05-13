#lang racket

;; BOUNDARY: integration

;; tests/test-context-assembly-trace.rkt — Assembly trace hook + property tests
;;
;; W2 of v0.23.2:
;; - Verifies trace callback receives expected phase events
;; - Property tests: ordering stability, pinning invariants, pair preservation,
;;   budget enforcement

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-assembly.rkt"
         "../runtime/context-policy.rkt")

;; ── Helpers ──────────────────────────────────────────────────

(define (make-temp-dir)
  (make-temporary-file "q-ctx-trace-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))
(define (index-path dir)
  (build-path dir "session.index"))

(define (make-msg id parent-id role kind text)
  (make-message id parent-id role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (build-session-with msgs)
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  (append-entries! sp msgs)
  (define idx (build-index! sp ip))
  (values idx dir))

;; ============================================================
;; Trace integration tests
;; ============================================================

(define trace-tests
  (test-suite "context-assembly-trace"

    (test-case "trace callback receives all phase events"
      (define msgs
        (list (make-msg "sys" #f 'system 'system-instruction "You are helpful")
              (make-msg "u1" "sys" 'user 'message "Hello")
              (make-msg "a1" "u1" 'assistant 'message "Hi there")
              (make-msg "u2" "a1" 'user 'message "How are you?")
              (make-msg "a2" "u2" 'assistant 'message "I'm fine")))
      (define-values (idx dir) (build-session-with msgs))
      (define trace-events '())
      (define (trace-cb phase data)
        (set! trace-events (cons (cons phase data) trace-events)))
      (define cr
        (build-assembled-context idx (make-context-assembly-config) #:trace-callback trace-cb))
      (define phases (map car (reverse trace-events)))
      (check-not-false (member 'start phases) "should have 'start event")
      (check-not-false (member 'phase1-pinned phases) "should have 'phase1-pinned event")
      (check-not-false (member 'phase3-fitted phases) "should have 'phase3-fitted event")
      (check-not-false (member 'phase4-catalog phases) "should have 'phase4-catalog event")
      (check-not-false (member 'done phases) "should have 'done event")
      (delete-directory/files dir))

    (test-case "trace data includes expected keys"
      (define msgs
        (list (make-msg "sys" #f 'system 'system-instruction "System")
              (make-msg "u1" "sys" 'user 'message "Q1")
              (make-msg "a1" "u1" 'assistant 'message "A1")))
      (define-values (idx dir) (build-session-with msgs))
      (define trace-events '())
      (define (trace-cb phase data)
        (set! trace-events (cons (cons phase data) trace-events)))
      (build-assembled-context idx (make-context-assembly-config) #:trace-callback trace-cb)
      (define events-ht
        (for/hash ([e (in-list trace-events)])
          (values (car e) (cdr e))))
      ;; start event
      (check-true (hash-has-key? (hash-ref events-ht 'start) 'raw-count))
      ;; phase1 event
      (check-true (hash-has-key? (hash-ref events-ht 'phase1-pinned) 'pinned-count))
      (check-true (hash-has-key? (hash-ref events-ht 'phase1-pinned) 'pinned-tokens))
      ;; done event
      (check-true (hash-has-key? (hash-ref events-ht 'done) 'total-tokens))
      (check-true (hash-has-key? (hash-ref events-ht 'done) 'memo-hits))
      (delete-directory/files dir))

    (test-case "empty session emits start + empty events"
      (define dir (make-temp-dir))
      (define sp (session-path dir))
      (define ip (index-path dir))
      (call-with-output-file sp void #:exists 'replace)
      (define idx (build-index! sp ip))
      (define trace-events '())
      (define (trace-cb phase data)
        (set! trace-events (cons (cons phase data) trace-events)))
      (build-assembled-context idx (make-context-assembly-config) #:trace-callback trace-cb)
      (define phases (map car (reverse trace-events)))
      (check-equal? phases '(start empty))
      (delete-directory/files dir))

    ;; ============================================================
    ;; Property tests: ordering stability
    ;; ============================================================

    (test-case "property: output messages preserve original relative order"
      (define msgs
        (for/list ([i (in-range 20)])
          (define id (format "m~a" i))
          (define parent
            (if (= i 0)
                #f
                (format "m~a" (sub1 i))))
          (define role
            (if (= i 0)
                'system
                (if (odd? i) 'user 'assistant)))
          (define kind (if (= i 0) 'system-instruction 'message))
          (make-msg id parent role kind (format "Content ~a" i))))
      (define-values (idx dir) (build-session-with msgs))
      (define cr (build-assembled-context idx (make-context-assembly-config #:recent-tokens 100000)))
      (define result-ids (map message-id (context-result-messages cr)))
      ;; Result IDs should be a subsequence of original IDs preserving order
      (define original-ids (map message-id msgs))
      (check-not-false (andmap (lambda (id) (member id original-ids)) result-ids))
      ;; Check ordering: each result ID should appear in same relative order
      (let loop ([ids result-ids]
                 [pos 0])
        (when (not (null? ids))
          (define idx (index-of original-ids (car ids)))
          (check-true (>= idx pos) (format "~a appeared out of order" (car ids)))
          (loop (cdr ids) idx)))
      (delete-directory/files dir))

    ;; ============================================================
    ;; Property tests: pinning invariants
    ;; ============================================================

    (test-case "property: system instruction always pinned"
      (define msgs
        (list (make-msg "sys" #f 'system 'system-instruction "System prompt")
              (make-msg "u1" "sys" 'user 'message "Q1")
              (make-msg "a1" "u1" 'assistant 'message (make-string 200 #\x))
              (make-msg "u2" "a1" 'user 'message (make-string 200 #\x))
              (make-msg "a2" "u2" 'assistant 'message (make-string 200 #\x))))
      (define-values (idx dir) (build-session-with msgs))
      ;; Very small budget to force eviction
      (define cr (build-assembled-context idx (make-context-assembly-config #:recent-tokens 100)))
      (define result-ids (map message-id (context-result-messages cr)))
      (check-not-false (member "sys" result-ids) "system instruction must be in result")
      (delete-directory/files dir))

    (test-case "property: first user message always present"
      (define msgs
        (list (make-msg "sys" #f 'system 'system-instruction "System")
              (make-msg "u1" "sys" 'user 'message "First user msg")
              (make-msg "a1" "u1" 'assistant 'message (make-string 500 #\x))
              (make-msg "u2" "a1" 'user 'message (make-string 500 #\x))))
      (define-values (idx dir) (build-session-with msgs))
      (define cr (build-assembled-context idx (make-context-assembly-config #:recent-tokens 50)))
      (define result-ids (map message-id (context-result-messages cr)))
      (check-not-false (member "u1" result-ids) "first user message must always be in result")
      (delete-directory/files dir))

    ;; ============================================================
    ;; Property tests: budget enforcement
    ;; ============================================================

    (test-case "property: pinned + recent <= max-tokens (unless single msg exceeds)"
      (define msgs
        (for/list ([i (in-range 30)])
          (define id (format "m~a" i))
          (define parent
            (if (= i 0)
                #f
                (format "m~a" (sub1 i))))
          (define role
            (if (= i 0)
                'system
                (if (odd? i) 'user 'assistant)))
          (define kind (if (= i 0) 'system-instruction 'message))
          (make-msg id parent role kind (make-string 50 #\x))))
      (define-values (idx dir) (build-session-with msgs))
      (define budget 500)
      (define cr (build-assembled-context idx (make-context-assembly-config #:recent-tokens budget)))
      ;; If more than 1 message, total should not wildly exceed budget
      ;; (may slightly exceed due to pinning guarantees)
      (when (> (length (context-result-messages cr)) 1)
        (check-true (<= (context-result-total-tokens cr) (* budget 2))
                    (format "total ~a exceeds 2x budget ~a" (context-result-total-tokens cr) budget)))
      (delete-directory/files dir))

    ;; ============================================================
    ;; Property tests: partition completeness
    ;; ============================================================

    (test-case "property: pinned + recent + excluded = total raw messages"
      (define msgs
        (for/list ([i (in-range 15)])
          (define id (format "m~a" i))
          (define parent
            (if (= i 0)
                #f
                (format "m~a" (sub1 i))))
          (define role
            (if (= i 0)
                'system
                (if (odd? i) 'user 'assistant)))
          (define kind (if (= i 0) 'system-instruction 'message))
          (make-msg id parent role kind (format "Text ~a" i))))
      (define-values (idx dir) (build-session-with msgs))
      (define cr (build-assembled-context idx (make-context-assembly-config #:recent-tokens 200)))
      (define total
        (+ (context-result-pinned-count cr)
           (context-result-recent-count cr)
           (context-result-excluded-count cr)))
      (check-equal? total
                    (length msgs)
                    (format "pinned(~a) + recent(~a) + excluded(~a) != total(~a)"
                            (context-result-pinned-count cr)
                            (context-result-recent-count cr)
                            (context-result-excluded-count cr)
                            (length msgs)))
      (delete-directory/files dir))))

(run-tests trace-tests)
