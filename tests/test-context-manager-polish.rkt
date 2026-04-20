#lang racket

;; tests/test-context-manager-polish.rkt — Wave 4 (#1394): Polish & Edge Cases
;;
;; Tests for catalog token caps, consecutive tool result collapsing,
;; budget enforcement drop priority, and pin guarantees.

(require rackunit
         rackunit/text-ui
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-manager.rkt"
         "../llm/token-budget.rkt")

;; ── Helpers ──────────────────────────────────────────────────

(define (msg-text m)
  (string-join (for/list ([part (in-list (message-content m))]
                          #:when (text-part? part))
                 (text-part-text part))
               " "))

(define (build-session old-count
                       #:old-prefix [old-prefix "old"]
                       #:old-content
                       [content-fn (lambda (i) (format "Content ~a with text to pad" i))])
  (define sys
    (make-message "sys"
                  #f
                  'system
                  'system-instruction
                  (list (make-text-part "System prompt"))
                  (current-seconds)
                  (hasheq)))
  (define u1
    (make-message "u1"
                  "sys"
                  'user
                  'message
                  (list (make-text-part "First user message"))
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
                    (list (make-text-part (content-fn i)))
                    (current-seconds)
                    (hasheq))))
  (define last-id
    (if (> old-count 0)
        (format "~a~a" old-prefix (sub1 old-count))
        "u1"))
  (define u2
    (make-message "u2"
                  last-id
                  'user
                  'message
                  (list (make-text-part "Recent question"))
                  (current-seconds)
                  (hasheq)))
  (define a2
    (make-message "a2"
                  "u2"
                  'assistant
                  'message
                  (list (make-text-part "Recent answer"))
                  (current-seconds)
                  (hasheq)))
  (append (list sys u1) old-msgs (list u2 a2)))

(define (with-index msgs thunk)
  (define dir (make-temporary-file "q-test-~a" 'directory))
  (define sp (build-path dir "session.jsonl"))
  (define ip (build-path dir "session.index"))
  (append-entries! sp msgs)
  (define idx (build-index! sp ip))
  (define result (thunk idx))
  (delete-directory/files dir #:must-exist? #f)
  result)

(define polish-tests
  (test-suite "context-manager-polish"

    ;; ── Catalog token cap ─────────────────────────────────────
    (test-case "generate-catalog respects max-catalog-tokens"
      ;; Create messages with long content → catalog entries would exceed token cap
      (define msgs
        (build-session 50
                       #:old-content
                       (lambda (i) (format "Message ~a with lots of text padding content here" i))))
      (define catalog
        (with-index msgs
                    (lambda (idx)
                      ;; Use very small budget → lots of excluded entries
                      (define cfg
                        (make-context-manager-config #:recent-tokens 30
                                                     #:max-catalog-entries 50
                                                     #:max-catalog-tokens 100))
                      (define-values (_result catalog) (assemble-context idx cfg))
                      catalog)))
      ;; Total catalog tokens should be under cap
      (define catalog-tokens
        (for/sum ([e (in-list catalog)]) (estimate-text-tokens (catalog-entry-summary e))))
      (check-true (<= catalog-tokens 100)
                  (format "Catalog tokens ~a exceeds cap 100" catalog-tokens)))

    ;; ── Pin guarantee under extreme budget ────────────────────
    (test-case "pinned items survive extreme budget pressure"
      (define msgs (build-session 20))
      (with-index msgs
                  (lambda (idx)
                    ;; Tiny budget: only 10 tokens — way less than pinned messages
                    (define cfg (make-context-manager-config #:recent-tokens 10))
                    (define-values (result catalog) (assemble-context idx cfg))
                    ;; System prompt must be present
                    (define sys-msgs
                      (filter (lambda (m) (eq? (message-kind m) 'system-instruction)) result))
                    (check-equal? (length sys-msgs) 1 "System prompt must survive")
                    ;; First user message must be present
                    (define user-msgs (filter (lambda (m) (eq? (message-role m) 'user)) result))
                    (check-true (>= (length user-msgs) 1) "First user message must survive")
                    (void))))

    ;; ── Consecutive tool messages collapsed in catalog ───────
    (test-case "catalog collapses consecutive tool messages"
      (define sys
        (make-message "sys"
                      #f
                      'system
                      'system-instruction
                      (list (make-text-part "System"))
                      (current-seconds)
                      (hasheq)))
      (define u1
        (make-message "u1"
                      "sys"
                      'user
                      'message
                      (list (make-text-part "Start"))
                      (current-seconds)
                      (hasheq)))
      ;; Create consecutive tool messages
      (define tool-msgs
        (for/list ([i (in-range 6)])
          (make-message
           (format "tool~a" i)
           (if (= i 0)
               "u1"
               (format "tool~a" (sub1 i)))
           'tool
           'tool-result
           (list (make-tool-result-part (format "tc~a" i) (format "Tool output ~a" i) #f))
           (current-seconds)
           (hasheq))))
      (define u2
        (make-message "u2"
                      "tool5"
                      'user
                      'message
                      (list (make-text-part "Recent"))
                      (current-seconds)
                      (hasheq)))
      (define a2
        (make-message "a2"
                      "u2"
                      'assistant
                      'message
                      (list (make-text-part "Reply"))
                      (current-seconds)
                      (hasheq)))
      (define msgs (append (list sys u1) tool-msgs (list u2 a2)))
      (with-index msgs
                  (lambda (idx)
                    (define cfg (make-context-manager-config #:recent-tokens 30))
                    (define-values (result catalog) (assemble-context idx cfg))
                    ;; Consecutive tool results should be collapsed to fewer catalog entries
                    (define tool-entries
                      (filter (lambda (e) (equal? (catalog-entry-role e) "tool")) catalog))
                    (check-true (< (length tool-entries) (length tool-msgs))
                                (format "Expected collapsed tool entries, got ~a for ~a tools"
                                        (length tool-entries)
                                        (length tool-msgs)))
                    (void))))

    ;; ── Empty session edge case ──────────────────────────────
    (test-case "empty session returns empty"
      (define dir (make-temporary-file "q-test-~a" 'directory))
      (define sp (build-path dir "session.jsonl"))
      (define ip (build-path dir "session.index"))
      ;; Write empty session
      (call-with-output-file sp (lambda (p) (void)) #:exists 'replace)
      (define idx (build-index! sp ip))
      (define cfg (make-context-manager-config))
      (define-values (result catalog) (assemble-context idx cfg))
      (check-equal? result '())
      (check-equal? catalog '())
      (delete-directory/files dir #:must-exist? #f))

    ;; ── Config defaults are reasonable ───────────────────────
    (test-case "config defaults are production-ready"
      (define cfg (make-context-manager-config))
      (check-equal? (context-manager-config-recent-tokens cfg) 30000)
      (check-equal? (context-manager-config-max-catalog-entries cfg) 40)
      (check-equal? (context-manager-config-max-catalog-tokens cfg) 2000)
      (check-equal? (context-manager-config-summary-window cfg) 4000))))

(run-tests polish-tests)
