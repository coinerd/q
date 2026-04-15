#lang racket

;; tests/test-context-builder.rkt — Tests for runtime/context-builder.rkt (#498)

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-builder.rkt")

;; Helpers
(define (make-temp-dir)
  (make-temporary-file "q-ctx-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (index-path dir)
  (build-path dir "session.index"))

(define (make-timestamped-message id parent-id role kind ts)
  (make-message id parent-id role kind
                (list (make-text-part (format "~a-entry" id)))
                ts (hasheq)))

(define context-builder-tests
  (test-suite
   "context-builder"

   ;; Build index from a simple linear session and extract context
   (test-case "build-session-context: linear path"
     (define dir (make-temp-dir))
     (define sp (session-path dir))
     (define ip (index-path dir))
     (define entries
       (list (make-timestamped-message "root" #f 'user 'message 1000)
             (make-timestamped-message "c1" "root" 'assistant 'message 1001)
             (make-timestamped-message "c2" "c1" 'user 'message 1002)))
     (append-entries! sp entries)
     (define idx (build-index! sp ip))
     (define ctx (build-session-context idx))
     ;; All 3 entries should be in context (message entries pass through)
     (check-equal? (length ctx) 3)
     (check-equal? (message-id (first ctx)) "root")
     (check-equal? (message-id (last ctx)) "c2")
     (delete-directory/files dir #:must-exist? #f))

   ;; Compaction summary truncates earlier messages
   (test-case "build-session-context: compaction truncates early entries"
     (define dir (make-temp-dir))
     (define sp (session-path dir))
     (define ip (index-path dir))
     (define entries
       (list (make-timestamped-message "root" #f 'user 'message 1000)
             (make-timestamped-message "c1" "root" 'assistant 'message 1001)
             (make-message "compact1" #f 'system 'compaction-summary
                           (list (make-text-part "summary"))
                           1002 (hasheq))
             (make-timestamped-message "c2" "compact1" 'user 'message 1003)))
     (append-entries! sp entries)
     (define idx (build-index! sp ip))
     (define ctx (build-session-context idx))
     ;; Compaction summary + c2 (root and c1 are before compaction)
     (check-equal? (length ctx) 2)
     ;; Compaction summary becomes user-role
     (check-equal? (message-role (first ctx)) 'user)
     (check-equal? (message-id (last ctx)) "c2")
     (delete-directory/files dir #:must-exist? #f))

   ;; Session-info entries are filtered out
   (test-case "build-session-context: filters session-info entries"
     (define dir (make-temp-dir))
     (define sp (session-path dir))
     (define ip (index-path dir))
     (define entries
       (list (make-message "info1" #f 'system 'session-info '() 1000
                           (hasheq 'version 2))
             (make-timestamped-message "root" "info1" 'user 'message 1001)))
     (append-entries! sp entries)
     (define idx (build-index! sp ip))
     (define ctx (build-session-context idx))
     (check-equal? (length ctx) 1)
     (check-equal? (message-id (first ctx)) "root")
     (delete-directory/files dir #:must-exist? #f))

   ;; Empty session returns empty context
   (test-case "build-session-context: empty session"
     (define dir (make-temp-dir))
     (define sp (session-path dir))
     (define ip (index-path dir))
     ;; Don't write any entries
     (define idx (build-index! sp ip))
     (define ctx (build-session-context idx))
     (check-equal? ctx '())
     (delete-directory/files dir #:must-exist? #f))

   ;; Branched session with branch! switch
   (test-case "build-session-context: branched path"
     (define dir (make-temp-dir))
     (define sp (session-path dir))
     (define ip (index-path dir))
     (define entries
       (list (make-timestamped-message "root" #f 'user 'message 1000)
             (make-timestamped-message "c1" "root" 'assistant 'message 1001)
             (make-timestamped-message "c2" "root" 'assistant 'message 1002)
             (make-timestamped-message "c1a" "c1" 'user 'message 1003)))
     (append-entries! sp entries)
     (define idx (build-index! sp ip))
     ;; Switch to c1a branch (root -> c1 -> c1a)
     (branch! idx "c1a")
     (define ctx (build-session-context idx))
     (check-equal? (length ctx) 3)
     (check-equal? (message-id (first ctx)) "root")
     (check-equal? (message-id (last ctx)) "c1a")
     (delete-directory/files dir #:must-exist? #f))
   ))

(run-tests context-builder-tests)

;; ============================================================
;; Token-aware context assembly tests (#646, #647)
;; ============================================================

(define token-aware-tests
  (test-suite
   "Token-aware context assembly"

   (test-case "estimate-message-tokens: empty message"
     (define msg (make-message "m1" #f 'user 'message (list) 1000 (hash)))
     (check-equal? (estimate-message-tokens msg) 0))

   (test-case "estimate-message-tokens: text message"
     (define msg (make-message "m1" #f 'user 'message
                               (list (make-text-part "Hello world")) 1000 (hash)))
     (check-true (>= (estimate-message-tokens msg) 1)))

   (test-case "estimate-message-tokens: long message"
     (define long-text (make-string 1000 #\a))
     (define msg (make-message "m1" #f 'user 'message
                               (list (make-text-part long-text)) 1000 (hash)))
     (define tokens (estimate-message-tokens msg))
     (check-true (> tokens 0)))

   (test-case "truncate-messages-to-budget: messages within budget are unchanged"
     (define msgs
       (for/list ([i (in-range 3)])
         (make-message (format "m~a" i) #f 'user 'message
                       (list (make-text-part "short")) 1000 (hash))))
     (define result (truncate-messages-to-budget msgs 10000))
     (check-equal? (length result) 3))

   (test-case "truncate-messages-to-budget: truncates oldest when over budget"
     (define msgs
       (for/list ([i (in-range 10)])
         (make-message (format "m~a" i) #f 'user 'message
                       (list (make-text-part (make-string 100 #\x))) 1000 (hash))))
     ;; Each message ~25 tokens. 10 messages = ~250 tokens. Budget 100 → ~4 fit.
     (define result (truncate-messages-to-budget msgs 100))
     (check-true (< (length result) 10))
     (check-true (>= (length result) 1))
     ;; Should keep the most recent messages (highest index)
     (check-equal? (message-id (last result)) "m9"))

   (test-case "truncate-messages-to-budget: preserves system instructions"
     (define sys-msg
       (make-message "sys" #f 'system 'system-instruction
                     (list (make-text-part "You are helpful.")) 1000 (hash)))
     (define user-msgs
       (for/list ([i (in-range 10)])
         (make-message (format "m~a" i) #f 'user 'message
                       (list (make-text-part (make-string 100 #\x))) 1000 (hash))))
     (define msgs (cons sys-msg user-msgs))
     ;; Tiny budget — system instruction should survive
     (define result (truncate-messages-to-budget msgs 10))
     (define sys-remaining (filter (lambda (m) (eq? (message-kind m) 'system-instruction)) result))
     (check-equal? (length sys-remaining) 1)
     (check-equal? (message-id (car sys-remaining)) "sys"))

   (test-case "truncate-messages-to-budget: preserves compaction summaries"
     (define summary-msg
       (make-message "summary1" #f 'system 'compaction-summary
                     (list (make-text-part "Summary of old messages")) 1000 (hash)))
     (define user-msgs
       (for/list ([i (in-range 10)])
         (make-message (format "m~a" i) #f 'user 'message
                       (list (make-text-part (make-string 100 #\x))) 1000 (hash))))
     (define msgs (cons summary-msg user-msgs))
     (define result (truncate-messages-to-budget msgs 10))
     (define summaries (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) result))
     (check-equal? (length summaries) 1))

   (test-case "truncate-messages-to-budget: empty list returns empty"
     (check-equal? (truncate-messages-to-budget '() 100) '()))

   (test-case "build-session-context/tokens: returns values"
     (define dir (make-temp-dir))
     (define sp (session-path dir))
     (define ip (build-path dir "index.jsonl"))
     (define entries
       (list (make-timestamped-message "root" #f 'user 'message 1000)
             (make-timestamped-message "c1" "root" 'assistant 'message 1001)))
     (append-entries! sp entries)
     (define idx (build-index! sp ip))
     (define-values (msgs tokens) (build-session-context/tokens idx #:max-tokens 10000))
     (check-true (>= (length msgs) 1))
     (check-true (>= tokens 0))
     (delete-directory/files dir #:must-exist? #f))
   ))

(run-tests token-aware-tests)
