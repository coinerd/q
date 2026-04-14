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
