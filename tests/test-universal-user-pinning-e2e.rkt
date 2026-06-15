#lang racket/base

;; @speed fast
;; @suite default
;; BOUNDARY: integration

;; tests/test-universal-user-pinning-e2e.rkt
;; End-to-end regression test for universal user message pinning through
;; build-assembled-context/v2 and the session-index path walk.

(require rackunit
         racket/list
         (only-in "helpers/temp-fs.rkt" with-temp-dir)
         "../util/message/protocol-types.rkt"
         "../util/content/content-parts.rkt"
         "../runtime/session/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context/context-assembly.rkt")

(displayln "test-universal-user-pinning-e2e.rkt loaded")

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (index-path dir)
  (build-path dir "session.index"))

(define (make-text-msg id parent-id role kind text ts)
  (make-message id parent-id role kind (list (make-text-part text)) ts (hasheq)))

(define (message-id-member? id msgs)
  (not (not (member id (map message-id msgs)))))

(test-case "build-assembled-context/v2 preserves every user prompt under tight budget"
  (with-temp-dir (dir)
                 (define sp (session-path dir))
                 (define ip (index-path dir))
                 ;; Linear multi-prompt session with large assistant/tool entries between prompts.
                 ;; The final small budget should force removable assistant/tool entries out, but
                 ;; universal pinning must preserve p1, p2, and p3.
                 (define entries
                   (list (make-text-msg "p1" #f 'user 'message "Re-build and restart server" 1000)
                         (make-text-msg "a1" "p1" 'assistant 'message (make-string 5000 #\a) 1001)
                         (make-text-msg "t1" "a1" 'tool 'tool-result (make-string 3000 #\b) 1002)
                         (make-text-msg "p2" "t1" 'user 'message "Create a list of pages" 1003)
                         (make-text-msg "a2" "p2" 'assistant 'message (make-string 5000 #\c) 1004)
                         (make-text-msg "t2" "a2" 'tool 'tool-result (make-string 3000 #\d) 1005)
                         (make-text-msg "p3" "t2" 'user 'message "Copy contents from live URLs" 1006)
                         (make-text-msg "a3" "p3" 'assistant 'message (make-string 5000 #\e) 1007)
                         (make-text-msg "tf" "a3" 'tool 'tool-result (make-string 3000 #\f) 1008)))
                 (append-entries! sp entries)
                 (define idx (build-index! sp ip))
                 (define config
                   (make-context-assembly-config #:recent-tokens 100
                                                 #:max-catalog-entries 0
                                                 #:max-catalog-tokens 0))
                 (define result
                   (build-assembled-context/v2 idx config (make-context-assembly-call-options)))
                 (define result-messages (context-result-messages result))
                 (for ([pid (in-list '("p1" "p2" "p3"))])
                   (check-true (message-id-member? pid result-messages)
                               (format "prompt ~a must survive end-to-end context assembly" pid)))
                 (check-true (> (context-result-excluded-count result) 0)
                             "tight budget should exclude at least one non-user message")))
