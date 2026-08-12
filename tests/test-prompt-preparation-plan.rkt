#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/test-prompt-preparation-plan.rkt
;; v0.99.92 W1 — Pure prompt preparation extraction (MA-10 trace equivalence).
;;
;; RED-FIRST: imports runtime/session/session-prompt-preparation.rkt, which must
;; not exist on the v0.99.92 W0 baseline. This file fails to compile until the
;; pure plan module lands. The plan must compute values only — never touch a
;; session, perform I/O, or mutate an index.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/session/session-prompt-preparation.rkt"
                  build-prompt-preparation-plan
                  prompt-preparation-plan?
                  prompt-preparation-plan-canonical-user-message
                  prompt-preparation-plan-post-append-index
                  prompt-preparation-plan-appended-entry
                  prompt-preparation-plan-parent-id
                  prompt-preparation-plan-context-messages
                  prompt-preparation-plan-model-name
                  prompt-preparation-plan-context-with-system)
         (only-in "../runtime/session-index/schema.rkt"
                  session-index
                  session-index?
                  session-index-active-leaf-id
                  session-index-by-id
                  session-index-children
                  session-index-entry-order
                  session-index-bookmarks
                  session-index-bookmark-sem
                  make-empty-index)
         (only-in "../util/message/message.rkt"
                  message?
                  message-id
                  message-role
                  message-kind
                  message-parent-id
                  message-content
                  make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../runtime/working-set.rkt" make-working-set working-set-entries)
         racket/string
         racket/list)

(define (make-test-message id parent-id role kind)
  (make-message id parent-id role kind (list (make-text-part "test")) 0 (hasheq)))

(define (make-test-index msgs active-id)
  (define by-id
    (for/hash ([m (in-list msgs)])
      (values (message-id m) m)))
  (define children
    (for/fold ([ch (for/hash ([m (in-list msgs)])
                     (values (message-id m) '()))])
              ([m (in-list msgs)])
      (define pid (message-parent-id m))
      (if (and pid (hash-has-key? by-id pid))
          (hash-update ch pid (lambda (lst) (append lst (list m))) '())
          ch)))
  (session-index by-id children (list->vector msgs) (hash) (box active-id) (make-semaphore 1)))

(define-test-suite
 prompt-preparation-plan-tests
 ;; ---- Plan shape / purity ----
 (test-case "plan returns a record with all preparation values"
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-true (prompt-preparation-plan? plan))
   (check-not-false (prompt-preparation-plan-canonical-user-message plan))
   (check-false (prompt-preparation-plan-post-append-index plan))
   (check-false (prompt-preparation-plan-appended-entry plan))
   (check-false (prompt-preparation-plan-parent-id plan))
   (check-true (list? (prompt-preparation-plan-context-messages plan)))
   (check-true (list? (prompt-preparation-plan-context-with-system plan))))
 (test-case "plan performs no index mutation (empty index stays empty)"
   (define idx (make-empty-index))
   (define before-by-id (session-index-by-id idx))
   (define before-order (session-index-entry-order idx))
   (define before-active (unbox (session-index-active-leaf-id idx)))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-not-false (prompt-preparation-plan-post-append-index plan)
                    "pure append still produces a post-append index")
   (check-equal? (session-index-by-id idx) before-by-id "index by-id unchanged")
   (check-equal? (session-index-entry-order idx) before-order "index entry order unchanged")
   (check-equal? (unbox (session-index-active-leaf-id idx))
                 before-active
                 "active leaf box unchanged by pure plan"))
 ;; ---- Parent selection: entries only (no index) ----
 (test-case "parent-id from last non-session-info history entry"
   (define hist
     (list (make-test-message "a" #f 'user 'message) (make-test-message "b" "a" 'assistant 'message)))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history hist
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (prompt-preparation-plan-parent-id plan) "b"))
 (test-case "parent-id #f when history empty"
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (prompt-preparation-plan-parent-id plan) #f))
 (test-case "trailing session-info entries are ignored for parent selection"
   (define hist
     (list (make-test-message "a" #f 'user 'message)
           (make-test-message "s1" "a" 'system 'session-info)))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history hist
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (prompt-preparation-plan-parent-id plan) "a"))
 ;; ---- Parent selection: with index (active leaf) ----
 (test-case "parent-id from active leaf when index present"
   (define idx
     (make-test-index (list (make-test-message "m1" #f 'user 'message)
                            (make-test-message "m2" "m1" 'assistant 'message))
                      "m2"))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (prompt-preparation-plan-parent-id plan) "m2"))
 (test-case "parent-id #f when active leaf is session-info"
   (define idx (make-test-index (list (make-test-message "s1" #f 'system 'session-info)) "s1"))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (prompt-preparation-plan-parent-id plan) #f))
 ;; ---- Message canonicalization ----
 (test-case "canonical user message is a user/kind=message with the exact text"
   (define plan
     (build-prompt-preparation-plan "hello world"
                                    #:history '()
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (define msg (prompt-preparation-plan-canonical-user-message plan))
   (check-true (message? msg))
   (check-eq? (message-role msg) 'user)
   (check-eq? (message-kind msg) 'message))
 (test-case "append-to-leaf/pure canonicalizes parent for message-struct input"
   ;; When an index is present, the plan's pure append fixes a missing parent.
   (define idx (make-test-index (list (make-test-message "leaf" #f 'user 'message)) "leaf"))
   (define input (make-test-message "new" #f 'user 'message))
   (define plan
     (build-prompt-preparation-plan input
                                    #:history '()
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (define appended (prompt-preparation-plan-appended-entry plan))
   (check-not-false appended)
   (check-equal? (message-parent-id appended) "leaf"))
 (test-case "post-append-index reflects the appended entry when index present"
   (define idx (make-test-index (list (make-test-message "leaf" #f 'user 'message)) "leaf"))
   (define plan
     (build-prompt-preparation-plan "hi"
                                    #:history '()
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (define post (prompt-preparation-plan-post-append-index plan))
   (check-not-false post)
   (check-true (session-index? post)))
 ;; ---- Path settings ----
 (test-case "path-derived model setting is surfaced on the plan"
   ;; Path-model changes are stripped by the indexed tree/tiered assembly, so
   ;; the effective setting surfaces through the linear (no-index) history,
   ;; exactly as in the historical caller.
   (define path-msg
     (make-message "p1"
                   #f
                   'system
                   'model-change
                   (list (make-text-part "model"))
                   0
                   (hasheq 'model "gpt-test")))
   (define plan
     (build-prompt-preparation-plan "hi"
                                    #:history (list path-msg)
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (prompt-preparation-plan-model-name plan) "gpt-test"))
 (test-case "no model change leaves model-name #f"
   (define plan
     (build-prompt-preparation-plan "hi"
                                    #:history '()
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (prompt-preparation-plan-model-name plan) #f))
 ;; ---- System injection ----
 (test-case "system instructions are prepended"
   (define plan
     (build-prompt-preparation-plan "hi"
                                    #:history '()
                                    #:index #f
                                    #:system-instructions '("be terse")
                                    #:provider? #f
                                    #:working-set #f))
   (define ctx (prompt-preparation-plan-context-with-system plan))
   (check-not-false (pair? ctx))
   (check-eq? (message-role (car ctx)) 'system))
 (test-case "context-with-system without instructions still guarantees leading system"
   (define plan
     (build-prompt-preparation-plan "hi"
                                    #:history '()
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (define ctx (prompt-preparation-plan-context-with-system plan))
   (check-eq? (message-role (car ctx)) 'system))
 ;; ---- Context-source branch ----
 (test-case "no index: linear context includes history then user message"
   (define hist (list (make-test-message "a" #f 'user 'message)))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history hist
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (define ctx (prompt-preparation-plan-context-messages plan))
   (check-equal? (length ctx) 2)
   (check-equal? (message-id (car ctx)) "a")
   (check-equal? (message-id (last ctx))
                 (message-id (prompt-preparation-plan-canonical-user-message plan))))
 (test-case "no index, empty history: context is just the user message"
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index #f
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (length (prompt-preparation-plan-context-messages plan)) 1))
 (test-case "index without provider: tree-walk context starts from index entries"
   (define idx
     (make-test-index (list (make-test-message "a" #f 'user 'message)
                            (make-test-message "b" "a" 'assistant 'message))
                      "b"))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-not-false (pair? (prompt-preparation-plan-context-messages plan))))
 (test-case "index with provider: tiered context path produces messages"
   (define idx
     (make-test-index (list (make-test-message "a" #f 'user 'message)
                            (make-test-message "b" "a" 'assistant 'message))
                      "b"))
   (define ws (make-working-set))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history '()
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #t
                                    #:working-set ws))
   (check-not-false (pair? (prompt-preparation-plan-context-messages plan))))
 (test-case "history and context lists are not mutated by the plan"
   (define hist (list (make-test-message "a" #f 'user 'message)))
   (define hist-ids (map message-id hist))
   (define idx
     (make-test-index (list (make-test-message "a" #f 'user 'message)
                            (make-test-message "b" "a" 'assistant 'message))
                      "b"))
   (define order-before (vector->list (session-index-entry-order idx)))
   (define plan
     (build-prompt-preparation-plan "hello"
                                    #:history hist
                                    #:index idx
                                    #:system-instructions '()
                                    #:provider? #f
                                    #:working-set #f))
   (check-equal? (map message-id hist) hist-ids "history list unchanged")
   (check-equal? (vector->list (session-index-entry-order idx))
                 order-before
                 "index entry order unchanged")))

(module+ main
  (run-tests prompt-preparation-plan-tests))
