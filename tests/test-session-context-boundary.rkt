#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/test-session-context-boundary.rkt
;; v0.99.92 W2 — Explicit context-build request/result boundary.
;;
;; RED-FIRST: imports runtime/session/session-context-boundary.rkt, which must
;; not exist on the v0.99.92 W1 baseline. The boundary must make the Context
;; @boundary unit
;; Assembly request/result explicit, stay pure, and keep Context Assembly
;; Runtime-owned and state session-owned.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/session/session-context-boundary.rkt"
                  context-build
                  context-build-request
                  context-build-request?
                  context-build-request-user-message
                  context-build-request-history
                  context-build-request-index
                  context-build-request-system-instructions
                  context-build-request-provider?
                  context-build-request-working-set
                  context-build-request-max-tokens
                  context-build-result
                  context-build-result?
                  context-build-result-canonical-user-message
                  context-build-result-post-append-index
                  context-build-result-appended-entry
                  context-build-result-parent-id
                  context-build-result-context-messages
                  context-build-result-model-name
                  context-build-result-context-with-system)
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
         (only-in "../runtime/working-set.rkt" make-working-set)
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

(define (make-request #:user-message [um "hello"]
                      #:history [hist '()]
                      #:index [idx #f]
                      #:system-instructions [si '()]
                      #:provider? [prov #f]
                      #:working-set [ws #f]
                      #:max-tokens [mt 4000])
  (context-build-request um hist idx si prov ws mt))

(define-test-suite
 context-build-boundary-tests
 (test-case "request/result types are explicit and transparent"
   (define req (make-request))
   (check-true (context-build-request? req))
   (check-eq? (context-build-request-user-message req) "hello")
   (check-false (context-build-request-index req))
   (define res (context-build req))
   (check-true (context-build-result? res))
   (check-not-false (context-build-result-canonical-user-message res))
   (check-true (list? (context-build-result-context-messages res)))
   (check-true (list? (context-build-result-context-with-system res))))
 (test-case "context-build is pure: caller index unchanged"
   (define idx
     (make-test-index (list (make-test-message "a" #f 'user 'message)
                            (make-test-message "b" "a" 'assistant 'message))
                      "b"))
   (define before-by-id (session-index-by-id idx))
   (define before-order (session-index-entry-order idx))
   (define before-active (unbox (session-index-active-leaf-id idx)))
   (define req (make-request #:index idx))
   (define res (context-build req))
   (check-not-false (context-build-result-post-append-index res))
   (check-equal? (session-index-by-id idx) before-by-id "caller index by-id unchanged")
   (check-equal? (session-index-entry-order idx) before-order "caller entry order unchanged")
   (check-equal? (unbox (session-index-active-leaf-id idx))
                 before-active
                 "caller active-leaf box unchanged"))
 (test-case "result drives the exact historical effects (E2/E3/E4 values)"
   (define idx (make-test-index (list (make-test-message "leaf" #f 'user 'message)) "leaf"))
   (define res (context-build (make-request #:index idx)))
   (define appended (context-build-result-appended-entry res))
   (check-not-false appended)
   (check-equal? (message-parent-id appended) "leaf" "append parent linkage")
   (check-not-false (context-build-result-post-append-index res))
   (check-equal? (context-build-result-canonical-user-message res)
                 appended
                 "canonical message is the appended entry"))
 (test-case "linear history branch: context is history plus user message"
   (define hist (list (make-test-message "a" #f 'user 'message)))
   (define res (context-build (make-request #:history hist)))
   (define ctx (context-build-result-context-messages res))
   (check-equal? (length ctx) 2)
   (check-equal? (message-id (car ctx)) "a")
   (check-equal? (message-id (last ctx))
                 (message-id (context-build-result-canonical-user-message res))))
 (test-case "tiered branch: provider present uses index context"
   (define idx
     (make-test-index (list (make-test-message "a" #f 'user 'message)
                            (make-test-message "b" "a" 'assistant 'message))
                      "b"))
   (define ws (make-working-set))
   (define res (context-build (make-request #:index idx #:provider? #t #:working-set ws)))
   (check-not-false (pair? (context-build-result-context-messages res))))
 (test-case "path-derived model setting surfaces via linear history"
   (define path-msg
     (make-message "p1"
                   #f
                   'system
                   'model-change
                   (list (make-text-part "model"))
                   0
                   (hasheq 'model "gpt-test")))
   (define res (context-build (make-request #:history (list path-msg))))
   (check-equal? (context-build-result-model-name res) "gpt-test"))
 (test-case "system instructions are injected into the final context"
   (define res (context-build (make-request #:system-instructions '("be terse"))))
   (define ctx (context-build-result-context-with-system res))
   (check-not-false (pair? ctx))
   (check-eq? (message-role (car ctx)) 'system))
 (test-case "max-tokens is a request field"
   (define idx
     (make-test-index (list (make-test-message "a" #f 'user 'message)
                            (make-test-message "b" "a" 'assistant 'message))
                      "b"))
   (define res (context-build (make-request #:index idx #:provider? #t #:max-tokens 4000)))
   (check-not-false (pair? (context-build-result-context-messages res)))))

(module+ main
  (run-tests context-build-boundary-tests))
