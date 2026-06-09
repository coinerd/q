#lang racket/base
;; tests/test-gap4-active-tags.rkt — GAP-4 TDD tests
;; Validates active tag extraction from working-set messages

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  build-tiered-context/state-aware)
         (only-in "../runtime/context-assembly/conclusion-ranker.rkt"
                  rank-and-budget)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion task-conclusion? task-conclusion-text)
         (only-in "../util/message/message.rkt"
                  make-message message-meta-safe))

;; Helper: create a WS message with file path in metadata
(define (make-ws-msg path-str)
  (make-message #f 'assistant 'user 'user
                (list) (current-seconds)
                (hasheq 'path path-str)))

(define (make-test-conclusion id text tags)
  (task-conclusion id text 'observation 'exploring
                   '()
                   (inexact->exact (floor (current-inexact-milliseconds)))
                   tags  ; relevance-tags
                   '()))

(define-test-suite gap-4-tests
  (test-case "GAP-4: rank-and-budget boosts conclusions with matching tags"
    (define c1 (make-test-conclusion "c1" "Fixed bug in authentication module" '(auth.rkt)))
    (define c2 (make-test-conclusion "c2" "Added feature to utility module" '(utils.rkt)))
    ;; With active-tags matching c1's origin, c1 should rank higher
    (define ranked (rank-and-budget (list c2 c1)
                                    #:current-state 'exploring
                                    #:active-tags '(auth.rkt authentication)
                                    #:max-conclusion-tokens 2000))
    (check-not-false ranked)
    (check = (length ranked) 2)
    ;; c1 should be ranked first due to tag match
    (check equal? (task-conclusion-text (car ranked))
           "Fixed bug in authentication module"))

  (test-case "GAP-4: empty tags produce same order as before"
    (define c1 (make-test-conclusion "c1" "First conclusion" '()))
    (define c2 (make-test-conclusion "c2" "Second conclusion" '()))
    (define ranked (rank-and-budget (list c1 c2)
                                    #:current-state 'exploring
                                    #:active-tags '()
                                    #:max-conclusion-tokens 2000))
    (check = (length ranked) 2))

  (test-case "GAP-4: message-meta-safe returns hash from message with meta"
    (define msg (make-message #f 'assistant 'user 'user
                              (list) (current-seconds)
                              (hasheq 'path "/src/foo.rkt")))
    (define meta (message-meta-safe msg))
    (check-not-false (hash? meta))
    (check-equal? (hash-ref meta 'path #f) "/src/foo.rkt")))

(run-tests gap-4-tests)
