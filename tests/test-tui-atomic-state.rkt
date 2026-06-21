#lang racket/base

;; @speed fast
;; @suite tui

;; q/tests/test-tui-atomic-state.rkt — Regression test for AXIS2-F05
;; Verify that atomic-state-update! prevents data loss under concurrent writes.

(require rackunit
         racket/match
         "../tui/context.rkt"
         "../tui/state.rkt")

;; Test 1: basic atomic update
(check-equal? (let ([b (box (initial-ui-state))])
                (atomic-state-update! b (lambda (s) (set-busy s #t)))
                (ui-state-busy? (unbox b)))
              #t
              "atomic-state-update! basic update")

;; Test 2: sequential updates preserve all mutations
(check-equal? (let ([b (box (initial-ui-state))])
                (atomic-state-update! b (lambda (s) (set-busy s #t)))
                (atomic-state-update! b (lambda (s) (clear-streaming s)))
                (ui-state-busy? (unbox b)))
              #t
              "sequential atomic updates preserve busy flag through clear-streaming")

;; Test 3: concurrent writes don't lose updates
(check-equal?
 (let* ([b (box (initial-ui-state))]
        [n 100]
        [sema (make-semaphore 1)]
        [barrier (make-semaphore 0)]
        [threads (for/list ([i (in-range n)])
                   (thread (lambda ()
                             (semaphore-wait barrier) ; wait for all threads to start
                             (atomic-state-update!
                              b
                              (lambda (s)
                                (add-transcript-entry s
                                                      (make-entry 'user
                                                                  (number->string i)
                                                                  (current-inexact-milliseconds)
                                                                  (hasheq))))))))])
   ;; Release all threads at once
   (for ([_ (in-range n)])
     (semaphore-post barrier))
   ;; Wait for all threads
   (for-each thread-wait threads)
   ;; All n entries should be present
   (length (ui-state-transcript (unbox b))))
 100
 "concurrent atomic updates don't lose entries")

;; Test 4: multiple boxes get independent semaphores
(check-not-exn (lambda ()
                 (let ([b1 (box (initial-ui-state))]
                       [b2 (box (initial-ui-state))])
                   (atomic-state-update! b1 (lambda (s) (set-busy s #t)))
                   (atomic-state-update! b2 (lambda (s) (set-busy s #f)))
                   (unless (and (ui-state-busy? (unbox b1)) (not (ui-state-busy? (unbox b2))))
                     (error 'test "independent boxes interfered")))))
