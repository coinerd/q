#lang racket

;; tests/test-session-navigation.rkt — FEAT-64: leaf pointer navigation

(require rackunit
         "../runtime/session-index.rkt"
         "../util/protocol-types.rkt")

;; Helper: create a session-index from a list of messages (in order)
(define (build-test-index messages)
  (define by-id (make-hash))
  (define children (make-hash))
  (for ([msg (in-list messages)])
    (hash-set! by-id (message-id msg) msg)
    (define pid (message-parent-id msg))
    (when pid
      (hash-set! children pid (cons (message-id msg) (hash-ref children pid '())))))
  (session-index by-id children (list->vector messages) (make-hash) (box #f) (make-semaphore 1)))

(define (make-msg id parent content)
  (message id parent 'user 'user content 1000 (hasheq)))

;; Tree:
;; root -> a -> b (leaf)
;; root -> c (leaf)
;; root -> d -> e -> f (leaf)
(define (make-test-index)
  (build-test-index (list (make-msg "root" #f "root")
                          (make-msg "a" "root" "a")
                          (make-msg "b" "a" "b")
                          (make-msg "c" "root" "c")
                          (make-msg "d" "root" "d")
                          (make-msg "e" "d" "e")
                          (make-msg "f" "e" "f"))))

;; ============================================================
;; navigate-to-entry!
;; ============================================================

(test-case "navigate-to-entry! returns entry and context"
  (define idx (make-test-index))
  (define result (navigate-to-entry! idx "b"))
  (check-not-false result)
  (check-equal? (message-id (navigate-result-entry result)) "b")
  ;; Branch: root -> a -> b
  (check-equal? (length (navigate-result-branch result)) 3)
  (check-true (navigate-result-leaf? result)))

(test-case "navigate-to-entry! returns #f for unknown entry"
  (define idx (make-test-index))
  (check-false (navigate-to-entry! idx "nonexistent")))

(test-case "navigate-to-entry! for non-leaf shows children"
  (define idx (make-test-index))
  (define result (navigate-to-entry! idx "a"))
  (check-not-false result)
  (check-false (navigate-result-leaf? result))
  (check-equal? (length (navigate-result-children result)) 1))

(test-case "navigate-to-entry! sets active leaf pointer"
  (define idx (make-test-index))
  (navigate-to-entry! idx "c")
  (define active (active-leaf idx))
  (check-not-false active)
  (check-equal? (message-id active) "c"))

;; ============================================================
;; navigate-to-leaf!
;; ============================================================

(test-case "navigate-to-leaf! succeeds for leaf node"
  (define idx (make-test-index))
  (define result (navigate-to-leaf! idx "f"))
  (check-not-false result)
  (check-equal? (message-id (navigate-result-entry result)) "f"))

(test-case "navigate-to-leaf! returns #f for non-leaf node"
  (define idx (make-test-index))
  (check-false (navigate-to-leaf! idx "a")))

;; ============================================================
;; navigate-next-leaf! / navigate-prev-leaf!
;; ============================================================

(test-case "navigate-next-leaf! cycles through leaves"
  (define idx (make-test-index))
  (navigate-to-entry! idx "b")
  (define r1 (navigate-next-leaf! idx))
  (check-not-false r1)
  (check-equal? (message-id (navigate-result-entry r1)) "c")
  (define r2 (navigate-next-leaf! idx))
  (check-not-false r2)
  (check-equal? (message-id (navigate-result-entry r2)) "f")
  ;; Wraps around
  (define r3 (navigate-next-leaf! idx))
  (check-not-false r3)
  (check-equal? (message-id (navigate-result-entry r3)) "b"))

(test-case "navigate-next-leaf! returns #f for empty index"
  (define idx (build-test-index '()))
  (check-false (navigate-next-leaf! idx)))

(test-case "navigate-prev-leaf! cycles backwards"
  (define idx (make-test-index))
  (navigate-to-entry! idx "c")
  (define r1 (navigate-prev-leaf! idx))
  (check-not-false r1)
  (check-equal? (message-id (navigate-result-entry r1)) "b"))

(test-case "navigate-prev-leaf! returns #f for empty index"
  (define idx (build-test-index '()))
  (check-false (navigate-prev-leaf! idx)))
