#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/class
         racket/string
         "../gui/components/rich-transcript-view.rkt"
         "../ui-core/theme-protocol.rkt")

;; Minimal mock text% that records operations
(define mock-text%
  (class object%
    (super-new)
    (define content "")
    (define locked #f)
    (define/public (insert str) (set! content (string-append content str)))
    (define/public (delete start end) (set! content ""))
    (define/public (last-position) (string-length content))
    (define/public (lock v) (set! locked v))
    (define/public (change-style delta [start 0] [end 0]) (void))
    (define/public (get-content) content)
    (define/public (is-locked?) locked)))

(define-test-suite
 test-diff-text
 (test-case "apply-diff-to-text! append inserts new message"
   (define text-obj (make-object mock-text%))
   (define old-msgs '())
   (define new-msgs (list (hash 'role "user" 'text "Hello")))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme))
   (define content (send text-obj get-content))
   (check-true (string-contains? content "You: Hello")))
 (test-case "apply-diff-to-text! no diff does nothing"
   (define text-obj (make-object mock-text%))
   (define msgs (list (hash 'role "user" 'text "Hello")))
   (apply-diff-to-text! text-obj msgs msgs (default-theme))
   (check-equal? (send text-obj get-content) ""))
 (test-case "apply-diff-to-text! reset clears and rebuilds"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "user" 'text "Old")))
   (define new-msgs '())
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme))
   ;; Reset causes full rebuild → content should be empty (no messages)
   (check-equal? (send text-obj get-content) ""))
 (test-case "apply-diff-to-text! multiple appends fall back to rebuild"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "user" 'text "A")))
   (define new-msgs
     (list (hash 'role "user" 'text "A")
           (hash 'role "assistant" 'text "B")
           (hash 'role "system" 'text "C")))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme))
   (define content (send text-obj get-content))
   (check-true (string-contains? content "You: A"))
   (check-true (string-contains? content "Assistant: B"))
   (check-true (string-contains? content "System: C")))
 (test-case "apply-diff-to-text! update-last falls back to rebuild"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hel")))
   (define new-msgs (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hello")))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme))
   (define content (send text-obj get-content))
   (check-true (string-contains? content "Hello"))))

(run-tests test-diff-text)
