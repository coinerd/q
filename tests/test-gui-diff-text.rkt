#lang racket/base

;; @speed fast
;; @suite default

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
    (define insert-with-position? #f)
    ;; insert: positional insert when pos given, append otherwise
    (define/public (insert str [pos #f])
      (when pos
        (set! insert-with-position? #t))
      (if pos
          (set! content (string-append (substring content 0 pos) str (substring content pos)))
          (set! content (string-append content str))))
    (define/public (delete start end) (set! content (substring content 0 start)))
    (define/public (last-position) (string-length content))
    (define/public (lock v) (set! locked v))
    (define/public (change-style delta [start 0] [end 0]) (void))
    (define/public (get-content) content)
    (define/public (get-text start end) (substring content start (min end (string-length content))))
    (define/public (is-locked?) locked)
    (define/public (was-insert-positional?) insert-with-position?)))

(define-test-suite
 test-diff-text
 (test-case "apply-diff-to-text! append inserts new message"
   (define text-obj (make-object mock-text%))
   (define old-msgs '())
   (define new-msgs (list (hash 'role "user" 'text "Hello")))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme) #f)
   (define content (send text-obj get-content))
   (check-true (string-contains? content "You: Hello")))
 (test-case "apply-diff-to-text! no diff does nothing"
   (define text-obj (make-object mock-text%))
   (define msgs (list (hash 'role "user" 'text "Hello")))
   (apply-diff-to-text! text-obj msgs msgs (default-theme) #f)
   (check-equal? (send text-obj get-content) ""))
 (test-case "apply-diff-to-text! reset clears and rebuilds"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "user" 'text "Old")))
   (define new-msgs '())
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme) #f)
   ;; Reset causes full rebuild → content should be empty (no messages)
   (check-equal? (send text-obj get-content) ""))
 (test-case "apply-diff-to-text! multiple appends fall back to rebuild"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "user" 'text "A")))
   (define new-msgs
     (list (hash 'role "user" 'text "A")
           (hash 'role "assistant" 'text "B")
           (hash 'role "system" 'text "C")))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme) #f)
   (define content (send text-obj get-content))
   (check-true (string-contains? content "You: A"))
   (check-true (string-contains? content "Assistant: B"))
   (check-true (string-contains? content "System: C")))
 (test-case "apply-diff-to-text! update-last falls back to rebuild"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hel")))
   (define new-msgs (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hello")))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme) #f)
   (define content (send text-obj get-content))
   (check-true (string-contains? content "Hello"))))

(define-test-suite
 test-incremental-append
 (test-case "incremental suffix append with last-len-box"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hel")))
   (define new-msgs (list (hash 'role "user" 'text "Hi") (hash 'role "assistant" 'text "Hello")))
   ;; First call: populate text-obj with old messages (no incremental yet)
   (apply-diff-to-text! text-obj '() old-msgs (default-theme) #f)
   (define content-before (send text-obj get-content))
   ;; Second call: update-last with incremental suffix append
   (define last-len-box (box (string-length "Hel")))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme) last-len-box)
   (define content-after (send text-obj get-content))
   (check-true (string-contains? content-after "Hello"))
   (check-true (> (string-length content-after) (string-length content-before))))
 (test-case "incremental updates last-len-box value"
   (define text-obj (make-object mock-text%))
   (define old-msgs (list (hash 'role "assistant" 'text "ABC")))
   (define new-msgs (list (hash 'role "assistant" 'text "ABCDEF")))
   (apply-diff-to-text! text-obj '() old-msgs (default-theme) #f)
   (define last-len-box (box 3))
   (apply-diff-to-text! text-obj old-msgs new-msgs (default-theme) last-len-box)
   (check-equal? (unbox last-len-box) 6)))

(define test-positional-insert
  (test-suite "positional insert (selection-safe)"
    (test-case "insert-message-into-text! uses explicit position"
      (define text-obj (make-object mock-text%))
      (insert-message-into-text! text-obj (hash 'role "user" 'text "hello") (default-theme))
      (check-true (send text-obj was-insert-positional?)))))

(run-tests (test-suite "gui-diff-text"
             test-diff-text
             test-incremental-append
             test-positional-insert))
