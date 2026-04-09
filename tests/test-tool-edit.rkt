#lang racket

(require rackunit
         "../tools/builtins/edit.rkt"
         "../tools/tool.rkt"
         racket/file)

;; ============================================================
;; tool-edit — basic edit
;; ============================================================

(test-case "tool-edit replaces text in file"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "hello world" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "hello" 'new-text "goodbye")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "goodbye world")
  (delete-file tmp))

(test-case "tool-edit returns error for missing file"
  (define result (tool-edit (hasheq 'path "/nonexistent.txt"
                                     'old-text "x"
                                     'new-text "y")))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text)
                                 "not found")))

(test-case "tool-edit returns error when old-text not found"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "hello world" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "missing" 'new-text "x")))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text)
                                 "not found"))
  (delete-file tmp))

(test-case "tool-edit returns error for ambiguous match"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "abc abc abc" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "abc" 'new-text "xyz")))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text)
                                 "appears"))
  (delete-file tmp))

(test-case "tool-edit details include replacements count"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "hello world" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "hello" 'new-text "hi")))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'replacements) 1)
  (delete-file tmp))

(test-case "tool-edit details include old-length and new-length"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "hello world" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "hello" 'new-text "greetings")))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'old-length) 5)
  (check-equal? (hash-ref (tool-result-details result) 'new-length) 9)
  (delete-file tmp))
