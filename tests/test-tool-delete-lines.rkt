#lang racket

(require rackunit
         "../tools/builtins/delete-lines.rkt"
         "../tools/tool.rkt"
         racket/file
         racket/string)

;; ============================================================
;; tool-delete-lines — basic deletion tests
;; ============================================================

(test-case "delete-lines: delete lines from middle of file"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (define content "line1\nline2\nline3\nline4\nline5")
  (display-to-file content tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 2 'end-line 3)))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "line1\nline4\nline5")
  (delete-file tmp))

(test-case "delete-lines: delete single line (start=end)"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb\nc" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 2 'end-line 2)))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "a\nc")
  (delete-file tmp))

(test-case "delete-lines: delete first line"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "first\nsecond\nthird" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 1 'end-line 1)))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "second\nthird")
  (delete-file tmp))

(test-case "delete-lines: delete last line"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb\nc" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 3 'end-line 3)))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "a\nb")
  (delete-file tmp))

(test-case "delete-lines: delete all lines"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "only-line" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 1 'end-line 1)))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "")
  (delete-file tmp))

;; ============================================================
;; tool-delete-lines — error cases
;; ============================================================

(test-case "delete-lines: file not found"
  (define result
    (tool-delete-lines (hasheq 'path "/nonexistent-dl-test.txt" 'start-line 1 'end-line 2)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "not found")))

(test-case "delete-lines: start-line < 1"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb\nc" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 0 'end-line 2)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "out of range"))
  (delete-file tmp))

(test-case "delete-lines: end-line > total lines"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 1 'end-line 5)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text)
                                "exceeds file length"))
  (delete-file tmp))

(test-case "delete-lines: start-line > end-line"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb\nc" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 3 'end-line 1)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "must be"))
  (delete-file tmp))

(test-case "delete-lines: missing path"
  (define result (tool-delete-lines (hasheq 'start-line 1 'end-line 2)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "path")))

(test-case "delete-lines: missing start-line"
  (define result (tool-delete-lines (hasheq 'path "/tmp/test.txt" 'end-line 2)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "start-line")))

(test-case "delete-lines: non-integer start-line"
  (define result (tool-delete-lines (hasheq 'path "/tmp/test.txt" 'start-line "abc" 'end-line 2)))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "integer")))

;; ============================================================
;; tool-delete-lines — details and backup
;; ============================================================

(test-case "delete-lines: result details include lines-deleted"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb\nc\nd\ne" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 2 'end-line 4)))
  (check-false (tool-result-is-error? result))
  (check-equal? (hash-ref (tool-result-details result) 'lines-deleted) 3)
  (check-equal? (hash-ref (tool-result-details result) 'start-line) 2)
  (check-equal? (hash-ref (tool-result-details result) 'end-line) 4)
  (check-equal? (hash-ref (tool-result-details result) 'remaining-lines) 2)
  (delete-file tmp))

(test-case "delete-lines: backup is created"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb\nc" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 2 'end-line 2)))
  (check-false (tool-result-is-error? result))
  (define backup (hash-ref (tool-result-details result) 'backup ""))
  (check-true (and (string? backup) (> (string-length backup) 0)))
  ;; Backup should exist
  (check-true (file-exists? backup))
  ;; Cleanup
  (delete-file tmp)
  (when (file-exists? backup)
    (delete-file backup)))

(test-case "delete-lines: preview shows deleted content"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (display-to-file "a\nb\nc" tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 2 'end-line 2)))
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "b"))
  (delete-file tmp))

(test-case "delete-lines: file content unchanged after error"
  (define tmp (make-temporary-file "q-test-dl-~a.txt"))
  (define original "a\nb\nc")
  (display-to-file original tmp #:exists 'replace)
  (define result (tool-delete-lines (hasheq 'path tmp 'start-line 3 'end-line 1)))
  (check-pred tool-result-is-error? result)
  (check-equal? (file->string tmp) original)
  (delete-file tmp))
