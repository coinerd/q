#lang racket

(require rackunit
         "../tools/builtins/edit.rkt"
         "../tools/tool.rkt"
         "../tools/registry-defaults.rkt"
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
  (define result (tool-edit (hasheq 'path "/nonexistent.txt" 'old-text "x" 'new-text "y")))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "not found")))

(test-case "tool-edit returns error when old-text not found"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "hello world" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "missing" 'new-text "x")))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "not found"))
  (delete-file tmp))

(test-case "tool-edit returns error for ambiguous match"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "abc abc abc" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "abc" 'new-text "xyz")))
  (check-pred tool-result-is-error? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text) "appears"))
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

;; ============================================================
;; tool-edit — near-match hint tests (v0.19.9)
;; ============================================================

(test-case "tool-edit: nonexistent old-text returns helpful error (no near-match)"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "hello world\nfoo bar\nbaz quux" tmp #:exists 'replace)
  (define result
    (tool-edit (hasheq 'path tmp 'old-text "completely unrelated gibberish" 'new-text "x")))
  (check-pred tool-result-is-error? result)
  (define msg (hash-ref (car (tool-result-content result)) 'text))
  (check-true (string-contains? msg "not found"))
  (check-true (string-contains? msg "Read the file first"))
  (check-false (string-contains? msg "Nearest match"))
  (delete-file tmp))

(test-case "tool-edit: close-but-wrong old-text returns near-match hint with line number"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "line one\n  pdf.cell(pw, 4, \"Niemand stoert\", align='C')\nline three"
                   tmp
                   #:exists 'replace)
  (define result
    (tool-edit
     (hasheq 'path tmp 'old-text "    pdf.cell(pw, 4, \"Niemand stoort\", align='C')" 'new-text "x")))
  (check-pred tool-result-is-error? result)
  (define msg (hash-ref (car (tool-result-content result)) 'text))
  (check-true (string-contains? msg "not found"))
  (check-true (string-contains? msg "Nearest match at line") msg)
  (check-true (string-contains? msg "Niemand stoert") msg)
  (check-true (string-contains? msg "Re-read") msg)
  (delete-file tmp))

(test-case "tool-edit: near-match picks closest of multiple candidates"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "def foo(x):\n    return x + 1\ndef bar(x):\n    return x - 1"
                   tmp
                   #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "    return x + 2" 'new-text "y")))
  (check-pred tool-result-is-error? result)
  (define msg (hash-ref (car (tool-result-content result)) 'text))
  (check-true (string-contains? msg "Nearest match at line") msg)
  (check-true (string-contains? msg "x + 1") msg)
  (delete-file tmp))

(test-case "tool-edit: near-match works with whitespace differences"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "  indented content here\nother line" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "indented content their" 'new-text "x")))
  (check-pred tool-result-is-error? result)
  (define msg (hash-ref (car (tool-result-content result)) 'text))
  (check-true (string-contains? msg "Nearest match") msg)
  (delete-file tmp))

;; ============================================================
;; Registry: edit tool has prompt-guidelines and strong description
;; ============================================================

(test-case "edit tool: prompt-guidelines set in registry"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define t (lookup-tool reg "edit"))
  (check-not-false t)
  (check-not-false (tool-prompt-guidelines t) "edit tool should have prompt-guidelines set")
  (check-true (string-contains? (tool-prompt-guidelines t) "verbatim"))
  (check-true (string-contains? (tool-prompt-guidelines t) "re-read")))

(test-case "edit tool: description contains verbatim guidance"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define t (lookup-tool reg "edit"))
  (check-not-false t)
  (check-true (string-contains? (tool-description t) "verbatim")))

;; ============================================================
;; tool-edit — v0.19.10 safeguards
;; ============================================================

(test-case "tool-edit: rejects old-text longer than 500 chars"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file (make-string 600 #\x) tmp #:exists 'replace)
  (define long-old-text (make-string 501 #\x))
  (define result (tool-edit (hasheq 'path tmp 'old-text long-old-text 'new-text "y")))
  (check-pred tool-result-is-error? result)
  (define msg (hash-ref (car (tool-result-content result)) 'text))
  (check-true (string-contains? msg "too long"))
  (check-true (string-contains? msg "500"))
  (check-true (string-contains? msg "Break your edit"))
  ;; File must not be modified
  (check-equal? (file->string tmp) (make-string 600 #\x))
  (delete-file tmp))

(test-case "tool-edit: accepts old-text at exactly 500 chars"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (define content (string-append (make-string 500 #\a) "suffix"))
  (display-to-file content tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text (make-string 500 #\a) 'new-text "REPLACED")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "REPLACEDsuffix")
  (delete-file tmp))

(test-case "tool-edit: creates backup before successful edit"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "original content here" tmp #:exists 'replace)
  (define result (tool-edit (hasheq 'path tmp 'old-text "original" 'new-text "modified")))
  (check-false (tool-result-is-error? result))
  ;; Check backup was saved and details include backup path
  (define details (tool-result-details result))
  (check-true (hash-has-key? details 'backup))
  (define backup-path (hash-ref details 'backup))
  (when (and (string? backup-path) (non-empty-string? backup-path))
    (check-true (file-exists? backup-path) "backup file should exist")
    (check-equal? (file->string backup-path) "original content here")
    (delete-file backup-path))
  (delete-file tmp))

(test-case "tool-edit: line-count integrity passes for normal edit"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "line1\nline2\nline3" tmp #:exists 'replace)
  ;; Replace one line — delta should be 0 (same number of lines)
  (define result (tool-edit (hasheq 'path tmp 'old-text "line2" 'new-text "LINE2")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "line1\nLINE2\nline3")
  (delete-file tmp))

(test-case "tool-edit: line-count integrity passes when adding lines"
  (define tmp (make-temporary-file "q-test-edit-~a.txt"))
  (display-to-file "line1\nline2" tmp #:exists 'replace)
  ;; Replace single line with two lines — expected delta = +1
  (define result (tool-edit (hasheq 'path tmp 'old-text "line1" 'new-text "line1a\nline1b")))
  (check-false (tool-result-is-error? result))
  (check-equal? (file->string tmp) "line1a\nline1b\nline2")
  (delete-file tmp))

(test-case "tool-edit: prompt-guidelines mention safeguards"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define t (lookup-tool reg "edit"))
  (check-not-false t)
  (define guidelines (tool-prompt-guidelines t))
  ;; v0.19.10: guidelines should mention the 500-char limit
  (check-true (string-contains? guidelines "500") "should mention 500-char limit"))
