#lang racket

(require rackunit
         "../tools/builtins/edit-contract.rkt"
         "../tools/registry-table/core-tools.rkt"
         (only-in "../tools/registry-table/spec.rkt"
                  tool-spec-name
                  tool-spec-schema
                  tool-spec-prompt-guidelines))

(define (apply content old-text new-text #:fuzzy? [fuzzy? #f])
  (apply-edit-contract content old-text new-text #:fuzzy? fuzzy?))

(test-case "empty old-text is explicitly rejected"
  (define result (apply "unchanged" "" "prefix"))
  (check-equal? (edit-contract-result-status result) 'empty-old-text)
  (check-equal? (edit-contract-result-content result) "unchanged")
  (check-equal? (edit-contract-result-replacements result) 0))

(test-case "overlapping exact matches are ambiguous"
  (define result (apply "aaa" "aa" "changed"))
  (check-equal? (edit-contract-result-status result) 'duplicate)
  (check-equal? (edit-contract-result-occurrences result) 2)
  (check-equal? (edit-contract-result-content result) "aaa"))

(test-case "multiple normalized-equivalent fuzzy matches are rejected"
  (define content "alpha  \nbeta\n---\nalpha\t\nbeta")
  (define result (apply content "alpha\nbeta" "changed" #:fuzzy? #t))
  (check-equal? (edit-contract-result-status result) 'ambiguous)
  (check-equal? (edit-contract-result-content result) content)
  (check-equal? (edit-contract-result-replacements result) 0))

(test-case "CRLF fuzzy match ending on newline consumes the complete CRLF"
  (define result (apply "alpha\r\nbeta" "alpha\n" "gamma\n" #:fuzzy? #t))
  (check-equal? (edit-contract-result-status result) 'ok)
  (check-equal? (edit-contract-result-content result) "gamma\nbeta"))

(test-case "leading-whitespace auto-fallback applies a unique indentation-drift match"
  ;; Live incident (#9366): model old-text was +1 space per level vs the file.
  (define content "(define (f)\n        (write-config! path\n          (hasheq 'a 1))\n")
  (define old-text "(define (f)\n         (write-config! path\n           (hasheq 'a 1))\n")
  (define result (apply content old-text "(define (g)\n        (write-config! path)\n"))
  (check-equal? (edit-contract-result-status result) 'ok)
  (check-equal? (edit-contract-result-replacements result) 1)
  (check-equal? (edit-contract-result-content result) "(define (g)\n        (write-config! path)\n")
  (check-true (edit-contract-result-fuzzy? result)))

(test-case "leading-whitespace auto-fallback does NOT fire on different content"
  (define content "(define (f)\n  (list 1 2 3))\n")
  (define old-text "(define (f)\n (list 9 9 9))\n") ; same-ish indent, different body
  (define result (apply content old-text "changed"))
  (check-equal? (edit-contract-result-status result) 'not-found)
  (check-equal? (edit-contract-result-content result) content))

(test-case "leading-whitespace auto-fallback reports ambiguity for repeated blocks"
  (define content "(define (a)\n   x\n)\n(define (a)\n   x\n)\n")
  (define old-text "(define (a)\n     x\n)\n") ; both occurrences differ only in indent
  (define result (apply content old-text "changed"))
  (check-equal? (edit-contract-result-status result) 'ambiguous)
  (check-equal? (edit-contract-result-content result) content)
  (check-equal? (edit-contract-result-replacements result) 0))

(test-case "leading-whitespace auto-fallback requires byte-exact content after indent strip"
  (define content "(define (f)\n  (list 1 2 3))\n")
  (define old-text "(define (f)\n   (list 1 2 4))\n") ; differs beyond indentation
  (define result (apply content old-text "changed"))
  (check-equal? (edit-contract-result-status result) 'not-found))

(test-case "fuzzy? contract rejects non-boolean values"
  (check-exn exn:fail:contract?
             (lambda () (apply-edit-contract "alpha" "alpha" "beta" #:fuzzy? "false"))))

(test-case "edit schema declares fuzzy? as an optional boolean"
  (define edit-spec
    (for/first ([spec (in-list core-tool-specs)]
                #:when (string=? (tool-spec-name spec) "edit"))
      spec))
  (check-not-false edit-spec)
  (define schema (tool-spec-schema edit-spec))
  (define fuzzy-schema (hash-ref (hash-ref schema 'properties) 'fuzzy? #f))
  (check-not-false fuzzy-schema)
  (check-equal? (hash-ref fuzzy-schema 'type #f) "boolean")
  (check-true (string? (hash-ref fuzzy-schema 'description #f)))
  (check-false (regexp-match? #rx"replace[_-]all" (tool-spec-prompt-guidelines edit-spec))))
