#lang racket/base

;; runtime/context-assembly/summary-entities.rkt — Entity extraction for summary validation
;;
;; SAL-04 fix: Extracts key entities (file paths, function/module names) from
;; messages so summaries can be checked for entity preservation.

(require racket/contract
         racket/string
         racket/list
         (only-in "../../util/message/message.rkt" message-content)
         (only-in "../../util/content/content-parts.rkt" text-part? text-part-text))

(provide (contract-out [extract-key-entities (->* (list?) (exact-positive-integer?) any/c)]
                       [check-entity-preservation (-> list? string? list?)]
                       [entity-preservation-appendix (-> list? string?)]))

;; File path patterns: /path/to/file.rkt, ./relative/path.rkt, src/file.rkt
(define FILE-PATH-RE
  (regexp (string-append "[/\\.]?" "[a-zA-Z0-9_./-]+" "\\.(rkt|rktl|scrbl|md|json|py)")))

;; Function/module name patterns: define-xxx, struct xxx
(define DEFINITION-RE (regexp (string-append "(define|defstruct|struct)" " [-a-zA-Z0-9_(]+")))

;; Extract top entities from a list of messages
(define (extract-key-entities messages [max-entities 20])
  (define all-text
    (string-join (for*/list ([m (in-list messages)]
                             [part (in-list (message-content m))]
                             #:when (text-part? part))
                   (text-part-text part))))
  (define file-paths (remove-duplicates (regexp-match* FILE-PATH-RE all-text) string=?))
  (define definitions (remove-duplicates (regexp-match* DEFINITION-RE all-text) string=?))
  (append (take-at-most file-paths (quotient max-entities 2))
          (take-at-most definitions (quotient max-entities 2))))

(define (take-at-most lst n)
  (take lst (min n (length lst))))

;; Check which entities are preserved in summary text
(define (check-entity-preservation entities summary-text)
  (for/list ([e (in-list entities)]
             #:when (not (string-contains? summary-text e)))
    e))

;; Generate appendix text for missing entities
(define (entity-preservation-appendix missing-entities)
  (if (null? missing-entities)
      ""
      (string-append "\n\n## Key Entities Referenced (auto-appended)\n"
                     (string-join (for/list ([e (in-list missing-entities)])
                                    (format "- ~a" e))
                                  "\n"))))
