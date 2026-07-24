#lang racket/base

;; tools/builtins/edit-contract.rkt — authoritative pure edit execution contract

(require racket/contract
         racket/string
         "edit-normalize.rkt")

(define DEFAULT-MAX-OLD-TEXT-LEN 500)
(define MAX-LINE-DELTA-DIFFERENCE 2)

(struct edit-contract-result (status content occurrences replacements fuzzy?) #:transparent)

(provide DEFAULT-MAX-OLD-TEXT-LEN
         (contract-out (struct edit-contract-result
                               ([status symbol?] [content string?]
                                                 [occurrences real?]
                                                 [replacements exact-nonnegative-integer?]
                                                 [fuzzy? boolean?]))
                       [apply-edit-contract
                        (->* (string? string? string?)
                             (#:fuzzy? boolean? #:max-old-text-len exact-nonnegative-integer?)
                             edit-contract-result?)]))

(define (string-index haystack needle [start 0])
  (define match (regexp-match-positions (regexp-quote needle) (substring haystack start)))
  (and match (+ start (caar match))))

(define (count-occurrences haystack needle)
  (define needle-length (string-length needle))
  (define haystack-length (string-length haystack))
  (if (or (zero? needle-length) (> needle-length haystack-length))
      0
      ;; Check every start position so overlapping matches are counted and
      ;; rejected as ambiguous.
      (for/sum ([start (in-range 0 (add1 (- haystack-length needle-length)))])
               (if (string=? (substring haystack start (+ start needle-length)) needle) 1 0))))

(define (replace-span content span new-text)
  (string-append (substring content 0 (car span)) new-text (substring content (cdr span))))

(define (line-count s)
  (length (string-split s "\n" #:trim? #f)))

(define (line-count-integrity-preserved? content old-text new-text new-content)
  (define expected-delta (- (line-count new-text) (line-count old-text)))
  (define actual-delta (- (line-count new-content) (line-count content)))
  (<= (abs (- actual-delta expected-delta)) MAX-LINE-DELTA-DIFFERENCE))

(define (replacement-outcome content old-text new-text span occurrences fuzzy?)
  (define new-content (replace-span content span new-text))
  (if (line-count-integrity-preserved? content old-text new-text new-content)
      (edit-contract-result 'ok new-content occurrences 1 fuzzy?)
      (edit-contract-result 'line-count-mismatch content occurrences 0 fuzzy?)))

(define (apply-edit-contract content
                             old-text
                             new-text
                             #:fuzzy? [fuzzy-enabled? #f]
                             #:max-old-text-len [max-old-text-len DEFAULT-MAX-OLD-TEXT-LEN])
  (cond
    [(zero? (string-length old-text)) (edit-contract-result 'empty-old-text content 0 0 #f)]
    [(> (string-length old-text) max-old-text-len) (edit-contract-result 'too-long content 0 0 #f)]
    [else
     (define occurrences (count-occurrences content old-text))
     (cond
       [(> occurrences 1) (edit-contract-result 'duplicate content occurrences 0 #f)]
       [(= occurrences 1)
        (define start (string-index content old-text))
        (define span (cons start (+ start (string-length old-text))))
        (replacement-outcome content old-text new-text span 1 #f)]
       [else
        (define fuzzy-spans
          (if fuzzy-enabled?
              (fuzzy-find-matches content old-text)
              '()))
        ;; W3: fuzzy-find-matches uses highest-score-only semantics — only
        ;; candidates with the maximal similarity score are returned.
        ;; When multiple candidates share the same top score, they are equal-
        ;; quality matches and the edit is rejected as ambiguous to prevent
        ;; silent mis-editing. Only a single unique highest-scoring match
        ;; proceeds to replacement.
        (cond
          [(> (length fuzzy-spans) 1)
           (edit-contract-result 'ambiguous content (length fuzzy-spans) 0 #t)]
          [(= (length fuzzy-spans) 1)
           (replacement-outcome content old-text new-text (car fuzzy-spans) 0 #t)]
          [else (edit-contract-result 'not-found content 0 0 #f)])])]))
