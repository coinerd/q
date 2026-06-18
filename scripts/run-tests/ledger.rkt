#lang racket/base

;; q/scripts/run-tests/ledger.rkt — Known broad-suite failure ledger support
;;
;; Ledger entries make broad-suite debt explicit and distinguish known failures
;; from new/unclassified failures and resolved historical failures.
;; STABILITY: internal test-runner infrastructure

(require json
         racket/list
         racket/match
         racket/path
         racket/string
         (only-in "parse.rkt" test-file-result-path test-file-result-exit-code classify-test-result))

(provide load-known-failure-ledger
         normalize-ledger-entry
         summarize-ledger-results
         ledger-summary-counts
         ledger-entry-matches-result?)

(define required-ledger-keys '(file category owner first_seen release_blocking issue notes))
(define missing-sentinel (gensym 'missing))

(define (hash-ref* h key [default #f])
  (cond
    [(hash-has-key? h key) (hash-ref h key)]
    [(hash-has-key? h (string->symbol (symbol->string key)))
     (hash-ref h (string->symbol (symbol->string key)))]
    [else default]))

(define (path->ledger-string p)
  (cond
    [(path? p) (path->string p)]
    [(string? p) p]
    [else (format "~a" p)]))

(define (normalize-path-string p)
  (define s (path->ledger-string p))
  (cond
    [(string-prefix? s "./") (substring s 2)]
    [else s]))

(define (normalize-category v)
  (cond
    [(symbol? v) (symbol->string v)]
    [(string? v) v]
    [else (format "~a" v)]))

(define (normalize-ledger-entry raw)
  (unless (hash? raw)
    (raise-arguments-error 'normalize-ledger-entry "ledger entry must be a JSON object" "entry" raw))
  (for ([key (in-list required-ledger-keys)])
    (when (eq? (hash-ref* raw key missing-sentinel) missing-sentinel)
      (raise-arguments-error 'normalize-ledger-entry
                             "ledger entry missing required key"
                             "key"
                             key
                             "entry"
                             raw)))
  (hasheq 'file
          (normalize-path-string (hash-ref* raw 'file))
          'category
          (normalize-category (hash-ref* raw 'category))
          'owner
          (hash-ref* raw 'owner)
          'first_seen
          (hash-ref* raw 'first_seen)
          'release_blocking
          (and (hash-ref* raw 'release_blocking) #t)
          'issue
          (hash-ref* raw 'issue)
          'notes
          (hash-ref* raw 'notes)))

(define (extract-ledger-entries payload)
  (cond
    [(list? payload) payload]
    [(hash? payload) (hash-ref* payload 'entries '())]
    [else '()]))

(define (load-known-failure-ledger path)
  (define payload (call-with-input-file path read-json))
  (map normalize-ledger-entry (extract-ledger-entries payload)))

(define (result-failure? r)
  (and (not (= (test-file-result-exit-code r) 0))
       (not (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))))

(define (result-path-string r)
  (normalize-path-string (test-file-result-path r)))

(define (result-category-string r)
  (symbol->string (classify-test-result r)))

(define (same-file? entry r)
  (string=? (hash-ref entry 'file) (result-path-string r)))

(define (ledger-entry-matches-result? entry r)
  (and (same-file? entry r) (string=? (hash-ref entry 'category) (result-category-string r))))

(define (matching-entry ledger r)
  (for/first ([entry (in-list ledger)]
              #:when (ledger-entry-matches-result? entry r))
    entry))

(define (file-entry ledger r)
  (for/first ([entry (in-list ledger)]
              #:when (same-file? entry r))
    entry))

(define (result->unclassified-entry r)
  (hasheq 'file
          (result-path-string r)
          'category
          (result-category-string r)
          'known_category
          (let ([entry (file-entry '() r)]) (and entry (hash-ref entry 'category #f)))))

(define (result->new-entry r)
  (hasheq 'file (result-path-string r) 'category (result-category-string r)))

(define (summarize-ledger-results ledger results)
  (define failures (filter result-failure? results))
  (define known
    (for/list ([r (in-list failures)]
               #:do [(define entry (matching-entry ledger r))]
               #:when entry)
      entry))
  (define new
    (for/list ([r (in-list failures)]
               #:when (not (file-entry ledger r)))
      (result->new-entry r)))
  (define unclassified
    (for/list ([r (in-list failures)]
               #:when (not (matching-entry ledger r)))
      (define by-file (file-entry ledger r))
      (hasheq 'file
              (result-path-string r)
              'category
              (result-category-string r)
              'known_category
              (and by-file (hash-ref by-file 'category #f))
              'issue
              (and by-file (hash-ref by-file 'issue #f)))))
  (define resolved
    (for/list ([entry (in-list ledger)]
               #:unless (for/or ([r (in-list failures)])
                          (same-file? entry r)))
      entry))
  (define release-blocking-known
    (filter (lambda (entry) (hash-ref entry 'release_blocking #f)) known))
  (hasheq 'known_failures
          known
          'new_failures
          new
          'unclassified_failures
          unclassified
          'resolved_known_failures
          resolved
          'release_blocking_known_failures
          release-blocking-known))

(define (ledger-summary-counts summary)
  (hasheq 'known_failures
          (length (hash-ref summary 'known_failures '()))
          'new_failures
          (length (hash-ref summary 'new_failures '()))
          'unclassified_failures
          (length (hash-ref summary 'unclassified_failures '()))
          'resolved_known_failures
          (length (hash-ref summary 'resolved_known_failures '()))
          'release_blocking_known_failures
          (length (hash-ref summary 'release_blocking_known_failures '()))))
