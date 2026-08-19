#lang racket/base

;; q/scripts/run-tests/ledger.rkt — Known broad-suite failure ledger support
;;
;; Ledger entries make broad-suite debt explicit and distinguish known failures
;; from new/unclassified failures and resolved historical failures.
;;
;; W8 QUARANTINE EXPIRY: an entry may carry `expires_on` (ISO YYYY-MM-DD).
;; While unexpired, the entry is quarantined (a tolerated, visible known
;; failure). From the expiry date onward (today >= expires_on) the quarantine
;; is over: the failure is reported as an escalating failure
;; (`expired_quarantine_failures`, escalate=#t) instead of being tolerated —
;; see docs/operations/test-regression-triage.md.
;; STABILITY: internal test-runner infrastructure

(require json
         racket/date
         racket/list
         racket/match
         racket/path
         racket/string
         (only-in "parse.rkt" test-file-result-path test-file-result-exit-code classify-test-result))

(provide load-known-failure-ledger
         normalize-ledger-entry
         summarize-ledger-results
         ledger-summary-counts
         ledger-entry-matches-result?
         ledger-entry-expired?
         valid-expires-on?
         today-iso8601)

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

;; Quarantine expiry dates must be unambiguous, zero-padded ISO dates so that
;; plain string comparison matches calendar order. Shape alone is not enough:
;; `2026-13-01` passes the regex but would sort permanently after every real
;; date and thus never expire. Validate calendar ranges too.
(define (valid-expires-on? v)
  (and (string? v)
       (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" v)
       (let ([month (string->number (substring v 5 7))]
             [day (string->number (substring v 8 10))])
         (and month day (<= 1 month 12) (<= 1 day 31)))))

(define (normalize-expires-on v)
  (cond
    [(or (not v) (eq? v 'null)) #f]
    [(valid-expires-on? v) v]
    [else (raise-arguments-error 'normalize-ledger-entry
                                 "expires_on must be an ISO YYYY-MM-DD string or null"
                                 "expires_on"
                                 v)]))

(define (pad2 n)
  (if (< n 10) (format "0~a" n) (number->string n)))

;; Local-timezone calendar date as "YYYY-MM-DD" (basis for quarantine expiry).
(define (today-iso8601)
  (define d (seconds->date (current-seconds)))
  (format "~a-~a-~a" (date-year d) (pad2 (date-month d)) (pad2 (date-day d))))

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
          (hash-ref* raw 'notes)
          'expires_on
          (normalize-expires-on (hash-ref* raw 'expires_on #f))))

;; #t once `today` has reached the entry's `expires_on` (the quarantine window
;; is [first_seen, expires_on); on the expiry date itself the entry escalates).
;; Entries without `expires_on` never expire through this mechanism.
(define (ledger-entry-expired? entry #:today [today (today-iso8601)])
  (define expires (hash-ref entry 'expires_on #f))
  (and expires (string>=? today expires)))

(define (extract-ledger-entries payload)
  (cond
    [(list? payload) payload]
    [(hash? payload) (hash-ref* payload 'entries '())]
    [else '()]))

(define (load-known-failure-ledger path)
  ;; The ledger file is optional: a missing path is an empty ledger (every
  ;; failure is "new"), not an error — W8's full-regression workflow always
  ;; passes --ledger and the file only appears once quarantined failures exist.
  (cond
    [(not (or (file-exists? path) (link-exists? path))) '()]
    [else (map normalize-ledger-entry
               (extract-ledger-entries (call-with-input-file path read-json)))]))

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

;; An expired quarantine match is surfaced as a failure that must escalate:
;; it keeps its identifying fields and carries escalate=#t so downstream
;; consumers (JSON evidence, triage) cannot mistake it for a tolerated known.
(define (result->escalation-entry entry)
  (hasheq 'file
          (hash-ref entry 'file)
          'category
          (hash-ref entry 'category)
          'expires_on
          (hash-ref entry 'expires_on #f)
          'owner
          (hash-ref entry 'owner)
          'issue
          (hash-ref entry 'issue)
          'escalate
          #t))

(define (summarize-ledger-results ledger results #:today [today (today-iso8601)])
  (define (expired? entry) (ledger-entry-expired? entry #:today today))
  (define failures (filter result-failure? results))
  (define known
    (for/list ([r (in-list failures)]
               #:do [(define entry (matching-entry ledger r))]
               #:when (and entry (not (expired? entry))))
      entry))
  (define expired-quarantine
    (for/list ([r (in-list failures)]
               #:do [(define entry (matching-entry ledger r))]
               #:when (and entry (expired? entry)))
      (result->escalation-entry entry)))
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
          'expired_quarantine_failures
          expired-quarantine
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
          'expired_quarantine_failures
          (length (hash-ref summary 'expired_quarantine_failures '()))
          'resolved_known_failures
          (length (hash-ref summary 'resolved_known_failures '()))
          'release_blocking_known_failures
          (length (hash-ref summary 'release_blocking_known_failures '()))))
