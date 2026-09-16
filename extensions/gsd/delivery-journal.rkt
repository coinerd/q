#lang racket/base
;; Coordinator-owned delivery state. Not campaign DONE/attempt state and NOT
;; delivery proof. JSON permits the trusted GitHub step adapter to consume the
;; same immutable Verify receipt. Callers hold the campaign lease for writes.
(require json
         racket/file
         racket/list
         racket/path)
(provide delivery-journal-path
         load-delivery-journal
         record-delivery-receipt!
         update-delivery-journal!
         valid-delivery-receipt?
         delivery-stages)
(define delivery-stages
  '("context-ready" "implementation-review"
                    "implementation-pr"
                    "implementation-ci"
                    "implementation-merged"
                    "binding-prepared"
                    "binding-review"
                    "binding-pr"
                    "binding-ci"
                    "binding-merged"
                    "governance"
                    "sync"
                    "delivered"
                    "awaiting-approval"
                    "retryable"
                    "blocked"))
(define (hex? s n)
  (and (string? s) (= (string-length s) n) (regexp-match? #px"^[0-9a-f]+$" s)))
(define (text? s)
  (and (string? s) (positive? (string-length s))))
(define (valid-delivery-receipt? r)
  (and (hash? r)
       (andmap (lambda (k) (text? (hash-ref r k #f))) '(repo branch origin evidence))
       (hex? (hash-ref r 'head #f) 40)
       (hex? (hash-ref r 'tree #f) 40)
       (exact-nonnegative-integer? (hash-ref r 'verified-at #f))))
(define (delivery-journal-path root plan wave)
  (unless (and (hex? plan 64) (exact-nonnegative-integer? wave))
    (error 'delivery-journal "invalid campaign/wave identity"))
  ;; Include ancestors of root, not just descendants: a redirected .planning
  ;; or checkout cannot place an authorization receipt outside its namespace.
  (define path
    (build-path (path->complete-path root)
                ".planning"
                "campaigns"
                plan
                (format "coordinator-w~a.json" wave)))
  (for/fold ([part (build-path "/")]) ([piece (in-list (cdr (explode-path path)))])
    (define next (build-path part piece))
    (when (link-exists? next)
      (error 'delivery-journal "symlinked journal path refused"))
    next)
  path)
(define (load-delivery-journal root plan wave)
  (define path (delivery-journal-path root plan wave))
  (and (file-exists? path)
       (let ([data (call-with-input-file path
                                         (lambda (in)
                                           (define v (read-json in))
                                           (unless (eof-object? (read-json in))
                                             (error 'delivery-journal "multiple journal values"))
                                           v))])
         (unless (and (hash? data)
                      (equal? (hash-ref data 'schema-version #f) 1)
                      (equal? (hash-ref data 'plan-id #f) plan)
                      (equal? (hash-ref data 'wave #f) wave)
                      (member (hash-ref data 'stage #f) delivery-stages)
                      (valid-delivery-receipt? (hash-ref data 'receipt #f)))
           (error 'delivery-journal "invalid journal; refusing to overwrite"))
         data)))
(define (save! root plan wave data)
  (define path (delivery-journal-path root plan wave))
  (make-parent-directory* path)
  (call-with-atomic-output-file path
                                (lambda (out _)
                                  (write-json data out)
                                  (newline out)))
  data)
(define (record-delivery-receipt! root plan wave receipt)
  (unless (valid-delivery-receipt? receipt)
    (error 'delivery-journal "invalid Verify receipt"))
  (define old (load-delivery-journal root plan wave))
  (cond
    [old
     (unless (equal? (hash-ref old 'receipt) receipt)
       (error 'delivery-journal "verified provenance changed; explicit reconciliation required"))
     old]
    [else
     (save! root
            plan
            wave
            (hasheq 'schema-version
                    1
                    'plan-id
                    plan
                    'wave
                    wave
                    'stage
                    "context-ready"
                    'receipt
                    receipt
                    'model-calls
                    0
                    'tokens
                    0
                    'cost
                    0
                    'usage-missing
                    #f))]))
(define (update-delivery-journal! root plan wave fields)
  (unless (and
           (hash? fields)
           (not (ormap (lambda (k) (hash-has-key? fields k)) '(receipt plan-id wave schema-version)))
           (or (not (hash-has-key? fields 'stage)) (member (hash-ref fields 'stage) delivery-stages)))
    (error 'delivery-journal "invalid journal update"))
  (define old (load-delivery-journal root plan wave))
  (unless old
    (error 'delivery-journal "missing verified provenance"))
  (save! root
         plan
         wave
         (for/fold ([data old]) ([(k v) (in-hash fields)])
           (hash-set data k v))))
