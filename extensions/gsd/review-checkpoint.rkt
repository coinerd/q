#lang racket/base
;; STABILITY: internal
;; Exact, premerge rejection -> durable authorization -> ordinary retry.
(require json
         racket/string
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         "review-rejection.rkt")
(provide resolve-review-checkpoint!
         pending-review-rejection-message)
;; Recover the journal-first / record-second crash window without dispatching
;; another reviewer or charging the durable rejection retry twice.
(define (pending-review-rejection-message journal)
  (define marker (and journal (journal-review-rejection journal)))
  (and marker
       (not (hash-ref marker 'superseded-by-attempt #f))
       (review-rejection-matches-receipt? marker (hash-ref journal 'receipt #f))
       (string-append "review-rejected: " (jsexpr->string (hash-set marker 'postmerge #f)))))
(define (resolve-review-checkpoint! root plan wave message)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (define prefix "review-rejected: ")
    (define data
      (and (string? message)
           (string-prefix? message prefix)
           (string->jsexpr (substring message (string-length prefix)))))
    (define journal (load-delivery-journal root plan wave))
    (define receipt (and journal (hash-ref journal 'receipt #f)))
    (cond
      [(not (and (hash? data)
                 (hash? receipt)
                 (boolean? (hash-ref data 'postmerge 'absent))
                 (normalize-review-findings (hash-ref data 'findings #f))
                 (for/and ([key '(head branch attempt-id attempt-fence)])
                   (equal? (hash-ref data key #f) (hash-ref receipt key #f)))))
       "malformed or stale review rejection checkpoint; no retry authorized"]
      [(hash-ref data 'postmerge)
       "postmerge binding review rejected: artifact repair unsupported; merged source is not rewound"]
      [else
       (define resolution
         (resolve-review-rejection! root
                                    plan
                                    wave
                                    (load-campaign-record root plan)
                                    #:findings (hash-ref data 'findings)
                                    #:expected-attempt-id (hash-ref data 'attempt-id)
                                    #:expected-fence (hash-ref data 'attempt-fence)))
       (cond
         [(review-rejection-refused? resolution) (review-rejection-resolution-reason resolution)]
         [else
          (persist-campaign! root (review-rejection-resolution-record resolution))
          #t])])))
