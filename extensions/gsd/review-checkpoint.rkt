#lang racket/base
;; STABILITY: internal
;; Exact, premerge rejection -> durable authorization -> ordinary retry.
;;
;; GSD simplification W5: authorization is TYPED. The internal API accepts the
;; rejection PAYLOAD (a hash carrying the exact receipt identity, the
;; normalized findings and the postmerge bit) and re-validates it against the
;; durable journal receipt. No internal decision parses a human message.
;;
;; The historic "review-rejected: {json}" message remains an EXTERNAL contract
;; (operator output and older coordinators), so it is preserved byte-for-byte:
;;   * pending-review-rejection-message renders it from the typed journal
;;     marker (never the other way round);
;;   * resolve-review-checkpoint! still accepts a legacy message STRING and
;;     converts it ONCE through the dated reader below
;;     (review-rejection-data-from-legacy-message). That reader is the only
;;     place that parses review rejection text, and it never runs on the
;;     internal typed path.
(require json
         racket/string
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         "review-rejection.rkt")
(provide resolve-review-checkpoint!
         pending-review-rejection-message
         pending-review-rejection-data
         review-rejection-data-from-legacy-message
         review-rejected-message-prefix)

(define review-rejected-message-prefix "review-rejected: ")

;; The journal's durable review-rejection marker as the TYPED authorization
;; payload (postmerge normalized to #f: a premerge checkpoint is the only one
;; that may authorize a retry). #f when no live, receipt-bound rejection is
;; pending.
(define (pending-review-rejection-data journal)
  (define marker (and journal (journal-review-rejection journal)))
  (and marker
       (not (hash-ref marker 'superseded-by-attempt #f))
       (review-rejection-matches-receipt? marker (hash-ref journal 'receipt #f))
       (hash-set marker 'postmerge #f)))

;; Legacy operator/API rendering of the same typed value. Constructed FROM the
;; typed payload; nothing downstream parses it back internally.
(define (pending-review-rejection-message journal)
  (define data (pending-review-rejection-data journal))
  (and data (string-append review-rejected-message-prefix (jsexpr->string data))))

;; DATED COMPAT READER (external/legacy callers only): the historic
;; "review-rejected: {json}" message -> the typed payload. Returns #f for
;; anything that is not that exact legacy envelope.
(define (review-rejection-data-from-legacy-message message)
  (and (string? message)
       (string-prefix? message review-rejected-message-prefix)
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (define data
           (string->jsexpr (substring message (string-length review-rejected-message-prefix))))
         (and (hash? data) data))))

;; Recover the journal-first / record-second crash window without dispatching
;; another reviewer or charging the durable rejection retry twice.
;; `rejection` is the typed payload (hash) or, for legacy external callers, the
;; historic message string.
(define (resolve-review-checkpoint! root plan wave rejection)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (define data
      (cond
        [(hash? rejection) rejection]
        [(string? rejection) (review-rejection-data-from-legacy-message rejection)]
        [else #f]))
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
