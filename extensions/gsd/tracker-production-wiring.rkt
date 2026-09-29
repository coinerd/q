#lang racket/base
;; v1.00.33 W1: explicit live composition of the receipt-authoritative
;; tracker pass. A config switch alone has no authority: the exact campaign,
;; wave, issue, repository and project option must all be bound. No credentials
;; are read here; the in-repo Racket adapter uses the authenticated `gh` CLI.
(require (only-in "../../runtime/settings.rkt" load-settings setting-ref*)
         "gh-cli-tracker-adapter.rkt"
         "github-port.rkt"
         "tracker-reconciliation.rkt")
(provide resolve-live-tracker-reconciler
         live-tracker-target?)

(define (live-tracker-target? base-dir
                              plan-id
                              wave-index
                              #:settings [settings
                                          (with-handlers ([exn:fail? (lambda (_) #f)])
                                            (load-settings base-dir))])
  (define tracker (and settings (setting-ref* settings '(gsd tracker) #f)))
  (and (hash? tracker)
       (eq? (hash-ref tracker 'live #f) #t)
       (equal? (hash-ref tracker 'plan-id #f) plan-id)
       (equal? (hash-ref tracker 'wave #f) wave-index)
       #t))

(define (required-binding tracker plan-id wave-index)
  (unless (and (hash? tracker)
               (equal? (hash-ref tracker 'plan-id #f) plan-id)
               (equal? (hash-ref tracker 'wave #f) wave-index)
               (exact-positive-integer? (hash-ref tracker 'issue-number #f))
               (equal? (hash-ref tracker 'board-field #f) "Status")
               (equal? (hash-ref tracker 'board-value #f) "Done")
               (for/and ([key '(project-item-id project-id field-id option-id)])
                 (let ([id (hash-ref tracker key #f)])
                   (and (string? id) (positive? (string-length id))))))
    (error 'resolve-live-tracker-reconciler
           "live tracker binding does not match this campaign/wave or lacks board IDs"))
  (for/fold ([binding (hasheq 'plan-id
                              plan-id
                              'wave
                              wave-index
                              'issue-number
                              (hash-ref tracker 'issue-number)
                              'board-field
                              "Status"
                              'board-value
                              "Done")])
            ([key '(project-item-id project-id field-id option-id)])
    (hash-set binding key (hash-ref tracker key))))

;; The single-wave binding is intentional: enabling a second wave requires a
;; new explicit operator decision. A mismatch is a typed checkpoint refusal,
;; not an attempt to guess an issue from the wave's title or Git remote.
(define (resolve-live-tracker-reconciler base-dir
                                         plan-id
                                         wave-index
                                         #:settings [settings
                                                     (with-handlers ([exn:fail? (lambda (_) #f)])
                                                       (load-settings base-dir))]
                                         #:adapter-maker [adapter-maker make-gh-cli-tracker-adapter])
  (define tracker (and settings (setting-ref* settings '(gsd tracker) #f)))
  (and (hash? tracker)
       (eq? (hash-ref tracker 'live #f) #t)
       (lambda (actual-base actual-plan actual-wave #:delivery-reader reader)
         (unless (and (equal? actual-plan plan-id)
                      (equal? actual-wave wave-index)
                      (equal? actual-base base-dir))
           (error 'resolve-live-tracker-reconciler "campaign invocation identity changed"))
         (define binding (required-binding tracker plan-id wave-index))
         (define adapter (adapter-maker #:live? #t #:repository (hash-ref tracker 'repository #f)))
         (define port (make-github-port adapter #:dry-run? #f))
         (reconcile-tracker-after-delivery! base-dir
                                            plan-id
                                            wave-index
                                            binding
                                            port
                                            #:delivery-reader reader))))
