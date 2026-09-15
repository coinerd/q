#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-handoff.rkt
(require rackunit
         racket/file
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/campaign-state.rkt")
(module+ test
  (test-case "exact campaign proof missing fails closed without historical suffix fallback"
    (define dir (make-temporary-file "delivery-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (define evidence (build-path dir "docs/reports/gsd-wave-evidence"))
                    (make-directory* evidence)
                    (display-to-file (format "(merge-sha ~a)" (make-string 40 #\b))
                                     (build-path evidence "old-w1.rktd"))
                    (check-false (verified-wave-merge-sha dir (make-string 64 #\a) 1)))
                  (lambda () (delete-directory/files dir))))
  (test-case "durable handoff is idempotent and keeps verified identity"
    (define dir (make-temporary-file "delivery-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (define w (make-campaign-wave 1 "test" 'done 1 #f))
                    (set-campaign-wave-status! w 'done)
                    (set-campaign-wave-delivery-branch! w "campaign/test-w1")
                    (set-campaign-wave-delivery-head-sha! w (make-string 40 #\c))
                    (define p (persist-delivery-handoff! dir (make-string 64 #\a) w "PR pending"))
                    (define before (file->bytes p))
                    (check-equal? p
                                  (persist-delivery-handoff! dir (make-string 64 #\a) w "PR pending"))
                    (check-equal? before (file->bytes p))
                    (define datum (call-with-input-file p read))
                    (check-equal? (hash-ref datum 'delivery-head-sha) (make-string 40 #\c))
                    (check-equal? (hash-ref datum 'status) 'delivery-pending)
                    (check-equal? (campaign-wave-status w) 'done))
                  (lambda () (delete-directory/files dir)))))
