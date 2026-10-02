#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-journal.rkt
(require rackunit
         racket/file
         racket/list
         json
         "../extensions/gsd/delivery-journal.rkt")
(define plan (make-string 64 #\a))
(define receipt
  (hasheq 'repo
          "/repo/q"
          'branch
          "campaign/w2"
          'head
          (make-string 40 #\b)
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"
          'verified-at
          1
          'evidence
          "full Verify passed"))
(define (with-root f)
  (define root (make-temporary-file "delivery-journal-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))
(module+ test
  (test-case "receipt is immutable; journal updates do not replace provenance"
    (with-root
     (lambda (root)
       (check-false (load-delivery-journal root plan 2))
       (define initial (record-delivery-receipt! root plan 2 receipt))
       (check-equal? (hash-ref initial 'stage) "context-ready")
       (define saved
         (update-delivery-journal! root plan 2 (hasheq 'stage "implementation-ci" 'pr 42)))
       (check-equal? (hash-ref saved 'receipt) receipt)
       (check-equal? (record-delivery-receipt! root plan 2 receipt) saved)
       (check-exn
        exn:fail?
        (lambda ()
          (record-delivery-receipt! root plan 2 (hash-set receipt 'head (make-string 40 #\d)))))
       (check-exn exn:fail?
                  (lambda () (update-delivery-journal! root plan 2 (hasheq 'receipt receipt))))
       (check-equal? (load-delivery-journal root plan 2) saved))))
  (test-case "invalid identity, stage, absent or malformed receipt fail closed"
    (with-root
     (lambda (root)
       (for ([p (list "../bad" "abc" (make-string 64 #\z))])
         (check-exn exn:fail? (lambda () (delivery-journal-path root p 0))))
       (check-exn exn:fail? (lambda () (delivery-journal-path root plan -1)))
       (check-exn exn:fail?
                  (lambda () (update-delivery-journal! root plan 2 (hasheq 'stage "delivered"))))
       (check-exn exn:fail?
                  (lambda () (record-delivery-receipt! root plan 2 (hash-remove receipt 'head))))
       (record-delivery-receipt! root plan 2 receipt)
       (check-exn exn:fail?
                  (lambda () (update-delivery-journal! root plan 2 (hasheq 'stage "invented")))))))
  (test-case "symlinked and corrupt journal never silently become a new delivery"
    (with-root (lambda (root)
                 (define path (delivery-journal-path root plan 2))
                 (make-parent-directory* path)
                 (display-to-file "{}" path)
                 (check-exn exn:fail? (lambda () (load-delivery-journal root plan 2)))
                 (delete-file path)
                 (make-file-or-directory-link (build-path root "outside") path)
                 (check-exn exn:fail? (lambda () (record-delivery-receipt! root plan 2 receipt))))))
  (test-case "verification context is optional at load, durable, and write-once"
    (with-root
     (lambda (root)
       (define vc
         (hasheq 'base (make-string 40 #\2)
                 'merge-sha (make-string 40 #\b)
                 'pr-head (make-string 40 #\4)
                 'branch "binding/5e6770e9-w0"
                 'repo-root "/repo/q"
                 'verified-at 1790744745
                 'snapshot-refs (list "origin/campaign/x/w0")))
       (check-false (hash-ref (record-delivery-receipt! root plan 2 receipt)
                              'verification-context
                              #f))
       (record-verification-context! root plan 2 vc)
       (check-equal? (hash-ref (load-delivery-journal root plan 2) 'verification-context) vc)
       (check-equal? (record-verification-context! root plan 2 vc)
                     (load-delivery-journal root plan 2))
       (check-exn
        exn:fail?
        (lambda ()
          (record-verification-context!
           root plan 2 (hash-set vc 'merge-sha (make-string 40 #\e))))))))
  (test-case "malformed verification context fails closed at write and at load"
    (with-root
     (lambda (root)
       (define path (delivery-journal-path root plan 2))
       (record-delivery-receipt! root plan 2 receipt)
       (for ([bad (list (hasheq 'base "abc"
                                'merge-sha (make-string 40 #\b)
                                'pr-head (make-string 40 #\4)
                                'branch "b/w0"
                                'repo-root "/repo/q"
                                'verified-at 1
                                'snapshot-refs (list "r"))
                        (hasheq 'base (make-string 40 #\2)
                                'merge-sha (make-string 40 #\b)
                                'pr-head (make-string 40 #\4)
                                'branch "b/w0"
                                'repo-root "/repo/q"
                                'verified-at 1
                                'snapshot-refs (list)))])
         (check-exn exn:fail? (lambda () (record-verification-context! root plan 2 bad))))
       ;; a hand-edited journal carrying a malformed slot cannot load at all
       (let ([data (hash-set (load-delivery-journal root plan 2)
                             'verification-context
                             (hasheq 'base "short"))])
         (call-with-output-file path
                                #:exists 'replace
                                (lambda (out) (write-json data)))
         (check-exn exn:fail? (lambda () (load-delivery-journal root plan 2)))))))
  (test-case "verification context requires existing verified provenance"
    (with-root
     (lambda (root)
       (check-exn
        exn:fail?
        (lambda ()
          (record-verification-context!
           root plan 2
           (hasheq 'base (make-string 40 #\2)
                   'merge-sha (make-string 40 #\b)
                   'pr-head (make-string 40 #\4)
                   'branch "b/w0"
                   'repo-root "/repo/q"
                   'verified-at 1
                   'snapshot-refs (list "r")))))))))
