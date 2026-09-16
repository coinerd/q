#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-receipt.rkt
(require rackunit
         racket/file
         racket/path
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt")
(define plan (make-string 64 #\a))
(define identity
  (hasheq 'repo
          "/repo"
          'branch
          "campaign/w2"
          'head
          (make-string 40 #\b)
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"))
(define (with-root f)
  (define root (make-temporary-file "delivery-receipt-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))
(module+ test
  (test-case "isolated receipt names surviving object repository, not disposable worktree"
    (with-root
     (lambda (root)
       (define repo (build-path root "q"))
       (define wt (build-path root "worker"))
       (make-directory repo)
       (git-quiet! repo "init" "-q")
       (hermetic-identity! repo)
       (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
       (git-quiet! repo "commit" "--allow-empty" "-qm" "baseline")
       (git-quiet! repo "worktree" "add" "-qb" "campaign/w2" (path->string wt))
       (define snapshot (committed-delivery-snapshot wt))
       (check-true (hash? snapshot))
       (check-equal? (string->path (hash-ref snapshot 'repo)) (path->directory-path repo))
       (git-quiet! repo "worktree" "remove" (path->string wt))
       (check-not-false
        (recover-legacy-delivery-receipt! root plan 2 "campaign/w2" (hash-ref snapshot 'head) repo))
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 2) 'receipt) 'head)
                     (hash-ref snapshot 'head))
       (check-false
        (recover-legacy-delivery-receipt! root plan 3 "campaign/w2" (make-string 40 #\f) repo)))))
  (test-case "only stable clean identity across successful Verify becomes a receipt"
    (with-root (lambda (root)
                 (define result
                   (verify-with-delivery-receipt root
                                                 plan
                                                 2
                                                 root
                                                 (lambda () 'approved)
                                                 #:approved? (lambda (v) (eq? v 'approved))
                                                 #:evidence (lambda (_) "full Verify log")
                                                 #:snapshot (lambda (_) identity)))
                 (check-eq? result 'approved)
                 (define receipt (hash-ref (load-delivery-journal root plan 2) 'receipt))
                 (check-equal? (hash-ref receipt 'head) (hash-ref identity 'head))
                 (check-equal? (hash-ref receipt 'evidence) "full Verify log"))))
  (test-case "dirty/missing identity, head changes, and failed Verify create no receipt"
    (for ([kind '(dirty changed failed)])
      (with-root
       (lambda (root)
         (define count 0)
         (verify-with-delivery-receipt
          root
          plan
          2
          root
          (lambda () (not (eq? kind 'failed)))
          #:approved? values
          #:evidence (lambda (_) "result")
          #:snapshot
          (lambda (_)
            (set! count (add1 count))
            (case kind
              [(dirty) #f]
              [(changed) (hash-set identity 'head (make-string 40 (if (= count 1) #\b #\d)))]
              [else identity])))
         (check-false (load-delivery-journal root plan 2))))))
  (test-case "legacy empty head cannot be replaced by current checkout"
    (with-root (lambda (root)
                 (check-false (recover-legacy-delivery-receipt! root plan 2 "" "" root))
                 (check-false (load-delivery-journal root plan 2))))))
