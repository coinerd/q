#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 120
(require rackunit
         racket/file
         "../extensions/gsd/evidence-repair-controller.rkt")
(module+ test
  (test-case "missing authority and stale live guards cannot admit a reviewer"
    (define base (make-temporary-file "repair-controller~a" 'directory))
    ;; Missing authority is a typed stop, never reviewer admission.
    (check-equal?
     (hash-ref (resume-evidence-repair! base (make-string 64 #\a) 0 #:guard (lambda () #t)) 'status)
     "blocked")
    (check-equal?
     (hash-ref (resume-evidence-repair! base (make-string 64 #\a) 0 #:guard (lambda () #f)) 'code)
     "stale-live-guard")
    (check-equal? (hash-ref (resume-evidence-repair! base (make-string 64 #\a) 0 #:guard #f) 'code)
                  "evidence-repair-blocked")
    (delete-directory/files base)))
