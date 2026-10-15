#lang racket/base
;; @covers extensions/gsd/campaign-state.rkt
;; @covers extensions/gsd/campaign-repository.rkt
;; @speed fast
(require rackunit racket/file racket/list racket/runtime-path
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt")
(define-runtime-path state-module "../extensions/gsd/campaign-state.rkt")
;; Dynamic lookup makes the initial RED specifically the missing feature API.
(define set-policy! (dynamic-require state-module 'set-campaign-record-execution-policy!))
(define policy (dynamic-require state-module 'campaign-record-execution-policy))
(define (with-project proc)
  (define dir (make-temporary-file "campaign-policy-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning" "waves"))
     (display-to-file "# Plan: Policy\n\n## Waves\n- [Inbox] W0: Zero → waves/W0-zero.md\n"
                      (build-path dir ".planning" "PLAN.md"))
     (display-to-file "# Wave 0\n\nGoal: zero\n" (build-path dir ".planning" "waves" "W0-zero.md"))
     (proc dir))
   (lambda () (delete-directory/files dir))))
(define (record-path dir rec)
  (build-path dir ".planning" "campaigns" (string-append (campaign-plan-id rec) ".rktd")))
(define (rewrite! path datum)
  (call-with-output-file path (lambda (out) (write datum out)) #:exists 'truncate))
(test-case "new seed defaults on, roundtrips, and policy never changes plan identity"
  (with-project
   (lambda (dir)
     (define rec (load-or-migrate-campaign! dir))
     (check-eq? (policy rec) 'owned-red-v1)
     (check-eq? (policy (load-campaign-record dir (campaign-plan-id rec))) 'owned-red-v1)
     (define pid (campaign-plan-id rec))
     (define before (call-with-input-file (record-path dir rec) read))
     (set-policy! rec #f)
     (persist-campaign! dir rec)
     (define after (call-with-input-file (record-path dir rec) read))
     (check-equal? (list-ref before 2) (list-ref after 2))
     (check-equal? pid (campaign-manifest-hash (campaign-record-manifest rec)))
     (check-exn exn:fail:contract? (lambda () (set-policy! rec 'unknown)))
     (check-false (policy (make-campaign-record pid (campaign-record-manifest rec) '() #f 0 #f 0 0))))))
(test-case "historical absence loads legacy and unknown policies fail closed"
  (with-project
   (lambda (dir)
     (define rec (load-or-migrate-campaign! dir))
     (define path (record-path dir rec))
     (define datum (call-with-input-file path read))
     (rewrite! path (drop-right datum 1))
     (check-false (policy (load-campaign-record dir (campaign-plan-id rec))))
     (for ([bad (in-list '(unknown "owned-red-v1" #t 1))])
       (rewrite! path (append (drop-right datum 1) (list bad)))
       (check-exn exn:fail:campaign-corrupt?
                  (lambda () (load-campaign-record dir (campaign-plan-id rec))))))))
(test-case "existing frozen snapshot without record stays legacy, never stamped"
  (with-project
   (lambda (dir)
     (define fresh (migrate-campaign! dir))
     (define snapshot (campaign-record-plan-snapshot-digest fresh))
     (define legacy (migrate-campaign! dir))
     (check-false (policy legacy))
     (check-equal? snapshot (campaign-record-plan-snapshot-digest legacy))
     (check-equal? (campaign-plan-id fresh) (campaign-plan-id legacy)))))
