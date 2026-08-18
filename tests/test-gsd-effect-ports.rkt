#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit

(require rackunit
         racket/list
         "../extensions/gsd/effect-ports.rkt"
         "helpers/gsd-port-fakes.rkt")

(module+ test
  (test-case "external-domain inventory is exact and port counts never exceed one"
    (check-equal? gsd-external-domains '(filesystem git github clock process event))
    (check-equal? (length gsd-external-domains) (length (remove-duplicates gsd-external-domains)))
    (check-equal? (hash-ref gsd-port-domain-counts 'github) 1)
    (for ([(domain count) (in-hash gsd-port-domain-counts)])
      (check-not-false (member domain gsd-external-domains))
      (check-true (<= count 1))))

  (test-case "aggregate contains one cohesive value per represented domain"
    (define-values (ports _state) (make-fake-gsd-effect-ports))
    (check-true (gsd-effect-ports? ports))
    (check-true (gsd-filesystem-port? (gsd-effect-ports-filesystem ports)))
    (check-true (gsd-git-port? (gsd-effect-ports-git ports)))
    (check-true (gsd-clock-port? (gsd-effect-ports-clock ports)))
    (check-true (gsd-process-port? (gsd-effect-ports-process ports)))
    (check-true (gsd-github-port? (gsd-effect-ports-github ports)))
    (check-true (procedure? (gsd-effect-ports-event-sink ports))))

  (test-case "aggregate contracts reject a wrong domain value"
    (define-values (ports _state) (make-fake-gsd-effect-ports))
    (check-exn exn:fail:contract?
               (lambda ()
                 (gsd-effect-ports 'not-a-filesystem
                                   (gsd-effect-ports-git ports)
                                   (gsd-effect-ports-clock ports)
                                   (gsd-effect-ports-process ports)
                                   (gsd-effect-ports-github ports)
                                   (gsd-effect-ports-event-sink ports)))))

  (test-case "filesystem fake is deterministic and records semantic operations"
    (define-values (ports state) (make-fake-gsd-effect-ports))
    (define fs (gsd-effect-ports-filesystem ports))
    ((gsd-filesystem-port-mkdir! fs) "/repo/.planning")
    ((gsd-filesystem-port-write-bytes! fs) "/repo/.planning/PLAN.md" #"v1")
    (check-equal? ((gsd-filesystem-port-read-bytes fs) "/repo/.planning/PLAN.md") #"v1")
    ((gsd-filesystem-port-rename! fs) "/repo/.planning/PLAN.md" "/repo/.planning/STATE.md")
    (check-false ((gsd-filesystem-port-kind fs) "/repo/.planning/PLAN.md"))
    (check-equal? ((gsd-filesystem-port-kind fs) "/repo/.planning/STATE.md") 'file)
    (define token ((gsd-filesystem-port-acquire-lock fs) "/repo/.planning/campaign.lock"))
    (check-not-false token)
    (check-false ((gsd-filesystem-port-acquire-lock fs) "/repo/.planning/campaign.lock"))
    ((gsd-filesystem-port-release-lock! fs) "/repo/.planning/campaign.lock" token)
    (check-not-false ((gsd-filesystem-port-acquire-lock fs) "/repo/.planning/campaign.lock"))
    (check-equal?
     (map car (fake-gsd-state-calls state))
     '(mkdir write read rename kind kind acquire-lock acquire-lock release-lock acquire-lock)))

  (test-case "git clock process and event fakes have independent deterministic behavior"
    (define-values (ports state) (make-fake-gsd-effect-ports))
    (define git (gsd-effect-ports-git ports))
    (define clock (gsd-effect-ports-clock ports))
    (define process (gsd-effect-ports-process ports))
    (check-equal? ((gsd-git-port-find-root git) "/repo/src") "/repo")
    (check-equal? ((gsd-git-port-head-summary git) "/repo" '("a.rkt" "b.rkt")) "abc123 change")
    (check-equal? ((gsd-clock-port-seconds clock)) 1700000000)
    (check-equal? ((gsd-clock-port-milliseconds clock)) 1700000000123)
    (fake-gsd-advance-clock! state 2000)
    (check-equal? ((gsd-clock-port-seconds clock)) 1700000002)
    (check-equal? ((gsd-clock-port-milliseconds clock)) 1700000002123)
    (check-equal? ((gsd-process-port-run process) "printf" '("ok") "/repo")
                  (gsd-process-result 0 #"ok\n" #""))
    ((gsd-process-port-stop-worker! process))
    ((gsd-effect-ports-event-sink ports) 'gsd.wave.started #hasheq((wave . 0)))
    (check-equal? (fake-gsd-state-events state) (list (cons 'gsd.wave.started #hasheq((wave . 0))))))

  (test-case "operation contracts reject malformed requests and broken adapters"
    (check-exn exn:fail:contract? (lambda () (gsd-clock-port 42 43)))
    (check-exn exn:fail:contract? (lambda () (gsd-git-port #f #f)))
    (define broken-clock (gsd-clock-port (lambda () "now") (lambda () 0)))
    (check-exn exn:fail:contract? (lambda () ((gsd-clock-port-seconds broken-clock))))
    (define-values (ports _state) (make-fake-gsd-effect-ports))
    (check-exn exn:fail:contract?
               (lambda ()
                 ((gsd-git-port-head-summary (gsd-effect-ports-git ports)) "/repo" 'not-a-list)))
    (check-exn exn:fail:contract?
               (lambda () ((gsd-effect-ports-event-sink ports) "not-a-symbol" #hasheq())))
    (check-exn exn:fail:contract?
               (lambda () ((gsd-effect-ports-event-sink ports) 'gsd.wave.started "not-a-hash")))
    (check-exn exn:fail:contract?
               (lambda () ((gsd-git-port-head-summary (gsd-effect-ports-git ports)) 42 '("a.rkt"))))))
