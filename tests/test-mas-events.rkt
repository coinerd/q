#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-mas-events.rkt — W3 (v0.99.7) MAS Typed Event Tests
;;
;; Tests for the five MAS coordination events:
;;   mas-artifact-produced-event, mas-test-result-event,
;;   mas-hypothesis-opened-event, mas-hypothesis-resolved-event,
;;   mas-blackboard-sync-event

(require rackunit
         rackunit/text-ui
         "../agent/event-structs/mas-events.rkt"
         (only-in "../agent/event-structs/base.rkt"
                  typed-event?
                  typed-event-type
                  typed-event-timestamp
                  typed-event-session-id
                  typed-event-turn-id))

(define suite
  (test-suite "MAS Events (W3 v0.99.7)"

    ;; ── mas-artifact-produced-event ──

    (test-case "mas-artifact-produced-event constructs with required fields"
      (define evt
        (make-mas-artifact-produced-event #:session-id "s1"
                                          #:turn-id "t1"
                                          #:name "plan.md"
                                          #:path "/tmp/plan.md"
                                          #:artifact-type 'markdown))
      (check-true (mas-artifact-produced-event? evt))
      (check-equal? (mas-artifact-produced-event-name evt) "plan.md")
      (check-equal? (mas-artifact-produced-event-path evt) "/tmp/plan.md")
      (check-equal? (mas-artifact-produced-event-artifact-type evt) 'markdown))

    (test-case "mas-artifact-produced-event has correct type string"
      (define evt
        (make-mas-artifact-produced-event #:session-id "s"
                                          #:turn-id "t"
                                          #:name "x"
                                          #:path "x"
                                          #:artifact-type 'source))
      (check-equal? (typed-event-type evt) "mas.artifact.produced"))

    (test-case "mas-artifact-produced-event optional agent-name defaults to #f"
      (define evt
        (make-mas-artifact-produced-event #:session-id "s"
                                          #:turn-id "t"
                                          #:name "x"
                                          #:path "x"
                                          #:artifact-type 'test))
      (check-false (mas-artifact-produced-event-agent-name evt)))

    (test-case "mas-artifact-produced-event accepts optional agent-name"
      (define evt
        (make-mas-artifact-produced-event #:session-id "s"
                                          #:turn-id "t"
                                          #:name "x"
                                          #:path "x"
                                          #:artifact-type 'test
                                          #:agent-name "executor"))
      (check-equal? (mas-artifact-produced-event-agent-name evt) "executor"))

    (test-case "mas-artifact-produced-event-fields lists all fields"
      (check-equal? mas-artifact-produced-event-fields '(name path artifact-type agent-name)))

    ;; ── mas-test-result-event ──

    (test-case "mas-test-result-event constructs with required fields"
      (define evt
        (make-mas-test-result-event #:session-id "s" #:turn-id "t" #:file "test.rkt" #:result 'pass))
      (check-true (mas-test-result-event? evt))
      (check-equal? (mas-test-result-event-file evt) "test.rkt")
      (check-equal? (mas-test-result-event-result evt) 'pass))

    (test-case "mas-test-result-event has correct type string"
      (define evt
        (make-mas-test-result-event #:session-id "s" #:turn-id "t" #:file "f" #:result 'fail))
      (check-equal? (typed-event-type evt) "mas.test.result"))

    (test-case "mas-test-result-event optional fields default to #f"
      (define evt
        (make-mas-test-result-event #:session-id "s" #:turn-id "t" #:file "f" #:result 'pass))
      (check-false (mas-test-result-event-duration-ms evt))
      (check-false (mas-test-result-event-test-count evt))
      (check-false (mas-test-result-event-pass-count evt)))

    (test-case "mas-test-result-event accepts optional fields"
      (define evt
        (make-mas-test-result-event #:session-id "s"
                                    #:turn-id "t"
                                    #:file "f"
                                    #:result 'pass
                                    #:duration-ms 150
                                    #:test-count 20
                                    #:pass-count 20))
      (check-equal? (mas-test-result-event-duration-ms evt) 150)
      (check-equal? (mas-test-result-event-test-count evt) 20)
      (check-equal? (mas-test-result-event-pass-count evt) 20))

    ;; ── mas-hypothesis-opened-event ──

    (test-case "mas-hypothesis-opened-event constructs with required fields"
      (define evt
        (make-mas-hypothesis-opened-event #:session-id "s"
                                          #:turn-id "t"
                                          #:id "h1"
                                          #:question "Why does X fail?"))
      (check-true (mas-hypothesis-opened-event? evt))
      (check-equal? (mas-hypothesis-opened-event-id evt) "h1")
      (check-equal? (mas-hypothesis-opened-event-question evt) "Why does X fail?"))

    (test-case "mas-hypothesis-opened-event has correct type string"
      (define evt
        (make-mas-hypothesis-opened-event #:session-id "s" #:turn-id "t" #:id "h" #:question "q"))
      (check-equal? (typed-event-type evt) "mas.hypothesis.opened"))

    (test-case "mas-hypothesis-opened-event optional fields default"
      (define evt
        (make-mas-hypothesis-opened-event #:session-id "s" #:turn-id "t" #:id "h" #:question "q"))
      (check-false (mas-hypothesis-opened-event-agent-name evt))
      (check-equal? (mas-hypothesis-opened-event-priority evt) 'normal))

    (test-case "mas-hypothesis-opened-event accepts optional fields"
      (define evt
        (make-mas-hypothesis-opened-event #:session-id "s"
                                          #:turn-id "t"
                                          #:id "h"
                                          #:question "q"
                                          #:agent-name "explorer"
                                          #:priority 'high))
      (check-equal? (mas-hypothesis-opened-event-agent-name evt) "explorer")
      (check-equal? (mas-hypothesis-opened-event-priority evt) 'high))

    ;; ── mas-hypothesis-resolved-event ──

    (test-case "mas-hypothesis-resolved-event constructs with required field"
      (define evt (make-mas-hypothesis-resolved-event #:session-id "s" #:turn-id "t" #:id "h1"))
      (check-true (mas-hypothesis-resolved-event? evt))
      (check-equal? (mas-hypothesis-resolved-event-id evt) "h1"))

    (test-case "mas-hypothesis-resolved-event has correct type string"
      (define evt (make-mas-hypothesis-resolved-event #:session-id "s" #:turn-id "t" #:id "h"))
      (check-equal? (typed-event-type evt) "mas.hypothesis.resolved"))

    (test-case "mas-hypothesis-resolved-event optional fields default to #f"
      (define evt (make-mas-hypothesis-resolved-event #:session-id "s" #:turn-id "t" #:id "h"))
      (check-false (mas-hypothesis-resolved-event-resolution evt))
      (check-false (mas-hypothesis-resolved-event-resolved-by evt)))

    (test-case "mas-hypothesis-resolved-event accepts optional fields"
      (define evt
        (make-mas-hypothesis-resolved-event #:session-id "s"
                                            #:turn-id "t"
                                            #:id "h"
                                            #:resolution "confirmed"
                                            #:resolved-by "verifier"))
      (check-equal? (mas-hypothesis-resolved-event-resolution evt) "confirmed")
      (check-equal? (mas-hypothesis-resolved-event-resolved-by evt) "verifier"))

    ;; ── mas-blackboard-sync-event ──

    (test-case "mas-blackboard-sync-event constructs with required field"
      (define snapshot (hasheq 'waves 3 'active-plan #t))
      (define evt
        (make-mas-blackboard-sync-event #:session-id "s" #:turn-id "t" #:state-snapshot snapshot))
      (check-true (mas-blackboard-sync-event? evt))
      (check-equal? (mas-blackboard-sync-event-state-snapshot evt) snapshot))

    (test-case "mas-blackboard-sync-event has correct type string"
      (define evt
        (make-mas-blackboard-sync-event #:session-id "s" #:turn-id "t" #:state-snapshot (hasheq)))
      (check-equal? (typed-event-type evt) "mas.blackboard.sync"))

    (test-case "mas-blackboard-sync-event optional source-agent defaults to #f"
      (define evt
        (make-mas-blackboard-sync-event #:session-id "s" #:turn-id "t" #:state-snapshot (hasheq)))
      (check-false (mas-blackboard-sync-event-source-agent evt)))

    (test-case "mas-blackboard-sync-event accepts optional source-agent"
      (define evt
        (make-mas-blackboard-sync-event #:session-id "s"
                                        #:turn-id "t"
                                        #:state-snapshot (hasheq)
                                        #:source-agent "supervisor"))
      (check-equal? (mas-blackboard-sync-event-source-agent evt) "supervisor"))

    ;; ── Inheritance from typed-event ──

    (test-case "all MAS events inherit typed-event"
      (define artifact
        (make-mas-artifact-produced-event #:session-id "s"
                                          #:turn-id "t"
                                          #:name "n"
                                          #:path "p"
                                          #:artifact-type 'source))
      (define test-res
        (make-mas-test-result-event #:session-id "s" #:turn-id "t" #:file "f" #:result 'pass))
      (define hyp-opened
        (make-mas-hypothesis-opened-event #:session-id "s" #:turn-id "t" #:id "h" #:question "q"))
      (define hyp-resolved
        (make-mas-hypothesis-resolved-event #:session-id "s" #:turn-id "t" #:id "h"))
      (define sync
        (make-mas-blackboard-sync-event #:session-id "s" #:turn-id "t" #:state-snapshot (hasheq)))
      (check-true (typed-event? artifact))
      (check-true (typed-event? test-res))
      (check-true (typed-event? hyp-opened))
      (check-true (typed-event? hyp-resolved))
      (check-true (typed-event? sync)))

    (test-case "MAS events carry session-id and turn-id"
      (define evt
        (make-mas-test-result-event #:session-id "my-session"
                                    #:turn-id "turn-5"
                                    #:file "f.rkt"
                                    #:result 'pass))
      (check-equal? (typed-event-session-id evt) "my-session")
      (check-equal? (typed-event-turn-id evt) "turn-5"))

    (test-case "MAS events have timestamp"
      (define evt
        (make-mas-artifact-produced-event #:session-id "s"
                                          #:turn-id "t"
                                          #:name "n"
                                          #:path "p"
                                          #:artifact-type 'source))
      (check-true (real? (typed-event-timestamp evt))))

    (test-case "MAS events support custom timestamp"
      (define evt
        (make-mas-hypothesis-resolved-event #:session-id "s"
                                            #:turn-id "t"
                                            #:id "h"
                                            #:timestamp 12345))
      (check-equal? (typed-event-timestamp evt) 12345))

    ;; ── Transparency ──

    (test-case "MAS events are transparent (equal? works)"
      (define e1
        (make-mas-test-result-event #:session-id "s"
                                    #:turn-id "t"
                                    #:file "f.rkt"
                                    #:result 'pass
                                    #:timestamp 100))
      (define e2
        (make-mas-test-result-event #:session-id "s"
                                    #:turn-id "t"
                                    #:file "f.rkt"
                                    #:result 'pass
                                    #:timestamp 100))
      (check-equal? e1 e2))

    (test-case "MAS events with different values are not equal"
      (define e1
        (make-mas-test-result-event #:session-id "s"
                                    #:turn-id "t"
                                    #:file "f.rkt"
                                    #:result 'pass
                                    #:timestamp 100))
      (define e2
        (make-mas-test-result-event #:session-id "s"
                                    #:turn-id "t"
                                    #:file "f.rkt"
                                    #:result 'fail
                                    #:timestamp 100))
      (check-not-equal? e1 e2))

    ;; ── Fields Constants ──

    (test-case "all event field lists are correct"
      (check-equal? mas-artifact-produced-event-fields '(name path artifact-type agent-name))
      (check-equal? mas-test-result-event-fields '(file result duration-ms test-count pass-count))
      (check-equal? mas-hypothesis-opened-event-fields '(id question agent-name priority))
      (check-equal? mas-hypothesis-resolved-event-fields '(id resolution resolved-by))
      (check-equal? mas-blackboard-sync-event-fields '(state-snapshot source-agent)))))

(run-tests suite)
