#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
;; tests/test-outcome-capture.rkt
;; W3B (#8940): Runtime capture — typed-tool-outcome → task-ledger-event.
;;
;; Tests that a typed-tool-outcome can be translated into a durable,
;; append-only task-ledger-event with correct provenance, kind mapping,
;; and payload safety (no raw content leaks).

(require rackunit
         rackunit/text-ui
         racket/list
         file/sha1
         "../util/outcome/outcome-types.rkt"
         "../runtime/task-memory/types.rkt"
         "../runtime/task-memory/outcome-capture.rkt")

(define ctx
  (make-capture-context #:session-id "sess-1"
                        #:project-id "proj-1"
                        #:task-id "task-1"
                        #:parent-task-id #f
                        #:branch-id "branch-1"
                        #:turn-id "turn-1"
                        #:request-id "req-1"
                        #:assembly-id "asm-1"
                        #:session-seq 5))

(define-test-suite
 outcome-capture-suite
 ;; ── capture-context ──
 (test-case "capture-context has all fields"
   (check-equal? (capture-context-session-id ctx) "sess-1")
   (check-equal? (capture-context-project-id ctx) "proj-1")
   (check-equal? (capture-context-task-id ctx) "task-1")
   (check-equal? (capture-context-parent-task-id ctx) #f)
   (check-equal? (capture-context-branch-id ctx) "branch-1")
   (check-equal? (capture-context-turn-id ctx) "turn-1")
   (check-equal? (capture-context-request-id ctx) "req-1")
   (check-equal? (capture-context-assembly-id ctx) "asm-1")
   (check-equal? (capture-context-session-seq ctx) 5))
 (test-case "capture-context with parent-task-id"
   (define c2
     (make-capture-context #:session-id "s"
                           #:project-id "p"
                           #:task-id "t"
                           #:parent-task-id "parent-t"
                           #:branch-id "b"
                           #:turn-id "tu"
                           #:request-id "r"
                           #:assembly-id "a"
                           #:session-seq 1))
   (check-equal? (capture-context-parent-task-id c2) "parent-t"))
 ;; ── Kind mapping ──
 (test-case "artifact success → artifact-modified"
   (define o
     (make-typed-tool-outcome #:kind 'artifact
                              #:tool-call-id "c1"
                              #:tool-name "edit"
                              #:status 'success
                              #:payload (hash 'path "foo.rkt")
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'artifact-modified))
 (test-case "test success → verification-passed"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c2"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'verification-passed))
 (test-case "test error → verification-failed"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c3"
                              #:tool-name "bash"
                              #:status 'error
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'verification-failed))
 (test-case "commit success → commit-created"
   (define o
     (make-typed-tool-outcome #:kind 'commit
                              #:tool-call-id "c4"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'commit-created))
 (test-case "push success → push-completed"
   (define o
     (make-typed-tool-outcome #:kind 'push
                              #:tool-call-id "c5"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'push-completed))
 (test-case "git-status success → tool-invoked"
   (define o
     (make-typed-tool-outcome #:kind 'git-status
                              #:tool-call-id "c6"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'tool-invoked))
 (test-case "conclusion → objective-set"
   (define o
     (make-typed-tool-outcome #:kind 'conclusion
                              #:tool-call-id "c7"
                              #:tool-name "record_conclusion"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'objective-set))
 (test-case "any kind with error status → error-occurred"
   (define o
     (make-typed-tool-outcome #:kind 'artifact
                              #:tool-call-id "c8"
                              #:tool-name "edit"
                              #:status 'error
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-event-kind e) 'error-occurred))
 ;; ── Provenance: correlation-id comes from tool-call-id ──
 (test-case "correlation-id is the tool-call-id"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "call-xyz"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-correlation-id e) "call-xyz"))
 (test-case "source-class is runtime-observed"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-source-class e) 'runtime-observed))
 (test-case "session-seq is propagated from context"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-session-seq e) 5))
 (test-case "event-id is generated and non-empty"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-true (and (string? (task-ledger-event-event-id e))
                    (positive? (string-length (task-ledger-event-event-id e))))))
 ;; ── Payload safety: safe hash is copied through ──
 (test-case "payload is propagated to event payload"
   (define o
     (make-typed-tool-outcome #:kind 'artifact
                              #:tool-call-id "c"
                              #:tool-name "edit"
                              #:status 'success
                              #:payload (hash 'path "foo.rkt" 'command-class 'raco-test)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (define pl (task-ledger-event-payload e))
   (check-equal? (hash-ref pl 'path) "foo.rkt"))
 ;; ── content-digest is computed and non-trivial ──
 (test-case "content-digest is a non-empty hex string"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash 'exit-code 0)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (define digest (task-ledger-event-content-digest e))
   (check-true (and (string? digest) (>= (string-length digest) 20))
               "digest should be a substantial hex string"))
 (test-case "different payloads produce different digests"
   (define o1
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash 'exit-code 0)
                              #:timestamp #f))
   (define o2
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash 'exit-code 1)
                              #:timestamp #f))
   (define e1 (capture-outcome->event ctx o1))
   (define e2 (capture-outcome->event ctx o2))
   (check-not-equal? (task-ledger-event-content-digest e1) (task-ledger-event-content-digest e2)))
 ;; ── Timestamp propagation ──
 (test-case "outcome timestamp is propagated"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp 1234567890))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-timestamp e) 1234567890))
 (test-case "outcome with #f timestamp gets current time"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define before (current-seconds))
   (define e (capture-outcome->event ctx o))
   (define after (current-seconds))
   (check-true (<= before (task-ledger-event-timestamp e) after)))
 ;; ── evidence-refs defaults to empty ──
 (test-case "evidence-refs is empty list by default"
   (define o
     (make-typed-tool-outcome #:kind 'test
                              #:tool-call-id "c"
                              #:tool-name "bash"
                              #:status 'success
                              #:payload (hash)
                              #:timestamp #f))
   (define e (capture-outcome->event ctx o))
   (check-equal? (task-ledger-event-evidence-refs e) '()))
 ;; ── capture-outcomes->events: batch ──
 (test-case "capture-outcomes->events translates a list"
   (define outcomes
     (list (make-typed-tool-outcome #:kind 'artifact
                                    #:tool-call-id "c1"
                                    #:tool-name "edit"
                                    #:status 'success
                                    #:payload (hash 'path "a.rkt")
                                    #:timestamp #f)
           (make-typed-tool-outcome #:kind 'test
                                    #:tool-call-id "c2"
                                    #:tool-name "bash"
                                    #:status 'success
                                    #:payload (hash)
                                    #:timestamp #f)))
   (define events (capture-outcomes->events ctx outcomes))
   (check-equal? (length events) 2)
   (check-equal? (task-ledger-event-event-kind (first events)) 'artifact-modified)
   (check-equal? (task-ledger-event-event-kind (second events)) 'verification-passed)))

;; ── compute-payload-digest ──

(test-case "compute-payload-digest returns hex string"
  (define d (compute-payload-digest (hash 'a 1 'b "two")))
  (check-true (and (string? d) (positive? (string-length d)))))

(test-case "compute-payload-digest is deterministic"
  (define d1 (compute-payload-digest (hash 'a 1)))
  (define d2 (compute-payload-digest (hash 'a 1)))
  (check-equal? d1 d2))

(run-tests outcome-capture-suite)
