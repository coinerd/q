#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
;; tests/test-context-selection-authority.rkt — W7 (#8944): Context selection authority tests
;;
;; Tests pure decision helpers: decide-retention, select-memory, include-checkpoint,
;; and the full selection-authority orchestration.
;;
;; Uses manually constructed data — no I/O, no backends.

(require rackunit
         rackunit/text-ui
         racket/match
         racket/list
         racket/string
         "../runtime/memory/types.rkt"
         "../runtime/context-assembly/task-checkpoint.rkt"
         "../runtime/context-selection/authority.rkt"
         "../runtime/task-memory/projection.rkt"
         (only-in "../util/message/message.rkt" message? message-role message-kind message-content)
         (only-in "../util/content/content-parts.rkt" text-part? text-part-text))

;; ============================================================
;; Helpers
;; ============================================================

;; Minimal message for testing
(define (make-msg kind role text)
  (let ([id (format "msg-~a" (random 100000))]) (hasheq 'id id 'kind kind 'role role 'text text)))

;; Minimal memory-item for testing
(define (make-mem-item id
                       content
                       #:scope [scope 'session]
                       #:type [type 'episodic]
                       #:expires-at [expires #f])
  (memory-item
   id
   type
   scope
   content
   (hasheq 'project-root "/p" 'session-id "s1" 'tags '() 'source "test" 'origin-message-id "m1")
   (if expires
       (hasheq 'sensitivity 'low 'confidence 0.5 'supersedes #f 'expires-at expires)
       (hasheq 'sensitivity 'low 'confidence 0.5 'supersedes #f))
   "2025-06-15T10:00:00Z"
   "2025-06-15T10:00:00Z"))

;; Minimal task checkpoint for testing
(define (make-checkpoint #:objective [obj "Build feature X"]
                         #:phase [phase "implementation"]
                         #:path [path "/p/src/foo.rkt"])
  (active-task-checkpoint obj ; objective (string)
                          '() ; constraints
                          phase ; current-phase
                          #f ; gsd-wave
                          (list path) ; owned-paths
                          (list (work-evidence 'edit "Implemented foo" "w1")) ; completed-work
                          'pending ; verification-state
                          #f ; workspace-revision
                          '() ; blockers
                          "Write tests for foo" ; next-action
                          #f ; next-trigger
                          0)) ; event-count

;; ============================================================
;; Test suite
;; ============================================================

(define-test-suite
 context-selection-authority-suite
 ;; ── decide-retention ──
 (test-case "decide-retention: small session keeps all in tier-a"
   (define d (decide-retention 10 #f '()))
   (check-equal? (retention-decision-tier-a-count d) 10)
   (check-equal? (retention-decision-tier-b-count d) 0)
   (check-equal? (retention-decision-tier-c-count d) 0)
   (check-false (retention-decision-compacted? d)))
 (test-case "decide-retention: normal session uses b/c tiers"
   (define d (decide-retention 100 #f '()))
   (check-true (> (retention-decision-tier-b-count d) 0))
   (check-true (> (retention-decision-tier-c-count d) 0)))
 (test-case "decide-retention: large session uses full profile"
   (define d (decide-retention 300 #f '()))
   (check-true (> (retention-decision-tier-a-count d) 10))
   (check-true (> (retention-decision-tier-b-count d) 20))
   (check-true (> (retention-decision-tier-c-count d) 4)))
 (test-case "decide-retention: compacted session uses compact profile"
   (define d (decide-retention 100 #t (make-list 20 #f)))
   (check-true (retention-decision-compacted? d))
   (check-equal? (retention-decision-tier-b-count d) 0)
   (check-equal? (retention-decision-tier-c-count d) 0))
 (test-case "decide-retention: compact profile remembers recent count"
   (define recent (make-list 15 #f))
   (define d (decide-retention 100 #t recent))
   (check-equal? (retention-decision-tier-a-count d) 15))
 (test-case "decide-retention: profile override works"
   (define d (decide-retention 10 #f '() #:preferred-profile 'full))
   (check-equal? (retention-decision-tier-a-count d) 20)
   (check-equal? (retention-decision-tier-b-count d) 50))
 (test-case "decide-retention: profile override compact works"
   (define d (decide-retention 100 #f '() #:preferred-profile 'compact))
   (check-equal? (retention-decision-tier-b-count d) 0)
   (check-equal? (retention-decision-tier-c-count d) 0))
 (test-case "decide-retention: reasoning is non-empty"
   (define d (decide-retention 50 #f '()))
   (check-true (> (length (retention-decision-reasoning d)) 0)))
 ;; ── select-memory ──
 (test-case "select-memory: empty items yields empty result"
   (define s (select-memory '() '() #:query-text "hello" #:limit 5))
   (check-equal? (length (memory-selection-items s)) 0)
   (check-equal? (memory-selection-session-count s) 0)
   (check-equal? (memory-selection-project-count s) 0)
   (check-equal? (memory-selection-expired-filtered s) 0))
 (test-case "select-memory: session items return in results"
   (define items (list (make-mem-item "s1" "session memory" #:scope 'session)))
   (define s (select-memory items '() #:query-text "session memory" #:limit 5))
   (check-equal? (length (memory-selection-items s)) 1)
   (check-equal? (memory-selection-session-count s) 1)
   (check-equal? (memory-selection-project-count s) 0))
 (test-case "select-memory: project items return in results"
   (define items (list (make-mem-item "p1" "project fact" #:scope 'project)))
   (define s (select-memory '() items #:query-text "project fact" #:limit 5))
   (check-equal? (length (memory-selection-items s)) 1)
   (check-equal? (memory-selection-project-count s) 1))
 (test-case "select-memory: hybrid recall blends both sources"
   (define s-items (list (make-mem-item "s1" "alpha" #:scope 'session)))
   (define p-items (list (make-mem-item "p1" "beta" #:scope 'project)))
   (define s (select-memory s-items p-items #:query-text "alpha" #:limit 5))
   (check-equal? (length (memory-selection-items s)) 2)
   (check-equal? (memory-selection-session-count s) 1)
   (check-equal? (memory-selection-project-count s) 1))
 (test-case "select-memory: expired items are filtered"
   (define expired
     (make-mem-item "e1" "old fact" #:expires-at "2024-01-01T00:00:00Z" #:scope 'project))
   (define valid (make-mem-item "v1" "current fact" #:scope 'project))
   (define s (select-memory '() (list expired valid) #:query-text "fact" #:limit 5))
   (check-equal? (length (memory-selection-items s)) 1)
   (check-equal? (memory-selection-expired-filtered s) 1)
   (check-equal? (memory-item-id (car (memory-selection-items s))) "v1"))
 (test-case "select-memory: respects limit"
   (define items
     (for/list ([i (in-range 20)])
       (make-mem-item (format "i~a" i) (format "item ~a" i) #:scope 'session)))
   (define s (select-memory items '() #:query-text "item" #:limit 3))
   (check-equal? (length (memory-selection-items s)) 3))
 ;; ── include-checkpoint ──
 (test-case "include-checkpoint: returns #f for #f input"
   (check-false (include-checkpoint #f)))
 (test-case "include-checkpoint: returns message for valid checkpoint"
   (define cp (make-checkpoint))
   (define msg (include-checkpoint cp))
   (check-true (message? msg))
   (check-equal? (message-role msg) 'system)
   (check-equal? (message-kind msg) 'system-instruction))
 (test-case "include-checkpoint: respects token budget"
   (define cp (make-checkpoint))
   (define msg (include-checkpoint cp #:token-budget 50))
   (check-true (message? msg))
   ;; 50 tokens * 4 chars/token = 200 chars max including header
   (define content (message-content msg))
   (define first (car content))
   (check-true (<= (string-length (text-part-text first)) 400))) ;; 100 tokens * 4 chars
 ;; ── selection-authority ──
 (test-case "selection-authority: empty state produces valid result"
   (define sel (selection-authority 10 #f '() #f '() '() "" '()))
   (check-true (authority-selection? sel))
   (define result (authority-selection-result sel))
   (check-true (selection-result? result))
   (check-equal? (selection-result-memory-count result) 0)
   (check-false (selection-result-checkpoint-included? result)))
 (test-case "selection-authority: includes memory when available"
   (define items (list (make-mem-item "s1" "hello" #:scope 'session)))
   (define sel (selection-authority 10 #f '() #f items '() "hello" '()))
   (define result (authority-selection-result sel))
   (check-true (> (selection-result-memory-count result) 0)))
 (test-case "selection-authority: includes checkpoint when given"
   (define cp (make-checkpoint))
   (define sel (selection-authority 10 #f '() cp '() '() "" '()))
   (define result (authority-selection-result sel))
   (check-true (selection-result-checkpoint-included? result)))
 (test-case "selection-authority: trace is populated"
   (define sel (selection-authority 10 #f '() #f '() '() "" '()))
   (check-true (> (length (authority-selection-trace sel)) 0))
   (check-true (string? (car (authority-selection-trace sel)))))
 (test-case "selection-authority: reasoning in result is populated"
   (define sel (selection-authority 10 #f '() #f '() '() "" '()))
   (define result (authority-selection-result sel))
   (check-true (> (length (selection-result-reasoning result)) 0)))
 ;; ── package-context ──
 (test-case "package-context: creates package with all parts"
   (define cp (make-checkpoint))
   (define cp-msg (include-checkpoint cp))
   (define pkg (package-context #f cp-msg '() '()))
   (check-true (context-package? pkg))
   (check-false (context-package-tiered-context pkg))
   (check-not-false (context-package-checkpoint-message pkg))
   (check-equal? (length (context-package-memory-messages pkg)) 0))
 (test-case "package-context: memory messages are included"
   (define mem-msgs (list (hasheq 'role 'system 'content "[Memory] test")))
   (define pkg (package-context #f #f mem-msgs '()))
   (check-equal? (length (context-package-memory-messages pkg)) 1)))

;; ============================================================
;; Run
;; ============================================================

(run-tests context-selection-authority-suite)
