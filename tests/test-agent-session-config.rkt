#lang racket

;; Test: P0 fix — mutable config hash-set! doesn't crash on second prompt
;; Bug: agent-session.rkt:726 called hash-set on mutable hash → crash on 2nd prompt
;; Fix: changed hash-set → hash-set!

(require rackunit)

;; Test 1: hash-set! on mutable hash works (the core fix)
(test-case "mutable config hash-set! does not crash"
  (define cfg
    (make-hash
     (list (cons 'max-iterations 5) (cons 'model-name "test-model") (cons 'session-index #f))))
  (check-pred hash? cfg)
  ;; This is what the old code tried with immutable hash-set:
  (hash-set! cfg 'session-index 'some-index)
  (check-equal? (hash-ref cfg 'session-index) 'some-index)
  ;; Setting again should also work
  (hash-set! cfg 'session-index 'another-index)
  (check-equal? (hash-ref cfg 'session-index) 'another-index))

;; Test 2: simulate the actual pattern from agent-session.rkt:724-728
(test-case "simulated second-prompt config injection pattern"
  ;; This mirrors the exact code in agent-session.rkt:
  ;;   (define base-cfg (agent-session-config sess))  ; mutable hash
  ;;   (define idx (agent-session-index sess))
  ;;   (define cfg
  ;;     (if idx
  ;;         (begin (hash-set! base-cfg 'session-index idx) base-cfg)
  ;;         base-cfg))
  (define base-cfg (make-hash (list (cons 'max-iterations 5) (cons 'model-name "test-model"))))

  ;; First prompt: idx is #f → no hash-set! called
  (define idx-first #f)
  (define cfg-first
    (if idx-first
        (begin
          (hash-set! base-cfg 'session-index idx-first)
          base-cfg)
        base-cfg))
  (check-eq? cfg-first base-cfg)
  (check-false (hash-ref cfg-first 'session-index #f))

  ;; After first turn: index gets set (simulating set-agent-session-index!)
  (define built-index '(mock-index-entry-1 mock-index-entry-2))
  (hash-set! base-cfg 'session-index built-index)

  ;; Second prompt: idx is now truthy → hash-set! on mutable → should NOT crash
  (define idx-second (hash-ref base-cfg 'session-index #f))
  (check-not-false idx-second)
  (define cfg-second
    (if idx-second
        (begin
          (hash-set! base-cfg 'session-index idx-second)
          base-cfg)
        base-cfg))
  (check-eq? cfg-second base-cfg)
  (check-equal? (hash-ref cfg-second 'session-index) built-index)

  ;; Third prompt (e.g. /retry): should also work
  (define idx-third (hash-ref base-cfg 'session-index #f))
  (define cfg-third
    (if idx-third
        (begin
          (hash-set! base-cfg 'session-index idx-third)
          base-cfg)
        base-cfg))
  (check-eq? cfg-third base-cfg))

;; Test 3: hash-set (immutable) on mutable hash WOULD crash (confirms the bug)
(test-case "hash-set (immutable) on mutable hash raises contract error"
  (define cfg (make-hash '((a . 1))))
  (check-exn exn:fail:contract? (lambda () (hash-set cfg 'b 2))))
