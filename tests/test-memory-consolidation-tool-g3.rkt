#lang racket/base

;;; test-memory-consolidation-tool-g3.rkt — W4: consolidate-memory tool tests

(require rackunit
         (only-in "../tools/tool.rkt" tool-execute tool? tool-result? tool-result-is-error?
                  tool-result-content tool-result-details)
         (only-in "../tools/builtins/memory-tools.rkt" consolidate-memory)
         (only-in "../runtime/memory/backends/memory-hash.rkt" make-memory-hash-backend)
         (only-in "../runtime/memory/types.rkt"
                  memory-item memory-item-content memory-item-id
                  memory-query memory-result-ok? memory-result-value)
         (only-in "../runtime/memory/protocol.rkt" gen:store-memory! gen:retrieve-memory)
         (only-in "../runtime/memory/service.rkt" current-memory-backend current-memory-policy)
         (only-in "../runtime/memory/policy.rkt" default-memory-policy))

(define (make-test-item id content [scope 'session])
  (memory-item id 'semantic scope content
               (hasheq 'source 'test 'session-id "s1"
                       'project-root "." 'tags '()
                       'origin-tool-call-id "tc1")
               (hasheq 'sensitivity 'public 'confidence 0.7 'supersedes '())
               "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))

(define (all-items backend)
  (define r (gen:retrieve-memory backend (memory-query "" #f #f #f #f #f 100 #t)))
  (if (memory-result-ok? r) (memory-result-value r) '()))

(test-case "W4: consolidate-memory tool exists and is registered"
  (check-true (tool? consolidate-memory)))

(test-case "W4: consolidate 2 items creates merged item with supersedes"
  (define backend (make-memory-hash-backend))
  (parameterize ([current-memory-backend backend]
                 [current-memory-policy default-memory-policy])
    (gen:store-memory! backend (make-test-item "m1" "content A"))
    (gen:store-memory! backend (make-test-item "m2" "content B"))
    (define handler (tool-execute consolidate-memory))
    (define result (handler (hasheq 'ids '("m1" "m2")
                                     'merged-content "merged AB"
                                     'scope "session"
                                     'session-id "s1"
                                     'project-root ".")))
    (check-false (tool-result-is-error? result))
    (define details (tool-result-details result))
    (check-not-false (hash-ref details 'merged-id #f))
    ;; 3 items total in backend, but retrieve shows only 1 (merged)
    ;; because post-retrieve-process removes superseded items
    (check-equal? (length (all-items backend)) 1)
    (define merged (car (all-items backend)))
    (check-equal? (memory-item-content merged) "merged AB")))

(test-case "W4: consolidate with keep-originals=false deletes originals"
  (define backend (make-memory-hash-backend))
  (parameterize ([current-memory-backend backend]
                 [current-memory-policy default-memory-policy])
    (gen:store-memory! backend (make-test-item "m3" "content C"))
    (gen:store-memory! backend (make-test-item "m4" "content D"))
    (define handler (tool-execute consolidate-memory))
    (define result (handler (hasheq 'ids '("m3" "m4")
                                     'merged-content "merged CD"
                                     'scope "session"
                                     'keep-originals? #f
                                     'session-id "s1"
                                     'project-root ".")))
    (check-false (tool-result-is-error? result))
    ;; Only 1 item: the merged one
    (check-equal? (length (all-items backend)) 1)))

(test-case "W4: consolidate with <2 IDs returns error"
  (define backend (make-memory-hash-backend))
  (parameterize ([current-memory-backend backend]
                 [current-memory-policy default-memory-policy])
    (define handler (tool-execute consolidate-memory))
    (define result (handler (hasheq 'ids '("m1") 'merged-content "x" 'scope "session" 'session-id "s1" 'project-root ".")))
    (check-true (tool-result-is-error? result))))

(test-case "W4: consolidate with missing IDs returns error"
  (define backend (make-memory-hash-backend))
  (parameterize ([current-memory-backend backend]
                 [current-memory-policy default-memory-policy])
    (gen:store-memory! backend (make-test-item "m5" "content E"))
    (define handler (tool-execute consolidate-memory))
    (define result (handler (hasheq 'ids '("m5" "nonexistent")
                                     'merged-content "merged"
                                     'scope "session"
                                     'session-id "s1"
                                     'project-root ".")))
    (check-true (tool-result-is-error? result))))

(test-case "W4: consolidate with empty content returns error"
  (define backend (make-memory-hash-backend))
  (parameterize ([current-memory-backend backend]
                 [current-memory-policy default-memory-policy])
    (define handler (tool-execute consolidate-memory))
    (define result (handler (hasheq 'ids '("a" "b") 'merged-content "" 'scope "session" 'session-id "s1" 'project-root ".")))
    (check-true (tool-result-is-error? result))))
