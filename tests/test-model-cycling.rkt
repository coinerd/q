#lang racket

;; tests/test-model-cycling.rkt — FEAT-65: runtime model cycling

(require rackunit
         "../runtime/agent-session.rkt"
         "../runtime/model-registry.rkt"
         "../agent/event-bus.rkt"
         "../tools/tool.rkt")

(define (make-test-session [model #f])
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (make-agent-session (hasheq 'provider
                              (make-hash)
                              'tool-registry
                              reg
                              'event-bus
                              bus
                              'session-dir
                              "/tmp"
                              'model-name
                              model)))

(define (make-test-model-registry . model-names)
  (define entries
    (for/list ([name (in-list model-names)])
      (model-entry name "test-provider" (hasheq 'base-url "http://localhost"))))
  (model-registry (for/hash ([e (in-list entries)])
                    (values (model-entry-name e) (list e)))
                  (hasheq "test-provider" (hasheq 'base-url "http://localhost"))
                  #f
                  #f))

;; ============================================================
;; set-model!
;; ============================================================

(test-case "set-model! switches the active model"
  (define sess (make-test-session "gpt-4"))
  (check-equal? (agent-session-model-name sess) "gpt-4")
  (set-model! sess "claude-3")
  (check-equal? (agent-session-model-name sess) "claude-3"))

(test-case "set-model! raises on non-string"
  (define sess (make-test-session))
  (check-exn exn:fail? (lambda () (set-model! sess 42))))

;; ============================================================
;; cycle-model!
;; ============================================================

(test-case "cycle-model! advances to next model"
  (define sess (make-test-session "a"))
  (define reg (make-test-model-registry "a" "b" "c"))
  (define result (cycle-model! sess reg))
  (check-equal? result "b")
  (check-equal? (agent-session-model-name sess) "b"))

(test-case "cycle-model! wraps around"
  (define sess (make-test-session "c"))
  (define reg (make-test-model-registry "a" "b" "c"))
  (define result (cycle-model! sess reg))
  (check-equal? result "a")
  (check-equal? (agent-session-model-name sess) "a"))

(test-case "cycle-model! starts at first if no current model"
  (define sess (make-test-session #f))
  (define reg (make-test-model-registry "x" "y"))
  (define result (cycle-model! sess reg))
  (check-equal? result "x"))

(test-case "cycle-model! returns #f for empty registry"
  (define sess (make-test-session "a"))
  (define reg (make-test-model-registry))
  (check-false (cycle-model! sess reg)))
