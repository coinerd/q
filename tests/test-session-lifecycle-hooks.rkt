#lang racket

;; BOUNDARY: integration

;;; tests/test-session-lifecycle-hooks.rkt — tests for session lifecycle hooks (#764)

(require rackunit
         rackunit/text-ui
         racket/file
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../extensions/api.rkt"
         "../extensions/loader.rkt"
         (only-in "../runtime/context-assembly.rkt"
                  make-context-assembly-config
                  context-assembly-config?
                  context-assembly-config-recent-tokens)
         (only-in "../runtime/runtime-helpers.rkt" maybe-dispatch-hooks))

(define (make-test-ext hook-name hooks-box)
  (extension (format "lifecycle-test-~a" hook-name)
             "1.0"
             "1"
             (hash hook-name
                   (lambda (payload)
                     (set-box! hooks-box
                               (cons (list hook-name (hash-ref payload 'reason #f))
                                     (unbox hooks-box)))))))

(test-case "session-start hook fires with reason new"
  (define ext-reg (make-extension-registry))
  (define hooks-fired (box '()))
  (register-extension! ext-reg (make-test-ext 'session-start hooks-fired))
  (maybe-dispatch-hooks ext-reg 'session-start (hasheq 'session-id "s1" 'reason 'new))
  (check-equal? (unbox hooks-fired) (list (list 'session-start 'new))))

(test-case "session-start hook fires with reason resume"
  (define ext-reg (make-extension-registry))
  (define hooks-fired (box '()))
  (register-extension! ext-reg (make-test-ext 'session-start hooks-fired))
  (maybe-dispatch-hooks ext-reg 'session-start (hasheq 'session-id "s2" 'reason 'resume))
  (check-equal? (unbox hooks-fired) (list (list 'session-start 'resume))))

(test-case "session-start hook fires with reason fork"
  (define ext-reg (make-extension-registry))
  (define hooks-fired (box '()))
  (register-extension! ext-reg (make-test-ext 'session-start hooks-fired))
  (maybe-dispatch-hooks ext-reg
                        'session-start
                        (hasheq 'session-id "s3" 'reason 'fork 'parent-session-id "parent"))
  (check-equal? (unbox hooks-fired) (list (list 'session-start 'fork))))

(test-case "session-shutdown hook fires"
  (define ext-reg (make-extension-registry))
  (define hooks-fired (box '()))
  (register-extension! ext-reg (make-test-ext 'session-shutdown hooks-fired))
  (maybe-dispatch-hooks ext-reg 'session-shutdown (hasheq 'session-id "s1" 'duration 42))
  (check-equal? (length (unbox hooks-fired)) 1))

(test-case "session-before-switch hook is observable"
  (define ext-reg (make-extension-registry))
  (define hooks-fired (box '()))
  (define sw-ext
    (extension "switch-test"
               "1.0"
               "1"
               (hash 'session-before-switch
                     (lambda (payload)
                       (set-box! hooks-fired
                                 (cons (hash-ref payload 'operation #f) (unbox hooks-fired)))))))
  (register-extension! ext-reg sw-ext)
  (maybe-dispatch-hooks ext-reg 'session-before-switch (hasheq 'session-id "s2" 'operation 'resume))
  (check-equal? (unbox hooks-fired) (list 'resume)))

(test-case "session-before-fork hook is observable"
  (define ext-reg (make-extension-registry))
  (define hooks-fired (box '()))
  (define fork-ext
    (extension "fork-test"
               "1.0"
               "1"
               (hash 'session-before-fork
                     (lambda (payload)
                       (set-box! hooks-fired
                                 (cons (hash-ref payload 'session-id #f) (unbox hooks-fired)))))))
  (register-extension! ext-reg fork-ext)
  (maybe-dispatch-hooks ext-reg
                        'session-before-fork
                        (hasheq 'session-id "parent-1" 'reason "user-fork"))
  (check-equal? (unbox hooks-fired) (list "parent-1")))

(test-case "integration: context assembly config valid during lifecycle"
  ;; Verify config validation works and lifecycle events can fire in sequence
  (define cfg
    (make-context-assembly-config #:recent-tokens 8000
                                  #:max-catalog-entries 10
                                  #:max-catalog-tokens 500
                                  #:summary-window 2000))
  (check-true (context-assembly-config? cfg))
  (check-equal? (context-assembly-config-recent-tokens cfg) 8000)
  ;; Simulate lifecycle sequence: start → shutdown
  (define ext-reg (make-extension-registry))
  (define events (box '()))
  (register-extension!
   ext-reg
   (extension
    "lifecycle-integration"
    "1.0"
    "1"
    (hash 'session-start
          (lambda (p) (set-box! events (cons (list 'start (hash-ref p 'reason #f)) (unbox events))))
          'session-shutdown
          (lambda (p)
            (set-box! events (cons (list 'shutdown (hash-ref p 'session-id #f)) (unbox events)))))))
  (maybe-dispatch-hooks ext-reg 'session-start (hasheq 'session-id "s1" 'reason 'new))
  (maybe-dispatch-hooks ext-reg 'session-shutdown (hasheq 'session-id "s1" 'duration 5))
  (check-equal? (reverse (unbox events)) (list (list 'start 'new) (list 'shutdown "s1"))))
