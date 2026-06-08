#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/helpers/session-fixture.rkt — Session fixture builders for tests
;;
;; Provides convenient session construction functions for tests.
;; Wraps the real agent-session creation with sensible defaults.

(require "../../runtime/agent-session.rkt"
         "../../runtime/session/session-config.rkt"
         "../../agent/event-bus.rkt"
         (only-in "../../tools/tool.rkt" make-tool-registry)
         (only-in "../../llm/provider.rkt" make-provider)
         (only-in "../../llm/model.rkt" make-model-response make-stream-chunk))

(provide make-test-session
         make-test-session-config)

;; make-test-session-config : -> session-config?
;;  Creates a session-config with a temp dir, mock provider, and fresh event bus.
(define (make-test-session-config #:dir [dir "/tmp/q-test"]
                                  #:provider [prov #f]
                                  #:tool-registry [reg #f]
                                  #:event-bus [bus #f]
                                  #:model [model "test-model"])
  (hash->session-config (hash 'provider
                              (or prov (make-dummy-provider))
                              'tool-registry
                              (or reg (make-tool-registry))
                              'event-bus
                              (or bus (make-event-bus))
                              'session-dir
                              dir
                              'model
                              model)))

;; make-test-session : -> agent-session?
;;  Creates a full agent-session with sensible test defaults.
(define (make-test-session #:dir [dir "/tmp/q-test"]
                           #:provider [prov #f]
                           #:tool-registry [reg #f]
                           #:event-bus [bus #f]
                           #:model [model "test-model"])
  (make-agent-session (make-test-session-config #:dir dir
                                                #:provider prov
                                                #:tool-registry reg
                                                #:event-bus bus
                                                #:model model)))

;; Internal: minimal provider that returns empty responses
(define (make-dummy-provider)
  (make-provider
   (lambda () "dummy-test-provider")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (make-model-response '()
                          (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                          "test-model"
                          'stop))
   (lambda (req)
     (list
      (make-stream-chunk #f #f (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0) #t)))))
