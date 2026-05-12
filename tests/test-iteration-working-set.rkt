#lang racket

;; tests/test-iteration-working-set.rkt — Working set integration into iteration loop

(require rackunit
         rackunit/text-ui
         racket/list
         "../runtime/working-set.rkt"
         (only-in "../runtime/iteration/main-loop.rkt" run-iteration-loop)
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../util/ids.rkt"
         (only-in "../llm/provider.rkt" make-mock-provider)
         (only-in "../llm/model.rkt" make-model-response))

;; Helper: create a user message
(define (make-user-msg text)
  (make-message (generate-id) #f 'user 'user (list (make-text-part text)) (current-seconds) (hasheq)))

;; Helper: create a tool result message
(define (make-tool-msg id text)
  (make-message id #f 'tool 'tool (list (make-text-part text)) (current-seconds) (hasheq)))

;; Helper: estimate tokens from a message (same logic as in iteration.rkt)
(define (estimate-msg-tokens m)
  (define c (message-content m))
  (define t
    (cond
      [(string? c) c]
      [(list? c)
       (apply string-append
              (for/list ([p (in-list c)]
                         #:when (text-part? p))
                (text-part-text p)))]
      [else ""]))
  (string-length t))

;; Helper: create a tool call hash
(define (make-tool name [path ""])
  (hasheq 'name name 'arguments (hasheq 'path path)))

(define iteration-ws-tests
  (test-suite "Iteration Working Set Integration"

    ;; ── T01: Loop accepts #:working-set ──
    (test-case "T01: iteration loop accepts #:working-set parameter"
      (define bus (make-event-bus))
      (define ws (make-working-set))
      (define ctx (list (make-user-msg "test")))
      (define mock-prov
        (make-mock-provider
         (make-model-response (list (hasheq 'type "text" 'text "done")) (hash) "mock" #f)))
      (define result
        (run-iteration-loop ctx
                            mock-prov
                            bus
                            #f
                            #f
                            "/tmp/test.log"
                            "test-session"
                            10
                            #:working-set ws))
      (check-pred loop-result? result)
      (check-equal? (loop-result-termination-reason result) 'completed))

    ;; ── T02: Loop works without #:working-set ──
    (test-case "T02: iteration loop initializes working set when not provided"
      (define bus (make-event-bus))
      (define ctx (list (make-user-msg "test")))
      (define mock-prov
        (make-mock-provider
         (make-model-response (list (hasheq 'type "text" 'text "done")) (hash) "mock" #f)))
      (define result (run-iteration-loop ctx mock-prov bus #f #f "/tmp/test.log" "test-session" 10))
      (check-pred loop-result? result)
      (check-equal? (loop-result-termination-reason result) 'completed))

    ;; ── T03: read updates ws with real messages ──
    (test-case "T03: read tool call updates working set with real message structs"
      (define ws (make-working-set))
      (define tc (list (make-tool "read" "/tmp/foo.rkt")))
      (define rm (list (make-tool-msg "m1" "content of foo")))
      (working-set-update! ws tc rm message-id estimate-msg-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (define e (car (working-set-entries ws)))
      (check-equal? (ws-entry-path e) "/tmp/foo.rkt")
      (check-equal? (ws-entry-message-id e) "m1"))

    ;; ── T04: edit removes from ws with real messages ──
    (test-case "T04: edit tool call removes path from working set"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (make-tool-msg "m1" "content"))
                           message-id
                           estimate-msg-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (working-set-update! ws
                           (list (make-tool "edit" "/tmp/foo.rkt"))
                           (list (make-tool-msg "m2" "edited"))
                           message-id
                           estimate-msg-tokens)
      (check-equal? (working-set-entry-count ws) 0))

    ;; ── T05: config hash contains working set ──
    (test-case "T05: config hash contains working set after hash-set"
      (define ws (make-working-set))
      (define config (hasheq 'max-iterations 10))
      (define config-with-ws (hash-set config 'working-set ws))
      (check-true (hash-has-key? config-with-ws 'working-set))
      (check-eq? (hash-ref config-with-ws 'working-set) ws))))

(module+ main
  (run-tests iteration-ws-tests))

(module+ test
  (run-tests iteration-ws-tests))
