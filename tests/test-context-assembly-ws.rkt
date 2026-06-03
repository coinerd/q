#lang racket
;; BOUNDARY: integration

;; tests/test-context-assembly-ws.rkt — Working set integration into context assembly

(require rackunit
         rackunit/text-ui
         racket/list
         racket/file
         "../util/message/protocol-types.rkt"
         "../runtime/working-set.rkt"
         "../runtime/context/context-pinning.rkt"
         "../runtime/context/context-assembly.rkt"
         "../runtime/session/session-store.rkt"
         "../runtime/session-index.rkt")

;; Helper: create a test message
(define (make-test-msg id role kind text [parent #f])
  (make-message id parent role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-temp-dir)
  (make-temporary-file "q-ctx-ws-test-~a" 'directory))

(define ws-tests
  (test-suite "Context Assembly Working Set Integration"

    (test-case "T01: working set messages included in assembled context"
      (define ws (make-working-set))
      (define tool-msg (make-test-msg "tool-1" 'tool 'tool-result "content of /tmp/foo.rkt"))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/foo.rkt")))
                           (list (make-message "tool-1"
                                               #f
                                               'tool
                                               'tool-result
                                               (list (make-text-part "content of /tmp/foo.rkt"))
                                               (current-seconds)
                                               (hasheq)))
                           message-id
                           (lambda (m) 20))
      (define msgs
        (list (make-test-msg "sys" 'system 'system-instruction "System prompt")
              (make-test-msg "u1" 'user 'message "Hello")
              tool-msg))
      (define tc (build-tiered-context msgs #:working-set-messages (list tool-msg)))
      (define assembled (tiered-context->message-list tc))
      (check-not-false (member tool-msg assembled))
      (check-not-false (member tool-msg (tiered-context-tier-a tc))))

    (test-case "T02: working set messages survive pair-preserving truncation"
      (define dir (make-temp-dir))
      (define sp (build-path dir "session.jsonl"))
      (define ip (build-path dir "session.index"))
      (define ws (make-working-set))
      (define tool-msg
        (make-message "tool-1"
                      #f
                      'tool
                      'tool-result
                      (list (make-text-part "content of /tmp/foo.rkt"))
                      (current-seconds)
                      (hasheq)))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/foo.rkt")))
                           (list (make-message "tool-1"
                                               #f
                                               'tool
                                               'tool-result
                                               (list (make-text-part "content of /tmp/foo.rkt"))
                                               (current-seconds)
                                               (hasheq)))
                           message-id
                           (lambda (m) 20))
      (define entries
        (cons (make-test-msg "sys" 'system 'system-instruction "System")
              (cons tool-msg
                    (for/list ([i (in-range 40)])
                      (make-message (format "m~a" i)
                                    (if (= i 0)
                                        #f
                                        (format "m~a" (sub1 i)))
                                    (if (even? i) 'user 'assistant)
                                    'message
                                    (list (make-text-part (format "Message ~a text" i)))
                                    (+ 1000 i)
                                    (hasheq))))))
      (append-entries! sp entries)
      (define idx (build-index! sp ip))
      (define config (make-context-assembly-config #:recent-tokens 100))
      (define result (build-assembled-context idx config #:working-set ws))
      (define result-ids (map message-id (context-result-messages result)))
      (check-not-false (member "tool-1" result-ids)))

    (test-case "T03: working set messages appear after system before recent"
      (define ws (make-working-set))
      (define sys-msg (make-test-msg "sys" 'system 'system-instruction "System"))
      (define tool-msg (make-test-msg "tool-1" 'tool 'tool-result "content"))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/foo.rkt")))
                           (list (make-message "tool-1"
                                               #f
                                               'tool
                                               'tool-result
                                               (list (make-text-part "content"))
                                               (current-seconds)
                                               (hasheq)))
                           message-id
                           (lambda (m) 10))
      (define msgs (list sys-msg tool-msg))
      (define tc (build-tiered-context msgs #:working-set-messages (list tool-msg)))
      (define tier-a (tiered-context-tier-a tc))
      (check-equal? (message-id (car tier-a)) "sys")
      (check-equal? (message-id (cadr tier-a)) "tool-1"))

    (test-case "T04: empty working set produces identical tiered output"
      (define msgs
        (list (make-test-msg "sys" 'system 'system-instruction "System")
              (make-test-msg "u1" 'user 'message "Hello")
              (make-test-msg "a1" 'assistant 'message "Hi")))
      (define tc-without (build-tiered-context msgs))
      (define tc-with (build-tiered-context msgs #:working-set-messages '()))
      (check-equal? (length (tiered-context-tier-a tc-without))
                    (length (tiered-context-tier-a tc-with)))
      (check-equal? (length (tiered-context-tier-b tc-without))
                    (length (tiered-context-tier-b tc-with)))
      (check-equal? (length (tiered-context-tier-c tc-without))
                    (length (tiered-context-tier-c tc-with))))

    (test-case "T05: working-set-resolve-messages finds correct messages"
      (define ws (make-working-set))
      (define msg-a (make-test-msg "a" 'tool 'tool-result "content A"))
      (define msg-b (make-test-msg "b" 'tool 'tool-result "content B"))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/a.rkt")))
                           (list (make-message "a"
                                               #f
                                               'tool
                                               'tool-result
                                               (list (make-text-part "content A"))
                                               (current-seconds)
                                               (hasheq)))
                           message-id
                           (lambda (m) 10))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/b.rkt")))
                           (list (make-message "b"
                                               #f
                                               'tool
                                               'tool-result
                                               (list (make-text-part "content B"))
                                               (current-seconds)
                                               (hasheq)))
                           message-id
                           (lambda (m) 10))
      (define all-msgs (list msg-a msg-b (make-test-msg "c" 'user 'message " unrelated")))
      (define resolved (working-set-resolve-messages ws all-msgs message-id))
      (check-equal? (length resolved) 2)
      (check-not-false (member msg-a resolved))
      (check-not-false (member msg-b resolved)))

    (test-case "T06: pinned token count includes working set messages"
      (define ws (make-working-set))
      (define tool-msg (make-test-msg "tool-1" 'tool 'tool-result (make-string 100 #\x)))
      (working-set-update! ws
                           (list (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/foo.rkt")))
                           (list (make-message "tool-1"
                                               #f
                                               'tool
                                               'tool-result
                                               (list (make-text-part (make-string 100 #\x)))
                                               (current-seconds)
                                               (hasheq)))
                           message-id
                           (lambda (m) 100))
      (define msgs (list (make-test-msg "sys" 'system 'system-instruction "System") tool-msg))
      (define tc (build-tiered-context msgs #:working-set-messages (list tool-msg)))
      (check-equal? (length (tiered-context-tier-a tc)) 2))))

(module+ main
  (run-tests ws-tests))
(module+ test
  (run-tests ws-tests))
