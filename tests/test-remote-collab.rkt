#lang racket/base

;; tests/test-remote-collab.rkt — Tests for remote-collab extension

(require rackunit
         racket/string
         "../extensions/remote-collab/remote-collab.rkt"
         "../extensions/remote-collab/ssh-helpers.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Error handling tests
;; ============================================================

(test-case "handle-remote-q requires host"
  (with-handlers ([exn:fail? (lambda (e)
                               (check-true (string-contains? (exn-message e) "host is required")))])
    (handle-remote-q (hasheq 'action "status"))
    (check-false "Should have raised error" #t)))

(test-case "handle-remote-q unknown action raises error"
  (with-handlers ([exn:fail? (lambda (e)
                               (check-true (string-contains? (exn-message e) "unknown action")))])
    (handle-remote-q (hasheq 'host "localhost" 'action "bogus"))
    (check-false "Should have raised error" #t)))

;; ============================================================
;; Handler tests (SSH fails on unreachable host — tests error path)
;; ============================================================

(test-case "ssh-execute fails on unreachable host"
  (define-values (ec out err)
    (ssh-execute "nonexistent.test.invalid"
                 "echo hello"
                 #:options (cons "-o" (cons "ConnectTimeout=1" (default-ssh-options)))))
  (check-not-equal? ec 0))

(test-case "handle-remote-q send returns error for unreachable host"
  (define result
    (handle-remote-q
     (hasheq 'host "nonexistent.test.invalid" 'action "send" 'session "test" 'prompt "hello world")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-remote-q capture returns error for unreachable host"
  (define result
    (handle-remote-q
     (hasheq 'host "nonexistent.test.invalid" 'action "capture" 'session "test" 'lines 50)))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-remote-q start returns error for unreachable host"
  (define result
    (handle-remote-q
     (hasheq 'host "nonexistent.test.invalid" 'action "start" 'session "test" 'cwd "/tmp")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-remote-q stop returns error for unreachable host"
  (define result
    (handle-remote-q (hasheq 'host "nonexistent.test.invalid" 'action "stop" 'session "test")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-remote-q interrupt returns error for unreachable host"
  (define result
    (handle-remote-q (hasheq 'host "nonexistent.test.invalid" 'action "interrupt" 'session "test")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-remote-q wait returns error for unreachable host"
  (define result
    (handle-remote-q
     (hasheq 'host "nonexistent.test.invalid" 'action "wait" 'session "test" 'timeout 5)))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; SSH options tests
;; ============================================================

(test-case "default-ssh-options includes StrictHostKeyChecking"
  (define opts (default-ssh-options))
  (define opts-str (string-join opts " "))
  (check-true (string-contains? opts-str "StrictHostKeyChecking=accept-new"))
  ;; Verify not using insecure 'no' by default
  (check-false (string-contains? opts-str "StrictHostKeyChecking=no")))

(test-case "default-ssh-options includes BatchMode"
  (define opts (default-ssh-options))
  (define opts-str (string-join opts " "))
  (check-true (string-contains? opts-str "BatchMode=yes")))

;; ============================================================
;; Tool registration test
;; ============================================================

(require "../extensions/context.rkt"
         "../extensions/dynamic-tools.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../agent/event-bus.rkt")

(test-case "remote-collab extension registers remote-q tool"
  (define reg (make-tool-registry))
  (define ctx
    (make-extension-ctx #:session-id "test"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)
                        #:tool-registry reg
                        #:command-registry (box (hash))))
  (define hook (hash-ref (extension-hooks the-extension) 'register-tools #f))
  (check-not-false hook "register-tools hook exists")
  (when hook
    (hook ctx (hasheq)))
  (check-not-false (lookup-tool reg "remote-q") "remote-q tool registered"))

;; ============================================================
;; M7 regression: Session name validation
;; ============================================================

(require (only-in "../extensions/remote-collab/remote-collab.rkt" valid-session-name?))

(test-case "valid-session-name? rejects shell metacharacters"
  (check-true (valid-session-name? "q-agent"))
  (check-true (valid-session-name? "my_session_123"))
  (check-false (valid-session-name? "session; rm -rf /"))
  (check-false (valid-session-name? "session$(pwned)"))
  (check-false (valid-session-name? "session`id`"))
  (check-false (valid-session-name? "session|cat")))

(test-case "remote-tmux rejects invalid session name"
  (check-exn exn:fail? (lambda () (remote-tmux "host" "; rm -rf /" 'status (hasheq)))))
