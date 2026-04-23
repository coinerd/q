#lang racket/base

;; tests/test-session-export.rkt — Tests for session-export extension

(require rackunit
         racket/file
         racket/string
         json
         "../extensions/session-export.rkt"
         "../extensions/context.rkt"
         "../extensions/dynamic-tools.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../tools/tool.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; Tool registration test
;; ============================================================

(test-case "session-export extension registers session-export tool"
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
    (hook ctx))
  (check-not-false (lookup-tool reg "session-export") "session-export tool registered"))

;; ============================================================
;; Handler tests
;; ============================================================

(test-case "handle-session-export requires session_path"
  (check-exn exn:fail? (lambda () (handle-session-export (hasheq 'format "html")))))

(test-case "handle-session-export errors on missing file"
  (check-exn exn:fail?
             (lambda () (handle-session-export (hasheq 'session_path "/tmp/nonexistent.jsonl")))))

(test-case "handle-session-export exports markdown"
  (define tmp (make-temporary-file "sess-~a.jsonl"))
  (call-with-output-file tmp
                         (lambda (out)
                           (displayln (jsexpr->string (hasheq 'role "user" 'content "Hello")) out)
                           (displayln (jsexpr->string (hasheq 'role "assistant" 'content "Hi there"))
                                      out))
                         #:exists 'replace)
  (define result (handle-session-export (hasheq 'session_path (path->string tmp) 'format "markdown")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "User"))
  (check-true (string-contains? text "Hello"))
  (delete-file tmp))

(test-case "handle-session-export exports html"
  (define tmp (make-temporary-file "sess-~a.jsonl"))
  (call-with-output-file
   tmp
   (lambda (out)
     (displayln (jsexpr->string (hasheq 'role "user" 'content "<script>alert(1)</script>")) out))
   #:exists 'replace)
  (define result (handle-session-export (hasheq 'session_path (path->string tmp) 'format "html")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "&lt;script&gt;"))
  (delete-file tmp))

(test-case "handle-session-export exports json"
  (define tmp (make-temporary-file "sess-~a.jsonl"))
  (call-with-output-file tmp
                         (lambda (out)
                           (displayln (jsexpr->string (hasheq 'role "user" 'content "Hello")) out))
                         #:exists 'replace)
  (define result (handle-session-export (hasheq 'session_path (path->string tmp) 'format "json")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "Hello"))
  (delete-file tmp))

(test-case "handle-session-export writes to output_path"
  (define tmp (make-temporary-file "sess-~a.jsonl"))
  (define out-tmp (make-temporary-file "out-~a.md"))
  (call-with-output-file tmp
                         (lambda (out)
                           (displayln (jsexpr->string (hasheq 'role "user" 'content "Hello")) out))
                         #:exists 'replace)
  (define result
    (handle-session-export (hasheq 'session_path
                                   (path->string tmp)
                                   'format
                                   "markdown"
                                   'output_path
                                   (path->string out-tmp))))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (check-true (file-exists? out-tmp))
  (check-true (string-contains? (file->string out-tmp) "Hello"))
  (delete-file tmp)
  (delete-file out-tmp))
