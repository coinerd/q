#lang racket

;; @speed fast
;; @suite default
;; @boundary integration
;; BUG-0089: exercise the real dangerous/externalizable write gateway with a
;; string working directory, including the trailing slash used by campaigns.
(require rackunit
         racket/runtime-path
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         (only-in "../tools/permission-gate.rkt" make-permissive-permission-config)
         (only-in "../sandbox/gateway-bridge.rkt"
                  current-execution-plane-enabled
                  current-worker-args
                  ensure-worker!
                  shutdown-worker!))

(define-runtime-path worker-main-path "../sandbox/worker-main.rkt")

(define (check-string-cwd-write relative?)
  (define repo (make-temporary-file "bug89-repo-~a" 'directory))
  (define workspace (build-path repo "workspace"))
  (define cwd (path->string (path->directory-path workspace)))
  (define target (build-path workspace "written.txt"))
  (define content "BUG-0089: real worker write\n")
  (dynamic-wind
   void
   (lambda ()
     ;; A minimal isolated repository; no live campaign or authority state.
     (make-directory (build-path repo ".git"))
     (make-directory workspace)
     (define registry (make-tool-registry))
     (register-default-tools! registry)
     (define write-tool (lookup-tool registry "write"))
     (check-true (tool-dangerous? write-tool))
     (check-true (tool-externalizable? write-tool))
     (check-true (string-suffix? cwd "/"))
     (parameterize ([current-execution-plane-enabled #t]
                    [current-worker-args (list "-tm" (path->string worker-main-path))])
       ;; Spawn roots cover this isolated repo. The request must still change
       ;; CWD to its nested workspace for the relative-path assertion to pass.
       (parameterize ([current-directory repo])
         (ensure-worker!))
       (define batch
         (run-tool-batch (list (make-tool-call "bug89-write"
                                               "write"
                                               (hasheq 'path
                                                       (if relative?
                                                           "written.txt"
                                                           (path->string target))
                                                       'content
                                                       content)))
                         registry
                         #:exec-context (make-exec-context #:working-directory cwd
                                                           #:permission-config
                                                           (make-permissive-permission-config))))
       (define results (scheduler-result-results batch))
       (check-equal? (length results) 1)
       (define result (car results))
       (check-false (tool-result-is-error? result) (format "~a" (tool-result-content result)))
       (check-false (regexp-match? #rx"raised" (format "~a" (tool-result-content result))))
       (check-true (file-exists? target))
       (when (file-exists? target)
         (check-equal? (file->string target) content))))
   (lambda ()
     (shutdown-worker!)
     (delete-directory/files repo))))

(test-case "absolute real worker write succeeds with trailing-slash string cwd"
  (check-string-cwd-write #f))

(test-case "relative real worker write lands inside string cwd"
  (check-string-cwd-write #t))
