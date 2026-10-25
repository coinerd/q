#lang racket

;; @speed fast
;; @suite default
;; @boundary integration
;; BUG-0089: exercise the real dangerous/externalizable write gateway with a
;; string working directory, including the trailing slash used by campaigns.
(require rackunit
         racket/runtime-path
         (only-in racket/string string-contains?)
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

;; ── R91/BUG-0091: trusted coordinator cwd extends worker roots across the
;; real IPC path, for write AND edit, and only for the request's dynamic
;; extent. The worker is spawned with parent root A; the target lives in
;; sibling B, OUTSIDE the spawn roots, so every success below is attributable
;; exclusively to the coordinator-supplied trusted working directory.
(define (check-trusted-cwd-cross-root)
  (define parent (make-temporary-file "r91-parent-~a" 'directory))
  (define sibling (make-temporary-file "r91-sibling-~a" 'directory))
  (define owned-cwd (path->string (path->directory-path sibling)))
  (define target (build-path sibling "owned.txt"))
  (define initial-content "R91: coordinator-owned write\n")
  (define edited-content "R91: coordinator-owned edit applied\n")
  (dynamic-wind
   void
   (lambda ()
     (define registry (make-tool-registry))
     (register-default-tools! registry)
     (define write-tool (lookup-tool registry "write"))
     (define edit-tool (lookup-tool registry "edit"))
     (check-true (tool-dangerous? write-tool))
     (check-true (tool-externalizable? write-tool))
     (check-true (tool-dangerous? edit-tool))
     (check-true (tool-externalizable? edit-tool))
     (parameterize ([current-execution-plane-enabled #t]
                    [current-worker-args (list "-tm" (path->string worker-main-path))])
       ;; The worker's spawn roots capture parent A ONLY (the gateway spawns
       ;; the worker with the gateway process's cwd).
       (parameterize ([current-directory parent])
         (ensure-worker!))
       (define (run-batch tool-name args exec-cwd)
         (define batch
           (run-tool-batch (list (make-tool-call "r91-call" tool-name args))
                           registry
                           #:exec-context (make-exec-context
                                           #:working-directory exec-cwd
                                           #:permission-config
                                           (make-permissive-permission-config))))
         (car (scheduler-result-results batch)))
       (define (result-text r) (format "~a ~a" (tool-result-content r) (tool-result-is-error? r)))
       ;; 1. WRITE into sibling B (outside spawn roots) via trusted cwd B.
       (define write-result
         (run-batch "write"
                    (hasheq 'path (path->string target) 'content initial-content)
                    owned-cwd))
       (check-false (tool-result-is-error? write-result) (result-text write-result))
       (check-true (file-exists? target))
       (check-equal? (file->string target) initial-content)
       ;; 2. EDIT the same sibling-B file with exact bytes via trusted cwd B.
       (define edit-result
         (run-batch "edit"
                    (hasheq 'path
                            (path->string target)
                            'old-text
                            "coordinator-owned write"
                            'new-text
                            "coordinator-owned edit applied")
                    owned-cwd))
       (check-false (tool-result-is-error? edit-result) (result-text edit-result))
       (check-equal? (file->string target) edited-content)
       ;; 3. A request whose tool hits an exception inside the worker's request
       ;; scope (bash cwd names a non-existent directory inside B; the spawn
       ;; fails inside the extended-root scope and the worker converts it at
       ;; its crash boundary). The dynamic extent must unwind regardless.
       (define exception-result
         (run-batch "bash"
                    (hasheq 'command "pwd" 'cwd (path->string (build-path sibling "vanished")))
                    owned-cwd))
       (check-true (tool-result-is-error? exception-result) (result-text exception-result))
       ;; 4. Roots are GONE on the next request: the same worker, coordinator
       ;; cwd back inside parent A, cannot write into sibling B anymore.
       (define denied-after-exception
         (run-batch "write"
                    (hasheq 'path (path->string (build-path sibling "escape.txt")) 'content "no")
                    (path->string (path->directory-path parent))))
       (check-true (tool-result-is-error? denied-after-exception)
                   (result-text denied-after-exception))
       (check-true (or (string-contains? (format "~a" (tool-result-content denied-after-exception))
                                         "not allowed")
                       (string-contains? (format "~a" (tool-result-content denied-after-exception))
                                         "refused"))
                   (format "refusal must be a path/trust denial: ~a"
                           (tool-result-content denied-after-exception)))
       (check-false (file-exists? (build-path sibling "escape.txt")))
       ;; 5. The earlier exact bytes are preserved and the worker remains
       ;; usable: trusted cwd B re-derives per request (statelessness proof).
       (define denied-then-allowed
         (run-batch "edit"
                    (hasheq 'path
                            (path->string target)
                            'old-text
                            "coordinator-owned edit applied"
                            'new-text
                            "coordinator-owned edit re-applied")
                    owned-cwd))
       (check-false (tool-result-is-error? denied-then-allowed) (result-text denied-then-allowed))
       (check-equal? (file->string target) "R91: coordinator-owned edit re-applied\n")))
   (lambda ()
     (shutdown-worker!)
     (delete-directory/files parent #:must-exist? #f)
     (delete-directory/files sibling #:must-exist? #f))))

(test-case "real worker write+edit outside spawn roots via trusted cwd only; roots drop next request"
  (check-trusted-cwd-cross-root))
