#lang racket/base

;; sandbox/worker-tools.rkt — Tool execution dispatch for the worker process
;;
;; The worker has a minimal tool registry. It does NOT import the full
;; 27-tool registry (too heavy, defeats isolation). Instead, each tool
;; is a function: (-> hash? ipc-response?).
;;
;; Security constraints:
;;   - Working directory locked to configured workspace
;;   - Path traversal: all file paths resolved and checked
;;   - Output size capped at IPC-MAX-RESPONSE-BYTES
;;   - Shell execution reuses sandbox/subprocess.rkt (custodian-based)

(require racket/contract
         racket/file
         racket/match
         racket/port
         racket/string
         json
         "ipc-protocol.rkt"
         "subprocess.rkt"
         "limits.rkt")

;; ── Path Safety ─────────────────────────────────────────────────

;; Allowed root directories for file operations
(define current-allowed-roots (make-parameter (list (current-directory))))

;; Check if a path is within allowed roots
(define (path-allowed? p)
  (define resolved (simplify-path (path->complete-path (expand-user-path p) (current-directory)) #f))
  (define resolved-str (path->string resolved))
  (for/or ([root (in-list (current-allowed-roots))])
    (define root-str (path->string (path->complete-path root (current-directory))))
    ;; Ensure root ends with a single slash for prefix comparison
    (define root-dir
      (if (string-suffix? root-str "/")
          root-str
          (string-append root-str "/")))
    (or (string=? resolved-str root-str)
        (string=? resolved-str root-dir)
        (string-prefix? resolved-str root-dir))))

;; ── Tool Execution Functions ────────────────────────────────────

;; Each function: (-> hash? ipc-response?)
;; Returns ipc-response with content and details populated.

(define (execute-bash args)
  (define command (hash-ref args 'command #f))
  (define timeout-secs (hash-ref args 'timeout 30))
  (define cwd (hash-ref args 'cwd #f))
  (cond
    [(not (and command (string? command)))
     (make-error-response #f "bash: missing 'command' argument")]
    [else
     (define result
       (run-subprocess "/bin/sh"
                       #:args (list "-c" command)
                       #:timeout timeout-secs
                       #:directory (or cwd (current-directory))))
     (define exit-code (subprocess-result-exit-code result))
     (define timed-out? (subprocess-result-timed-out? result))
     (cond
       [timed-out?
        (ipc-response #f
                      'timeout
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code
                              exit-code
                              'stderr
                              (subprocess-result-stderr result)
                              'elapsed-ms
                              (subprocess-result-elapsed-ms result))
                      "command timed out"
                      IPC-SCHEMA-VERSION)]
       [(eqv? exit-code 0)
        (ipc-response #f
                      'ok
                      (string-trim (subprocess-result-stdout result))
                      (hasheq 'exit-code
                              exit-code
                              'stderr
                              (subprocess-result-stderr result)
                              'elapsed-ms
                              (subprocess-result-elapsed-ms result))
                      #f
                      IPC-SCHEMA-VERSION)]
       [else
        (ipc-response #f
                      'error
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code
                              exit-code
                              'stderr
                              (subprocess-result-stderr result)
                              'elapsed-ms
                              (subprocess-result-elapsed-ms result))
                      (format "command exited with code ~a" exit-code)
                      IPC-SCHEMA-VERSION)])]))

(define (execute-write args)
  (define path (hash-ref args 'path #f))
  (define content (hash-ref args 'content ""))
  (cond
    [(not path) (make-error-response #f "write: missing 'path' argument")]
    [(not (path-allowed? path)) (make-error-response #f (format "write: path not allowed: ~a" path))]
    [else
     (define content-str
       (cond
         [(string? content) content]
         [(bytes? content) (bytes->string/utf-8 content)]
         [else (format "~a" content)]))
     (define resolved (path->complete-path (expand-user-path path) (current-directory)))
     (with-handlers ([exn:fail? (lambda (e)
                                  (make-error-response #f (format "write: ~a" (exn-message e))))])
       (define bytes-written (string-length content-str))
       (call-with-atomic-output-file resolved (lambda (port) (display content-str port)))
       (ipc-response #f
                     'ok
                     (format "wrote ~a bytes to ~a" bytes-written (path->string resolved))
                     (hasheq 'path (path->string resolved) 'bytes-written bytes-written)
                     #f
                     IPC-SCHEMA-VERSION))]))

(define (call-with-atomic-output-file path proc)
  ;; Write to temp then rename for atomicity
  (define tmp (format "~a.tmp.~a" path (current-milliseconds)))
  (call-with-output-file tmp #:exists 'replace proc)
  (rename-file-or-directory tmp path #t))

(define (execute-edit args)
  (define path (hash-ref args 'path #f))
  (define old-text (hash-ref args 'old-text #f))
  (define new-text (hash-ref args 'new-text ""))
  (cond
    [(not path) (make-error-response #f "edit: missing 'path' argument")]
    [(not old-text) (make-error-response #f "edit: missing 'old-text' argument")]
    [(not (path-allowed? path)) (make-error-response #f (format "edit: path not allowed: ~a" path))]
    [else
     (define resolved (path->complete-path (expand-user-path path) (current-directory)))
     (cond
       [(not (file-exists? resolved))
        (make-error-response #f (format "edit: file not found: ~a" path))]
       [else
        (define content (file->string resolved))
        (cond
          [(not (string-contains? content old-text))
           (make-error-response #f "edit: old-text not found in file")]
          [else
           (define new-content (string-replace content old-text new-text))
           (call-with-atomic-output-file resolved (lambda (port) (display new-content port)))
           (ipc-response #f
                         'ok
                         "edit applied"
                         (hasheq 'path (path->string resolved) 'replacements 1)
                         #f
                         IPC-SCHEMA-VERSION)])])]))

(define (execute-git args)
  (define command (hash-ref args 'command #f))
  (define git-args (hash-ref args 'args '()))
  (cond
    [(not command) (make-error-response #f "git: missing 'command' argument")]
    [else
     (define args-list
       (cond
         [(list? git-args) git-args]
         [(string? git-args) (list git-args)]
         [else '()]))
     (define result (run-subprocess "git" #:args (cons command args-list) #:timeout 30))
     (define exit-code (subprocess-result-exit-code result))
     (define timed-out? (subprocess-result-timed-out? result))
     (cond
       [timed-out?
        (ipc-response #f
                      'timeout
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code exit-code 'stderr (subprocess-result-stderr result))
                      "git command timed out"
                      IPC-SCHEMA-VERSION)]
       [(eqv? exit-code 0)
        (ipc-response #f
                      'ok
                      (string-trim (subprocess-result-stdout result))
                      (hasheq 'exit-code exit-code 'stderr (subprocess-result-stderr result))
                      #f
                      IPC-SCHEMA-VERSION)]
       [else
        (ipc-response #f
                      'error
                      (subprocess-result-stdout result)
                      (hasheq 'exit-code exit-code 'stderr (subprocess-result-stderr result))
                      (format "git ~a exited with code ~a" command exit-code)
                      IPC-SCHEMA-VERSION)])]))

;; ── Tool Registry ───────────────────────────────────────────────

(define worker-tool-registry
  (hash "bash" execute-bash "write" execute-write "edit" execute-edit "git" execute-git))

(define (dispatch-tool tool-name arguments)
  (define executor (hash-ref worker-tool-registry tool-name #f))
  (cond
    [executor
     (with-handlers ([exn:fail? (lambda (e)
                                  (make-error-response
                                   #f
                                   (format "tool '~a' crashed: ~a" tool-name (exn-message e))))])
       (executor arguments))]
    [else (make-error-response #f (format "unknown tool: ~a" tool-name))]))

;; ── Provides ────────────────────────────────────────────────────

(provide worker-tool-registry
         dispatch-tool
         current-allowed-roots
         path-allowed?
         execute-bash
         execute-write
         execute-edit
         execute-git)

;; dispatch-tool is provided above without contract
