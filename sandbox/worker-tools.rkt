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

;; LF3 (v0.99.4): Walk path components to resolve symlinks on the longest
;; existing prefix. Prevents symlink-based escapes when non-existent
;; directories exist after a symlink in the path chain. For example,
;; /allowed-root/symlink→/etc/a/b/file.txt would previously pass the check
;; because the else branch only called simplify-path (no symlink resolution).
(define (resolve-longest-prefix p)
  (define parts (explode-path p))
  (let loop ([remaining parts]
             [resolved-prefix #f])
    (cond
      [(null? remaining) (or resolved-prefix (simplify-path p #f))]
      [else
       (define candidate
         (if resolved-prefix
             (build-path resolved-prefix (car remaining))
             (car remaining)))
       (cond
         [(or (file-exists? candidate) (directory-exists? candidate))
          ;; Component exists — resolve symlinks and continue
          (loop (cdr remaining) (simplify-path (resolve-path candidate) #f))]
         [(link-exists? candidate)
          ;; Broken symlink — reject (resolve-path would raise, caught by outer handler → #f)
          (raise (exn:fail (format "broken symlink: ~a" candidate) (current-continuation-marks)))]
         [else
          ;; Non-existent component — append remaining to resolved prefix
          (if resolved-prefix
              (for/fold ([acc resolved-prefix]) ([part (in-list remaining)])
                (build-path acc part))
              (simplify-path p #f))])])))

;; H3: Resolve symlinks before checking — simplify-path alone does NOT resolve symlinks.
;; A symlink inside the allowed root pointing to /etc would pass the old check.
;; resolve-path follows symlinks and raises exn on broken links → we reject those.
;; For non-existent files (new writes), resolve the parent directory instead.
;; LF3 (v0.99.4): When neither file nor parent dir exists, walk path components
;; to resolve symlinks on the longest existing prefix.
(define (resolve-path-safely p)
  (define complete (path->complete-path (expand-user-path p) (current-directory)))
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (cond
      [(file-exists? complete) (simplify-path (resolve-path complete) #f)]
      ;; File doesn't exist yet (e.g. new write) — resolve parent dir, then append filename
      [(let-values ([(base name must-be-dir?) (split-path complete)])
         (and base (directory-exists? base)))
       (let-values ([(base name must-be-dir?) (split-path complete)])
         (define parent-resolved (simplify-path (resolve-path base) #f))
         (if name
             (build-path parent-resolved name)
             parent-resolved))]
      ;; LF3 (v0.99.4): Neither file nor parent dir exists — walk path
      ;; components to resolve symlinks on the longest existing prefix.
      [else (resolve-longest-prefix complete)])))

;; H3: Normalize a root directory at comparison time (resolve symlinks)
(define (normalize-root r)
  (with-handlers ([exn:fail? (lambda (_) r)])
    (simplify-path (resolve-path (path->complete-path r (current-directory))) #f)))

;; Check if a path is within allowed roots
(define (path-allowed? p)
  (define resolved (resolve-path-safely p))
  ;; If resolve-path failed (broken symlink or missing file), reject
  (and resolved
       (let ([resolved-str (path->string resolved)])
         (for/or ([root (in-list (current-allowed-roots))])
           (define normalized-root (normalize-root root))
           (define root-str (path->string (path->complete-path normalized-root (current-directory))))
           ;; Ensure root ends with a single slash for prefix comparison
           (define root-dir
             (if (string-suffix? root-str "/")
                 root-str
                 (string-append root-str "/")))
           (or (string=? resolved-str root-str)
               (string=? resolved-str root-dir)
               (string-prefix? resolved-str root-dir))))))

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
