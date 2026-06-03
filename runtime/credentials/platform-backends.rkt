#lang racket/base

;; runtime/credentials/platform-backends.rkt — macOS + Windows credential backends + capabilities
;; Extracted from credential-backend.rkt (v0.73.5 A-4)

(require racket/contract
         racket/string
         racket/port
         "../../util/error/error-helpers.rkt"
         "../../util/error/errors.rkt"
         (only-in "protocol.rkt"
                  credential-backend
                  credential-backend?
                  backend-name
                  backend-available?
                  current-shell-command-runner
                  shell-escape)
         (only-in "env-backend.rkt" make-env-credential-backend))

(provide make-macos-keychain-credential-backend
         make-windows-credential-backend
         credential-backend-capabilities)

;; ═══════════════════════════════════════════════════════════════════
;; Platform capability matrix
;; ═══════════════════════════════════════════════════════════════════

(define (credential-backend-capabilities)
  (define runner (current-shell-command-runner))
  (define (cmd-available? cmd)
    (with-handlers ([exn:fail? (λ (_) #f)])
      (define out (open-output-string))
      (runner (format "which ~a 2>/dev/null" cmd) out)
      (positive? (string-length (get-output-string out)))))
  (hash 'env
        (backend-available? (make-env-credential-backend))
        'file
        #t
        'memory
        #t
        'keychain-linux
        (cmd-available? "secret-tool")
        'keychain-macos
        (cmd-available? "security")
        'keychain-windows
        (cmd-available? "cmdkey")
        'platform
        (symbol->string (system-type 'os))))

;; ═══════════════════════════════════════════════════════════════════
;; macOS security backend
;; ═══════════════════════════════════════════════════════════════════

(define (run-security-command args)
  (define runner (current-shell-command-runner))
  (define out (open-output-string))
  (define cmd (format "security ~a 2>&1" args))
  (define result (runner cmd out))
  (values (get-output-string out) result))

(define (make-macos-keychain-credential-backend)
  (credential-backend
   "macos-keychain"
   ;; store!
   (λ (be provider-name api-key)
     (define user
       (or (getenv "USER")
           (getenv "LOGNAME")
           (with-handlers ([exn:fail? (λ (_) #f)])
             (path->string (find-system-path 'who-am-i)))
           "unknown"))
     (define-values (out ok?)
       (run-security-command (format "add-generic-password -a '~a' -s 'q-credential-~a' -w '~a' -U"
                                     (shell-escape user)
                                     (shell-escape provider-name)
                                     (shell-escape api-key))))
     (unless ok?
       (raise-credential-error (format "macOS keychain store failed for '~a'" provider-name)
                               "macos-keychain"
                               provider-name)))
   ;; load
   (λ (be provider-name env-var)
     (define user
       (or (getenv "USER")
           (getenv "LOGNAME")
           (with-handlers ([exn:fail? (λ (_) #f)])
             (path->string (find-system-path 'who-am-i)))
           "unknown"))
     (define-values (out ok?)
       (run-security-command (format "find-generic-password -a '~a' -s 'q-credential-~a' -w"
                                     (shell-escape user)
                                     (shell-escape provider-name))))
     (if ok?
         (hash 'api-key (string-trim out) 'provider provider-name 'source "macos-keychain")
         #f))
   ;; delete!
   (λ (be provider-name)
     (define user
       (or (getenv "USER")
           (getenv "LOGNAME")
           (with-handlers ([exn:fail? (λ (_) #f)])
             (path->string (find-system-path 'who-am-i)))
           "unknown"))
     (define-values (out ok?)
       (run-security-command (format "delete-generic-password -a '~a' -s 'q-credential-~a'"
                                     (shell-escape user)
                                     (shell-escape provider-name))))
     (void))
   ;; list
   (λ (be) '())
   ;; available?
   (λ (be)
     (with-handlers ([exn:fail? (λ (_) #f)])
       (define runner (current-shell-command-runner))
       (define out (open-output-string))
       (runner "which security 2>/dev/null" out)
       (positive? (string-length (get-output-string out)))))))

;; ═══════════════════════════════════════════════════════════════════
;; Windows Credential Manager backend
;; ═══════════════════════════════════════════════════════════════════

(define (run-cmdkey-command args)
  (define runner (current-shell-command-runner))
  (define out (open-output-string))
  (define cmd (format "cmdkey ~a 2>&1" args))
  (define result (runner cmd out))
  (values (get-output-string out) result))

(define (make-windows-credential-backend)
  (log-warning
   "Windows Credential Manager stores API key via cmdkey CLI arguments. ~~ Use env-only policy on shared machines for enhanced security.")
  (credential-backend
   "windows-credential-manager"
   ;; store!
   (λ (be provider-name api-key)
     (define-values (out ok?)
       (run-cmdkey-command (format "/generic:q-credential-~a /user:q-api-key /pass:'~a'"
                                   (shell-escape provider-name)
                                   (shell-escape api-key))))
     (unless ok?
       (raise-credential-error (format "Windows credential store failed for '~a'" provider-name)
                               "windows-credential-manager"
                               provider-name)))
   ;; load
   (λ (be provider-name env-var)
     (define-values (out ok?) (run-cmdkey-command (format "/list:q-credential-~a" provider-name)))
     (if (and ok? (string-contains? out (format "q-credential-~a" provider-name)))
         (begin
           (log-warning
            "Windows Credential Manager cannot retrieve stored passwords. ~~ Credential marked as present but key unavailable.")
           #f)
         #f))
   ;; delete!
   (λ (be provider-name)
     (run-cmdkey-command (format "/delete:q-credential-~a" provider-name))
     (void))
   ;; list
   (λ (be)
     (define-values (out ok?) (run-cmdkey-command "/list"))
     (if ok?
         (let ([lines (string-split out "\n")])
           (for/list ([line (in-list lines)]
                      #:when (string-contains? line "q-credential-"))
             (define m (regexp-match #rx"q-credential-([a-zA-Z0-9_-]+)" line))
             (if m
                 (cadr m)
                 line)))
         '()))
   ;; available?
   (λ (be)
     (with-handlers ([exn:fail? (λ (_) #f)])
       (define runner (current-shell-command-runner))
       (define out (open-output-string))
       (runner "which cmdkey 2>/dev/null || where cmdkey 2>nul" out)
       (positive? (string-length (get-output-string out)))))))
