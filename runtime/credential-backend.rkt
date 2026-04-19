#lang racket/base

;; runtime/credential-backend.rkt — Credential backend abstraction layer
;;
;; Issue #1291: GAP-02a — Pluggable credential backends
;; Issue #1292: GAP-02b — OS keychain integration
;;
;; Provides a unified interface for storing/retrieving credentials from
;; different backends: file (JSON), environment variables, memory (testing),
;; OS keychain (secret-tool / macOS security), and chained (fallback chain).

(require json
         racket/file
         racket/string
         racket/path
         racket/match
         racket/list
         racket/system
         racket/port)

;; Backend struct
(provide (struct-out credential-backend)

         ;; Constructors
         make-file-credential-backend
         make-env-credential-backend
         make-memory-credential-backend
         make-keychain-credential-backend
         make-chained-credential-backend

         ;; Generic operations
         backend-name
         backend-store!
         backend-load
         backend-delete!
         backend-list-providers
         backend-available?)

;; ═══════════════════════════════════════════════════════════════════
;; Backend struct
;; ═══════════════════════════════════════════════════════════════════

;; A credential backend with a name and a set of operation functions.
;; Each operation function takes (backend provider-name [#:env-var ...]) etc.
(struct credential-backend
        (name ; string — human-readable name ("file", "env", "memory", "keychain", "chained")
         store-fn ; (backend provider-name api-key) → void?
         load-fn ; (backend provider-name [#:env-var string?]) → (or/c #f hash?)
         delete-fn ; (backend provider-name) → void?
         list-fn ; (backend) → (listof string?)
         available?-fn ; (backend) → boolean?
         )
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Generic operations
;; ═══════════════════════════════════════════════════════════════════

(define (backend-name be)
  (credential-backend-name be))

(define (backend-store! be provider-name api-key)
  ((credential-backend-store-fn be) be provider-name api-key))

(define (backend-load be provider-name #:env-var [env-var #f])
  ((credential-backend-load-fn be) be provider-name env-var))

(define (backend-delete! be provider-name)
  ((credential-backend-delete-fn be) be provider-name))

(define (backend-list-providers be)
  ((credential-backend-list-fn be) be))

(define (backend-available? be)
  ((credential-backend-available?-fn be) be))

;; ═══════════════════════════════════════════════════════════════════
;; File backend — JSON file on disk (~/.q/credentials.json)
;; ═══════════════════════════════════════════════════════════════════

(define (make-file-credential-backend [path #f])
  (define cred-path (or path (build-path (find-system-path 'home-dir) ".q" "credentials.json")))
  (credential-backend "file"
                      ;; store!
                      (λ (be provider-name api-key) (file-store! cred-path provider-name api-key))
                      ;; load
                      (λ (be provider-name env-var) (file-load cred-path provider-name))
                      ;; delete!
                      (λ (be provider-name) (file-delete! cred-path provider-name))
                      ;; list
                      (λ (be) (file-list-providers cred-path))
                      ;; available?
                      (λ (be) (file-available? cred-path))
                      ;; extra: store the path for introspection
                      ))

(define (file-load-raw path)
  (cond
    [(not (file-exists? path)) (hash)]
    [else
     (with-handlers ([exn:fail? (λ (e) (hash))])
       (define content (call-with-input-file path read-json))
       (if (eof-object? content)
           (hash)
           (let ([providers (hash-ref content 'providers (hash))])
             (for/hash ([(k v) (in-hash providers)])
               (values (if (symbol? k)
                           (symbol->string k)
                           k)
                       v)))))]))

(define (file-store! path provider-name api-key)
  (define existing (file-load-raw path))
  (define updated (hash-set existing provider-name (hasheq 'api-key api-key)))
  (define file-content
    (hasheq 'providers
            (for/hash ([(k v) (in-hash updated)])
              (values (if (string? k)
                          (string->symbol k)
                          k)
                      v))))
  (file-write-atomic! path file-content))

(define (file-load path provider-name)
  (define all (file-load-raw path))
  (define entry (hash-ref all provider-name #f))
  (cond
    [(not entry) #f]
    [else
     (define key (hash-ref entry 'api-key #f))
     (if (and key (non-empty-string? key))
         (hasheq 'api-key key 'source "file" 'provider provider-name)
         #f)]))

(define (file-delete! path provider-name)
  (define all (file-load-raw path))
  (define updated (hash-remove all provider-name))
  (define file-content
    (hasheq 'providers
            (for/hash ([(k v) (in-hash updated)])
              (values (if (string? k)
                          (string->symbol k)
                          k)
                      v))))
  (file-write-atomic! path file-content))

(define (file-list-providers path)
  (hash-keys (file-load-raw path)))

(define (file-available? path)
  (cond
    [(file-exists? path) #t]
    [else
     (define dir (path-only path))
     (and dir
          (or (directory-exists? dir)
              (with-handlers ([exn:fail? (λ (e) #f)])
                (make-directory* dir)
                #t)))]))

(define (file-write-atomic! path data)
  (define dir (path-only path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir))
  (define tmp (make-temporary-file "credential-~a.tmp" #f (or dir (find-system-path 'temp-dir))))
  (with-handlers ([exn:fail? (λ (e)
                               (with-handlers ([exn:fail? void])
                                 (delete-file tmp))
                               (raise e))])
    (call-with-output-file tmp (λ (out) (write-json data out)) #:exists 'truncate)
    (rename-file-or-directory tmp path #t)
    (file-or-directory-permissions path #o600)))

;; ═══════════════════════════════════════════════════════════════════
;; Environment variable backend — read-only
;; ═══════════════════════════════════════════════════════════════════

;; Naming convention: Q_<PROVIDER>_API_KEY (provider uppercased, hyphens→underscores)
(define (provider->env-var provider-name)
  (define normalized (string-upcase (string-replace provider-name "-" "_")))
  (format "Q_~a_API_KEY" normalized))

(define (make-env-credential-backend)
  (credential-backend "env"
                      ;; store! — read-only, raises error
                      (λ (be provider-name api-key)
                        (error 'backend-store! "Environment backend is read-only"))
                      ;; load
                      (λ (be provider-name env-var)
                        (define var (or env-var (provider->env-var provider-name)))
                        (define val (getenv var))
                        (if (and val (non-empty-string? val))
                            (hasheq 'api-key val 'source "environment" 'provider provider-name)
                            #f))
                      ;; delete! — no-op
                      (λ (be provider-name) (void))
                      ;; list — not possible to enumerate env vars meaningfully
                      (λ (be) '())
                      ;; available?
                      (λ (be) #t)))

;; ═══════════════════════════════════════════════════════════════════
;; Memory backend — in-memory hash (for testing)
;; ═══════════════════════════════════════════════════════════════════

(define (make-memory-credential-backend)
  (define store (make-hash))
  (credential-backend "memory"
                      ;; store!
                      (λ (be provider-name api-key) (hash-set! store provider-name api-key))
                      ;; load
                      (λ (be provider-name env-var)
                        (define val (hash-ref store provider-name #f))
                        (if val
                            (hasheq 'api-key val 'source "memory" 'provider provider-name)
                            #f))
                      ;; delete!
                      (λ (be provider-name) (hash-remove! store provider-name))
                      ;; list
                      (λ (be) (hash-keys store))
                      ;; available?
                      (λ (be) #t)))

;; ═══════════════════════════════════════════════════════════════════
;; OS Keychain backend — uses secret-tool (Linux) or security (macOS)
;; ═══════════════════════════════════════════════════════════════════

;; Key attribute for secret-tool: q-agent/provider/<name>
(define (keychain-attrs provider-name)
  (list (cons "application" "q-agent") (cons "provider" provider-name)))

(define (keychain-label provider-name)
  (format "q-agent: ~a" provider-name))

(define (secret-tool-available?)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define out (open-output-string))
    (parameterize ([current-output-port out]
                   [current-error-port (open-output-nowhere)])
      (system "which secret-tool"))
    (string-contains? (get-output-string out) "secret-tool")))

(define (make-keychain-credential-backend)
  (credential-backend "keychain"
                      ;; store!
                      (λ (be provider-name api-key) (keychain-store! provider-name api-key))
                      ;; load
                      (λ (be provider-name env-var) (keychain-load provider-name))
                      ;; delete!
                      (λ (be provider-name) (keychain-delete! provider-name))
                      ;; list
                      (λ (be) (keychain-list-providers))
                      ;; available?
                      (λ (be) (secret-tool-available?))))

(define (keychain-store! provider-name api-key)
  (unless (secret-tool-available?)
    (error 'keychain-store! "secret-tool not available"))
  (define attrs (keychain-attrs provider-name))
  (define attr-args
    (string-join (for/list ([a (in-list attrs)])
                   (format "--~a '~a'" (car a) (shell-escape (cdr a))))
                 " "))
  (define cmd
    (format "echo '~a' | secret-tool store --label '~a' ~a"
            (shell-escape api-key)
            (shell-escape (keychain-label provider-name))
            attr-args))
  (unless (system cmd)
    (error 'keychain-store! "secret-tool store failed for ~a" provider-name)))

(define (keychain-load provider-name)
  (unless (secret-tool-available?)
    #f)
  (define attrs (keychain-attrs provider-name))
  (define attr-args
    (string-join (for/list ([a (in-list attrs)])
                   (format "--~a '~a'" (car a) (shell-escape (cdr a))))
                 " "))
  (define cmd (format "secret-tool lookup ~a 2>/dev/null" attr-args))
  (define out (open-output-string))
  (define ok
    (parameterize ([current-output-port out]
                   [current-error-port (open-output-nowhere)])
      (system cmd)))
  (define result (string-trim (get-output-string out)))
  (if (and ok (non-empty-string? result))
      (hasheq 'api-key result 'source "keychain" 'provider provider-name)
      #f))

(define (keychain-delete! provider-name)
  (unless (secret-tool-available?)
    (void))
  (define attrs (keychain-attrs provider-name))
  (define attr-args
    (string-join (for/list ([a (in-list attrs)])
                   (format "--~a '~a'" (car a) (shell-escape (cdr a))))
                 " "))
  (define cmd (format "secret-tool clear ~a 2>/dev/null" attr-args))
  (system cmd)
  (void))

(define (keychain-list-providers)
  (unless (secret-tool-available?)
    '())
  ;; Search for all q-agent secrets
  (define cmd "secret-tool search application q-agent 2>/dev/null")
  (define out (open-output-string))
  (parameterize ([current-output-port out]
                 [current-error-port (open-output-nowhere)])
    (system cmd))
  (define output (get-output-string out))
  ;; Parse provider names from output
  (for/list ([line (in-list (string-split output "\n"))]
             #:when (string-contains? line "provider"))
    (define m (regexp-match #rx"provider *= *(.+)" line))
    (if m
        (string-trim (cadr m))
        "unknown")))

;; ═══════════════════════════════════════════════════════════════════
;; Chained backend — tries multiple backends in order
;; ═══════════════════════════════════════════════════════════════════

(define (make-chained-credential-backend backends)
  (credential-backend "chained"
                      ;; store! — writes to first writable backend
                      (λ (be provider-name api-key)
                        (define stored #f)
                        (for ([sub (in-list backends)]
                              #:break stored)
                          (with-handlers ([exn:fail? (λ (e) (void))])
                            (backend-store! sub provider-name api-key)
                            (set! stored #t)))
                        (unless stored
                          (error 'backend-store! "No writable backend available")))
                      ;; load — tries each backend in order, returns first hit
                      (λ (be provider-name env-var)
                        (for/or ([sub (in-list backends)])
                          (with-handlers ([exn:fail? (λ (e) #f)])
                            (backend-load sub provider-name #:env-var (or env-var #f)))))
                      ;; delete! — deletes from all backends
                      (λ (be provider-name)
                        (for ([sub (in-list backends)])
                          (with-handlers ([exn:fail? (λ (e) (void))])
                            (backend-delete! sub provider-name))))
                      ;; list — merges from all backends
                      (λ (be)
                        (remove-duplicates (apply append
                                                  (for/list ([sub (in-list backends)])
                                                    (with-handlers ([exn:fail? (λ (e) '())])
                                                      (backend-list-providers sub))))))
                      ;; available? — true if any backend available
                      (λ (be)
                        (for/or ([sub (in-list backends)])
                          (with-handlers ([exn:fail? (λ (e) #f)])
                            (backend-available? sub))))))

;; ═══════════════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════════════

(define (non-empty-string? v)
  (and (string? v) (> (string-length (string-trim v)) 0)))

(define (shell-escape s)
  (string-replace s "'" "'\\''"))
