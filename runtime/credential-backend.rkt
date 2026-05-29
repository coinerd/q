#lang racket/base

;; runtime/credential-backend.rkt — Credential backend abstraction layer
;;
;; Issue #1291: GAP-02a — Pluggable credential backends
;; Issue #1292: GAP-02b — OS keychain integration
;;
;; Provides a unified interface for storing/retrieving credentials from
;; different backends: file (JSON), environment variables, memory (testing),
;; OS keychain (secret-tool / macOS security), and chained (fallback chain).

(require "../util/json-helpers.rkt"
         "../util/error-helpers.rkt"
         racket/string
         racket/function)
(require "../util/errors.rkt")
(require racket/contract
         json
         racket/file
         racket/string
         racket/path
         racket/match
         racket/list
         racket/port)

;; Command runner seam for keychain backends (mockable for tests)
(provide current-external-command-runner

         ;; Backend struct
         credential-backend
         credential-backend?
         credential-backend-name
         credential-backend-store-fn
         credential-backend-load-fn
         credential-backend-delete-fn
         credential-backend-list-fn
         credential-backend-available?-fn
         ;; Credential policy (v0.70.1)
         credential-policy?
         valid-credential-policies
         credential-backend-capabilities
         (contract-out
          [make-file-credential-backend (->* () ((or/c path-string? #f)) credential-backend?)]
          [make-env-credential-backend (-> credential-backend?)]
          [make-memory-credential-backend (-> credential-backend?)]
          [make-keychain-credential-backend (-> credential-backend?)]
          [make-chained-credential-backend (-> (listof credential-backend?) credential-backend?)]
          [backend-name (-> credential-backend? string?)]
          [backend-store! (-> credential-backend? string? string? void?)]
          [backend-load
           (->* (credential-backend? string?) (#:env-var (or/c string? #f)) (or/c hash? #f))]
          [backend-delete! (-> credential-backend? string? void?)]
          [backend-list-providers (-> credential-backend? (listof string?))]
          [backend-available? (-> credential-backend? boolean?)]
          [make-policy-aware-backend
           (->* (credential-backend?)
                (#:policy (or/c 'auto 'keychain-preferred 'keychain-required 'env-only)
                          #:warn-port (or/c output-port? #f))
                credential-backend?)]
          [make-macos-keychain-credential-backend (-> credential-backend?)]))

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
     (with-safe-fallback (hash)
                         (define content (read-json-file path))
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
     (and dir (or (directory-exists? dir) (with-safe-fallback #f (make-directory* dir) #t)))]))

(define (file-write-atomic! path data)
  (define dir (path-only path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir))
  (define tmp (make-temporary-file "credential-~a.tmp" #f (or dir (find-system-path 'temp-dir))))
  (with-handlers ([exn:fail? (λ (e)
                               (with-safe-fallback (void) (delete-file tmp))
                               (raise e))])
    (write-json-file tmp data)
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
  (credential-backend
   "env"
   ;; store! — read-only, raises error
   (λ (be provider-name api-key)
     (raise-credential-error "Environment backend is read-only" "env" "store-disabled"))
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

;; ═══════════════════════════════════════════════════════════════════
;; Command runner seam — injectable for testing
;; ═══════════════════════════════════════════════════════════════════

;; Default: runs command via subprocess. Returns (values exit-code stdout-string).
;; Override for testing: (parameterize ([current-external-command-runner mock-fn]) ...)
(define current-external-command-runner
  (make-parameter (λ (executable-path args #:stdin [stdin-str #f])
                    (with-safe-fallback (values 1 "")
                                        (define-values (sp out-port in-port err-port)
                                          (subprocess #f #f #f executable-path args))
                                        (when stdin-str
                                          (display stdin-str in-port)
                                          (close-output-port in-port))
                                        (define out (open-output-string))
                                        (copy-port out-port out)
                                        (close-input-port out-port)
                                        (close-input-port err-port)
                                        (values (subprocess-status sp) (get-output-string out))))))

;; Key attribute for secret-tool: q-agent/provider/<name>
(define (keychain-attrs provider-name)
  (list (cons "application" "q-agent") (cons "provider" provider-name)))

(define (keychain-label provider-name)
  (format "q-agent: ~a" provider-name))

;; Run secret-tool via the injectable command runner.
;; Returns (values exit-code stdout-string).
(define (run-secret-tool args #:stdin [stdin-str #f])
  ((current-external-command-runner) (find-executable-path "secret-tool") args #:stdin stdin-str))

(define (secret-tool-available?)
  (with-safe-fallback #f (define-values (status _) (run-secret-tool '("--version"))) (= status 0)))

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
    (raise-credential-error "secret-tool not available" "keychain" "missing-secret-tool"))
  (define attrs (keychain-attrs provider-name))
  (define args
    (append* (list "store" "--label" (keychain-label provider-name))
             (for/list ([a (in-list attrs)])
               (list (format "--~a" (car a)) (cdr a)))))
  (define-values (status _) (run-secret-tool args #:stdin (string-append api-key "\n")))
  (unless (= status 0)
    (raise-credential-error (format "secret-tool store failed for ~a" provider-name)
                            "keychain"
                            "store-failed")))

(define (keychain-load provider-name)
  (unless (secret-tool-available?)
    #f)
  (define attrs (keychain-attrs provider-name))
  (define args
    (cons "lookup"
          (append* (for/list ([a (in-list attrs)])
                     (list (format "--~a" (car a)) (cdr a))))))
  (define-values (status out) (run-secret-tool args))
  (define result (string-trim out))
  (if (and (= status 0) (non-empty-string? result))
      (hasheq 'api-key result 'source "keychain" 'provider provider-name)
      #f))

(define (keychain-delete! provider-name)
  (unless (secret-tool-available?)
    (void))
  (define attrs (keychain-attrs provider-name))
  (define args
    (cons "clear"
          (append* (for/list ([a (in-list attrs)])
                     (list (format "--~a" (car a)) (cdr a))))))
  (run-secret-tool args)
  (void))

(define (keychain-list-providers)
  (unless (secret-tool-available?)
    '())
  ;; Search for all q-agent secrets
  (define-values (status out) (run-secret-tool '("search" "--all" "application" "q-agent")))
  ;; Parse provider names from output
  (for/list ([line (in-list (string-split out "\n"))]
             #:when (string-contains? line "provider"))
    (define m (regexp-match #rx"provider *= *(.+)" line))
    (if m
        (string-trim (cadr m))
        "unknown")))

;; ═══════════════════════════════════════════════════════════════════
;; Chained backend — tries multiple backends in order
;; ═══════════════════════════════════════════════════════════════════

(define (make-chained-credential-backend backends)
  (credential-backend
   "chained"
   ;; store! — writes to first writable backend
   (λ (be provider-name api-key)
     (define stored #f)
     (for ([sub (in-list backends)]
           #:break stored)
       (with-safe-fallback (void) (backend-store! sub provider-name api-key) (set! stored #t)))
     (unless stored
       (raise-credential-error "No writable backend available" "chained" "all-backends-readonly")))
   ;; load — tries each backend in order, returns first hit
   (λ (be provider-name env-var)
     (for/or ([sub (in-list backends)])
       (with-safe-fallback #f (backend-load sub provider-name #:env-var (or env-var #f)))))
   ;; delete! — deletes from all backends
   (λ (be provider-name)
     (for ([sub (in-list backends)])
       (with-safe-fallback (void) (backend-delete! sub provider-name))))
   ;; list — merges from all backends
   (λ (be)
     (remove-duplicates (apply append
                               (for/list ([sub (in-list backends)])
                                 (with-safe-fallback '() (backend-list-providers sub))))))
   ;; available? — true if any backend available
   (λ (be)
     (for/or ([sub (in-list backends)])
       (with-safe-fallback #f (backend-available? sub))))))

;; ═══════════════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════════════

(define (shell-escape s)
  (string-replace s "'" "'\\''"))

;; ═══════════════════════════════════════════════════════════════════
;; Credential policy (v0.70.1)
;; ═══════════════════════════════════════════════════════════════════

;; Valid policy modes
(define valid-credential-policies '(auto keychain-preferred keychain-required env-only))

(define (credential-policy? v)
  (and (symbol? v) (member v valid-credential-policies) #t))

;; Policy-aware backend wrapper.
;; Wraps an inner backend (typically chained) with policy enforcement.
;;
;; 'auto               — pass-through, no enforcement
;; 'keychain-preferred — warn when falling back to file/env for store
;; 'keychain-required  — raise on file store, warn on file load
;; 'env-only           — raise on file store, file load returns #f
(define (make-policy-aware-backend inner
                                   #:policy [policy 'auto]
                                   #:warn-port [warn-port (current-error-port)])
  (case policy
    [(auto) inner]
    [else
     (define (emit-warning msg)
       (when warn-port
         (fprintf warn-port "WARNING [credential-policy ~a]: ~a\n" policy msg)))
     (define (file-fallback-violation action)
       (raise-credential-error
        (format "~a forbidden by credential policy '~a' — file fallback not allowed" action policy)
        "policy"
        (format "~a-forbidden" action)))
     (credential-backend
      (format "policy-aware(~a, ~a)" policy (backend-name inner))
      ;; store!
      (λ (be provider-name api-key)
        (define inner-name (backend-name inner))
        (cond
          [(eq? policy 'keychain-required)
           (when (member inner-name '("file" "chained"))
             (file-fallback-violation "store"))
           (backend-store! inner provider-name api-key)]
          [(eq? policy 'env-only)
           (when (member inner-name '("file" "chained" "keychain"))
             (file-fallback-violation "store"))
           (backend-store! inner provider-name api-key)]
          [(eq? policy 'keychain-preferred)
           (when (member inner-name '("file"))
             (emit-warning
              (format "storing credential for '~a' in file backend — consider using keychain or env" provider-name)))
           (backend-store! inner provider-name api-key)]
          [else (backend-store! inner provider-name api-key)]))
      ;; load
      (λ (be provider-name env-var)
        (define inner-name (backend-name inner))
        (cond
          [(eq? policy 'env-only)
           (if (member inner-name '("file" "keychain" "chained"))
               (begin
                 (emit-warning
                  (format "loading credential for '~a' from '~a' blocked by env-only policy" provider-name inner-name))
                 #f)
               (backend-load inner provider-name #:env-var (or env-var #f)))]
          [else
           (define result (backend-load inner provider-name #:env-var (or env-var #f)))
           (when (and result
                      (eq? policy 'keychain-preferred)
                      (member inner-name '("file")))
             (emit-warning
              (format "loaded credential for '~a' from file backend — consider using keychain" provider-name)))
           result]))
      ;; delete!
      (λ (be provider-name) (backend-delete! inner provider-name))
      ;; list
      (λ (be) (backend-list-providers inner))
      ;; available?
      (λ (be) (backend-available? inner)))]))

;; ═══════════════════════════════════════════════════════════════════
;; Platform capability matrix (v0.70.2)
;; ═══════════════════════════════════════════════════════════════════

;; Returns a hash describing which backends are available on the
;; current platform, based on actual command availability checks.
(define (credential-backend-capabilities)
  (define runner (current-external-command-runner))
  (define (cmd-available? cmd)
    (with-handlers ([exn:fail? (λ (_) #f)])
      (define out (open-output-string))
      (runner (format "which ~a 2>/dev/null" cmd) out)
      (positive? (string-length (get-output-string out)))))
  (hash 'env (backend-available? (make-env-credential-backend))
        'file #t
        'memory #t
        'keychain-linux (cmd-available? "secret-tool")
        'keychain-macos (cmd-available? "security")
        'keychain-windows (cmd-available? "cmdkey")
        'platform (symbol->string (system-type 'os))))

;; ═══════════════════════════════════════════════════════════════════
;; macOS security backend (v0.70.2)
;; ═══════════════════════════════════════════════════════════════════

;; Uses the macOS `security` command-line tool to store/load/delete
;; credentials in the macOS Keychain. Fully mockable via
;; current-external-command-runner.

(define (run-security-command args)
  (define runner (current-external-command-runner))
  (define out (open-output-string))
  (define err (open-output-string))
  (define cmd (format "security ~a 2>&1" args))
  (define result (runner cmd out))
  (values (get-output-string out) result))

(define (make-macos-keychain-credential-backend)
  (credential-backend
   "macos-keychain"
   ;; store!
   (λ (be provider-name api-key)
     (define-values (out ok?) (run-security-command
                               (format "add-generic-password -a '~a' -s 'q-credential-~a' -w '~a' -U"
                                       (getenv "USER") provider-name api-key)))
     (unless ok?
       (raise-credential-error
        (format "macOS keychain store failed for '~a'" provider-name)
        "macos-keychain" provider-name)))
   ;; load
   (λ (be provider-name env-var)
     (define-values (out ok?) (run-security-command
                               (format "find-generic-password -a '~a' -s 'q-credential-~a' -w"
                                       (getenv "USER") provider-name)))
     (if ok?
         (hash 'api-key (string-trim out) 'provider provider-name 'source "macos-keychain")
         #f))
   ;; delete!
   (λ (be provider-name)
     (define-values (out ok?) (run-security-command
                               (format "delete-generic-password -a '~a' -s 'q-credential-~a'"
                                       (getenv "USER") provider-name)))
     (void))
   ;; list
   (λ (be)
     ;; Keychain doesn't support enumeration easily; return empty
     '())
   ;; available?
   (λ (be)
     (with-handlers ([exn:fail? (λ (_) #f)])
       (define runner (current-external-command-runner))
       (define out (open-output-string))
       (runner "which security 2>/dev/null" out)
       (positive? (string-length (get-output-string out)))))))
