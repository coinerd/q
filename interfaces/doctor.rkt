#lang racket/base

;; interfaces/doctor.rkt — `q doctor` setup and provider diagnostics
;;
;; Validates the local runtime environment and reports actionable
;; setup problems.  Runs without network access.  Each check returns
;; a check-result struct consumed by run-doctor for display.
;;
;; Exit code: 0 if no errors, 1 if any error found.

(require racket/list
         racket/string
         racket/format
         racket/match
         racket/system
         racket/port
         racket/file
         json
         "../runtime/settings.rkt"
         "../runtime/auth-store.rkt"
         "../runtime/model-registry.rkt"
         "../util/config-paths.rkt")

(provide
 ;; Struct
 (struct-out check-result)

 ;; Main entry
 run-doctor

 ;; Individual checks (exported for testing)
 check-racket-version
 check-packages
 check-config-dir
 check-config-file
 check-credentials
 check-session-dir
 check-tui-packages)

;; ============================================================
;; Struct
;; ============================================================

;; status: 'ok | 'warning | 'error
(struct check-result (name status message) #:transparent)

;; ============================================================
;; Internal helpers
;; ============================================================

;; Run a shell command and capture its stdout as a string.
;; Returns #f if the command fails or is not found.
(define (shell-command-output cmd)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define port (open-output-string))
    (define result
      (parameterize ([current-output-port port]
                     [current-error-port (open-output-nowhere)])
        (system cmd)))
    (if result
        (string-trim (get-output-string port))
        #f)))

;; Try to write a small file to a directory to verify writability.
(define (directory-writable? dir)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define tmp (make-temporary-file "q-doctor-~a.tmp" #f dir))
    (delete-file tmp)
    #t))

;; Parse a version string like "8.12.0.4" into a list of integers.
(define (parse-version-string s)
  (with-handlers ([exn:fail? (λ (_) '())])
    (map string->number (string-split (or s "") "."))))

;; Compare two version lists lexicographically.
;; Returns #t if actual >= required.
(define (version>=? actual required)
  (cond
    [(and (null? actual) (null? required)) #t]
    [(null? required) #t]
    [(null? actual) #f]
    [else
     (define a (car actual))
     (define r (car required))
     (cond
       [(> a r) #t]
       [(< a r) #f]
       [else (version>=? (cdr actual) (cdr required))])]))

;; ============================================================
;; Check: Racket version >= 8.10
;; ============================================================

(define (check-racket-version)
  (define output (shell-command-output "racket --version 2>/dev/null"))
  (cond
    [(not output)
     (check-result "Racket" 'error "not found — install Racket >= 8.10")]
    [else
     ;; Parse version from output like "Welcome to Racket v8.12.0.4 [cs]."
     (define m (regexp-match #rx"v([0-9]+\\.[0-9]+(\\.[0-9]+)*)" output))
     (define ver-str (and m (cadr m)))
     (define ver-parts (parse-version-string ver-str))
     (if (version>=? ver-parts '(8 10))
         (check-result "Racket" 'ok (format "v~a" (or ver-str "?")))
         (check-result "Racket" 'error (format "v~a — need >= 8.10" (or ver-str "?"))))]))

;; ============================================================
;; Check: required packages load
;; ============================================================

(define (check-packages)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (check-result "Packages" 'error
                                   (format "failed to load q: ~a"
                                           (exn-message e))))])
    (dynamic-require ''q/main #f)
    (check-result "Packages" 'ok "q modules load successfully")))

;; ============================================================
;; Check: config directory (~/.q/)
;; ============================================================

(define (check-config-dir)
  (define dir (global-config-dir))
  (cond
    [(not (directory-exists? dir))
     (check-result "Config dir" 'warning
                   (format "~a does not exist" dir))]
    [(not (directory-writable? dir))
     (check-result "Config dir" 'error
                   (format "~a exists but is not writable" dir))]
    [else
     (check-result "Config dir" 'ok (format "~a exists, writable" dir))]))

;; ============================================================
;; Check: config file (~/.q/config.json)
;; ============================================================

(define (check-config-file)
  (define dir (global-config-dir))
  (define cfg-path (build-path dir "config.json"))
  (cond
    [(not (file-exists? cfg-path))
     (check-result "Config file" 'warning
                   (format "~a not found (run q once to create)" cfg-path))]
    [else
     (define content
       (with-handlers ([exn:fail? (λ (e) #f)])
         (call-with-input-file cfg-path
           (λ (in) (read-json in)))))
     (cond
       [(not content)
        (check-result "Config file" 'error
                      (format "~a — failed to parse JSON" cfg-path))]
       [(eof-object? content)
        (check-result "Config file" 'error
                      (format "~a — empty file" cfg-path))]
       [(not (hash? content))
        (check-result "Config file" 'error
                      (format "~a — top-level value is not an object" cfg-path))]
       [else
        (check-result "Config file" 'ok (format "~a — valid JSON" cfg-path))])]))

;; ============================================================
;; Check: credentials (API keys)
;; ============================================================

;; Try to load settings and resolve at least one provider with a key.
(define (check-credentials)
  (define settings (load-settings (current-directory)))
  (define merged (q-settings-merged settings))
  ;; Check environment variables directly for common providers
  (define anthropic-key (getenv "ANTHROPIC_API_KEY"))
  (define openai-key (getenv "OPENAI_API_KEY"))
  (define env-keys-found
    (append
     (if (and anthropic-key (> (string-length (string-trim anthropic-key)) 0))
         '("ANTHROPIC_API_KEY") '())
     (if (and openai-key (> (string-length (string-trim openai-key)) 0))
         '("OPENAI_API_KEY") '())))
  ;; Check credential file
  (define cred-file (credential-file-path))
  (define cred-file-exists? (file-exists? cred-file))
  ;; Check config file for provider keys
  (define prov-names (provider-names settings))
  (define configured-providers
    (for/list ([name (in-list prov-names)])
      (define prov-cfg (provider-config settings name))
      (define cred (lookup-credential name prov-cfg))
      (and cred (credential-provider-name cred))))
  (define providers-with-keys
    (filter (lambda (x) x) configured-providers))

  (cond
    [(not (null? env-keys-found))
     (check-result "Credentials" 'ok
                   (format "env: ~a" (string-join env-keys-found ", ")))]
    [(not (null? providers-with-keys))
     (check-result "Credentials" 'ok
                   (format "configured: ~a" (string-join providers-with-keys ", ")))]
    [(and cred-file-exists?)
     (check-result "Credentials" 'warning
                   "credentials file exists but no keys resolved")]
    [(null? prov-names)
     (check-result "Credentials" 'warning
                   "no providers configured — set ANTHROPIC_API_KEY or OPENAI_API_KEY, or edit ~/.q/config.json")]
    [else
     (check-result "Credentials" 'error
                   (format "providers configured (~a) but no API keys found"
                           (string-join (map ~a prov-names) ", ")))]))

;; ============================================================
;; Check: session directory (~/.q/sessions/)
;; ============================================================

(define (check-session-dir)
  (define dir (default-session-dir))
  (cond
    [(not (directory-exists? dir))
     ;; Try to create it
     (with-handlers ([exn:fail?
                      (λ (e)
                        (check-result "Session dir" 'error
                                      (format "~a does not exist and cannot be created"
                                              dir)))])
       (make-directory* dir)
       (check-result "Session dir" 'ok (format "~a created" dir)))]
    [(not (directory-writable? dir))
     (check-result "Session dir" 'error
                   (format "~a exists but is not writable" dir))]
    [else
     (check-result "Session dir" 'ok (format "~a exists, writable" dir))]))

;; ============================================================
;; Check: TUI packages (optional)
;; ============================================================

(define (check-tui-packages)
  (define available?
    (with-handlers ([exn:fail? (λ (_) #f)])
      (dynamic-require 'char-term #f)
      #t))
  (if available?
      (check-result "TUI (char-term)" 'ok "available")
      (check-result "TUI (char-term)" 'warning "not installed — TUI mode will not work (raco pkg install char-term)")))

;; ============================================================
;; run-doctor — main entry point
;; ============================================================

;; Run all diagnostic checks and print results.
;; Returns 0 if no errors, 1 if any error found.
(define (run-doctor)
  (displayln "q doctor — checking your setup...\n")
  (define results
    (list
     (check-racket-version)
     (check-packages)
     (check-config-dir)
     (check-config-file)
     (check-credentials)
     (check-session-dir)
     (check-tui-packages)))
  (for ([r results])
    (match-define (check-result name status message) r)
    (define icon (case status [(ok) "\u2713"] [(warning) "\u26A0"] [(error) "\u2717"]))
    (printf "  ~a ~a: ~a\n" icon name message))
  (define errors (filter (λ (r) (eq? (check-result-status r) 'error)) results))
  (define warnings (filter (λ (r) (eq? (check-result-status r) 'warning)) results))
  (printf "\n  ~a errors, ~a warnings\n" (length errors) (length warnings))
  (if (null? errors) 0 1))
