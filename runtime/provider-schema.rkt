#lang racket/base

;; runtime/provider-schema.rkt — Load built-in provider definitions from providers.rktd
;;
;; Reads the declarative provider schema and populates a provider-registry
;; with placeholder entries and model metadata. The factory layer creates
;; real provider instances on demand when credentials are available.
;;
;; #5242: providers.rktd schema
;; #5243: dynamic provider creation from registry

(require racket/contract
         racket/list
         racket/match
         racket/runtime-path
         "provider-registry.rkt"
         "../llm/provider.rkt")

(provide (contract-out
          [load-providers-schema (->* () ((or/c path-string? path?)) list?)]
          [load-builtin-providers!
           (->* (provider-registry?) ((or/c path-string? path?)) exact-nonnegative-integer?)]
          [list-builtin-provider-names (-> (listof string?))]
          [provider-is-placeholder? (-> provider-info? boolean?)]
          [builtin-provider-config (-> provider-registry? string? (or/c hash? #f))])
         validate-provider-entry)

;; Runtime path to the schema file
(define-runtime-path providers-schema-file "providers.rktd")

;; Validate a single provider schema entry.
;; Returns #t if valid, or a string error message.
;; Required: car is a symbol, (base-url . string?) and (default-model . string?).
(define (validate-provider-entry entry)
  (cond
    [(not (pair? entry)) "entry must be a pair"]
    [(not (symbol? (car entry))) (format "provider name must be a symbol, got: ~a" (car entry))]
    [else
     (define def (cdr entry))
     (define base-url (pfield def 'base-url #f))
     (define default-model (pfield def 'default-model #f))
     (define models (pfield def 'models '()))
     (cond
       [(not (string? base-url)) (format "provider ~a: base-url must be a string" (car entry))]
       [(not (string? default-model))
        (format "provider ~a: default-model must be a string" (car entry))]
       [(not (list? models)) (format "provider ~a: models must be a list" (car entry))]
       [else #t])]))

;; Load the raw provider schema as an alist.
;; Validates entries — malformed entries are skipped with a warning.
(define (load-providers-schema [path providers-schema-file])
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define raw (call-with-input-file path read))
    (for/list ([entry (in-list (if (list? raw)
                                   raw
                                   '()))]
               #:when (let ([v (validate-provider-entry entry)])
                        (when (string? v)
                          (log-warning "provider-schema: skipping malformed entry: ~a" v))
                        (eq? v #t)))
      entry)))

;; Extract a field from a provider definition alist
(define (pfield def field [default #f])
  (let ([entry (assoc field def)])
    (if entry
        (cdr entry)
        default)))

;; Parse a model entry from the schema into individual fields.
;; Model entry format: ("model-id" (name . "...") (context-window . N) (capabilities . (sym ...)))
(define (parse-model-entry entry)
  (define model-id
    (if (string? (car entry))
        (car entry)
        (pfield entry 'id)))
  (define rest
    (if (string? (car entry))
        (cdr entry)
        entry))
  (define model-name (pfield rest 'name model-id))
  (define context-window (pfield rest 'context-window #f))
  (define caps-raw (pfield rest 'capabilities '()))
  (define capabilities
    (for/hash ([s (in-list (if (list? caps-raw)
                               caps-raw
                               (list caps-raw)))])
      (values s #t)))
  (values model-id model-name context-window capabilities))

;; Placeholder provider for schema-driven entries.
;; The real provider is created on demand by the factory.
(define (make-placeholder-provider name)
  (make-provider (lambda () name)
                 (lambda () (hasheq 'streaming #t 'placeholder #t))
                 (lambda (req)
                   (raise (exn:fail (format "Provider ~a not yet configured — set API key first" name)
                                    (current-continuation-marks))))
                 (lambda (req)
                   (raise (exn:fail (format "Provider ~a not yet configured — set API key first" name)
                                    (current-continuation-marks))))))

;; Load built-in provider definitions from schema and register them.
;; Returns the number of providers registered.
(define (load-builtin-providers! registry [path providers-schema-file])
  (define schema (load-providers-schema path))
  (for/fold ([count 0]) ([entry (in-list schema)])
    (define prov-name (symbol->string (car entry)))
    (define def (cdr entry))
    (define base-url (pfield def 'base-url ""))
    (define default-model (pfield def 'default-model ""))
    (define auth-type (pfield def 'auth-type 'api-key))
    (define factory-sym (pfield def 'factory 'openai-compatible))
    (define models-raw (pfield def 'models '()))
    ;; Register provider with schema metadata
    (define metadata
      (hasheq 'base-url
              base-url
              'default-model
              default-model
              'auth-type
              auth-type
              'factory
              factory-sym
              'builtin
              #t))
    (register-provider! registry prov-name (make-placeholder-provider prov-name) #:config metadata)
    ;; Register models for this provider
    (for ([m-entry (in-list models-raw)])
      (define-values (m-id m-name m-cw m-caps) (parse-model-entry m-entry))
      (when m-id
        (register-model! registry
                         #:id m-id
                         #:name m-name
                         #:provider-name prov-name
                         #:context-window m-cw
                         #:capabilities m-caps)))
    (add1 count)))

;; Check if a registered provider is still a placeholder.
(define (provider-is-placeholder? pinfo)
  (define prov (provider-info-provider pinfo))
  (define caps (provider-capabilities prov))
  (and (hash? caps) (hash-ref caps 'placeholder #f)))

;; Get the config hash for a built-in provider by name.
(define (builtin-provider-config registry name)
  (define pinfo (lookup-provider registry name))
  (and pinfo (provider-info-config pinfo)))

;; List schema-defined provider names.
(define (list-builtin-provider-names)
  (map (lambda (entry) (symbol->string (car entry))) (load-providers-schema)))
