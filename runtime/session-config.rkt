#lang racket/base

;; runtime/session-config.rkt — typed session configuration struct
;;
;; Replaces the mutable hasheq anti-pattern (44+ hash-ref sites across
;; runtime/ and wiring/) with a proper struct that implements gen:dict
;; for full backward compatibility.
;;
;; v0.30.3 W0: Struct definition + gen:dict interface.
;;
;; NOTE (v0.30.3 W1 discovery): Racket's hash-ref does NOT dispatch to
;; gen:dict. All 44+ consumer sites use hash-ref, not dict-ref. Full
;; wiring requires migrating sites to dict-ref (v0.30.4+ scope). The
;; struct + dict interface is ready for incremental adoption.

(require racket/dict
         (only-in "../agent/event-bus.rkt" event-bus?)
         (only-in "../extensions/api.rkt" extension-registry?)
         (only-in "../tools/tool.rkt" tool-registry?)
         (only-in "../util/cancellation.rkt" cancellation-token?))

(provide (struct-out session-config)
         session-config?
         hash->session-config
         session-config->hash)

;; ── session-config struct ────────────────────────────────────────
;;
;; 24 fields covering all keys accessed at runtime.
;; All fields are optional (#f by default) to allow staged construction
;; during build-runtime-from-cli.

(struct session-config
  (;; Core infrastructure
   provider                             ; LLM provider
   tool-registry                        ; ToolRegistry
   event-bus                            ; EventBus
   extension-registry                   ; ExtRegistry
   model-registry                       ; ModelRegistry
   settings                             ; q-settings?

   ;; Identity / paths
   model-name
   session-dir
   project-dir
   home-dir
   config-path

   ;; Prompts / instructions
   system-instructions

   ;; Token / budget controls
   max-context-tokens
   max-iterations
   max-iterations-hard
   thinking-level

   ;; Tool / extension controls
   working-set
   parallel-tools
   cancellation-token

   ;; Tier B/C limits
   tier-b-count
   tier-c-count

   ;; Extra keys (catch-all for rare keys)
   templates
   trace-logger
   verbose?
   extra)                               ; remaining keys

  #:transparent
  #:methods gen:dict
  [(define (dict-ref c key [default (lambda ()
                                       (raise-arguments-error
                                        'session-config-ref
                                        (format "key not found: ~a" key)
                                        "key" key))])
     (session-config-ref c key default))
   (define (dict-set c key val)
     (session-config-set c key val))
   (define (dict-remove c key)
     (session-config-remove c key))
   (define (dict-has-key? c key)
     (session-config-has-key? c key))
   (define (dict-keys c)
     (session-config-keys c))
   (define (dict-count c)
     (+ 24 (hash-count (session-config-extra c))))])

;; ── Key → field mapping ──────────────────────────────────────────

;; Sentinel for "key is a struct field" vs "key is in extra"
(define struct-field-keys
  '(provider tool-registry event-bus extension-registry
    model-registry settings model-name session-dir
    project-dir home-dir config-path system-instructions
    max-context-tokens max-iterations max-iterations-hard
    thinking-level working-set parallel-tools
    cancellation-token tier-b-count tier-c-count
    templates trace-logger verbose?))

(define (session-config-ref c key [default #f])
  (cond
    [(memq key struct-field-keys)
     (case key
       [(provider)             (session-config-provider c)]
       [(tool-registry)        (session-config-tool-registry c)]
       [(event-bus)            (session-config-event-bus c)]
       [(extension-registry)   (session-config-extension-registry c)]
       [(model-registry)       (session-config-model-registry c)]
       [(settings)             (session-config-settings c)]
       [(model-name)           (session-config-model-name c)]
       [(session-dir)          (session-config-session-dir c)]
       [(project-dir)          (session-config-project-dir c)]
       [(home-dir)             (session-config-home-dir c)]
       [(config-path)          (session-config-config-path c)]
       [(system-instructions)  (session-config-system-instructions c)]
       [(max-context-tokens)   (session-config-max-context-tokens c)]
       [(max-iterations)       (session-config-max-iterations c)]
       [(max-iterations-hard)  (session-config-max-iterations-hard c)]
       [(thinking-level)       (session-config-thinking-level c)]
       [(working-set)          (session-config-working-set c)]
       [(parallel-tools)       (session-config-parallel-tools c)]
       [(cancellation-token)   (session-config-cancellation-token c)]
       [(tier-b-count)         (session-config-tier-b-count c)]
       [(tier-c-count)         (session-config-tier-c-count c)]
       [(templates)            (session-config-templates c)]
       [(trace-logger)         (session-config-trace-logger c)]
       [(verbose?)             (session-config-verbose? c)])]
    [(hash-has-key? (session-config-extra c) key)
     (hash-ref (session-config-extra c) key)]
    [(procedure? default) (default)]
    [else default]))

(define (session-config-set c key val)
  (case key
    [(provider)             (struct-copy session-config c [provider val])]
    [(tool-registry)        (struct-copy session-config c [tool-registry val])]
    [(event-bus)            (struct-copy session-config c [event-bus val])]
    [(extension-registry)   (struct-copy session-config c [extension-registry val])]
    [(model-registry)       (struct-copy session-config c [model-registry val])]
    [(settings)             (struct-copy session-config c [settings val])]
    [(model-name)           (struct-copy session-config c [model-name val])]
    [(session-dir)          (struct-copy session-config c [session-dir val])]
    [(project-dir)          (struct-copy session-config c [project-dir val])]
    [(home-dir)             (struct-copy session-config c [home-dir val])]
    [(config-path)          (struct-copy session-config c [config-path val])]
    [(system-instructions)  (struct-copy session-config c [system-instructions val])]
    [(max-context-tokens)   (struct-copy session-config c [max-context-tokens val])]
    [(max-iterations)       (struct-copy session-config c [max-iterations val])]
    [(max-iterations-hard)  (struct-copy session-config c [max-iterations-hard val])]
    [(thinking-level)       (struct-copy session-config c [thinking-level val])]
    [(working-set)          (struct-copy session-config c [working-set val])]
    [(parallel-tools)       (struct-copy session-config c [parallel-tools val])]
    [(cancellation-token)   (struct-copy session-config c [cancellation-token val])]
    [(tier-b-count)         (struct-copy session-config c [tier-b-count val])]
    [(tier-c-count)         (struct-copy session-config c [tier-c-count val])]
    [(templates)            (struct-copy session-config c [templates val])]
    [(trace-logger)         (struct-copy session-config c [trace-logger val])]
    [(verbose?)             (struct-copy session-config c [verbose? val])]
    [else (struct-copy session-config c
                       [extra (hash-set (session-config-extra c) key val)])]))

(define (session-config-remove c key)
  (case key
    [(provider)             (struct-copy session-config c [provider #f])]
    [(tool-registry)        (struct-copy session-config c [tool-registry #f])]
    [(event-bus)            (struct-copy session-config c [event-bus #f])]
    [(extension-registry)   (struct-copy session-config c [extension-registry #f])]
    [(model-registry)       (struct-copy session-config c [model-registry #f])]
    [(settings)             (struct-copy session-config c [settings #f])]
    [(model-name)           (struct-copy session-config c [model-name #f])]
    [(session-dir)          (struct-copy session-config c [session-dir #f])]
    [(project-dir)          (struct-copy session-config c [project-dir #f])]
    [(home-dir)             (struct-copy session-config c [home-dir #f])]
    [(config-path)          (struct-copy session-config c [config-path #f])]
    [(system-instructions)  (struct-copy session-config c [system-instructions #f])]
    [(max-context-tokens)   (struct-copy session-config c [max-context-tokens #f])]
    [(max-iterations)       (struct-copy session-config c [max-iterations #f])]
    [(max-iterations-hard)  (struct-copy session-config c [max-iterations-hard #f])]
    [(thinking-level)       (struct-copy session-config c [thinking-level #f])]
    [(working-set)          (struct-copy session-config c [working-set #f])]
    [(parallel-tools)       (struct-copy session-config c [parallel-tools #f])]
    [(cancellation-token)   (struct-copy session-config c [cancellation-token #f])]
    [(tier-b-count)         (struct-copy session-config c [tier-b-count #f])]
    [(tier-c-count)         (struct-copy session-config c [tier-c-count #f])]
    [(templates)            (struct-copy session-config c [templates #f])]
    [(trace-logger)         (struct-copy session-config c [trace-logger #f])]
    [(verbose?)             (struct-copy session-config c [verbose? #f])]
    [else (struct-copy session-config c
                       [extra (hash-remove (session-config-extra c) key)])]))

(define (session-config-has-key? c key)
  (or (and (memq key struct-field-keys) #t)
      (hash-has-key? (session-config-extra c) key)))

(define (session-config-keys c)
  (append '(provider tool-registry event-bus extension-registry
            model-registry settings model-name session-dir
            project-dir home-dir config-path system-instructions
            max-context-tokens max-iterations max-iterations-hard
            thinking-level working-set parallel-tools
            cancellation-token tier-b-count tier-c-count
            templates trace-logger verbose?)
          (hash-keys (session-config-extra c))))

;; ── Conversion helpers ───────────────────────────────────────────

(define (hash->session-config h)
  (session-config
   (hash-ref h 'provider #f)
   (hash-ref h 'tool-registry #f)
   (hash-ref h 'event-bus #f)
   (hash-ref h 'extension-registry #f)
   (hash-ref h 'model-registry #f)
   (hash-ref h 'settings #f)
   (hash-ref h 'model-name #f)
   (hash-ref h 'session-dir #f)
   (hash-ref h 'project-dir #f)
   (hash-ref h 'home-dir #f)
   (hash-ref h 'config-path #f)
   (hash-ref h 'system-instructions #f)
   (hash-ref h 'max-context-tokens #f)
   (hash-ref h 'max-iterations #f)
   (hash-ref h 'max-iterations-hard #f)
   (hash-ref h 'thinking-level #f)
   (hash-ref h 'working-set #f)
   (hash-ref h 'parallel-tools #f)
   (hash-ref h 'cancellation-token #f)
   (hash-ref h 'tier-b-count #f)
   (hash-ref h 'tier-c-count #f)
   (hash-ref h 'templates #f)
   (hash-ref h 'trace-logger #f)
   (hash-ref h 'verbose? #f)
   ;; Everything else goes into extra
   (for/hash ([(k v) (in-hash h)]
              #:when (not (memq k '(provider tool-registry event-bus
                                    extension-registry model-registry settings
                                    model-name session-dir project-dir home-dir
                                    config-path system-instructions
                                    max-context-tokens max-iterations
                                    max-iterations-hard thinking-level
                                    working-set parallel-tools
                                    cancellation-token tier-b-count tier-c-count
                                    templates trace-logger verbose?))))
     (values k v))))

(define (session-config->hash c)
  (for/fold ([h (hash-copy (session-config-extra c))])
            ([key (in-list '(provider tool-registry event-bus
                            extension-registry model-registry settings
                            model-name session-dir project-dir home-dir
                            config-path system-instructions
                            max-context-tokens max-iterations
                            max-iterations-hard thinking-level
                            working-set parallel-tools
                            cancellation-token tier-b-count tier-c-count
                            templates trace-logger verbose?))]
             [acc (in-list (list (session-config-provider c)
                                 (session-config-tool-registry c)
                                 (session-config-event-bus c)
                                 (session-config-extension-registry c)
                                 (session-config-model-registry c)
                                 (session-config-settings c)
                                 (session-config-model-name c)
                                 (session-config-session-dir c)
                                 (session-config-project-dir c)
                                 (session-config-home-dir c)
                                 (session-config-config-path c)
                                 (session-config-system-instructions c)
                                 (session-config-max-context-tokens c)
                                 (session-config-max-iterations c)
                                 (session-config-max-iterations-hard c)
                                 (session-config-thinking-level c)
                                 (session-config-working-set c)
                                 (session-config-parallel-tools c)
                                 (session-config-cancellation-token c)
                                 (session-config-tier-b-count c)
                                 (session-config-tier-c-count c)
                                 (session-config-templates c)
                                 (session-config-trace-logger c)
                                 (session-config-verbose? c)))])
    (hash-set! h key acc)
    h))
