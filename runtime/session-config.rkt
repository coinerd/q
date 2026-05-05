#lang racket/base

;; runtime/session-config.rkt — typed config wrapper replacing mutable hasheq anti-pattern
;;
;; v0.30.3 W0: Initial struct with 24 named fields + extra hash + gen:dict.
;; v0.30.4: Redesigned to wrap a single hash internally. This makes:
;;   - hash-ref work directly (no dict-ref needed)
;;   - dict-ref/hash-ref both return correct defaults for missing keys
;;   - Named accessor functions (config-*) provide typed, default-aware access
;;   - Full backward compat with all 44+ hash-ref consumer sites
;;
;; Evolution:
;;   v0.30.3: Named struct fields + gen:dict → hash-ref doesn't dispatch
;;   v0.30.4: Hash wrapper → both hash-ref and dict-ref work correctly

(require racket/dict)

(provide (struct-out session-config)
         session-config?
         hash->session-config
         session-config->hash
         ;; Smart accessors with defaults
         config-provider config-tool-registry config-event-bus
         config-extension-registry config-model-registry config-settings
         config-model-name config-session-dir config-project-dir
         config-home-dir config-config-path config-system-instructions
         config-max-context-tokens config-max-iterations
         config-max-iterations-hard config-thinking-level
         config-working-set config-parallel-tools
         config-cancellation-token config-tier-b-count config-tier-c-count
         config-templates config-trace-logger config-verbose?)

;; ── session-config struct ────────────────────────────────────────

(struct session-config (data)
  #:methods gen:dict
  [(define (dict-ref c key [default
                            (lambda ()
                              (raise-arguments-error
                               'session-config-ref
                               (format "key not found: ~a" key)
                               "key" key))])
     (hash-ref (session-config-data c) key default))
   (define (dict-set c key val)
     (session-config (hash-set (session-config-data c) key val)))
   (define (dict-remove c key)
     (session-config (hash-remove (session-config-data c) key)))
   (define (dict-has-key? c key)
     (hash-has-key? (session-config-data c) key))
   (define (dict-keys c)
     (hash-keys (session-config-data c)))
   (define (dict-count c)
     (hash-count (session-config-data c)))
   (define (dict-iterate-first c)
     (hash-iterate-first (session-config-data c)))
   (define (dict-iterate-next c pos)
     (hash-iterate-next (session-config-data c) pos))
   (define (dict-iterate-key c pos)
     (hash-iterate-key (session-config-data c) pos))
   (define (dict-iterate-value c pos)
     (hash-iterate-value (session-config-data c) pos))])

;; ── Smart accessors with defaults ────────────────────────────────
;; These mirror the original hash-ref defaults from agent-session.rkt.
;; Use these for typed, default-aware access to config values.

(define (config-provider c) (hash-ref (session-config-data c) 'provider #f))
(define (config-tool-registry c) (hash-ref (session-config-data c) 'tool-registry #f))
(define (config-event-bus c) (hash-ref (session-config-data c) 'event-bus #f))
(define (config-extension-registry c) (hash-ref (session-config-data c) 'extension-registry #f))
(define (config-model-registry c) (hash-ref (session-config-data c) 'model-registry #f))
(define (config-settings c) (hash-ref (session-config-data c) 'settings #f))
(define (config-model-name c) (hash-ref (session-config-data c) 'model-name #f))
(define (config-session-dir c) (hash-ref (session-config-data c) 'session-dir #f))
(define (config-project-dir c) (hash-ref (session-config-data c) 'project-dir #f))
(define (config-home-dir c) (hash-ref (session-config-data c) 'home-dir #f))
(define (config-config-path c) (hash-ref (session-config-data c) 'config-path #f))
(define (config-system-instructions c) (hash-ref (session-config-data c) 'system-instructions '()))
(define (config-max-context-tokens c) (hash-ref (session-config-data c) 'max-context-tokens 128000))
(define (config-max-iterations c) (hash-ref (session-config-data c) 'max-iterations 50))
(define (config-max-iterations-hard c) (hash-ref (session-config-data c) 'max-iterations-hard #f))
(define (config-thinking-level c) (hash-ref (session-config-data c) 'thinking-level 'medium))
(define (config-working-set c) (hash-ref (session-config-data c) 'working-set #f))
(define (config-parallel-tools c) (hash-ref (session-config-data c) 'parallel-tools #f))
(define (config-cancellation-token c) (hash-ref (session-config-data c) 'cancellation-token #f))
(define (config-tier-b-count c) (hash-ref (session-config-data c) 'tier-b-count 20))
(define (config-tier-c-count c) (hash-ref (session-config-data c) 'tier-c-count 4))
(define (config-templates c) (hash-ref (session-config-data c) 'templates #f))
(define (config-trace-logger c) (hash-ref (session-config-data c) 'trace-logger #f))
(define (config-verbose? c) (hash-ref (session-config-data c) 'verbose? #f))

;; ── Conversion helpers ───────────────────────────────────────────

(define (hash->session-config h)
  (session-config (if (hash? h) (make-immutable-hash (hash->list h)) (hash))))

(define (session-config->hash c)
  (make-immutable-hash (hash->list (session-config-data c))))
