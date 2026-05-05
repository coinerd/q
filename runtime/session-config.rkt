#lang racket/base

;; runtime/session-config.rkt — typed config wrapper replacing mutable hasheq anti-pattern
;;
;; A session-config wraps an immutable hash and implements gen:dict,
;; enabling transparent dict-ref/dict-set/dict-remove access from consumers.
;;
;; Design:
;;   - Wraps a single immutable hash (session-config-data)
;;   - gen:dict delegates all operations to the internal hash
;;   - 27 smart accessor functions (config-*) provide typed, default-aware access
;;   - hash->session-config and session-config->hash for conversion
;;   - Full iteration protocol (dict-iterate-first/next/key/value) for for/hash
;;
;; Migration note (v0.30.4–v0.30.5):
;;   All runtime/ consumer files now use dict-ref instead of hash-ref.
;;   hash-ref does NOT dispatch to gen:dict in Racket; only dict-ref does.
;;   Wiring files (run-modes, run-interactive, run-json-rpc) still use raw hashes
;;   which is correct — those configs are CLI-derived, not session-configs.
;;
;; Providers consumed: agent-session.rkt, session-lifecycle.rkt,
;;   session-compaction.rkt, iteration.rkt, turn-orchestrator.rkt,
;;   tool-coordinator.rkt, iteration/retry-policy.rkt

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
         config-templates config-trace-logger config-verbose?
         config-max-tokens config-token-budget-threshold config-session-index)

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
(define (config-max-tokens c) (hash-ref (session-config-data c) 'max-tokens 8192))
(define (config-token-budget-threshold c) (hash-ref (session-config-data c) 'token-budget-threshold #f))
(define (config-session-index c) (hash-ref (session-config-data c) 'session-index #f))

;; ── Conversion helpers ───────────────────────────────────────────

(define (hash->session-config h)
  (session-config (if (hash? h) (make-immutable-hash (hash->list h)) (hash))))

(define (session-config->hash c)
  (make-immutable-hash (hash->list (session-config-data c))))
