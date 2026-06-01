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

(require racket/dict
         racket/contract
         (only-in "../runtime/working-set.rkt" working-set?)
         (only-in "../llm/provider.rkt" provider?)
         (only-in "layer-adapters.rkt" tool-registry?)
         (only-in "../agent/event-bus.rkt" event-bus?)
         (only-in "layer-adapters.rkt" extension-registry?)
         (only-in "../runtime/model-registry.rkt" model-registry?)
         (only-in "../runtime/trace-logger.rkt" trace-logger?)
         (only-in "../runtime/settings.rkt" q-settings?)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../runtime/session-index/schema.rkt" session-index?)
         (only-in "context-assembly/state-aware-builder.rkt"
                  current-task-state-aware-assembly?
                  current-conclusion-token-budget))

;; Global rollout rate for state-aware assembly (0.0 = none, 1.0 = all)
(define current-task-state-aware-rollout-rate (make-parameter 0.0))

;; v0.77.7 W7.1: Graduated activation profile.
;; Profiles control which context assembly features are active:
;;   'off           — everything disabled (safe default)
;;   'observe       — telemetry only, no behavior change
;;   'bounded       — budget + ranking active, no auto-distill/rollback
;;   'self-healing  — bounded + auto-distill + rollback actions (behind flags)
;;   'full          — all features active (not recommended yet)
(define current-context-assembly-profile (make-parameter 'off))

(define valid-profiles '(off observe bounded self-healing full))

(define (context-assembly-profile? v)
  (and (symbol? v) (memq v valid-profiles) #t))

;; Apply a profile: sets individual feature flags according to the profile.
(define (apply-context-assembly-profile! profile)
  (case profile
    [(off) (current-task-state-aware-assembly? #f)]
    [(observe) (current-task-state-aware-assembly? #f)] ; telemetry still works
    [(bounded)
     (current-task-state-aware-assembly? #t)
     (current-conclusion-token-budget 2000)]
    [(self-healing)
     (current-task-state-aware-assembly? #t)
     (current-conclusion-token-budget 2000)]
    [(full)
     (current-task-state-aware-assembly? #t)
     (current-conclusion-token-budget 2000)]
    [else (void)]))

(provide session-config?
         current-task-state-aware-rollout-rate
         current-context-assembly-profile
         context-assembly-profile?
         apply-context-assembly-profile!
         ;; Smart accessors with defaults
         (contract-out
          [hash->session-config (-> hash? session-config?)]
          [session-config->hash (-> session-config? hash?)]
          [resolve-max-iterations-hard
           (-> session-config? exact-nonnegative-integer? exact-positive-integer?)]
          [normalize-session-config-hash (-> any/c hash?)]
          [config-provider (-> session-config? (or/c #f provider?))]
          [config-tool-registry (-> session-config? (or/c #f tool-registry?))]
          [config-event-bus (-> session-config? (or/c #f event-bus?))]
          [config-extension-registry (-> session-config? (or/c #f extension-registry?))]
          [config-model-registry (-> session-config? (or/c #f model-registry?))]
          [config-settings (-> session-config? (or/c #f q-settings?))]
          [config-model-name (-> session-config? (or/c #f string?))]
          [config-session-dir (-> session-config? (or/c #f path-string?))]
          [config-project-dir (-> session-config? (or/c #f path-string?))]
          [config-home-dir (-> session-config? (or/c #f path-string?))]
          [config-config-path (-> session-config? (or/c #f path-string?))]
          [config-system-instructions (-> session-config? list?)]
          [config-max-context-tokens (-> session-config? exact-positive-integer?)]
          [config-max-iterations (-> session-config? exact-positive-integer?)]
          [config-max-iterations-hard (-> session-config? (or/c #f exact-positive-integer?))]
          [config-thinking-level (-> session-config? (or/c 'off 'minimal 'low 'medium 'high 'xhigh))]
          [config-working-set (-> session-config? (or/c #f working-set?))]
          [config-parallel-tools (-> session-config? (or/c #f boolean?))]
          [config-cancellation-token (-> session-config? (or/c #f cancellation-token?))]
          [config-tier-b-count (-> session-config? exact-nonnegative-integer?)]
          [config-tier-c-count (-> session-config? exact-nonnegative-integer?)]
          [config-templates (-> session-config? (or/c #f hash?))]
          [config-trace-logger (-> session-config? (or/c #f trace-logger?))]
          [config-verbose? (-> session-config? boolean?)]
          [config-max-tokens (-> session-config? exact-positive-integer?)]
          [config-token-budget-threshold (-> session-config? (or/c #f exact-nonnegative-integer?))]
          [config-session-index (-> session-config? (or/c #f session-index?))]
          [config-task-state-aware? (-> session-config? boolean?)]
          [config-context-assembly-profile (-> session-config? symbol?)]))

;; ── session-config struct ────────────────────────────────────────

(struct session-config (data)
  #:methods gen:dict
  [(define (dict-ref c
                     key
                     [default
                      (lambda ()
                        (raise-arguments-error 'session-config-ref
                                               (format "key not found: ~a" key)
                                               "key"
                                               key))])
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

(define (config-provider c)
  (hash-ref (session-config-data c) 'provider #f))
(define (config-tool-registry c)
  (hash-ref (session-config-data c) 'tool-registry #f))
(define (config-event-bus c)
  (hash-ref (session-config-data c) 'event-bus #f))
(define (config-extension-registry c)
  (hash-ref (session-config-data c) 'extension-registry #f))
(define (config-model-registry c)
  (hash-ref (session-config-data c) 'model-registry #f))
(define (config-settings c)
  (hash-ref (session-config-data c) 'settings #f))
(define (config-model-name c)
  (hash-ref (session-config-data c) 'model-name #f))
(define (config-session-dir c)
  (hash-ref (session-config-data c) 'session-dir #f))
(define (config-project-dir c)
  (hash-ref (session-config-data c) 'project-dir #f))
(define (config-home-dir c)
  (hash-ref (session-config-data c) 'home-dir #f))
(define (config-config-path c)
  (hash-ref (session-config-data c) 'config-path #f))
(define (config-system-instructions c)
  (hash-ref (session-config-data c) 'system-instructions '()))
(define (config-max-context-tokens c)
  (hash-ref (session-config-data c) 'max-context-tokens 128000))
(define (config-max-iterations c)
  (hash-ref (session-config-data c) 'max-iterations 50))
(define (config-max-iterations-hard c)
  (hash-ref (session-config-data c) 'max-iterations-hard #f))
(define (config-thinking-level c)
  (hash-ref (session-config-data c) 'thinking-level 'medium))
(define (config-working-set c)
  (hash-ref (session-config-data c) 'working-set #f))
(define (config-parallel-tools c)
  (hash-ref (session-config-data c) 'parallel-tools #f))
(define (config-cancellation-token c)
  (hash-ref (session-config-data c) 'cancellation-token #f))
(define (config-tier-b-count c)
  (hash-ref (session-config-data c) 'tier-b-count 20))
(define (config-tier-c-count c)
  (hash-ref (session-config-data c) 'tier-c-count 4))
(define (config-templates c)
  (hash-ref (session-config-data c) 'templates #f))
(define (config-trace-logger c)
  (hash-ref (session-config-data c) 'trace-logger #f))
(define (config-verbose? c)
  (hash-ref (session-config-data c) 'verbose? #f))
(define (config-max-tokens c)
  (hash-ref (session-config-data c) 'max-tokens 8192))
(define (config-token-budget-threshold c)
  (hash-ref (session-config-data c) 'token-budget-threshold #f))
(define (config-session-index c)
  (hash-ref (session-config-data c) 'session-index #f))
(define (config-task-state-aware? c)
  (hash-ref (session-config-data c) 'task-state-aware? #f))

;; v0.77.9 T2.4: Profile accessor from config hash
(define (config-context-assembly-profile c)
  (hash-ref (session-config-data c) 'context-assembly-profile 'off))

;; ── Dynamic default resolution ─────────────────────────────────

(define (resolve-max-iterations-hard config max-iterations)
  (or (config-max-iterations-hard config) (max (quotient (* max-iterations 8) 5) 80)))

;; ── Conversion helpers ───────────────────────────────────────────

(define (normalize-session-config-hash h)
  (define known-keys
    '(provider tool-registry
               event-bus
               extension-registry
               model-registry
               settings
               model-name
               session-dir
               project-dir
               home-dir
               config-path
               system-instructions
               max-context-tokens
               max-iterations
               max-iterations-hard
               thinking-level
               working-set
               parallel-tools
               cancellation-token
               tier-b-count
               tier-c-count
               templates
               trace-logger
               verbose?
               max-tokens
               token-budget-threshold
               session-index
               task-state-aware?))
  (define base
    (if (hash? h)
        (make-immutable-hash (hash->list h))
        (hash)))
  (for/fold ([acc base]) ([(k v) (in-hash base)])
    (cond
      [(not (member k known-keys))
       (log-warning "session-config: unknown key ~a, preserving" k)
       acc]
      [(and (eq? k 'thinking-level) (string? v)) (hash-set acc k (string->symbol v))]
      [else acc])))

(define (hash->session-config h)
  (session-config (normalize-session-config-hash h)))

(define (session-config->hash c)
  (make-immutable-hash (hash->list (session-config-data c))))
;; v0.31.x milestone placeholder
