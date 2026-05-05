#lang racket/base

;; tests/test-session-config.rkt — unit tests for session-config struct + gen:dict
;;
;; v0.30.3 W0: 20+ tests for dict interface round-trip.

(require rackunit
         racket/dict
         "../runtime/session-config.rkt")

;; ── Constructor tests ────────────────────────────────────────────

(test-case "session-config constructor with all #f"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq)))
  (check-false (session-config-provider c))
  (check-false (session-config-model-name c))
  (check-equal? (hash-count (session-config-extra c)) 0))

(test-case "session-config constructor with values"
  (define c (session-config 'prov 'reg 'bus 'ext 'mreg 'sett
                            "gpt-4" "/tmp/s" "/tmp/p" "/home" "/cfg"
                            '() 128000 10 20 'medium
                            #f 3 #f 5 2
                            '() #f #t (hasheq)))
  (check-eq? (session-config-provider c) 'prov)
  (check-equal? (session-config-model-name c) "gpt-4")
  (check-equal? (session-config-max-context-tokens c) 128000))

;; ── gen:dict: dict-ref ──────────────────────────────────────────

(test-case "dict-ref on struct fields"
  (define c (session-config 'my-prov #f #f #f #f #f
                            "model-1" #f #f #f #f
                            #f #f #f #f 'high
                            #f #f #f #f #f
                            #f #f #f (hasheq)))
  (check-eq? (dict-ref c 'provider) 'my-prov)
  (check-equal? (dict-ref c 'model-name) "model-1")
  (check-eq? (dict-ref c 'thinking-level) 'high))

(test-case "dict-ref with default for missing struct field"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq)))
  ;; Struct fields always exist — #f is a valid value, default not used
  (check-false (dict-ref c 'provider #f))
  (check-false (dict-ref c 'model-name "default"))
  ;; Extra key that doesn't exist → default is used
  (check-equal? (dict-ref c 'nonexistent-key "default") "default"))

(test-case "dict-ref on extra keys"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq 'custom-key 'custom-val)))
  (check-eq? (dict-ref c 'custom-key) 'custom-val))

(test-case "dict-ref error on unknown key without default"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq)))
  (check-exn exn:fail?
             (lambda () (dict-ref c 'nonexistent))))

;; ── gen:dict: dict-set ──────────────────────────────────────────

(test-case "dict-set on struct field returns new struct"
  (define c1 (session-config #f #f #f #f #f #f
                              "old" #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f (hasheq)))
  (define c2 (dict-set c1 'model-name "new"))
  (check-equal? (session-config-model-name c1) "old")
  (check-equal? (session-config-model-name c2) "new"))

(test-case "dict-set on extra key"
  (define c1 (session-config #f #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f (hasheq)))
  (define c2 (dict-set c1 'extra-key 42))
  (check-equal? (dict-ref c2 'extra-key) 42))

(test-case "dict-set chain preserves values"
  (define c1 (session-config #f #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f (hasheq)))
  (define c2 (dict-set c1 'provider 'p))
  (define c3 (dict-set c2 'model-name "m"))
  (define c4 (dict-set c3 'max-iterations 5))
  (check-eq? (dict-ref c4 'provider) 'p)
  (check-equal? (dict-ref c4 'model-name) "m")
  (check-equal? (dict-ref c4 'max-iterations) 5))

;; ── gen:dict: dict-has-key? ─────────────────────────────────────

(test-case "dict-has-key? on struct fields"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq)))
  (check-true (dict-has-key? c 'provider))
  (check-true (dict-has-key? c 'model-name))
  (check-false (dict-has-key? c 'no-such-key)))

(test-case "dict-has-key? on extra"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq 'x 1)))
  (check-true (dict-has-key? c 'x)))

;; ── gen:dict: dict-remove ───────────────────────────────────────

(test-case "dict-remove resets struct field to #f"
  (define c1 (session-config 'prov #f #f #f #f #f
                              "m" #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f (hasheq)))
  (define c2 (dict-remove c1 'provider))
  (check-false (session-config-provider c2)))

(test-case "dict-remove on extra key"
  (define c1 (session-config #f #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f #f #f
                              #f #f #f (hasheq 'x 1 'y 2)))
  (define c2 (dict-remove c1 'x))
  (check-false (dict-has-key? c2 'x))
  (check-equal? (dict-ref c2 'y) 2))

;; ── gen:dict: dict-keys ─────────────────────────────────────────

(test-case "dict-keys includes all 24 fields"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq)))
  (check-equal? (length (dict-keys c)) 24))

(test-case "dict-keys includes extra keys"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq 'a 1 'b 2)))
  (check-equal? (length (dict-keys c)) 26))

;; ── gen:dict: dict-count ────────────────────────────────────────

(test-case "dict-count"
  (define c (session-config #f #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f #f #f
                            #f #f #f (hasheq 'a 1)))
  (check-equal? (dict-count c) 25))

;; ── Conversion: hash→session-config→hash round-trip ────────────

(test-case "hash->session-config preserves known keys"
  (define h (make-hash (list (cons 'provider 'p)
                             (cons 'model-name "m")
                             (cons 'max-iterations 10))))
  (define c (hash->session-config h))
  (check-eq? (session-config-provider c) 'p)
  (check-equal? (session-config-model-name c) "m")
  (check-equal? (session-config-max-iterations c) 10))

(test-case "hash->session-config puts unknown keys in extra"
  (define h (make-hash (list (cons 'custom 'val)
                             (cons 'provider 'p))))
  (define c (hash->session-config h))
  (check-eq? (session-config-provider c) 'p)
  (check-equal? (hash-ref (session-config-extra c) 'custom) 'val))

(test-case "session-config->hash round-trip"
  (define h1 (make-hash (list (cons 'provider 'p)
                              (cons 'model-name "m")
                              (cons 'custom 'val))))
  (define c (hash->session-config h1))
  (define h2 (session-config->hash c))
  (check-eq? (hash-ref h2 'provider) 'p)
  (check-equal? (hash-ref h2 'model-name) "m")
  (check-eq? (hash-ref h2 'custom) 'val))
