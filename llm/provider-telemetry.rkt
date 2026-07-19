#lang racket/base
;; STABILITY: public

;; llm/provider-telemetry.rkt — provider-neutral lifecycle provenance (v0.99.52 W5)
;;
;; Links the q-side identity chain (session-id, turn-id, q-request-id,
;; configured provider/model, adapter) to provider-NATIVE identity extracted
;; from actual transport responses (native-request-id, native-response-id,
;; native-model).
;;
;; Design (Finding F-07/A-06):
;;   - Identity is exact-chain and provider-response correlated, never inferred
;;     or locally asserted. A telemetry record carries an explicit `source`
;;     discriminator: 'provider-native (real transport response), 'mock
;;     (in-process test provider), or 'local-invented (synthetic, e.g. a
;;     spawn-subagent bridge response with no upstream provider call).
;;   - The correlation verifier (`telemetry-chain-valid?`) accepts a started
;;     -> completed -> terminal chain only when the q-side identity is
;;     consistent AND the completed record carries non-empty native identity
;;     from a 'provider-native source. Empty / mock / mismatched / unrelated /
;;     locally-invented identity FAILS.
;;   - Records are versioned (schema-version 2) and serialize to canonical
;;     jsexpr for the production telemetry chain.

(require racket/contract
         racket/string)

(provide provider-telemetry
         provider-telemetry?
         provider-telemetry-schema-version
         provider-telemetry-session-id
         provider-telemetry-turn-id
         provider-telemetry-q-request-id
         provider-telemetry-adapter
         provider-telemetry-configured-provider
         provider-telemetry-configured-model
         provider-telemetry-native-request-id
         provider-telemetry-native-response-id
         provider-telemetry-native-model
         provider-telemetry-source
         provider-telemetry-stage
         (contract-out
          [make-provider-telemetry
           (->* (#:session-id string?
                              #:turn-id string?
                              #:q-request-id string?
                              #:adapter string?
                              #:configured-provider string?
                              #:configured-model string?
                              #:stage (or/c 'started 'completed 'terminal))
                (#:native-request-id (or/c string? #f)
                                     #:native-response-id (or/c string? #f)
                                     #:native-model (or/c string? #f)
                                     #:source (or/c 'provider-native 'mock 'local-invented))
                provider-telemetry?)]
          [provider-telemetry->jsexpr (-> provider-telemetry? hash?)]
          [jsexpr->provider-telemetry (-> hash? provider-telemetry?)]
          [provider-native-provenance? (-> provider-telemetry? boolean?)]
          [telemetry-chain-valid?
           (->* (provider-telemetry? provider-telemetry?) (provider-telemetry?) boolean?)]
          [gen-q-request-id (-> string?)]
          [response-native-identity
           (->* (#:adapter string? #:native-response-id (or/c string? #f))
                (#:native-model (or/c string? #f) #:native-request-id (or/c string? #f))
                (or/c hash? #f))])
         CURRENT-PROVIDER-TELEMETRY-SCHEMA-VERSION)

;; ============================================================
;; Schema version
;; ============================================================

(define CURRENT-PROVIDER-TELEMETRY-SCHEMA-VERSION 2)

;; ============================================================
;; Record
;; ============================================================

(struct provider-telemetry
        (schema-version session-id
                        turn-id
                        q-request-id
                        adapter
                        configured-provider
                        configured-model
                        native-request-id
                        native-response-id
                        native-model
                        source
                        stage)
  #:transparent)

(define (make-provider-telemetry #:session-id session-id
                                 #:turn-id turn-id
                                 #:q-request-id q-request-id
                                 #:adapter adapter
                                 #:configured-provider configured-provider
                                 #:configured-model configured-model
                                 #:stage stage
                                 #:native-request-id [native-request-id #f]
                                 #:native-response-id [native-response-id #f]
                                 #:native-model [native-model #f]
                                 #:source [source 'provider-native])
  (provider-telemetry CURRENT-PROVIDER-TELEMETRY-SCHEMA-VERSION
                      session-id
                      turn-id
                      q-request-id
                      adapter
                      configured-provider
                      configured-model
                      native-request-id
                      native-response-id
                      native-model
                      source
                      stage))

;; ============================================================
;; Serialization (round-trip)
;; ============================================================

(define (provider-telemetry->jsexpr t)
  (hasheq 'schemaVersion
          (provider-telemetry-schema-version t)
          'sessionId
          (provider-telemetry-session-id t)
          'turnId
          (provider-telemetry-turn-id t)
          'qRequestId
          (provider-telemetry-q-request-id t)
          'adapter
          (provider-telemetry-adapter t)
          'configuredProvider
          (provider-telemetry-configured-provider t)
          'configuredModel
          (provider-telemetry-configured-model t)
          'nativeRequestId
          (or (provider-telemetry-native-request-id t) "")
          'nativeResponseId
          (or (provider-telemetry-native-response-id t) "")
          'nativeModel
          (or (provider-telemetry-native-model t) "")
          'source
          (symbol->string (provider-telemetry-source t))
          'stage
          (symbol->string (provider-telemetry-stage t))))

(define (jsexpr->provider-telemetry h)
  (provider-telemetry (hash-ref h 'schemaVersion CURRENT-PROVIDER-TELEMETRY-SCHEMA-VERSION)
                      (hash-ref h 'sessionId "")
                      (hash-ref h 'turnId "")
                      (hash-ref h 'qRequestId "")
                      (hash-ref h 'adapter "")
                      (hash-ref h 'configuredProvider "")
                      (hash-ref h 'configuredModel "")
                      (let ([v (hash-ref h 'nativeRequestId #f)]) (if (equal? v "") #f v))
                      (let ([v (hash-ref h 'nativeResponseId #f)]) (if (equal? v "") #f v))
                      (let ([v (hash-ref h 'nativeModel #f)]) (if (equal? v "") #f v))
                      (let ([s (hash-ref h 'source "provider-native")])
                        (if (string? s)
                            (string->symbol s)
                            s))
                      (let ([s (hash-ref h 'stage "completed")])
                        (if (string? s)
                            (string->symbol s)
                            s))))

;; ============================================================
;; Provenance discriminator
;; ============================================================

;; provider-native-provenance? : provider-telemetry? -> boolean?
;; #t only when the record carries provider-native identity extracted from an
;; actual transport response: source is 'provider-native AND native-response-id
;; is non-empty. Mock / local-invented / empty-native records are NOT native.
(define (provider-native-provenance? t)
  (and (eq? (provider-telemetry-source t) 'provider-native)
       (non-empty-id? (provider-telemetry-native-response-id t))))

(define (non-empty-id? v)
  (and (string? v) (> (string-length (string-trim v)) 0)))

;; ============================================================
;; Correlation verifier (Test 3)
;; ============================================================

;; telemetry-chain-valid? : provider-telemetry? provider-telemetry? [provider-telemetry?] -> boolean?
;;
;; Accepts a started -> completed -> terminal(?) chain only when:
;;   1. q-side identity (session-id, turn-id, q-request-id) is consistent
;;      across all supplied records, and every one is non-empty;
;;   2. the completed record carries provider-native identity
;;      (provider-native-provenance? is #t).
;;
;; Empty / mock / mismatched / unrelated / locally-invented identity FAILS.
(define (telemetry-chain-valid? started completed [terminal #f])
  (and (identity-consistent? started completed)
       (or (not terminal) (identity-consistent? started terminal))
       (every-non-empty-q-identity? (if terminal
                                        (list started completed terminal)
                                        (list started completed)))
       (provider-native-provenance? completed)))

(define (identity-consistent? a b)
  (and (equal? (provider-telemetry-session-id a) (provider-telemetry-session-id b))
       (equal? (provider-telemetry-turn-id a) (provider-telemetry-turn-id b))
       (equal? (provider-telemetry-q-request-id a) (provider-telemetry-q-request-id b))))

(define (every-non-empty-q-identity? ts)
  (for/and ([t (in-list ts)])
    (and (non-empty-id? (provider-telemetry-session-id t))
         (non-empty-id? (provider-telemetry-turn-id t))
         (non-empty-id? (provider-telemetry-q-request-id t)))))

;; ============================================================
;; Response-identity helper (used by adapters)
;; ============================================================

;; response-native-identity : #:adapter #:native-response-id [#:native-model #:native-request-id] -> (or/c hash? #f)
;; Returns a provenance hash only when a non-empty native response id was extracted
;; from the actual transport response; #f otherwise (so make-model-response keeps
;; provenance #f and the chain correctly reports non-native).
(define (response-native-identity #:adapter adapter
                                  #:native-response-id native-response-id
                                  #:native-model [native-model #f]
                                  #:native-request-id [native-request-id #f])
  (if (non-empty-id? native-response-id)
      (hasheq 'adapter
              adapter
              'native-response-id
              native-response-id
              'native-model
              (or native-model "")
              'native-request-id
              (or native-request-id ""))
      #f))

;; ============================================================
;; q-request-id generator
;; ============================================================

;; gen-q-request-id : -> string?
;; Generates a fresh q-side request id. Pure string, no external state.
(define q-request-counter (box 0))

(define (gen-q-request-id)
  (define n (unbox q-request-counter))
  (set-box! q-request-counter (add1 n))
  (format "qrq-~a-~a" (current-inexact-milliseconds) n))
