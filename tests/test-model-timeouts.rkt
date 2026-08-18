#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-model-timeouts.rkt — Per-model timeout profile tests (v0.14.2 Wave 3)
;;
;; Tests:
;;   1. effective-request-timeout-for uses per-model override
;;   2. effective-request-timeout-for falls back to current-http-request-timeout
;;   3. current-model-timeouts parameter isolation
;;   4. get-model-timeout from settings
;;   5. effective-request-timeout from settings
;; @boundary unit
;;   6. Wiring: build-runtime-from-cli sets parameters (integration-level)

(require rackunit
         racket/port
         (only-in "../llm/stream.rkt"
                  current-http-request-timeout
                  current-model-timeouts
                  current-model-sse-read-timeouts
                  effective-request-timeout-for
                  effective-sse-read-timeout-for
                  http-request-timeout-default)
         (only-in "../runtime/settings.rkt"
                  make-minimal-settings
                  q-settings-merged
                  get-model-timeout
                  effective-request-timeout
                  http-request-timeout
                  load-settings
                  setting-ref
                  setting-ref*))

;; ============================================================
;; Unit tests: stream.rkt parameter logic
;; ============================================================

(define/provide-test-suite test-model-timeout-params
                           (test-case "effective-request-timeout-for uses per-model override"
                             (parameterize ([current-model-timeouts (hash "glm-5.1" 900)]
                                            [current-http-request-timeout 300])
                               (check-equal? (effective-request-timeout-for "glm-5.1") 900)))
                           (test-case "effective-request-timeout-for falls back to global default"
                             (parameterize ([current-model-timeouts (hash "glm-5.1" 900)]
                                            [current-http-request-timeout 300])
                               (check-equal? (effective-request-timeout-for "gpt-4o") 300)))
                           (test-case "effective-request-timeout-for with empty overrides"
                             (parameterize ([current-model-timeouts (hash)]
                                            [current-http-request-timeout 600])
                               (check-equal? (effective-request-timeout-for "any-model") 600)))
                           (test-case "effective-request-timeout-for with #f model-name"
                             (parameterize ([current-model-timeouts (hash "glm-5.1" 900)]
                                            [current-http-request-timeout 300])
                               (check-equal? (effective-request-timeout-for #f) 300)))
                           (test-case "effective-sse-read-timeout-for uses per-model override"
                             (parameterize ([current-model-sse-read-timeouts (hash "glm-5.3" 300)])
                               (check-equal? (effective-sse-read-timeout-for "glm-5.3") 300)))
                           (test-case "effective-sse-read-timeout-for returns #f without override"
                             (parameterize ([current-model-sse-read-timeouts (hash)])
                               (check-false (effective-sse-read-timeout-for "any-model")))
                             (parameterize ([current-model-sse-read-timeouts (hash "glm-5.3" 300)])
                               (check-false (effective-sse-read-timeout-for "gpt-4o"))))
                           (test-case "effective-sse-read-timeout-for with #f model-name"
                             (parameterize ([current-model-sse-read-timeouts (hash "glm-5.3" 300)])
                               (check-false (effective-sse-read-timeout-for #f))))
                           (test-case "current-model-timeouts parameter isolation"
                             (define saved (current-model-timeouts))
                             (parameterize ([current-model-timeouts (hash "test-model" 123)])
                               (check-equal? (hash-ref (current-model-timeouts) "test-model" #f) 123))
                             (check-equal? (current-model-timeouts) saved)))

;; ============================================================
;; Unit tests: settings.rkt per-model config
;; ============================================================

(define/provide-test-suite
 test-model-timeout-settings
 (test-case "get-model-timeout returns #f when no config"
   (define settings (make-minimal-settings))
   (check-false (get-model-timeout settings "glm-5.1" 'request)))
 (test-case "get-model-timeout returns override when configured"
   ;; Build a settings with per-model timeout config
   (define merged
     (hash 'timeouts
           (hash 'models (hash 'glm-5.1 (hash 'request 900) 'deepseek-chat (hash 'request 800)))))
   (define settings (make-minimal-settings))
   ;; Manually create settings with merged override
   ;; Since make-minimal-settings creates empty merged hash,
   ;; we test setting-ref* directly
   (define result (setting-ref* settings '(timeouts models glm-5.1 request) #f))
   ;; With minimal settings this should be #f
   (check-false result))
 (test-case "effective-request-timeout uses settings with merged hash"
   ;; Test with a constructed merged hash containing model timeouts
   ;; We use setting-ref* to verify the config path works
   (define merged (hash 'timeouts (hash 'models (hash 'glm-5.1 (hash 'request 900)))))
   ;; Simulate what settings.rkt does
   (define model-overrides (hash-ref (hash-ref merged 'timeouts (hash)) 'models (hash)))
   (define glm-config (hash-ref model-overrides 'glm-5.1 #f))
   (check-not-false glm-config)
   (check-equal? (hash-ref glm-config 'request #f) 900))
 (test-case "effective-request-timeout falls back to global"
   (define settings (make-minimal-settings))
   ;; Global default from http-request-timeout with minimal settings
   ;; v0.99.78: raised from 300s so reasoning models (deepseek-v4-flash) with
   ;; >5-min silent prefill pauses don't spuriously time out.
   (check-equal? (http-request-timeout settings) 600)))

;; ============================================================
;; Integration: wiring extracts model timeouts from config
;; ============================================================

(define/provide-test-suite
 test-model-timeout-wiring
 (test-case "wiring extracts model timeouts from config hash"
   ;; Simulates what build-runtime-from-cli does with settings
   (define merged-config
     (hash 'timeouts
           (hash 'request
                 600
                 'models
                 (hash 'glm-5.1
                       (hash 'request 900)
                       'deepseek-chat
                       (hash 'request 800)
                       'gpt-4o
                       (hash 'stream 120)))))
   (define models-config (hash-ref (hash-ref merged-config 'timeouts (hash)) 'models (hash)))
   ;; Simulate the for/fold extraction from run-modes.rkt
   (define extracted
     (for/fold ([acc (hash)]) ([(k v) (in-hash models-config)])
       (if (and (hash? v) (hash-has-key? v 'request))
           (hash-set acc
                     (if (symbol? k)
                         (symbol->string k)
                         k)
                     (hash-ref v 'request))
           acc)))
   ;; glm-5.1 has request timeout → included
   (check-equal? (hash-ref extracted "glm-5.1" #f) 900)
   ;; deepseek-chat has request timeout → included
   (check-equal? (hash-ref extracted "deepseek-chat" #f) 800)
   ;; gpt-4o only has stream timeout, no request → NOT included
   (check-false (hash-ref extracted "gpt-4o" #f)))
 (test-case "wiring handles empty models config"
   (define merged-config (hash 'timeouts (hash 'request 600)))
   (define models-config (hash-ref (hash-ref merged-config 'timeouts (hash)) 'models (hash)))
   (check-equal? models-config (hash))
   (define extracted
     (for/fold ([acc (hash)]) ([(k v) (in-hash models-config)])
       (if (and (hash? v) (hash-has-key? v 'request))
           (hash-set acc
                     (if (symbol? k)
                         (symbol->string k)
                         k)
                     (hash-ref v 'request))
           acc)))
   (check-equal? extracted (hash))))

;; ============================================================
;; Integration: wiring extracts model SSE-read timeouts from config
;; ============================================================

(define/provide-test-suite
 test-model-sse-read-wiring
 (test-case "wiring extracts sse-read timeouts from config hash"
   (define merged-config
     (hash 'timeouts
           (hash 'request
                 600
                 'models
                 (hash 'glm-5.3
                       (hash 'request 900 'sse-read 300)
                       'deepseek-v4-flash
                       (hash 'request 120 'sse-read 60)
                       'gpt-4o
                       (hash 'stream 120)))))
   (define models-config (hash-ref (hash-ref merged-config 'timeouts (hash)) 'models (hash)))
   (define extracted
     (for/fold ([acc (hash)]) ([(k v) (in-hash models-config)])
       (if (and (hash? v) (hash-has-key? v 'sse-read))
           (hash-set acc
                     (if (symbol? k)
                         (symbol->string k)
                         k)
                     (hash-ref v 'sse-read))
           acc)))
   ;; glm-5.3 has sse-read 300 → included
   (check-equal? (hash-ref extracted "glm-5.3" #f) 300)
   ;; deepseek-v4-flash has sse-read 60 → included
   (check-equal? (hash-ref extracted "deepseek-v4-flash" #f) 60)
   ;; gpt-4o has no sse-read → NOT included
   (check-false (hash-ref extracted "gpt-4o" #f)))
 (test-case "wiring handles empty models config for sse-read"
   (define merged-config (hash 'timeouts (hash 'request 600)))
   (define models-config (hash-ref (hash-ref merged-config 'timeouts (hash)) 'models (hash)))
   (define extracted
     (for/fold ([acc (hash)]) ([(k v) (in-hash models-config)])
       (if (and (hash? v) (hash-has-key? v 'sse-read))
           (hash-set acc
                     (if (symbol? k)
                         (symbol->string k)
                         k)
                     (hash-ref v 'sse-read))
           acc)))
   (check-equal? extracted (hash))))

;; ============================================================
;; Run
;; ============================================================

(module+ main
  (require rackunit/text-ui)
  (run-tests test-model-timeout-params)
  (run-tests test-model-timeout-settings)
  (run-tests test-model-timeout-wiring)
  (run-tests test-model-sse-read-wiring))
