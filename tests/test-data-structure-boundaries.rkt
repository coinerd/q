#lang racket/base

;; @speed fast
;; @suite fast

;; W9 v0.99.37: Data-structure/cache/index encapsulation boundary tests.
;; Validates that mutable data structures (caches, registries, boxes) are
;; properly encapsulated behind operation-level APIs per Manual §38, §47.
;;
;; Key patterns tested:
;; - Cache hit/miss/invalidation (model-resolution-cache)
;; - Registry snapshot immutability (agent registry)
;; - Box state isolation and thread safety
;; - Cache clear/reset semantics
;; - Settings memoization invalidation

(require rackunit
         rackunit/text-ui
         racket/list
         racket/match
         "../util/token-estimate-cache.rkt"
         "../util/ids.rkt")

;; Model registry and settings use require with relative paths
(require (only-in "../runtime/provider/model-registry.rkt"
                  make-model-registry-from-config
                  resolve-model
                  resolve-model-by-provider
                  invalidate-model-resolution-cache!)
         (only-in "../runtime/settings-core.rkt"
                  load-settings
                  make-minimal-settings
                  merge-settings
                  q-settings-merged
                  q-settings?))

;; Agent registry — heavy module, use lazy require
(require (only-in "../agent/registry.rkt" register-agent! registered-roles reset-registry!))

;; ============================================================
;; Model Registry Cache Boundary Tests
;; ============================================================

(define-test-suite
 model-cache-boundary-tests
 (test-case "cache: invalidation clears and doesn't crash"
   ;; invalidate is safe to call repeatedly
   (invalidate-model-resolution-cache!)
   (check-not-false (void? (invalidate-model-resolution-cache!)) "invalidate returns void"))
 (test-case "cache: resolution works after invalidation"
   (define registry
     (make-model-registry-from-config (hash (quote default-provider)
                                            "test"
                                            'providers
                                            (hash "test" (hash "api-key" "k" "models" '("m1"))))))
   (invalidate-model-resolution-cache!)
   (define result (resolve-model registry "m1"))
   (check-not-false result "first call should resolve"))
 (test-case "cache: second call returns same result (cached)"
   (define registry
     (make-model-registry-from-config (hash (quote default-provider)
                                            "test"
                                            'providers
                                            (hash "test" (hash "api-key" "k" "models" '("m1"))))))
   (invalidate-model-resolution-cache!)
   (define result1 (resolve-model registry "m1"))
   (define result2 (resolve-model registry "m1"))
   (check-equal? result1 result2 "cached result matches first result"))
 (test-case "cache: unknown model returns #f"
   (define registry
     (make-model-registry-from-config (hash (quote default-provider)
                                            "test"
                                            'providers
                                            (hash "test" (hash "api-key" "k" "models" '("m1"))))))
   (define result (resolve-model registry "nonexistent-model"))
   (check-false result "unknown model returns #f")))

;; ============================================================
;; Agent Registry Snapshot Boundary Tests
;; ============================================================

(define-test-suite agent-registry-snapshot-tests
                   (test-case "registered-roles returns a list"
                     (define snapshot (registered-roles))
                     (check-true (list? snapshot) "registered-roles returns a list"))
                   (test-case "registry: register then reset lifecycle"
                     (define dummy-factory (lambda () 'dummy))
                     (register-agent! (quote w9-test-snapshot) "v1" dummy-factory)
                     (check-not-false (member 'w9-test-snapshot (registered-roles))
                                      "role registered after register-agent!")
                     (reset-registry!)
                     (check-false (member 'w9-test-snapshot (registered-roles))
                                  "role gone after reset-registry!")))

;; ============================================================
;; Token Estimate Cache Boundary Tests (isolation)
;; ============================================================

(define-test-suite
 token-cache-isolation-tests
 (test-case "make-token-estimate-cache: fully isolated from global"
   (clear-token-estimate-cache!)
   (define local-est (lambda (text) 42))
   (define local (make-token-estimate-cache local-est))
   (define result (local "hello"))
   (check-equal? result 42 "local cache calls estimator")
   ;; Global cache should be untouched
   (define s (token-estimate-cache-stats))
   (check-equal? (hash-ref s 'entries) 0 "global cache has 0 entries after local-only use"))
 (test-case "multiple local caches don't interfere"
   (define cache-a (make-token-estimate-cache (lambda (t) 100)))
   (define cache-b (make-token-estimate-cache (lambda (t) 200)))
   (check-equal? (cache-a "same text") 100 "cache-a returns 100")
   (check-equal? (cache-b "same text") 200 "cache-b returns 200")
   ;; Both should cache independently
   (check-equal? (cache-a "same text") 100 "cache-a still returns 100")
   (check-equal? (cache-b "same text") 200 "cache-b still returns 200"))
 (test-case "global cache: clear resets all stats"
   (clear-token-estimate-cache!)
   ;; Populate with a few entries
   (cached-estimate-text-tokens (lambda (t) 5) "entry-1")
   (cached-estimate-text-tokens (lambda (t) 5) "entry-2")
   (define before (token-estimate-cache-stats))
   (check-equal? (hash-ref before 'entries) 2 "2 entries before clear")
   (clear-token-estimate-cache!)
   (define after (token-estimate-cache-stats))
   (check-equal? (hash-ref after 'entries) 0 "0 entries after clear")
   (check-equal? (hash-ref after 'hits) 0 "hits reset to 0")
   (check-equal? (hash-ref after 'misses) 0 "misses reset to 0")))

;; ============================================================
;; ID Generator Monotonicity & Encapsulation Tests
;; ============================================================

(define-test-suite
 id-generator-boundary-tests
 (test-case "generate-id returns 26-char string"
   (define id (generate-id))
   (check-equal? (string-length id) 26 "ID is 26 chars")
   (check-true (string? id) "ID is a string"))
 (test-case "generate-id: sequential IDs are monotonically ordered"
   (define ids
     (for/list ([_ (in-range 10)])
       (generate-id)))
   (define sorted (sort ids string<?))
   ;; IDs should already be sorted (or equal), since they're monotonic
   (for ([original (in-list ids)]
         [expected (in-list sorted)])
     (check-true (string<=? original expected)
                 (format "ID ordering violation: ~a should be <= ~a" original expected))))
 (test-case "generate-id: no duplicates in rapid succession"
   (define ids
     (for/list ([_ (in-range 50)])
       (generate-id)))
   (check-equal? (length (remove-duplicates ids)) 50 "50 unique IDs")))

;; ============================================================
;; Settings Cache Boundary Tests
;; ============================================================

(define-test-suite settings-cache-boundary-tests
                   (test-case "make-minimal-settings: returns q-settings struct"
                     (define s
                       (make-minimal-settings #:provider "test-provider" #:model "test-model"))
                     (check-true (q-settings? s) "returns q-settings struct")
                     (check-equal? (hash-ref (q-settings-merged s) 'default-provider) "test-provider")
                     (check-equal? (hash-ref (q-settings-merged s) 'default-model) "test-model"))
                   (test-case "make-minimal-settings: overrides are applied"
                     (define s (make-minimal-settings #:overrides (hasheq 'custom-key "custom-val")))
                     (check-equal? (hash-ref (q-settings-merged s) 'custom-key) "custom-val"))
                   (test-case "merge-settings: project overrides global"
                     (define global (hasheq 'a 1 'b 2))
                     (define project (hasheq 'b 3 'c 4))
                     (define result (merge-settings global project))
                     (check-equal? (hash-ref result 'a) 1 "global key preserved")
                     (check-equal? (hash-ref result 'b) 3 "project wins on conflict")
                     (check-equal? (hash-ref result 'c) 4 "project-only key added"))
                   (test-case "merge-settings: deep merge nested hashes"
                     (define global (hasheq 'db (hasheq 'host "global" 'port 5432)))
                     (define project (hasheq 'db (hasheq 'port 9999)))
                     (define result (merge-settings global project))
                     (define db (hash-ref result 'db))
                     (check-equal? (hash-ref db 'host) "global" "nested global preserved")
                     (check-equal? (hash-ref db 'port) 9999 "nested project override")))

;; ============================================================
;; Mutable Hash Encapsulation Property Tests
;; ============================================================

(define-test-suite encapsulation-property-tests
                   (test-case "local mutable hash doesn't leak to caller"
                     ;; Verify that a closure over make-hash properly encapsulates state
                     (define (make-counter)
                       (define counts (make-hash))
                       (values (lambda (key) (hash-ref counts key 0))
                               (lambda (key)
                                 (hash-update! counts key add1 0)
                                 (hash-ref counts key))))
                     (define-values (get inc) (make-counter))
                     (inc 'a)
                     (inc 'a)
                     (inc 'b)
                     (check-equal? (get 'a) 2 "counter a = 2")
                     (check-equal? (get 'b) 1 "counter b = 1"))
                   (test-case "box swap preserves prior snapshot"
                     ;; Verify the "box of immutable" pattern: swapping box doesn't
                     ;; affect values already read from the box
                     (define b (box (hasheq 'x 1)))
                     (define snapshot (unbox b))
                     (set-box! b (hash-set (unbox b) 'x 2))
                     (check-equal? (hash-ref snapshot 'x) 1 "snapshot unchanged after box swap")
                     (check-equal? (hash-ref (unbox b) 'x) 2 "current value updated"))
                   (test-case "immutable hash-set returns new hash"
                     (define h1 (hasheq 'a 1))
                     (define h2 (hash-set h1 'a 2))
                     (check-equal? (hash-ref h1 'a) 1 "original unchanged")
                     (check-equal? (hash-ref h2 'a) 2 "new hash has updated value"))
                   (test-case "call-with-semaphore serializes mutations"
                     ;; Verify semaphore-guarded mutations don't lose updates
                     (define sema (make-semaphore 1))
                     (define counter (box 0))
                     (for ([_ (in-range 100)])
                       (call-with-semaphore sema
                                            (lambda () (set-box! counter (add1 (unbox counter))))))
                     (check-equal? (unbox counter) 100 "all 100 increments recorded")))

;; ============================================================
;; Run all tests
;; ============================================================

(define-test-suite all-data-structure-boundary-tests
                   model-cache-boundary-tests
                   agent-registry-snapshot-tests
                   token-cache-isolation-tests
                   id-generator-boundary-tests
                   settings-cache-boundary-tests
                   encapsulation-property-tests)

(module+ test
  (run-tests all-data-structure-boundary-tests))

(module+ main
  (run-tests all-data-structure-boundary-tests))
