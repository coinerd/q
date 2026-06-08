#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-facade-surface.rkt — Tests for facade module curation
;;
;; v0.29.6 W2: Verify facade modules have intentional surfaces.

(require rackunit
         racket/port
         racket/file
         racket/runtime-path)

(define-runtime-path q-root-raw "..")
(define q-root (simplify-path q-root-raw))
(define (q-file . parts)
  (apply build-path q-root parts))

;; ============================================================
;; 1. Facade modules with all-from-out are intentional
;; ============================================================

(test-case "protocol-types.rkt re-exports 8 sub-modules (intentional facade)"
  (define content (call-with-input-file (q-file "util" "message" "protocol-types.rkt") port->string))
  (define count (length (regexp-match* #rx"all-from-out" content)))
  (check-true (>= count 8)
              (format "expected >= 8 all-from-out in protocol-types.rkt, found ~a" count)))

(test-case "context-assembly.rkt re-exports 3 sub-modules (intentional facade)"
  (define content (call-with-input-file (q-file "runtime" "context" "context-assembly.rkt") port->string))
  (define all-from-count (length (regexp-match* #rx"all-from-out" content)))
  (define explicit-count (length (regexp-match* #rx"provide" content)))
  ;; Either uses all-from-out or explicit provides (S1-F4 refactor)
  (check-true
   (or (>= all-from-count 3) (>= explicit-count 2))
   (format
    "expected facade pattern in context-assembly.rkt (all-from-out>=3 or provide>=2), found ~a/~a"
    all-from-count
    explicit-count)))

(test-case "event-structs.rkt re-exports sub-modules (intentional facade)"
  (define content (call-with-input-file (q-file "agent" "event-structs.rkt") port->string))
  (define all-from-count (length (regexp-match* #rx"all-from-out" content)))
  (define struct-out-count (length (regexp-match* #rx"struct-out" content)))
  ;; After v0.85.0 W2, all-from-out was replaced with explicit provides (struct-out entries)
  (check-true (or (>= all-from-count 6) (>= struct-out-count 6))
              (format "expected >= 6 all-from-out or struct-out in event-structs.rkt, found ~a/~a" all-from-count struct-out-count)))

;; ============================================================
;; 2. Non-facade modules should NOT use all-from-out excessively
;; ============================================================

(test-case "agent/loop.rkt does not use all-from-out"
  (define content (call-with-input-file (q-file "agent" "loop.rkt") port->string))
  (check-false (regexp-match? #rx"all-from-out" content)
               "agent/loop.rkt should have explicit provides"))

(test-case "tools/scheduler.rkt does not use all-from-out"
  (define content (call-with-input-file (q-file "tools" "scheduler.rkt") port->string))
  (check-false (regexp-match? #rx"all-from-out" content)
               "tools/scheduler.rkt should have explicit provides"))

;; ============================================================
;; 3. SDK surface is explicit
;; ============================================================

(test-case "interfaces/sdk.rkt re-exports sdk-core and sdk-compat"
  (define content (call-with-input-file (q-file "interfaces" "sdk.rkt") port->string))
  (check-not-false (regexp-match? #rx"all-from-out.*sdk-core" content))
  (check-not-false (regexp-match? #rx"all-from-out.*sdk-compat" content)))

(test-case "interfaces/sdk-public.rkt has explicit provides"
  (define content (call-with-input-file (q-file "interfaces" "sdk-public.rkt") port->string))
  (check-not-false (regexp-match? #rx"provide" content)))

;; ============================================================
;; 4. Total all-from-out count (baseline)
;; ============================================================

(test-case "total all-from-out count is documented"
  (define source-dirs
    (list (build-path q-root "runtime")
          (build-path q-root "interfaces")
          (build-path q-root "agent")
          (build-path q-root "tools")
          (build-path q-root "extensions")
          (build-path q-root "util")))
  (define total
    (for/sum ([dir source-dirs])
             (for/sum ([f
                        (in-list (find-files (lambda (p)
                                               (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                                    (not (regexp-match? #rx"test" (path->string p)))
                                                    (not (regexp-match? #rx"compiled"
                                                                        (path->string p)))))
                                             dir))])
                      (with-handlers ([exn:fail? (lambda (_) 0)])
                        (define content (call-with-input-file f port->string))
                        (length (regexp-match* #rx"all-from-out" content))))))
  ;; Baseline: ~30-50 all-from-out (mostly in facades)
  (check-true (and (>= total 20) (<= total 100))
              (format "all-from-out total: ~a (expected 20-100 range)" total)))
