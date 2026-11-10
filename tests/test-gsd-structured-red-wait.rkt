#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; tests/test-gsd-structured-red-wait.rkt — #9897 evidence suite (v1.00.32 W1).
;;
;; The structured-RED probe launch inside capture-red-checkpoint! waited on
;; a HARDCODED 60 s verification-wait with a matching 60 s #:timeout-ms kill.
;; Legitimate probes cost more under load (solo cost 26.5 s; 2 of 4 probes
;; killed in the v1.00.32 W2 canary), so slow hosts were wave-failed on
;; wall-clock alone. The flip under test:
;;
;;   gsd.structured-red-wait-seconds — default 300 (the default-exec-limits
;;   120/300 family), read via setting-ref* against the merged config and
;;   floor-clamped at >= 60 s. ONE derived value drives BOTH the registry
;;   verification-wait and the job's #:timeout-ms, so a slow probe is killed
;;   by the single registry timeout, not double-killed.
;;
;; RED baseline (pre-fix): the settings key and its resolution helpers do
;; not exist. The new bindings are therefore resolved DYNAMICALLY so the
;; RED baseline fails at runtime with clean rackunit failures instead of a
;; module-load error (a failing compile would read as a broken tree).

(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../extensions/gsd/red-checkpoint.rkt"
         (only-in "../runtime/settings.rkt" load-settings make-minimal-settings))

(define-runtime-path red-checkpoint-src "../extensions/gsd/red-checkpoint.rkt")

;; ── The #9897 bindings under test (dynamic: #f before the flip) ──────

(define (red-binding name)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require red-checkpoint-src name)))

(define structured-red-wait-default-seconds (red-binding 'structured-red-wait-default-seconds))
(define structured-red-wait-floor-seconds (red-binding 'structured-red-wait-floor-seconds))
(define structured-red-wait-seconds (red-binding 'structured-red-wait-seconds))
(define structured-red-wait-ms (red-binding 'structured-red-wait-ms))

(define (require-flip)
  (unless (and structured-red-wait-default-seconds
               structured-red-wait-floor-seconds
               structured-red-wait-seconds
               structured-red-wait-ms)
    (fail
     "#9897 flip not implemented: red-checkpoint.rkt does not export the
structured-red-wait settings resolution helpers")))

;; ── Synthetic settings helpers (same on-disk seam as BUG-0044) ────────
;; Writes a .q/config.json shaped like the real project config and loads it
;; via the same load-settings seam the checkpoint uses, so the pins exercise
;; the merged-config path (no source edits required).

(define (make-configured-dir json-content)
  (define dir (make-temporary-file "structured-red-wait-~a" 'directory))
  (define cfg-dir (build-path dir ".q"))
  (make-directory* cfg-dir)
  (call-with-output-file (build-path cfg-dir "config.json")
                         (lambda (out) (display json-content out))
                         #:exists 'truncate)
  dir)

;; The structured-RED launch region of red-checkpoint.rkt: everything
;; between the guard before the structured job and the completion need.
(define (structured-region src)
  (define start-marker "stale guard before structured RED")
  (define end-marker "structured RED job incomplete")
  (define a (regexp-match-positions (regexp-quote start-marker) src))
  (define b (regexp-match-positions (regexp-quote end-marker) src))
  (and a b (substring src (cdar a) (caar b))))

(define structured-red-wait-suite
  (test-suite "#9897: the structured-RED probe wait is settings-derived"

    (test-case "the #9897 flip exists: red-checkpoint exports the resolution helpers"
      (check-true (and structured-red-wait-default-seconds #t))
      (check-true (and structured-red-wait-floor-seconds #t))
      (check-true (and structured-red-wait-seconds #t))
      (check-true (and structured-red-wait-ms #t)))

    (test-case "pinned policy constants: default 300 s, floor 60 s"
      (require-flip)
      (check-equal? structured-red-wait-default-seconds 300)
      (check-equal? structured-red-wait-floor-seconds 60))

    (test-case "absent key (or no settings object) resolves to the 300 s default"
      (require-flip)
      (check-equal? (structured-red-wait-seconds #f) 300)
      (check-equal? (structured-red-wait-seconds (make-minimal-settings)) 300)
      (check-equal? (structured-red-wait-seconds (load-settings (make-configured-dir "{}"))) 300))

    (test-case "configured override is honored through the checkpoint's resolution path"
      (require-flip)
      (define dir (make-configured-dir "{\"gsd\":{\"structured-red-wait-seconds\":450}}"))
      (check-equal? (structured-red-wait-seconds (load-settings dir)) 450)
      ;; end-to-end: exactly the call shape capture-red-checkpoint! uses
      (check-equal? (structured-red-wait-ms dir) 450000.0))

    (test-case "floor clamp: a configured value below 60 s resolves to 60 s"
      (require-flip)
      (for ([seconds (in-list '(30 59))])
        (check-equal?
         (structured-red-wait-seconds
          (load-settings (make-configured-dir (format "{\"gsd\":{\"structured-red-wait-seconds\":~a}}"
                                                      seconds))))
         60))
      ;; the boundary is inclusive: 60 s configured stays 60 s
      (define dir (make-configured-dir "{\"gsd\":{\"structured-red-wait-seconds\":60}}"))
      (check-equal? (structured-red-wait-seconds (load-settings dir)) 60)
      (check-equal? (structured-red-wait-ms dir) 60000.0))

    (test-case "typo'd values keep the default — a bad settings file never crashes a checkpoint"
      (require-flip)
      (for ([raw (in-list '("\"banana\"" "0" "-1" "1.5"))])
        (check-equal?
         (structured-red-wait-seconds
          (load-settings (make-configured-dir (format "{\"gsd\":{\"structured-red-wait-seconds\":~a}}"
                                                      raw))))
         300)))

    (test-case "the hardcoded 60 s is gone from the structured-RED path; one derived value drives wait and kill"
      (define region (structured-region (file->string red-checkpoint-src)))
      (unless (string? region)
        (fail "structured-RED region markers not found in red-checkpoint.rkt"))
      (check-false (string-contains? region "60000.0"))
      (check-true (string-contains? region "structured-red-wait-ms"))
      (check-true (string-contains? region "#:timeout-ms wait-ms"))
      (check-true (string-contains?
                   region
                   "(verification-wait registry (start-result-job-id started) wait-ms)")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests structured-red-wait-suite))
