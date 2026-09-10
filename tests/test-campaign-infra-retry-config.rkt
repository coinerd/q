#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-campaign-infra-retry-config.rkt — BUG-0067 settings wiring.
;;
;; policy.rkt documented "Settings key: gsd.campaign-infra-retries" since
;; v1.00.22, but no accessor ever read it — the budget was not actually
;; configurable (BUG-0044 class defect). BUG-0067 wires three keys through
;; runtime/settings-query.rkt + the settings.rkt facade, consumed by
;; resolve-effective-infra-retry-policy at the go-orchestrator composition
;; root:
;;   gsd.campaign-infra-retries   — fast-lane budget (canonical default 3)
;;   gsd.campaign-infra-patience  — slow-lane horizon seconds (default 7200)
;;   gsd.campaign-infra-max-delay — single-backoff cap seconds (default 900)
;; Invalid values warn-and-default; a typo'd settings file must NEVER
;; crash a campaign mid-wave (settings-query contract).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/runtime-path
         "../runtime/settings-query.rkt"
         (only-in "../runtime/settings.rkt" load-settings)
         (only-in "../extensions/gsd/infra-retry-policy.rkt" resolve-effective-infra-retry-policy)
         (only-in "../extensions/gsd/policy.rkt"
                  current-gsd-campaign-infra-retries
                  current-gsd-campaign-infra-patience
                  current-gsd-campaign-infra-max-delay))

(define-runtime-path go-orchestrator-src "../extensions/gsd/go-orchestrator.rkt")
(define-runtime-path settings-query-src "../runtime/settings-query.rkt")

;; ── Synthetic settings file helper (BUG-0044 action-4 pattern) ──
;; Writes a config.json shaped like the real <proj>/.q/config.json and
;; loads it via the same load-settings seam the orchestrator uses.

(define (write-settings-file json-content)
  (define dir (make-temporary-file "infra-cfg-~a" 'directory))
  (define cfg-dir (build-path dir ".q"))
  (make-directory* cfg-dir)
  (define cfg-path (build-path cfg-dir "config.json"))
  (call-with-output-file cfg-path (lambda (out) (display json-content out)) #:exists 'truncate)
  cfg-path)

(define infra-config-suite
  (test-suite "BUG-0067: campaign infra-retry policy is settings-driven"

    (test-case "accessors return #f when settings are absent/unloadable"
      (check-false (gsd-campaign-infra-retries #f))
      (check-false (gsd-campaign-infra-patience #f))
      (check-false (gsd-campaign-infra-max-delay #f)))

    (test-case "valid on-disk keys flow through load-settings → accessors"
      (define cfg
        (write-settings-file
         "{\"gsd\":{\"campaign-infra-retries\":8,\"campaign-infra-patience\":3600,\"campaign-infra-max-delay\":300}}"))
      (define settings (load-settings #:config-path cfg))
      (check-equal? (gsd-campaign-infra-retries settings) 8)
      (check-equal? (gsd-campaign-infra-patience settings) 3600)
      (check-equal? (gsd-campaign-infra-max-delay settings) 300)
      ;; Resolver: settings values beat parameter defaults.
      (define-values (retries patience max-delay) (resolve-effective-infra-retry-policy settings))
      (check-equal? retries 8)
      (check-equal? patience 3600)
      (check-equal? max-delay 300))

    (test-case "invalid values warn-and-default instead of crashing"
      (define cfg
        (write-settings-file
         "{\"gsd\":{\"campaign-infra-retries\":\"eight\",\"campaign-infra-patience\":true,\"campaign-infra-max-delay\":-5}}"))
      (define settings (load-settings #:config-path cfg))
      (check-false (gsd-campaign-infra-retries settings))
      (check-false (gsd-campaign-infra-patience settings))
      (check-false (gsd-campaign-infra-max-delay settings))
      ;; And the resolver lands on the canonical parameter defaults.
      (define-values (retries patience max-delay) (resolve-effective-infra-retry-policy settings))
      (check-equal? retries (current-gsd-campaign-infra-retries))
      (check-equal? patience (current-gsd-campaign-infra-patience))
      (check-equal? max-delay (current-gsd-campaign-infra-max-delay)))

    (test-case "patience 0 is a legal explicit value (legacy fail-closed)"
      (define cfg (write-settings-file "{\"gsd\":{\"campaign-infra-patience\":0}}"))
      (define settings (load-settings #:config-path cfg))
      (check-equal? (gsd-campaign-infra-patience settings) 0))

    (test-case "composition root consumes the resolver (wiring pin)"
      (define src (file->string go-orchestrator-src))
      (check-true (string-contains? src "resolve-effective-infra-retry-policy")
                  "go-orchestrator must resolve the effective infra policy from settings")
      (define sq (file->string settings-query-src))
      (check-true (string-contains? sq "gsd-campaign-infra-retries"))
      (check-true (string-contains? sq "gsd-campaign-infra-patience"))
      (check-true (string-contains? sq "gsd-campaign-infra-max-delay")))))

(module+ main
  (run-tests infra-config-suite))
