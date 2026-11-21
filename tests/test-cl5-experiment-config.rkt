#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-cl5-experiment-config.rkt — CL5 fixed experiment configuration (#9982)
;;
;; Covers scripts/cl5/experiment-config.rkt:
;;   1. The owner-approved configuration is exactly deepseek/deepseek-flash, thinking high
;;   2. Both arms share one model identity and keep their own pinned revisions
;;   3. Experiment and series metadata serialize the identity (incl. pi version)
;;   4. Series metadata mismatches are rejected (no silent configuration merge)
;;   5. An unavailable model refuses without fallback; a near-match is not accepted
;;   6. Unsupported thinking levels, inconsistent full ids, bad/equal revisions refuse
;;   7. Effective session configuration is verified (substitution and absence both raise)
;;   8. The configuration document and module agree (drift guard)
;;   9. The frozen CL0 task corpus is untouched by this configuration change

(require json
         rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         (file "../scripts/cl5/experiment-config.rkt")
         (file "../scripts/cl5/task-spec.rkt")
         (file "../scripts/cl5/trial-runner.rkt"))

(define-runtime-path config-doc "../docs/reports/CL5-EXPERIMENT-CONFIG.md")

(define config (experiment-config #:pi-version "1.6.0"))

;; A modified copy of the config for negative cases.
(define (config-with key value)
  (hash-set config key value))

(define (model-list . pairs)
  (for/list ([pair (in-list pairs)])
    (cons (first pair) (second pair))))

(define (session-events #:provider [provider "deepseek"]
                        #:model [model "deepseek-flash"]
                        #:thinking [thinking "high"])
  (list
   (hash 'type "session" 'timestamp "2026-11-21T22:29:03.368Z" 'version 3)
   (hash 'type "model_change" 'timestamp "2026-11-21T22:29:03.503Z" 'provider provider 'modelId model)
   (hash 'type
         "thinking_level_change"
         'timestamp
         "2026-11-21T22:29:03.503Z"
         'thinkingLevel
         thinking)))

(module+ test
  (test-case "configuration is the owner-approved deepseek/deepseek-flash, thinking high"
    (check-equal? (hash-ref config 'provider) "deepseek")
    (check-equal? (hash-ref config 'model) "deepseek-flash")
    (check-equal? (hash-ref config 'full-id) "deepseek/deepseek-flash")
    (check-equal? (hash-ref config 'thinking) "high")
    (check-equal? (hash-ref config 'tool-allowlist) "read,bash,edit,write")
    (check-equal? (hash-ref config 'pi-version) "1.6.0")
    (check-not-false (memq 'high allowed-thinking-levels))
    (check-equal? (validate-experiment-config config) config))

  (test-case "both arms share one model identity and keep distinct pinned revisions"
    (define baseline (arm-config config 'baseline))
    (define intervention (arm-config config 'intervention))
    (check-equal? (hash-ref baseline 'revision) baseline-revision)
    (check-equal? (hash-ref intervention 'revision) intervention-revision)
    (check-equal? (hash-ref baseline 'revision) "003e425d2f1206436eb9c0b84353604ce85bb082")
    (check-not-equal? (hash-ref baseline 'revision) (hash-ref intervention 'revision))
    (for ([key (in-list '(provider model full-id thinking tool-allowlist))])
      (check-equal? (hash-ref baseline key) (hash-ref intervention key) (format "arm field ~a" key)))
    (check-equal? (hash-ref baseline 'arm) 'baseline)
    (check-equal? (hash-ref intervention 'arm) 'intervention)
    (check-exn exn:fail? (lambda () (arm-config config 'historical-kimi))))

  (test-case "experiment and series metadata serialize the identity"
    (define experiment (experiment-metadata config #:host "host-a"))
    (check-equal? (hash-ref experiment 'provider) "deepseek")
    (check-equal? (hash-ref experiment 'model) "deepseek-flash")
    (check-equal? (hash-ref experiment 'full-id) "deepseek/deepseek-flash")
    (check-equal? (hash-ref experiment 'thinking) "high")
    (check-equal? (hash-ref experiment 'baseline-revision) baseline-revision)
    (check-equal? (hash-ref experiment 'intervention-revision) intervention-revision)
    (check-equal? (hash-ref experiment 'pi-version) "1.6.0")
    (define series (series-metadata config 'baseline #:host "host-a" #:n 3 #:rotation '(T1 T3)))
    (check-equal? (hash-ref series 'arm) 'baseline)
    (check-equal? (hash-ref series 'revision) baseline-revision)
    (check-equal? (hash-ref series 'thinking) "high")
    (check-equal? (hash-ref series 'rotation) '("T1" "T3"))
    ;; Round-trips through JSON as written by run-series.
    (define encoded (jsexpr->string (->jsexpr series)))
    (define decoded (string->jsexpr encoded))
    (check-equal? (hash-ref decoded 'model) "deepseek-flash")
    (check-equal? (hash-ref decoded 'revision) baseline-revision)
    (check-equal? (hash-ref decoded 'thinking) "high")
    (check-equal? (hash-ref decoded 'pi-version) "1.6.0"))

  (test-case "series metadata mismatch is rejected instead of merged"
    (define requested (series-metadata config 'baseline #:host "host-a" #:n 3))
    (check-equal? (check-series-metadata requested requested) requested)
    (for ([key (in-list '(revision provider model thinking pi-version))])
      (check-exn exn:fail?
                 (lambda ()
                   (check-series-metadata (hash-set requested key "historical-kimi-value") requested))
                 (format "mismatch on ~a must raise" key))))

  (test-case "an unavailable model refuses; a near-match is not a fallback"
    (define advertised
      (model-list '("deepseek" "deepseek-flash")
                  '("deepseek" "deepseek-v4-pro")
                  '("openrouter" "~deepseek/deepseek-flash-latest")))
    (check-equal? (assert-model-available advertised config) (cons "deepseek" "deepseek-flash"))
    ;; Same model name under a different provider is not the configured model.
    (check-exn exn:fail?
               (lambda ()
                 (assert-model-available (model-list '("openrouter" "deepseek-flash")) config)))
    ;; A near-match name must not be accepted silently.
    (check-exn exn:fail?
               (lambda ()
                 (assert-model-available
                  (model-list '("deepseek" "deepseek-flash-preview")
                              '("openrouter" "~deepseek/deepseek-flash-latest"))
                  config)))
    (check-exn exn:fail? (lambda () (assert-model-available '() config))))

  (test-case "parse-model-list reads the pi table shape and ignores headers"
    (define text
      (string-append "provider      model                       context  max-out  thinking  images\n"
                     "deepseek      deepseek-flash              1M       384K     yes       yes\n"
                     "deepseek      deepseek-v4-pro             1M       384K     yes       no\n"
                     "openrouter    ~deepseek/deepseek-flash-latest  1.0M  943.7K  yes    yes\n"))
    (check-equal? (parse-model-list text)
                  (append (model-list '("deepseek" "deepseek-flash") '("deepseek" "deepseek-v4-pro"))
                          (list (cons "openrouter" "~deepseek/deepseek-flash-latest"))))
    (check-equal? (parse-model-list "") '()))

  (test-case "invalid configurations fail closed before any session"
    (check-exn exn:fail? (lambda () (validate-experiment-config (config-with 'thinking "ultra"))))
    (check-exn exn:fail?
               (lambda ()
                 (validate-experiment-config (config-with 'full-id "deepseek/deepseek-v4-pro"))))
    (check-exn exn:fail?
               (lambda () (validate-experiment-config (config-with 'baseline-revision "003e425d"))))
    (check-exn
     exn:fail?
     (lambda () (validate-experiment-config (config-with 'intervention-revision baseline-revision)))))

  (test-case "effective configuration is verified from the transcript"
    (define verified (verify-effective-config (session-events) config))
    (check-equal? (hash-ref verified 'provider) "deepseek")
    (check-equal? (hash-ref verified 'model) "deepseek-flash")
    (check-equal? (hash-ref verified 'thinking) "high")
    (check-true (hash-ref verified 'verified?))
    ;; A substitution is never accepted, neither provider, model nor thinking.
    (check-exn exn:fail?
               (lambda () (verify-effective-config (session-events #:provider "kimi-coding") config)))
    (check-exn exn:fail?
               (lambda ()
                 (verify-effective-config (session-events #:model "deepseek-v4-pro") config)))
    (check-exn exn:fail?
               (lambda () (verify-effective-config (session-events #:thinking "medium") config)))
    ;; Absence of an event is also a failure, not an implicit default.
    (check-exn exn:fail?
               (lambda ()
                 (verify-effective-config (list (first (session-events)) (second (session-events)))
                                          config)))
    (check-exn exn:fail? (lambda () (verify-effective-config '() config))))

  (test-case "the configuration document and module agree"
    (define text (file->string (simplify-path config-doc)))
    (for ([needle (in-list (list "deepseek"
                                 "deepseek-flash"
                                 "deepseek/deepseek-flash"
                                 "high"
                                 "read,bash,edit,write"
                                 baseline-revision
                                 intervention-revision))])
      (check-true (string-contains? text needle) (format "document must name ~a" needle)))
    (check-true (string-contains? text "not a matched experiment"))
    (check-true (string-contains? text "no fallback")))

  (test-case "the frozen CL0 task corpus is unchanged by the configuration change"
    (check-equal? (map (lambda (spec) (hash-ref spec 'id)) task-specs) '(T1 T3 T4 T2 T5))
    (check-equal? corpus-revision "003e425d2f1206436eb9c0b84353604ce85bb082")
    (check-equal? (map (lambda (spec) (hash-ref spec 'oracle)) task-specs)
                  '(text text text execution execution))
    (for ([spec (in-list task-specs)])
      (check-true (string? (hash-ref spec 'prompt)))
      ;; The baseline arm revision is the corpus revision the prompts were frozen at.
      (check-equal? (hash-ref spec 'corpus-revision) (hash-ref config 'baseline-revision)))))
