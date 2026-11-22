#lang racket/base

;; scripts/cl5/experiment-config.rkt — the single explicit CL5 experiment
;; configuration (CL5 #9982, owner-approved 2026-11-21).
;;
;; Authority: docs/reports/CL5-EXPERIMENT-CONFIG.md. Both arms of the W3
;; agent trials consume THIS module: the baseline arm pinned at the frozen
;; CL0 corpus revision and the intervention arm at its own separately pinned
;; revision. The model identity is never a default and never a runner
;; parameter — one config hash is passed to every trial, so the two arms
;; cannot drift.
;;
;; Fail-closed rules (CL5 requirement 3):
;;   * an unsupported thinking level, an inconsistent full id, a non-SHA
;;     revision, or equal arm revisions raise before any session starts;
;;   * a model missing from the provider's advertised model list raises —
;;     there is NO fallback to another model or provider, and a quota error
;;     is reported, never worked around;
;;   * after a session, the EFFECTIVE provider/model/thinking read from the
;;     session transcript must equal the requested configuration; a
;;     substitution (e.g. a different model answering) raises instead of
;;     being recorded as a valid trial.
;;
;; Historical note recorded in the config doc: earlier CL5 attempts ran on
;; kimi-coding/k3-256k. Those numbers are not a matched comparison with this
;; configuration and must not be reported as one.

(require json
         racket/list
         racket/string)

(provide experiment-config
         experiment-provider
         experiment-model
         experiment-full-id
         experiment-thinking
         experiment-tool-allowlist
         allowed-thinking-levels
         baseline-revision
         intervention-revision
         arm-config
         arm-revisions
         validate-experiment-config
         parse-model-list
         assert-model-available
         effective-config
         verify-effective-config
         experiment-metadata
         series-metadata
         series-identity-keys
         check-series-metadata)

;; ---- the configuration ----------------------------------------------------------

;; Owner-approved fixed configuration for every W3 trial, both arms.
(define experiment-provider "deepseek")
(define experiment-model "deepseek-flash")
(define experiment-full-id "deepseek/deepseek-flash")
(define experiment-thinking "high")

;; Identical tool allowlist in both arms (CL0 §1: identical tool set).
(define experiment-tool-allowlist "read,bash,edit,write")

;; pi's accepted thinking levels (pi --help).
(define allowed-thinking-levels '(off minimal low medium high xhigh max))

;; Baseline arm: the frozen CL0 corpus revision.
(define baseline-revision "003e425d2f1206436eb9c0b84353604ce85bb082")

;; Intervention arm: separately pinned revision for the post-CL1..CL4 tree.
;; Deliberately a full SHA (not "main"): a floating ref would let the arm move
;; mid-series without an explicit config change.
(define intervention-revision "ec88d4638483a8fb4710ed20872d790d7457a8c6")

(define (experiment-config #:pi-version [pi-version #f])
  (hash 'id
        "cl5-owner-approved-2026-11-21"
        'provider
        experiment-provider
        'model
        experiment-model
        'full-id
        experiment-full-id
        'thinking
        experiment-thinking
        'tool-allowlist
        experiment-tool-allowlist
        'baseline-revision
        baseline-revision
        'intervention-revision
        intervention-revision
        'pi-version
        pi-version))

;; ---- validation (fail closed before any session) ---------------------------------

(define (full-sha? value)
  (and (string? value) (regexp-match? #px"^[0-9a-f]{40}$" value)))

(define (raise-config kind message #:key [key #f] #:value [value #f])
  (if key
      (raise-arguments-error 'validate-experiment-config message "key" key "value" value)
      (raise-arguments-error 'validate-experiment-config message)))

;; Raises on anything that would make a series invalid: unsupported thinking
;; level, inconsistent full id, non-SHA or equal arm revisions, empty tools.
(define (validate-experiment-config config)
  (define thinking (hash-ref config 'thinking #f))
  (unless (memq (if (string? thinking)
                    (string->symbol thinking)
                    thinking)
                allowed-thinking-levels)
    (raise-config 'thinking
                  "unsupported thinking level (no substitution is attempted)"
                  #:key 'thinking
                  #:value thinking))
  (define provider (hash-ref config 'provider #f))
  (define model (hash-ref config 'model #f))
  (define full-id (hash-ref config 'full-id #f))
  (unless (and (string? provider) (string? model) (string? full-id))
    (raise-config 'identity "provider, model and full-id must all be strings"))
  (unless (equal? full-id (format "~a/~a" provider model))
    (raise-config 'identity "full-id must equal <provider>/<model>" #:key 'full-id #:value full-id))
  (define baseline (hash-ref config 'baseline-revision #f))
  (define intervention (hash-ref config 'intervention-revision #f))
  (unless (full-sha? baseline)
    (raise-config 'baseline
                  "baseline revision must be a full 40-character SHA"
                  #:key 'baseline-revision
                  #:value baseline))
  (unless (full-sha? intervention)
    (raise-config 'intervention
                  "intervention revision must be a full 40-character SHA"
                  #:key 'intervention-revision
                  #:value intervention))
  (when (equal? baseline intervention)
    (raise-config 'revisions "baseline and intervention revisions must differ"))
  (define tools (hash-ref config 'tool-allowlist #f))
  (unless (and (string? tools) (not (equal? tools "")))
    (raise-config 'tools "tool allowlist must be a non-empty string"))
  config)

;; ---- arms -----------------------------------------------------------------------

(define (arm-revisions config)
  (hash 'baseline
        (hash-ref config 'baseline-revision)
        'intervention
        (hash-ref config 'intervention-revision)))

;; One arm's trial identity: revision + the identical model configuration.
(define (arm-config config arm)
  (validate-experiment-config config)
  (define revision
    (case arm
      [(baseline) (hash-ref config 'baseline-revision)]
      [(intervention) (hash-ref config 'intervention-revision)]
      [else (raise-arguments-error 'arm-config "unknown arm" "arm" arm)]))
  (hash 'arm
        arm
        'revision
        revision
        'provider
        (hash-ref config 'provider)
        'model
        (hash-ref config 'model)
        'full-id
        (hash-ref config 'full-id)
        'thinking
        (hash-ref config 'thinking)
        'tool-allowlist
        (hash-ref config 'tool-allowlist)))

;; ---- provider model availability --------------------------------------------------

;; Parse `pi --list-models <provider>` table rows into (provider . model)
;; pairs; the header row and any banner lines are ignored. Pure.
(define (parse-model-list text)
  (define rows
    (for/list ([line (in-list (string-split (or text "") "\n"))]
               #:when (not (equal? (string-trim line) "")))
      (string-split (string-trim line))))
  (for/list ([row (in-list rows)]
             #:when (>= (length row) 2)
             #:unless (equal? (first row) "provider"))
    (cons (first row) (second row))))

;; Raises when the configured provider/model is not advertised — never
;; substitutes a similar model.
(define (assert-model-available model-list config)
  (define wanted (cons (hash-ref config 'provider) (hash-ref config 'model)))
  (unless (member wanted model-list)
    (raise-arguments-error 'assert-model-available
                           "configured model is not available (no fallback attempted)"
                           "provider"
                           (car wanted)
                           "model"
                           (cdr wanted)
                           "available"
                           (map (lambda (pair) (format "~a/~a" (car pair) (cdr pair))) model-list)))
  wanted)

;; ---- effective configuration from a transcript -------------------------------------

;; Events of one transcript type, in order.
(define (events-of-type events type)
  (for/list ([event (in-list events)]
             #:when (equal? (hash-ref event 'type #f) type))
    event))

;; Read the EFFECTIVE provider/model/thinking from a session transcript.
;; Reports the distinct observed identities so a caller can see the evidence.
(define (effective-config events)
  (define model-events (events-of-type events "model_change"))
  (define thinking-events (events-of-type events "thinking_level_change"))
  (hash 'provider
        (and (pair? model-events) (hash-ref (last model-events) 'provider #f))
        'model
        (and (pair? model-events) (hash-ref (last model-events) 'modelId #f))
        'thinking
        (and (pair? thinking-events) (hash-ref (last thinking-events) 'thinkingLevel #f))
        'model-changes
        (for/list ([event (in-list model-events)])
          (format "~a/~a" (hash-ref event 'provider #f) (hash-ref event 'modelId #f)))
        'thinking-changes
        (for/list ([event (in-list thinking-events)]) (hash-ref event 'thinkingLevel #f))))

;; Fail closed: the session must show exactly the requested identity for its
;; WHOLE lifetime. Checking only the last event would accept an intermediate
;; switch that was later reverted (A -> B -> A), which is exactly what a
;; fixed-model series must reject. Any mismatching event raises, and a missing
;; event type is as invalid as a substituted one.
(define (verify-effective-config events config)
  (define requested-provider (hash-ref config 'provider))
  (define requested-model (hash-ref config 'model))
  (define requested-thinking (hash-ref config 'thinking))
  (define model-events (events-of-type events "model_change"))
  (define thinking-events (events-of-type events "thinking_level_change"))
  (when (null? model-events)
    (raise-arguments-error 'verify-effective-config "session records no model_change event"))
  (when (null? thinking-events)
    (raise-arguments-error 'verify-effective-config
                           "session records no thinking_level_change event"))
  (for ([event (in-list model-events)])
    (define provider (hash-ref event 'provider #f))
    (define model (hash-ref event 'modelId #f))
    (unless (and (equal? provider requested-provider) (equal? model requested-model))
      (raise-arguments-error 'verify-effective-config
                             "session observed a different provider/model at some point"
                             "requested"
                             (format "~a/~a" requested-provider requested-model)
                             "observed"
                             (format "~a/~a" provider model)
                             "timestamp"
                             (hash-ref event 'timestamp #f))))
  (for ([event (in-list thinking-events)])
    (define thinking (hash-ref event 'thinkingLevel #f))
    (unless (equal? thinking requested-thinking)
      (raise-arguments-error 'verify-effective-config
                             "session observed a different thinking level at some point"
                             "requested"
                             requested-thinking
                             "observed"
                             thinking
                             "timestamp"
                             (hash-ref event 'timestamp #f))))
  (hash 'provider
        requested-provider
        'model
        requested-model
        'thinking
        requested-thinking
        'verified?
        #t
        'model-changes
        (length model-events)
        'thinking-changes
        (length thinking-events)))

;; ---- metadata ------------------------------------------------------------------

;; Experiment-level metadata: the whole configuration plus both pinned
;; revisions. `pi-version` and `host` are recorded because a series is only
;; comparable within one CLI version and host class.
(define (experiment-metadata config #:host [host "unknown"])
  (validate-experiment-config config)
  (hash 'experiment
        "CL5 (#9982)"
        'config-id
        (hash-ref config 'id)
        'provider
        (hash-ref config 'provider)
        'model
        (hash-ref config 'model)
        'full-id
        (hash-ref config 'full-id)
        'thinking
        (hash-ref config 'thinking)
        'tool-allowlist
        (hash-ref config 'tool-allowlist)
        'baseline-revision
        (hash-ref config 'baseline-revision)
        'intervention-revision
        (hash-ref config 'intervention-revision)
        'pi-version
        (hash-ref config 'pi-version #f)
        'host
        host))

;; Per-series metadata: one arm, its revision, and the shared identity.
(define (series-metadata config arm #:host [host "unknown"] #:n [n 3] #:rotation [rotation '()])
  (define arm-cfg (arm-config config arm))
  (hash 'arm
        arm
        'revision
        (hash-ref arm-cfg 'revision)
        'full-id
        (hash-ref arm-cfg 'full-id)
        'provider
        (hash-ref arm-cfg 'provider)
        'model
        (hash-ref arm-cfg 'model)
        'full-id
        (hash-ref arm-cfg 'full-id)
        'thinking
        (hash-ref arm-cfg 'thinking)
        'tool-allowlist
        (hash-ref arm-cfg 'tool-allowlist)
        'pi-version
        (hash-ref config 'pi-version #f)
        'host
        host
        'n
        n
        'rotation
        (map (lambda (task) (symbol->string task)) rotation)))

;; Series files are append-only, so a second run may only extend a series
;; whose stamped identity matches EXACTLY: both pinned arms, the model
;; identity, the thinking level, the tool allowlist, the CLI version, the
;; host class, the trial count and the rotation. A changed value is an error
;; — the honest response is a new series, never a merge of two experiments.
(define series-identity-keys
  '(arm revision provider model full-id thinking tool-allowlist pi-version host n rotation))

;; The running pi version must be present and must equal the stamp: a series
;; cannot claim a CLI version its runner never verified.
(define (check-series-metadata existing requested)
  (define running-version (hash-ref requested 'pi-version #f))
  (unless (and (string? running-version) (not (equal? running-version "")))
    (raise-arguments-error 'check-series-metadata
                           "requested series metadata has no verified pi version"
                           "pi-version"
                           running-version))
  (for ([key (in-list series-identity-keys)])
    (unless (equal? (hash-ref existing key #f) (hash-ref requested key #f))
      (raise-arguments-error 'check-series-metadata
                             "series metadata mismatch; a series never mixes experiments"
                             "key"
                             key
                             "existing"
                             (hash-ref existing key #f)
                             "requested"
                             (hash-ref requested key #f))))
  requested)
