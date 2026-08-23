;; Provider change-locality policy — v0.99.91 W3-B (Path B)
;;
;; Provider protocol markers are parsed as Racket datum atoms, so comments do
;; not count. A marker may occur only in its declared adapter ownership paths.
;; Generic streaming modules must stay free of provider protocol logic; the
;; C1-C8 list below freezes the already-approved shared-helper exemption set
;; from the W0 evidence. It is an exemption registry, not an exhaustive ban on
;; new neutral helpers: unlisted neutral code remains allowed as long as it
;; carries no provider protocol marker.
;;
;; Scope: this is a syntactic high-precision fitness gate, not a data-flow or
;; security analysis. It detects markers in literal/constructor/alist hash keys,
;; compound literals, strings, bytes, and regexps, but not values smuggled
;; through computed indirection (e.g. an alist built from a variable).

((version . 1)
 (provider-protocols
  (anthropic
   (owners . ("llm/anthropic.rkt"
              "llm/anthropic-helpers.rkt"
              "llm/anthropic/format.rkt"
              "llm/anthropic/sse.rkt"))
   (markers . (("content_block_delta" . string-literal)
               ("input_json_delta" . string-literal)
               ("message_start" . string-literal))))
  (gemini
   (owners . ("llm/gemini.rkt"))
   (markers . (("usageMetadata" . hash-key)
               ("functionCall" . hash-key)
               ("candidates" . hash-key))))
  (openai-compatible
   (owners . ("llm/openai-compatible.rkt"))
   (markers . (("reasoning_content" . hash-key)
               ("finish_reason" . hash-key))))
  (azure-openai
   (owners . ("llm/azure-openai.rkt"))
   (markers . (("api-version" . hash-key)
               ("api-version=" . string-literal)))))
 (generic-streaming-modules . ("llm/stream.rkt" "llm/adapters/eager-stream.rkt"))
 (neutral-helpers
  ("llm/http-helpers.rkt"
   (primitives . (make-provider-http-request check-provider-status! translate-stop-reason))
   (evidence . (C1 C5 C6)))
  ("llm/stream.rkt"
   (primitives . (stream-sse-events
                  parse-sse-line
                  parse-sse-lines
                  sse-done?
                  close-port-after-stream
                  accumulate-tool-call-deltas
                  call-with-request-timeout
                  read-line/timeout
                  read-response-body/timeout))
   (evidence . (C2 C3 C7 C8)))
  ;; v1.00.13 W1 (#9461): the timeout-configuration parameter moved from
  ;; llm/stream.rkt to the policy module (single-owner rule, RL-1/AC-1).
  ("llm/request-policy.rkt"
   (primitives . (current-http-request-timeout
                  resolve-request-network-policy
                  resolve-request-network-policy-for-model
                  sse-phase-timeout-secs))
   (evidence . (C2 C3)))
  ("llm/provider-errors.rkt"
   (primitives . (provider-error classify-http-status raise-provider-error))
   (evidence . (C4)))))
