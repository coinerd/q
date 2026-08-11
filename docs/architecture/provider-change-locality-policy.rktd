;; Provider change-locality policy — v0.99.91 W3-B (Path B)
;;
;; Provider protocol markers are parsed as Racket datum atoms, so comments do
;; not count. A marker may occur only in its declared adapter ownership paths.
;; Generic streaming modules may contain only normalized stream mechanics and
;; the explicitly listed neutral C1-C8 helpers from the frozen W0 evidence.

((version . 1)
 (provider-protocols
  (anthropic
   (owners . ("llm/anthropic.rkt"
              "llm/anthropic-helpers.rkt"
              "llm/anthropic/format.rkt"
              "llm/anthropic/sse.rkt"))
   (markers . ("content_block_delta" "input_json_delta" "message_start")))
  (gemini
   (owners . ("llm/gemini.rkt"))
   (markers . ("usageMetadata" "functionCall" "candidates")))
  (openai-compatible
   (owners . ("llm/openai-compatible.rkt"))
   (markers . ("reasoning_content" "finish_reason")))
  (azure-openai
   (owners . ("llm/azure-openai.rkt"))
   (markers . ("api-version" "api-version="))))
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
                  read-response-body/timeout
                  current-http-request-timeout))
   (evidence . (C2 C3 C7 C8)))
  ("llm/provider-errors.rkt"
   (primitives . (provider-error classify-http-status raise-provider-error))
   (evidence . (C4)))))
