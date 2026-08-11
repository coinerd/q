#lang racket/base

;; @speed fast
;; @suite security

;; BOUNDARY: integration
;; v0.99.91 W2-B (#9239): versioned provider-specific wire evidence.

(require json
         rackunit
         racket/generator
         racket/list
         racket/match
         racket/port
         racket/runtime-path
         racket/string
         "../llm/anthropic.rkt"
         "../llm/gemini.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/azure-openai.rkt"
         "../llm/stream.rkt"
         "../llm/model.rkt"
         "../util/credential-redaction.rkt"
         "helpers/provider-contract-matrix.rkt"
         "helpers/provider-differential-fixtures.rkt"
         racket/file)

(define-runtime-path fixture-helper-source "helpers/provider-differential-fixtures.rkt")

(define (entry provider kind)
  (or (differential-fixture-entry-for provider kind)
      (error 'entry "missing fixture ~a/~a" provider kind)))

(define (json-fixture provider kind)
  (load-differential-json (entry provider kind)))

(define (byte-fixture provider kind)
  (load-differential-bytes (entry provider kind)))

(define (fixture-expected provider kind)
  (differential-fixture-entry-expected (entry provider kind)))

(define (provider-event->chunks provider event)
  (match provider
    ['anthropic (anthropic-parse-single-event event (box #f) (box #f) (box 0))]
    ['gemini (gemini-parse-single-event event)]
    ['openai-compatible (list (normalize-openai-chunk event))]
    ['azure-openai (list (normalize-openai-chunk event))]))

(define (framing-observable provider)
  (define events (parse-sse-lines (bytes->string/utf-8 (byte-fixture provider 'framing))))
  (define chunks (apply append (map (lambda (event) (provider-event->chunks provider event)) events)))
  (define text-chunk (findf (lambda (chunk) (stream-chunk-delta-text chunk)) chunks))
  (list (length events) (and text-chunk (stream-chunk-delta-text text-chunk))))

(define (tool-observable provider)
  (define wire (json-fixture provider 'tools))
  (define chunks
    (match provider
      ['anthropic
       (define tool-id (box #f))
       (define tool-name (box #f))
       (define tool-index (box 0))
       (apply append
              (for/list ([event (in-list (hash-ref wire 'events))])
                (anthropic-parse-single-event event tool-id tool-name tool-index)))]
      [_ (provider-event->chunks provider (hash-ref wire 'event))]))
  (define chunk (findf stream-chunk-delta-tool-call chunks))
  (define delta (and chunk (stream-chunk-delta-tool-call chunk)))
  (define function (and delta (hash-ref delta 'function #f)))
  (define arguments (and function (hash-ref function 'arguments #f)))
  (list (and function (hash-ref function 'name #f))
        (cond
          [(hash? arguments) 'structured]
          [(string? arguments) 'partial-json]
          [else #f])))

(define (usage-observable provider)
  (define wire (json-fixture provider 'usage))
  (define chunks (provider-event->chunks provider (hash-ref wire 'event)))
  (define done (findf stream-chunk-done? chunks))
  (define usage (and done (stream-chunk-usage done)))
  (and usage (sort (hash-keys usage) symbol<?)))

(define (run-stream-timeout-recipe provider recipe)
  (define-values (in out) (make-pipe))
  (define event (hash-ref recipe 'event #f))
  (dynamic-wind void
                (lambda ()
                  (when event
                    (write-bytes (string->bytes/utf-8 (format "data: ~a\n" (jsexpr->string event)))
                                 out)
                    (flush-output out))
                  (define stream
                    (stream-sse-events in
                                       (lambda (wire) (provider-event->chunks provider wire))
                                       #:initial-timeout 0.1
                                       #:thinking-timeout 0.1
                                       #:stream-timeout 0.1
                                       #:max-total-timeout 2))
                  (with-handlers ([exn:fail:network:timeout:stream? values])
                    (when event
                      (stream))
                    (stream)
                    #f))
                (lambda ()
                  (unless (port-closed? out)
                    (close-output-port out))
                  (unless (port-closed? in)
                    (close-input-port in)))))

(test-case "W2-B1: v1 manifest is a complete 20-cell fixture bijection"
  (check-equal? differential-fixture-version 1)
  (check-equal? (check-differential-fixtures!) '())
  (check-equal? (length differential-fixture-entries) 20)
  (check-equal? differential-fixture-providers provider-contract-names)
  (check-equal? differential-fixture-kinds '(framing tools usage malformed timeout)))

(test-case "W2-B2: committed fixture bytes are deterministic, digested, and redacted"
  (for ([fixture (in-list differential-fixture-entries)])
    (define first (load-differential-bytes fixture))
    (define second (load-differential-bytes fixture))
    (check-equal? first second)
    (check-false (contains-secret-leak? (bytes->string/utf-8 first #\uFFFD))
                 (format "secret-like data in ~a" (differential-fixture-entry-path fixture))))
  (define helper-source (file->string fixture-helper-source))
  (check-false (regexp-match? #rx"llm/(anthropic|gemini|openai|azure)" helper-source)
               "test fixture loader must not become a shared provider parser"))

(test-case "W2-B3: provider-specific framing bytes drive each REAL stream parser"
  (for ([provider (in-list differential-fixture-providers)])
    (define expected (fixture-expected provider 'framing))
    (check-equal? (framing-observable provider)
                  (list (hash-ref expected 'event_count) (hash-ref expected 'delta_text))
                  (symbol->string provider))))

(test-case "W2-B4: tool fixtures retain provider-native argument modes"
  (for ([provider (in-list differential-fixture-providers)])
    (define expected (fixture-expected provider 'tools))
    (check-equal? (tool-observable provider)
                  (list (hash-ref expected 'name) (string->symbol (hash-ref expected 'argument_mode)))
                  (symbol->string provider))))

(test-case "W2-B5: usage fixtures preserve the documented streaming asymmetry"
  (for ([provider (in-list differential-fixture-providers)])
    (define expected (fixture-expected provider 'usage))
    (check-equal? (usage-observable provider)
                  (map string->symbol (hash-ref expected 'keys))
                  (symbol->string provider))))

(test-case "W2-B6: malformed wire bytes are retained exactly and skipped"
  (for ([provider (in-list differential-fixture-providers)])
    (define raw (byte-fixture provider 'malformed))
    (define parsed (parse-sse-lines (bytes->string/utf-8 raw #\uFFFD)))
    (check-equal? (subbytes raw 0 6) #"data: ")
    (check-equal? (length parsed) (hash-ref (fixture-expected provider 'malformed) 'parsed_count))))

(test-case "W2-B7: timeout recipes execute setup/initial/thinking/content phases"
  (for ([provider (in-list differential-fixture-providers)])
    (define recipe (json-fixture provider 'timeout))
    (define recipe-expected (hash-ref recipe 'expected))
    (check-equal? recipe-expected (fixture-expected provider 'timeout))
    (define operation (hash-ref recipe 'operation))
    (define expected-phase (string->symbol (hash-ref recipe-expected 'phase)))
    (cond
      [(string=? operation "blocked-request")
       (define cleaned? (box #f))
       (check-equal? (hash-ref recipe-expected 'exception) "exn:fail:network:timeout")
       (check-exn exn:fail:network:timeout?
                  (lambda ()
                    (call-with-request-timeout (lambda () (sync never-evt))
                                               #:timeout 0.1
                                               #:cleanup (lambda () (set-box! cleaned? #t)))))
       (check-true (unbox cleaned?))
       (check-equal? expected-phase 'setup)]
      [else
       (check-equal? (hash-ref recipe-expected 'exception) "exn:fail:network:timeout:stream")
       (define timeout (run-stream-timeout-recipe provider recipe))
       (check-pred exn:fail:network:timeout:stream? timeout)
       (check-equal? (exn:fail:network:timeout:stream-phase timeout) expected-phase)
       (check-equal? (exn:fail:network:timeout:stream-received-any-data? timeout)
                     (hash-ref recipe-expected 'received_any_data))
       (check-equal? (exn:fail:network:timeout:stream-output-chars timeout)
                     (hash-ref recipe-expected 'output_chars))])))

(test-case "W2-B8: differential evidence is not artificially equalized"
  (check-not-equal? (byte-fixture 'anthropic 'framing) (byte-fixture 'gemini 'framing))
  (check-not-equal? (byte-fixture 'openai-compatible 'framing) (byte-fixture 'azure-openai 'framing))
  (check-not-equal? (usage-observable 'anthropic) (usage-observable 'gemini)))
