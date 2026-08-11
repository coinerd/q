#lang racket/base

;; @speed fast
;; @suite arch

(require rackunit
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string)

(define-runtime-path tests-dir ".")
(define root (simplify-path (build-path tests-dir "..")))
(define decision-path
  (build-path root "docs" "architecture" "provider-hardening-terminal-v0.99.91.rktd"))

(define (read-one path)
  (call-with-input-file path
                        (lambda (in)
                          (define datum (read in))
                          (check-true (eof-object? (read in))
                                      "terminal ledger must contain exactly one datum")
                          datum)))

(define (source path)
  (file->string (build-path root path)))

(define expected-ids
  (append (for/list ([n (in-range 1 24)])
            (string->symbol (format "C~a" n)))
          '(G1 G2 G3)))

(test-case "W4-B1: terminal ledger closes MA-09 with an exact C1-C23/G1-G3 bijection"
  (define ledger (read-one decision-path))
  (check-equal? (hash-ref ledger 'schema-version) 1)
  (check-eq? (hash-ref ledger 'finding) 'MA-09)
  (check-eq? (hash-ref ledger 'status) 'closed)
  (check-eq? (hash-ref ledger 'decision) 'no-shared-production-abstraction)
  (define entries (hash-ref ledger 'entries))
  (check-equal? (sort (map (lambda (entry) (hash-ref entry 'id)) entries) symbol<?)
                (sort expected-ids symbol<?))
  (check-equal? (length entries)
                (length (remove-duplicates (map (lambda (entry) (hash-ref entry 'id)) entries))))
  (for ([entry (in-list entries)])
    (check-true (pair? (hash-ref entry 'evidence)) (format "~a needs evidence" (hash-ref entry 'id)))
    (check-not-false (member (hash-ref entry 'disposition)
                             '(shared-primitive provider-protocol
                                                retained-local
                                                intentional-duplication
                                                documented-asymmetry)))))

(test-case "W4-B2: Path B remains immutable and forbidden production abstractions are absent"
  (define ledger (read-one decision-path))
  (check-eq? (hash-ref ledger 'approved-path) 'path-b)
  (check-false (file-exists? (build-path root "llm" "provider-base.rkt")))
  (check-equal? (hash-ref ledger 'forbidden)
                '(provider-base shared-request-parser
                                shared-event-parser
                                protocol-template-flags
                                artificial-provider-equality)))

(test-case "W4-B3: C18 authentication headers remain four explicit provider protocols"
  (check-true (string-contains? (source "llm/anthropic/sse.rkt") "x-api-key: ~a"))
  (check-true (string-contains? (source "llm/anthropic/sse.rkt") "anthropic-version:"))
  (check-true (string-contains? (source "llm/gemini.rkt") "x-goog-api-key: ~a"))
  (check-true (string-contains? (source "llm/openai-compatible.rkt") "Authorization: Bearer ~a"))
  (check-true (string-contains? (source "llm/azure-openai.rkt") "api-key: ~a")))

(test-case "W4-B4: G1 status thresholds remain explicit and block a shared streaming template"
  (check-true (string-contains? (source "llm/anthropic/sse.rkt") "(>= status-code 400)"))
  (check-true (string-contains? (source "llm/gemini.rkt") "(>= status-code 400)"))
  (check-true (string-contains? (source "llm/openai-compatible.rkt") "(>= status-code 300)"))
  (check-true (string-contains? (source "llm/azure-openai.rkt") "(unless (= status-code 200)")))

(test-case "W4-B5: corrected G2 and G3 asymmetries are pinned"
  (define openai (source "llm/openai-compatible.rkt"))
  (check-equal? (length (regexp-match* #rx"[(]effective-request-timeout-for[ ]+[a-z-]+[)]" openai))
                2
                "OpenAI must retain exactly the non-stream and stream per-model timeout calls")
  (for ([path (in-list '("llm/anthropic/sse.rkt" "llm/gemini.rkt" "llm/azure-openai.rkt"))])
    (check-false (string-contains? (source path) "effective-request-timeout-for")))
  (check-true (string-contains? openai "openai-wrap-stream-error"))
  (for ([path (in-list '("llm/anthropic/sse.rkt" "llm/gemini.rkt" "llm/azure-openai.rkt"))])
    (check-false (string-contains? (source path) "wrap-stream-error"))))

(test-case "W4-B6: C11/C13 remain local and C22 remains an isolated Kimi eager-stream quirk"
  (define azure (source "llm/azure-openai.rkt"))
  (define anthropic-sse (source "llm/anthropic/sse.rkt"))
  (check-true (string-contains? azure "(string->url url-str)"))
  (for ([path (in-list '("llm/anthropic/sse.rkt" "llm/gemini.rkt"
                                                 "llm/openai-compatible.rkt"
                                                 "llm/azure-openai.rkt"))])
    (check-not-false (regexp-match? #rx"string-trim[ ]+base-url[ ]+\"/\"" (source path))))
  (check-true (string-contains? anthropic-sse "kimi-coding"))
  (check-true (string-contains? anthropic-sse "kimi-eager-stream-chunks"))
  (check-true (string-contains? (source "llm/adapters/eager-stream.rkt") "(define (eager-stream")))
