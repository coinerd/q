#lang racket/base

;; @speed fast
;; @suite arch
;; @boundary unit
;; BOUNDARY: architecture

(require rackunit
         racket/list
         racket/runtime-path
         racket/string
         "helpers/provider-change-locality.rkt")

(define-runtime-path repo-root "..")
(define-runtime-path policy-path "../docs/architecture/provider-change-locality-policy.rktd")

(define policy (load-provider-locality-policy policy-path))

(define expected-protocol-policy
  '((anthropic ("llm/anthropic.rkt" "llm/anthropic-helpers.rkt"
                                    "llm/anthropic/format.rkt"
                                    "llm/anthropic/sse.rkt")
               (("content_block_delta" string-literal) ("input_json_delta" string-literal)
                                                       ("message_start" string-literal)))
    (gemini ("llm/gemini.rkt")
            (("usageMetadata" hash-key) ("functionCall" hash-key) ("candidates" hash-key)))
    (openai-compatible ("llm/openai-compatible.rkt")
                       (("reasoning_content" hash-key) ("finish_reason" hash-key)))
    (azure-openai ("llm/azure-openai.rkt")
                  (("api-version" hash-key) ("api-version=" string-literal)))))

(define expected-neutral-policy
  '(("llm/http-helpers.rkt" (make-provider-http-request check-provider-status! translate-stop-reason)
                            (C1 C5 C6))
    ("llm/stream.rkt" (stream-sse-events parse-sse-line
                                         parse-sse-lines
                                         sse-done?
                                         close-port-after-stream
                                         accumulate-tool-call-deltas
                                         call-with-request-timeout
                                         read-line/timeout
                                         read-response-body/timeout
                                         current-http-request-timeout)
                      (C2 C3 C7 C8))
    ("llm/provider-errors.rkt" (provider-error classify-http-status raise-provider-error) (C4))))

(define (marker-policy-datum marker)
  (list (protocol-marker-value marker) (protocol-marker-context marker)))

(define (marker-source marker binding)
  (case (protocol-marker-context marker)
    [(hash-key)
     (format "(define ~a (hash-ref payload '~a #f))" binding (protocol-marker-value marker))]
    [(string-literal) (format "(define ~a ~s)" binding (protocol-marker-value marker))]))

(define (problem-kind? kind problem)
  (and (pair? problem) (eq? (car problem) kind)))

(test-case "W3-B1: policy is versioned, complete, and pins only W0 C1-C8 neutral helpers"
  (check-equal? (provider-locality-policy-version policy) 1)
  (check-equal? (for/list ([protocol (in-list (provider-locality-policy-protocols policy))])
                  (list (provider-protocol-name protocol)
                        (provider-protocol-owners protocol)
                        (map marker-policy-datum (provider-protocol-markers protocol))))
                expected-protocol-policy)
  (check-equal? (for/list ([helper (in-list (provider-locality-policy-neutral-helpers policy))])
                  (list (neutral-helper-module helper)
                        (neutral-helper-primitives helper)
                        (neutral-helper-evidence helper)))
                expected-neutral-policy)
  (check-equal?
   (sort (remove-duplicates (append-map neutral-helper-evidence
                                        (provider-locality-policy-neutral-helpers policy)))
         symbol<?)
   '(C1 C2 C3 C4 C5 C6 C7 C8))
  (check-equal? (check-provider-locality-policy policy repo-root) '()))

(test-case "W3-B2: current production LLM sources respect provider ownership and neutrality"
  (check-equal? (check-provider-change-locality policy (production-llm-source-units repo-root)) '()))

(test-case "W3-B3: positive probes accept every marker in every allowed ownership path"
  (for* ([protocol (in-list (provider-locality-policy-protocols policy))]
         [owner (in-list (provider-protocol-owners protocol))]
         [marker (in-list (provider-protocol-markers protocol))])
    (check-equal?
     (check-provider-change-locality policy (list (source-unit owner (marker-source marker 'probe))))
     '()
     (format "allowed owner rejected for ~a/~a in ~a"
             (provider-protocol-name protocol)
             (protocol-marker-value marker)
             owner))))

(test-case "W3-B4: negative probes reject every marker in every generic streaming module"
  (for* ([generic (in-list (provider-locality-policy-generic-streaming-modules policy))]
         [protocol (in-list (provider-locality-policy-protocols policy))]
         [marker (in-list (provider-protocol-markers protocol))])
    (define violations
      (check-provider-change-locality policy
                                      (list (source-unit generic (marker-source marker 'leaked)))))
    (check-equal? (length violations) 1)
    (define violation (car violations))
    (check-equal? (locality-violation-reason violation) 'generic-stream-protocol)
    (define message (locality-violation->string violation))
    (check-true (string-contains? message generic))
    (check-true (string-contains? message (protocol-marker-value marker)))
    (check-true (string-contains? message (symbol->string (provider-protocol-name protocol))))
    (for ([owner (in-list (provider-protocol-owners protocol))])
      (check-true (string-contains? message owner)
                  (format "developer message omitted allowed owner ~a" owner)))))

(test-case "W3-B5: negative probes reject every marker in another provider adapter"
  (define protocols (provider-locality-policy-protocols policy))
  (for* ([protocol (in-list protocols)]
         [marker (in-list (provider-protocol-markers protocol))])
    (define wrong-owner
      (car (provider-protocol-owners (findf (lambda (candidate)
                                              (not (eq? (provider-protocol-name candidate)
                                                        (provider-protocol-name protocol))))
                                            protocols))))
    (define violations
      (check-provider-change-locality policy
                                      (list (source-unit wrong-owner
                                                         (marker-source marker 'leaked)))))
    (check-equal? (length violations) 1)
    (check-equal? (locality-violation-reason (car violations)) 'wrong-provider-owner)
    (check-true (string-contains? (locality-violation->string (car violations))
                                  "allowed ownership path"))))

(test-case "W3-B6: context-specific matching ignores comments and neutral identifiers"
  (check-equal?
   (check-provider-change-locality
    policy
    (list (source-unit "llm/stream.rkt"
                       ";; content_block_delta is documentation only\n(define candidates '())")))
   '()))

(test-case "W3-B7: compound literals and hash keys cannot hide markers; readers fail closed"
  (define marker "content_block_delta")
  (for ([source (in-list (list (format "(define leaked #~s)" marker)
                               (format "(define leaked #(~s))" marker)
                               (format "(define leaked #rx~s)" marker)
                               (format "(define leaked (box ~s))" marker)))])
    (define violations
      (check-provider-change-locality policy (list (source-unit "llm/stream.rkt" source))))
    (check-equal? (length violations) 1 source)
    (check-equal? (locality-violation-reason (car violations)) 'generic-stream-protocol))
  ;; gemini hash-key markers hidden in literals, constructors, and alist builders
  (for ([source (in-list (list "(define leaked #hash((\"candidates\" . 1)))"
                               "(define leaked (hash 'functionCall 1))"
                               "(define leaked (hasheq 'functionCall 1))"
                               "(define leaked (make-hasheq '((functionCall . 1))))"))])
    (define violations
      (check-provider-change-locality policy (list (source-unit "llm/stream.rkt" source))))
    (check-equal? (length violations) 1 source)
    (check-equal? (locality-violation-reason (car violations)) 'generic-stream-protocol))
  (define malformed
    (check-provider-change-locality policy
                                    (list (source-unit "llm/stream.rkt"
                                                       "(define leaked \"content_block_delta\") ("))))
  (check-equal? (length malformed) 1)
  (check-equal? (locality-violation-reason (car malformed)) 'source-read-error)
  (check-true (string-contains? (locality-violation->string (car malformed)) "reader failed")))

(define (replace-unit units path synthetic)
  (append (for/list ([unit (in-list units)]
                     #:unless (string=? (source-unit-path unit) path))
            unit)
          (list synthetic)))

(test-case "W3-B8: allowlist drift is rejected without inventing helper defects"
  (define original (provider-locality-policy-neutral-helpers policy))
  (define drifted-policy
    (struct-copy provider-locality-policy policy [neutral-helpers (cons (car original) original)]))
  (define problems (check-provider-locality-policy drifted-policy repo-root))
  (check-not-false (findf (lambda (problem) (problem-kind? 'neutral-helper-allowlist-drift problem))
                          problems))
  (check-false (findf (lambda (problem) (problem-kind? 'neutral-primitive-ownership problem))
                      problems))
  (check-false (findf (lambda (problem) (problem-kind? 'missing-neutral-primitive-definition problem))
                      problems))
  (check-false (findf (lambda (problem) (problem-kind? 'missing-neutral-primitive-export problem))
                      problems)))

(test-case "W3-B8b: a neutral primitive defined in a second module violates ownership"
  (define real-units (production-llm-source-units repo-root))
  (define intruder
    (source-unit "llm/adapters/eager-stream.rkt"
                 "#lang racket/base\n(define (make-provider-http-request . _) (void))"))
  (define problems
    (check-provider-locality-policy-units
     policy
     (replace-unit real-units "llm/adapters/eager-stream.rkt" intruder)))
  (define ownership (findf (lambda (p) (problem-kind? 'neutral-primitive-ownership p)) problems))
  (check-not-false ownership)
  (check-equal? (cadr ownership) 'make-provider-http-request)
  (check-equal? (caddr ownership) "llm/http-helpers.rkt"))

(test-case "W3-B8c: provide semantics decide definition and export defects independently"
  (define real-units (production-llm-source-units repo-root))
  (define (with-http-helpers source)
    (check-provider-locality-policy-units
     policy
     (replace-unit real-units "llm/http-helpers.rkt" (source-unit "llm/http-helpers.rkt" source))))
  (define well-formed
    (string-append
     "#lang racket/base\n"
     "(define (make-provider-http-request . _) (void))\n"
     "(define (check-provider-status! . _) (void))\n"
     "(define (translate-stop-reason . _) (void))\n"
     "(provide make-provider-http-request check-provider-status! translate-stop-reason)"))
  (check-equal? (with-http-helpers well-formed) '())
  (define removed-def
    (string-append
     "#lang racket/base\n"
     "(provide make-provider-http-request check-provider-status! translate-stop-reason)"))
  (define removed-problems (with-http-helpers removed-def))
  (check-not-false (findf (lambda (p) (problem-kind? 'missing-neutral-primitive-definition p))
                          removed-problems))
  (define excluded
    (string-append "#lang racket/base\n"
                   "(define (make-provider-http-request . _) (void))\n"
                   "(define (check-provider-status! . _) (void))\n"
                   "(define (translate-stop-reason . _) (void))\n"
                   "(provide (except-out (all-defined-out) make-provider-http-request))"))
  (define excluded-problems (with-http-helpers excluded))
  (define excl
    (findf (lambda (p) (problem-kind? 'missing-neutral-primitive-export p)) excluded-problems))
  (check-not-false excl)
  (check-equal? (caddr excl) 'make-provider-http-request)
  (define renamed
    (string-append "#lang racket/base\n"
                   "(define (make-provider-http-request . _) (void))\n"
                   "(define (check-provider-status! . _) (void))\n"
                   "(define (translate-stop-reason . _) (void))\n"
                   "(provide (rename-out (make-provider-http-request renamed-helper)))"))
  (define renamed-problems (with-http-helpers renamed))
  (check-not-false (findf (lambda (p) (problem-kind? 'missing-neutral-primitive-export p))
                          renamed-problems)))
